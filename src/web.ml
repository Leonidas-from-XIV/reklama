(* Reklama REST UI *)

(* Replace stdlib data structures with alternate version with more functionality
   kinda like `from containers import *` in Python but controlled *)
open Containers
open Cohttp_lwt_unix
(* Load some operators from the concurrency library Lwt, in particular >>= *)
open Lwt.Infix

(* This is where the Reklama.Ad.DataBase will be stored and modified *)
module Db : sig
  (* Restrict the module to only these public functions *)
  (* Open Reklama, so we don't have to prefix every type with Reklama. *)
  open Reklama
  (* This type is abstract, no need to leak the implementation *)
  type 'a t
  (* Some public functions *)
  val create : Ad.t Ad.DataBase.t -> Ad.t Ad.DataBase.t t
  val match' : Ad.t Ad.DataBase.t t -> string option -> interest list -> Ptime.t -> Ad.t option Lwt.t
  val retrieve : Ad.t Ad.DataBase.t t -> Ptime.t -> Ad.DataBase.key -> Ad.t option Lwt.t
  val view : Ad.t Ad.DataBase.t t -> string option -> Ad.t -> Uri.t option Lwt.t
end = struct
  (* The implementation of Db.t is an Lwt_mvar, but this is not exposed
     to the outside *)
  type 'a t = 'a Lwt_mvar.t
  (* Creates an instance of the state. The state is an Lwt_mvar, which
     is like an atom that can only be dereferenced once at a time, all other
     processes which attempt to do so will block *)
  let create st =
    Lwt_mvar.create st

  (* internal function to deref the mvar, apply the function f to it and
     save the new value to the mvar and then return the new value. *)
  let with_db db f =
    (* Take dereferences db and returns the value, then the function puts
       the result back. *)
    Lwt_mvar.take db >>= fun ads ->
      let result, ads' = f ads in
      (* The result of putting is unit/()/void so return what we saved before *)
      Lwt_mvar.put db ads' >|= fun () ->
        result

  (* retrieve a matching ad from the database *)
  (* Just like Clojure can have all kinds of characters in a binding, OCaml can
     have ' in a name since `match` is a keyword *)
  let match' db channel interests current_time =
    (* deref the mvar *)
    with_db db @@ fun ads ->
      (* retrieve a match from the db *)
      match Reklama.find_matching_ad ads channel interests current_time with
      (* this does not change the db, so return verbatim *)
      | Some ad -> (Some ad, ads)
      | None -> (None, ads)

  (* retrieve a matching ad from the database *)
  let retrieve db current_time id =
    with_db db @@ fun ads ->
      match Reklama.find_ad_by_id ads current_time id with
      | Some ad -> (Some ad, ads)
      | None -> (None, ads)

  (* this counts a view for the ad passed in *)
  let view db channel ad =
    with_db db @@ fun ads ->
      (* oh how convenient, `Reklama.Ad.view` returns data in exactly the
         right format *)
      Reklama.Ad.view channel ad ads
end

(* A bit of shortcut boilerplate to set up Webmachine.
   Webmachine is a bit like Erlang's… Webmachine or Clojure's Liberator:
   There is a state graph which the request pass through with decision nodes
   (methods in the following code) which determine which path the request
   will take and then which HTTP return code it will have.

   Very convenient to write REST APIs which comply rather strongly to the
   spirit of HTTP return codes. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end

(* an OCaml object bound to a class. OCaml OOP is pretty unique and rarely
   used but for the purposes of understanding this code it is enough
   to know that these objects can be instantiated and act a bit like
   closures.

   For a more detailed view, there is a short intro in German.
   http://www2.in.tum.de/hp/file?fid=592 *)

class virtual base = object(self)
  (* Import predefined method implementations from another class *)
  inherit [Cohttp_lwt_body.t] Wm.resource

  (* A function for returning data. As we will never return any
     body (either redirect or not found) this can be empty *)
  method private to_json rd =
    (* `Foo `Bar, etc are so-called polymorphic variants. They don't mean
        anything by themselves and can be created at will, like Clojure
        keywords. In this case Webmachine knows that `Empty means
        "no response". *)
    Wm.continue `Empty rd

 (* Definition of MIME-types to handler functions which return said
    data. Webmachine supports content negotiation so it can
    automatically pick the right format depending on whether the
    client mentioned he prefers json or some other format like
    XML or HTML *)
  method content_types_provided rd =
    (* We only support returning JSON *)
    Wm.continue [
      "application/json", self#to_json
    ] rd

  (* In the same way, we can accept multiple MIME types as POST
     body. *)
  method content_types_accepted rd =
    (* We don't read POST bodies so we don't need to accept anything *)
    Wm.continue [] rd

  (* Check whether resource exists, are we going the 200 route or
     maybe something else? *)
  method resource_exists rd =
    (* It never exists because we need to redirect or 404 later *)
    Wm.continue false rd
end

class ad db = object(self)
  inherit base

  (* Placeholder to store the ad that we looked up *)
  val matching_ad = Ref.create None

  (* Check whether there was an ad. If it was continue, if not,
     410 Gone *)
  method previously_existed rd =
    let current_time = Ptime_clock.now () in
    (* Retrieve ad by the :id param from the route *)
    Db.retrieve db current_time (self#id rd) >>= function
      | None -> Wm.continue false rd
      | retrieved ->
          (* found an ad, save it for later *)
          (* this is like swap! in Clojure *)
          matching_ad
          |> Ref.update (function
            | None -> retrieved
            | previous -> previous);
          Wm.continue true rd

  (* Check whether to return 307 Moved Temporarily. This is the proper
     return code since we don't want potential caches to cache this
     response *)
  method moved_temporarily rd =
    (* ! is like @ in Clojure and dereferences *)
    match !matching_ad with
      | None -> Wm.continue None rd
      | Some ad ->
          (* Yep, the previous handlers found an atom, count the
             view to retrieve it's URI. *)
          Db.view db None ad >>= fun uri ->
            (* Pass the URI to Webmachine so it can set the Location
               header properly. *)
            Wm.continue uri rd

  (* Helper function to retrieve :id from the request URL *)
  method private id rd =
    (* Convert the string to int *)
    int_of_string @@ Wm.Rd.lookup_path_info_exn "id" rd
end

(* Handle matching ad by interests & channel *)
class match_ad db = object
  inherit base

  (* place holder for both the ad and the channel for which it was requested *)
  val retrieved = Ref.create None

  method previously_existed rd =
    (* read the channel from the query params *)
    let channel = Uri.get_query_param rd.Wm.Rd.uri "channel" in
    (* read a list of interests by comma separated query params *)
    let interests = match Uri.get_query_param' rd.Wm.Rd.uri "interests" with
      | Some ints -> ints
      | None -> [] in
    let current_time = Ptime_clock.now () in
    (* attempt to match an ad to the request *)
    Db.match' db channel interests current_time >>= function
      | None -> Wm.continue false rd
      | Some ad ->
          (* found an ad *)
          retrieved
          |> Ref.update (function
            (* now we also save the channel as we need it for counting
               a view on the channel *)
            | None -> Some (ad, channel)
            | previous -> previous);
          Wm.continue true rd

  method moved_temporarily rd =
    match !retrieved with
      | None -> Wm.continue None rd
      | Some (ad, channel)  ->
          (* This time we count the channel as well *)
          Db.view db channel ad >>= fun uri ->
            Wm.continue uri rd
end

(* some set-up boilerplate *)
let main () =
  let port = 8080 in
  (* Create an instance of the Db with ads read from file *)
  let db = Db.create @@ Reklama.load_initial_db "ads.sexp" in
  let routes = [
    ("/ad/:id", fun () -> new ad db);
    ("/ad", fun () -> new match_ad db);
  ] in
  let callback (ch, conn) request body =
    (* What to do when a request happens? *)
    let open Cohttp in
    Wm.dispatch' routes ~body ~request
    >|= (function
      | None -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result)
    >>= fun (status, headers, body, path) ->
      Server.respond ~headers ~body ~status ()
  in
  (* Set up the server with the callback function to handle requests *)
  let config = Server.make ~callback () in
  Server.create ~mode:(`TCP(`Port port)) config
  >>= fun () ->
    Printf.eprintf "Listining on 0.0.0.0:%d\n" port;
    Lwt.return_unit

let () =
  Lwt_main.run @@ main ()
