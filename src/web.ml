open Containers
open Cohttp_lwt_unix
open Lwt.Infix

module Db = struct
  let create st =
    Lwt_mvar.create st

  let with_db db f =
    Lwt_mvar.take db >>= fun ads ->
      let result, ads' = f ads in
      Lwt_mvar.put db ads' >|= fun () ->
        result

  let get db current_time id =
    with_db db @@ fun ads ->
      match Reklama.find_ad_by_id ads id current_time with
      | Some ad -> (Some ad, ads)
      | None -> (None, ads)

  let match_ db channel interests current_time =
    with_db db @@ fun ads ->
      match Reklama.find_matching_ad ads channel interests current_time with
      | Some ad -> (Some ad, ads)
      | None -> (None, ads)

  let retrieve db current_time id =
    with_db db @@ fun ads ->
      match Reklama.find_ad_by_id ads current_time id with
      | Some ad -> (Some ad, ads)
      | None -> (None, ads)

  let view db channel ad =
    with_db db @@ fun ads ->
      Reklama.Ad.view channel ad ads
end

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end

class ad db = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method private to_json rd =
    let current_time = Ptime_clock.now () in
    Db.retrieve db current_time (self#id rd) >>= function
      | None -> Wm.continue (`String "{}") rd
      | Some ad ->
          Db.view db None ad >>= function
            | None -> Wm.continue (`String "{}") rd
            | Some uri -> Wm.continue (`String "{}") @@ Wm.Rd.redirect uri rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method resource_exists rd =
    let current_time = Ptime_clock.now () in
    Db.retrieve db current_time (self#id rd) >>= function
      | None -> Wm.continue false rd
      | Some _ -> Wm.continue true rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private id rd =
    int_of_string (Wm.Rd.lookup_path_info_exn "id" rd)
end

class match_ad db = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method private to_json rd =
    let channel = Uri.get_query_param rd.Wm.Rd.uri "channel" in
    let interests = match Uri.get_query_param' rd.Wm.Rd.uri "interests" with
      | Some ints -> ints
      | None -> [] in
    let current_time = Ptime_clock.now () in
    Db.match_ db channel interests current_time >>= function
      | None -> Wm.continue (`String "{}") rd
      | Some ad ->
          Db.view db channel ad >>= function
            | None -> Wm.continue (`String "{}") rd
            | Some uri -> Wm.continue (`String "{}") @@ Wm.Rd.redirect uri rd

  method resource_exists rd =
    let channel = Uri.get_query_param rd.Wm.Rd.uri "channel" in
    let interests = match Uri.get_query_param' rd.Wm.Rd.uri "interests" with
      | Some ints -> ints
      | None -> [] in
    let current_time = Ptime_clock.now () in
    Db.match_ db channel interests current_time >>= function
      | Some _ -> Wm.continue true rd
      | None -> Wm.continue false rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd
end

let main () =
  let port = 8080 in
  let db = Db.create @@ Reklama.load_initial_db "ads.sexp" in
  let routes = [
    ("/ad/:id", fun () -> new ad db);
    ("/ad", fun () -> new match_ad db);
  ] in
  let callback (ch, conn) request body =
    let open Cohttp in
    Wm.dispatch' routes ~body ~request
    >|= (function
      | None -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result)
    >>= fun (status, headers, body, path) ->
      Server.respond ~headers ~body ~status ()
  in
  let config = Server.make ~callback () in
  Server.create ~mode:(`TCP(`Port port)) config
  >>= fun () ->
    Printf.eprintf "Listining on 0.0.0.0:%d\n" port;
    Lwt.return_unit

let () =
  Lwt_main.run @@ main ()
