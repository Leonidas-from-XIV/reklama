(** Reklama, a ad retrieval service *)

(* Use Containers, a small alternative Stdlib with useful… container types. *)
open Containers

(* some type aliases and definitions *)
type timestamp = Ptime.t

type interest = string

(* a channel has a name and a number of interests associated with it *)
type channel = {
  name: string;
  categories: interest list
}

(* combinator to read a value of channel type from an s-expression *)
let channel_of_sexp e =
  CCSexp.Traverse.(
    field "name" to_string e >>= fun name ->
    field "categories" to_list e >>= fun cats ->
      map_opt to_string cats >>= fun categories ->
      return {name; categories})

(* combinator for pretty printing string lists *)
let print_categories = Format.(list ~start:"[" ~stop:"]" string)

(* pretty printer for channel types *)
let print_channel out v =
  (* similar to printf, but allows custom printers with %a *)
  Format.fprintf out
    "{name = \"%s\"; categories = @[<hov>%a@]}"
    v.name
    print_categories v.categories;;

(* a channel might only have a limited number of views available *)
type channel_views = (channel * int)

(* pretty printer combinator for channel_view tuples *)
let print_channel_views = Format.(pair print_channel int)

(* parser for channel views from s-expressions *)
let channel_view_of_sexp e =
  CCSexp.Traverse.(to_pair e >>= fun (ch, v) ->
    to_int v >>= fun v ->
    channel_of_sexp ch >>= fun ch ->
      return (ch, v))

(* create a Set whose items are going to be interests, therefore strings *)
module Categories = Set.Make(String)

(* Variant of List.map which stops mapping after the first change *)
let rec map_first f = function
  | [] -> []
  | x::xs -> let x' = f x in
    match x = x' with
    | true -> x::map_first f xs
    | false -> x'::xs

module Ad : sig
  (* the type signature. Note that we just say type t, therefore the contents of t are
     not public and inaccessible outside of this module. This is deliberate, since the
     only way to get to the URI is by calling view *)
  type t
  val print : Format.t -> t -> unit
  val categories : string option -> t -> Categories.t
  val within_time : timestamp -> t -> bool
  val views_left : t -> bool
  val views_for_channel_left : string option -> t -> bool
  module DataBase : Map.S with type key = int
  val of_sexp : CCSexp.t -> t option
  val db_of_ad_list : t list -> t DataBase.t
  val view : string option -> t -> t DataBase.t -> Uri.t option * t DataBase.t
  val build : int -> Ptime.t -> Ptime.t -> int -> Uri.t -> channel_views list -> interest list -> t
end = struct
  (* here begins the implementation of the module *)
  type t = {
    id: int;
    starting: timestamp;
    ending: timestamp;
    views: int;
    uri: Uri.t;
    channels: channel_views list;
    categories: interest list;
  }

  (* the database holds a mapping of id -> ad, for quick access via ID. *)
  module DataBase = Map.Make(Int)

  (* as the fields are not accessible from outside, this is a constructor function
     which takes all the fields as arguments and returns a record on type Ad.t *)
  let build id starting ending views uri channels categories =
    {id; starting; ending; views; uri; channels; categories}

  (* a pretty printer. Accessing fields from outside is prohibited but printing the
     whole thing can be useful *)
  let print out v =
    Format.fprintf out
      "{id = %d; starting = %a; ending = %a; views = %d; uri = \"%a\"; channels = @[<hov>%a@]; categories = @[<hov>%a@]}"
      v.id
      (Ptime.pp_human ()) v.starting
      (Ptime.pp_human ()) v.ending
      v.views
      Uri.pp_hum v.uri
      Format.(list print_channel_views) v.channels
      print_categories v.categories

  (* collect the categories of an ad. If a channel was passed, it will try to find the
     channel and add its categories to the ad *)
  let categories channel ad =
    (* |> is a kind of threading operator like Clojure's `->>`, `a |> b c` resolves to `b c a`
       but unlike Clojure this is not a macro but a function *)
    (* match does a pattern matching, similar to say, Erlang, but with the advantage of
       the OCaml compiler making sure we match exactly what we expect to match *)
    (* The channel is an option type, so it's either None or Some "name", we have to
       match upon this *)
    (match channel with
    | None -> []
    | Some cname -> ad.channels
      (* find_pred takes a returns the first item that matches the predicate or None *)
      |> List.find_pred (fun (ch, _) -> ch.name = cname)
      (* function is like fun an anonymous function but immediately does a pattern match *)
      |> function
        | None -> []
        | Some (ch, _) -> ch.categories)
    |> Categories.add_list @@ Categories.of_list ad.categories
    (* @@ is like Haskell's $ operator, effektively wraps the right hand side with parens *)

  (* Checks whether the ad is within the specified time *)
  let within_time time ad =
    (* functions can take labelled arguments, kind of like Python *)
    Ptime.is_later time ~than:ad.starting && Ptime.is_earlier time ~than:ad.ending

  (* Checks whether the ad has any views left *)
  let views_left ad =
    ad.views > 0

  (* Checks whether the ad has any views left when coming from this channel *)
  let views_for_channel_left channel ad =
    match channel with
    (* if no channel was specified, then it matches *)
    | None -> true
    | Some channel_name ->
        ad.channels
        |> List.find_pred (fun (ch, _) -> ch.name = channel_name)
        |> function
        (* if the channel is not in the list, it is not restricted to any number of views *)
        | None -> true
        | Some (_, views) -> views > 0

  (* read an Ad.t value form an s-expression *)
  let of_sexp e =
    (* this is also a parsing combinator, taking fields from an sexp and converting
       them to OCaml values. Basically this is a chain of nested functions which
       either all succeed and return an Ad.t or fail returning None *)
    CCSexp.Traverse.(
      field "id" to_int e >>= fun id ->
      field "starting" to_string e >>= fun starting ->
      field "ending" to_string e >>= fun ending ->
      field "views" to_int e >>= fun views ->
      field "uri" to_string e >>= fun uri ->
      field "categories" to_list e >>= fun categories ->
      map_opt to_string categories >>= fun categories ->
      field "channels" to_list e >>= fun channel_views ->
      map_opt channel_view_of_sexp channel_views >>= fun channels ->
        Ptime.of_rfc3339 starting |> Result.to_opt >>= fun (starting, _, _) ->
        Ptime.of_rfc3339 ending |> Result.to_opt >>= fun (ending, _, _) ->
          let uri = Uri.of_string uri in
          return {id; starting; ending; views; uri; channels; categories})

  (* Counts a view for a channel *)
  let count_channel_view channel channel_views =
    match channel with
    | None -> channel_views
    | Some channel -> channel_views
      |> map_first @@ fun (chan, views) ->
        (* decrease the amount of available views when the channel name matches *)
        match chan.name = channel with
        | false -> (chan, views)
        | true -> (chan, views - 1)

  (* Counts a view for an ad *)
  let count_view channel ad db =
    db
    |> DataBase.update ad.id @@ function
      | None -> None
      (* if there was an ad by that id, then build a new one, decrementing
         the view counters *)
      | Some ad -> Some {ad with
          views = ad.views - 1;
          channels = ad.channels |> count_channel_view channel}

  (* construct a database out of a list of ads *)
  let db_of_ad_list ads =
    ads
    |> List.map (fun ad -> (ad.id, ad))
    |> List.to_seq
    |> DataBase.of_seq

  (* Returns the URI of the ad along with a new version of the database
     with the view counted. *)
  let view channel ad db =
    let open CCOpt.Infix in
    let uri = DataBase.get ad.id db >>= fun ad ->
      if ad.views > 0 then Some ad.uri else None
    in
    match uri with
    | Some uri -> (Some uri, count_view channel ad db)
    | None -> (None, db)
end

(* filters the database for entries which match the specified interests *)
let filter_for_interests channel interests db =
  interests
  |> List.fold_left (fun acc interest ->
      db |> List.filter (fun ad ->
        ad
        |> Ad.categories channel
        |> Categories.mem interest)
      |> (@) acc)
      []

(* filters the database for ads which are within the given time *)
let filter_for_time current_time db =
  db
  |> List.filter @@ Ad.within_time current_time

(* filters the database for ads which have views left *)
let filter_for_views db =
  db
  |> List.filter Ad.views_left

(* filters the database for channels whose channel restrictions for the given
   channel have fields left *)
let filter_for_channel_views channel db =
  db
  |> List.filter @@ Ad.views_for_channel_left channel

(* returns a matching ad for the channel, interests and time given *)
let find_matching_ad db channel interests current_time =
  db
  |> Ad.DataBase.values
  |> List.of_seq
  |> filter_for_interests channel interests
  |> filter_for_time current_time
  |> filter_for_views
  |> filter_for_channel_views channel
  (* see how many ads we got and pick one *)
  |> function
  | [] -> None
  | ad::_ -> Some ad

(* returns a matching ad by a known ad id *)
let find_ad_by_id db current_time id =
  (match Ad.DataBase.get id db with
    | Some ad -> [ad]
    | None -> [])
    (* still filter for time and views *)
    |> filter_for_time current_time
    |> filter_for_views
    (* got either 1 or 0 results, pick it *)
    |> function
    | [] -> None
    | ad::_ -> Some ad

(* loads a database of ads from the specified file name *)
let load_initial_db filename =
  match CCSexpM.parse_file filename with
  (* We can't do anything about a wrong file except to quit *)
  | `Error _ -> failwith "S-Expression parsing failure"
  | `Ok sexp -> sexp
    |> CCSexp.Traverse.list_all Ad.of_sexp
    |> Ad.db_of_ad_list
