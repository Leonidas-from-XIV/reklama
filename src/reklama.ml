open Containers

type timestamp = Ptime.t

type interest = string

type channel = {
  name: string;
  categories: interest list
}

let channel_of_sexp e =
  CCSexp.Traverse.(
    field "name" to_string e >>= fun name ->
    field "categories" to_list e >>= fun cats ->
      map_opt to_string cats >>= fun categories ->
      return {name; categories})

let print_channel out v =
  Format.fprintf out
    "{name = \"%s\"; categories = %a}"
    v.name
    Format.(list ~start:"[" ~stop:"]" string) v.categories;;

type channel_views = (channel * int)

let print_channel_views = Format.(pair print_channel int)

let channel_view_of_sexp e =
  CCSexp.Traverse.(to_pair e >>= fun (ch, v) ->
    to_int v >>= fun v ->
    channel_of_sexp ch >>= fun ch ->
      return (ch, v))

module Categories = Set.Make(String)

(* Variant of List.map which stops mapping after the first change *)
let rec map_first f = function
  | [] -> []
  | x::xs -> let x' = f x in
    match x = x' with
    | true -> x::map_first f xs
    | false -> x'::xs

module Ad : sig
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
  type t = {
    id: int;
    starting: timestamp;
    ending: timestamp;
    views: int;
    uri: Uri.t;
    channels: channel_views list;
    categories: interest list;
  }
  module DataBase = Map.Make(Int)

  let build id starting ending views uri channels categories =
    {id; starting; ending; views; uri; channels; categories}

  let print out v =
    Format.fprintf out
      "{id = %d; starting = %a; ending = %a; views = %d; uri = \"%a\"; channels = %a}"
      v.id
      (Ptime.pp_human ()) v.starting
      (Ptime.pp_human ()) v.ending
      v.views
      Uri.pp_hum v.uri
      Format.(list print_channel_views) v.channels

  let categories channel ad =
    (match channel with
    | None -> []
    | Some cname -> ad.channels
      |> List.find_pred (fun (ch, _) -> ch.name = cname)
      |> (function
        | None -> []
        | Some (ch, _) -> ch.categories))
    |> Categories.add_list @@ Categories.of_list ad.categories

  let within_time time ad =
    Ptime.is_later time ~than:ad.starting && Ptime.is_earlier time ~than:ad.ending

  let views_left ad =
    ad.views > 0


  let views_for_channel_left channel ad =
    match channel with
    | None -> true
    | Some channel_name ->
        let rec loop = function
          | [] -> None
          | (k, v)::cs -> if k.name = channel_name then
            Some v else loop cs
        in
        match loop ad.channels with
        | None -> false
        | Some views -> views > 0

  let of_sexp e =
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

  let count_channel_view channel channel_views =
    match channel with
    | None -> channel_views
    | Some channel -> channel_views
      |> map_first @@ fun (chan, views) ->
        match chan.name = channel with
        | false -> (chan, views)
        | true -> (chan, views - 1)

  let count_view channel ad db =
    db
    |> DataBase.update ad.id (function
      | None -> None
      | Some ad -> Some {ad with
          views = ad.views - 1;
          channels = ad.channels |> count_channel_view channel})

  let db_of_ad_list ads =
    ads
    |> List.map (fun ad -> (ad.id, ad))
    |> List.to_seq
    |> DataBase.of_seq

  let view channel ad db =
    let open CCOpt.Infix in
    let uri = DataBase.get ad.id db >>= fun ad ->
      if ad.views > 0 then Some ad.uri else None
    in
    match uri with
    | Some uri -> (Some uri, count_view channel ad db)
    | None -> (None, db)
end

let filter_for_interests channel interests db =
  interests
  |> List.fold_left (fun acc interest ->
      db |> List.filter (fun ad ->
        ad
        |> Ad.categories channel
        |> Categories.mem interest)
      |> (@) acc)
      []

let filter_for_time current_time db =
  db
  |> List.filter @@ Ad.within_time current_time

let filter_for_views db =
  db
  |> List.filter Ad.views_left

let filter_for_channel_views channel db =
  db
  |> List.filter @@ Ad.views_for_channel_left channel

let find_matching_ad db channel interests current_time =
  let db =
    db
    |> Ad.DataBase.values
    |> List.of_seq
    |> filter_for_interests channel interests
    |> filter_for_time current_time
    |> filter_for_views
    |> filter_for_channel_views channel
  in
  match db with
  | [] -> None
  | ad::_ -> Some ad

let find_ad_by_id db current_time id =
  let potential = (match Ad.DataBase.get id db with
    | Some ad -> [ad]
    | None -> [])
    |> filter_for_time current_time
    |> filter_for_views
  in
  match potential with
  | [] -> None
  | ad::_ -> Some ad

let load_initial_db filename =
  match CCSexpM.parse_file filename with
  | `Error _ -> failwith "SExp parsing failure"
  | `Ok sexp -> sexp
    |> CCSexp.Traverse.list_all Ad.of_sexp
    |> Ad.db_of_ad_list
