open Containers

type timestamp = float

type interest = string

type channel = {
  name: string;
  categories: interest list
}

type channel_views = (channel * int)

type ad = {
  id: int;
  starting: timestamp;
  ending: timestamp;
  views: int;
  uri: string;
  channels: channel_views list;
}

type database = ad list

module CategorySet = Set.Make(String)

let ad_categories ad =
  ad.channels
  |> List.fold_left (fun set (ch, _) ->
      CategorySet.add_list set ch.categories)
  CategorySet.empty

let filter_for_interests interests db =
  interests
  |> List.fold_left (fun acc interest ->
      db |> List.filter (fun ad ->
        ad
        |> ad_categories
        |> CategorySet.mem interest)
      |> (@) acc)
      []

let filter_for_time current_time db =
  db
  |> List.filter @@ fun e ->
    e.starting <= current_time && current_time <= e.ending

let find_matching_ad db channel interests current_time =
  let db =
    db
    |> filter_for_interests interests
    |> filter_for_time current_time
  in
  match db with
  | [] -> None
  | {id=id}::_ -> Some id

let channel_of_sexp e =
  CCSexp.Traverse.(
    field "name" to_string e >>= fun name ->
    field "categories" to_list e >>= fun cats ->
      map_opt to_string cats >>= fun categories ->
      return {name; categories})

let channel_view_of_sexp e =
  CCSexp.Traverse.(to_pair e >>= fun (ch, v) ->
    to_int v >>= fun v ->
    channel_of_sexp ch >>= fun ch ->
      return (ch, v))

let ad_of_sexp e =
  CCSexp.Traverse.(
    field "id" to_int e >>= fun id ->
    field "starting" to_float e >>= fun starting ->
    field "ending" to_float e >>= fun ending ->
    field "views" to_int e >>= fun views ->
    field "uri" to_string e >>= fun uri ->
    field "channels" to_list e >>= fun channel_views ->
      map_opt channel_view_of_sexp channel_views >>= fun channels ->
      return {id; starting; ending; views; uri; channels})

let load_initial_db filename =
  match CCSexpM.parse_file filename with
  | `Error _ -> failwith "SExp parsing failure"
  | `Ok sexp -> CCSexp.Traverse.list_all ad_of_sexp sexp

let main () =
  let initial_db = load_initial_db "ads.sexp" in
  print_string "Channel: ";
  let ch = read_line () in
  let rec loop interests =
    print_string "Interest: ";
    match read_line () with
    | "" -> interests
    | interest -> loop @@ interest::interests
  in
  let interests = loop [] in
  match find_matching_ad initial_db ch interests 0.0 with
  | Some ad_id -> Printf.printf "Found %d\n" ad_id
  | None -> print_endline "No matches found"

let () =
  main ()
