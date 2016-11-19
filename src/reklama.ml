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

let nyt = {name="nyt"; categories = ["travel"; "cooking"]}

let initial_db = [
  {id = 23; starting=0.0; ending=0.0; views=100; uri=""; channels=[(nyt, 10)]}
]

let main () =
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

let ad_of_sexp e =
  CCSexp.Traverse.(
    field "id" to_int e >>= fun id ->
    field "starting" to_float e >>= fun starting ->
    field "ending" to_float e >>= fun ending ->
    field "views" to_int e >>= fun views ->
    field "uri" to_string e >>= fun uri ->
      let channels = [] in
      return {id; starting; ending; views; uri; channels})

let load_initial_db filename =
  match CCSexpM.parse_file filename with
  | `Error _ -> None
  | `Ok sexp -> match sexp with
    | `List ads -> Some 1
    | `Atom _ -> None

let () =
  ignore @@ load_initial_db "ads.sexp";
  main ()
