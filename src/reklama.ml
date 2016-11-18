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
  channels: channel_views list;
}

type database = ad list

let found f xs = match List.find_pred f xs with
  | Some _ -> true
  | None -> false

module CategorySet = Set.Make(String)

let ad_categories ad =
  ad.channels
  |> List.fold_left (fun set (ch, _) ->
      CategorySet.add_list set ch.categories)
  CategorySet.empty

let find_matching_ad db channel interests current_time =
  let db = List.fold_left (fun acc interest ->
    db |> List.filter (fun ad ->
      ad_categories ad
      |> CategorySet.mem interest)
    |> (@) acc)
    [] interests in
  match db with
  | [] -> None
  | {id=id}::_ -> Some id

let nyt = {name="nyt"; categories = ["travel"; "cooking"]}

let initial_db = [
  {id = 23; starting=0.0; ending=0.0; views=100; channels=[(nyt, 10)]}
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

let () =
  main ()
