open Containers

type timestamp = float

type interest = string

type channel = {
  name: string;
  categories: interest list
}

type channel_views = (channel * int CCLock.t)

type ad = {
  id: int;
  starting: timestamp;
  ending: timestamp;
  views: int CCLock.t;
  channels: channel_views list
}

type database = ad list

let find_matching_ad db channel interests =
  None

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
  ignore @@ find_matching_ad [] ch interests;
  ()

let () =
  main ()
