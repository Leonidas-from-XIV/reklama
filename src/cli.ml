let do_single_request db =
  print_string "Channel: ";
  match read_line () with
  | "" -> None
  | ch -> let rec loop interests =
    print_string "Interest: ";
    match read_line () with
      | "" -> interests
      | interest -> loop @@ interest::interests
    in
    let interests = loop [] in
    begin match Reklama.find_matching_ad db (Some ch) interests 0.0 with
    | Some ad_id -> Format.printf "Found %a\n" Reklama.Ad.print ad_id
    | None -> print_endline "No matches found"
    end;
    Some db

let main () =
  let db = Reklama.load_initial_db "ads.sexp" in
  let rec loop = function
    | Some db -> loop @@ do_single_request db
    | None -> () in
  loop @@ Some db

let () =
  main ()
