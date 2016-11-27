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
    let current_time = Ptime_clock.now () in
    match Reklama.find_matching_ad db (Some ch) interests current_time with
    | None ->
        print_endline "No matches found";
        Some db
    | Some ad ->
        Format.printf "Found %a\n" Reklama.Ad.print ad;
        let (uri, db) = Reklama.Ad.view (Some ch) ad db in
        (match uri with
        | None -> ()
        | Some uri -> Format.printf "URI is %a\n" Uri.pp_hum uri);
        Some db

let main () =
  let db = Reklama.load_initial_db "ads.sexp" in
  let rec loop = function
    | Some db -> loop @@ do_single_request db
    | None -> () in
  loop @@ Some db

let () =
  main ()
