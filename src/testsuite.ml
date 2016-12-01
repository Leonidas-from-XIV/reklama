open Containers

let ad_id = 23

let channel = Reklama.{
    name = "nyt";
    categories = []
  }

let uri = Uri.of_string "https://nytimes.com/"

let ad = QCheck.(
  small_int
  |> map @@ fun views ->
    (views,
     Reklama.Ad.build ad_id Ptime.min Ptime.max views uri [(channel, views / 2)] ["travel"]))

let ad_maybe_with_interest = QCheck.(
  (pair pos_int bool) |> map @@ fun (id, include_travel) ->
    Reklama.Ad.build id Ptime.min Ptime.max max_int uri [] (if include_travel then ["travel"] else []))

let ads_with_interests = QCheck.(list ad_maybe_with_interest)

let db_with_interests = QCheck.(ads_with_interests |> map @@ fun ads -> Reklama.Ad.db_of_ad_list ads)

let proper_amount_of_views = QCheck.(Test.make ~count:1000 ad @@
  fun (views, ad) ->
    let db = Reklama.Ad.db_of_ad_list [ad] in
    let rec loop db uris =
      match Reklama.find_ad_by_id db (Ptime_clock.now ()) ad_id with
      | None -> uris
      | Some ad ->
          match Reklama.Ad.view None ad db with
          | (None, _) -> uris
          | (Some uri, db) -> loop db (uri::uris)
    in
    let uris = loop db [] in
    List.length uris = views)

let proper_amount_of_channel_views = QCheck.(Test.make ~count:1000 ad @@
  fun (views, ad) ->
    let channel_views = views / 2 in
    let channel_name = Some Reklama.(channel.name) in
    let db = Reklama.Ad.db_of_ad_list [ad] in
    let rec loop db uris =
      match Reklama.find_matching_ad db channel_name ["travel"] (Ptime_clock.now ()) with
      | None -> uris
      | Some ad ->
          match Reklama.Ad.view channel_name ad db with
          | (None, _) -> uris
          | (Some uri, db) -> loop db (uri::uris)
    in
    let uris = loop db [] in
    List.length uris = channel_views)

let list_all pred xs =
  xs
  |> List.find_pred (fun x -> not @@ pred x)
  |> CCOpt.is_none

let list_any pred xs =
  xs
  |> List.find_pred pred
  |> CCOpt.is_some

let all_matches_proper_interest = QCheck.(Test.make ~count:1000 db_with_interests @@
  fun db ->
    let channel_name = Some Reklama.(channel.name) in
    let rec loop db categories = function
      | 0 -> categories
      | n ->
        match Reklama.find_matching_ad db channel_name ["travel"] (Ptime_clock.now ()) with
        | None -> categories
        | Some ad ->
            match Reklama.Ad.view channel_name ad db with
            | (None, _) -> categories
            | (_, db) -> loop db ((Reklama.Ad.categories channel_name ad)::categories) (n-1)
    in
    let categories = loop db [] 100 in
    let has_travel_category categories =
      categories
      |> Reklama.Categories.to_list
      |> list_any (fun x -> x = "travel")
    in
    list_all has_travel_category categories)

let main () =
  QCheck_runner.run_tests_main [
    proper_amount_of_views;
    proper_amount_of_channel_views;
    all_matches_proper_interest;
  ]

let () =
  main ()
