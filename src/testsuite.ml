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

let main () =
  QCheck_runner.run_tests_main [
    proper_amount_of_views;
    proper_amount_of_channel_views;
  ]

let () =
  main ()
