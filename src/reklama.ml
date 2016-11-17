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

let () =
  print_endline "Hello World"
