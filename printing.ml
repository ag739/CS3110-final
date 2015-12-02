#load "str.cma"
#require "async"
open Async.Std

let sec n = Core.Std.Time.Span.of_float n

let return_after v delay =
  after (sec delay) >>= fun () ->
  return v

let rec printing_string_lst (lst : bytes list) =
  match lst with
  | []-> return ()
  | h::t-> return_after h 0.01 >>= fun s -> print_string s; printing_string_lst t

let split_string (s : string) =
  Str.split (Str.regexp "") s

let tell_story s =
  let lst = split_string s in
  printing_string_lst lst