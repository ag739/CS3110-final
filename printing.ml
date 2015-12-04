open Async.Std

let sec n = Core.Std.Time.Span.of_float n

let return_after v delay =
  after (sec delay) >>= fun () ->
  return v

let rec printing_string_lst (lst : bytes list) =
  match lst with
  | []-> return ()
  | h::t-> return_after h 0.01 >>= fun s -> print_string s; printing_string_lst t

let rec split_string s =
  if String.length s = 1 then [s] else
  [String.sub s 0 1]@(split_string (String.sub s 1 ((String.length s)-1)))

let tell_story s =
  let lst = split_string s in
  printing_string_lst lst