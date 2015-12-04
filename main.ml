open Printing
open Gameplay

let () =
  let _ = tell_story intro_string in
  game (first_camldex (read_line ()))