open Gameplay

let () =
  let () = print_string intro_string in
  game (first_camldex (read_line ()))