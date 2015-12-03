open Gameplay

(* TODO
 * Write specifications
 * Update design doc
 * Finish trainer_battle
 * Randomized catching
 * Finish testing
 *)

let () =
  let () = print_string intro_string in
  game (first_camldex (read_line ()))