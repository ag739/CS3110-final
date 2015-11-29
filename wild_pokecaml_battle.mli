open Pokecaml

(** Returns true if the trainer has caught them all*)
val all_caught : pokecaml list -> bool

(** Returns true if the all the trainer’s pokecaml have
  * fainted and have HP of 0*)
val all_fainted : pokecaml list -> bool

(** Return true if the wild pokecaml has 0 hp *)
val has_won : pokecaml -> bool

(** Returns true if the trainer has caught the pokecaml*)
val catch : pokecaml -> bool

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the wild pokecaml*)
val battle : pokecaml list -> pokecaml -> int -> pokecaml list

(** Takes first pokecaml and second pokecaml as input, returns second pokecaml
  * with lowered HP *)
val attack : pokecaml -> (string * int) -> pokecaml -> pokecaml

val run_wild : pokecaml list -> pokecaml list