open Pokecaml

(** Returns true if the trainer has caught the pokecaml*)
val catch : pokecaml -> bool

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the wild pokecaml*)
val battle : pokecaml list -> pokecaml -> int -> pokecaml list

val run_wild : pokecaml list -> pokecaml list