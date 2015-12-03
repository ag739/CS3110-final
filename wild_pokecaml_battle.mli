open Pokecaml

(** Returns true if the trainer has caught the pokecaml*)
val catch : pokecaml -> bool

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the wild pokecaml*)
val battle : pokecaml list -> pokecaml -> int -> pokecaml list

val run_wild : pokecaml list -> pokecaml list

val update_camldex_after_catch : pokecaml list -> pokecaml -> pokecaml list

val perform_user_attack : string -> pokecaml -> pokecaml -> pokecaml list ->
                          pokecaml list