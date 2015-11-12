(** Returns true if the trainer has caught them all*)
val all_caught : camldex -> bool

(** Returns true if the all the trainer’s pokecaml have
  * fainted and have HP of 0*)
val all_fainted : camldex -> bool

(** Return true if the wild pokecaml has 0 hp *)
val has_won : pokecaml -> bool

(** Returns true if the trainer has caught the pokecaml*)
val catch : pokecaml -> bool

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the wild pokecaml*)
val battle : pokecaml list -> pokecaml -> unit

(** Takes first pokecaml and second pokecaml as input, returns second pokecaml
  * with lowered HP *)
val attack : pokecaml -> pokecaml -> pokecaml