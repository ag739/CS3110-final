open Pokecaml

(** A record containing a string of the trainer name, a list of its pokecaml,
  * and a string of its introduction
  *)
type trainer

(** A list containing all of the trainers *)
val all_trainers : trainer list

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the opponents pokecaml list  *)
val battle : pokecaml list -> pokecaml list -> pokecaml list

(** Returns true if all of your pokecaml have 0 hp *)
val has_lost : pokecaml list -> bool

(** Return true if all of the trainer’s pokecaml have 0 hp *)
val has_won : pokecaml list -> bool

val run_trainer : pokecaml list -> pokecaml list