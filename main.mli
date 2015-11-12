(** A record containing a string of its name, a list of attacks, a string of
  * its type, and an int HP
  *)
type pokecaml

(** A list containing all of the trainer’s pokecaml*)
val camldex : pokecaml list

(** A list of all pokecaml in the game*)
val all_pokecaml : pokecaml list

(** The main REPL to handle input and return output. It takes the
  * camldex as input *)
val game : pokecaml list -> unit