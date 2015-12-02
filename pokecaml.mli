  type p_type = Hardware | Software | Humanities

  (** A record containing a string of its name, a list of attacks, a string of its
    * type, and an int HP *)
  type pokecaml = {name: string; attacks: (string*int) list;
                   pokecaml_type: p_type; hp: int}

  (** A list containing all of the trainer’s pokecaml*)
  val camldex : pokecaml list

  (** A list of all pokecaml in the game*)
  val all_pokecaml : pokecaml list

  (** A switch function to allow you to switch pokecaml during battles
  It will allow you to choose which pokecaml is the first in your camldex.*)
  val switch: pokecaml list -> pokecaml list

  (** Check if list of pokecaml all have hp of 0 *)
  val all_fainted: pokecaml list -> bool

  (** Check if a single pokecaml has an hp of 0 *)
  val has_fainted: pokecaml -> bool

  val find_by_name : pokecaml list -> string -> pokecaml

(** Takes first pokecaml and second pokecaml as input, returns second pokecaml
  * with lowered HP *)
  val attack : pokecaml -> (string * int) -> pokecaml -> pokecaml

  (** Returns: first pokecaml in the list that does not have an hp of 0.*)
  val first_pokecaml : pokecaml list -> pokecaml

  (**Print all attacks*)
  val print_attacks: (string * int) list -> unit