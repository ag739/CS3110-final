  type p_type = Hardware | Software | Humanities

  (** A record containing a string of its name, a list of attacks, a string of its
    * type, and an int HP *)
  type pokecaml = {name: string; attacks: (string*int) list;
                   pokecaml_type: p_type; hp: int}

  (** A list containing all of the trainer’s pokecaml*)
  val camldex : pokecaml list

  (** A list of all pokecaml in the game*)
  val all_pokecaml : pokecaml list
