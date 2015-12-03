
  (**
    * [p_type] represents the type of the [pokecaml]
    *)
  type p_type = Hardware | Software | Humanities

  (**
    * [pokecaml] contains a string of its [name], a list of the names and powers
    * of its [attacks], its [pokecaml_type] represented as a [p_type], and its
    * [hp].
    *)
  type pokecaml = {name: string; attacks: (string*int) list;
                   pokecaml_type: p_type; hp: int}

  (**
    * [camldex] is a set of the player's [pokecaml]
    * RI: [camldex] must not contain more than one of each pokecaml.
    *)
  val camldex : pokecaml list

  (**
    * [all_pokecaml] is a set of all [pokecaml] in the game
    * RI: [pokecaml] must not contain more than one of each pokecaml.
    *)
  val all_pokecaml : pokecaml list

  (**
    * [switch camldex] allows the player to switch their pokecaml during a
    * battle. It outputs a new list with the chosen [pokecaml] as the first
    * element.
    *)
  val switch: pokecaml list -> pokecaml list

  (**
    * [all_fainted camldex] is true iff all [pokecaml] in [camldex] have an hp
    * of 0
    *)
  val all_fainted: pokecaml list -> bool

  (**
    * [all_fainted p] is true iff [p] has an hp of 0
    *)
  val has_fainted: pokecaml -> bool

  (**
    * [find_by_name lst name] is the [pokecaml] in [lst] with the provided
    * [name], and fails if the [pokecaml] is not in [lst]
    *)
  val find_by_name : pokecaml list -> string -> pokecaml

  (**
    * [attack p1 a p2] calculates the damage when [p1] uses [a] against [p2] and
    * returns a copy of [p2] with an hp lowered by the amount of damage.
    *)
  val attack : pokecaml -> (string * int) -> pokecaml -> pokecaml

  (** [first_pokecaml lst] returns the [pokecaml] with the lowest index in [lst]
    * that does not have an [hp] of 0.
    *)
  val first_pokecaml : pokecaml list -> pokecaml

  (** [print_attacks lst] prints all items in [lst] so that the format is
    * "Attack 1 name
       Attack 2 name
       ...
       Attack n name"
    *)
  val print_attacks: (string * int) list -> unit

  (**
    * [valid_attack p s] is true iff [p] has an attack named [s].
    *)
  val valid_attack: pokecaml -> string -> bool

  (**
    * [get_attack p s] is the pair of the attack name that is equal to [s]
    * and its associated damage.
    *)
  val get_attack: pokecaml -> string -> string * int