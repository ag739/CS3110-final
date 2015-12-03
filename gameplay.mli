open Pokecaml
open Wild_pokecaml_battle
open Trainer_battle

type command = Quit | Camldex | Help | Battle | Heal | Undetermined

val all_caught : pokecaml list -> bool

val find_command : string -> command

val quitting : string -> bool

val get_list_index : pokecaml list -> pokecaml -> int -> int

val heal_all : pokecaml list -> pokecaml list

val game : pokecaml list -> unit

val first_camldex : string -> pokecaml list

val intro_string : string
