open Pokecaml
open Wild_pokecaml_battle
open Trainer_battle
open Async.Std

  (**
    * [command] contains all possible commands for the main REPL in the game.
    *)
  type command = Quit | Camldex | Help | Battle | Heal | Undetermined

  (**
    * [all_caught camldex] is true iff the length of the [camldex] is equal to
    * the length of [all_pokecaml].
    *)
  val all_caught : pokecaml list -> bool

  (**
    * [find_command input] matches [input] with a proper [command]. It is case-
    * insensitive, such that "quit" is [Quit], "camldex" is [Camldex], "help" is
    * [Help], "battle" is [Battle], "heal pokecaml" is [Heal], and anything else
    * is [Undetermined].
    *)
  val find_command : string -> command

  (**
    * [quitting s] is true is case-insensitive [s] is "y" and false if
    * case-insensitive [s] is "n". If [s] is neither, [quitting] is called on a
    * new input.
    *)
  val quitting : string -> bool

  (**
    * [get_list_index lst p start_ind] increments [start_ind] until [start_ind]
    * is the position of [p] in [lst]. When called, [start_ind] should be 0.
    * Will fail if [p] is not in [lst].
    *)
  val get_list_index : pokecaml list -> pokecaml -> int -> int

  (**
    * [heal_all camldex] outputs a new list with all the [pokecaml] from
    * [camldex] having their [hp] from [all_pokecaml].
    *)
  val heal_all : pokecaml list -> pokecaml list

  (**
    * [game camldex] is the main REPL for normal gameplay (not in a battle).
    *)
  val game : pokecaml list -> unit

  (**
    * [first_camldex input] creates a pokecaml list containing the record of the
    * [pokecaml] whose [name] is equal to [input].
    *)
  val first_camldex : unit -> pokecaml list

  (**
    * [intro_string] is the string printed when the game is started.
    *)
  val intro_string : string
