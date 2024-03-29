open Pokecaml

  (**
    * [trainer] contains a string of its [tname], a list of [pokecaml] to be in
    * its [poke_list], and a string to act as its [intro].
    *)
  type trainer = {tname: string; poke_list: pokecaml list; intro: string}

  (**
    * [all_trainers] is a list of all [trainer] types in the game.
    *)
  val all_trainers : trainer list

  (**
  * [run_trainer camldex] calls a battle REPL that handles input and outputs
  * an updated list to reflect [hp] changes from battles
  *)
  val run_trainer : pokecaml list -> pokecaml list

  (**
    * [switched_trainer_inventory camldex p] returns a list with the [pokecaml]
    * with the highest [hp] in the first position and the trainer's remaining
    * [pokecaml]s in his/her [camldex] in the remaining positions.
    *)
  val switched_trainer_inventory : pokecaml list -> pokecaml list