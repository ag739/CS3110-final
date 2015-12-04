open Pokecaml

(**
  * [run_wild camldex] calls a battle REPL that handles input and outputs
  * an updated list to reflect [hp] changes from battles and [pokecaml] added to
  * the [camldex].
  *)
  val run_wild : pokecaml list -> pokecaml list

  (**
    * [update_camldex_after_catch camldex p] returns [camldex] with [p] appended
    * to the end if [camldex] does not already contain [p]. If [camldex] already
    * contains [p], it returns [camldex].
    *)
  val update_camldex_after_catch : pokecaml list -> pokecaml -> pokecaml list

  (**
    * [perform_user_attack input p1 p2 camldex] checks if [input] is a valid
    * attack in [p1]'s list of attacks, performs the attack, and provides an
    * updated [camldex].
    *)
  val perform_user_attack : string -> pokecaml -> pokecaml -> pokecaml list ->
                            pokecaml list