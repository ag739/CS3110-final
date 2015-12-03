open Trainer_battle
open Pokecaml
open Wild_pokecaml_battle
open Gameplay

(* Tests of Gameplay *)
let test_quit_command () = assert (find_command "quit" = Quit)
let test_Quit_command () = assert (find_command "Quit" = Quit)

let test_camldex_command () = assert (find_command "camldex" = Camldex)
let test_Camldex_command () = assert (find_command "Camldex" = Camldex)

let test_help_command () = assert (find_command "Help" = Help)
let test_Help_command () = assert (find_command "help" = Help)

let test_battle_command () = assert (find_command "Battle" = Battle)
let test_Battle_command () = assert (find_command "battle" = Battle)

let test_heal_command () = assert (find_command "heal pokecaml" = Heal)
let test_Heal_command () = assert (find_command "Heal PokeCaml" = Heal)

let test_undetermined_command () = assert (find_command "abcd" = Undetermined)

let test_y_quitting () = assert (quitting "y")

let test_n_quitting () = assert (not (quitting "n"))

(* Test Trainers*)
let test_all_trainers () = assert (all_trainers =
        [{tname = "DJ OCaml";
          poke_list = [{name = "Recursee"; attacks = [("Base case", 8); ("Rec", 12);
          ("Return", 10); ("Tail-recursion", 10)]; pokecaml_type = Software; hp = 100};
          {name = "Deferredata"; attacks = [("Bind", 6); ("Upon", 4);
          ("Return", 7); (">>=", 12)]; pokecaml_type = Hardware; hp = 100}];
          intro = "This battle that is about to start is going to be FIRE!";
          };

          {tname = "Prelim Master";
          poke_list = [{name = "Camlchu"; attacks = [("Electrocute", 8)];
          pokecaml_type = Hardware; hp = 100};
          {name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 100};
          {name = "Piazza"; attacks = [("Question", 3)];
          pokecaml_type = Software; hp = 100}];
          intro = "If I can ruin the curve by getting a perfect score on all of my prelims, I can definitely defeat you!";
          };

          {tname = "Anonymous";
          poke_list = [{name = "Piazza"; attacks = [("Question", 3)];
          pokecaml_type = Software; hp = 100}];
          intro = "My name will always remain anonymous to classmates!";
          };
        ])
(* Test of Pokecaml *)
let test_all_pokecaml () = assert (all_pokecaml =
        [{name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 100};
         {name = "Camlchu"; attacks = [("Electrocute", 8)];
          pokecaml_type = Hardware; hp = 100};
         {name = "Piazza"; attacks = [("Question", 3)];
          pokecaml_type = Software; hp = 100};
         {name = "Immutabilitypuff"; attacks = [("Pattern Match", 10); ("Infinite Recursion", 2)];
          pokecaml_type = Software; hp = 100};
         {name = "Recursee"; attacks = [("Base case", 8); ("Rec", 12);
          ("Return", 10); ("Tail-recursion", 10)]; pokecaml_type = Software; hp = 100};
         {name = "Deferredata"; attacks = [("Bind", 6); ("Upon", 4);
          ("Return", 7); (">>=", 12)]; pokecaml_type = Hardware; hp = 100};
         {name = "Proofle"; attacks = [("Induction", 10); ("Equivalence", 8);
          ("Math", 7); ("Specify", 11)]; pokecaml_type = Humanities; hp = 100}
        ])

let fainted_pokecaml =
  [{name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
    pokecaml_type = Software; hp = 0};
    {name = "Camlchu"; attacks = [("Electrocute", 8)];
    pokecaml_type = Hardware; hp = 0};
    {name = "Piazza"; attacks = [("Question", 3)];
    pokecaml_type = Software; hp = 0}]

let test_all_fainted () = assert (all_fainted fainted_pokecaml)

let test_has_fainted () = assert (has_fainted (List.nth fainted_pokecaml 0))

(*TODO: Test switch*)

let test_attack () =
  assert (attack {name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 100} ("Eval", 10)
          {name = "Camlchu"; attacks = [("Electrocute", 8)];
          pokecaml_type = Hardware; hp = 100} =
          {name = "Camlchu"; attacks = [("Electrocute", 8)];
          pokecaml_type = Hardware; hp = 60})

let test_first_pokecaml_not_hp_0 () =
  assert (first_pokecaml all_pokecaml = {name = "Interpreter";
          attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 100})

let test_first_pokecaml_some_hp_0 () =
  let camls = [{name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 0};
         {name = "Camlchu"; attacks = [("Electrocute", 8)];
          pokecaml_type = Hardware; hp = 0};
         {name = "Piazza"; attacks = [("Question", 3)];
          pokecaml_type = Software; hp = 50};
         {name = "Immutabilitypuff"; attacks = [("Pattern Match", 10); ("Infinite Recursion", 2)];
          pokecaml_type = Software; hp = 80};] in
  assert (first_pokecaml camls = {name = "Piazza"; attacks = [("Question", 3)];
          pokecaml_type = Software; hp = 50})

(* Tests of Wild_pokemon_battle *)

let test_caml = {name = "Interpreter"; attacks = [("Eval", 10); ("Env", 8)];
          pokecaml_type = Software; hp = 90}

let test_catch_true () =
  assert (catch {test_caml with hp = 15})

let test_catch_false () =
  assert (not (catch {test_caml with hp = 16}))

let test_update_camldex_after_catch_in () =
  assert (update_camldex_after_catch all_pokecaml test_caml = all_pokecaml)

let test_update_camldex_after_catch_out () =
  assert (update_camldex_after_catch fainted_pokecaml {name = "Proofle"; attacks = [("Induction", 10); ("Equivalence", 8);
          ("Math", 7); ("Specify", 11)]; pokecaml_type = Humanities; hp = 100} = fainted_pokecaml@[{name = "Proofle"; attacks = [("Induction", 10); ("Equivalence", 8);
          ("Math", 7); ("Specify", 11)]; pokecaml_type = Humanities; hp = 100}])

let () =
  test_quit_command ();
  test_Quit_command ();
  test_camldex_command ();
  test_Camldex_command ();
  test_help_command ();
  test_Help_command ();
  test_battle_command ();
  test_Battle_command ();
  test_heal_command ();
  test_Heal_command ();
  test_undetermined_command ();
  test_y_quitting ();
  test_n_quitting ();
  test_all_trainers ();
  test_all_pokecaml ();
  test_has_fainted ();
  test_all_fainted ();
  test_attack ();
  test_first_pokecaml_not_hp_0 ();
  test_first_pokecaml_some_hp_0 ();
  test_catch_false ();
  test_catch_true ();
  test_update_camldex_after_catch_in ();
  test_update_camldex_after_catch_out ();
  print_endline "All tests passed!"