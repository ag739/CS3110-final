open Trainer_battle
open Pokecaml
open Wild_pokecaml_battle

(*let _ = assert (find_command "quit" = Quit)
let _ = assert (find_command "Quit" = Quit)
let _ = assert (find_command "camldex" = Camldex)
let _ = assert (find_command "Camldex" = Camldex)
let _ = assert (find_command "Help" = Help)
let _ = assert (find_command "help" = Help)
let _ = assert (find_command "Battle" = Battle)
let _ = assert (find_command "battle" = Battle)
let _ = assert (find_command "heal pokecaml" = Heal)
let _ = assert (find_command "Heal PokeCaml" = Heal)
let _ = assert (find_command "abcd" = Undetermined)*)

let _ = assert (all_trainers =
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

let _ = assert (all_pokecaml =
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

let _ = assert (all_fainted fainted_pokecaml)

let _ = assert (has_fainted (List.nth fainted_pokecaml 0))

let _ = print_endline "All tests passed!"