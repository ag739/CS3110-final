open Pokecaml

type trainer = {name: string; poke_list: pokecaml list; intro: string}

let all_trainers  = [ {name = "DJ OCaml";
                      poke_list = [
                        {name = "Recursee"; attacks= [("Base case", 8); ("Rec", 12);
                        ("Return", 7); ("Tail-recursion", 10)];
                        pokecaml_type = Software; hp= 100};
                        {name = "Deferredata"; attacks= [("Bind", 6); ("Upon", 4);
                        ("Return", 7); (">>=", 12)]; pokecaml_type = Hardware; hp= 100};];
                      intro = "This battle that is about to start is going to be FIRE!"};
                      {name = "Prelim Master";
                      poke_list = [
                        {name = "Camlchu"; attacks= [("electrocute" ,8)];
                        pokecaml_type= Hardware; hp= 100};
                        {name = "Interpreter"; attacks= [("eval", 10);("env"), 8];
                        pokecaml_type = Software; hp= 100};
                        {name = "Piazza"; attacks= [("question", 3)];
                        pokecaml_type= Humanities; hp= 100};];
                      intro = "If I can ruin the curve by getting a perfect score
                      on all of my prelims, I can definitely defeat you!"};
                      {name = "Anonymous";
                      poke_list = [
                        {name = "Piazza"; attacks= [("question", 3)];
                        pokecaml_type= Humanities; hp= 100};];
                      intro = "My name will always remain anonymous to classmates!";
                      }
                    ]

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex (p1) and the opponents pokecaml list (p2) *)
let battle (p1: pokecaml list) (p2: pokecaml list) : pokecaml list =
  (*TODO implement*)
  if all_fainted p1 then
    let () = print_string "All of your pokecaml fainted...GAMEOVER" in p1
  else if all_fainted p2 then
    let () = print_endline "You defeated this trainer!" in p1
  else let () = print_endline "This hasn't been implemented yet" in p1

let run_trainer (camldex : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.name ^ " appeared!") in
  let () = print_endline trainer.intro in
  battle camldex trainer.poke_list