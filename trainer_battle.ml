open Pokecaml

type trainer = {name: string; poke_list: pokecaml list; intro: string}

let all_trainers  = [{name = "Random trainer";
                      poke_list = [
                        {name = "Piazza"; attacks= [("question", 3)];
                        pokecaml_type= Humanities; hp= 100};
                      ]; intro = "Don't be anonymous...show your face!"
                    }]

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