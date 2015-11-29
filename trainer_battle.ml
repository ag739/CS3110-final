open Pokecaml

type trainer = {name: string; poke_list: pokecaml list; intro: string}

let all_trainers  = [{name = "Chirag";
                      poke_list = [
                        {name = "Chiragzard"; attacks= [("piazza", 3)];
                        pokecaml_type= Humanities; hp= 100};
                      ]; intro = "I’m better than you. Someone needs to stop me."
                    }]

(** Checks if all pokecaml in a pokecaml list have fainted, a helper function
for has_lost and has_won *)
let all_fainted (camldex: pokecaml list) : bool =
  let rec check_hp c =
    (match c with
    | [] -> 0
    | h::t -> h.hp + (check_hp t)) in
  (check_hp camldex) = 0

let has_lost (camldex: pokecaml list) : bool =
  (all_fainted camldex) = true

let has_won (t_camldex: pokecaml list) : bool =
  (all_fainted t_camldex) = true

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex (p1) and the opponents pokecaml list (p2) *)
let battle (p1: pokecaml list) (p2: pokecaml list) : pokecaml list =
  (*TODO implement*)
  if has_lost p1 then
    let () = print_string "You lost...I guess you won't be the next PokeCaml master.\n
    GAMEOVER" in p1
  else if has_won p2 then
    let () = print_endline "You defeated this trainer!" in p1
  else let () = print_endline "This hasn't been implemented yet" in p1

let run_trainer (camldex : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.name ^ " appeared!") in
  let () = print_endline trainer.intro in
  battle camldex trainer.poke_list