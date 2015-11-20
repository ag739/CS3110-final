open Pokecaml

type trainer = {name: string; poke_list: string list; intro: string}

let all_trainers  = [{name = "Chirag"; poke_list = ["Chiragzard"]; intro =
                    "I’m better than you. Someone needs to stop me."}]

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
  * Takes as input the CamlDex and the opponents pokecaml list  *)
let battle (p1: pokecaml list) (p2: pokecaml list) : unit =
  failwith "TODO el oh el u suck"
