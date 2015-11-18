open Pokecaml

(** A record containing a string of the trainer name, a list of its pokecaml,
and a string of its introduction *)
type trainer = {name: string; poke_list: pokecaml list; intro: string}

(** A list containing all of the trainers *)
let all_trainers  = [{name = "Chirag"; poke_list = ["Chiragzard"]; intro =
                    "I’m better than you. Someone needs to stop me."}]

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the opponents pokecaml list  *)
let battle (p1: pokecaml list) (p2: pokecaml list) : unit =
  failwith "el oh el u suck"

(** Checks if all pokecaml in a pokecaml list have fainted, a helper function
for has_lost and has_won *)
let all_fainted (camldex: pokecaml list) : bool =
  let rec check_hp c =
    (match c with
    | [] -> 0
    | h::t -> h.hp + (check_hp t)) in
  (check_hp camldex) = 0

(** Returns true if all of your pokecaml have 0 hp *)
let has_lost (camldex: pokecaml list) : bool =
  (all_fainted camldex) = true

(** Return true if all of the trainer’s pokecaml have 0 hp *)
let has_won (camldex: pokecaml list) : bool =
  (all_fainted camldex) = true
