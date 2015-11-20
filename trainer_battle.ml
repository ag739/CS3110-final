open Pokecaml

type trainer = {name: string; poke_list: string list; intro: string}

let all_trainers  = [{name = "Chirag"; poke_list = ["Chiragzard"]; intro =
                    "I’m better than you. Someone needs to stop me."}]

<<<<<<< HEAD
(** Checks if all pokecaml in a pokecaml list have fainted, a helper function
for has_lost and has_won *)
let all_fainted (p_list: pokecaml list) : bool =
=======
(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the opponents pokecaml list  *)
let battle (p1: pokecaml list) (p2: pokecaml list) : unit =
  failwith "el oh el u suck"

(** Checks if all pokecaml in a pokecaml list have fainted, a helper function
for has_lost and has_won *)
let all_fainted (camldex: pokecaml list) : bool =
>>>>>>> ab415d691e8cff1a41c4c17e605c1fd1d8d01beb
  let rec check_hp c =
    (match c with
    | [] -> 0
    | h::t -> h.hp + (check_hp t)) in
<<<<<<< HEAD
  (check_hp p_list) = 0
=======
  (check_hp camldex) = 0
>>>>>>> ab415d691e8cff1a41c4c17e605c1fd1d8d01beb

let has_lost (camldex: pokecaml list) : bool =
  (all_fainted camldex) = true

<<<<<<< HEAD
let has_won (t_camldex: pokecaml list) : bool =
  (all_fainted t_camldex) = true

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex and the opponents pokecaml list  *)
let battle (p1: pokecaml list) (p2: pokecaml list) : unit =
  failwith "TODO el oh el u suck"
=======
let has_won (camldex: pokecaml list) : bool =
  (all_fainted camldex) = true

>>>>>>> ab415d691e8cff1a41c4c17e605c1fd1d8d01beb
