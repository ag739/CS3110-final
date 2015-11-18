open Pokecaml

let all_caught (camldex : pokecaml list): bool =
  (List.length camldex) = (List.length all_pokecaml)

let all_fainted (camldex : pokecaml list) : bool =
  let rec check_hp c =
    (match c with
    | [] -> 0
    | h::t -> h.hp + (check_hp t)) in
  (check_hp camldex) = 0

let has_won (wild : pokecaml) : bool =
  wild.hp = 0

let catch (wild : pokecaml) : bool =
  if wild.hp > 15 then false else true

let compare_types p1 p2 =
  let p1_type = p1.pokecaml_type in
  let p2_type = p2.pokecaml_type in
  match p1_type, p2_type with
  | (Software, Software) -> 3
  | (Software, Hardware) -> 4
  | (Software, Humanities) -> 5
  | (Hardware, Software) -> 2
  | (Hardware, Hardware) -> 3
  | (Hardware, Humanities) -> 4
  | (Humanities, Humanities) -> 3
  | (Humanities, Hardware) -> 2
  | (Humanities, Software) -> 1

let attack (p1 : pokecaml) (attack : (string * int) ) (p2 : pokecaml) : pokecaml =
  let damage = (compare_types p1 p2) * (snd attack) in
  let current_hp = p2.hp in
  let new_hp = current_hp - damage in
  if new_hp < 0 then {p2 with hp = 0} else
  {p2 with hp = new_hp}

let rec first_pokecaml camldex =
  match camldex with
  | [] -> failwith "should have a pokecaml in the camldex"
  | h::t -> if h.hp > 0 then h else first_pokecaml t

let rec print_attacks (attacks : (string * int) list) : unit =
  match attacks with
  | [] -> print_newline ()
  | (a,_)::t -> let () = print_string (a ^ "\n") in print_attacks t

let rec battle (camldex : pokecaml list) (wild : pokecaml) (player: int) : unit =
  (* If it is our player, print out possible moves or the option to try to catch. Input must be one of these options or they need to input again. After input, print out damage or result of catch *)
(* If it is not our player, print out what move the wild pokecaml is playing and the damage *)
  if player = 0 then
    let current_pokecaml = first_pokecaml camldex in
    let () = print_string "It's your turn! What will you do?\n
                           Type 'catch' to catch the wild pokecaml.\n
                           Or, you can type any of your attacks:" in
    let () = print_attacks current_pokecaml.attacks in
    let input = String.lowercase (read_line ()) in
    if input = "catch" then
      if (catch wild) = true then failwith "TODO"
        (* TODO: update your camldex and end the battle*)
      else battle camldex wild 1
    else
      (* TODO: make sure input is a valid attack *)
      battle camldex wild 1
  else
    (* TODO: randomly pick the wild pokecaml's attack and adjust hp *)
    let () = print_string (wild.name ^ " attacked with ") in
    battle camldex wild 0