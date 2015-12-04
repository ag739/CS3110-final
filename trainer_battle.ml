open Pokecaml

type trainer = {tname: string; poke_list: pokecaml list; intro: string}

let file = Yojson.Basic.from_file (Sys.argv.(1))

let trainer_string_extract (t : Yojson.Basic.json) (key : string)
                           : string list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "trainers"
  |> flatten
  |> filter_member key
  |> filter_string

let trainer_list_extract (t : Yojson.Basic.json) (key : string) =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "trainers"
  |> flatten
  |> filter_member key
  |> filter_list

let trainer_names = trainer_string_extract file "name"

let trainer_intros = trainer_string_extract file "intro"

let trainer_pokecaml_names =
  let open Yojson.Basic.Util in List.map
  (fun file -> file |> filter_string) (trainer_list_extract file "pokecaml")

let rec trainer_pokecaml_record_list (lst : string list) =
  match lst with
  | [] -> []
  | h::t -> (find_by_name all_pokecaml h)::(trainer_pokecaml_record_list t)

let trainer_record (index : int) : trainer =
  { tname = List.nth trainer_names index;
    poke_list = trainer_pokecaml_record_list
    (List.nth trainer_pokecaml_names index);
    intro = List.nth trainer_intros index; }

let rec all_trainer_generator (lst : string list) (start_ind : int)
                               : trainer list =
  match lst with
  | [] -> []
  | h::t -> trainer_record start_ind::all_trainer_generator t (start_ind + 1)

let all_trainers = all_trainer_generator trainer_names 0

let desc_compare (x : int) (y : int) : int =
  if x = y then 0 else
  if x < y then 1 else
  -1

let sort_attacks (attacks : (string * int) list) : (string * int) list =
  List.sort (fun x y -> desc_compare (snd x) (snd y)) attacks

let determine_attack (attacks : (string * int) list) : (string * int) =
  let rand = Random.int 15 in
  match sort_attacks attacks with
  | [] -> failwith "Pokecaml's attacks list was empty!"
  | a::[] -> a
  | a::b::[] -> if rand < 9 then a else b
  | a::b::c::[] -> if rand < 8 then a else if rand < 12 then b else c
  | a::b::c::d::t -> if rand < 6 then a else if rand < 10 then b else if
                     rand < 13 then c else d

let sort_pokecaml_by_hp (t_list : pokecaml list) : pokecaml list =
  List.sort (fun x y -> desc_compare (x.hp) (y.hp)) t_list

let best_pokecaml_choice (t_list : pokecaml list) : pokecaml =
  List.nth (sort_pokecaml_by_hp t_list) 0

let determine_switch (p : pokecaml) (t_list : pokecaml list) : bool =
  let new_lst = remove t_list p in
  let () = Random.self_init () in
  if p.hp < 30 && Random.int 6 < 2 && not (all_fainted new_lst) && not
  (best_pokecaml_choice t_list = p) then
    true
  else false

let switched_trainer_inventory (t_list : pokecaml list)
                               : pokecaml list =
  let best = best_pokecaml_choice t_list in
  best::(remove t_list best)

let rec perform_user_attack (input : string) (user_pokecaml : pokecaml)
                            (t_pokecaml : pokecaml) (user_list : pokecaml list)
                            (t_list : pokecaml list) (turn: int)
                            : pokecaml list =
  if (valid_attack user_pokecaml input) then
    let a = get_attack user_pokecaml input in
    let t_pokecaml = attack user_pokecaml a t_pokecaml in
    let () = print_string ("You attacked with " ^ input ^ "\n") in
    let ()=print_endline("Trainer's "^t_pokecaml.name^"'s HP is now "^
      string_of_int(t_pokecaml.hp)) in
    let () =
      (if has_fainted t_pokecaml then
        print_endline ("Trainer's "^t_pokecaml.name ^ " has fainted!\n")
      else print_newline ()) in
    let t_list = update_camldex_after_attack t_list t_pokecaml in
    if turn = 0 then battle user_list t_list 1 else battle user_list t_list 0
  else
    let () = print_string "You did not enter a valid input.\n" in
    battle user_list t_list turn

and switch_logic (t_pokecaml: pokecaml) (user_pokecaml: pokecaml)
                  (t_list : pokecaml list) (user_list: pokecaml list)
                  (turn: int) : pokecaml list =
  let new_t_list = switched_trainer_inventory t_list in
  let () = print_endline ("The trainer withdrew "^t_pokecaml.name^
    " and sent out "^(List.nth new_t_list 0).name^"!")
  in battle user_list new_t_list 0

and trainer_logic (t_pokecaml: pokecaml) (user_pokecaml: pokecaml)
                  (t_list : pokecaml list) (user_list: pokecaml list)
                  (turn: int) : pokecaml list =
  let () = print_endline ("\nIt's the trainers turn with pokecaml "^
    t_pokecaml.name) in
  if determine_switch t_pokecaml t_list then
    switch_logic t_pokecaml user_pokecaml t_list user_list turn
  else
    let a = determine_attack (t_pokecaml.attacks) in
    let user_pokecaml = attack t_pokecaml a user_pokecaml in
    let () = print_endline ("Trainer's "^t_pokecaml.name ^ " attacked with "
      ^ (fst a)) in
    let () = print_endline (user_pokecaml.name^"'s HP is now "^
      string_of_int(user_pokecaml.hp)) in
    let user_list = update_camldex_after_attack user_list user_pokecaml in
    if has_fainted user_pokecaml then
      let () = print_endline (user_pokecaml.name^" fainted!\n") in
      if all_fainted user_list then
        let () = print_endline ("All of your pokecaml fainted...GAMEOVER\n") in
        exit 0
      else
        let () = print_endline "Switch pokecaml..." in
      let () = print_newline() in battle (switch user_list) t_list 0
    else
      let () = print_newline() in battle user_list t_list 0

and p0_logic (user_list: pokecaml list) (t_list: pokecaml list) (turn: int)
             (user_pokecaml : pokecaml) (t_pokecaml : pokecaml)
             : pokecaml list =
    let () = print_string "It's your turn! What will you do?\n
                          Type \"switch\" to switch your pokecaml.\n
                          Or, you can type any of your attacks:\n" in
    let () = print_attacks user_pokecaml.attacks in
    let () = print_string ">>> " in
    let input = String.lowercase (read_line ()) in
    match input with
    | "switch" -> battle (switch user_list) t_list 1
    | _ -> perform_user_attack input user_pokecaml t_pokecaml user_list t_list 0

and battle (user_list: pokecaml list) (t_list: pokecaml list) (turn: int)
                : pokecaml list =
  if all_fainted user_list then
    let () = print_string "All of your pokecaml fainted...GAMEOVER" in user_list
  else if all_fainted t_list then
    let () = print_endline "You defeated this trainer!\n" in user_list
  else
    let my_p = first_pokecaml user_list in
    let trainer_p = first_pokecaml t_list in
    if turn = 0 then
      p0_logic user_list t_list turn my_p trainer_p
    else
      trainer_logic trainer_p my_p t_list user_list turn

let run_trainer (user_list : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.tname ^ " appeared!") in
  let () = print_endline ("\""^trainer.intro^"\"\n") in
  let trainer_pokecaml = first_pokecaml (trainer.poke_list) in
  let () = print_endline (trainer.tname ^
    " is using " ^ trainer_pokecaml.name^"\n")
  in battle user_list trainer.poke_list 0