open Pokecaml

let catch (wild : pokecaml) : bool =
  let () = Random.self_init () in
  if wild.hp > 15 && (Random.int 10) < 7 then false else true

let rec list_mem (user_list : pokecaml list) (item : pokecaml) : bool =
  match user_list with
  | [] -> false
  | h::t -> if h.name = item.name then true else list_mem t item

let update_camldex_after_catch (user_list : pokecaml list) (p : pokecaml)
                                : pokecaml list =
  if list_mem user_list p then user_list else user_list@[p]

let wild_attack (p : pokecaml) : (string * int) =
  let attack_list = p.attacks in
  let index = Random.int (List.length attack_list) in
  List.nth attack_list index

let rec p0_logic (user_list : pokecaml list) (wild : pokecaml) (player: int)
                 : pokecaml list =
  let current_pokecaml = first_pokecaml user_list in
  let () = print_string "It's your turn! What will you do?\n
  Type \"catch\" to catch the wild pokecaml.\n
  Type \"switch\" to switch your pokecaml.\n
  Or, you can type any of your attacks:\n" in
  let () = print_attacks current_pokecaml.attacks in
  let () = print_string ">>> " in
  let input = String.lowercase (read_line ()) in
  match input with
  | "switch" -> battle (switch user_list) wild 1
  | "catch" ->
      if (catch wild) = true then
        let () = print_endline ("You successfully caught " ^ wild.name ^ "!") in
        update_camldex_after_catch user_list wild
      else let () =print_string("Catch did not work\n") in
        battle user_list wild 1
    | _ -> perform_user_attack input current_pokecaml wild user_list

and p1_logic (user_list : pokecaml list) (wild : pokecaml) (player: int)
             : pokecaml list =
  let current_pokecaml = first_pokecaml user_list in
  let opponent_attack = wild_attack wild in
  let current_pokecaml = attack wild opponent_attack current_pokecaml in
  let ()=print_string ("Wild "^wild.name ^" attacked with "^ (fst opponent_attack)) in
  let () = print_newline () in
  let () = print_endline (current_pokecaml.name ^ "'s HP is now " ^
    string_of_int(current_pokecaml.hp)) in
  let user_list = update_camldex_after_attack user_list current_pokecaml in
  if has_fainted current_pokecaml then
    handle_fainted current_pokecaml user_list wild
  else
   let () = print_newline() in battle user_list wild 0

and perform_user_attack (input : string) (user_pokecaml : pokecaml)
                            (wild : pokecaml) (user_list : pokecaml list)
                            : pokecaml list =
  if (valid_attack user_pokecaml input) then
    let a = get_attack user_pokecaml input in
    let wild = attack user_pokecaml a wild in
    let () = print_string ("\nYou attacked with " ^ input ^ "\n") in
    let ()=print_endline("Wild "^wild.name^"'s HP is now "
      ^string_of_int(wild.hp)) in
    if has_fainted wild then
      let () = print_endline ("\nYou defeated Wild " ^ wild.name ^ "!") in
      user_list
    else let () = print_newline () in battle user_list wild 1
  else
    let () = print_string "You did not enter a valid input.\n" in
    battle user_list wild 0

and handle_fainted (user_pokecaml : pokecaml) (user_list : pokecaml list)
                   (wild : pokecaml) : pokecaml list =
  let () = print_endline (user_pokecaml.name^" fainted!") in
  if all_fainted user_list then
    let () = print_endline ("All of your pokecaml fainted...GAMEOVER\n") in
    exit 0
  else
    let () = print_endline "Switch pokecaml..." in
    let () = print_newline() in battle (switch user_list) wild 0

and battle (user_list : pokecaml list) (wild : pokecaml) (player: int)
           : pokecaml list =
  if player = 0 then
    p0_logic user_list wild player
  else
    p1_logic user_list wild player

let run_wild (user_list : pokecaml list) : pokecaml list =
  let length_pokecamls = List.length all_pokecaml in
  let random_int = Random.int (length_pokecamls) in
  let wild = List.nth all_pokecaml random_int in
  let () = print_endline ("\nA wild " ^ wild.name ^ " appeared!\n") in
  battle user_list wild 0