open Pokecaml

let catch (wild : pokecaml) : bool =
  let () = Random.self_init () in
  if wild.hp > 15 && (Random.int 10) < 7 then false else true

let rec list_mem (lst : pokecaml list) (item : pokecaml) : bool =
  match lst with
  | [] -> false
  | h::t -> if h.name = item.name then true else list_mem t item

let update_camldex_after_catch (camldex : pokecaml list) (p : pokecaml)
                                : pokecaml list =
  if list_mem camldex p then camldex else camldex@[p]

let wild_attack (p : pokecaml) : (string * int) =
  let attack_list = p.attacks in
  let index = Random.int (List.length attack_list) in
  List.nth attack_list index

let rec update_camldex_after_attack (camldex : pokecaml list) (p : pokecaml)
                                    : pokecaml list =
  match camldex with
  | [] -> []
  | h::t when h.name=p.name -> p::t
  | h::t -> h::(update_camldex_after_attack t p)

let rec perform_user_attack (input : string) (current_pokecaml : pokecaml)
                            (wild : pokecaml) (camldex : pokecaml list)
                            : pokecaml list =
  if (valid_attack current_pokecaml input) then
    let a = get_attack current_pokecaml input in
    let wild = attack current_pokecaml a wild in
    let () = print_string ("You attacked with " ^ input ^ "\n") in
    let ()=print_endline(wild.name^"'s HP is now "^string_of_int(wild.hp)) in
    if has_fainted wild then
      let () = print_endline ("You defeated " ^ wild.name ^ "!") in camldex
    else let () = print_newline () in battle camldex wild 1
  else
    let () = print_string "You did not enter a valid input.\n" in
    battle camldex wild 0

and handle_fainted (current_pokecaml : pokecaml) (camldex : pokecaml list)
                   (wild : pokecaml) : pokecaml list =
  let () = print_endline (current_pokecaml.name^" fainted!") in
  if all_fainted camldex then
    let () = print_endline ("All of your pokecaml fainted...GAMEOVER") in
    exit 0
  else
    let () = print_endline "Switch pokecaml..." in
    let () = print_newline() in battle (switch camldex) wild 0

and battle (camldex : pokecaml list) (wild : pokecaml) (player: int) : pokecaml list =
  let current_pokecaml = first_pokecaml camldex in
  if player = 0 then
    (let () = print_string "It's your turn! What will you do?\n
                           Type \"catch\" to catch the wild pokecaml.\n
                           Type \"switch\" to switch your pokecaml.\n
                           Or, you can type any of your attacks:\n" in
    let () = print_attacks current_pokecaml.attacks in
    let () = print_string ">>> " in
    let input = String.lowercase (read_line ()) in
    match input with
    | "switch" -> battle (switch camldex) wild 1
    | "catch" ->
      if (catch wild) = true then
        let () = print_endline ("You successfully caught " ^ wild.name ^ "!") in
        update_camldex_after_catch camldex wild
      else let () =print_string("Catch did not work\n") in battle camldex wild 1
    | _ -> perform_user_attack input current_pokecaml wild camldex)
  else
    let opponent_attack = wild_attack wild in
    let current_pokecaml = attack wild opponent_attack current_pokecaml in
    let ()=print_string (wild.name ^" attacked with "^ (fst opponent_attack)) in
    let () = print_newline () in
    let () = print_endline (current_pokecaml.name ^ "'s HP is now " ^
                            string_of_int(current_pokecaml.hp)) in
    let camldex = update_camldex_after_attack camldex current_pokecaml in
    if has_fainted current_pokecaml then
      handle_fainted current_pokecaml camldex wild
    else
      let () = print_newline() in battle camldex wild 0

let run_wild (camldex : pokecaml list) : pokecaml list =
  let length_pokecamls = List.length all_pokecaml in
  let random_int = Random.int (length_pokecamls) in
  let wild = List.nth all_pokecaml random_int in
  let () = print_endline ("A wild " ^ wild.name ^ " appeared!") in
  battle camldex wild 0