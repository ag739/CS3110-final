open Pokecaml

let catch (wild : pokecaml) : bool =
  if wild.hp > 15 then false else true

let update_camldex_after_catch camldex p =
  if List.mem p camldex then camldex else camldex@[p]

let rec first_pokecaml camldex =
  match camldex with
  | [] -> failwith "TODO: need a has_lost function"
  | h::t -> if h.hp > 0 then h else first_pokecaml t

let rec print_attacks (attacks : (string * int) list) : unit =
  match attacks with
  | [] -> print_newline ()
  | (a,_)::t -> let () = print_string (a ^ "\n") in print_attacks t

let rec get_attack (p: pokecaml) (a : string) : (string * int) =
  match (p.attacks) with
  | [] -> failwith "Empty list"
  | (s,i)::t -> if s = a then (s,i) else get_attack {p with attacks=t} a

let rec valid_attack (p) (input) =
  match p.attacks with
  | [] -> false
  | (s,i)::t -> if s = input then true
                else valid_attack {p with attacks=t} input

let wild_attack p =
  let attack_list = p.attacks in
  let index = Random.int (List.length attack_list) in
  List.nth attack_list index

let rec update_camldex_after_attack camldex p =
  match camldex with
  | [] -> []
  | h::t when h.name=p.name -> p::t
  | h::t -> h::(update_camldex_after_attack t p)

let rec battle (camldex : pokecaml list) (wild : pokecaml) (player: int)=
  (*TODO: Need to output that you're switching the pokecaml when current one dies
          Case insensitive user input*)
  let current_pokecaml = first_pokecaml camldex in
  if player = 0 then
    let () = print_string "It's your turn! What will you do?\n
                           Type \"catch\" to catch the wild pokecaml.\n
                           Type \"switch\" to switch your pokecaml.\n
                           Or, you can type any of your attacks:\n" in
    let () = print_attacks current_pokecaml.attacks in
    let input = read_line () in
    if input = "switch" then
      battle (switch camldex) wild 1
    else if input = "catch" then
      if (catch wild) = true then
        let () = print_endline ("You successfully caught " ^ wild.name ^ "!") in
        update_camldex_after_catch camldex wild
      else let () =print_string("Catch did not work\n") in battle camldex wild 1
    else
      if (valid_attack current_pokecaml input) then
        let a = get_attack current_pokecaml input in
        let wild = attack current_pokecaml a wild in
        let () = print_string ("You attacked with " ^ input ^ "\n") in
        let ()=print_string(wild.name^"'s HP is now "^string_of_int(wild.hp)) in
        if has_fainted wild then
          let () = print_endline ("You defeated " ^ wild.name ^ "!") in camldex
        else let () = print_newline () in battle camldex wild 1
      else
        let () = print_string "You did not enter a valid input.\n" in
        battle camldex wild 0
  else
    let opponent_attack = wild_attack wild in
    let current_pokecaml = attack wild opponent_attack current_pokecaml in
    let ()=print_string (wild.name ^" attacked with "^ (fst opponent_attack)) in
    let () = print_newline () in
    let () = print_endline (current_pokecaml.name ^ "'s HP is now " ^
                            string_of_int(current_pokecaml.hp)) in
    let camldex = update_camldex_after_attack camldex current_pokecaml in
    if has_fainted current_pokecaml then
      let () = print_endline (current_pokecaml.name^" fainted!") in
      if all_fainted camldex then
        let () = print_endline ("All of your pokecaml fainted...GAMEOVER") in
        exit 0
      else
        let () = print_endline "Switch pokecaml..." in
        let () = print_newline() in battle (switch camldex) wild 0
    else
      let () = print_newline() in battle camldex wild 0

let run_wild (camldex : pokecaml list) : pokecaml list =
  let length_pokecamls = List.length all_pokecaml in
  let random_int = Random.int (length_pokecamls) in
  let wild = List.nth all_pokecaml random_int in
  let () = print_endline ("A wild " ^ wild.name ^ " appeared!") in
  battle camldex wild 0