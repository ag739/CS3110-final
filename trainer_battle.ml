open Pokecaml

type trainer = {tname: string; poke_list: pokecaml list; intro: string}

let file = Yojson.Basic.from_file (Sys.argv.(1))

let trainer_string_extract (t : Yojson.Basic.json) (key : string) : string list =
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
    poke_list = trainer_pokecaml_record_list (List.nth trainer_pokecaml_names index);
    intro = List.nth trainer_intros index; }

let rec all_trainer_generator (lst : string list) (start_int : int)
                               : trainer list =
  match lst with
  | [] -> []
  | h::t -> trainer_record start_int::all_trainer_generator t (start_int + 1)

let all_trainers = all_trainer_generator trainer_names 0

let sort_attacks (lst : (string * int) list) : (string * int) list =
  List.sort (fun x y -> compare (snd x) (snd y)) lst

let determine_attack (lst : (string * int) list) : (string * int) =
  let rand = Random.int 15 in
  match sort_attacks lst with
  | [] -> failwith "Pokecaml's attacks list was empty!"
  | a::[] -> a
  | a::b::[] -> if rand < 9 then a else b
  | a::b::c::[] -> if rand < 8 then a else if rand < 12 then b else c
  | a::b::c::d::t -> if rand < 6 then a else if rand < 10 then b else if
                     rand < 13 then c else d

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex (p1) and the opponents pokecaml list (p2) *)
let rec perform_user_attack (input : string) (player : pokecaml)(opponent : pokecaml)
                        (p_list : pokecaml list) (o_list : pokecaml list) (turn: int)
                        : pokecaml list =
  if (valid_attack player input) then
    let a = get_attack player input in
    let opponent = attack player a opponent in
    let () = print_string ("You attacked with " ^ input ^ "\n") in
    let ()=print_endline(opponent.name^"'s HP is now "^string_of_int(opponent.hp)) in
    let () =
      (if has_fainted opponent then
        print_endline (opponent.name ^ " has fainted!")
      else print_newline ()) in
    let o_list = update_camldex_after_attack o_list opponent in
    if turn = 0 then battle p_list o_list 1 else battle p_list o_list 0
  else
    let () = print_string "You did not enter a valid input.\n" in
    battle p_list o_list turn

and battle (p1: pokecaml list) (p2: pokecaml list) (turn: int)
                : pokecaml list =
  if all_fainted p1 then
    let () = print_string "All of your pokecaml fainted...GAMEOVER" in p1
  else if all_fainted p2 then
    let () = print_endline "You defeated this trainer!" in p1
  else
    let my_p = first_pokecaml p1 in
    let trainer_p = first_pokecaml p2 in
    if turn = 0 then
      let () = print_string "It's your turn! What will you do?\n
                            Type \"switch\" to switch your pokecaml.\n
                            Or, you can type any of your attacks:\n" in
      let () = print_attacks my_p.attacks in
      let () = print_string ">>> " in
      let input = String.lowercase (read_line ()) in
      match input with
      | "switch" -> battle (switch p1) p2 1
      | _ -> let p1 = perform_user_attack input my_p trainer_p p1 p2 0 in
              battle p1 p2 1
    else
      let () = print_endline ("It's the trainers turn with pokecaml "^trainer_p.name) in
      let () = print_endline "TODO: implement trainer logic..." in
      battle p1 p2 0

let run_trainer (camldex : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.tname ^ " appeared!") in
  let () = print_endline trainer.intro in
  let trainer_pokecaml = first_pokecaml (trainer.poke_list) in
  let () = print_endline (trainer.tname ^ " is using " ^ trainer_pokecaml.name) in
  battle camldex trainer.poke_list 0