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

let trainer_record (index : int) =
  { tname = List.nth trainer_names index;
    poke_list = trainer_pokecaml_record_list (List.nth trainer_pokecaml_names index);
    intro = List.nth trainer_intros index; }

let rec all_trainer_generator lst start_int : trainer list=
  match lst with
  | [] -> []
  | h::t -> trainer_record start_int::all_trainer_generator t (start_int + 1)

let all_trainers= all_trainer_generator trainer_names 0

let sort_attacks lst =
  List.sort (fun x y -> compare (snd x) (snd y)) lst

let determine_attack lst =
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
let perform_user_attack (input : string) (player : pokecaml)(opponent : pokecaml)
                            (p_list : pokecaml list) (o_list : pokecaml list) =
  (*TODO:
   * check if input was a valid attack and get the attack pair
   * perform attack on opponent; print that you printed the opponent and their new hp
   * check if opponent fainted; if yes, print that you defeated the opponent and switch to next pokecaml
   * check if all of the opponent's pokecaml have fainted; if yes you won
  *)
  p_list

let rec battle (p1: pokecaml list) (p2: pokecaml list) (player: int): pokecaml list =
  if all_fainted p1 then
    let () = print_string "All of your pokecaml fainted...GAMEOVER" in p1
  else if all_fainted p2 then
    let () = print_endline "You defeated this trainer!" in p1
  else
    let my_p = first_pokecaml p1 in
    let trainer_p = first_pokecaml p2 in
    if player = 0 then
      let () = print_string "It's your turn! What will you do?\n
                            Type \"switch\" to switch your pokecaml.\n
                            Or, you can type any of your attacks:\n" in
      let () = print_attacks my_p.attacks in
      let () = print_string ">>> " in
      let input = String.lowercase (read_line ()) in
      match input with
      | "switch" -> battle (switch p1) p2 1
      | _ -> let () = print_endline "TODO: perform user attack" in
              let p1 = perform_user_attack input my_p trainer_p p1 p2 in
              battle p1 p2 1
    else
      let () = print_endline ("It's the trainers turn with pokecaml "^trainer_p.name) in
      let () = print_endline "TODO: implement trainer logic" in p1

let run_trainer (camldex : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.tname ^ " appeared!") in
  let () = print_endline trainer.intro in
  battle camldex trainer.poke_list 0