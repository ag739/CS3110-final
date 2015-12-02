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

(** A battle REPL to handle input and return output.
  * Takes as input the CamlDex (p1) and the opponents pokecaml list (p2) *)
let battle (p1: pokecaml list) (p2: pokecaml list) : pokecaml list =
  (*TODO implement*)
  if all_fainted p1 then
    let () = print_string "All of your pokecaml fainted...GAMEOVER" in p1
  else if all_fainted p2 then
    let () = print_endline "You defeated this trainer!" in p1
  else let () = print_endline "This hasn't been implemented yet" in p1

let run_trainer (camldex : pokecaml list) : pokecaml list =
  let length_trainers = List.length all_trainers in
  let random_int = Random.int (length_trainers) in
  let trainer = List.nth all_trainers random_int in
  let () = print_endline ("Trainer " ^ trainer.tname ^ " appeared!") in
  let () = print_endline trainer.intro in
  battle camldex trainer.poke_list