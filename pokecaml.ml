type p_type = Hardware | Software | Humanities

type pokecaml = { name: string;
                  attacks: (string*int) list;
                  pokecaml_type: p_type;
                  hp: int
                }

let file = Yojson.Basic.from_file (Sys.argv.(1))

let top_level_string_extract (t : Yojson.Basic.json) (key : string)
                             : string list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "pokecaml"
  |> flatten
  |> filter_member key
  |> filter_string

let top_level_int_extract (t : Yojson.Basic.json) (key : string) : int list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "pokecaml"
  |> flatten
  |> filter_member key
  |> filter_int

let top_level_list_extract (t : Yojson.Basic.json) (key : string) =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "pokecaml"
  |> flatten
  |> filter_member key
  |> filter_list

let pokecaml_names = top_level_string_extract file "name"

let pokecaml_p_types = top_level_string_extract file "p_type"

let pokecaml_hps = top_level_int_extract file "HP"

let pokecaml_attacks = top_level_list_extract file "attacks"

let pokecaml_attack_names =
  let open Yojson.Basic.Util in List.map
  (fun file -> filter_member "attack" file |> filter_string) pokecaml_attacks

let pokecaml_attack_damages =
  let open Yojson.Basic.Util in List.map
  (fun file -> filter_member "damage" file |> filter_int) pokecaml_attacks

let rec make_pairs (x : string list) (y : int list) : (string * int) list =
  match x, y with
  | [] , [] -> []
  | h1::t1, h2::t2 -> (h1,h2)::make_pairs t1 t2
  | _, _ -> failwith "improperly formatted json attacks"

let rec find_by_name (user_list : pokecaml list) (name : string) : pokecaml =
  match user_list with
  | [] -> failwith "Pokecaml does not exist"
  | h::t -> if h.name = name then h else find_by_name t name

let get_attack_pair (index : int) : (string * int) list =
  match (List.nth pokecaml_attack_names index),
  (List.nth pokecaml_attack_damages index) with
  | x , y -> make_pairs x y
  (*((List.nth pokecaml_attack_names index), (List.nth pokecaml_attack_damages index))*)

let pokecaml_record (index : int) : pokecaml=
  { name = List.nth pokecaml_names index;
    attacks = get_attack_pair index;
    pokecaml_type = (match List.nth pokecaml_p_types index with
                    | "Hardware" -> Hardware
                    | "Software" -> Software
                    | "Humanities" -> Humanities
                    | _ -> failwith "improperly formatted p_type in json");
    hp = List.nth pokecaml_hps index;}

let rec all_pokecaml_generator (lst : string list) (start_int : int)
                               : pokecaml list=
  match lst with
  | [] -> []
  | h::t -> pokecaml_record start_int::all_pokecaml_generator t (start_int + 1)

let all_pokecaml= all_pokecaml_generator pokecaml_names 0

let all_fainted (user_list : pokecaml list) : bool =
  let rec helper c = (match c with
  | [] -> 0
  | h::t -> h.hp + helper t
  ) in helper user_list = 0

let has_fainted (p : pokecaml) : bool =
  p.hp = 0

let rec first_pokecaml (user_list : pokecaml list) : pokecaml =
  match user_list with
  | [] -> failwith "This should not happen because we check if you lost"
  | h::t -> if h.hp > 0 then h else first_pokecaml t

let rec print_attacks (attacks : (string * int) list) : unit =
  match attacks with
  | [] -> print_newline ()
  | (a,_)::t -> let () = print_string (a ^ "\n") in print_attacks t

let compare_types (p1 : pokecaml) (p2 : pokecaml) : int =
  let p1_type = p1.pokecaml_type in
  let p2_type = p2.pokecaml_type in
  let () = Random.self_init () in
  let change = Random.int 3 in
  let num =
  match p1_type, p2_type with
  | (Software, Software) -> 3
  | (Software, Hardware) -> 4
  | (Software, Humanities) -> 5
  | (Hardware, Software) -> 2
  | (Hardware, Hardware) -> 3
  | (Hardware, Humanities) -> 4
  | (Humanities, Humanities) -> 3
  | (Humanities, Hardware) -> 2
  | (Humanities, Software) -> 1 in
  if change = 2 then num + 1 else if change = 0 then num - 1 else num

let attack (p1 : pokecaml) (a : (string * int) ) (p2 : pokecaml) : pokecaml =
  let damage = (compare_types p1 p2) * (snd a) in
  let current_hp = p2.hp in
  let new_hp = current_hp - damage in
  if new_hp < 0 then {p2 with hp = 0} else
  {p2 with hp = new_hp}

let rec remove (lst : pokecaml list) (x : pokecaml) : pokecaml list =
  match lst with
  | [] -> []
  | h::t -> if h = x then t else h::(remove t x)

let rec new_list (name : string) (user_list : pokecaml list) : pokecaml list =
  let pokecaml =
    List.filter (fun x -> String.lowercase (x.name) =
    String.lowercase (name)) user_list in
  match pokecaml with
  | [] -> let () = print_string
            "You don't have this pokecaml in your camldex.\n>>> " in
          new_list (read_line ()) user_list
  | h::t -> if String.lowercase (h.name) = String.lowercase (name) then
              if h.hp = 0 then
                let () = print_string
                  "This pokecaml has fainted. It cannot be used.\n>>> " in
                new_list (read_line ()) user_list
              else
                let () = print_endline ("You are now using " ^ h.name ^"\n") in
                h::(remove user_list h)
            else failwith "camldex should be a set"


let rec all_names (lst : pokecaml list) : unit =
  match lst with
  | [] -> ()
  | h::t -> print_endline ("\""^h.name^"\""^"; hp: "^string_of_int(h.hp));
                all_names t

let switch (user_list : pokecaml list) : pokecaml list =
  let () = print_endline "\nThese are the pokecaml in your camldex:" in
  let () = all_names user_list in
  let () = print_string "\n>>> " in
  new_list (read_line ()) user_list

let rec get_attack (p: pokecaml) (a : string) : (string * int) =
  match (p.attacks) with
  | [] -> failwith "Empty list"
  | (s,i)::t -> if String.lowercase s = a then (s,i)
                else get_attack {p with attacks=t} a

let rec valid_attack (p: pokecaml) (input : string) : bool =
  match p.attacks with
  | [] -> false
  | (s,i)::t -> if (String.lowercase s) = input then true
                else valid_attack {p with attacks=t} input

let rec update_camldex_after_attack (user_list : pokecaml list) (p : pokecaml)
                                    : pokecaml list =
  match user_list with
  | [] -> []
  | h::t when h.name=p.name -> p::t
  | h::t -> h::(update_camldex_after_attack t p)