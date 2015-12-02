type p_type = Hardware | Software | Humanities

type pokecaml = { name: string;
                  attacks: (string*int) list;
                  pokecaml_type: p_type;
                  hp: int
                }

let file = Yojson.Basic.from_file (Sys.argv.(1))

let top_level_string_extract (t : Yojson.Basic.json) (key : string) : string list =
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

let rec make_pairs x y =
  match x, y with
  | [] , [] -> []
  | h1::t1, h2::t2 -> (h1,h2)::make_pairs t1 t2
  | _, _ -> failwith "improperly formatted json attacks"

let rec find_by_name lst name =
  match lst with
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

let rec all_pokecaml_generator lst start_int : pokecaml list=
  match lst with
  | [] -> []
  | h::t -> pokecaml_record start_int::all_pokecaml_generator t (start_int + 1)

let camldex = []

let all_pokecaml= all_pokecaml_generator pokecaml_names 0

let all_fainted (camldex : pokecaml list) : bool =
  let rec helper c = (match c with
  | [] -> 0
  | h::t -> h.hp + helper t
  ) in helper camldex = 0

let has_fainted (p : pokecaml) : bool =
  p.hp = 0

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

let attack (p1 : pokecaml) (a : (string * int) ) (p2 : pokecaml) : pokecaml =
  let damage = (compare_types p1 p2) * (snd a) in
  let current_hp = p2.hp in
  let new_hp = current_hp - damage in
  if new_hp < 0 then {p2 with hp = 0} else
  {p2 with hp = new_hp}

let rec remove lst x =
  match lst with
  | [] -> []
  | h::t -> if h = x then t else h::(remove t x)


(*TODO: Make this work recursively so you can choose again*)
(*let rec new_lst item lst original_lst=
  match lst with
  | [] -> let () = print_string "You don't have this pokecaml in your camldex.\n>>> " in
          []
  | h::t -> if String.lowercase (h.name) = String.lowercase (item) then
              if h.hp = 0 then
                let () = print_string "This pokecaml has fainted. It cannot be used.\n>>> " in
                []
              else let() = print_endline "wooooo" in h::(List.)
            else h::(new_lst item t original_lst)*)

let rec new_list name lst =
  let pokecaml = List.filter (fun x -> String.lowercase (x.name) = String.lowercase (name)) lst in
  match pokecaml with
  | [] -> let () = print_string "You don't have this pokecaml in your camldex.\n>>> " in
          new_list (read_line ()) lst
  | h::t -> if String.lowercase (h.name) = String.lowercase (name) then
              if h.hp = 0 then
                let () = print_string "This pokecaml has fainted. It cannot be used.\n>>> " in
                new_list (read_line ()) lst
              else h::(remove lst h)
            else failwith "camldex should be a set"


let rec all_names lst =
  match lst with
  | [] -> ()
  | h::t -> print_endline ("\""^h.name^"\""^"; hp: "^string_of_int(h.hp));
                all_names t

let switch (camldex : pokecaml list) =
  let () = print_endline "These are the pokecaml in your camldex:" in
  let () = all_names camldex in
  let () = print_string ">>> " in
  new_list (read_line ()) camldex
