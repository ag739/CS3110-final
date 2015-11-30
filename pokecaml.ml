type p_type = Hardware | Software | Humanities

type pokecaml = { name: string;
                  attacks: (string*int) list;
                  pokecaml_type: p_type;
                  hp: int
                }

let camldex = []

let all_pokecaml=[{name = "Camlchu"; attacks= [("electrocute" ,8)];
                    pokecaml_type= Hardware; hp= 100};
                  {name = "Piazza"; attacks= [("question", 3)];
                    pokecaml_type= Humanities; hp= 100};
                  {name = "Immutabilitypuff"; attacks= [("pattern match", 10);
                    ("infinite recursion", 2)]; pokecaml_type= Software; hp= 100};
                  {name = "Recursee"; attacks= [("Base case", 8); ("Rec", 12);
                    ("Return", 7); ("Tail-recursion", 10)];
                    pokecaml_type = Software; hp= 100};
                  {name = "Deferredata"; attacks= [("Bind", 6); ("Upon", 4);
                    ("Return", 7); (">>=", 12)]; pokecaml_type = Hardware; hp= 100};
                  {name = "Proofle"; attacks= [("Induction", 10);
                    ("Equivalence", 8); ("Math", 7); ("Specify", 11)];
                    pokecaml_type = Humanities; hp= 100}]

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

let rec new_lst item lst original_lst=
  match lst with
  | [] -> []
  | h::t -> if String.lowercase (h.name) = String.lowercase (item) then
              if h.hp = 0 then
                let () = print_endline "This pokecaml has fainted. Pick a different one\n>>> " in
                new_lst (read_line()) original_lst original_lst
              else h::t
            else (new_lst item t original_lst)@[h]

let rec all_names lst =
  match lst with
  | [] -> ()
  | h::t -> print_endline ("\""^h.name^"\""^"; hp: "^string_of_int(h.hp));
                all_names t

let switch (camldex : pokecaml list) =
  (*TODO: check if user put in valid pokecaml*)
  let () = print_endline "These are the pokecaml in your camldex:" in
  let () = all_names camldex in
  let () = print_string ">>> " in
  new_lst (read_line ()) camldex camldex