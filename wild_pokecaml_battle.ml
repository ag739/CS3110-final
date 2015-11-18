open Pokecaml

let all_caught (camldex : pokecaml list): bool =
  (List.length camldex) = (List.length all_pokecaml)

let all_fainted (camldex : pokecaml list) : bool =
  let rec check_hp c =
    (match c with
    | [] -> 0
    | h::t -> h.hp + (check_hp t)) in
  (check_hp camldex) = 0

let has_won (wild : pokecaml) : bool =
  wild.hp = 0

let catch (wild : pokecaml) : bool =
  if wild.hp > 15 then false else true

let attack (p1 : pokecaml) (p2 : pokecaml) : pokecaml =
  failwith "TODO3"

let battle (camldex : pokecaml list) (wild : pokecaml) : unit=
  failwith "TODO4"