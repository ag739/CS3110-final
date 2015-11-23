open Pokecaml

type command = Quit | Camldex | Help | Battle | Undetermined

let find_command (input : string) : command =
  match (String.lowercase input) with
  | "quit" -> Quit
  | "camldex" -> Camldex
  | "help" -> Help
  | "battle" -> Battle
  | _ -> Undetermined

let rec pokecaml_moves (moves : (string*int) list) =
  match moves with
  | [] -> ""
  | h::t -> ((fst h)^"; ")^(pokecaml_moves t)

let print_type (poke_type: p_type) =
  match poke_type with
  | Hardware -> "Hardware"
  | Software -> "Software"
  | Humanities -> "Humanities"

let rec print_camldex (camldex: pokecaml list) : string =
  match camldex with
  | [] -> ""
  | h::t -> "Name: "^h.name^"; Type: "^(print_type h.pokecaml_type)^
            "; Moves: "^(pokecaml_moves (h.attacks))^"\n\n"^(print_camldex t)

let rec game (camldex: pokecaml list) : unit =
  let () = print_string ">>> " in
  let input = read_line () in
  match find_command input with
  | Quit -> let () = print_endline "Goodbye!" in exit 0
  | Camldex -> let () = print_endline (print_camldex camldex) in game camldex
  | Help -> let () = print_endline "Possible commands are:\n
                   \"Battle\" to fight an opponent\n
                   \"Camldex\" to see your Camldex\n
                   \"Quit\" to end the game forever" in game camldex
  | Battle -> failwith "battle unimplemented"
  | Undetermined -> let () =
      print_endline "Your command was not recognized. Please type a valid command or type help."
      in game camldex


let () =
  let () = print_endline "Welcome to Pokecaml! Catchy text goes here!!" in
  game []