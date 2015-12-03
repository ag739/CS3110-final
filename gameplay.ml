open Pokecaml
open Wild_pokecaml_battle
open Trainer_battle

type command = Quit | Camldex | Help | Battle | Heal | Undetermined

let all_caught (camldex : pokecaml list): bool =
  (List.length camldex) = (List.length all_pokecaml)

let find_command (input : string) : command =
  match (String.lowercase input) with
  | "quit" -> Quit
  | "heal pokecaml" -> Heal
  | "camldex" -> Camldex
  | "help" -> Help
  | "battle" -> Battle
  | _ -> Undetermined

let rec pokecaml_moves (moves : (string*int) list) : string =
  match moves with
  | [] -> ""
  | h::t -> ((fst h)^"; ")^(pokecaml_moves t)

let print_type (poke_type: p_type) : string =
  match poke_type with
  | Hardware -> "Hardware"
  | Software -> "Software"
  | Humanities -> "Humanities"

let rec print_camldex (camldex: pokecaml list) : string =
  match camldex with
  | [] -> ""
  | h::t -> "Name: "^h.name^"; Type: "^(print_type h.pokecaml_type)^
            "; Moves: "^(pokecaml_moves (h.attacks))^"\n\n"^(print_camldex t)

let rec quitting (input : string) : bool =
  if input = "n" then false else
  if input = "y" then
    let () = print_endline "I guess you won't be the next Pokecaml master...goodbye!"
    in true
  else let () = print_endline "Please enter Y or N" in
  quitting (String.lowercase (read_line ()))

let rec get_list_index (lst : pokecaml list) (item : pokecaml) (ind : int)
                        : int =
  match lst with
  | [] -> failwith "item not in list"
  | h::t -> if h.name = item.name then ind else get_list_index t item (ind+1)

let rec heal_all (camldex : pokecaml list) : pokecaml list =
  match camldex with
  | [] -> []
  | h::t -> let index = get_list_index all_pokecaml h 0 in
            let f = fun x -> List.nth all_pokecaml index = x in
            let p = List.find f all_pokecaml in p::(heal_all t)

let you_won_msg =
  "You wanna be the very best, like no one ever was.
  To catch them was your real test, to save us is your cause.
  You traveled across Gates Hall, searching far and wide.
  Each Pokecaml to understand, the functional power inside!
  (Pokecaml gotta catch them all)
  It's you and OCaml, you know it's your destiny!
  Pokecaml, oh, they're your best friend in a world of programming languages.
  Pokecaml (gotta catch em all),
  Elegance so true, immutability will pull you through!
  3110 trained me well...PO-KE-CAML, GOTTA CATCH EM ALL!\n
  So, congrats on being a pokecaml master! GAMEOVER."

let rec game (camldex: pokecaml list) : unit =
  let _ = Random.self_init () in
  if all_caught camldex
    then print_endline you_won_msg
  else
  let () = print_string ">>> " in
  let input = read_line () in
  match find_command input with
  | Quit -> let () = print_endline "Are you sure you want to quit? Y/N" in
            let input = String.lowercase (read_line ()) in
            if quitting input then exit 0 else game camldex
  | Camldex -> let () = print_endline (print_camldex camldex) in game camldex
  | Heal -> let () = print_endline "Successfully healed all pokecaml in camldex!" in
            game (heal_all camldex)
  | Help -> let () = print_endline "Possible commands are:\n
                   \"Battle\" to fight an opponent\n
                   \"Camldex\" to see your Camldex\n
                   \"Heal Pokecaml\" to heal all of your pokecaml\n
                   \"Quit\" to end the game forever" in game camldex
  | Battle -> let camldex =
            (if Random.int 2 = 0 then run_wild camldex else run_trainer camldex) in game camldex
  | Undetermined -> let () =
      print_endline "Your command was not recognized. Please type a valid command or type help."
      in game camldex

let rec first_camldex (input : string) : pokecaml list =
  match (String.lowercase input) with
  | "recursee" -> let () = print_endline "You have picked Recursee!" in
                  let () = print_endline "You're ready to start your journey!
                  Type \"help\" if you forgot the commands I taught you." in
                  [find_by_name all_pokecaml "Recursee"]
  | "deferredata" -> let () = print_endline "You have picked Deferredata!" in
                  let () = print_endline "You're ready to start your journey!
                  Type \"help\" if you forgot the commands I taught you." in
                  [find_by_name all_pokecaml "Deferredata"]
  | "proofle" -> let () = print_endline "You have picked Proofle!" in
                  let () = print_endline "You're ready to start your journey!" in
                  [find_by_name all_pokecaml "Proofle"]
  | _ -> let () = print_string "Please try again, Professor Michael \"Oak\" Clarkson does not have that Pokecaml!\n\n>>> " in first_camldex (read_line ())

let intro_string =
  "  Hello! I've been waiting for you.\n  ...
  Oh, my name? Professor Michael \"Oak\" Clarkson. I'm assuming you've heard
  about the...situation.\n  ...\n  No? Oh, do we have some catching up to do!
  Basically, one of the project team's robotic PokeCaml experiments have gone
  horribly wrong, so Pokecaml have gone rogue and are all over Gates Hall.
  The project team members decided to go with it, and have also gone rogue,
  collecting Pokecaml, and in general harassing the student population.
  Some may say that Cornell is now scarier than having three back to back
  prelims during Fall Break. I believe you can be the very best functional
  Pokecaml trainer; you are one of my top students in CS3110, after all.
  I'll give you unlimited CamlBalls and your first Pokecaml.
  I'll be here to help you out. Remember the commands I taught you in class:
                   \"Battle\" to fight an opponent
                   \"Camldex\" to see your Camldex
                   \"Heal Pokecaml\" to heal all of your pokecaml
                   \"Quit\" to end the game forever
  I have three Pokecaml, but I can only give you one:
                  Recursee, a Software Pokecaml
                  Deferredata, a Hardware Pokecaml
                  Proofle, a Humanities Pokecaml
  Which do you choose? \n>>> "