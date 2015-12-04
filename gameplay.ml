open Pokecaml
open Wild_pokecaml_battle
open Trainer_battle

type command = Quit | Camldex | Help | Battle | Heal | Undetermined

let all_caught (user_list : pokecaml list): bool =
  (List.length user_list) = (List.length all_pokecaml)

let find_command (input : string) : command =
  match (String.lowercase input) with
  | "quit" -> Quit
  | "heal pokecaml" -> Heal
  | "camldex" -> Camldex
  | "help" -> Help
  | "battle" -> Battle
  | _ -> Undetermined

let rec pokecaml_moves (attacks : (string*int) list) : string =
  match attacks with
  | [] -> ""
  | h::t -> ((fst h)^"; ")^(pokecaml_moves t)

let print_type (poke_type: p_type) : string =
  match poke_type with
  | Hardware -> "Hardware"
  | Software -> "Software"
  | Humanities -> "Humanities"

let rec print_camldex (user_list: pokecaml list) : string =
  match user_list with
  | [] -> ""
  | h::t -> "Name: "^h.name^"; Type: "^(print_type h.pokecaml_type)^
            "; Moves: "^(pokecaml_moves (h.attacks))^"\n\n"^(print_camldex t)

let rec quitting (input : string) : bool =
  if input = "n" then false else
  if input = "y" then
    let () = print_endline
      "\nI guess you won't be the next Pokecaml master...goodbye!" in true
  else let () = print_endline "\nPlease enter Y or N\n>>> " in
  quitting (String.lowercase (read_line ()))

let rec get_list_index (user_list : pokecaml list) (item : pokecaml) (ind : int)
                        : int =
  match user_list with
  | [] -> failwith "item not in list"
  | h::t -> if h.name = item.name then ind else get_list_index t item (ind+1)

let rec heal_all (camldex : pokecaml list) : pokecaml list =
  match camldex with
  | [] -> []
  | h::t -> let index = get_list_index all_pokecaml h 0 in
            let f = fun x -> List.nth all_pokecaml index = x in
            let p = List.find f all_pokecaml in p::(heal_all t)

let you_won_msg =
  "\nYou wanna be the very best, like no one ever was."^
  "To catch them was your real test, to save us is your cause."^
  "You traveled across Gates Hall, searching far and wide."^
  "Each Pokecaml to understand, the functional power inside!"^
  "(Pokecaml gotta catch them all)"^
  "It's you and OCaml, you know it's your destiny!"^
  "Pokecaml, oh, they're your best friend in a world of programming languages."^
  "Pokecaml (gotta catch em all),"^
  "Elegance so true, immutability will pull you through!"^
  "3110 trained me well...PO-KE-CAML, GOTTA CATCH EM ALL!\n"^
  "So, congrats on being a pokecaml master! GAMEOVER.\n"

let rec game (user_list: pokecaml list) : unit =
  let _ = Random.self_init () in
  if all_caught user_list
    then print_endline you_won_msg
  else
    let () = print_string "\n>>> " in
    let input = read_line () in
    match find_command input with
    | Quit -> let () = print_string "\nAre you sure you want to quit? Y/N\n>>> "
              in let input = String.lowercase (read_line ()) in
              if quitting input then exit 0 else game user_list
    | Camldex -> let () = print_endline (print_camldex user_list) in
                  game user_list
    | Heal -> let () = print_endline ("\nSuccessfully healed all pokecaml in "^
                "camldex!") in
              game (heal_all user_list)
    | Help -> let () = print_endline "\nPossible commands are:\n
                     \"Battle\" to fight an opponent\n
                     \"Camldex\" to see your Camldex\n
                     \"Heal Pokecaml\" to heal all of your pokecaml\n
                     \"Quit\" to end the game forever" in game user_list
    | Battle -> let user_list =
              (if Random.int 2 = 0 then
                run_wild user_list
              else
                run_trainer user_list) in game user_list
    | Undetermined -> let () = print_endline (
                        "\nYour command was not recognized."^
                        " Please type a valid command or type help.")
                      in game user_list

let rec first_camldex () : pokecaml list =
  let input = read_line () in
  match (String.lowercase input) with
  | "recursee" -> let () = print_endline "\nYou have picked Recursee!" in
                  let () = print_endline "You're ready to start your journey!
                  Type \"help\" if you forgot the commands I taught you." in
                  [find_by_name all_pokecaml "Recursee"]
  | "deferredata" -> let () = print_endline "\nYou have picked Deferredata!" in
                  let () = print_endline "You're ready to start your journey!
                  Type \"help\" if you forgot the commands I taught you." in
                  [find_by_name all_pokecaml "Deferredata"]
  | "proofle" -> let () = print_endline "\nYou have picked Proofle!" in
                 let () = print_endline "You're ready to start your journey!" in
                 [find_by_name all_pokecaml "Proofle"]
  | _ -> let () = print_string (
                    "\nPlease try again, Professor Michael \"Oak\" "^
                    "Clarkson does not have that Pokecaml!\n\n>>> ")
                  in first_camldex ()

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
                   \"Help\" to view all options
                   \"Battle\" to fight an opponent
                   \"Camldex\" to see your current pokecaml list
                   \"Heal Pokecaml\" to heal all of your pokecaml
                   \"Quit\" to end the game forever
  I have three Pokecaml, but I can only give you one:
                  Recursee, a Software Pokecaml
                  Deferredata, a Hardware Pokecaml
                  Proofle, a Humanities Pokecaml
  Which do you choose? \n\n>>>  "