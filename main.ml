open Pokecaml
open Wild_pokecaml_battle
open Trainer_battle

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

let rec quitting (input : string) : bool =
  if input = "n" then false else
  if input = "y" then
    let () = print_endline "I guess you won't be the next Pokecaml master...goodbye!"
    in true
  else let () = print_endline "Please enter Y or N" in
  quitting (String.lowercase (read_line ()))

let rec game (camldex: pokecaml list) : unit =
  let () = print_string ">>> " in
  let input = read_line () in
  match find_command input with
  | Quit -> let () = print_endline "Are you sure you want to quit? Y/N" in
            let input = String.lowercase (read_line ()) in
            if quitting input then exit 0 else game camldex
  | Camldex -> let () = print_endline (print_camldex camldex) in game camldex
  | Help -> let () = print_endline "Possible commands are:\n
                   \"Battle\" to fight an opponent\n
                   \"Camldex\" to see your Camldex\n
                   \"Quit\" to end the game forever" in game camldex
  | Battle -> let camldex =
            (if Random.int 2 = 0 then run_wild camldex else run_trainer camldex) in game camldex
  | Undetermined -> let () =
      print_endline "Your command was not recognized. Please type a valid command or type help."
      in game camldex

let rec first_camldex input =
  match (String.lowercase input) with
  | "recursee" -> let () = print_endline "You have picked Recursee!" in
                  [{name = "Recursee"; attacks= [("Base case", 8); ("Rec", 12);
                  ("Return", 7); ("Tail-recursion", 10)];
                  pokecaml_type = Software; hp= 100}]
  | "deferredata" -> let () = print_endline "You have picked Deferredata!" in
                  [{name = "Deferredata"; attacks= [("Bind", 6); ("Upon", 4);
                  ("Return", 7); (">>=", 12)];
                  pokecaml_type = Hardware; hp= 100}]
  | "proofle" -> let () = print_endline "You have picked Proofle!" in
                  [{name = "Proofle"; attacks= [("Induction", 10);
                  ("Equivalence", 8); ("Math", 7); ("Specify", 11)];
                  pokecaml_type = Humanities; hp= 100}]
  | _ -> let () = print_string "Please try again, Professor Michael \"Oak\" Clarkson does not have that Pokecaml!\n\n>>> " in first_camldex (read_line ())

let () =
  let () = print_string "  Hello! I've been waiting for you.\n  ...
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
                   \"Quit\" to end the game forever
  I have three Pokecaml, but I can only give you one:
                  Recursee, a Software Pokecaml
                  Deferredata, a Hardware Pokecaml
                  Proofle, a Humanities Pokecaml
  Which do you choose? \n>>> " in
  game (first_camldex (read_line ()))