open Printing
open Gameplay
open Async.Std


let _ = upon (tell_story intro_string)
  (fun _ -> game (first_camldex ()))

let _ = Scheduler.go ()