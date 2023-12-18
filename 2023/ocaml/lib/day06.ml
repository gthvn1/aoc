(** [distanceTraveled rd hold] returns the distance traveled in millimeter for a race
    duration of [rd] while holding the button during [hold] milliseconds. *)
let distanceTraveled rd = function
  | hold when hold >= rd -> 0
  | hold when hold = 0 -> 0
  | hold as speed ->
      let remaining_time = rd - hold in
      remaining_time * speed
(* just for clarity we use speed *)

(** [getAllDistance rd] returns a list of all distances traveled for a given race
    duration [rd] while holding button from 0 to [rd] milliseconds *)
let getAllDistance rd =
  let rec getAllDistance' = function
    | 0 -> 0 :: [] (* we could skip the hold of 0 ms but let's keep it *)
    | hold -> distanceTraveled rd hold :: getAllDistance' (hold - 1)
  in
  getAllDistance' rd |> List.rev
(* we reverse the list but we could have do it in right order but it adds
   a comparaison in the math. Not sure what it is better... *)

(** [waysToBeatRecord rd record] returns the number of ways to beat the
    [record] for a given race duration [rd].  *)
let waysToBeatRecord rd record =
  getAllDistance rd |> List.filter (fun v -> v > record) |> List.length

(** [beatRecordEachRace lr ld] will return the sum of the number of ways we can
    beat the record from list of record [ld] and list of distance [ld].
    [lr] and [ld] must have the same lenght. *)
let rec beatRecodeEachRace (lr : int list) (ld : int list) : int =
  match (lr, ld) with
  | [], [] -> 1
  | [], _ | _, [] -> failwith "not the same lenght"
  | r :: xr, d :: xd -> waysToBeatRecord r d * beatRecodeEachRace xr xd

(** inputs *)
let sample = [ "Time:      7  15   30"; "Distance:  9  40  200" ]

let part1 _ =
  beatRecodeEachRace [ 59; 79; 65; 75 ] [ 597; 1234; 1032; 1328 ]
  |> string_of_int

let part2 _ =
  (* use tail recursion to avoid stack overflow during the evaluation *)
  let race_time = 59796575 in
  let distance = 597123410321328 in
  let rec waysToBeat acc hold =
    if hold = race_time then acc
    else
      let d = distanceTraveled race_time hold in
      let new_acc = if d > distance then acc + 1 else acc in
      waysToBeat new_acc (hold + 1)
  in
  waysToBeat 0 0 |> string_of_int
