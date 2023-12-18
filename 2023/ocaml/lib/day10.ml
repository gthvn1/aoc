(*

== GRID
A grid looks like this:

     ..F7.
     .FJ|.
     SJ.L7
     |F--J
     LJ...

- top left is (0,0)
- bottom right is (4,4)

== RULES
 - | is a vertical pipe connecting north and south.
 - - is a horizontal pipe connecting east and west.
 - L is a 90-degree bend connecting north and east.
 - J is a 90-degree bend connecting north and west.
 - 7 is a 90-degree bend connecting south and west.
 - F is a 90-degree bend connecting south and east.
 - . is ground; there is no pipe in this tile.
 - S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
*)

type direction = North | South | East | West
type color = Red | Green | Blue

(* Green is on the path, Red is on the right and Blue on the left *)
type tile = char * color option
type grid = tile array array

(** [print_grid g] prints the grid [g] to stdout. *)
let print_grid (g : grid) : unit =
  for y = 0 to Array.length g.(0) - 1 do
    for x = 0 to Array.length g - 1 do
      let tile = fst g.(x).(y) in
      let style =
        if tile = 'S' then [ ANSITerminal.Background ANSITerminal.Yellow ]
        else
          match snd g.(x).(y) with
          | Some Green -> [ ANSITerminal.Background ANSITerminal.Green ]
          | Some Red -> [ ANSITerminal.Background ANSITerminal.Red ]
          | Some Blue -> [ ANSITerminal.Background ANSITerminal.Blue ]
          | _ -> [ ANSITerminal.Background ANSITerminal.Black ]
      in
      ANSITerminal.printf style "%c" tile
    done;
    print_newline ()
  done

(** [is_in_the_grid pos g] returns true if the position is in the grid *)
let is_in_the_grid pos g =
  let dimx = Array.length g in
  let dimy = Array.length g.(0) in
  let x, y = pos in
  x >= 0 && x < dimx && y >= 0 && y < dimy

(** [get_symbol_opt pos g] returns the character at position [pos] if it is in the grid [g]. *)
let get_symbol_opt (pos : int * int) (g : grid) : char option =
  let x, y = pos in
  if is_in_the_grid pos g then Some (fst g.(x).(y)) else None

(** [set_tile_color c pos g] set the tile as color [c] if it is inside the grid and if it 
    has no color yet. It it is Green or has the same color it is not an error we just don't
    do anything. It we are trying to change Red to Blue or Blue to Red we are in trouble because
    a node cannot be inside and outside the maze... *)
let set_tile_color (c : color) (pos : int * int) (g : grid) : bool =
  let x, y = pos in
  if is_in_the_grid pos g then (
    let symbol = fst g.(x).(y) in
    match snd g.(x).(y) with
    | None ->
        g.(x).(y) <- (symbol, Some c);
        true
    | Some Green -> false (* never update Green color *)
    | Some c' ->
        if c != c' && symbol = '.' then (
          print_grid g;
          Printf.printf "%d, %d is %s, cannot color dot to %s\n" x y
            (match c' with Red -> "Red" | Blue -> "Blue" | Green -> "Green")
            (match c with Red -> "Red" | Blue -> "Blue" | Green -> "Green");
          failwith "Find a leak")
        else g.(x).(y) <- (symbol, Some c);
        false)
  else false

(** [set_dot_color c pos g] set the tile to color [c] only if it is a dot *)
let set_dot_color (c : color) (pos : int * int) (g : grid) : bool =
  let x, y = pos in
  if is_in_the_grid pos g then
    match fst g.(x).(y) with '.' -> set_tile_color c pos g | _ -> false
  else false

(** [check_boundaries_opt pos d g] returns the position reached from [pos]
    when following the direction [d] on grid [g] if it is possible. *)
let check_boundaries_opt pos d g =
  let x, y = pos in
  let x', y' =
    match d with
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | East -> (x + 1, y)
    | West -> (x - 1, y)
  in
  if is_in_the_grid (x', y') g then Some (x', y') else None

(** [move_one_step_opt from pos g] tries to reach a new position from [pos]
    when coming from direction [from] in the grid [g]. And it also
    returns the direction where we come from. If we cannot move we return None.
    Example:
    - We are coming from east and we found J
      => We return south (because we are now going to north) and x, y +1
*)
let move_one_step_opt (coming_from : direction) (pos : int * int) (g : grid) :
    direction * (int * int) option =
  (* We can compute the next position for all directions now *)
  let move_north = check_boundaries_opt pos North g in
  let move_south = check_boundaries_opt pos South g in
  let move_east = check_boundaries_opt pos East g in
  let move_west = check_boundaries_opt pos West g in
  match get_symbol_opt pos g with
  | Some '|' -> (
      (* connecting north and south *)
      match coming_from with
      | South -> (South, move_north)
      | North -> (North, move_south)
      | _ as other -> (other, None))
  | Some '-' -> (
      (* connecting east to west *)
      match coming_from with
      | East -> (East, move_west)
      | West -> (West, move_east)
      | _ as other -> (other, None))
  | Some 'L' -> (
      (* L is a 90-degree bend connecting north and east.*)
      match coming_from with
      | North -> (West, move_east)
      | East -> (South, move_north)
      | _ as other -> (other, None))
  | Some 'J' -> (
      (* J is a 90-degree bend connecting north and west.*)
      match coming_from with
      | North -> (East, move_west)
      | West -> (South, move_north)
      | _ as other -> (other, None))
  | Some '7' -> (
      (* 7 is a 90-degree bend connecting south and west.*)
      match coming_from with
      | South -> (East, move_west)
      | West -> (North, move_south)
      | _ as other -> (other, None))
  | Some 'F' -> (
      (* F is a 90-degree bend connecting south and east.*)
      match coming_from with
      | South -> (West, move_east)
      | East -> (North, move_south)
      | _ as other -> (other, None))
  | _ ->
      (* We are considering Start like End so once reached we don't move like '.' . *)
      (coming_from, None)

(** [can_start_up s g] return position if we can go up from start [s]. *)
let can_start_up s g =
  let x, y = s in
  let up = (x, y - 1) in
  match get_symbol_opt up g with
  | Some '|' | Some '7' | Some 'F' -> Some up
  | _ -> None

(** [can_start_down s g] return position if we can go down from start [s]. *)
let can_start_down s g =
  let x, y = s in
  let down = (x, y + 1) in
  match get_symbol_opt down g with
  | Some '|' | Some 'J' | Some 'L' -> Some down
  | _ -> None

(** [can_start_left s g] return position if we can go left from start [s]. *)
let can_start_left s g =
  let x, y = s in
  let left = (x - 1, y) in
  match get_symbol_opt left g with
  | Some '-' | Some 'F' | Some 'L' -> Some left
  | _ -> None

(** [can_start_right s g] return position if we can go right from start [s]. *)
let can_start_right s g =
  let x, y = s in
  let right = (x + 1, y) in
  match get_symbol_opt right g with
  | Some '-' | Some '7' | Some 'J' -> Some right
  | _ -> None

(** [where_to_go_from_start start_pos g] returns the first position that is available from
    starting point [start_pos] and the direction we are coming from.
    Example:
      if from [start_pos] we can go up we will return (South, (x, y-1))
    If we cannot go somewhere we just return None. *)
let where_to_go_from_start start_pos g =
  let try_up = can_start_up start_pos g in
  let try_down = can_start_down start_pos g in
  let try_left = can_start_left start_pos g in
  let try_right = can_start_right start_pos g in
  match get_symbol_opt start_pos g with
  | Some 'S' ->
      if Option.is_some try_up then Some (South, Option.get try_up)
      else if Option.is_some try_down then Some (North, Option.get try_down)
      else if Option.is_some try_right then Some (West, Option.get try_right)
      else if Option.is_some try_left then Some (East, Option.get try_left)
      else None
  | _ -> None

(** [walk_to_dst coming_from pos g] returns the steps to reach the destination starting
    from [pos]. It also returns the symbol reached. During the walk we can start painting
    dots. *)
let walk_to_dst (coming_from : direction) (pos : int * int) (g : grid) :
    int * (int * int) =
  let rec aux steps dir p =
    let x, y = p in
    let symbol = fst g.(x).(y) in
    (* we update the tile to our right (it is a choice) as Red and to our left as
       Blue if it is a '.' before moving. *)
    let _ =
      match (dir, symbol) with
      | North, 'J' ->
          set_tile_color Red (x - 1, y) g
          || set_tile_color Blue (x + 1, y) g
          || set_tile_color Blue (x, y + 1) g
      | North, 'L' ->
          set_tile_color Red (x - 1, y) g
          || set_tile_color Blue (x + 1, y) g
          || set_tile_color Red (x, y + 1) g
      | North, _ ->
          set_tile_color Red (x - 1, y) g || set_tile_color Blue (x + 1, y) g
      | South, 'F' ->
          set_tile_color Red (x + 1, y) g
          || set_tile_color Blue (x - 1, y) g
          || set_tile_color Blue (x, y - 1) g
      | South, '7' ->
          set_tile_color Red (x + 1, y) g
          || set_tile_color Blue (x - 1, y) g
          || set_tile_color Red (x, y - 1) g
      | South, _ ->
          set_tile_color Red (x + 1, y) g || set_tile_color Blue (x - 1, y) g
      | East, 'L' ->
          set_tile_color Red (x, y - 1) g
          || set_tile_color Blue (x, y + 1) g
          || set_tile_color Blue (x - 1, y) g
      | East, 'F' ->
          set_tile_color Red (x, y - 1) g
          || set_tile_color Blue (x, y + 1) g
          || set_tile_color Red (x - 1, y) g
      | East, _ ->
          set_tile_color Red (x, y - 1) g || set_tile_color Blue (x, y + 1) g
      | West, '7' ->
          set_tile_color Red (x, y + 1) g
          || set_tile_color Blue (x, y - 1) g
          || set_tile_color Blue (x + 1, y) g
      | West, 'J' ->
          set_tile_color Red (x, y + 1) g
          || set_tile_color Blue (x, y - 1) g
          || set_tile_color Red (x + 1, y) g
      | West, _ ->
          set_tile_color Red (x, y + 1) g || set_tile_color Blue (x, y - 1) g
    in
    (* we update the current tile as in the path (aka Green) *)
    g.(x).(y) <- (symbol, Some Green);
    match move_one_step_opt dir p g with
    | _, None -> (steps, p)
    | new_dir, Some new_pos -> aux (steps + 1) new_dir new_pos
  in
  aux 1 coming_from pos (* we have already done the first step *)

(** [propagate_colors g ] propagates the red and blue colors. It returns true if a propagation
    occurs, false otherwise *)
let propagate_colors g : bool =
  let color (x, y) : bool =
    let up = (x, y - 1) in
    let down = (x, y + 1) in
    let left = (x - 1, y) in
    let right = (x + 1, y) in
    match snd g.(x).(y) with
    | Some Red ->
        set_tile_color Red up g || set_tile_color Red down g
        || set_tile_color Red left g || set_tile_color Red right g
    | Some Blue ->
        set_tile_color Blue up g || set_tile_color Blue down g
        || set_tile_color Blue left g
        || set_tile_color Blue right g
    | _ -> false
  in
  let updated = ref false in
  let _ =
    for y = 0 to Array.length g.(0) - 1 do
      for x = 0 to Array.length g - 1 do
        (* If we are red but one of our neighbour is black the we set it to red *)
        updated := !updated || color (x, y)
      done
    done
  in
  !updated

(** [get_dot_insides g] returns the number of red and black dots in the grid *)
let get_dot_colors g : int * int * int * int =
  (* Before counting the number of red tiles we need to
     propagate the information to the node that are red but
     connecting to a node that is still black. *)
  while propagate_colors g do
    ()
  done;
  let green = ref 0 in
  let red = ref 0 in
  let black = ref 0 in
  let nocolor = ref 0 in
  for y = 0 to Array.length g.(0) - 1 do
    for x = 0 to Array.length g - 1 do
      match snd g.(x).(y) with
      | Some Green -> green := !green + 1
      | Some Red -> red := !red + 1
      | Some Blue -> black := !black + 1
      | None -> nocolor := !nocolor + 1
    done
  done;
  (!nocolor, !green, !red, !black)

(** [init_grid input] returns a matrix initialized with strings find in [input]
    with a tuple that corresponds to the position of the start. *)
let init_grid (input : string list) : grid * (int * int) =
  let dimx = String.length @@ List.hd input in
  let dimy = List.length input in
  let start = ref (0, 0) in
  let g = Array.make_matrix dimx dimy ('.', None) in
  List.iteri
    (fun y line ->
      List.iteri
        (fun x c ->
          g.(x).(y) <- (c, None);
          if c = 'S' then start := (x, y) else ())
        (String.to_seq line |> List.of_seq))
    input;
  (g, !start)

let sample1 = [ "....."; ".S-7."; ".|.|."; ".L-J."; "....." ]
let sample2 = [ "..F7."; ".FJ|."; "SJ.L7"; "|F--J"; "LJ..." ]

let sample3 =
  [
    "...........";
    ".S-------7.";
    ".|F-----7|.";
    ".||.....||.";
    ".||.....||.";
    ".|L-7.F-J|.";
    ".|..|.|..|.";
    ".L--J.L--J.";
    "...........";
  ]

let sample4 =
  [
    ".F----7F7F7F7F-7....";
    ".|F--7||||||||FJ....";
    ".||.FJ||||||||L7....";
    "FJL7L7LJLJ||LJ.L-7..";
    "L--J.L7...LJS7F-7L7.";
    "....F-J..F7FJ|L7L7L7";
    "....L7.F7||L7|.L7L7|";
    ".....|FJLJ|FJ|F7|.LJ";
    "....FJL-7.||.||||...";
    "....L---J.LJ.LJLJ...";
  ]

let sample5 =
  [
    "FF7FSF7F7F7F7F7F---7";
    "L|LJ||||||||||||F--J";
    "FL-7LJLJ||||||LJL-77";
    "F--JF--7||LJLJ7F7FJ-";
    "L---JF-JLJ.||-FJLJJ7";
    "|F|F-JF---7F7-L7L|7|";
    "|FFJF7L7F-JF7|JL---7";
    "7-L-JL7||F7|L7F-7F7|";
    "L.L7LFJ|||||FJL7||LJ";
    "L7JLJL-JLJLJL--JLJ.L";
  ]

let part1 ls =
  let g, start = init_grid ls in
  let steps, _ =
    match where_to_go_from_start start g with
    | None -> failwith "Nowhere to go from start"
    | Some (coming_from, pos) -> walk_to_dst coming_from pos g
  in
  steps / 2 |> string_of_int

(**
  Try to use:
     - https://en.wikipedia.org/wiki/Pick's_theorem
     - https://fr.mathigon.org/task/picks-theorem
  But we don't find how to compute the area.

  So we come up with another maybe working algorithm:
  - When going along the path, a '.' on your left is inside or outside.
  - Let's consider it inside. In that case all '.' on your left are in.
  - And all '.' connected to an inside '.' is also in.
  - If will give us the number of '.' whithin the loop. And if our
    hypothesis is wrong then it will give us the number of '.' that are
    outside but as we know the number of dot we will be able to get the
    other one.
 *)

let part2 _ = "Found 15 solutions... not perfect"

(*
let part2 ls =
  let g, start = init_grid ls in
  let _ =
    (* mark start as green *)
    g.(fst start).(snd start) <- ('S', Some Green);
    match where_to_go_from_start start g with
    | None -> failwith "Nowhere to go from start"
    | Some (coming_from, pos) -> walk_to_dst coming_from pos g
  in
  let nocolor, green, red, black = get_dot_colors g in
  Printf.printf "day10: [debug] nocolor: %d green: %d  red: %d  black: %d\n"
    nocolor green red black;
  (* print_grid g; *)
  black |> string_of_int
*)
