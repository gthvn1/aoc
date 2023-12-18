type image = string list

let rec print_image = function
  | [] -> print_newline ()
  | s :: xs ->
      print_endline s;
      print_image xs

(** [all_dots s] returns true if all characters if a given string [s]
      are dots. *)
let all_dots s = String.for_all (fun c -> c = '.') s

(** [all_dots_on_column im j] returns true if all characters of the
   given image [im] are all dots on column [j]. *)
let all_dots_on_column (im : image) (j : int) : bool =
  List.for_all (fun s -> s.[j] = '.') im

(** [get_expanded_cols im] returns a list of booleans that are set to yes
   if the column needs to be extended and false if not. *)
let get_expanded_cols (im : image) : bool list =
  let rec aux im j =
    if j = String.length (List.hd im) then []
    else all_dots_on_column im j :: aux im (j + 1)
  in
  aux im 0

(** [expand_row i] will add a new string if all characters are '.' *)
let rec expand_row_by (n : int) (im : image) : image =
  match im with
  | [] -> []
  | s :: xs ->
      let exts = if all_dots s then List.init n (fun _ -> s) else [ s ] in
      exts @ expand_row_by n xs

let expand_col_by (n : int) (im : image) : image =
  let need_expansion = get_expanded_cols im in
  let rec expand_with_cond (cl : char list) (need_exp : bool list) : char list =
    match cl with
    | [] -> []
    | c :: xs ->
        let new_c =
          if List.hd need_exp then List.init n (fun _ -> c) else [ c ]
        in
        new_c @ expand_with_cond xs (List.tl need_exp)
  in
  let rec aux im =
    match im with
    | [] -> []
    | s :: xs ->
        let cl = String.to_seq s |> List.of_seq in
        let new_string =
          expand_with_cond cl need_expansion |> List.to_seq |> String.of_seq
        in
        new_string :: aux xs
  in
  aux im

(** [expand_universe im] expands the image [im] into the two dimensions. *)
let expand_universe_by (n : int) (im : image) : image =
  expand_row_by n im |> expand_col_by n

(** [planets_of_string str] returns the index of planets for given
    string [str].
    Example: "..#....#" -> [2, 7]
    *)
let planets_of_string (str : string) : int list =
  String.to_seq str |> List.of_seq
  |> List.mapi (fun i c -> (i, c = '#'))
  |> List.filter (fun (_, b) -> b)
  |> List.map (fun (i, _) -> i)

(** [get_planets im] return a list of coordinates of planets in the [im]. *)
let planets_of_img (img : image) : (int * int) list =
  List.map planets_of_string img
  |> List.mapi (fun i lv -> List.map (fun v -> (v, i)) lv)
  |> List.concat

let compute_distance (p1 : int * int) (p2 : int * int) : int =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  abs (x1 - x2) + abs (y1 - y2)

(* [planets_distances img] returns all distances between all planets *)
let planets_distances (img : image) : int list =
  let p = planets_of_img img in
  let rec aux acc = function
    | [] -> acc
    | v :: xv -> aux (List.map (compute_distance v) xv @ acc) xv
  in
  aux [] p

let sample1 = [ "...#......"; ".........."; "...#......" ]

let sample =
  [
    "...#......";
    ".......#..";
    "#.........";
    "..........";
    "......#...";
    ".#........";
    ".........#";
    "..........";
    ".......#..";
    "#...#.....";
  ]

let part1 ls =
  expand_universe_by 2 ls |> planets_distances |> List.fold_left ( + ) 0
  |> string_of_int

(** ************************************************************************
    For part2 the approach is not good because it takes too much time. Use
    matrix and keep track of the expansions. And just compute them when needed.
  *** ********************************************************************** *)

let print_universe (g : (char * bool) array array) (e : bool array array) : unit
    =
  let x_range = Array.length g in
  let y_range = Array.length g.(0) in
  for y = 0 to y_range - 1 do
    for x = 0 to x_range - 1 do
      Printf.printf "%c %c " (fst g.(x).(y)) (if e.(x).(y) then 'o' else ' ')
    done;
    print_newline ()
  done

let fill_galaxy (matrix : (char * bool) array array) (cl : char list list) =
  let x_range = Array.length matrix in
  let y_range = Array.length matrix.(0) in
  for y = 0 to y_range - 1 do
    for x = 0 to x_range - 1 do
      matrix.(x).(y) <- (List.nth (List.nth cl y) x, false)
    done
  done

let gen_expansions (g : (char * bool) array array) : bool array array =
  let x_range = Array.length g in
  let y_range = Array.length g.(0) in
  let expansions = Array.make_matrix x_range y_range false in
  let _ =
    (* Find col with only '.' *)
    for x = 0 to x_range - 1 do
      if Array.fold_left (fun acc v -> acc && fst v = '.') true g.(x) then
        Array.iteri (fun i _ -> expansions.(x).(i) <- true) expansions.(x)
    done;
    (* Find row with only '.' *)
    for y = 0 to y_range - 1 do
      let acc = ref true in
      for x = 0 to x_range - 1 do
        if fst g.(x).(y) <> '.' then acc := false
      done;
      if !acc then
        for x = 0 to x_range - 1 do
          expansions.(x).(y) <- true
        done
    done
  in
  expansions

let compute_distance2 (exp : bool array array) (exp_val : int) (p1 : int * int)
    (p2 : int * int) : int =
  let x1, y1 = p1 in
  let x2, y2 = p2 in

  let d = ref 0 in
  (* go from x1 to x2  *)
  let sx = min x1 x2 in
  let ex = max x1 x2 in
  for x = sx + 1 to ex do
    d := !d + if exp.(x).(y1) then exp_val else 1
  done;
  (* go from y1 + 1 to y2 following x2 *)
  let sy = min y1 y2 in
  let ey = max y1 y2 in
  for y = sy + 1 to ey do
    d := !d + if exp.(x2).(y) then exp_val else 1
  done;
  !d

let part2 ls =
  (*let _ =*)
  let cl : char list list =
    List.map (fun s -> String.to_seq s |> List.of_seq) ls
  in
  (* Print cl *)
  (* let _ = List.iter (fun l -> List.iter (fun c -> print_char c) l; print_newline ()) cl in *)
  let x_range = List.length (List.hd cl) in
  let y_range = List.length cl in
  (* Here is the galaxy *)
  let galaxy : (char * bool) array array =
    Array.make_matrix x_range y_range ('.', false)
  in
  (* Here is the matrix of expansions. To be in expansion a point must be
     on a row or a colomn that only has '.' *)
  fill_galaxy galaxy cl;
  let expansions : bool array array = gen_expansions galaxy in

  (* print_universe galaxy expansions; *)
  let planets = planets_of_img ls in

  let rec aux acc = function
    | [] -> acc
    | v :: xv ->
        aux (List.map (compute_distance2 expansions 1_000_000 v) xv @ acc) xv
  in
  aux [] planets |> List.fold_left ( + ) 0 |> string_of_int

(* can be used with watch and without dune for testing purpose *)
