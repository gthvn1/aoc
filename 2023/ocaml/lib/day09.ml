(** [all_zeros lst] returns true if all items of [lst] are equal to 0. *)
let all_zeros lst = List.for_all (fun x -> x = 0) lst

(** [list_of_diff lst] returns the list of differences between two items of [lst].
     [1; 3; 5; 7] -> [2; 2; 2] *)
let list_of_diff lst =
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | x :: y :: tl -> aux ((y - x) :: acc) (y :: tl)
  in
  aux [] lst |> List.rev

(** [list_of_all_diffs lst] returns a list of list of all successive diffs starting from [lst] *)
let list_of_all_diffs (lst : int list) : int list list =
  let rec aux acc l =
    let diff = list_of_diff l in
    if all_zeros diff then diff :: acc else diff :: aux acc diff
  in
  aux [] lst

(** [last_elmt lst] returns the last element of [lst] *)
let last_elmt lst = List.nth lst (List.length lst - 1)

(*
  example:
  Let's take the input list [10; 13; 16; 21; 30; 45]
  - First we compute the list of diffs
  [[10; 13; 16; 21; 30; 45]
  [3; 3; 5; 9; 15];
  [0; 2; 4; 6];
  [2; 2; 2];
  [0; 0]]
  => start from the last list and take 0
  => take the last element of the previous list 2 and add it 0 + 2
  => take the last element of the previous list 6 and add it 2 + 6
  => take the last element of the previous list 15 and add it 8 + 15
  => take the last element of the previous list 45 and add it 23 + 45
  And the result is  68
   *)
let find_last_value (lst : int list) : int =
  let diffs = list_of_all_diffs lst in
  List.fold_right (fun l acc -> last_elmt l + acc) (lst :: diffs) 0

(*
  example:
  Let's take the input list [10; 13; 16; 21; 30; 45]
  - First we compute the list of diffs
  [[10; 13; 16; 21; 30; 45]
  [3; 3; 5; 9; 15];
  [0; 2; 4; 6];
  [2; 2; 2];
  [0; 0]]
  => start from the last list and take 0
  => take the first element of the previous list 2 and remove 0 from it: 2
  => take the first element of the previous list 0 and remove 2 from it: -2
  => take the first element of the previous list 3 and remove -2 from it: 5
  => take the first element of the previous list 10 and remove 5 from it: 5
  And the result is 5
   *)
let find_first_value (lst : int list) : int =
  let diffs = list_of_all_diffs lst in
  List.fold_right (fun l acc -> List.hd l - acc) (lst :: diffs) 0

(** [list_of_string str] returns a list of int from a string *)
let list_of_string (str : string) : int list =
  List.map int_of_string @@ String.split_on_char ' ' str

let sample = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ]

(** [compute_all_next_values lst] returns a list of all next values starting from [lst] *)
let part1 ls =
  let inputs = List.map list_of_string ls in
  List.map find_last_value inputs |> List.fold_left ( + ) 0 |> string_of_int

let part2 ls =
  let inputs = List.map list_of_string ls in
  List.map find_first_value inputs |> List.fold_left ( + ) 0 |> string_of_int
