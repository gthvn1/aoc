let rec isInList (v : int) = function
  | [] -> false
  | x :: xs -> if x = v then true else isInList v xs

let rec numberOfWinning (win : int list) (num : int list) : int =
  match num with
  | [] -> 0
  | x :: xs ->
      if isInList x win then 1 + numberOfWinning win xs
      else numberOfWinning win xs

(** [getPoints str] returns the number of win for a given string.
    For example
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" -> 4
 *)
let getPoints (str : string) : int =
  let cards =
    List.nth (String.split_on_char ':' str) 1
    |> String.trim |> String.split_on_char '|'
  in
  let win_cards =
    List.nth cards 0 |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  let my_cards =
    List.nth cards 1 |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  let wins =
    numberOfWinning
      (List.map int_of_string win_cards)
      (List.map int_of_string my_cards)
  in
  wins

let rec computePoints = function
  | 0 -> 0
  | 1 -> 1
  | n -> 2 * computePoints (n - 1)

let sample =
  [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
  ]

(* [updateCopies list i n v] updates [n] items in the list [l] starting at index [i] by a
   value of [v]. *)
let updateCopies (cards : int list) (i : int) (n : int) (v : int) : int list =
  let update idx x =
    if n <> 0 && i <= idx && idx <= i + n - 1 then x + v else x
  in
  List.mapi update cards

let rec totalScratch (ls : string list) (index : int) (l : int list) : int list
    =
  let points = getPoints (List.nth ls index) in
  if index = List.length l - 1 then l
  else
    totalScratch ls (index + 1)
      (updateCopies l (index + 1) points (List.nth l index))

let part1 ls =
  List.map getPoints ls |> List.map computePoints |> List.fold_left ( + ) 0
  |> string_of_int

let part2 ls =
  let noc = List.map (fun _ -> 1) ls in
  (* noc is number of cards and it is init points to 1 that is the number of original cards *)
  List.fold_left ( + ) 0 (totalScratch ls 0 noc) |> string_of_int
