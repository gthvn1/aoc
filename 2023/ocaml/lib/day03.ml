type number = { value : int; line : int; start : int; fin : int }

let isDigit = function '0' .. '9' -> true | _ -> false

(** [numbersOfString line str] returns a list of numbers for a given string.
    For example "..*123%.." -> [{value= 123, line = [line], start = 3, end = 5}] *)
let numbersOfString (line : int) (str : string) : number list =
  (* char_list = ['.', '.', ...] *)
  let char_list = String.to_seq str |> List.of_seq in
  (* acc is the accumulator, cur is the current number *)
  let rec nos (acc : number list) (idx : int) (cur : number option)
      (cl : char list) =
    match cl with
    | [] -> (
        match cur with
        | Some n ->
            { value = n.value; line = n.line; start = n.start; fin = idx }
            :: acc
        | None -> acc)
    | c :: xc -> (
        if isDigit c then
          let d = int_of_char c - int_of_char '0' in
          let new_cur =
            match cur with
            | Some n ->
                Some
                  {
                    value = (n.value * 10) + d;
                    line = n.line;
                    start = n.start;
                    fin = 0;
                  }
            | None -> Some { value = d; line; start = idx; fin = 0 }
          in
          nos acc (idx + 1) new_cur xc
        else
          (* Not a digit *)
          match cur with
          | Some n ->
              let num =
                {
                  value = n.value;
                  line = n.line;
                  start = n.start;
                  fin = idx - 1;
                }
              in
              nos (num :: acc) (idx + 1) None xc
          | None -> nos acc (idx + 1) None xc)
  in
  nos [] 0 None char_list

(** it is the same than numbersOfString but argument is a list of string *)
let numbersOfStringList (ls : string list) : number list =
  let rec nosl (idx : int) (str : string list) : number list =
    match str with
    | [] -> []
    | s :: ls -> numbersOfString idx s @ nosl (idx + 1) ls
  in
  nosl 0 ls

(** [gridOfString str] transform a list of strings into a list of list of chars
    ["un"; "deux"] -> [['u'; 'n']; ['d'; 'e'; 'u'; 'x']]*)
let gridOfStringList = List.map (fun s -> String.to_seq s |> List.of_seq)

(** return true if the character at line [line] and index [idx] in the grid is a symbol *)
let isSymbol (g : char list list) (line : int) (idx : int) : bool =
  let line_len = List.length (List.nth g 0) in
  let number_of_lines = List.length g in
  if line < 0 || line >= number_of_lines || idx < 0 || idx >= line_len then
    false
  else
    let (c : char) = List.nth (List.nth g line) idx in
    match c with '0' .. '9' | '.' -> false | _ -> true

(** [getSymbol grid line idx] returns an option that is a tuple that can contains the position
    of the symbols in the [grid] and its caracter for a given [line] and [index] in the line.
    The position is the offset in the grid. For example if the grid is 5x5 the first character of
    the first line is at offset 0 and the first character of the seconde line is at offset 6. *)
let getSymbol g (line : int) (idx : int) : (int * char) option =
  let line_len = List.length (List.nth g 0) in
  let number_of_lines = List.length g in
  if line < 0 || line >= number_of_lines || idx < 0 || idx >= line_len then None
  else
    let (c : char) = List.nth (List.nth g line) idx in
    match c with
    | '0' .. '9' | '.' -> None
    | _ -> Some ((line * line_len) + idx, c)

(** [connectedSymbols g start end line] return the list of symbols and their index for
    a given interval from [start] to [end] included of a given [line]. *)
let rec connectedSymbols g idx idx_end line =
  if idx > idx_end then []
  else
    match getSymbol g line idx with
    | None -> connectedSymbols g (idx + 1) idx_end line
    | Some e -> e :: connectedSymbols g (idx + 1) idx_end line

(** [listOfConnected g n] returns a list of tuple (index, symbol) that is the
    list of symbols connected to the number [n] with their index in the grid [g]. *)
let listOfConnected (g : char list list) (n : number) : (int * char) list =
  let line_before = n.line - 1 in
  let line_after = n.line + 1 in
  let idx_start = n.start - 1 in
  let idx_end = n.fin + 1 in
  let sym_before =
    match getSymbol g n.line idx_start with None -> [] | Some e -> [ e ]
  in
  let sym_after =
    match getSymbol g n.line idx_end with None -> [] | Some e -> [ e ]
  in
  connectedSymbols g idx_start idx_end line_before
  @ connectedSymbols g idx_start idx_end line_after
  @ sym_before @ sym_after

(** [isConnected g n] returns true if the number [n] is connected to a symbol in the grid [g]. *)
let isConnected (g : char list list) (n : number) : bool =
  match listOfConnected g n with [] -> false | _ -> true

(** [listOfConnectedStars grid] returns a list of tuple where the first item is the value of a node and the second value
    is the position of stars connected to this node. 
    Output will look like:
    [(114, []); (467, [13]); (633, []); (35, [13]); (617, [43])] *)
let listOfConnectedStars (g : char list list) (n : number list) :
    (int * int list) list =
  let positionOfConnectedStars (g : char list list) (n : number) : int list =
    listOfConnected g n
    |> List.filter (fun (_, c) -> c = '*')
    |> List.map (fun (p, _) -> p)
  in
  List.map (fun n -> (n.value, positionOfConnectedStars g n)) n

let rec listContains (i : int) (l2 : int list) : bool =
  match l2 with
  | [] -> false
  | x :: xs -> if x = i then true else listContains i xs

let rec itemInCommon (l1 : int list) (l2 : int list) : bool =
  match l1 with
  | [] -> false
  | x :: xs -> if listContains x l2 then true else itemInCommon xs l2

(** [getGearRation v vs] is getting the gear ration of [v] for a given list [vs].
    v is a tuple with a value and a list of (position, stars) that can be empty.
    The function will iterate over [vs] to check if another item as in its list
    of (position, stars) a star at the same position. If it is the case it returns
    the product of the two. *)
let rec getGearRatio (v : int * int list) (vs : (int * int list) list) : int =
  match vs with
  | [] -> 0
  | v' :: vs' ->
      let value_a, list_a = v in
      let value_b, list_b = v' in
      if itemInCommon list_a list_b then value_a * value_b
      else getGearRatio v vs'

let sample_grid =
  [
    "467..114..";
    "...*......";
    "..35..633.";
    "......#...";
    "617*......";
    ".....+.58.";
    "..592.....";
    "......755.";
    "...$.*....";
    ".664.598..";
  ]

let grid = gridOfStringList sample_grid
let list_of_numbers = numbersOfStringList sample_grid

let part1 ls =
  let g = gridOfStringList ls in
  let ln = numbersOfStringList ls in
  (* list of numbers *)
  let fln = List.filter (fun n -> isConnected g n) ln in
  (* filtered list of numbers *)
  List.fold_left (fun v n -> v + n.value) 0 fln |> string_of_int

let part2 ls =
  let g = gridOfStringList ls in
  let ln = numbersOfStringList ls in
  let lcs = listOfConnectedStars g ln in
  let rec computeGearRatio = function
    | [] -> 0
    | v :: vs -> getGearRatio v vs + computeGearRatio vs
  in
  computeGearRatio lcs |> string_of_int
