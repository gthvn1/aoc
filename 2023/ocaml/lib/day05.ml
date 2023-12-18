(*
  - We have 7 maps
  "seed-to-soil map:"
  - Can we represent map with list of tuples (maybe use map record)
    - seed-to-soil: [(50, 98, 2); (52, 50, 48)]
  - How can we get the relation between soil and seed
    - map is a tuple: destination, source, lenght
    - example: source = 12 => we check 2nd until 2nd + 3nd
    -                   12 =>   98 , 100
 *)
type map = { dst : int; src : int; len : int }

(* [getSrcMap s lm] returns some map if [s] fits into one map of list of map [lm]
   and none if not. *)
let rec getSrcMap (s : int) (lm : map list) : map option =
  match lm with
  | [] -> None
  | e :: es ->
      if e.src <= s && s < e.src + e.len then Some e else getSrcMap s es

(** [getDstMap d lm] returns an option mao if [d] fits into one map. *)
let rec getDstMap (d : int) (lm : map list) : map option =
  match lm with
  | [] -> None
  | e :: es ->
      if e.dst <= d && d < e.dst + e.len then Some e else getDstMap d es

(* [dstFromSrc s lm] returns the destination for a given [s] by looking into
   the list of maps [lm].
   For example for lm = [{dst: 50, src: 98, len: 2}; {dst: 52, src: 50, len: 48}]
    - destOfSrc 10 lm = 10
    - destOfSrc 57 lm = 59
    - destOfSrc 99 lm = 51 *)
let dstFromSrc (s : int) (lm : map list) : int =
  (*
     Check if s falls in one of the map
     - if no => destination is s
     - if yes => get the difference between the base src and dest and add it
     Example:
      - 10 => has no map, dst = 10
      - 57 => is into (52, 50, 48), diff = 50 - 52 = -2 => dst = 57 - (-2) = 59
      - 99 => is into (50, 98, 2), diff = 98-50 = 48 => dst = 99 - 48 = 51
  *)
  match getSrcMap s lm with
  | None -> s
  | Some m ->
      let diff = m.src - m.dst in
      s - diff

(* [srcFromDst d] returns the source from a given destination [d] following the maps [m].
   For example for lm = [{dst: 50, src: 98, len: 2}; {dst: 52, src: 50, len: 48}]
      - 10 => has no map, => 10
      - 59 => is into (52, 50, 48), diff = 50-52 = -2 => dst = 59 - 2 = 57
      - 51 => is into (50, 98, 2), diff = 98-50 = 48 => dst = 51 + 48 = 99
*)
let srcFromDst (d : int) (lm : map list) : int =
  match getDstMap d lm with
  | None -> d
  | Some m ->
      let diff = m.dst - m.src in
      d - diff

(*
  With dstFromSrc we just need to go throuh the list of list of map to reach the destination
*)
let rec seedToLocation (maps : map list list) (seed : int) : int =
  match maps with
  | [] -> seed
  | m :: lm -> seedToLocation lm (dstFromSrc seed m)

let rec locationToSeed (maps : map list list) (location : int) : int =
  match maps with
  | [] -> location
  | m :: lm -> locationToSeed lm (srcFromDst location m)

let parseMap (str : string) : map =
  let ints = String.split_on_char ' ' str in
  {
    dst = List.nth ints 0 |> int_of_string;
    src = List.nth ints 1 |> int_of_string;
    len = List.nth ints 2 |> int_of_string;
  }

(* Now the problem is to build the list of list of maps...*)
let buildListOfMaps (str_list : string list) : map list list =
  let isDigit = function '0' .. '9' -> true | _ -> false in
  let fsl = List.filter (fun s -> s <> "") str_list in
  let rec blom (maps : map list list) (cur : map list) (str : string list) =
    (* we just skip seeds for now. So we are interested by strings that starts with digit and strings
       that contains -to-*)
    match str with
    | [] -> cur :: maps
    | s :: xs ->
        if isDigit s.[0] then blom maps (parseMap s :: cur) xs
        else if String.ends_with ~suffix:"map:" s then blom (cur :: maps) [] xs
        else blom maps cur xs
  in
  blom [] [] fsl |> List.rev |> List.filter (fun l -> List.length l <> 0)

let rec getSeeds = function
  | [] -> []
  | s :: ls ->
      if String.starts_with ~prefix:"seeds:" s then
        List.nth (String.split_on_char ':' s) 1
        |> String.trim |> String.split_on_char ' '
        |> List.map (fun s -> int_of_string s)
      else getSeeds ls

let findLowestLocation (seeds : int list) (maps : map list list) : int =
  List.map (seedToLocation maps) seeds |> List.fold_left min Int.max_int

let findLowestLocationFromRange (s : int) (r : int) (maps : map list list) : int
    =
  let rec fllfr (loc : int) (s' : int) (r' : int) : int =
    if r' <= 0 then loc
    else
      let new_loc = seedToLocation maps s' in
      if new_loc < loc then fllfr new_loc (s' + 1) (r' - 1)
      else fllfr loc (s' + 1) (r' - 1)
  in
  fllfr Int.max_int s r

let sample =
  [
    "seeds: 79 14 55 13";
    "";
    "seed-to-soil map:";
    "50 98 2";
    "52 50 48";
    "";
    "soil-to-fertilizer map:";
    "0 15 37";
    "37 52 2";
    "39 0 15";
    "";
    "fertilizer-to-water map:";
    "49 53 8";
    "0 11 42";
    "42 0 7";
    "57 7 4";
    "";
    "water-to-light map:";
    "88 18 7";
    "18 25 70";
    "";
    "light-to-temperature map:";
    "45 77 23";
    "81 45 19";
    "68 64 13";
    "";
    "temperature-to-humidity map:";
    "0 69 1";
    "1 0 69";
    "";
    "humidity-to-location map:";
    "60 56 37";
    "56 93 4";
  ]

let part1 ls =
  findLowestLocation (getSeeds ls) (buildListOfMaps ls) |> string_of_int

let rec belongsToSeedsRange (v : int) = function
  | [] | _ :: [] -> false
  | s :: r :: xs ->
      if s <= v && v < s + r then true else belongsToSeedsRange v xs

let part2 _ = "Need to find a better solution...it takes around 10s currently"
(*
let part2 ls =
  let seeds = getSeeds ls in
  let maps = buildListOfMaps ls |> List.rev in
  (* we are looking from dest *)
  let rec seedFromLoc (loc : int) (mll : map list list) =
    match mll with [] -> loc | m :: ml -> seedFromLoc (srcFromDst loc m) ml
  in
  let rec findLower (current : int) =
    let s = seedFromLoc current maps in
    if belongsToSeedsRange s seeds then current else findLower (current + 1)
  in
  findLower 0 |> string_of_int
*)
