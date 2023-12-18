(* A hand consists of five cards labeled one of
   A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. *)

(* order is important, we start from the lowest strenght
   Note: For part1 'CJ' is between 'CT' and 'CQ'. *)
type card = CJ | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CQ | CK | CA
type hand = card list

type hand_kind =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind

type bids = hand * int

(** [card_of_char c] returns the card corresponding to the character [c] *)
let card_of_char = function
  | '2' -> C2
  | '3' -> C3
  | '4' -> C4
  | '5' -> C5
  | '6' -> C6
  | '7' -> C7
  | '8' -> C8
  | '9' -> C9
  | 'T' -> CT
  | 'J' -> CJ
  | 'Q' -> CQ
  | 'K' -> CK
  | 'A' -> CA
  | _ -> failwith "invalid hand"

(** [sort_hand h] returns a list where cards in [h] are sorted *)
let sort_hand h = List.sort compare h

(** [get_repartition h] returns the repartition of the cards.
    For part2 'CJ' is at index 0. *)
let get_repartition (h : hand) : int list =
  let repartition = List.init 13 (fun _ -> 0) in
  let rec aux (r : int list) = function
    | [] -> r
    | h :: t ->
        let index =
          match h with
          | CJ -> 0
          | C2 -> 1
          | C3 -> 2
          | C4 -> 3
          | C5 -> 4
          | C6 -> 5
          | C7 -> 6
          | C8 -> 7
          | C9 -> 8
          | CT -> 9
          | CQ -> 10
          | CK -> 11
          | CA -> 12
        in
        aux (List.mapi (fun i x -> if i = index then x + 1 else x) r) t
  in
  aux repartition h

(** [contains n l] returns true if the list [l] contains the integer [n]. *)
let contains n l = List.exists (fun x -> x = n) l

let five_of_a_kind (rep : int list) = contains 5 rep
let four_of_a_kind (rep : int list) = contains 4 rep
let full_house (rep : int list) = contains 3 rep && contains 2 rep

let three_of_a_kind (rep : int list) =
  let rep' = List.filter (fun x -> x = 3) rep in
  List.length rep' = 1

let two_pair (rep : int list) =
  let rep' = List.filter (fun x -> x = 2) rep in
  List.length rep' = 2

let one_pair (rep : int list) =
  let rep' = List.filter (fun x -> x = 2) rep in
  List.length rep' = 1

(** [hand_kind_of_hand h] returns the highest hand kind of a given hand [h].
    Order for testing is important because we want to return one_pair
    for one_pair only. So testing two_pair before is important. *)
let hand_kind_of_hand (h : hand) : hand_kind =
  let repartition = get_repartition h in
  if five_of_a_kind repartition then FiveOfAKind
  else if four_of_a_kind repartition then FourOfAKind
  else if full_house repartition then FullHouse
  else if three_of_a_kind repartition then ThreeOfAKind
  else if two_pair repartition then TwoPair
  else if one_pair repartition then OnePair
  else HighCard

(** [hand_kind_of_hand_with_jokers h] returns the hand kind of a given
      hand [h] if we are considering 'J' as a joker. *)
let hand_kind_of_hand_with_jokers (h : hand) : hand_kind =
  let rep = get_repartition h in
  let jokers = List.hd rep in
  (* number of jokers *)
  let r = List.tl rep in
  (* repartition without the joker *)
  let hk =
    if five_of_a_kind r then FiveOfAKind
    else if four_of_a_kind r then FourOfAKind
    else if full_house r then FullHouse
    else if three_of_a_kind r then ThreeOfAKind
    else if two_pair r then TwoPair
    else if one_pair r then OnePair
    else HighCard
  in
  (* now we update the hand according to the number of jokers *)
  match hk with
  | FiveOfAKind -> FiveOfAKind (* max already *)
  | FourOfAKind -> if jokers = 1 then FiveOfAKind else FourOfAKind
  | FullHouse -> FullHouse (* there is no joker if you have a full house *)
  | ThreeOfAKind ->
      if jokers = 1 then FourOfAKind
      else if jokers = 2 then FiveOfAKind
      else ThreeOfAKind
  | TwoPair -> if jokers = 1 then FullHouse else TwoPair
  | OnePair ->
      if jokers = 1 then ThreeOfAKind
      else if jokers = 2 then FourOfAKind
      else if jokers = 3 then FiveOfAKind
      else OnePair
  | HighCard ->
      if jokers = 1 then OnePair
      else if jokers = 2 then ThreeOfAKind
      else if jokers = 3 then FourOfAKind
      else if jokers = 4 || jokers = 5 then FiveOfAKind
      else HighCard

(** [bids_of_string str] returns a tuple that is the hand and the bid
    for a given string.
    For example:
    "32T3K 7" -> [C3; C2; CT; C3; CK], 7 *)
let bids_of_string (str : string) : bids =
  let str_split = String.split_on_char ' ' str in
  let hand_str = List.hd str_split in
  let value = List.nth str_split 1 |> int_of_string in
  let hand = String.to_seq hand_str |> List.of_seq |> List.map card_of_char in
  (hand, value)

(** [bids_of_string_list ls] returns a list of bids for a given list of
    strings [ls]. *)
let bids_of_string_list (ls : string list) : bids list =
  List.map bids_of_string ls

(** [compare_hands ~joker h1 h2] returns true if the hand [h1] is stronger than [h2].
    Comparaison is done with joker of [joker] is set to true. *)
let compare_hands ~(joker : bool) (h1 : hand) (h2 : hand) : int =
  let h1_kind =
    if joker then hand_kind_of_hand_with_jokers h1 else hand_kind_of_hand h1
  in
  let h2_kind =
    if joker then hand_kind_of_hand_with_jokers h2 else hand_kind_of_hand h2
  in
  if h1_kind > h2_kind then 1
  else if h1_kind < h2_kind then -1
  else
    (* if both hand are of the same kind we compare their cards starting
       by the first one *)
    let rec aux = function
      | [], [] -> 0
      | h1 :: t1, h2 :: t2 ->
          if h1 > h2 then 1 else if h1 < h2 then -1 else aux (t1, t2)
      | _ -> failwith "invalid hand"
    in
    aux (h1, h2)

(** [sort_bids ~joker bids] return the list of bids sorted from the lowest hand
    to the strongest one. If joker is set to true then 'J' are jokers. *)
let sort_bids ~(joker : bool) (bids : bids list) : bids list =
  List.sort (fun (h1, _) (h2, _) -> compare_hands ~joker h1 h2) bids

let sample = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let part1 ls =
  let answer =
    bids_of_string_list ls |> sort_bids ~joker:false
    |> List.mapi (fun i (_, v) -> (i + 1, v))
    |> List.fold_left (fun acc (i, v) -> acc + (i * v)) 0
    |> string_of_int
  in
  String.concat " " [ answer; "is now wrong because for part2 'J' is a jocker" ]

let part2 ls =
  bids_of_string_list ls |> sort_bids ~joker:true
  |> List.mapi (fun i (_, v) -> (i + 1, v))
  |> List.fold_left (fun acc (i, v) -> acc + (i * v)) 0
  |> string_of_int
