type range = {
    start: int;
    end_range: int;
}

let equal_halves s =
    let len = String.length s in
    let first = String.sub s 0 (len/2) in
    let second = String.sub s (len/2) (len/2) in
    first = second

let invalid num =
    let s = (String.trim (string_of_int num)) in
    let len = String.length s in
    if len mod 2 == 0 && equal_halves s then true else false

let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let split_into_chunks s chunk_len =
  let n = String.length s in
  let rec aux i acc =
    if i < 0 then acc
    else
      let chunk = String.sub s i chunk_len in
      aux (i - chunk_len) (chunk :: acc)
  in
  aux (n - chunk_len) []

let is_invalid_inner a s =
    let strs = split_into_chunks s a in
    let first = List.hd strs in
    (List.length strs != 1 && List.for_all (fun elem -> 
    elem = first) strs)


(* To get whether a number is invalid, we find the factors of its length
   if the lenght is prime, the only possibliity is all the elements being the same
   otherwise, if x = z * y, we find the first z elements and check if they are repeated y times for every z and y

   to get the factors, we go from 0 -> sqrt(n) and check if the n is divisible byu i
*)

let factors num =
    List.fold_left (fun acc i ->
        if (i != 0 && num mod i == 0) then (i, num / i) :: acc else acc
    ) [] (range 0 (int_of_float (ceil (sqrt (float_of_int num)))))

let invalid_part_2 num =
    let s = String.trim (string_of_int num) in
    let fact = factors (String.length s) in
    Option.is_some (List.find_opt (fun fac ->
     let (a, b) = fac in
     let inv = is_invalid_inner a s || is_invalid_inner b s in
     inv
    ) fact)


let parse_ranges text =
    let split = String.split_on_char ',' (String.trim text) in
    List.map (fun s -> 
        match String.split_on_char '-' (String.trim s) with
        | left :: [right] -> {
            start = int_of_string left;
            end_range = int_of_string right;
        }
        | _ -> {
            start = 0;
            end_range = 0;
        }
    ) split

(* What we want is to parse the input by seperating it by comma, then
    we would have a bunch of ranges. We brute force our way through from
    the start of a range to the end of it, finding all magic numbers.
    those numbers are ones that have even length AND the first half equals the second half exactly

   *)
let part1 text =
     let ranges = parse_ranges text in
     List.fold_left (fun acc elem -> 
       acc + List.fold_left(fun inner_acc inner_elem ->
            if invalid inner_elem then inner_acc + inner_elem else inner_acc
        ) 0 (range elem.start elem.end_range) 
    ) 0 ranges

let part2 text =
     let ranges = parse_ranges text in
     List.fold_left (fun acc elem -> 
       acc + List.fold_left(fun inner_acc inner_elem ->
            if invalid_part_2 inner_elem then inner_acc + inner_elem else inner_acc
        ) 0 (range elem.start elem.end_range) 
    ) 0 ranges
