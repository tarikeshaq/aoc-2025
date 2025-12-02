type dial = {
    position: int;
    num_zeros_seen: int;
}

type direction =
    L | R

type instruction = {
    direction: direction;
    num: int;
}

let pos_mod num m =
  let res = num mod m in
  if res < 0 then res + m else res

let reduce_num d num =
    if num > 100 then
    let new_dial = {
        position = d.position;
        num_zeros_seen = d.num_zeros_seen + (num / 100);
    } in
    (num mod 100, new_dial)
    else
    (num, d)

let rotate_left d num =
    let (num, d) = reduce_num d num in
    let d = if d.position - num <= 0 && d.position != 0 then {
        position = d.position;
        num_zeros_seen = d.num_zeros_seen + 1;
    } else d in
    {
       position = pos_mod (d.position - num) 100;
       num_zeros_seen = d.num_zeros_seen;
    }


let rotate_right d num =
    let (num, d) = reduce_num d num in
    let d = if d.position + num >= 100 && d.position != 0 then 
    {
        position = d.position;
        num_zeros_seen = d.num_zeros_seen + 1;
    } else d in
    {
        position = (d.position + num) mod 100;
        num_zeros_seen = d.num_zeros_seen;
    }

let rotate d instruction = match instruction.direction with
    | L -> rotate_left d instruction.num
    | R -> rotate_right d instruction.num 
    
 

let parse_line line =
    let len = String.length line in
    let dir = String.sub line 0 1 in
    let num = String.sub line 1 (len - 1) in
    let direction = if String.starts_with ~prefix:"L" dir then L else R in
    {
        direction = direction; 
        num = int_of_string num;
    }

let part1 lines =
    let instructions = List.map parse_line lines in
    let (_, res) = List.fold_left (fun acc elem ->
      let (d, res) = acc in 
      let d = rotate d elem in
      (d, if d.position == 0 then res + 1 else res)
    ) ({
        position = 50;
        num_zeros_seen = 0;
    }, 0) instructions in
    string_of_int res

let part2 lines =
    let instructions = List.map parse_line lines in
    let d = List.fold_left (fun acc elem ->
     let res = rotate acc elem in
     res
    ) {
        position = 50;
        num_zeros_seen = 0;
    } instructions in
    string_of_int d.num_zeros_seen 
