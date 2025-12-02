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

let rotate d instruction = 
    let (num, d) = reduce_num d instruction.num in
    let (cond, num) =
    match instruction.direction with
    | L -> ((fun pos -> pos - num <= 0), -num) 
    | R -> ((fun pos -> pos + num >= 100), num) in
    let num_zeros_seen = if cond d.position && d.position != 0 then d.num_zeros_seen + 1 else d.num_zeros_seen in
    {
       position = pos_mod (d.position + num) 100;
       num_zeros_seen = num_zeros_seen;
    }

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
    let (_, res) = List.fold_left (fun acc elem ->
      let instruction = parse_line elem in
      let (d, res) = acc in 
      let d = rotate d instruction in
      (d, if d.position == 0 then res + 1 else res)
    ) ({
        position = 50;
        num_zeros_seen = 0;
    }, 0) lines in
    string_of_int res

let part2 lines =
    let d = List.fold_left (fun acc elem ->
     let instruction = parse_line elem in
     rotate acc instruction
    ) {
        position = 50;
        num_zeros_seen = 0;
    } lines in
    string_of_int d.num_zeros_seen 
