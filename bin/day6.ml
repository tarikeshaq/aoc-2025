type operation =
    Mult | Add

type problem = {
    nums: int list;
    op: operation;
}

let trim lines =
    List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> String.length s != 0) |> Array.of_list) lines
    |> Array.of_list

 let transpose default lines = 
    let n = Array.length lines in
    let m = Array.fold_left (fun acc arr -> max (Array.length arr) acc) 0 lines in
    let res = Array.make_matrix m n default in
    List.iter (fun i ->
        List.iter (fun j ->
            if j < Array.length lines.(i) then
            res.(j).(i) <- lines.(i).(j)
            ) (Util.range 0 (m-1)) 
    ) (Util.range 0 (n-1));
    res   

let make_problems columns =
    Array.fold_left (fun acc c ->
        let len = Array.length c in
        let op = match c.(len-1) with
        | "*" -> Mult
        | _ -> Add in
        let res = List.fold_left (fun acc i -> int_of_string c.(i) :: acc) [] (Util.range 0 (len - 2)) in
        {
            nums = res;
            op = op;
        } :: acc
    ) [] columns

let digit_of_char c =
    int_of_char c - int_of_char '0'

let reduce_col col =
    Array.fold_left (fun acc c ->
        match (c, acc) with
        | (' ', ret) -> ret
        | ('*', Some((num, _))) -> Some((num, Some(Mult)))
        | ('+', Some((num, _))) -> Some((num, Some(Add)))
        | (n, None) -> Some((digit_of_char n, None))
        | (n, Some((num, _))) -> 
            let new_num = digit_of_char n + (num * 10) in
            Some((new_num, None))
    ) None col

let make_column_problems columns =
    let (res, _) = Array.fold_left (fun (res, curr) col ->
        match reduce_col col with
        | None -> (res, curr)
        | Some((num, None)) -> (res, num :: curr)
        | Some((num, Some(op))) -> ({
            nums = (num :: curr);
            op = op;
        } :: res, [])
        
    ) ([], []) columns in
    res


let eval_problems ps =
    List.fold_left (fun acc p ->
     acc + match p.op with 
        | Mult -> List.fold_left (fun acc i -> acc * i) 1 p.nums
        | Add -> List.fold_left (fun acc i -> acc + i) 0 p.nums
    ) 0 ps

let part1 lines = 
   trim lines |>
   transpose "."
    |> make_problems
    |> eval_problems

let part2 lines =
    List.map (fun l -> String.to_seq l |> Array.of_seq) lines
    |> Array.of_list
    |> transpose ' '
    |> Array.to_seq |> List.of_seq |> List.rev |> Array.of_list
    |> make_column_problems
    |> eval_problems
