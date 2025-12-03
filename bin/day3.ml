let remove_xth_and_append l x n =
    List.fold_left (fun (i, acc) e ->
        if i = x then (i+1, acc)
        else 
            (i+1, e :: acc)
    ) (0, []) l
    |> (fun (_, res) -> n :: res)
    |> List.rev

let joltage bank len =
    String.fold_left (fun acc c -> 
        if List.length acc < len then acc @ [c]
        else
         let (_, idx) = List.fold_left (fun (best, idx_to_replace) e -> 
                let candidate = remove_xth_and_append acc e c in
                let curr_score = Util.join_list_to_int candidate in
                if (curr_score > best) then (curr_score, e) else (best, idx_to_replace) 
            ) (Util.join_list_to_int acc, -1) (Util.range 0 (len-1)) in
        if idx = -1 then acc else (remove_xth_and_append acc idx c)
    ) [] bank 
    |> Util.join_list_to_int 

let sum_joltage lines len =
    List.fold_left (fun acc bank -> acc + joltage bank len) 0 lines

let part1 lines = sum_joltage lines 2 

let part2 lines = sum_joltage lines 12
