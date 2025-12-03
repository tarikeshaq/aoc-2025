let revoke_xth l x n =
    List.fold_left (fun acc e ->
        let (i, acc) = acc in
        if i == x then (i+1, acc)
        else 
            (i+1, e :: acc)
    ) (0, []) l
    |> (fun (_, res) -> n :: res)
    |> List.rev

let joltage bank len =
    String.fold_left (fun acc c -> 
        if List.length acc < len then acc @ [c]  (*TODO: use cons and reverse*)
        else
         let (_, idx) = List.fold_left (fun best e -> 
                let (best, idx_to_replace) = best in
                let candidate = revoke_xth acc e c in
                let curr_score = Util.join_list_to_int candidate in
                if (curr_score > best) then (curr_score, e) else (best, idx_to_replace) 
            ) (Util.join_list_to_int acc, -1) (Util.range 0 (len-1)) in
        if idx == -1 then acc else (revoke_xth acc idx c)
    ) [] bank 
    |> Util.join_list_to_int 



let part1 lines =
   List.fold_left (fun acc bank ->
       acc + (joltage bank 2) 
    ) 0 lines 

let part2 lines =
   List.fold_left (fun acc bank ->
       acc + (joltage bank 12) 
    ) 0 lines 


