let make_matrix lines =
    Util.make_matrix_of lines (fun c ->
        match c with
            | '@' -> 1
            | _ -> 0
    )
    
let dirs = [(0, 1); (0, -1); (1, 0); (1, -1); (1, 1); (-1, 0); (-1, 1); (-1, -1)] 
let find_removable matrix =
    let valid (dirI, dirJ) i j =
        (dirI + i) >= 0 && (dirI + i) < Array.length matrix &&
            (dirJ + j) >= 0 && (dirJ + j) < (matrix.(i) |> Array.length)
    in
    let (_, res) = Array.fold_left (fun (i, acc) row -> 
        Array.fold_left (fun (j, inner_acc) elem -> 
            if elem = 0 then (j+1, inner_acc)
            else
            List.fold_left (fun acc (dirI, dirJ) ->
              if valid (dirI, dirJ) i j && matrix.(dirI + i).(dirJ + j) = 1 then acc + 1 else acc
            ) 0 dirs
            |> (fun num -> if num < 4 then (j+1, ((i, j) :: inner_acc)) else (j+1, inner_acc))
        ) (0, []) row 
        |> (fun (_, inner_acc) -> (i+1, acc @ inner_acc))
    ) (0, []) matrix in
    res
    
let part1 lines =
    make_matrix lines |> find_removable |> List.length

let part2 lines =
    let rec remove acc matrix =
        let removable = find_removable matrix in
        if (List.length removable = 0) then acc
        else begin
            List.iter (fun (i, j) -> matrix.(i).(j) <- 0) removable;
            remove (acc + List.length removable) matrix
        end
    in
    make_matrix lines |> remove 0
