let make_matrix lines = Util.make_matrix_of lines Fun.id 
    
let dirs = [(0, 1); (0, -1); (1, 0); (1, -1); (1, 1); (-1, 0); (-1, 1); (-1, -1)] 

let find_removable matrix =
    let n = Array.length matrix in
    let m = Array.length matrix.(0) in
    let valid i j (di, dj) =
        let ni = di + i in
        let nj = dj + j in
        ni>= 0 && ni < n && nj >= 0 && nj < m && matrix.(ni).(nj) = '@'
    in
    List.filter (fun (i, j) ->
        matrix.(i).(j) = '@' && List.filter (valid i j) dirs |> List.length < 4 
    ) (Util.matrix_indices matrix) 
    
let part1 lines = make_matrix lines |> find_removable |> List.length

let part2 lines =
    let rec remove acc matrix =
        let removable = find_removable matrix in
        match removable with
        | [] -> acc
        | _ -> 
         List.iter (fun (i, j) -> matrix.(i).(j) <- '.' ) removable;
         remove (acc + List.length removable) matrix
    in
    make_matrix lines |> remove 0
