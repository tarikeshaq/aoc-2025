let make_matrix lines =
    Util.make_matrix_of lines (fun c ->
        match c with
            | '@' -> 1
            | _ -> 0
    )
    
let dirs = [(0, 1); (0, -1); (1, 0); (1, -1); (1, 1); (-1, 0); (-1, 1); (-1, -1)] 
let extract_idx idx matrix =
    let m = Array.length matrix.(0) in
    let i = idx / m in
    let j = idx mod m in
    (i, j)

let find_removable matrix =
    let n = Array.length matrix in
    let m = Array.length matrix.(0) in
    let valid i j (dirI, dirJ) =
        let i = dirI + i in
        let j = dirJ + j in
        i >= 0 &&
        i < n &&
        j >= 0 && 
        j < m &&
        matrix.(i).(j) = 1
    in
    List.filter (fun idx ->
        let (i, j) = extract_idx idx matrix in
        matrix.(i).(j) = 1 &&
        List.find_all (valid i j) dirs |> List.length |> (fun num -> num < 4)
    ) (Util.range 0 ((n * m) - 1))
    
let part1 lines = make_matrix lines |> find_removable |> List.length

let part2 lines =
    let rec remove acc matrix =
        let removable = find_removable matrix in
        if (List.length removable = 0) then acc
        else begin
            List.iter (fun idx ->
                let (i, j) = extract_idx idx matrix in
                matrix.(i).(j) <- 0
            ) removable;
            remove (acc + List.length removable) matrix
        end
    in
    make_matrix lines |> remove 0
