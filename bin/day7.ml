type location =
    Start | Empty | Splitter 

let make_location c =
    match c with 
        | 'S' -> Start
        | '.' -> Empty
        | '^' -> Splitter
        | _ -> failwith "unexpected location"

let parse_input lines = Util.make_matrix_of lines make_location



(* This would look alot simpler as a dfs, but I wanted to mess around with queues and hashtables in ocaml *)
let relevant_splitters m =
    let seen = Hashtbl.create (Array.length m) in
    let valid_idx i j =
        i >= 0 && i < Array.length m && j >= 0 && j < Array.length m.(i) 
        && (Hashtbl.find_opt seen (i, j) |> Option.is_none)
    in
    let q = Queue.create () in
    let start_idx = List.find (fun (i, j) -> m.(i).(j) = Start) (Util.matrix_indices m) in
    let rec bfs acc q =
        if Queue.is_empty q then acc
        else
        let (i, j) = Queue.pop q in
        if valid_idx i j then begin
        Hashtbl.add seen (i, j) 1;
        match m.(i).(j) with
        | Start | Empty -> 
          Queue.push (i+1, j) q;
          bfs acc q
        | Splitter ->
          Queue.push (i+1, j+1) q;
          Queue.push (i+1, j-1) q;
          bfs ((i, j) :: acc) q
        end
        else
        bfs acc q
    in
    Queue.push start_idx q;
    bfs [] q


(* We do a buttom up DP with a one row as a memo, the fact the splitters don't show on the edges simplifies our edge cases *)
let count_paths m =
    let n = Array.length m in
    let memo_size = Array.length m.(0) in
    let memo = Array.make memo_size 1 in 
    for i = (n-1) downto 0 do
        for j = 0 to (memo_size - 1) do
            if m.(i).(j) = Splitter then memo.(j) <- (memo.(j-1) + memo.(j+1)) 
        done
    done;
    let j = List.find (fun j -> m.(0).(j) = Start) (Util.range 0 (n-1)) in
    memo.(j)
    


let part2 lines = parse_input lines |> count_paths



let part1 lines = 
    parse_input lines
    |> relevant_splitters
    |> List.length
