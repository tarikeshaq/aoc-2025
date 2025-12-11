type state =
 | SeenDac | SeenFft | SeenNone | SeenBoth


let parse_line input tbl =
    match String.split_on_char ':' input with
    | start :: [rest] -> 
        let neighbors = String.split_on_char ' ' rest |> List.filter (fun s -> s <> "") in
        Hashtbl.replace tbl start neighbors;
        List.iter (fun n -> 
            if Hashtbl.find_opt tbl n |> Option.is_none then Hashtbl.add tbl n []
        ) neighbors;
    | _ -> failwith "unexpected input"

let count_paths tbl start dest =
    let memo = Hashtbl.create (Hashtbl.length tbl) in
    let rec aux s e =
    match Hashtbl.find_opt memo s with
    | Some(res) -> res
    | None ->
        Hashtbl.add memo s 0;
        let res = if s = e then 1
        else
        let neighbors = Hashtbl.find tbl s in
        List.fold_left (fun acc i ->
            acc + aux i e
        ) 0 neighbors in
        Hashtbl.replace memo s res;
        res
    in
    aux start dest

let default_state = 
    [|0; 0; 0; 1|]

let empty_state =
    [|0; 0; 0; 0|]

let resolve_state curr_state neighbor_state curr_node =
    match curr_node with
    | "dac" -> begin
     curr_state.(0) <- curr_state.(0) + neighbor_state.(2);
     curr_state.(1) <- curr_state.(1) + neighbor_state.(3);
     curr_state.(2) <- 0;
     curr_state.(3) <- 0;
    end
    | "fft" -> begin
     curr_state.(0) <- curr_state.(0) + neighbor_state.(1);
     curr_state.(1) <- 0;
     curr_state.(2) <- curr_state.(2) + neighbor_state.(3);
     curr_state.(3) <- 0;
    end
    | _ -> begin
     curr_state.(0) <- curr_state.(0) + neighbor_state.(0);
     curr_state.(1) <- curr_state.(1) + neighbor_state.(1);
     curr_state.(2) <- curr_state.(2) + neighbor_state.(2);
     curr_state.(3) <- curr_state.(3) + neighbor_state.(3);
     end 

let count_paths_2 tbl start dest =
    let memo = Hashtbl.create (Hashtbl.length tbl) in
    let rec aux s e =
    match Hashtbl.find_opt memo s with
    | Some(res) -> res
    | None ->
        Hashtbl.add memo s (Array.copy empty_state);
        let res = if s = e then (Array.copy default_state)
        else
        let neighbors = Hashtbl.find tbl s in
        List.fold_left (fun acc i ->
            let neighbor_state = aux i e in
            resolve_state acc neighbor_state s;
            acc
        ) (Array.copy empty_state) neighbors in
        Hashtbl.replace memo s res;
        res
    in
    aux start dest 
        

let print_graph graph =
    Hashtbl.iter (fun k v -> 
        Printf.printf "%s:" k;
        List.iter (fun n -> Printf.printf "%s," n) v;
        Printf.printf "\n";
    ) graph


let part1 lines =
    let graph = Hashtbl.create (List.length lines) in
    List.iter (fun line -> parse_line line graph) lines;
    count_paths graph "svr" "out" (* should change the input and use "you" to get right answer *)

let part2 lines = 
    let graph = Hashtbl.create (List.length lines) in
    List.iter (fun line -> parse_line line graph) lines;
    (count_paths_2 graph "svr" "out").(0)
