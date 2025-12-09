type junction = {
    id: int;
    x: int;
    y: int;
    z: int;
} 


type connection = {
    left: junction;
    right: junction;
    distance: float;
}

let parse_junction id s =
    match String.split_on_char ',' s with
        | x :: y :: z :: [] -> {
        id = id;
        x = int_of_string x;
        y = int_of_string y;
        z = int_of_string z;
        }
        | _ -> failwith "invalid junction"

let make_connection c1 c2 =
    let dist = Util.safe_distance (c1.x, c1.y, c1.z) (c2.x, c2.y, c2.z) in
    {
        left = c1;
        right = c2;
        distance = dist;
    }

let parse_input lines = 
    List.mapi parse_junction lines 
    |> Array.of_list

let make_connections junctions =
    let n = Array.length junctions in
    List.fold_left ( fun acc i ->
       List.fold_left ( fun inner j ->
            make_connection junctions.(i) junctions.(j) :: inner
       ) acc (Util.range (i+1) (n-1))
    ) [] (Util.range 0 (n-2))

let sort_connections connections = 
    List.fast_sort (fun c1 c2 ->
        let diff =  (c1.distance -. c2.distance) in
        if diff < 0.0 then -1
        else if diff > 0.0 then 1
        else 0
    ) connections 

let connect junctions connections iter = 
    let n = Array.length junctions in
    let dist = Util.mk_disjoint n in
    List.take iter connections |>
    List.iter (fun c ->
     let left = c.left.id in
     let right = c.right.id in
     let _ = Util.join_sets dist left right in
     ()
    );
    dist

let connect_spanning junctions connections =
    let n = Array.length junctions in
    let dist = Util.mk_disjoint n in
    let added = ref 0 in
    let last_connection = List.find (fun c ->
     let left = c.left.id in
     let right = c.right.id in
     if Util.join_sets dist left right then added := !added + 1;
     !added = (n-1)
    ) connections in
    last_connection.left.x * last_connection.right.x


let part1 lines = 
    let junctions = parse_input lines in
    let connections = make_connections junctions |> sort_connections in
    connect junctions connections 1000
    |> Util.get_largest_x 3
    |> List.fold_left (fun acc i -> if i = 0 then acc else acc * i) 1

let part2 lines =
    let junctions = parse_input lines in
    let connections = make_connections junctions |> sort_connections in
    connect_spanning junctions connections
    
