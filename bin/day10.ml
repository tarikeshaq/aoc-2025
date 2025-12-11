type button = {
    ids: int list;
}


type input = {
    end_state: int;
    joltage: int array;
    length: int;
    buttons: button list;
}


let remove_char f s =
    let b = Buffer.create (String.length s) in
    String.iter (fun c -> if not (f c) then Buffer.add_char b c) s;
    Buffer.contents b

let parse_joltage elem = 
 remove_char (fun c -> c = '{' || c = '}') elem
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list
    
let parse_end_state end_state =
    String.fold_left (fun (acc, l) c ->
      if c = ']' || c = '[' then (acc, l)
      else
        let num = if c = '.' then 0 else 1 in
        ((acc lsl 1) lor num, l+1)
    ) (0, 0) end_state

let parse_button elem =
    let ids = remove_char (fun c -> c = '(' || c = ')') elem
    |> String.split_on_char ','
    |> List.map int_of_string  in
    {
        ids = ids;
    }

let parse_buttons buttons =
    let rec aux acc l =
        match l with
        | joltage_s :: [] -> (acc, parse_joltage joltage_s)
        | elem :: rest ->
            aux (parse_button elem :: acc) rest
        | [] -> (acc, Array.make 0 1)
    in
    aux [] buttons

let parse_line line =
   match String.split_on_char ' ' line with
   | end_state :: rest -> 
    let (end_state_num, l) = parse_end_state end_state in
    let (buttons, joltage) = parse_buttons rest in
        {
            end_state = end_state_num;
            length = l;
            buttons = buttons;
            joltage = joltage;
        }
   | _ -> failwith "unexpected input"


let find_shortest_path input =
    let q = Queue.create () in
    let seen = Hashtbl.create 10 in
    let rec aux q seen =
        let (state, dist) = Queue.pop q in
        if state = input.end_state then dist 
        else begin
        Hashtbl.add seen state true;
        List.iter (fun button -> 
            let res = List.fold_left (fun acc id ->
                let mask = (1 lsl (input.length - id - 1)) in
                let new_num = acc lxor mask in
                new_num 
            ) state button.ids in
            if Hashtbl.find_opt seen res |> Option.is_none then
            Queue.push ((res, dist+1)) q
        ) input.buttons;
        aux q seen
        end
    in
    Queue.push ((0, 0)) q;
    aux q seen


let make_lp_input inp =
    let num_rows = Array.length inp.joltage in
    let num_cols = List.length inp.buttons in
    let arr = Array.init num_rows (fun _ -> Array.make num_cols 0.0) in
    let results = Array.init num_rows (fun i -> float_of_int inp.joltage.(i)) in
    let buttons = Array.of_list inp.buttons in
    for i = 0 to num_rows - 1 do
        for j = 0 to num_cols - 1 do
            let button = buttons.(j) in
            if List.exists (fun id -> 
                id = i
            )  button.ids then arr.(i).(j) <- 1.0;
        done;
    done;
    (arr, results)
    
    
let lp_solve inp =
    let (a, b) = make_lp_input inp in
    Lpu.solve_min_sum a b


let part1 lines =
    List.map parse_line lines 
        |> List.map find_shortest_path
    |> List.fold_left (fun acc num -> acc + num) 0
 
let part2 lines =
    List.map parse_line lines 
        |> List.map lp_solve 
    |> List.fold_left (fun acc num -> acc +. num) 0.0
       
