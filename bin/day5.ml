type range = {
    rstart: int;
    rend : int;
}

type parseType = 
    Range | ID 

let parse_range line =
    match String.split_on_char '-' line with
        | start :: rend:: _ ->
            let rstart = int_of_string start in
            let rend = int_of_string rend in
            {
            rstart = rstart;
            rend = rend;
            }
        | _ -> failwith "unexpected misformated line"
    


let parse_input lines =
    let (res, _) = List.fold_left (fun acc line ->
            let ((ranges, ids), parseT) = acc in
            if line = "" then ((ranges, ids), ID)
            else
            match parseT with
            | Range -> parse_range line |> (fun range -> ((range :: ranges, ids), parseT))
            | ID -> int_of_string line |> (fun id -> ((ranges, id :: ids), parseT))
        ) (([], []), Range) lines in
    res

let merge_sorted ranges =
    let (res, last_range) = List.fold_left (fun (acc, active_range) range ->
        match active_range with 
        | None -> (acc, Some(range))
        | Some(old) ->
            if old.rend >= range.rstart then (acc, Some({
                rstart = old.rstart;
                rend = (max old.rend range.rend);
            }))
            else
            (old :: acc, Some(range))
    ) ([], None) ranges in
    match last_range with
    | Some(r) -> r :: res
    | _ -> res

let merge_ranges ranges =
   List.fast_sort (fun r1 r2 -> r1.rstart - r2.rstart) ranges
    |> merge_sorted

let valid_ids ids ranges =
    List.filter (fun id -> 
        List.find_opt (fun range -> id >= range.rstart && id <= range.rend) ranges
        |> Option.is_some
    ) ids



let part1 text =
    parse_input text 
    |> (fun (ranges, ids) -> 
        merge_ranges ranges
        |> valid_ids ids
        |> List.length
    ) 

let count_ids ranges =
    List.fold_left (fun acc range ->
        acc + (range.rend - range.rstart + 1)
    ) 0 ranges

let part2 text =
   parse_input text
    |> (fun (ranges, _) -> 
        merge_ranges ranges
        |> count_ids
    )
