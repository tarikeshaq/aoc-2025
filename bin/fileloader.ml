let rec lines acc ic =
    try
     let line = input_line ic in 
     lines (line :: acc) ic 
    with End_of_file ->
        In_channel.close ic;
        List.rev acc

let load_text file_path =
    let ic = open_in file_path in
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content


let load_file file_path = open_in file_path |> lines [] 
    
