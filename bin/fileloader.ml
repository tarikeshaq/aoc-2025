let rec lines ic acc: string list =
    try
     let line = input_line ic in 
     lines ic (line :: acc)
    with End_of_file ->
        List.rev acc

let load_text file_path =
    let ic = open_in file_path in
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content


let load_file file_path =
    let ic = open_in file_path in
    lines ic []
    
