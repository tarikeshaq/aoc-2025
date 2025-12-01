let rec lines ic acc: string list =
    try
     let line = input_line ic in 
     lines ic (line :: acc)
    with End_of_file ->
        List.rev acc


let load_file file_path =
    let ic = open_in file_path in
    lines ic []
    
