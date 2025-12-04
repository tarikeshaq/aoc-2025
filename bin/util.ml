let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let join_list_to_int l =
    l |> List.to_seq |> String.of_seq |> int_of_string 

let make_matrix_of (lines: string list) (f: char -> 'a): 'a array array =
     List.map (fun s -> 
        Seq.map (fun c -> 
           f c
        ) (String.to_seq s)
        |> Array.of_seq
    ) lines |> List.to_seq |> Array.of_seq

