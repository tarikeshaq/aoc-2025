let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let join_list_to_int l = List.to_seq l |> String.of_seq |> int_of_string 

let make_matrix_of lines f =
     List.map (fun s -> 
        String.to_seq s |> Seq.map (fun c -> f c) |> Array.of_seq
    ) lines |> Array.of_list

let arr_fold_i f acc arr =
    let rec aux f acc arr i =
     if i = Array.length arr then acc
     else
        aux f (f i acc arr.(i)) arr (i + 1)
    in
    aux f acc arr 0
