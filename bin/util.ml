let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let join_list_to_int l =
    l |> List.to_seq |> String.of_seq |> int_of_string 



