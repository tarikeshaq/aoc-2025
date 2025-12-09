let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let get_combination n m =
    let ( let* ) xs f = List.concat_map f xs in
    let* i = List.init n Fun.id in
    let* j = List.init m Fun.id in
    [(i, j)]

let matrix_indices matrix =
    let n = Array.length matrix in
    let m = Array.length matrix.(0) in
    get_combination n m 


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

module IntMap = Map.Make (struct
    type t = int

    let compare = compare
end)

type distjoint = {
    parents: int array;
    size: int array;
}

let mk_disjoint n =
    let arr = Array.make n 0 in
    let size = Array.make n 1 in
    for i = 0 to (n-1) do
        arr.(i) <- i;
    done;
    {
        parents = arr;
        size = size;
    }

let rec find_rep dist n =
    if dist.parents.(n) = n then n
    else begin
      let root = find_rep dist dist.parents.(n) in
      dist.parents.(n) <- root;
      root
    end 

let debug_dist dist =
    for i = 0 to (Array.length dist.parents - 1) do
        Printf.printf "Idx %d has value %d and root %d\n" i dist.parents.(i) (find_rep dist i); 
    done


let is_same_set dist v1 v2 =
    find_rep dist v1 = find_rep dist v2

let join_sets dist v1 v2 =
    let rep1 = find_rep dist v1 in
    let rep2 = find_rep dist v2 in
    if rep1 <> rep2 then begin
     dist.parents.(rep1) <- rep2;
     dist.size.(rep2) <- dist.size.(rep2) + dist.size.(rep1);
     true
    end
    else
    false
    


let squared_distance (x1, y1, z1) (x2, y2, z2) =
    let dx = x1 - x2 in
    let dy = y1 - y2 in
    let dz = z1 - z2 in
    dx * dx + dy * dy + dz * dz   

let get_largest_x x dist =
    let (_, res) = Array.fold_left (fun (i, acc) e ->
        let res = if i = e then dist.size.(i) :: acc else acc in
        (i + 1, res)
    ) (0, []) dist.parents in
    List.fast_sort (fun a b -> b - a) res
    |> List.take x
