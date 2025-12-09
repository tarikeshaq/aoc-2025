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
    arr: int array;
}

let mk_disjoint n =
    let arr = Array.make n 0 in
    for i = 0 to (n-1) do
        arr.(i) <- i;
    done;
    {
        arr = arr;
    }

let rec find_rep dist n =
    let arr = dist.arr in
    if arr.(n) = n then n
    else
    find_rep dist arr.(n)

let debug_dist dist =
    for i = 0 to (Array.length dist.arr - 1) do
        Printf.printf "Idx %d has value %d and root %d\n" i dist.arr.(i) (find_rep dist i); 
    done


let is_same_set dist v1 v2 =
    find_rep dist v1 = find_rep dist v2

let join_sets dist v1 v2 =
    let rep1 = find_rep dist v1 in
    let rep2 = find_rep dist v2 in
    dist.arr.(rep1) <- rep2


(* just in case the numbers are really big *)
let safe_distance (x1, y1, z1) (x2, y2, z2) =
    let dx = abs (x1 - x2) in
    let dy = abs (y1 - y2) in
    let dz = abs (z1 - z2) in
    let max_comp = max dx dy |> max dz in
    if max_comp = 0 then 0.0 else
    let max_float = float_of_int max_comp in
    let dxf = float_of_int dx /. max_float in
    let dyf = float_of_int dy /. max_float in
    let dzf = float_of_int dz /. max_float in
    max_float *. sqrt (dxf *. dxf +. dyf *. dyf +. dzf *. dzf)
    

(* Ideally we keep track of the num of elements in each set as we build them up, but meh *)
let get_largest_x x dist =
   let n = Array.length dist.arr in
    let res = Array.make n 0 in 
    for i = 0 to (n-1) do
      let root = find_rep dist i in
      res.(root) <- (res.(root) + 1)
    done;
    Array.to_list res |> List.fast_sort (fun a b -> b - a)
    |> List.take x
