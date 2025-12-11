(* Yes, this calls a library that runs the linear programming for me. I thought about implemementing simplex and will eventually do that... but for now, smarter people built smarter solutions *)

open Lp

let solve_min_sum a b =
  let n = Array.length a.(0) in 
  let m = Array.length a in
  
  let vars = Array.init n (fun i -> var ~integer:true (Printf.sprintf "x%d" i)) in
  
  let objective = minimize (
    Array.fold_left (fun acc v -> acc ++ v) (c 0.0) vars
  ) in
  
  let constraints = Array.init m (fun i ->
    let row_expr = Array.mapi (fun j coef ->
      c coef *~ vars.(j)
    ) a.(i) in
    let lhs = Array.fold_left (fun acc e -> acc ++ e) (c 0.0) row_expr in
    lhs =~ c b.(i)  
  ) |> Array.to_list in
  
  let problem = make objective constraints in
  
  match Lp_glpk.solve problem with
  | Ok (obj_value, _) ->
      obj_value
  | Error msg ->
      failwith msg
