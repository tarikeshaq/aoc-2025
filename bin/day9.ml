type direction =
    | H
    | V

type side = {
    d: direction;
    anchor: int;
    s: int;
    e: int;
}

let is_in_polygon (x, y) vertical_segments =
   List.filter (
        fun side ->
            side.anchor > x  && side.s < y && y < side.e
        ) vertical_segments |>
    List.length |> (fun i -> i mod 2 = 1)



let make_rec_sides (x1, y1) (x2, y2) =
    let min_y = min y1 y2 in
    let min_x = min x1 x2 in
    let max_y = max y1 y2 in
    let max_x = max x1 x2 in
[{d = V; anchor = x1; s = min_y; e = max_y};
    {d = V; anchor = x2; s = min_y; e = max_y};
{d = H; anchor = y1; s = min_x; e = max_x};
        {d = H; anchor = y2; s = min_x; e = max_x}]

let intersects rec_sides poly_sides =
    List.exists(fun side ->
            List.exists(fun rec_side ->
                if side.d = rec_side.d then false
                else
                   side.anchor > rec_side.s && side.anchor < rec_side.e 
                    && rec_side.anchor > side.s && rec_side.anchor < side.e
            ) rec_sides 
        ) poly_sides
        

let contained (x1, y1) (x2, y2) poly_sides vertical_sides =
    let rec_sides = make_rec_sides (x1, y1) (x2, y2) in
    let are_all_vertix_ok = (is_in_polygon (x1, y1) vertical_sides) && (is_in_polygon (x2, y2) vertical_sides) && (is_in_polygon (x1, y2) vertical_sides) && (is_in_polygon (x2, y1) vertical_sides) in
    let intesect = intersects rec_sides poly_sides in
    are_all_vertix_ok && (not intesect)

let make_sides points =
    List.fold_left (fun acc ((x1, y1), (x2, y2)) ->
            if x1 = x2 && y1 = y2 then acc 
            else
            let min_y = min y1 y2 in
            let min_x = min x1 x2 in
            let max_y = max y1 y2 in
            let max_x = max x1 x2 in
            if x1 = x2 then 
            { d = V; anchor = x1; s = min_y; e = max_y; }  :: acc 
            else if y1 = y2 then
            { d = H; anchor = y1; s = min_x; e = max_x; }  :: acc 
            else acc 
        ) [] (Util.all_pairs points)

let make_vertical_segments sides =
    List.filter (fun side -> side.d = V) sides

let parse_point s =
    match String.split_on_char ',' s with
     | i :: j :: [] -> (int_of_string i, int_of_string j)
     | _ -> failwith "unexpected input"

let max_area_2 points =
    let sides = make_sides points in
    let vertical_segments = make_vertical_segments sides in
    List.fold_left (fun acc (x1, y1) ->
        max acc (List.fold_left (fun inner (x2, y2) ->
            if contained (x1, y1) (x2, y2) sides vertical_segments  then begin
            let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1) in
            max area inner 
            end
            else inner
        ) acc points)
    ) 0 points


let max_area points =
    List.fold_left (fun acc (x1, y1) ->
        max acc (List.fold_left (fun inner (x2, y2) ->
            let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1) in
            max area inner 
        ) acc points)
    ) 0 points


let part1 lines =
    List.map parse_point lines
    |> max_area


let part2 lines =
    List.map parse_point lines
    |> max_area_2


