let filename = "input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let parse_pairs x = 
  let split_pairs x = match String.split_on_char ',' x with
    x :: y :: _ -> (x, y)
    | _ -> ("", "") in
  let parse_pair x = match String.split_on_char '-' x with
    x :: y :: _ -> (int_of_string x, int_of_string y)
    | _ -> (0, 0) in
  List.map (fun (x, y) -> (parse_pair x, parse_pair y)) @@ List.map split_pairs x

(* let is_contained ((a, b), (c, d)) =
  (a <= c && d <= b) || (c <= a && b <= d) *)

let is_partially_contained ((a, b), (c, d)) =
  (a <= d && b >= c) ||
  (d <= a && c >= b)
    
let int_of_bool b = if b then 1 else 0
let contained_count contained_fun = List.fold_left (fun x y -> x + int_of_bool y) 0 @@
  List.map contained_fun @@
  parse_pairs @@ 
  read_lines filename

(* let () = print_int (contained_count is_contained) *)
let () = print_int (contained_count is_partially_contained)