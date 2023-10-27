module SetChar = Set.Make(Char)

let filename = "input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* let split_in_two x =
  let length = String.length x in
  let first_half = String.sub x 0 (length / 2) in
  let second_half = String.sub x (length / 2) (length / 2) in
  (first_half, second_half) *)

let split_in_three x =
  let rec aux = function
  | x :: y :: z :: tl ->
    (x, y, z) :: aux tl
  | _ -> [] in
  aux x

let explode_string s = List.init (String.length s) (String.get s)

(* let find_intersection (x, y) = 
  let make_set (lst: char list) = List.fold_right SetChar.add lst SetChar.empty in
  let set_x = make_set (explode_string x) in
  let set_y = make_set (explode_string y) in
  List.hd @@ SetChar.elements @@ SetChar.inter set_x set_y  *)

let find_intersection_v2 (x, y, z) = 
  let make_set (lst: char list) = List.fold_right SetChar.add lst SetChar.empty in
  let set_x = make_set (explode_string x) in
  let set_y = make_set (explode_string y) in
  let set_z = make_set (explode_string z) in
  List.hd @@ SetChar.elements @@ SetChar.inter set_z @@ SetChar.inter set_x set_y

let scoring x = 
  if 'a' <= x && x <= 'z' then (Char.code x) - (Char.code 'a') + 1
  else (Char.code x) - (Char.code 'A') + 27

(* let intersections = List.map find_intersection @@ 
  List.map split_in_two @@ 
  read_lines filename *)

let intersections = List.map find_intersection_v2 @@ 
  split_in_three @@ 
  read_lines filename

let score = List.fold_left (+) 0 @@
  List.map scoring intersections

let () = print_int score