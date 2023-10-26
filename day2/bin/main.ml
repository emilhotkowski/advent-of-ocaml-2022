let filename = "input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let rounds = List.map (fun x -> (x.[0], x.[2])) @@ read_lines filename

(* let calculate_score lst = 
  let score x y = 
    match x, y with
      'A', 'X' -> 3
      | 'A', 'Y' -> 6
      | 'A', 'Z' -> 0 
      | 'B', 'X' -> 0
      | 'B', 'Y' -> 3
      | 'B', 'Z' -> 6
      | 'C', 'X' -> 6
      | 'C', 'Y' -> 0
      | _ -> 3 in
  let score_choice = function
    | 'X' -> 1
    | 'Y' -> 2
    | _ -> 3 in
  let rec aux cnt = function
    | [] -> cnt
    | (x, y) :: tl -> 
      aux (cnt + (score x y) + (score_choice y)) tl in
  aux 0 lst *)

  let calculate_score lst = 
    let score x y = 
      match x, y with
        'A', 'X' -> 0 + 3
        | 'A', 'Y' -> 3 + 1
        | 'A', 'Z' -> 6 + 2 
        | 'B', 'X' -> 0 + 1
        | 'B', 'Y' -> 3 + 2
        | 'B', 'Z' -> 6 + 3
        | 'C', 'X' -> 0 + 2
        | 'C', 'Y' -> 3 + 3
        | _ -> 6 + 1 in
    let rec aux cnt = function
      | [] -> cnt
      | (x, y) :: tl -> 
        aux (cnt + (score x y)) tl in
    aux 0 lst
let () = print_int (calculate_score rounds)