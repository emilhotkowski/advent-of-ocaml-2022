let filename = "input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let sum_by_elf lines =
  let rec aux all acc = function 
    | [] -> (acc :: all)
    | hd :: tl ->
      if hd = "" then aux (acc :: all) 0 tl
      else aux all (acc + int_of_string hd) tl in
  List.rev (aux [] 0 lines)

let lines = read_lines filename
let elves = sum_by_elf lines

(* let biggest_calories = List.hd @@ List.rev @@ List.sort compare elves   *)
let top_three = match List.rev @@ List.sort compare elves with 
      x :: y :: z :: _ -> x + y + z
      | _ -> 0

let () = print_int top_three
