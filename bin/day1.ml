open! Printf

let result = Advent.read_lines "input.txt"

let rec group input result = 
  match input with
  | [] -> result
  | "" :: rest -> group rest (0 :: result)
  | cals :: rest -> 
      group
        rest
        (match result with
        | [] -> [int_of_string cals]
        | hd :: rest -> (hd + int_of_string cals) :: rest);;

(* Part 1 *)
let rec max_of_list input cur =
  match input with
  | [] -> cur
  | hd :: rest -> max_of_list rest (max hd cur)
;;

let () = Format.sprintf "Part 1: %d" (max_of_list (group result []) 0) |> print_endline


(* Part 1.5 *)
let idiomatic_max input = List.fold_left (fun a x -> max a x) 0 input
let () = 
  Format.sprintf "Part 1(cleaner): %d" (idiomatic_max (group result [])) |> print_endline

(* Part 2 *)
(* Search a list of ints and find the maximum 3 numbers *)
let rec max3 input (m1, m2, m3) =
  match input with
  | [] -> (m1, m2, m3)
  | hd :: rest ->
      max3
        rest
        (match (m1, m2, m3) with
        | m1, m2, _ when hd > m1 -> hd, m1, m2
        | m1, m2, _ when hd > m2 -> m1, hd, m2
        | m1, m2, m3 when hd > m3 -> m1, m2, hd
        | _ -> m1, m2, m3)

let () = 
  let m1, m2, m3 = max3 (group result []) (0, 0, 0) in
  Format.sprintf "Part 2: %d" ((m1 + m2 + m3)) |> print_endline