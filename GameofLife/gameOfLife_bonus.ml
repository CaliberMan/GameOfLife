#use "list_tools.ml";;
#use "game_of_life.ml";;

(* Takes in a file name and a list. Fills the file with the elements of the list *)
let write filename list =
let oc = open_out filename in
let rec aux = function
[] -> close_out oc
| e :: l -> Printf.fprintf oc "%s " e ; aux l
in aux list ;;

(* Takes in a ml life and turns each sentence into an element of a list *)

let load name =
let ic = open_in name in
let try_read () =
try Some ( input_line ic ) with 
End_of_file -> None in
let rec loop () = 
match try_read () with
Some s -> s ::( loop () )
| None -> close_in ic; []
in loop () ;;

(**)

let load_board name =
let ic = open_in name in
let try_read () =
try Some (input_line ic) with End_of_file -> None in
let rec loop () = 
match try_read () with
Some s -> let rec aux x n =
          match s.[x] with
            _ when x = n -> []
           |' '| ']'|'['|';' -> aux (x+1) n
           |_ -> (int_of_char s.[x]-48)::(aux (x+1) n) in aux 0 (String.length(s)-1)::(loop()) 
|None -> close_in ic; [] in loop();;

let listF list n = 
"["^(let rec aux l n =
      if n = 0 then "["^aux l (n+1)
      else
      match l with
      []-> "]"
      |[e]-> string_of_int(e)^"]"
      |e::l1 -> string_of_int(e)^";"^ (aux l1 n) in aux list n);;

let save_board filename list =
let oc = open_out filename in
let rec aux n = function
[] -> close_out oc
|e::[]-> Printf.fprintf oc "%s]" (listF e n); aux (n+1) []
|e::l -> Printf.fprintf oc "%s;\n" (listF e n); aux (n+1) l in aux 0 list ;;

(*Patterns*)

let init_pattern size pattern =
let board = init_board (size, size) 0 in 
let rec aux pattern board =
match pattern with
[]-> board
|(a, b)::l-> aux l (put_cell 1 (a, b) board)
in aux pattern board;;

let new_game_pattern board size nb =
game board size nb;;

(*Life*)

let remaining board =
let rec aux = function
[]-> false
|e::l -> let rec aux2 b2 =
         match b2 with
         []-> aux l
         |p::l1 -> if p = 1 then true else aux2 l1
in aux2 e in aux board;;

let new_game_survival size nb_cells =
clear_graph();
let board = new_board size nb_cells in
let rec aux board =
if remaining board = true then 
begin
draw_board (next_generation board size) 10; aux (next_generation board size) 
end
else draw_board (next_generation board size) 10
in aux board;;

let new_game_pattern_survival board size =
let rec aux board =
if remaining board = true then 
begin
draw_board (next_generation board size) 10; aux (next_generation board size) 
end
else draw_board (next_generation board size) 10
in aux board;;

(*Optimisations*)

let rec game_op board size n =
match n with
0 -> draw_board board 10
|_ -> game_op (next_generation board size) size (n-1);;

let new_game_op size nb_cells n =
let board = new_board size nb_cells in game_op board size n;;

(*Main Bonus*)

