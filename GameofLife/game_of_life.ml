#use "list_tools.ml";;
#load "graphics.cma";;
open Graphics ;;

open_graph "1200x800";;

let draw_cell (x, y) size color =
let grey = rgb 127 127 127 in
set_color grey;
draw_rect (x) (y) (size) (size);
set_color color;
fill_rect (x+1)(y) (size-1) (size-1);;

let cell_color = function
| 0 -> white
| _ -> black ;;

let draw_board board cellsize = 
let rec aux b1 (x, y) = 
match b1 with
[]-> ()
|e::l1 -> let rec aux2 a (x1, y1) =
match a with
[]-> aux l1 ((x1+cellsize), y)
|p::l-> draw_cell (x1, y1) cellsize (cell_color p); aux2 l (x1, y1+cellsize)
in aux2 e (x, y) in aux board (100, 100);;

(*RULES*)

(*1 = Alive
  0 = Dead *)
let rules0 cell near =
match cell with
0 -> if near = 3 then 1 else 0
|1 -> if near = 2 || near = 3 then 1 else 0
|_ -> invalid_arg "rules0: cell must be either 0 or 1";;

let count_neighbors (x, y) board size =
let x1 = x-1 and x2 = x+1 and y1 = y-1 and y2 = y+1 in
(if x1 < 0 || y1 < 0 then 0 else (get_cell (x1, y1) board))+
(if x1 < 0 then 0 else (get_cell (x1, y) board))+
(if x1 < 0 || y2 > (size-1) then 0 else (get_cell (x1, y2) board))+
(if y1 < 0 then 0 else (get_cell (x, y1) board))+
(if y2 > (size-1) then 0 else (get_cell (x, y2) board))+
(if x2 > (size-1) || y1 < 0 then 0 else (get_cell (x2, y1) board))+
(if x2 > (size-1) then 0 else (get_cell (x2, y) board))+
(if x2 > (size-1) || y2 > (size-1) then 0 else (get_cell (x2, y2) board));;

(*LIFE*)

let rec seed_life board size nb_cell =
(*make a variable that gives a random number and decides how many reandom numbers to put in *)
let x1 = Random.int size and y1 = Random.int size in
match nb_cell with
0 -> board
|_ -> seed_life (put_cell 1 (x1, y1) board) size (nb_cell-1);;

let new_board size nb_cell =
let board = init_board (size, size) 0 in
seed_life board size nb_cell;;

let next_generation board size =
let rec aux (x, y) b1 n1 n2 =
match b1 with
_ when n1 = (size) -> []
|l -> let rec aux2 b2 (x1, y1) n =
match b2 with
_ when n = (size) -> []
|l -> (rules0 (get_cell (x1, y1) b2) (count_neighbors (x1, y1) b2 size))::(aux2 board (x1, y1+1) (n+1))
in (aux2 board (x, y) n2)::aux (x+1,y) b1 (n1+1) n2
in aux (0, 0) board 0 0;;

let rec game board size n =
match n with
0 -> draw_board board 10
|_ -> draw_board (next_generation board size) 10; game (next_generation board size) size (n-1);;

let new_game size nb_cells n =
let board = new_board size nb_cells in game board size n;;

