type ('terminal, 'nonterminal) symbol =
    | T of 'terminal
    | N of 'nonterminal

type ('nonterminal, 'terminal) parse_tree =
  	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  	| Leaf of 'terminal

(* convert gram1 rules into production function *)
let rec get_alt_list rules nt_val =
	match rules with
	| [] -> []
	| (h1, h2) :: t ->
		if h1 = nt_val then h2 :: (get_alt_list t nt_val)
		else get_alt_list t nt_val
;;

(* conerts hw1 style grammar to hw2 style grammar *)
let convert_grammar gram1 =
	(fst gram1), (function nt_val -> get_alt_list (snd gram1) nt_val)
;;

(* returns true if leaf, false if node *)
let is_leaf symbol =
	match symbol with
	| Node (x, y) -> false
	| Leaf x -> true
;;

(* mutually recursive function that adds leaves to leaves_list *)
let rec parse_tree_list tree leaves_list =
	match tree with
	| Leaf x -> (Leaf x) :: leaves_list
	| Node (x, y) -> iterate_list y leaves_list
(* iterates through list of symbols if symbol is a Node *)
and iterate_list l leaves_list =
	match l with
	| [] -> leaves_list
	| h :: t -> 
		if is_leaf h then h :: (iterate_list t leaves_list)
		else (parse_tree_list h leaves_list) @ (iterate_list t leaves_list)
;;

(* traverses parse tree left to right and yields list of leaves encountered *)
let rec parse_tree_leaves tree =
	parse_tree_list tree []
;;