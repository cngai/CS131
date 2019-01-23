type ('terminal, 'nonterminal) symbol =
    | T of 'terminal
    | N of 'nonterminal

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

(* traverses parse tree left to right and yields list of leaves encountered *)
let rec parse_tree_leaves tree =
	match tree 
;;