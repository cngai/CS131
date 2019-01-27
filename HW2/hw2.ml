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

(* takes in symbol ==> Leaf x, returns only x *)
let delete_type symbol =
    match symbol with
    | Leaf x -> x
 (*    | Node (x, y) -> x *)
;;

(* mutually recursive function that adds leaves to leaves_list *)
let rec parse_tree_list tree leaves_list =
	match tree with
	| Leaf x -> x :: leaves_list
	| Node (x, y) -> iterate_list y leaves_list
(* iterates through list of symbols if symbol is a Node *)
and iterate_list l leaves_list =
	match l with
	| [] -> leaves_list
	| h :: t -> 
		if is_leaf h then (delete_type h) :: (iterate_list t leaves_list)
		else (parse_tree_list h leaves_list) @ (iterate_list t leaves_list)
;;

(* traverses parse tree left to right and yields list of leaves encountered *)
let parse_tree_leaves tree =
	parse_tree_list tree []
;;

let match_term symbol accept frag =
    match frag with
    | [] -> None
    | h :: t ->
        if h = symbol then (accept t)
        else None
;;

let rec append_matchers m1 matchers =
    match matchers with
    | [] -> m1
    | h :: t -> (append_matchers (fun accept -> m1 (fun frag -> h accept frag)) t)
;;

(* non-terminal symbol matcher *)
let rec match_nt pf ss al accept frag =
    match al with
    | [] -> None
    | h1 :: t1 ->
        let matcher = (
            match h1 with
            | [] -> None
            | h2 :: t2 -> 
                (append_matchers 
                    (match_maker pf h2)
                    (List.map (match_maker pf) t2)
                accept frag
                )
        ) in
        if matcher = None then match_nt pf ss t1 accept frag
        else matcher
and match_maker pf symbol =
    match symbol with
    | N x -> match_nt pf x (pf x)
    | T x -> match_term x
;;

(* returns matcher for grammar gram *)
let make_matcher gram =
    match gram with
    | (start_symb, prod_func) -> let alt_list = (prod_func start_symb) in
        (fun accept frag -> match_nt prod_func start_symb alt_list accept frag)
;;

