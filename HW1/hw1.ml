(* define symbol *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* auxiliary function for subset *)
let rec find e l =
	match l with
	| [] -> false
	| h :: t -> h = e || find e t
;;

(* returns true iff every element of list a is an element of list b *)
let rec subset a b =
	match a with
	| [] -> true
	| h :: t ->
		if find h b then subset t b
		else false
;;

(* returns true iff two sets are equal *)
let equal_sets a b = 
	(subset a b) && (subset b a)
;;

(* return union of two sets *)
let rec set_union a b =
	match a with
	| [] -> b
	| h :: t  ->
		if find h b then set_union t b
		else set_union t (h :: b)
;;

(* return intersection of two sets *)
let rec set_intersection a b =
	match a with
	| [] -> []
	| h :: t ->
		if find h b then h :: (set_intersection t b)
		else set_intersection t b
;;

(* returns difference of two sets *)
let rec set_diff a b =
	match a with
	| [] -> []
	| h :: t ->
		if find h b then set_diff t b
		else h :: (set_diff t b)
;;