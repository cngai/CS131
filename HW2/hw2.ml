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

(* mutually recursive function that adds leaves to leaves_list *)
let rec parse_tree_list tree leaves_list =
	match tree with
	| Leaf x -> x :: leaves_list
	| Node (x, y) -> iterate_list y leaves_list
(* iterates through list of symbols if symbol is a Node *)
and iterate_list l leaves_list =
	match l with
	| [] -> leaves_list
	| h :: t -> (parse_tree_list h leaves_list) @ (iterate_list t leaves_list)
;;

(* traverses parse tree left to right and yields list of leaves encountered *)
let parse_tree_leaves tree =
	parse_tree_list tree []
;;

let rec matcher start_symb prod_func alt_list accept frag =
    match alt_list with
    | [] -> None (* could not find frag in entire prod_func *)
    | h_alt_list :: rem_alt_list ->
        let element = (match_element prod_func h_alt_list accept frag) in
        match element with
        | None -> matcher start_symb prod_func rem_alt_list accept frag (* keep trying to find element with rest of alt_list *)
        | Some x -> Some x
and match_element prod_func rhs_rules accept frag =
    match rhs_rules with
    | [] -> accept frag (* made it to end of rhs_rules, will return Some x *)
    | _ -> (* somewhere in middle of rhs_rules *)
        match frag with
        | [] -> None (* reached end of frag, will call matcher again with next set of rhs_rules *)
        | h_frag :: rem_frag ->
            match rhs_rules with
            | [] -> None (* made it to end of rhs_rules *)
            | (T t_val) :: rem_rules ->
                (* call match_element again with next rhs_rules and next frag *)
                if h_frag = t_val then (match_element prod_func rem_rules accept rem_frag)
                (* wrong path, next set of rhs_rules *)
                else None
            | (N nt_val) :: rem_rules ->
                (* new acceptor made with remaining rules *)
                let new_accept = (match_element prod_func rem_rules accept) in
                (* call matcher but with new nt_val as start symbol *)
                matcher nt_val prod_func (prod_func nt_val) new_accept frag
;;

let make_matcher gram =
    let start_symb = fst gram in
    let prod_func = snd gram in
    (fun accept frag -> matcher start_symb prod_func (prod_func start_symb) accept frag)
;;

let rec iterate_each_list new_list prod_func frag=
    match new_list with
    (* reached end of new_list, you didn't match anything, return None *)
    | [] -> []
    | _ ->
        match frag with
        (* reached end of frag, frag is satisfied, should only return [Leaf x :: []] which is just [Leaf x] *)
        | [] -> []
        | h_frag :: rem_frag ->
            match new_list with
            | [] -> []
            | (T t_val) :: rem_new_list ->
                (* reached LEAF NODE, want to return Leaf t_val *)
                if h_frag = t_val then ([Leaf t_val] @ (iterate_each_list rem_new_list prod_func rem_frag))
                (* keep iterating through rem_new_list with same frag *)
                else iterate_each_list rem_new_list prod_func frag
            | (N nt_val) :: rem_new_list ->
                (* take N nt_val and iterate alt list again with nt_val as new start_symbol *)
                (* will return Some (Node (val, [...])) *)
                [(Node (nt_val, (iterate_alt_list nt_val prod_func (prod_func nt_val) frag)))]                     
and iterate_alt_list start_symb prod_func alt_list frag =
    (* iterate through alt_list and try all lists in alt_list *)
    match alt_list with
    (* reached end of alt_list, you didn't match anything, return None *)
    | [] -> []
    | h_alt_list :: rem_alt_list ->
        (* iterate through individual list of rules, returns Node or Leaf *)
        (iterate_each_list h_alt_list prod_func frag)
;;

let accept_all string = Some string;;

let check_matcher gram frag =
    let start_symb = fst gram in
    let prod_func = snd gram in
    if (make_matcher gram accept_all frag) = (Some [])
    (* this Some is the actual Some in the answer *)
    then Some (Node (start_symb, (iterate_alt_list start_symb prod_func (prod_func start_symb) frag)))
    else None
;;

let make_parser gram =
    (fun frag -> check_matcher gram frag)
;;
    
