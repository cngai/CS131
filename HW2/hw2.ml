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

let rec get_new_deriv t_list rem_deriv =
    match t_list with
    | [] -> []
    | (T t_val) :: rem_t_list -> []
    | (N n_val) :: rem_t_list -> (* n_val holds Incrop *)
        match rem_deriv with
        | [] -> []
        | (x, x_list) :: t_deriv -> (* x holds Lvalue, Expr, Term, ... *)
            if n_val = x then rem_deriv
            else get_new_deriv t_list t_deriv
;;

let rec iterate_x_list x_list rem_deriv gram =
    match x_list with
    | [] -> []
    | h_list :: t_list ->
        match h_list with
        | (T t_val) -> ([Leaf t_val] @ (iterate_x_list t_list rem_deriv gram))
        | (N n_val) -> 
            let new_deriv = get_new_deriv t_list rem_deriv in
            (convert_deriv gram rem_deriv) @ (iterate_x_list t_list new_deriv gram)
and convert_deriv gram derivation =
    match derivation with
    | [] -> []
    | (x, x_list) :: t_deriv ->
        [Node (x, (iterate_x_list x_list t_deriv gram))]
;;


let make_parser gram =
    (fun derivation -> convert_deriv gram derivation)
;;
