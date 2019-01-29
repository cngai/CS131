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

(* iterates through each alternative list to check all arbitrary parse trees *)
let rec iterate_alt_list start_symb prod_func alt_list accept frag =
    match alt_list with
    (* reached end of alt_list, you didn't match anything, return None *)
    | [] -> None
    | h_alt_list :: rem_alt_list ->
        (* iterate through individual list of rules *)
        let none_or_some = (iterate_each_list h_alt_list prod_func accept frag) in
        match none_or_some with
        (* keep trying to find element with rest of alt_list *)
        | None -> iterate_alt_list start_symb prod_func rem_alt_list accept frag
        | Some x -> Some x
(* iterate through each list of rules sequentially *)
and iterate_each_list new_list prod_func accept frag =
    match new_list with
    (* made it to end of rhs_rules, will return Some x *)
    | [] -> accept frag 
    | _ ->
        match frag with
        (* reached end of frag, no suffix, will return Some [] *)
        | [] -> None
        | h_frag :: rem_frag ->
            match new_list with
            (* made it to end of new_list *)
            | [] -> None
            | (T t_val) :: rem_new_list ->
                (* call iterate_each_list again with next new_list and remaining frag *)
                if h_frag = t_val then (iterate_each_list rem_new_list prod_func accept rem_frag)
                (* wrong path, next set of rhs_rules *)
                else None
            | (N nt_val) :: rem_new_list ->
                (* new acceptor made with remaining list *)
                let new_accept = (iterate_each_list rem_new_list prod_func accept) in
                (* call iterate_alt_list but with new nt_val as start symbol *)
                iterate_alt_list nt_val prod_func (prod_func nt_val) new_accept frag
;;

(* return matcher for grammar gram *)
let make_matcher gram =
    let start_symb = fst gram in
    let prod_func = snd gram in
    (fun accept frag -> iterate_alt_list start_symb prod_func (prod_func start_symb) accept frag)
;;


(* takes rem_deriv and returns back new deriv with starting point (head of) t_list *)
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

(* iterate through each list in derivation to get parse tree *)
let rec iterate_x_list x_list rem_deriv gram =
    match x_list with
    | [] -> []
    | h_list :: t_list ->
        match h_list with
        | (T t_val) -> ([Leaf t_val] @ (iterate_x_list t_list rem_deriv gram))
        | (N n_val) -> 
            let new_deriv = get_new_deriv t_list rem_deriv in
            (convert_deriv gram rem_deriv) @ (iterate_x_list t_list new_deriv gram)
(* take derivation of frag and turn into parse tree *)
and convert_deriv gram derivation =
    match derivation with
    | [] -> []
    | (x, x_list) :: t_deriv ->
        [Node (x, (iterate_x_list x_list t_deriv gram))]
;;

(* iterates through each alternative list to check all arbitrary parse trees *)
let rec iterate_alt_list_d start_symb prod_func alt_list accept derivation frag =
    match alt_list with
    (* reached end of alt_list, you didn't match anything, return None *)
    | [] -> None
    | h_alt_list :: rem_alt_list ->
        (* iterate through individual list of rules *)
        let none_or_some = (iterate_each_list_d h_alt_list prod_func accept (derivation@[start_symb, h_alt_list]) frag) in
        match none_or_some with
        (* keep trying to find element with rest of alt_list *)
        | None -> iterate_alt_list_d start_symb prod_func rem_alt_list accept derivation frag
        | Some x -> Some x
(* iterate through each list of rules sequentially *)
and iterate_each_list_d new_list prod_func accept derivation frag =
    match new_list with
    (* made it to end of rhs_rules, will return Some x *)
    | [] -> accept derivation frag 
    | _ ->
        match frag with
        (* reached end of frag, no suffix, will return Some [] *)
        | [] -> None
        | h_frag :: rem_frag ->
            match new_list with
            (* made it to end of new_list *)
            | [] -> None
            | (T t_val) :: rem_new_list ->
                (* call iterate_each_list again with next new_list and remaining frag *)
                if h_frag = t_val then (iterate_each_list_d rem_new_list prod_func accept derivation rem_frag)
                (* wrong path, next set of rhs_rules *)
                else None
            | (N nt_val) :: rem_new_list ->
                (* new acceptor made with remaining list *)
                let new_accept = (iterate_each_list_d rem_new_list prod_func accept) in
                (* call iterate_alt_list but with new nt_val as start symbol *)
                iterate_alt_list_d nt_val prod_func (prod_func nt_val) new_accept derivation frag
;;

(* return matcher for grammar gram *)
let get_derivation gram =
    let start_symb = fst gram in
    let prod_func = snd gram in
    (fun accept frag -> iterate_alt_list_d start_symb prod_func (prod_func start_symb) accept [] frag)
;;

(* custom accceptor function that returns the derivation of the given frag *)
let accept_all deriv string = Some (deriv, string);;

(* get derivation of frag and then turn into parse tree *)
let get_parse_tree gram frag =
    let derivation = get_derivation gram accept_all frag in
    match derivation with
    | None -> None
    | Some (deriv, s) -> 
        let tree_list = convert_deriv gram deriv in
        match tree_list with
        | [] -> None
        | h :: t ->
            Some h
;;

let make_parser gram =
    (fun frag -> get_parse_tree gram frag)
;;
