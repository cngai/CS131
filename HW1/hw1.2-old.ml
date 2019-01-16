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

(* returns fixed point of given function *)
let rec computed_fixed_point eq f x =
	if eq (f x) x then x
	else computed_fixed_point eq f (f x)
;;

(* takes in rule and checks to see if it is terminal *)
let is_terminal rule =
	match rule with
	| N x -> false
	| T x -> true
;;

(* takes in a rules and checks to see if it is non-terminal *)
let rec contains_nt_rule rules =
	match rules with
	| [] -> false
	| h :: t ->
		if is_terminal h then contains_nt_rule t
		else true
;;

(* takes in list of rules and returns only terminal rules *)
let rec get_term_rules rules =
	match rules with
	| [] -> rules
	| (h1, h2) :: t ->
		if not (contains_nt_rule h2) then (h1, h2) :: (get_term_rules t)
		else get_term_rules t
;;

(* removes all terminal rules from rhs of rules pair *)
let rec remove_terminals rhs =
	match rhs with
	| [] -> []
	| h :: t ->
		if is_terminal h then remove_terminals t
		else h :: (remove_terminals t)
;;

(* take list of rules but only return lhs *)
let rec get_lhs rules =
	match rules with
	| [] -> []
	| h :: t -> (N(fst h) :: get_lhs t)
;;

(* takes list of rules and returns list minus excl_rule *)
(* makes sure nonterminal rule is not looping through itself *)
let exclude_own_rule rules excl_rule =
	set_diff rules [excl_rule]
;;

(* check to see if unknown rules can be reached from start symbol *)
let rec good_rules u_rules term_rules =
	match u_rules with
	| [] -> term_rules
	| (h1, h2) :: t ->
		if subset (remove_terminals h2) ((get_lhs (exclude_own_rule u_rules (h1, h2))) @ (get_lhs term_rules)) && (not (find (h1, h2) term_rules))
		then good_rules t ((h1, h2) :: term_rules)
		else good_rules t term_rules
;;

let get_nt_rules all_rules term_rules = set_diff all_rules term_rules;;

(* remove all non-terminal rules that cannot be immediately reached by start symbol *)
let rec get_rules_with_start_symb rules start_symbol =
	match rules with
	| [] -> []
	| (h1, h2) :: t ->
		if h1 = start_symbol then (h1, h2) :: (get_rules_with_start_symb t start_symbol)
		else get_rules_with_start_symb t start_symbol
;;

(* let rec filter_nt_rules nt_rules rhs_nt_start =
	match nt_rules with
	| [] -> []
	| (h1, h2) :: t ->
		if find (N(h1)) rhs_nt_start then () *)

(*
collect and return list of all reachable rules
let rec get_new_rules u_rules term_rules =
	let terminals = get_nt_rules (good_rules u_rules term_rules) in
	if (List.length term_rules) = (List.length terminals) then terminals
	else get_new_rules u_rules terminals
;;*)

(* get list of all non-terminal rhs rules *)
let rec get_rhs_nt nt_rules =
	match nt_rules with
	| [] -> []
	| h :: t -> remove_terminals (snd h @ get_rhs_nt t)
;;

(* remove all terminal rules that do not match symbol type of any non-terminal rhs rules *)
let rec filter_term_rules term_rules rhs_nt_rules =
	match term_rules with
	| [] -> []
	| (h1, h2) :: t ->
		if find (N(h1)) rhs_nt_rules then (h1, h2) :: (filter_term_rules t rhs_nt_rules)
		else filter_term_rules t rhs_nt_rules
;;

(* reorders new rules to be in same order as orig_rules *)
let rec reorder_rules orig_rules new_rules =
	match orig_rules with
	| [] -> []
	| h :: t ->
		if find h new_rules then h :: (reorder_rules t new_rules)
		else reorder_rules t new_rules
;;

(* ways to change *)
(* let term_rules = get_term_rules (snd g) || omit let rules = .. *)
(* let new_rules = reorder_rules rules ... *)
(* let non_term_rules = set_diff rules term_rules *)

(* returns a copy of grammar g with all unreachable rules removed *)
let filter_reachable g =
	let rules = snd g in
	let term_rules = get_term_rules rules in
	let all_nt_rules = (get_nt_rules (good_rules (set_diff rules term_rules) term_rules) term_rules) in
	let nt_rules_start = get_rules_with_start_symb all_nt_rules (fst g) in
	(* let nt_rules = get_rhs_nt nt_rules_start in *)
	let nt_rules = (filter_term_rules all_nt_rules (get_rhs_nt nt_rules_start)) in
	let filtered_term_rules = (set_union (get_rules_with_start_symb term_rules (fst g)) (filter_term_rules term_rules (get_rhs_nt nt_rules))) in
	
	(*let filtered_term_rules = filter_term_rules term_rules (get_rhs_nt nt_rules) in*)
	fst g,
	(reorder_rules rules (nt_rules @ filtered_term_rules))
;;
