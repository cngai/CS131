type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let my_convert_grammar_test0 = convert_grammar awksub_grammar

let my_get_alt_list_test0 = get_alt_list awksub_rules Num

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let parse_tree =
    (Node ("N1", [Leaf "L1"; (Node ("N2", [Leaf "L2"])); Leaf "L3"]))

let my_is_leaf_test0 = is_leaf (Leaf "L3")
let my_is_leaf_test1 = not (is_leaf (Node ("N2", [Leaf "L2"])))
let my_is_leaf_test2 = not (is_leaf (Node ("N1", [Leaf "L1"; (Node ("N2", [Leaf "L2"])); Leaf "L3"])))

let my_parse_tree_leaves_test0 = parse_tree_leaves parse_tree