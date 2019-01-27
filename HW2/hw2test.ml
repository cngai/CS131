(*type awksub_nonterminals =
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
*)

(* let my_is_leaf_test0 = is_leaf (Leaf "L3")
let my_is_leaf_test1 = not (is_leaf (Node ("N2", [Leaf "L2"])))
let my_is_leaf_test2 = not (is_leaf (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])])))

let my_parse_tree_leaves_test0 = parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
let my_parse_tree_leaves_test1 = parse_tree_leaves (Leaf 3)
let my_parse_tree_leaves_test2 = parse_tree_leaves (Node ("N1", [Leaf "L1"; (Node ("N2", [Leaf "L2"])); Leaf "L3"]))
let my_parse_tree_leaves_test3 = parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5]); Leaf 6]))
let my_parse_tree_leaves_test3 = parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5]); Leaf 6; Node ("-", [Leaf 7; Leaf 8])])) *)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x


type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
     [[N Num];
      [N Lvalue];
      [N Incrop; N Lvalue];
      [N Lvalue; N Incrop];
      [T"("; N Expr; T")"]]
     | Lvalue ->
     [[T"$"; N Expr]]
     | Incrop ->
     [[T"++"];
      [T"--"]]
     | Binop ->
     [[T"+"];
      [T"-"]]
     | Num ->
     [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
      [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])


let test0 =
  ((make_matcher awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((make_matcher awkish_grammar accept_all ["9"])
   = Some [])

let test2 =
  ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some ["+"])

let test3 =
  ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

(* This one might take a bit longer.... *)
let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some [])

 let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])

