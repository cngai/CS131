let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type fr_nonterminals =
  | One | Two | Three | Four | Five | Six

let fr_rules =
   [One, [T"+"; N One; T"-"];
    One, [N Five];
    One, [N Two];
    One, [N Three; N Two];
    One, [N Two; N Three];
    Two, [T"^"; N One];
    Three, [T","];
    Three, [T"!"];
    Four, [T"="];
    Four, [T"~"];
    Five, [T"n"];
    Five, [T")"];
    Five, [T";"];
    Five, [T"+"];]

let fr_grammar = One, fr_rules
let converted_grammar = convert_grammar fr_grammar

let make_matcher_test =
	((make_matcher converted_grammar accept_all ["+"; "+"; "+"; "+"; "n"; "-"; "-"; "-"; "-"; "h"; "f"; "&"])
	= Some ["h"; "f"; "&"])

let fr_frag = ["+"; "^"; "+"; "^"; ";"; "!"; "-"; "-"]

let make_parser_test =
	(make_parser converted_grammar fr_frag)
	= Some (Node (One, [Leaf "+"; Node (One, [Node (Two, [Leaf "^"; Node (One,
		[Leaf "+"; Node (One, [Node (Two, [Leaf "^"; Node (One, [Node (Five, [Leaf ";"])])]);
			Node (Three, [Leaf "!"])]); Leaf "-"])])]); Leaf "-"]))

let make_parser_test_inverse =
	match make_parser converted_grammar fr_frag with
	| Some tree -> parse_tree_leaves tree = fr_frag
	| _ -> false