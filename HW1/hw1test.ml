let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [2;4]
let my_subset_test2 = subset [1;1] [4;1]
let my_subset_test3 = subset [1;1] [1]
let my_subset_test4 = subset [1;2;3;4] [1;9;2;8;3;7;4;6]
let my_subset_test5 = not (subset [1;5;4] [3;5;4;2;6])
let my_subset_test6 = not (subset [1] [])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1] [1]
let my_equal_sets_test2 = equal_sets [1;4] [1;4]
let my_equal_sets_test3 = equal_sets [1;2;3] [1;1;2;2;3;3]
let my_equal_sets_test4 = equal_sets [1;1;2] [2;2;1]
let my_equal_sets_test5 = not (equal_sets [1;2] [2;4])
let my_equal_sets_test6 = not (equal_sets [3;5] [])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [] [1]) [1]
let my_set_union_test2 = equal_sets (set_union [1] [2;3]) [1;2;3]
let my_set_union_test3 = equal_sets (set_union [1;2] [2;3]) [1;2;3]
let my_set_union_test4 = equal_sets (set_union [1;2;3;4] [5;6;7;8]) [1;2;3;4;5;6;7;8]
let my_set_union_test5 = equal_sets (set_union [2;3;3;3;3] [5;3;3;1;]) [1;2;3;5]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [] [1]) []
let my_set_intersection_test2 = equal_sets (set_intersection [1;2] [2;3]) [2]
let my_set_intersection_test3 = equal_sets (set_intersection [1;1;2] [3;3;2;2;1]) [1;2]
let my_set_intersection_test4 = equal_sets (set_intersection [1;2;3;4] [3;4;5;6]) [3;4]
let my_set_intersection_test5 = equal_sets (set_intersection [1;2;2] [2;2;3]) [2]

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [] [1]) []
let my_set_diff_test2 = equal_sets (set_diff [2] []) [2]
let my_set_diff_test3 = not (equal_sets (set_diff [] [3]) [3])
let my_set_diff_test4 = equal_sets (set_diff [1;2;3;4] [3;4]) [1;2]
let my_set_diff_test5 = equal_sets (set_diff [1;1;1;2;2;2] [2]) [1]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 5) 999999 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x /. 0.5) 1. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 5.) 1. = infinity
let my_computed_fixed_point_test3 = computed_fixed_point (=) exp 1.= infinity
let my_computed_fixed_point_test4 = computed_fixed_point (=) sqrt 100000. = 1.
let my_computed_fixed_point_test5 = computed_fixed_point (=) (fun x -> x / 5) 999999 = 0
let my_computed_fixed_point_test6 = computed_fixed_point (=) (fun x -> x *. 5.) (-1.) = neg_infinity 

type fr_nonterminals =
  | One | Two | Three | Four | Five | Six

let fr_rules =
   [One, [T"+"; N One; T"-"];
    One, [N Five; N Six];
    One, [N One; N Four; N One];
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
    Five, [T"+"];
    Six, []]

let fr_grammar = One, fr_rules

let my_filter_reachable_test0 = filter_reachable fr_grammar = fr_grammar
let my_filter_reachable_test1 = filter_reachable (One, List.tl fr_rules) = (One, List.tl fr_rules)
let my_filter_reachable_test2 = filter_reachable (One, List.tl (List.tl fr_rules)) =
        (One,
        [One, [N One; N Four; N One];
        One, [N Two];
        One, [N Three; N Two];
        One, [N Two; N Three];
        Two, [T "^"; N One];
        Three, [T ","];
        Three, [T"!"];
        Four, [T"="];
        Four, [T"~"]])

let my_filter_reachable_test3 = filter_reachable (Six, fr_rules) = (Six, [Six, []])
let my_filter_reachable_test4 = filter_reachable (Two, fr_rules) = (Two, fr_rules)

