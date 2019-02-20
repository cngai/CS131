/* TOWER */

% check to see if N columns per row
length_col([], _).	% base case
length_col([H | T], N) :-
	length(H, N),
	length_col(T, N).

% checks to make sure elements of List are between 1 and N
in_range(N, List) :- fd_domain(List, 1, N).

% maps first column of original matrix to first row of transposed matrix
/* accepts tail of original matrix (N-1 x N),
list of remainder of first row of orig matrix (1 x N-1) that'll become first row trans matrix,
and bottom right remainder of original matrix (N-1 x N-1) */
orig_to_trans([], [], []).
orig_to_trans([[H1 | T1] | T1_2], [H1 | T2], [T1 | T3]) :-
	orig_to_trans(T1_2, T2, T3).

% returns true if new N x N matrix (Trans_T) is transpose of orig N x N matrix (T)
/* -take first column of tail and put into T2 and put as first row of transposed matrix
-Rest is smaller N-1 x N-1 matrix
-recursively fill out top right half of transposed matrix
-Trans_Rem is transposed matrix
-call orig_to_trans again to append head of T1 to head of Trans_rem to fill out rest of N-1 rows */
transpose([], []).
transpose([[H1 | T1] | T1_2], [[H1 | T2] | T2_1]) :- 
	orig_to_trans(T1_2, T2, Rem),
	transpose(Rem, Trans_Rem),
	orig_to_trans(T2_1, T1, Trans_Rem).

% make sure all counts are of length N
check_lengths(counts(Top, Bottom, Left, Right), N) :-
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N).

% returns true if H is less than Curr_Height, will subsequently increment Observed_Count
% has to recursively iterate through entire Curr_List so only returns true if all elmnts in Curr_List < Curr_Height
smaller_height([], _).
smaller_height([H | T], Curr_Height) :-
	H #< Curr_Height,	% FD constraint
	smaller_height(T, Curr_Height).

% if nothing in row, Observed_Count should be 0
get_observed_count([], _, Observed_Count) :- Observed_Count = 0.
get_observed_count([H_elmnt | T_elmnt], Curr_List, Observed_Count) :-
	% check next element in row
	append(Curr_List, [H_elmnt], Next_List),
	get_observed_count(T_elmnt, Next_List, New_Observed_Count),

	% if H of Before < H_elmnt, increment Observed_Count, else keep it same
	(smaller_height(Curr_List, H_elmnt) -> Observed_Count is New_Observed_Count + 1 ; New_Observed_Count = Observed_Count).

% takes in matrix and specified count list
% checks to see if heights of towers match specified count in each column
match_counts([], []).
match_counts([H_row | T_row], [H_cnt | T_cnt]) :-
	get_observed_count(H_row, [], Observed_Count),
	H_cnt = Observed_Count,	% check if Observed_Count is equal to specified count
	match_counts(T_row, T_cnt). % check rest of columns

% reverses entire matrix using reverse/2 predicate
reverse_matrix([], []).
reverse_matrix([H_matrix | T_matrix], [H_rev | T_rev]) :-
	reverse(H_matrix, H_rev),
	reverse_matrix(T_matrix, T_rev).

% make sure all counts of matrix are correct
check_counts(Orig, Trans, Top, Bottom, Left, Right) :-
	% CHECK TOP COUNT
	match_counts(Trans, Top),

	% CHECK BOTTOM COUNT
	reverse_matrix(Trans, Rev_Trans),
	match_counts(Rev_Trans, Bottom),

	% CHECK LEFT COUNT
	match_counts(Orig, Left),

	% CHECK RIGHT ZCOUNT
	reverse_matrix(Orig, Rev_Orig),
	match_counts(Rev_Orig, Right).

% N - nonnegative integer specifying size of grid
% T - list of N lists, each representing row of square grid
% C - structure w/ function symbol counts and arity 4; args are list of counts for t, b, l, r
tower(N, T, C) :-
	length(T, N),	% checks to make sure N rows
	length_col(T, N), % checks to make sure N columns per row

	% ROWS
	maplist(in_range(N), T), % checks to make sure all elements in rows of T are between 1 and N
	maplist(fd_all_different, T), % checks to make sure all elements in rows of T are unique

	% COLUMNS
	transpose(T, Trans_T), % get transpose of T
	maplist(in_range(N), Trans_T),
	maplist(fd_all_different, Trans_T),

	% FIND SOLUTION
	maplist(fd_labeling, T),

	% COUNTS
	C = counts(T_Ct, B_Ct, L_Ct, R_Ct), % make sure arity of 4
	check_lengths(C, N),

	% MAKE SURE COUNTS ARE CORRECT
	check_counts(T, Trans_T, T_Ct, B_Ct, L_Ct, R_Ct).


/* PLAIN TOWER */

% make sure all elements of list are between Lower and Upper range
plain_domain([], _, _).
plain_domain([H_elmnt | T_elmnt], Lower, Upper) :-
	H_elmnt #>= Lower,
	H_elmnt #=< Upper,
	plain_domain(T_elmnt, Lower, Upper).

% make sure all rows/cols of matrix are in range
plain_in_range(N, List) :- plain_domain(List, 1, N).

% recursively check if Val is in remainder of list
is_all_different(_, []).
is_all_different(Val, [H | T]) :-
	Val #\= H,
	is_all_different(Val, T).

% makes sure all elements in list are different
no_repeats(_, []).
no_repeats(Before, [H_elmnt | T_elmnt]) :-
	is_all_different(H_elmnt, T_elmnt),
	append(Before, [H_elmnt], After),
	no_repeats(After, T_elmnt).

% make sure all elements in rows/cols are different
plain_different(List) :- no_repeats([], List).

% find solution utilizing permutation/2, findall/3, between/3 predicates
% permutation(?Xs, ?Ys) ==> true when Xs is permutation of Ys
% findall(+Template, :Goal, -Bag) ==> create list of instantiations Template
% gets on backtracking over Goal and unify result with Bag
% between(+Low, +High, ?Value) ==> true if Value is between Low and High
plain_labeling(N, List) :-
	findall(Number, between(1, N, Number), Bag), % find all solutions
	permutation(Bag, List).	% get different permutations of each row and try to find solution

% enumerate possible integer solutions using member/2 and is/2
% member(?Elem, ?List). ==> True if Elem is member of List
% Number is +Expr. ==> True when Number is value to which Expr evaluates
plain_tower(N, T, C) :-
	length(T, N),
	length_col(T, N),

	% ROWS
	maplist(plain_in_range(N), T),
	maplist(plain_different, T),

	% COLUMNS
	transpose(T, Trans_T),
	maplist(plain_in_range(N), Trans_T),
	maplist(plain_different, Trans_T),

	% FIND SOLUTION
	maplist(plain_labeling(N), T),

	% COUNTS
	C = counts(T_Ct, B_Ct, L_Ct, R_Ct), % make sure arity of 4
	check_lengths(C, N),

	% MAKE SURE COUNTS ARE CORRECT
	check_counts(T, Trans_T, T_Ct, B_Ct, L_Ct, R_Ct).




