% check to see if N columns per row
length_col([], _).	% base case
length_col([H | T], N) :-
	length(H, N),
	length_col(T, N).

% checks to make sure elements of List are between 1 and N
in_range(N, List) :-
	fd_domain(List, 1, N).

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
	transpose(T, Trans_T).

	%C = counts(Top, Bottom, Left, Right).

% enumerate possible integer solutions using member/2 and is/2
% member(?Elem, ?List). ==> True if Elem is member of List
% Number is +Expr. ==> True when Number is value to which Expr evaluates
plain_tower(N, T, C) :-
	raining.