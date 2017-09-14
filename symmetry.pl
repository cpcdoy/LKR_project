get_suffix(N, L, _) :-
    length(L, Size),
   	Size < N,
    !, fail.
get_suffix(N, L, R) :-
    length(L, Size),
    Pos is Size - N,
    Pos = 0,
    R = [],
    !.
get_suffix(N, L, [Elt|R]) :-
    nth0(N, L, Elt),
    I is N + 1,
    get_suffix(I, L, R).

get_suffix_array(L, R) :-
    length(L, Size),
    N is Size - 1,
    get_suffix_array_hat(N, L, Tmp),
    reverse(Tmp, R).

get_prefix_array(L, R) :-
    reverse(L, Tmp),
    get_suffix_array(Tmp, R).

get_suffix_array_hat(N, _, R) :-
    N == -1,
    R = [],
    !.
get_suffix_array_hat(N, L, [Elt|R]) :-
    get_suffix(N, L, Elt),
    I is N - 1,
    get_suffix_array_hat(I, L, R).

compare_suffix(Sub, L, R) :-
    compare_suffix_hat(0, 0, Sub, L, R).
compare_suffix_hat(N, Cnt, _, L, R) :-
    length(L, Size),
    Size == N,
    R = Cnt,
    !.
compare_suffix_hat(N, Cnt, Sub, L, R) :-
    nth0(N, L, Suff),
    prefix(Sub, Suff),
    I is N + 1,
    Incr is Cnt + 1,
    compare_suffix_hat(I, Incr, Sub, L, R).
compare_suffix_hat(N, Cnt, Sub, L, R) :-
    I is N + 1,
    compare_suffix_hat(I, Cnt, Sub, L, R).

prefix(X, L) :- append(X, _, L).
suffix(X, L) :- append(_, X, L).
sublist(X, L) :- suffix(S, L), prefix(X, S).

find_elem_in_list(X, [X|_], X).
find_elem_in_list(X, [_|L], _) :-
    find_elem_in_list(X, L, _).

match_suffix_prefix([X|_], [X|_], X).
match_suffix_prefix([X|_], L2, X) :-
    find_elem_in_list(X, L2, X),
    !.

match_suffix_prefix([_|L1], L2, X) :-
    match_suffix_prefix(L1, L2, X).

get_symmetry(L, X) :-
    findall(Len-Sub, (sublist(Sub, L), length(Sub, Len)), S),
    keysort(S, Sorted),
  	member(_-Sub, Sorted),
    get_symmetry_lists([Sub], X).

get_symmetry_lists([L|_], X) :-
    get_symmetry_list(L, X),
    !.

get_symmetry_lists([_|S], X) :-
	get_symmetry_lists(S, X).

get_symmetry_list(L, X) :-
    get_suffix_array(L, R1),
    get_prefix_array(L, R2),
    match_suffix_prefix(R1, R2, X),
    length(X, R),
    R > 2,
    not((R mod 2) is 0),
    write(R mod 2).

%cmd trace, get_symmetry([a, c, b, c, a, b, a, c, d, s, s, s, s, s, s], S).
