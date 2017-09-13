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

%Cmd Line
trace, get_suffix_array([a, a, b, x, c, a, b, c, c], R),
compare_suffix([a,b,c], R, Cnt).
