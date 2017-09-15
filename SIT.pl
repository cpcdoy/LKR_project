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

% Pass L as suffix array
compare_all_suffixes(L, Cnt, R) :-
    get_suffix_array(L, SuffArray),
    compare_all_suffixes_hat(0, SuffArray, 0, [], Cnt, R).
    
compare_all_suffixes_hat(N, L, Cnt, LongestSuff, Hcnt, R) :-
    length(L, Size),
    I is Size - N,
    I == 0,
    Hcnt = Cnt,
    R = LongestSuff,
    !.

compare_all_suffixes_hat(N, L, Cnt, StoredSuff, Hcnt, R) :-
    nth0(N , L, NewSuff),
    nth0(N , L, NewSuffTmp),
    compare_suffix(NewSuff, NewSuffTmp, N, 0, L, Nb, CurSuff),
    compare_repeated_suffixes(StoredSuff, CurSuff, Nb, Cnt, CntRes, SuffRes),
    I is N + 1,
    compare_all_suffixes_hat(I, L, CntRes, SuffRes, Hcnt, R).

compare_repeated_suffixes(_, CurSuff, Nb, Cnt, CntRes, R) :-
    Nb > Cnt,
   	R = CurSuff,
    CntRes = Nb,
    !.
compare_repeated_suffixes(StoredSuff, _, Nb, Cnt, CntRes, R) :-
    Nb < Cnt,
   	R = StoredSuff,
    CntRes = Cnt,
    !.
compare_repeated_suffixes(StoredSuff, CurSuff, Nb, Cnt, CntRes, R) :-
    Nb == Cnt,
    length(CurSuff, CurSuffLen),
    length(StoredSuff, StoredSuffLen),
    CurSuffLen > StoredSuffLen,
    R = CurSuff,
    CntRes = Nb,
    !.
compare_repeated_suffixes(StoredSuff, _, Nb, Cnt, CntRes, R) :-
    Nb == Cnt,
    R = StoredSuff,
    CntRes = Nb,
    !.
    
compare_all_suffixes_hat(N, L, Cnt, LongestSuff, Hcnt, R) :-
    I is N + 1,
    compare_all_suffixes_hat(I, L, Cnt, LongestSuff, Hcnt, R).

compare_suffix(Sub, SubTmp, _, Cnt, _, Hcnt, R) :-
    length(SubTmp, Size),
    Size == 0,
    Hcnt = Cnt,
    R = Sub,
    !.

compare_suffix(_, SubTmp, SubIdx, Cnt, L, Hcnt, R) :-
    compare_suffix_hat(0, 0, SubTmp, SubIdx, L, NewCnt),
    NewCnt > Cnt,
    append(NewSub, [_], SubTmp),
    compare_suffix(SubTmp, NewSub, SubIdx, NewCnt, L, Hcnt, R).

compare_suffix(Sub, SubTmp, SubIdx, Cnt, L, Hcnt, R) :-
    append(NewSub, [_], SubTmp),
    compare_suffix(Sub, NewSub, SubIdx, Cnt, L, Hcnt, R).

compare_suffix_hat(N, Cnt, _, _, L, R) :-
    length(L, Size),
    Size == N,
    R = Cnt,
    !.

compare_suffix_hat(N, Cnt, Sub, SubIdx, L, R) :-
    nth0(N, L, Suff),
    prefix(Sub, Suff),
    length(Sub, SubLen),
    is_continuous(SubLen, SubIdx, N, Sub, Suff),
    I is N + 1,
    Incr is Cnt + 1,
    compare_suffix_hat(I, Incr, Sub, SubIdx, L, R).

compare_suffix_hat(N, Cnt, Sub, SubIdx, L, R) :-
    I is N + 1,
    compare_suffix_hat(I, Cnt, Sub, SubIdx, L, R).

take(0, _, []).
take(N, [H|T], [H|R]) :-
    N > 0,
    M is N - 1,
    take(M, T, R).

drop(0, L, L).
drop(N, [_|T], R) :-
    N > 0,
    M is N - 1,
    drop(M, T, R).

prefix(X, L) :-
    append(X, _, L).
suffix(X, L) :-
    append(_, X, L).
sublist(X, L) :-
    suffix(S, L),
    prefix(X, S),
    length(X, R),
    R > 0.

find_elem_in_list(X, [X|_], X).
find_elem_in_list(X, [_|L], _) :-
    find_elem_in_list(X, L, _).

match_suffix_prefix([X|_], [X|_], X).
match_suffix_prefix([X|_], L2, X) :-
    find_elem_in_list(X, L2, X),
    !.

match_suffix_prefix([_|L1], L2, X) :-
    match_suffix_prefix(L1, L2, X).

%
label_start(Max, _, Max, _, Off, Off).
label_start(Max, _, Max, _, Off, _) :-
    label_start(Max, _, Max, _, Off, Off).

label_start(Max, Len, G, I, Off, R) :-
    G < Max,
    G1 is G + 1,
    I1 is I + 1,
    I1 == Len,
    Off1 is Off + 1,
    Len1 is Len - 1,
    label_start(Max, Len1, G1, 0, Off1, R).

label_start(Max, Len, G, I, Off, R) :-
    G < Max,
    I1 is I + 1,
    G1 is G + 1,
    label_start(Max, Len, G1, I1, Off, R).

:- dynamic a/0. 
get_all_start_labels(L, LenS, LenL, R, S) :-
   not(a),
   assert(a),
   R = [],
   get_all_start_labels(L, LenS, LenL, R, S).
get_all_start_labels([], _, _, R, R) :-
   retract(a),
   !.
get_all_start_labels([], _, _, R, _) :-
    get_all_start_labels(_, _, _, R, R).
get_all_start_labels([_|L], I, LenL, R, S) :-
    I1 is I + 1,
    label_start(I1, LenL, 0, 0, 0, Label),
    get_all_start_labels(L, I1, LenL, [Label|R], S).

take(0, _, []).
take(N, [H|T], [H|R]) :-
    N > 0,
    M is N - 1,
    take(M, T, R).

drop(0, L, L).
drop(N, [_|T], R) :-
    N > 0,
    M is N - 1,
    drop(M, T, R).

% iteration
is_continuous(_, SubIdx, SuffIdx, _, _) :-
    SubIdx == SuffIdx,
    !.
is_continuous(SubLen, SubIdx, SuffIdx, Sub, Suff) :-
    drop(SubLen, Suff, NewSuff),
    prefix(Sub, NewSuff),
    NewSuffIdx is SuffIdx + SubLen,
    is_continuous(SubLen, SubIdx, NewSuffIdx, Sub, NewSuff).

iteration(L, R) :-
    compare_all_suffixes(L, Cnt, RepeatedSq),
    cut_string(L, [], Cnt, RepeatedSq, Rest, R).

cut_string(L, Acc, Cnt, RepeatedSq, Rest, R) :-
    prefix(RepeatedSq, L),
    Acc == [],
    length(RepeatedSq, Len),
    LenToCut is Len * Cnt,
    drop(LenToCut, L, Tmp),
    append([Cnt, "*", "("|RepeatedSq], [")", "+"|Tmp], Res),
    R = Res,
    !.
cut_string(L, Acc, Cnt, RepeatedSq, Rest, R) :-
    prefix(RepeatedSq, L),
    Rest = Acc,
    length(RepeatedSq, Len),
    LenToCut is Len * Cnt,
    length(L, ExactLen),
    LenToCut == ExactLen,
    append(Rest, ["+", Cnt , "*", "("| RepeatedSq], Tmp),
    append(Tmp, [")"], Rtmp),
    R = Rtmp,
    !.
cut_string(L, Acc, Cnt, RepeatedSq, Rest, R) :-
    prefix(RepeatedSq, L),
    Rest = Acc,
    length(RepeatedSq, Len),
    LenToCut is Len * Cnt,
    drop(LenToCut, L, Tmp),
    append(Rest, ["+", Cnt , "*", "("| RepeatedSq], LeftPart),
    append(LeftPart, [")"], Rtmp),
    append(Rtmp, ["+"|Tmp], Res),
    R = Res,
    !.
cut_string(L, Acc, Cnt, RepeatedSq, Rest, R) :-
    take(1, L, Elt),
    append(Elt, Acc, NewAcc),
	drop(1, L, DroppedL),
    cut_string(DroppedL, NewAcc, Cnt, RepeatedSq, Rest, R).

% symmetry
cut_string_sym(L, Acc, RepeatedSq, Center, Pattern, Rest, R) :-
    prefix(RepeatedSq, L),
    Acc == [],
    length(RepeatedSq, Len),
    LenToCut is Len,
    drop(LenToCut, L, Tmp),
    length(Tmp, LenTmp),
    LenTmp \== 0,
    append(Center, [")", ",", "("], CC),
    append(CC, Pattern, CP),
    append([ "S", "[", "("|CP], [")", "]" , "+"|Tmp], Res),
    R = Res,
    !.
cut_string_sym(L, Acc, RepeatedSq, Center, Pattern, Rest, R) :-
    prefix(RepeatedSq, L),
    Acc == [],
    length(RepeatedSq, Len),
    LenToCut is Len,
    drop(LenToCut, L, _),
    append(Center, [")", ",", "("], CC),
    append(CC, Pattern, CP),
    append([ "S", "[", "("|CP], [")", "]"], Res),
    R = Res,
    !.
cut_string_sym(L, Acc, RepeatedSq, Center, Pattern, Rest, R) :-
    prefix(RepeatedSq, L),
    Rest = Acc,
    length(RepeatedSq, Len),
    LenToCut is Len,
    length(L, ExactLen),
    LenToCut == ExactLen,
    append(Center, [")", ",", "("], CC),
    append(CC, Pattern, CP),
    append(Rest, ["+" , "S", "[", "("| CP], Tmp),
    append(Tmp, [")", "]"], Rtmp),
    R = Rtmp,
    !.
cut_string_sym(L, Acc, RepeatedSq, Center, Pattern, Rest, R) :-
    prefix(RepeatedSq, L),
    Rest = Acc,
    length(RepeatedSq, Len),
    LenToCut is Len,
    drop(LenToCut, L, Tmp),
    append(Center, [")", ",", "("], CC),
    append(CC, Pattern, CP),
    append(Rest, ["+" , "S", "[", "("| CP], LeftPart),
    append(LeftPart, [")", "]"], Rtmp),
    append(Rtmp, ["+"|Tmp], Res),
    R = Res,
    !.
cut_string_sym(L, Acc, RepeatedSq, Center, Pattern, Rest, R) :-
    take(1, L, Elt),
    append(Elt, Acc, NewAcc),
	drop(1, L, DroppedL),
    cut_string_sym(DroppedL, NewAcc, RepeatedSq, Center, Pattern, Rest, R).

longest([L], L) :-
   !.
longest([H|T], H) :-
   length(H, N),
   longest(T, X),
   length(X, M),
   N > M,
   !.
longest([_|T], X) :-
   longest(T, X),
   !.

sublist_index(L, M, N, S) :-
    findall(E, (between(M, N, I), nth1(I, L, E)), S).

get_symmetry_pattern(L, Center, Pattern) :-
    length(L, LenL),
   	not(0 is LenL mod 2),
    CenterPos is LenL div 2,
    sublist_index(L, 0, CenterPos, Pattern),
    CenterPos1 is CenterPos,
    nth0(CenterPos1, L, Center).

get_symmetry(L, R) :-
    %get_symmetry_(L, S),
    findall(X, get_symmetry_(L, X), S1),
    longest(S1, S),
	get_symmetry_pattern(S, Center, Pattern),
    cut_string_sym(L, [], S, [Center], Pattern, _, R).

get_symmetry_(L, X) :-
    findall(Sub, sublist(Sub, L), S),
  	member(Sub, S),
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
    not(0 is (R mod 2)).
