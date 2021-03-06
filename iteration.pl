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
%iteration([a,a,a,a,a,y,o,y,o,a], R).
