exp --> term.
exp --> term, [+], exp.

term --> factor.
term --> digit, [*], exp.

factor --> elem.
factor --> ['S'], ['['], sym, [']']. %S[(A)(B),(C)]
factor --> ['<'], alt, ['>'], ['/'], ['<'], alt, ['>']. %<(A)>/<(B)(C)(D)>
factor --> ['('], exp, [')'].

sym --> factor.
sym --> factor, [','], factor.
sym --> factor, sym.

alt --> factor.
alt --> factor, alt.

elem --> char.
elem --> char, elem.
char --> [D], {is_alnum(D)}.
digit --> [D], {is_alnum(D)}.
digit --> [D], {number(D)}.

nbr_to_char(N, Cs) :-
    name(Cs, [N]).
str_to_list(S, Cs) :-
    name(S, Xs),
    maplist(nbr_to_char, Xs, Cs).

eval(L) :-
    str_to_list(L, X),
    exp(X, []).
