coeff(Poly,Cs):-
    mono(Poly,Mono),
    monocoef(Mono,Cs).

monocoef([m(C,_Pow) | Mono], [C | Cs]) :-
    !,
    monocoef(Mono,Cs).
monocoef([],[]).

parse_pol(Poly,Poly):-
    is_pol(Poly),
    !.
parse_pol(Mono,poly([Mono])):-
    is_mono(Mono),
    !.
parse_pol(Pow,Poly):-
    as_pol(Pow,Poly).

mono(Poly,Res):-
    parse_pol(Poly,poly(Mono)),
    mono_null(Mono,Res).
mono_null([],m(0,0)):- !.
mono_null(Mono,Mono).

max_pow(Poly,Pow):-
    mono(Poly,Mono),
    mono_pow(Mono,Pows),
    pow_null(Pows,NPows),
    max_list(NPows,Pow).
mono_pow([m(C,TPow) | Mono], [P | Pows]):-
    !,
    mono_pow([m(C,TPow)|Mono],Pows).
mono_pow([m(_C,_TP)|Mono],Pows):-
    !,
    mono_pow(Mono,Pows).
mono_pow([],[]).
pow_null([],[0]) :- !.
pow_null(Pows,Pows).

min_pow(Poly, Pow):-
    mono(Poly,Mono),
    mono_pow(Mono,Pows),
    pow_null(Pows,NPows),
    min_list(NPows,Pow).

plus(Poly1,Poly2,poly(Res)):-
    mono(Poly1,Mono1),
    mono(Poly2,Mono2),
    append(Mono1,Mono2,Monos),
    equals(Monos,Res).
% Entender monocompare a.k.a. compare
%compare(<,m(_C1,TPow),m(_C2,TPow)):-

equals(Monos,Res):-
    predsort(compare,Monos,SortedMono),
    mono_equals(SortedMono,Res).
mono_equals([m(C1,TPow),m(C2,TPow) | Monos],EqMono):-
    C is C1 + C2,
    !,
    mono_equals(Monos,EqMono



