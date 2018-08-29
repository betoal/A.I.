consecutivos(A,B,[A,B|_]):-!.
consecutivos(A,B,[_|R]):-
    consecutivos(A,B,R).

repite.
repite:-
    repite.

lee_valores:-
    repite,
    read(X),
    write(X),
    nl,
    X==ya.
