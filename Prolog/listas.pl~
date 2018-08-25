pertenece_a(X,[X|_]):-!.
pertenece_a(X,[_|Z]):-
    pertenece_a(X,Z).


combina([],Lista,Lista):-!.
combina([X|Lista1],Lista2,[X|Lista3]):-
    combina(Lista1,Lista2,Lista3).
