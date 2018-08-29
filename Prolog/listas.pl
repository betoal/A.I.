% combina(i,i,o).
% combina(i,i,i).
% combina(o,i,i).
% combina(o,o,i).

pertenece_a(X,[X|_]):-!.
pertenece_a(X,[_|Z]):-
    pertenece_a(X,Z).

combina([X|Lista1],Lista2,[X|Lista3]):-
    combina(Lista1,Lista2,Lista3).
combina([],Lista,Lista):-!.
