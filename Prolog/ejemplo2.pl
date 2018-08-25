grande(bisonte).
grande(oso).
grande(elefane).
chico(gato).
cafe(oso).
cafe(bisonte).
negro(gato).
gris(elefante).
oscuro(Z) :-
cafe(Z).
oscuro(Z) :-
negro(Z).

% ?-oscuro(X),grande(X),write(X),nl,fail.
























