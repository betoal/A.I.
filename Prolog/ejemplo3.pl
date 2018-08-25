valor_max(X,Y,Z):-
    X>Y, Z is X.
valor_max(X,Y,Z):-
    X=<Y, Z is Y.
