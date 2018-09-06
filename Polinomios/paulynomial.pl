%PA_head es la cabeza del polinomio 1, Y es la cola.
%Respectivamente  PB_head y PB_tail para el polinomio 2
%Respectivamente PC_head y PC_tail para la suma de los polinomios

sum([],[],[]). %caso base las tres listas están vacia
sum([],PB_tail,PB_tail).
sum(PB_head,[],PB_head).
sum([PA_head|PA_tail],[PB_head|PB_tail],[PC_head|PC_tail]):-
    sum(PA_tail,PB_tail,PC_tail);
    PC_head is (PA_head+PB_head).


%PA_head es la cabeza del polinomio 1, PA_tail es la cola.
%Respectivamente  PB_head PB_tail y para el polinomio 2
%Respectivamente PC_head y PC_tail para la resta de los polinomios
minus([],[],[]).
minus([],PB_tail,PB_tail).
minus(PB_head,[],PB_head).
minus([PA_head|PA_tail],[PB_head|PB_tail],[PC_head|PC_tail]):-
    minus(PA_tail,PB_tail,PC_tail),
    PC_head is (PA_head-PB_head).

times([],[],[]).
times([],PB_tail,PB_tail).
times(PB_head,[],PB_head).
times([PA_head|PA_tail],[PB_head|PB_tail],[PC_head|PC_tail]):-
	times(PA_tail,PB_tail,PC_tail),
	PC_head is (PA_head*PB_head).


evaluate([],_X, 0).
evaluate([Head|Tail], X, Pow) :-
    evaluate(Tail, X, Pow1),
    Pow is Pow1 * X + Head.

compose([],_X, [0]).
compose([Head|Tail], X, Pow) :-
    evaluate(Tail, X, Pow1),
    times(X,Pow,Res),
    sum([Head],Res,).


differentiate([],[],_).
differentiate([_|B],NewCoeff,0):-
    differentiate(B,NewCoeff,1).
differentiate([Coeff|B],[NewCoeff|D],Pow):-
    differentiate(B,D,Pow+1),
    NewCoeff is Coeff*Pow.

print([]).
print([A|B]) :-
	format(' ~w^~w +~n',A),
	print(B).

