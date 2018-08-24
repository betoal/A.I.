/* Este es
 *  un comentario.
 * El . es una cláusula. Los utilizan los hechos y reglas.*/
 % Este también es un comentario.

 % El predicado hombre tiene un argumento cuyo valor es el nombre
 % de un hombre.

hombre(jose).
hombre(juan).
mujer(maria).
% El primer argumento debe ser el nombre de un papa, el segundo es el
% nombre de su hijo. Puede ser al revés, pero siempre se tiene que
% escribir de la misma manera.
papa(juan,jose).
papa(juan,maria).
% Se pueden asignar varias propiedades a una constante.
valioso(dinero).
dificilDeObtener(dinero).
% Pedro le da un libro a Antonio
le_da(pedro,libro,antonio).

hermana(X,Y):-
    papa(Z,X),
    mujer(X),
    papa(Z,Y),
    X\==Y.
hijo(X,Y):-
    papa(Y,X),
    hombre(X).
humano(X):-
    mujer(X);
    hombre(X).
/* Otra forma de expresar el or. Se pueden tener varias reglas para la misma conclusion.
humano(X):-
    mujer(X).
humano(X):-
    hombre(X).
El predicado es no determinístico porque tiene varias definiciones o si
tiene más de 1 cláusula.
*/
