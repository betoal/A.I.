:- dynamic pais/1.
pais(holanda).
pais(francia).

repite.
repite:-
    repite.

escribe_paises:-
    pais(X),
    %X\==ya,
    write(X),
    nl,
    fail.
escribe_paises.

main:-
    write("Dame los nombre de varios paises y escribe ya"),
    write(" cuando quieras terminar."),
    nl,
    repite,
    read(Pais),
    assert(pais(Pais)),
    Pais==ya,
    retract(pais(Pais)),
    escribe_paises.

