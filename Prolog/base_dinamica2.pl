:- dynamic antes_de/2.

antes_de(ayer,hoy).
antes_de(hoy,ma�ana).

main:-
    not(antes_de(ayer,ma�ana)),write("uno"),nl,
    assert((antes_de(X,Y):-antes_de(X,Z),antes_de(Z,Y))),
    write("dos"),nl,
    antes_de(ayer,ma�ana),write("tres"),nl,!.
