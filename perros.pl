perros(pastor_aleman, [juli, esteba, pancho]).
perros(san_bernardo, [master, rigan, mujamad]).
perros(french_poodle, [figaro, piojo, ramiro]).

pertenece(E,L):-L=[E|_].
pertenece(E,[_|T]):-pertenece(E,T).
pastor_aleman(P):-perros(pastor_aleman, L), pertenece(P,L).
raza(R):-perros(_,L),pertenece(R,L).