%ejercicio de listas

perros(pastor_aleman, [juli,esteban,pancho]).
perros(san_bernardo, [master,reagan,mrholis]).
perros(french_poodle, [mariano,piojo,liendre]).


pertenece(E,L):-L=[E|_].
pertenece(E,[_|T]):-pertenece([E,T]).

pastor_aleman(P):-perros(pastor_aleman,L), pertenece(P,L).
raza(R):-perros(_,L),pertenece(R,L).
