
/*abuelos*/
padres(abuelaconcha,ivan).
padres(abuelogerudiel,ivan).
padres(fabio,rosalba).
padres(abuelapelos,rosalba).
padres(abuelapelos,silvio).
padres(fabio,silvio).

/*otras relaciones*/
padres(rosalba, jorge).
padres(rosalba, julia).
padres(rosalba, saul).
padres(ivan, jorge).
padres(ivan, saul).
padres(ivan, julia).

/*sexo*/
mujer(julia).
mujer(rosalba).
mujer(abuelaconcha).
mujer(abuelapelos).

hombre(fabio).
hombre(saul).
hombre(jorge).
hombre(ivan).
hombre(silvio).
hombre(abuelogerudiel).

/*relaciones*/
abuela(X,Z):-padres(X,Y),padres(Y,Z),mujer(X).
abuelo(X,Z):-padres(X,Y),padres(Y,Z),hombre(X).

esposo(X,Y):-padres(X,Z),padres(Y,Z), hombre(X).
esposa(X,Y):-padres(X,Z),padres(Y,Z), mujer(X).

hermano(X,Y):-padres(P,X),padres(P,Y),hombre(X).
hermana(X,Y):-padres(P,X),padres(P,Y),mujer(X).

tio(X,Y):- padres(Z,Y), padres(A,Z), padres(A,X).

sobrino(A,B):- tio(B,A).



