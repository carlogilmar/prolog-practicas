	
/*PROGRAMA 1 EN PROLOG

1.-Declarar base de relaciones logicas

*/

	progenitor(clara,jose).
	progenitor(tomas,jose).
	progenitor(tomas,isabel).
	progenitor(jose,ana).
	progenitor(jose,patricia).
	progenitor(patricia,jaime).

/*

Para demostrar que clara es bisabuela de jaime se implementa la siguiente conjuncion que debe resultar en un TRUE

progenitor(clara,X), progenitor(X,Y), progenitor(Y,jaime)

Primero se intenta satisfacer la primera X del (clara,X)

Esa X va en la segunda (X,Y), asi para hallar Y se debe recurrir a la ultima (Y, jaime)...

(Y,jaime) es igual a Patricia es progenitor de Jaime

Por lo tanto Jose es progenitor de Patricia

Y Clara es progenitor de Jose

Por lo tanto n debe arrojar un valor FALSE


2.- Declarando reglas...

*/

abuelo(X,Y):-progenitor(X,Z), progenitor(Z,Y).
tio(X,Y):-progenitor(Z,Y), progenitor(V,Z), progenitor(V,X).

/*
Revisando las reglas anteriores:

X es abuelo de Y, pero X es progenitor de Z, mientras Z es progenitor de Y

X es tio de Y, donde Z es progenitor de Y mientras que V es progenitor de Z, entonces V es progenitor de X
*/


es_progenitor(X):- progenitor(X,Z) | progenitor(X,Z), progenitor(Z,A) | progenitor(X,Z), progenitor(Z,A), progenitor(A,B).
/*
Evalua si X es progenitor, si es progenitor o si es abuelo, o si es bisabuelo arroja valor TRUE
*/

/*3.- REGLAS RECURSIVAS<------- ahondar que no entendí muy bien */

predecesor(X,Y):-progenitor(X,Y).
predecesor(X,Y):-progenitor(X,Z), progenitor(Z,Y).

/*4.- VARIABLES ANONIMAS*/

tiene_hijo(X):- progenitor(X,_).

/*5.- FUNCTORES*/

direccion(clara,datos("Av_holi","Colonia Happy","1222")).


/*DEJANDO EL ARBOL FAMILIAR DE LADO

vertical(segmento(punto(X,A),punto(X,B)).
horizontal(segmento(punto(A,Y), punto(B,Y)).

6.- UNIFICACIÓN<---- NO ENTENDÍ NADA :/

*/













