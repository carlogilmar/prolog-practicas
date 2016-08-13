

/*INFERENCIAS*/

/*relaciones*/
esun(gato,mamifero).
esun(oso, mamifero).
esun(mamifero,animal).
esun(pez,animal).
esun(ballena, mamifero).

posee(mamifero,pelo).
posee(gato,pelo).
posee(oso,pelo).

vive(ballena,agua).
vive(pez,agua).

/*herencia*/
es(X,Y):- esun(X,Y).
es(X,Y):- esun(X,Z),esun(Z,Y).

tiene(X,Y):- posee(X,Y).
tiene(X,Y):- esun(X,Z), posee(Z,Y).
