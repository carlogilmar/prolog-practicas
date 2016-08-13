dos_patas(trex).
dos_patas(velociraptor).
dos_patas(titanosaurus).
dos_patas(megalosaurus).

cuatro_patas(cuellolargo).
cuatro_patas(triceratops).
cuatro_patas(plesiosaurio).
cuatro_patas(sauropodo).

planta(helecho).
planta(elodea).

come_a(titanosaurus,megalosaurus).
come_a(megalosaurus,trex).
come_a(trex,velociraptor).
come_a(velociraptor,cuellolargo).


come_a(cuellolargo,helecho).
come_a(triceratops,helecho).
come_a(plesiosaurio,elodea).
come_a(sauropodo,elodea).


come_plantas(X):- come_a(X,Y), planta(Y).

carnivoro(X):- come_a(X,Y), dos_patas(Y).

come_carne(X):- come_a(X,Y), cuatro_patas(Y) | come_a(X,Y), dos_patas(Y).

cadena_asc(W,X,Y):- come_a(W,X), come_a(X,Y).

es_comido(X,Y,Z):- come_a(Y,X), come_a(Z,Y).
