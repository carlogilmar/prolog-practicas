/*SEGUNDO RELEASE DE RPOLOG LISTAS*/



inversa(L1,L):- inversa (L1,[],L).
inversa([],L,L).
inversa([X|L1],L2,L3):- inversa(L1,[X|L2],L3).
