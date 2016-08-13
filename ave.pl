pingui(chilliwilly).
pingui(tuinqui).
pingui(submarine).
pingui(leo).
pingui(mariano).
pingui(rambo).
pingui(sambo).

cana(wonder).
cana(eriko).
cana(veroniko).
cana(bob).
cana(patricio).
cana(galileo).
cana(lucho).

avest(sor).
avest(camila).
avest(leonora).
avest(panchita).
avest(galilea).
avest(paquita).
avest(anastasia).

leon(pao).
leon(paulino).
leon(pancho).
leon(panuco).

perico(gil).
perico(german).
perico(gulio).
perico(gumercindo).


ave(X):-pingui(X) | cana(X) | avest(X) | perico(X).

vuela(X):-ave(X), not( pingui(X) ),not( avest(X) ).

ave_no_vuela(X):- ave(X), not( cana(X) ), not( perico(X) ).

no_es_ave(X):- not( cana(X) ), not( perico(X) ), not( pingui(X)), not(avest(X)).



