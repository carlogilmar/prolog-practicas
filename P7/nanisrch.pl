% NANI SEARCH - A sample adventure game

% Copyright (C) 1990-2002 Amzi! inc.
% All rights reserved

% Nani Search is designed to illustrate Prolog programming.  It
% is an implementation of the principle example used in
% APT - The Active Prolog  Tutor.

main:- nani_search.       % main entry point (punto de entrada)
								  % es necesario escribirlo en el listener para
								  % poder iniciar con el juego

nani_search:-
  init_dynamic_facts,     % predicados que no se compilan
  
  % Se muestra una descripción del juego y cómo se controla

  write('*******************************************************************'),nl,
  write('********************* PAOLA YETZIRATH *****************************'),nl,
  write('*******************************************************************'),nl,
  nl,
  write('NANI SEARCH - Un ejemplo de un juego de aventura'),nl,
  write('Copyright (C) Amzi! inc. 1990-2002'),nl,
  write('No hay derecho reservados, usalo como quieras'),nl,
  nl,
  write('Nani Search esta diseñado para ilustrar la programacion en Prolog.'),nl,
  write('Por lo tanto, podría ser el juego más simple de aventura.El juego'),nl,
  write('es el ejemplo principal utilizado en APT - El Prolog Activo'),nl,
  write('Tutor. Fuente completa se incluye también.'),nl,
  nl,
  write('La persona como el aventurero es una niña de un tres años'),nl,
  write('de edad.  La Nani es tu manta de seguridad.  Se esta haciendo'),nl,
  write('tarde y está cansada, pero no se puede dormir sin su Nani'),nl,
  write('Tu misión es encontrar a Nani!!'),nl,
  nl,
  write('Tu controlas el juego usando comandos simples en Ingles'),nl,
  write('expresando la acción que quieres tomar. Ustede puede ir a'),nl,
  write('otras habitaciones, mirar alrededor, miras las cosas'),nl,
  write('tomar las cosas, soltar cosas, comer cosas, ver el inventario'),nl,
  write('de las cosas que tienes, y encender o apagar las cosas.'),nl,
  nl,
  write('Pulse cualquier tecla para continuar.'),get0(_),
  write('Escriba "help" si usted necesita mas ayuda en la mecanica.'),nl,
  write('Escriba "hint" si quiere una piesta.'),nl,
  write('Escriba "quit" si quiere salir.'),nl,
  nl,
  write('Disfrute el juego.'),nl,
  nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     interpreter are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

% Aqui se enuncian los predicados del juego

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(quit):-quit,!.

% Estos son los predicados que controlan la salida del juego. Si
% el jugador ha tomado la nani, la llamada "have(nani)" se
% tendrá éxito y el command_loop se completará. De lo contrario se produce un error
% Y command_loop repetirán.

%Si nani es encontrada, nos felicita porque tenemos a salvo a nani
nanifound:-
  have(nani),        
  write('Congratulations, you saved the Nani.'),nl,
  write('Now you can rest secure.'),nl,nl.

% si abandonamos el juego nos dice que si estamos seguros de abandonar el juego
% y que será una noche de miedo por no tener a nani
quit:-
  write('Giving up?  It''s going to be a scary night'),nl,
  write('and when you get the Nani it''s not going'),nl,
  write('to smell right.'),nl,nl.

% El comando de ayuda
% Nos dice que se usaran oraciones simples en Ingles como comandos
% Nos pone ejemplo de los comandos que se pueden usar

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   eat something         (ex. eat the apple)'),nl,
  write('   turn something on     (ex. turn on the light)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

% Se muestra una pista para poder llegar al sótano
hint:-
  write('You need to get to the cellar, and you can''t unless'),nl,
  write('you get some light.  You can''t turn on the cellar'),nl,
  write('light, but there is a flash light in the desk in the'),nl,
  write('office you might use.'),nl,nl,
  look.

% Son los hechos iniciales que describen el juego.  Habitaciones y puertas no cambian,
% por lo que son compilados.

room(office).				% oficina es una habitacion
room(kitchen).				% cocina es una habitacion
room('dining room').		% comedor es una habitacion
room(hall).				   % pasillo es una habitacion
room(cellar).				% sotano es una habitacion

door(office,hall). 					% hay una puerta que comunica el oficina y el pasillo
door(hall,'dining room').			% hay una puerta que comunica el pasillo y el comedor
door('dining room',kitchen).		% hay una puerta que comunica el comedor y la cocina
door(kitchen,cellar).				% hay una puerta que comunica la cocina y el sotano
door(kitchen,office).				% hay una puerta que comunica la cocina y la oficina

connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).

% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the interpreter at
% run time.  This predicate is called when "nanisrch" starts up.

% afirmar la localizacion de las cosas en las habitaciones

init_dynamic_facts:-
  assertz(location(desk,office)),
  assertz(location(apple,kitchen)),
  assertz(location(flashlight,desk)),
  assertz(location('washing machine',cellar)),
  assertz(location(nani,'washing machine')), 		% nani esta en la lavadora
  assertz(location(table,kitchen)),
  assertz(location(crackers,desk)),
  assertz(location(broccoli,kitchen)),
  assertz(here(kitchen)),
  assertz(turned_off(flashlight)).


% Muebles que estan en las habitaciones
furniture(desk).
furniture('washing machine').
furniture(table).

% Alimentos comestibles
edible(apple).
edible(crackers).

% Alimento que no le gusta
tastes_yuchy(broccoli).




%%%%%%%% COMANDOS %%%%%%%%%%%%%%%%%%%%%%%%%%

% Go to mueve al jugador de habitacion en habitacion.

goto(Room):-
  can_go(Room),                 % check for legal move
  puzzle(goto(Room)),           % check for special conditions
  moveto(Room),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Room):-                  % if there is a connection it 
  here(Here),             % is a legal move.
  connect(Here,Room),!.
can_go(Room):-
  respond(['You can''t get to ',Room,' from here']),fail.

moveto(Room):-                  % update the database with the
  retract(here(_)),             % new room
  asserta(here(Room)).

% Ver la lista de las cosas que se encuentran en las habitaciones, y las conexiones

% Lo que puede ver
look:-
  here(Here),
  respond(['You are in the ',Here]),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following rooms:'),nl,
  list_connections(Here).

list_things(Place):-
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_things(_).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

% Para ver dentro de las cosas que estan en las habitaciones

look_in(Thing):-
  location(_,Thing),                  % make sure there's at least one
  write('The '),write(Thing),write(' contains:'),nl,
  list_things(Thing).
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).

% take es para tomar las cosas de los muebles

take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]).

is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.             % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  fail.

contains(Thing,Here):-                % recursive definition to find
  location(Thing,Here).         % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-           % you can't take the furniture
  furniture(Thing),
  respond(['You can''t pick up a ',Thing]),
  !,fail.
is_takable(_).                % not furniture, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old place
  asserta(have(Thing)).            % and add to your possessions

% drop - permite tranportar cosas de habitacion a habitacion

drop(Thing):-
  have(Thing),          % you must have the thing to drop it
  here(Here),           % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)).
drop(Thing):-
  respond(['You don''t have the ',Thing]).

% eat, Para permitirle come

eat(Thing):-
  have(Thing),
  eat2(Thing).
eat(Thing):-
  respond(['You don''t have the ',Thing]).
  
eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was good']).
eat2(Thing):-
  tastes_yuchy(Thing),
  respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).

% inventory  son las cosas que tenemos

inventory:-
  have(X),        % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

% turn_on para encender o apagar cosas, en este caso la linterna o interruptor

turn_on(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_on(Thing):-
  have(Thing),
  turn_on2(Thing).
turn_on(Thing):-
  respond(['You don''t have the ',Thing]).

turn_on2(Thing):-
  turned_on(Thing),
  respond([Thing,' is already on']).
turn_on2(Thing):-
  turned_off(Thing),
  retract(turned_off(Thing)),
  asserta(turned_on(Thing)),
  respond([Thing,' turned on']).
turn_on2(Thing):-
  respond(['You can''t turn a ',Thing,' on']).

% turn_off - I didn't feel like implementing turn_off

turn_off(Thing):-
  respond(['I lied about being able to turn things off']).

% The only special puzzle in Nani Search has to do with going to the
% cellar.  Puzzle is only called from goto for this reason.  Other
% puzzles pertaining to other commands could easily be added.

puzzle(goto(cellar)):-
  have(flashlight),
  turned_on(flashlight),!.
puzzle(goto(cellar)):-
  write('You can''t go to the cellar because it''s dark in the'),nl,
  write('cellar, and you''re afraid of the dark.'),nl,
  !,fail.
puzzle(_).

% respond simplifies writing a mixture of literals and variables
 
respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Simple English command interpreter.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a 
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_on) --> [switch,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining,room].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,flashlight) --> [flash,light].
noun(thing,'washing machine') --> [washing,machine].
noun(thing,'dirty clothes') --> [dirty,clothes].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing,light) --> [X,light], {room(X)}.
noun(thing,flashlight) --> [light], {have(flashlight)}.
noun(thing,light) --> [light].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
	get0(C),
	readword(C, W, C1),  	% Read word starting with C, C1 is first new
	restsent(C1, Ws), !.    % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !.  % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
	readword(C,W1,C1),	 	% Else read next word and rest of sentence
	restsent(C1,Ws).

readword(C,W,C1) :-			% Some words are single characters
	single_char(C),	    	% i.e. punctuation
	!, 
	name(W, [C]),	      	% get as an atom
	get0(C1).
readword(C, W, C1) :-
	is_num(C),		 			% if we have a number --
	!,
	number_word(C, W, C1, _).  % convert it to a genuine number
readword(C,W,C2) :-			% otherwise if charcter does not
	in_word(C, NewC),	  		% delineate end of word - keep
	get0(C1),		  			% accumulating them until 
	restword(C1,Cs,C2),		% we have all the word     
	name(W, [NewC|Cs]).		% then make it an atom
readword(C,W,C2) :-			% otherwise
	get0(C1),       
	readword(C1,W,C2).	 	% start a new word

restword(C, [NewC|Cs], C2) :-
	in_word(C, NewC),
	get0(C1),
	restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
	is_num(C),
	!,
	get0(C2),
	number_word(C2, W1, C1, P10),
	Pow10 is P10 * 10,
	W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
	C =< 0'9,
	C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).