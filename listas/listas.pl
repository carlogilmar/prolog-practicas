%listas en prolog
%conteo
%suma de elementos
%buscar
%concatenar
%reversa
%ultimo elemento
%elminar elemento
%añadir por delante
%añadir por detras
%lista vacia
%listas iguales

%caso 1------------------------------------------------conteo de elementos-------------------------------
%cuando una lista este vacia es decir entre corchetes
%el numero de elementos es 0

		cuenta_elementos([],0).


%caso 2 hacer la relacion de conteo
%N es el numero de elementos de la lista
%la barrita indica la cabeza o final de la lista
%el guion bajo es una variable anonima

%en la relacion mandaremos el final de la cola L 
%Tam es el tamanio de los elementos
%N es el tamaño de la lista mas la cabeza por eso el mas uno

		cuenta_elementos([_|L],N):- cuenta_elementos(L,TAM), N is TAM+1. 


%en el interprete se debe llamar cuenta_elementos([1,2,3],N).
%N=3

%caso 2-----------------------------------------------sumar elementos-------------------------------------

%sumar los elementos de una lista

%uso de recursividad en caso vacio asi como caso nvacio

%caso vacio
%si se envia una lista vacia la suma es cero

		suma_lista([],0).

%caso nvacio
%se tiene el inicio X final L sumatoria N

		suma_lista([X|L], N):- suma_lista(L,C), N is C+X.

%ejemplo de corrida recursiva 4,7,2

%suma_lista(4,7,2),N
%es caso nvacio--------------------------------------------------primera iteracion
%por lo tanto suma_lista([4|7,2],N)
%y en la relacion puntoguion suma_lista([7,2], C)
%entonces N es C+ X pero no se sabe cual es C ni X, pendiente

%suma_lista([7,2],C) es caso nvacio------------------------------segunda iteracion
%por lo tanto suma_lista([7|2],N)
%y en la relacion punto guion suma_lista([2],C), N es C mas X
%pendiente la ultima parte

%suma_lista([2],C) es un caso nvacio-----------------------------tercera iteracion
%suma_lista([2| ],N)
%en la relacion puntoguion suma_lista([],C)
%ultima parte pendiente

%suma_lista([],C) es un caso vacio-------------------------------cuarta iteracion
%por lo tanto el valor de C es cero

%------------------------------------------------regresando a la tercera iteracion
%regresamos a completar las partes pendientes
%X inicio de la lista para esta iteracion equivale a 2
%por lo tanto N es igual a C=0 mas 2
%N vale 2

%--------------------------------------------------regresando a la segunda iteracion
%el valor de N se convierte en el valor de C
%N es C que vale 2 mas el inicio de la lista X que vale 7
%N vale 9

%-------------------------------------------------regresando a la primera iteracion
%N es C que vale 9 mas el inicio de la listA que vale 4
%N vale 13

%caso3------------------------------------------------------------buscar---------------------------

%usar la funcion member para buscar un elemento en una lista
%en interprete usar de la siguiente forma
%member(8, [1,2,3,4,5,7,8])
%true

%tambien permite agregar elementos a una cola no instanciada

%member(1, lista_sin_instancia).


%caso 4---------------------------------------------------- Concatenar------------------------------

%caso vacio
%si noexiste lista llena entonces el elemento forma el primer elemento de la lista
	
	concatenar([],L,L).

%caso nvacio
%el primer elemento es la lista
%el segundo elemento es el que se agrega
%el tercer elemento es la lista modificada

%la relacion puntoguion toma la cola de la lista inicial, el elemento nuevo X
%se itera

    concatenar([A|B], X, [A|C]):- concatenar(B,X,C).

%caso5--------------------------------------------------------------------- ultimoelemento --------------------

%caso de un elemento
%cuando solo hay un elemeneto en la lista el es el ultimo elemento tambien

    ultimo(E,[E|[]]).

%caso varios elementos
%la relacion puntoguion se llama a si misma con el elemento ultimo en buscqueda
%con la cola

    ultimo(E,[_|C]):- ultimo(E,C).

%ultimo(E,[1,2,3])
%ultimo(E,[2,3])
%ultimo(E,[3])


%primerelemento----------------------------------------------------------------

%cuando solo hay un elemento, ese es el primero babe
primero([E], [E|_ ]).

%primero(E,[C|_]):- primero(E,C).

%primero(a,1,2,3)
%primero(a,1)



%caso6------------------------------------------------reversa---------------------------------
%en interprete usar
%reverse([1,2,3,4],X).
%equis es la reversa

%caso7--------------------------------------------  eliminiar un elemento---------------------

%para una lista simple

    eliminar_elemento([X|C],X,C).

%para una lista con mas elementos

    eliminar_elemento([Y|C1],E,[Y|C2]):-eliminar_elemento(C1,E,C2).


%ejemplo
%eliminar_elemento([1,2,3],2,R)
%R es (1,3)

% -------- añadir por delante (L,E,R) -------------------
    añadir_por_delante([X|Xs],E,R):-concatenar([E],[X|Xs],R).

%----------   añadir por detras(L,E,R) ------------------
    añadir_por_detras([],[E],[E]).
    añadir_por_detras([X|Xs],E,[X|Xz]):-concatenar(Xs,[E],Xz).

%-------------   Lista Vacia --------------------------
     es_lista_vacia(L):- L == [].
     es_lista_vacia([|]).

    %-------------  listas Iguales -----------------------
     listas_iguales(X,Y):- X==Y.
     listas_iguales([A|Ca],[B|Cb]):- A==B,listas_iguales(Ca,Cb).

%-----------------------  palindromo(L) -----------------
    palindromo(L):- reverse(L,L).


