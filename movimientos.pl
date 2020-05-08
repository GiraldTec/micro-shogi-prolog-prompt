
%realizarMovimiento([('R',1,0),('B',3,1),('P',0,1)],[('b',0,0),('p',3,3)],S1,S2).
% F1-> Lista de fichas que pueden moverse, Salida->Lista de posiciones resultado, C-> ficha capturada (o espacio sino)
% F1-> Lista de fichas del adversario

realizarMovimiento(F1,F2,Salida1,Salida2) :- 
			member((F,X,Y),F1),
			append(F1,F2,Ft),
			movimiento(Ft,(F,X,Y),(Xm,Ym)),

			comprobarMovimiento(Ft,(F,Xm,Ym),C),
			
			sustituirEnLista(F1,(F,Xm,Ym),Salida1Aux),
			esEvolucion(F,C,Fe),
			sustituirEvolucion(Salida1Aux,(Fe,Xm,Ym),Salida1),
			
			eliminarEnLista(F2,C,Salida2).

% TODO standarizar movimientos

% comprobarMovimiento /5 
% > Ft fichas del tablero
% > (F,Xo,Yo) Ficha en posicion original
% > (F,Xm,Ym) Ficha movida
% < C la captura
% < (F1,Xm,Ym) Estado final de la ficha tras el movimiento

comprobarMovimiento(Ft,(F,Xm,Ym),C) :- 
			esValido(Ft,(F,Xm,Ym)),
			esCaptura(Ft,(F,Xm,Ym),C).

comprobarMovimiento(Ft,(Xo,Yo),(Xm,Ym),C,(F1,Xm,Ym)) :-
	sacaNombreFicha((Xo,Yo),Ft,F),
	movimiento(Ft,(F,Xo,Yo),(Xm,Ym)),
	esValido(Ft,(F,Xm,Ym)),
	esCaptura(Ft,(F,Xm,Ym),C),
	esEvolucion(F,C,F1).
	
sacaNombreFicha(_,[],' '):-!.
sacaNombreFicha((Y1,X1),[(F,Y2,X2)|_],F):- Y1 = Y2, X1 = X2, !.
sacaNombreFicha(F,[_|Xs],NF):- sacaNombreFicha(F,Xs,NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOVIMIENTOS DE LAS FICHAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PEON ---> CABALLO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% PEON %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Casillas posibles del Peon Superior (el que va hacia abajo)
movimiento(_,('P',X,Y),(X,Y2)) :- Y2 is Y+1, enTablero(X,Y2).

% Casillas posibles del Peon Inferior (el que va hacia arriba)
movimiento(_,('p',X,Y),(X,Y2)) :- Y2 is Y-1, enTablero(X,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% CABALLO %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Casillas posibles del Caballo Superior
movimiento(_,('N',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y+2, enTablero(X2,Y2).
movimiento(_,('N',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y+2, enTablero(X2,Y2).

% Casillas posibles del Caballo Inferior
movimiento(_,('n',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y-2, enTablero(X2,Y2).
movimiento(_,('n',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y-2, enTablero(X2,Y2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERAL DE ORO ---> TORRE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% GENERAL DE ORO %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

movimiento(_,('g',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,('g',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,('G',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y+1, enTablero(X2,Y2).
movimiento(_,('G',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y+1, enTablero(X2,Y2).
% Comunes...
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'G'; F = 'g') , X2 is X , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'G'; F = 'g') , X2 is X-1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'G'; F = 'g') , X2 is X+1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'G'; F = 'g') , X2 is X , Y2 is Y+1, enTablero(X2,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TORRE %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

movimiento(Ft,(F,X,Y),(X2,Y)) :- (F = 'R'; F = 'r'),torreIzq(Ft,(F,X,Y),X,(X2,Y)).
movimiento(Ft,(F,X,Y),(X2,Y)) :- (F = 'R'; F = 'r'),torreDer(Ft,(F,X,Y),X,(X2,Y)).
movimiento(Ft,(F,X,Y),(X,Y2)) :- (F = 'R'; F = 'r'),torreArr(Ft,(F,X,Y),Y,(X,Y2)).
movimiento(Ft,(F,X,Y),(X,Y2)) :- (F = 'R'; F = 'r'),torreAbj(Ft,(F,X,Y),Y,(X,Y2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REY NO EVOLUCIONA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y+1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X , Y2 is Y+1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y+1, enTablero(X2,Y2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERAL DE PLATA ---> LANCERO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% GENERAL DE PLATA %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Casillas posibles del Plata
movimiento(_,('S',X,Y),(X,Y2)) :- Y2 is Y+1, enTablero(X,Y2).
movimiento(_,('s',X,Y),(X,Y2)) :- Y2 is Y-1, enTablero(X,Y2).
% Comunes...
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'S'; F = 's') , X2 is X-1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'S'; F = 's') , X2 is X+1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'S'; F = 's') , X2 is X-1 , Y2 is Y+1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'S'; F = 's') , X2 is X+1 , Y2 is Y+1, enTablero(X2,Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% LANZA %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Casillas posibles de la Lanza Superior
movimiento(Ft,('L',X,Y),(X,Y2)) :- torreAbj(Ft,('L',X,Y),Y,(X,Y2)).

% Casillas posibles de la Lanza Inferior
movimiento(Ft,('l',X,Y),(X,Y2)) :- torreArr(Ft,('L',X,Y),Y,(X,Y2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ALFIL --> PEON CORONADO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% ALFIL %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

movimiento(Ft,(F,X,Y),(X2,Y2)) :- (F = 'B'; F = 'b'), alfilNO(Ft,(F,X,Y),X,Y,(X2,Y2)).
movimiento(Ft,(F,X,Y),(X2,Y2)) :- (F = 'B'; F = 'b') , alfilNE(Ft,(F,X,Y),X,Y,(X2,Y2)).
movimiento(Ft,(F,X,Y),(X2,Y2)) :- (F = 'B'; F = 'b') , alfilSE(Ft,(F,X,Y),X,Y,(X2,Y2)).
movimiento(Ft,(F,X,Y),(X2,Y2)) :- (F = 'B'; F = 'b') , alfilSO(Ft,(F,X,Y),X,Y,(X2,Y2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% PEON CORONADO %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Casillas posibles del Peón Coronado
movimiento(_,('c',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,('c',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,('C',X,Y),(X2,Y2)) :- X2 is X-1 , Y2 is Y+1, enTablero(X2,Y2).
movimiento(_,('C',X,Y),(X2,Y2)) :- X2 is X+1 , Y2 is Y+1, enTablero(X2,Y2).
% Comunes...
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'C'; F = 'c') , X2 is X , Y2 is Y-1, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'C'; F = 'c') , X2 is X-1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'C'; F = 'c') , X2 is X+1 , Y2 is Y, enTablero(X2,Y2).
movimiento(_,(F,X,Y),(X2,Y2)) :- (F = 'C'; F = 'c') , X2 is X , Y2 is Y+1, enTablero(X2,Y2).


%%
%% Validación
%%

enTablero(X,Y):-
	X >= 0,
	X < 4,
	Y >= 0,
	Y < 5.

% Un movimiento es valido:
% si come a una ficha de otro bando
% si no cae sobre ninguna casilla
% >lista de fichas en juego
% >nombre de la ficha que se movio 
% >posicion a la que se ha mueve la ficha 

esValido([],_).
esValido([(_,X2,Y2)|Fs],(Ficha,X1,Y1)):-
			(X2 =\= X1;
			Y2 =\= Y1),!,
			esValido(Fs,(Ficha,X1,Y1)).
esValido([(F,X,Y)|_],(Ficha,X,Y)):-
			distintoBando(F,Ficha).



% es captura
% si cae en una ficha de otro bando
% >lista de fichas en juego
% >ficha que se mueve y posicion a la que se mueve
% <captura

%esCaptura([('R',1,0),('B',3,1),('b',0,3),('P',0,1),('p',3,3)],('B',3,1),C).
esCaptura([],(_,X,Y),('',X,Y)).
esCaptura([(_,X2,Y2)|Fs],(Ficha,X1,Y1),Captura):-
	(X2 =\= X1;
	Y2 =\= Y1),!,
	esCaptura(Fs,(Ficha,X1,Y1),Captura).
esCaptura([(F,X,Y)|_],(_,X,Y),(F,X,Y)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
restarUnoOMas(1,0,1):-!.
restarUnoOMas(N,M,1):-
	N > 0,
	M is N-1.
restarUnoOMas(N,M2,D):-
	N > 0,
	M is N-1,
	restarUnoOMas(M,M2,D2),
	D is D2 + 1.
	
sumarUnoOMas(3,4,1):-!.
sumarUnoOMas(N,M,1):-
	N < 5,
	M is N+1.
sumarUnoOMas(N,M2,D):-
	N < 5,
	M is N+1,
	sumarUnoOMas(M,M2,D2),
	D is D2 + 1.
	
sustituirEnLista([],(_,_,_),[]).
sustituirEnLista([(F1,X1,Y1)|Xs],(F2,X2,Y2),[(F1,X1,Y1)|Ys]):-
			F1\==F2,
			sustituirEnLista(Xs,(F2,X2,Y2),Ys).
			
sustituirEnLista([(F,_,_)|Xs],(F,X2,Y2),[(F,X2,Y2)|Xs]).


sustituirEvolucion([],(_,_,_),[]).
sustituirEvolucion([(_,Xm,Ym)|Xs],(Fe,Xm,Ym),[(Fe,Xm,Ym)|Xs]):-!.
sustituirEvolucion([(F1,X1,Y1)|Xs],(Fe,X2,Y2),[(F1,X1,Y1)|Ys]):-
	sustituirEvolucion(Xs,(Fe,X2,Y2),Ys).


eliminarEnLista([],(_,_,_),[]).
eliminarEnLista([(F1,X1,Y1)|Xs],(F2,X2,Y2),[(F1,X1,Y1)|Ys]):-
			F1\==F2,
			eliminarEnLista(Xs,(F2,X2,Y2),Ys).
			
eliminarEnLista([(F,_,_)|Xs],(F,_,_),Xs).

% minuscula >96
% mayuscula <91
distintoBando(F1,F2) :- char_code(F1,V1), V1<91, char_code(F2,V2), V2>96,!.
distintoBando(F1,F2) :- char_code(F1,V1), V1>91, char_code(F2,V2), V2<96.

mismoBando(F1,F2):- \+distintoBando(F1,F2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MECANISMO DE EVOLUCION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% esEvolucion('b',('',4,5)).
esEvolucion(F,('',_,_),F):-!.
esEvolucion(F,(_,_,_),F1):-
	evolucion(F,F1).

evolucion('K','K'):-!.
evolucion('k','k'):-!.

evolucion('P','N'):-!.
evolucion('p','n'):-!.

evolucion('G','R'):-!.
evolucion('g','r'):-!.

evolucion('S','L'):-!.
evolucion('s','l'):-!.

evolucion('B','C'):-!.
evolucion('b','c'):-!.

evolucion(B,A):-
	evolucion(A,B),!.


fRs(O,_):-(O is 5;O is 10),!.
fRs(N,M):-
	M is N+1.
fRs(N,M2):-
	M is N+1,
	fRs(M,M2).

fRr(O,_):-(O is 5;O is 0),!.
fRr(N,M):-
	M is N-1.
fRr(N,M2):-
	M is N-1,
	fRr(M,M2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Funciones Auxiliares% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
alfilNO(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J - 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),distintoBando(F1,F),!.
alfilNO(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J - 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),mismoBando(F1,F),!,fail.
alfilNO(_,(_,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J - 1, enTablero(X2,Y2).
alfilNO(Ft,(F,X,Y),I,J,(X2,Y2)):- I1 is I - 1, J1 is J - 1, enTablero(I1,J1), alfilNO(Ft,(F,X,Y),I1,J1,(X2,Y2)).


alfilNE(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J - 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),distintoBando(F1,F),!.
alfilNE(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J - 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),mismoBando(F1,F),!,fail.
alfilNE(_,(_,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J - 1, enTablero(X2,Y2).
alfilNE(Ft,(F,X,Y),I,J,(X2,Y2)):- I1 is I + 1, J1 is J - 1, enTablero(I1,J1), alfilNE(Ft,(F,X,Y),I1,J1,(X2,Y2)).


alfilSE(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J + 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),distintoBando(F1,F),!.
alfilSE(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J + 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),mismoBando(F1,F),!,fail.
alfilSE(_,(_,_,_),I,J,(X2,Y2)):- X2 is I + 1, Y2 is J + 1, enTablero(X2,Y2).
alfilSE(Ft,(F,X,Y),I,J,(X2,Y2)):- I1 is I + 1, J1 is J + 1, enTablero(I1,J1), alfilSE(Ft,(F,X,Y),I1,J1,(X2,Y2)).


alfilSO(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J + 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),distintoBando(F1,F),!.
alfilSO(Ft,(F,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J + 1, enTablero(X2,Y2), member((F1,X2,Y2),Ft),mismoBando(F1,F),!,fail.
alfilSO(_,(_,_,_),I,J,(X2,Y2)):- X2 is I - 1, Y2 is J + 1, enTablero(X2,Y2).
alfilSO(Ft,(F,X,Y),I,J,(X2,Y2)):- I1 is I - 1, J1 is J + 1, enTablero(I1,J1), alfilSO(Ft,(F,X,Y),I1,J1,(X2,Y2)).

torreIzq(Ft,(F,_,Y),J,(X2,Y)):- X2 is J - 1, enTablero(X2,Y), member((F1,X2,Y),Ft),distintoBando(F1,F),!.
torreIzq(Ft,(F,_,Y),J,(X2,Y)):- X2 is J - 1, enTablero(X2,Y), member((F1,X2,Y),Ft),mismoBando(F1,F),!,fail.
torreIzq(_,(_,_,Y),J,(X2,Y)):- X2 is J - 1, enTablero(X2,Y).
torreIzq(Ft,(F,X,Y),J,(X2,Y)):- J1 is J - 1, enTablero(J1,Y), torreIzq(Ft,(F,X,Y),J1,(X2,Y)).


torreDer(Ft,(F,_,Y),J,(X2,Y)):- X2 is J + 1, enTablero(X2,Y), member((F1,X2,Y),Ft),distintoBando(F1,F),!.
torreDer(Ft,(F,_,Y),J,(X2,Y)):- X2 is J + 1, enTablero(X2,Y), member((F1,X2,Y),Ft),mismoBando(F1,F),!,fail.
torreDer(_,(_,_,Y),J,(X2,Y)):- X2 is J + 1, enTablero(X2,Y).
torreDer(Ft,(F,X,Y),J,(X2,Y)):- J1 is J + 1, enTablero(J1,Y), torreDer(Ft,(F,X,Y),J1,(X2,Y)).


torreArr(Ft,(F,X,_),J,(X,Y2)):- Y2 is J - 1, enTablero(X,Y2), member((F1,X,Y2),Ft),distintoBando(F1,F),!.
torreArr(Ft,(F,X,_),J,(X,Y2)):- Y2 is J - 1, enTablero(X,Y2), member((F1,X,Y2),Ft),mismoBando(F1,F),!,fail.
torreArr(_,(_,X,_),J,(X,Y2)):- Y2 is J - 1, enTablero(X,Y2).
torreArr(Ft,(F,X,Y),J,(X,Y2)):- J1 is J - 1, enTablero(X,J1), torreArr(Ft,(F,X,Y),J1,(X,Y2)).


torreAbj(Ft,(F,X,_),J,(X,Y2)):- Y2 is J + 1, enTablero(X,Y2), member((F1,X,Y2),Ft),distintoBando(F1,F),!.
torreAbj(Ft,(F,X,_),J,(X,Y2)):- Y2 is J + 1, enTablero(X,Y2), member((F1,X,Y2),Ft),mismoBando(F1,F),!,fail.
torreAbj(_,(_,X,_),J,(X,Y2)):- Y2 is J + 1, enTablero(X,Y2).
torreAbj(Ft,(F,X,Y),J,(X,Y2)):- J1 is J + 1, enTablero(X,J1), torreAbj(Ft,(F,X,Y),J1,(X,Y2)).
