:-use_module(library(clpfd)).
:-[movimientos].		% es un include para poder llamar a los metodos de "movimientos.pl"
:-[minimaxPoda].
:-[aperturasLibro].

:- dynamic opcion/1.
:- dynamic posiciones/2.
:- dynamic turno/1.
:- dynamic nivel/1.

%		   0     1     2     3
%		+-----+-----+-----+-----+
%	 0	|     |     |     |     |
%		+-----+-----+-----+-----+
%	 1	|     |     |     |     |
%		+-----+-----+-----+-----+
%	 2	|     |     |     |     |
%		+-----+-----+-----+-----+
%	 3	|     |     |     |     |
%		+-----+-----+-----+-----+
%	 4	|     |     |     |     |
%		+-----+-----+-----+-----+

% Ver tablero vac�o de muestra
muestra:-
	print('      0     1     2     3  \n'),
	print('   +-----+-----+-----+-----+\n'),
	print('0  |     |     |     |     |\n'),
	print('   +-----+-----+-----+-----+\n'),
	print('1  |     |     |     |     |\n'),
	print('   +-----+-----+-----+-----+\n'),
	print('2  |     |     |     |     |\n'),
	print('   +-----+-----+-----+-----+\n'),
	print('3  |     |     |     |     |\n'),
	print('   +-----+-----+-----+-----+\n'),
	print('4  |     |     |     |     |\n'),
	print('   +-----+-----+-----+-----+\n').
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Jugador 1 = minusculas %%
	%% Jugador 2 = mayusculas %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Opci�n de juego:		   %%
	%%						   %%
	%% a: Persona1 vs Persona2 %%
	%% b: Persona vs PC        %%
	%% c: PC1 vs PC2           %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	%%%%%%%%%%%%%%%%%%%%%%
	%% Memoria:			%%
	%%					%%
	%% opcion(X)		%%
	%% posiciones(X,Y)	%%
	%% turno(Num)		%%
	%% nivel(Num)		%%
	%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
comienzaPartida:-
	
	% Dejamos la memoria limpia:
	retractall(opcion(_)),
	retractall(posiciones(_,_)),
	retractall(turno(_)),
	retractall(nivel(_)),
	
	write('Proyecto de Micro-Shogi por:'),nl,
	write('o-------------------------o'),nl,
	write('| > @claraantolingarcia   |'),nl,
	write('| > ------tbd--------     |'),nl,
	write('| > @GiraldTec            |'),nl,
	write('o-------------------------o'),
	nl,nl,
	write('�A qu� modalidad quieres jugar?'),nl,
	write('\ta: Jugador1 vs Jugador2'),nl,
	write('\tb: Jugador vs PC'),nl,
	write('\tc: PC1 vs PC2'),nl,
	write('\teoc -> Salir'),nl,
	
	read(X),
	(X = 'a'; X = 'b'; X = 'c'), !,
	assert(opcion(X)),
	
	write('Quieres empezar el primero o el segundo?\n'),
	write('\t1: Primero\n'),
	write('\t2: Segundo\n'),
	
	read(Y),
	asignaPosJugadores(Y),
	
	assert(turno(1)),
	
	pregunta,
	
	TableroInicial = [('K',0,0),('B',1,0),('G',2,0),('S',3,0),('P',0,1),('k',3,4),('b',2,4),('g',1,4),('s',0,4),('p',3,3)],
	tablero(TableroInicial),!.	% empieza el Jugador 1 a mover
	
comienzaPartida :-
	write('Hasta pronto!').
	
	
pregunta:-
	(opcion('b'); opcion('c')),!,
	write('�A qu� nivel de �rbol quieres jugar?\n'),
	read(N),
	assert(nivel(N)).
	
pregunta.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
tablero(Xs):-
	Acs = [['','','',''],['','','',''],['','','',''],['','','',''],['','','','']],
	tableroAux(Xs,Acs,Zss),!,
	rellenaTablero(Zss),
	escribePantalla,
	primeraJugada(Xs,Xs2),
	cambioDeTurno,
	tablero2(Xs2).
	
tablero2(Xs):-
	Acs = [['','','',''],['','','',''],['','','',''],['','','',''],['','','','']],
	tableroAux(Xs,Acs,Zss),!,
	rellenaTablero(Zss),
	escribePantalla,
	primeraJugada(Xs,Xs2),
	cambioDeTurno,
	tablero3(Xs2).
	
tablero3(Xs):-
	condicionVictoria(Xs,Ganador),
	(Ganador = 'PC'; Ganador = 'PC1'; Ganador = 'PC2';
	Ganador = 'jugador' ; Ganador = 'jugador1' ; Ganador = 'jugador2'),!,
	
	Acs = [['','','',''],['','','',''],['','','',''],['','','',''],['','','','']],
	tableroAux(Xs,Acs,Zss),!,
	rellenaTablero(Zss),
	escribePantalla,
	
	write('*******  Game Over *******'), nl,
	write('���El jugador '),
	write(Ganador),
	write(' ha ganado la partida!!!'), nl,
	write('**************************'), nl,

	comienzaPartida.
	
tablero3(Xs):-
	Acs = [['','','',''],['','','',''],['','','',''],['','','',''],['','','','']],
	tableroAux(Xs,Acs,Zss),!,
	rellenaTablero(Zss),
	escribePantalla,
	mueve(Xs,Xs2),
	cambioDeTurno,
	tablero3(Xs2).
	
condicionVictoria(Xs,G):-
	buscaReyes(Xs,[F|Ys]),
	length([F|Ys],1),!,
	averiguaGanador(F,G).
	
condicionVictoria(_,'').
	
averiguaGanador(F,G):-	% ficha mayuscula
	char_code(F,Vf),
	Vf < 91,!,
	posiciones((J1,X),(J2,Y)),
	sacaGanadorMay(J1,X,J2,Y,G).
	
averiguaGanador(_,G):-	% ficha minuscula
	posiciones((J1,X),(J2,Y)),
	sacaGanadorMin(J1,X,J2,Y,G).
	
sacaGanadorMay(J1,2,_,_,J1):-!.
sacaGanadorMay(_,_,J2,2,J2).

sacaGanadorMin(J1,1,_,_,J1):-!.
sacaGanadorMin(_,_,J2,1,J2).
	
	
buscaReyes([],[]).
buscaReyes([(F,_,_)|Xs],Zs):-
	(F = 'k' ; F = 'K'), !,
	buscaReyes(Xs,Ys),
	append([F],Ys,Zs).
	
buscaReyes([_|Xs],Zs):-
	buscaReyes(Xs,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escribePantalla:-
	turno(N),
	posiciones((J,N),_), !,
	write(J), 
	write(': mueve ficha'), escribeLado(N), nl,
	write('           Introduce la posici�n de la ficha que quieras mover "(Columna,Fila)"\n').
	
escribePantalla:-
	turno(N),
	posiciones(_,(J,N)),
	write(J), 
	write(': mueve ficha'), escribeLado(N), nl,
	write('           Introduce la posici�n de la ficha que quieras mover "(Columna,Fila)"\n').

asignaPosJugadores(N):-
	opcion('a'), !,
	quienEstaJugando(N,X,Y),
	assert(posiciones(('jugador1',X),('jugador2',Y))).
	
asignaPosJugadores(N):-
	opcion('b'), !,
	quienEstaJugando(N,X,Y),
	assert(posiciones(('jugador',X),('PC',Y))).
	
asignaPosJugadores(N):-
	opcion('c'),
	quienEstaJugando(N,X,Y),
	assert(posiciones(('PC1',X),('PC2',Y))).
	
quienEstaJugando(1,1,2).
quienEstaJugando(2,2,1).

cambioDeTurno:-
	turno(1),!,
	retractall(turno(_)),
	assert(turno(2)).
	
cambioDeTurno:-
	turno(2),
	retractall(turno(_)),
	assert(turno(1)).

escribeLado(1):-
	write(' el de abajo').

escribeLado(2):-
	write(' el de arriba').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
mueve(Xs,Xs2):-	% Turno del PC
	turno(2), 
	(posiciones((J,2),_);posiciones(_,(J,2))),
	(J = 'PC'; J = 'PC2'; J='PC1'),
	nivel(N), 
	separa(Xs,Fmax,Fmin),
	minimax(Fmax,Fmin,N,_,Fr,FnR),
	append(Fr,FnR,Xs2).
	
mueve(Xs,Xs2):-	% Turno del PC
	turno(1), 
	(posiciones((J,1),_);posiciones(_,(J,1))),
	(J = 'PC';J = 'PC1'; J = 'PC2'),
	nivel(N),
	separa(Xs,Fmin,Fmax),
	minimax(Fmax,Fmin,N,_,Fr,FnR),
	append(Fr,FnR,Xs2).
	
mueve(Xs,Xs2):-	% Turno del jugador
	read(X),						% leemos la ficha a mover
	evalua(X,Xs,Y,FichaCorrecta),	% preguntamos por su casilla destino
	FichaCorrecta = 1,
	comprobarMovimiento(Xs,X,Y,FC,FF),	% FC = Ficha Capturada, FF = Ficha final (la ficha que mueve, evolucione o no)
	realizaMov(X,FF,FC,Xs,Xs2).
	
mueve(Xs,Xs2):-	% La ficha no es correcta
	write('Posicion de ficha incorrecta\n'),
	write('Introduce la posici�n de una ficha que quieras mover "(Columna,Fila)"\n'),
	mueve(Xs,Xs2).

	%%%%%%% XXX comprobar
primeraJugada(Xs,Xs2):-
	turno(T),
	(posiciones((J,T),_);posiciones(_,(J,T))),
	(J = 'PC';J = 'PC2'),
	T = 2, !,
	separa(Xs,Fmax,Fmin),
	primerMovimiento(Fmax,Fr),
	append(Fr,Fmin,Xs2).

primeraJugada(Xs,Xs2):-	% Turno del PC
	turno(T),
	(posiciones((J,T),_);posiciones(_,(J,T))),
	(J = 'PC';J = 'PC1'),
	T = 1, !,
	separa(Xs,Fmax,Fmin),
	primerMovimiento(Fmin,Fr),
	append(Fr,Fmax,Xs2).
	
primeraJugada(Xs,Xs2):-	% Turno del jugador
	mueve(Xs,Xs2).
	
	%%%%%%% XXX comprobar
	
evalua(X,Xs,Y,FichaCorrecta):-
	evaluaFichaJugador(X,Xs,FichaCorrecta),
	FichaCorrecta = 1,!,
	write('           Introduce la posici�n de destino "(Columna,Fila)"\n'),
	read(Y).

evalua(_,_,_,_,0).
	
evaluaFichaJugador(F,Xs,1):-	% jugador 1 ficha correcta
	turno(1),
	sacaNombreFicha(F,Xs,NF),
	char_code(NF,V),
	V > 96, V < 123, !.
	
evaluaFichaJugador(_,_,0):-		% jugador 1 ficha incorrecta
	turno(1),!.

evaluaFichaJugador(F,Xs,0):-	% jugador 2 ficha incorrecta
	turno(2),
	sacaNombreFicha(F,Xs,NF),
	char_code(NF,V),
	((V > 96, V < 123); V = 32), !.
	
evaluaFichaJugador(_,_,1).		% jugador 2 ficha correcta
	
sacaSiFichaComida((X,Y),[(F,X,Y)|_],F):- !.
sacaSiFichaComida((X,Y),[_|Xs],FC):-
	sacaSiFichaComida((X,Y),Xs,FC).
	
separa([],[],[]).
separa([(F,X,Y)|Xs],Fmax,[(F,X,Y)|Fmin]):-
	char_code(F,V),
	V > 96, V < 123, !,
	separa(Xs,Fmax,Fmin).
	
separa([(F,X,Y)|Xs],[(F,X,Y)|Fmax],Fmin):-
	separa(Xs,Fmax,Fmin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


realizaMov((XOrigen,YOrigen),FichaOrigenCambiada,('',_,_),Xs,Zs):-	% No hemos comido a nadie, insertamos FichaOrigenCambiada en su nuevo lugar
	!, elimina(('',XOrigen,YOrigen),Xs,[],Xs1),
	append(Xs1,[FichaOrigenCambiada],Zs).
	
realizaMov((XOrigen,YOrigen),FichaOrigenCambiada,FichaComida,Xs,Zs):-	% Hemos comido a FichaComida, eliminamos FichaComida del tablero ytambien (XOrigen,YOrigen),e insertamos FichaOrigenCambiada
	elimina(FichaComida,Xs,[],Xs1),
	elimina(('',XOrigen,YOrigen),Xs1,[],Xs2),
	append(Xs2,[FichaOrigenCambiada],Zs).
	
	
elimina(_,[],Ac,Ac):- !.

elimina((_,Y1,X1),[(_,Y2,X2)|Xs],Ac,Xs2):-
	%F1 = F2,
	Y1 = Y2,
	X1 = X2, !,
	append(Ac,Xs,Xs2).
	
elimina(Y,[X|Xs],Ac,Xs2):-
	append(Ac,[X],Ac1),
	elimina(Y,Xs,Ac1,Xs2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vamos recorriendo cada ficha
tableroAux([],Acss,Acss):- !.

tableroAux([F|Xs],Acss,Zss):-
	buscaFila(F,0,Acss,[],Zcs),
	tableroAux(Xs,Zcs,Zss).

% buscamos la fila correcta e insertamos la ficha en ella
% buscaFila(_,_,[],Ac,Ac).

buscaFila((Nombre,Col,Fila),N,[Xs|Xss],Acs,Zss):-	% estamos en la fila correcta
	Fila = N,
	buscaColumna((Nombre,Col,Fila),0,Xs,[],Ys),
	append(Acs,[Ys],Z),
	append(Z,Xss,Zss).
	
buscaFila(F,N,[Xs|Xss],Acss,Zss):-	% no estamos en la fila correcta todav�a
	N1 is N+1,
	append(Acss,[Xs],Zcss),
	buscaFila(F,N1,Xss,Zcss,Zss).
	
% metemos la ficha en la columna correcta
buscaColumna(_,_,[],Ac,Ac).
	

buscaColumna((Nombre,Col,_),N,[_|Xs],Ac,Ys):-	% estamos en la columna correcta
	Col = N, !,
	append(Ac,[Nombre],Zs),
	append(Zs,Xs,Ys).
	
buscaColumna(F,N,[XX|Xs],Acs,Ys):-	% no estamos en la posicion correcta todav�a, yhabia una ficha
	N1 is N+1,
	append(Acs,[XX],Zs),
	buscaColumna(F,N1,Xs,Zs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rellenamos el tablero con las listas que nos dan
rellenaTablero([[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4],[E1,E2,E3,E4]]) :-
	FilaA = ['|',A1,'|',A2,'|',A3,'|',A4,'|'],
	FilaB = ['|',B1,'|',B2,'|',B3,'|',B4,'|'],
	FilaC = ['|',C1,'|',C2,'|',C3,'|',C4,'|'],
	FilaD = ['|',D1,'|',D2,'|',D3,'|',D4,'|'],
	FilaE = ['|',E1,'|',E2,'|',E3,'|',E4,'|'],

	convierte(FilaA,'',FilaAStringAux),
	convierte(FilaB,'',FilaBStringAux),
	convierte(FilaC,'',FilaCStringAux),
	convierte(FilaD,'',FilaDStringAux),
	convierte(FilaE,'',FilaEStringAux),
	
	concat(' 0  ', FilaAStringAux, FilaAString),
	concat(' 1  ', FilaBStringAux, FilaBString),
	concat(' 2  ', FilaCStringAux, FilaCString),
	concat(' 3  ', FilaDStringAux, FilaDString),
	concat(' 4  ', FilaEStringAux, FilaEString),
	
	pintaTablero(FilaAString,FilaBString,FilaCString,FilaDString,FilaEString).
	
% a�adimos el ultimo elemento yun enter '\n' al final.
convierte([X],S,Z) :-
	creaFicha(X,W),
	concat(S,W,Y),
	concat(Y,'\n',Z),!.
	
% Convertimos una lista de caracteres en una cadena string
convierte([X|Xs],S,Z) :-
	creaFicha(X,W),
	concat(S,W,Y),
	convierte(Xs,Y,Z).
	
% Transformamos la ficha 'A' en \A/  la ficha 'a' en / A\
creaFicha('','     '):- !.
creaFicha('|','|'):- !.
creaFicha(X,W):-	% Ficha minuscula
	char_code(X,V),
	V > 96, V < 123, !,
	V2 is V - 32,
	atom_codes(C,[V2]),
	atom_chars(W,[' ','/',C,'\\',' ']).
	
creaFicha(X,W):-	% Ficha mayuscula
	atom_chars(W,[' ','\\',X,'/',' ']).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Pintamos el tablero ylo sacamos por pantalla.
% Tablero inicial:
% tablero([('K',0,0),('B',1,0),('G',2,0),('S',3,0),('P',0,1),('k',3,4),('b',2,4),('g',1,4),('s',0,4),('p',3,3)]).

pintaTablero(A,B,C,D,E):-
	print('       0     1     2     3   \n'),
	print('    +-----+-----+-----+-----+\n'),
	print(A),
	print('    +-----+-----+-----+-----+\n'),
	print(B),
	print('    +-----+-----+-----+-----+\n'),
	print(C),
	print('    +-----+-----+-----+-----+\n'),
	print(D),
	print('    +-----+-----+-----+-----+\n'),
	print(E),
	print('    +-----+-----+-----+-----+\n').
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%