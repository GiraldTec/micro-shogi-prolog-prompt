:-[movimientos].

heuristica(Fyo,_,-99999):-	\+(member(('k',_,_),Fyo);member(('K',_,_),Fyo)),!. 
% mi rey no está en juego

heuristica(_,Ftu,+99999):-	\+(member(('k',_,_),Ftu);member(('K',_,_),Ftu)),!. 

heuristica(Fyo,Ftu,NVal):-
	append(Fyo,Ftu,Ft),
	sumaH(Fyo,Ftu,Ft,Hyo),
	sumaAdversario(Ftu,Fyo,Ft,Htu),
	diferenciaPesos(Fyo,Ftu,0,DP),	% Calculo la diferencia de los pesos de mis fichas con las del contrario
	A is Hyo - Htu,
	NVal is A + DP,!.
	%NVal is Hyo + DP,!.


% sumaH 
% El algoritmo a seguir es A + B = valor de la heuristica para esa configuracion de tablero
% donde:

sumaH([],_,_,Ac,Ac). % al final del todo devuelve el valor acumulado
sumaH(Fyo,_,_,H):-
	pBloqueado(Fyo,PP),
	lBloqueado(Fyo,PL),
	nBloqueado(Fyo,PN),
	fichasAdelantadas(Fyo,PA),
	penalizacionRey(Fyo,Rey),		%penalizar si el rey está muy adelantado rey
	A is PP + PL,
	B is A + PA,
	C is B + PN,
	H is C + Rey.
	
sumaAdversario([],_,_,Ac,Ac).
sumaAdversario(Fyo,_,_,H):-
	pBloqueado(Fyo,PP),
	lBloqueado(Fyo,PL),
	nBloqueado(Fyo,PN),
	fichasAdelantadas(Fyo,PA),
	A is PP + PL,
	B is A + PA,
	C is B + PN,
	H is C/2.

penalizacionRey([],0):-!.
penalizacionRey([('K',_,0)|_],0):-!.
penalizacionRey([('k',_,4)|_],0):-!.
penalizacionRey([('K',_,1)|_],-50):-!.
penalizacionRey([('k',_,3)|_],-50):-!.
penalizacionRey([('K',_,_)|_],-50000):-!.
penalizacionRey([('k',_,_)|_],-50000):-!.
penalizacionRey([_|Xs],Rey):-
	penalizacionRey(Xs,Rey).	

diferenciaPesos([],[],Ac,Ac).

diferenciaPesos([],[(F,_,_)|Fb],Ac,DP):-
	peso(F,P),
	Ac1 is Ac - P,
	diferenciaPesos([],Fb,Ac1,DP).
	
diferenciaPesos([(F,_,_)|Fa],[],Ac,DP):-
	peso(F,P),
	Ac1 is Ac + P,
	diferenciaPesos(Fa,[],Ac1,DP).
	
diferenciaPesos([(FA,_,_)|Fa],[(FB,_,_)|Fb],Ac,DP):-
	peso(FA,PA),
	peso(FB,PB),
	C is Ac + PA,		% sumamos nuestro peso
	Ac1 is C - PB,		% restamos el peso contrincante
	diferenciaPesos(Fa,Fb,Ac1,DP).

pBloqueado(Fichas,PP):-
	member(('P',_,Y),Fichas),
	Y = 4, !,
	peso('P',P),
	PP is -50 * P.
	
pBloqueado(Fichas,PP):-
	member(('p',_,Y),Fichas),
	Y = 0, !,
	peso('p',P),
	PP is -50 * P.
	
pBloqueado(_,0).
	
lBloqueado(Fichas,PL):-
	member(('L',_,Y),Fichas),
	Y = 4, !,
	peso('L',P),
	PL is -50 * P.
	
lBloqueado(Fichas,PL):-
	member(('l',_,Y),Fichas),
	Y = 0, !,
	peso('l',P),
	PL is -50 * P.
	
lBloqueado(_,0).

nBloqueado(Fichas,PN):-
	member(('N',_,Y),Fichas),
	Y > 2, !,
	peso('N',P),
	PN is -50 * P.
	
nBloqueado(Fichas,PN):-
	member(('n',_,Y),Fichas),
	Y < 2, !,
	peso('n',P),
	PN is -50 * P.
	
nBloqueado(_,0).


fichasAdelantadas([],0).
fichasAdelantadas([(F,_,Y)|Fyo],PA):- % si mi ficha es minúscula yesta adelantada 
	fichasAdelantadas(Fyo,Paux),
	char_code(F,Vf),
	Vf > 96,
	Y < 4,!,
	peso(F,P),
	Paux2 is 100 * P,
	PA is Paux + Paux2.
fichasAdelantadas([(F,_,_)|Fyo],PA):- % si mi ficha es minúscula yesta NO adelantada 
	fichasAdelantadas(Fyo,Paux),
	char_code(F,Vf),
	Vf > 96,!,
	PA is Paux + 0.

fichasAdelantadas([(F,_,Y)|Fyo],PA):- % si mi ficha es mayúscula yesta adelantada 
	fichasAdelantadas(Fyo,Paux),
	char_code(F,Vf),
	Vf < 91,
	Y > 0,!,
	peso(F,P),
	Paux2 is 100 * P,
	PA is Paux + Paux2.
fichasAdelantadas([(F,_,_)|Fyo],PA):- % si mi ficha es mayúscula yesta NO adelantada 
	fichasAdelantadas(Fyo,Paux),
	char_code(F,Vf),
	Vf < 91,!,
	PA is Paux + 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% PESOS CAPTURAS %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peso('K',3).
peso('k',3).

peso('B',3.5).
peso('b',3.5).

peso('G',3).
peso('g',3).

peso('P',1).
peso('p',1).

peso('C',3).
peso('c',3).

peso('S',4).
peso('s',4).

peso('N',2).
peso('n',2).

peso('L',1.3).
peso('l',1.3).

peso('R',5).
peso('r',5).
