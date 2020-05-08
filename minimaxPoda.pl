:-[movimientos].
:-[heuristica].
:- dynamic mejor/3.
:- dynamic alfa/1.
:- dynamic beta/1.
:- dynamic tmp/3.
 
 
%minimax([('K',0,0),('B',1,0),('G',2,0),('S',3,0),('P',0,1)],[('k',3,4),('b',2,4),('g',1,4),('s',0,4),('p',3,3)],N,MVal,FxR,FnR). 
%%%%% Interfaz
minimax(Fmax,Fmin,N,MVal,FxR,FnR) :-
		retractall(alfa(_)),retractall(beta(_)),retractall(mejor(_,_,_)),retractall(tmp(_,_,_)),
		assert(alfa(-99999)),assert(beta(99999)),
		jugada(Fmax,Fmin,N,MVal,[FxR|_],[FnR|_]).
		%mejor(MVal,FxR,FnR).

%%% Ejecuta y guarda si se ha mejorado el resultado
jugada(Fmax,Fmin,N,Val,FmaxR,FminR) :- 
			repetir(max,Fmax,Fmin,N,0,Val,FmaxR,FminR).
			%guardarSiMejor(Val,FmaxR,FminR).

%%%%% Comprobación de hoja por falta de fichas
repetir(_,[],Fmin,_,_,-99999,[],[Fmin]) :- !.
repetir(_,Fs,[],_,_,99999,[Fs],[]) :- !.

repetir(_,Fmax,Fmin,_,_,-99999,[Fmax],[Fmin]) :-
			\+(member(('k',_,_),Fmin);member(('K',_,_),Fmin)),!.
repetir(_,Fmax,Fmin,_,_,+99999,[Fmax],[Fmin]) :-
			\+(member(('k',_,_),Fmax);member(('K',_,_),Fmax)),!.


%%%%% Casos base (llego al nivel buscado)
% XXX cambiaod y aun asi hace lo mismo
% poda Alfa ---- (alfa(V2),V2=<NVal2,!,write('poda A'),nl);
% poda Beta ---- (beta(V),NVal2>=V,!,write('poda B'),nl);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% cambiado de "[FxR],[FnR]) :- !,"
repetir(max,Fmax,Fmin,0,_,NVal,[FmaxR],[FminR]) :- !,
			(
				(
				realizarMovimiento(Fmax,Fmin,FxR,FnR), %movimiento de max
				heuristica(FxR,FnR,NVal2),
				%write('nodo con valor: '),write(NVal2),nl,
				guardarVTmp(NVal2,FxR,FnR), % guarda el valor temporal (aqui se guardara el mejor hijo)
				(
				%(beta(V),NVal2>=V,!,write('poda B'),nl);
				fail) % si no se ha podado, repite
				);(V=V) %dummy,para que no pete
			),
			tmp(NVal,FmaxR,FminR), % coge los valores del mejor
			actualizaAlfa(NVal), % de la poda
			retract(tmp(_,_,_)). %elimina el temporal
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% cambiado de "[FxR],[FnR]) :- !,"
repetir(min,Fmax,Fmin,0,_,NVal,[FmaxR],[FminR]) :- !,
			(
				(
				realizarMovimiento(Fmin,Fmax,FnR,FxR),
				heuristica(FxR,FnR,NVal2),
				%write('nodo con valor: '),write(NVal2),nl,
				guardarVTmp(NVal2,FxR,FnR),
				(
				(alfa(V2),V2=<NVal2,!);
				fail)
				);(V=V)
			),
			tmp(NVal,FmaxR,FminR),
			actualizaAlfa(NVal),
			retract(tmp(_,_,_)).
			
%%%%%% Caso recursivo
repetir(max,Fmax,Fmin,N,P,NVal,FmaxR,FminR) :-
			P2 is P+1, %aumenta la profundidad a la que ha llegado
			N2 is N-1, %disminuye el numero de veces que tiene que repetir
			(
				(
				realizarMovimiento(Fmax,Fmin,FxR,FnR), %movimiento de max
				repetir(min,FxR,FnR,N2,P2,NVal2,FmaxR2,FminR2), %llamada recursiva
				append([FnR],FminR2,FminR1), % agrega el nuevo movimiento
				append([FxR],FmaxR2,FmaxR1),
				%actualizaAlfa(NVal2),
				guardarVTmp(NVal2,FmaxR1,FminR1), % guarda el valor temporal (aqui se guardara el mejor hijo)
				(
				%(beta(V),NVal2>=V,!);
				fail) % si no se ha podado, repite
				);(V=V) %dummy,para que no pete
			),
			tmp(NVal,FmaxR,FminR), % coge los valores del mejor
			actualizaAlfa(NVal), % de la poda
			retract(tmp(_,_,_)). %elimina el temporal

repetir(min,Fmax,Fmin,N,P,NVal,FmaxR,FminR) :-
			P2 is P+1,%aumenta la profundidad a la que ha llegado
			N2 is N-1, %disminuye el numero de veces que tiene que repetir
			(
				(
				realizarMovimiento(Fmin,Fmax,FnR,FxR),
				repetir(max,FxR,FnR,N2,P2,NVal2,FmaxR2,FminR2),
				append([FnR],FminR2,FminR1),
				append([FxR],FmaxR2,FmaxR1),
				%actualizaBeta(NVal2),
				guardarVTmp(NVal2,FmaxR1,FminR1),
				(
				(alfa(V2),V2=<NVal2,!);
				fail)
				);(V=V)
			),
			tmp(NVal,FmaxR,FminR),
			actualizaAlfa(NVal),
			retract(tmp(_,_,_)).
	

%en caso de que haya habido poda,.
%repetir(_,Fmax,Fmin,_,_,-99999,[Fmax],[Fmin]).

			
actualizaAlfa(NV):-
			alfa(V),!,
			((NV>V,retractall(alfa(_)),assert(alfa(NV)));NV=<V).
actualizaAlfa(NV):-
			assert(alfa(NV)).	
			
actualizaBeta(NV):-
			beta(V),!,
			((NV<V,retractall(beta(_)),assert(beta(NV)));NV>=V).
actualizaBeta(NV):-
			assert(beta(NV)).	
				
			
meterAlfaEnBeta :-
			alfa(V),retractall(beta(_)),assert(beta(V)).
	
meterBetaEnAlfa :-
			beta(V),retractall(alfa(_)),assert(alfa(V)).
			
guardarVTmp(Val,FmaxR,FminR) :-
			tmp(MVal,_,_),!,
			((Val>MVal,
			retract(tmp(_,_,_)),
			assert(tmp(Val,FmaxR,FminR))
			%,write('mejorValor'),write(Val),nl
			);Val=<MVal).
guardarVTmp(Val,FmaxR,FminR) :- 
			assert(tmp(Val,FmaxR,FminR)).

guardarSiMejor(Val,[FmaxR|_],[FminR|_]) :-
%guardarSiMejor(Val,FmaxR,FminR) :-  % Esta linea sirve para que devuelva la lista de jugadas
			mejor(MVal,_,_),!,
			((Val>MVal,
			retractall(mejor(_,_,_)),
			assert(mejor(Val,FmaxR,FminR)));Val=<MVal).
guardarSiMejor(Val,[FmaxR|_],[FminR|_]) :-		
%guardarSiMejor(Val,FmaxR,FminR) :-  % Esta linea sirve para que devuelva la lista de jugadas
			assert(mejor(Val,FmaxR,FminR)).