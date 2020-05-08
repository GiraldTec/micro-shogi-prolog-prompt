:-[movimientos].
:-[heuristica].
:- dynamic mejor/3 .

minimax(Fmax,Fmin,N,MVal,FxR,FnR) :-
		((jugada(Fmax,Fmin,N,Val,[FmaxR|_],_),
		fail);
		(mejor(MVal,FxR,FnR),retractall(mejor(_,_,_)))).

jugada(Fmax,Fmin,N,Val,FmaxR,FminR) :- 
			repetir(Fmax,Fmin,N,1,Val,FmaxR,FminR),
			guardarSiMejor(Val,FmaxR,FminR).


repetir([],Fmin,_,P,NVal,[],Fmin) :- !, NVal is -99999/P.
repetir(Fs,[],_,P,NVal,Fs,[]) :- !, NVal is 99999/P.
repetir(Fmax,Fmin,0,P,NVal,[FxR],[FnR]) :-
			realizarMovimiento(Fmax,Fmin,FxR,FnR),
			heuristica(FxR,FnR,NV),
			NVal is NV/P.
		
repetir(Fmax,Fmin,N,P,NVal,FmaxR,FminR) :-
			N =\= 0,
			P2 is P+1,
			N2 is N-1,
			realizarMovimiento(Fmax,Fmin,FxR,FnR),
			repetir(FnR,FxR,N2,P2,NVal,FminR2,FmaxR2),
			append([FnR],FminR2,FminR),
			append([FxR],FmaxR2,FmaxR).
			
guardarSiMejor(Val,[FmaxR|_],[FminR|_]) :-
			mejor(MVal,_,_),!,
			((Val>MVal,
			retractall(mejor(_,_,_)),
			assert(mejor(Val,FmaxR,FminR)));Val=<MVal).
guardarSiMejor(Val,[FmaxR|_],[FminR|_]) :-
			assert(mejor(Val,FmaxR,FminR)).