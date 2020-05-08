
primerMovimiento([(F,X,Y)|Fmax],[Fr|Fmax]) :-
			((F = 'G',maybe);F = 'S'),!,
			moverFicha((F,X,Y),Fr).
			
primerMovimiento([(F,X,Y)|Fmax],[Fr|Fmax]) :-
			((F = 'g',maybe);F = 's'),!,
			moverFicha((F,X,Y),Fr).
			
primerMovimiento([(F,X,Y)|Fmax],[(F,X,Y)|FxR]) :- 
			primerMovimiento(Fmax,FxR).
			
moverFicha(('G',X,Y),('G',X2,Y2)):-
			Y2 is Y+1,
			((X2 is X-1,maybe);X2 is X).
		
moverFicha(('S',X,Y),('S',X2,Y2)):-
			Y2 is Y+1,
			((X2 is X-1,maybe);X2 is X).		
			
moverFicha(('g',X,Y),('g',X2,Y2)):-
			Y2 is Y-1,
			((X2 is X+1,maybe);X2 is X).
		
moverFicha(('s',X,Y),('s',X2,Y2)):-
			Y2 is Y-1,
			((X2 is X+1,maybe);X2 is X).		
			