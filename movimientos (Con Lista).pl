
% Casillas posibles del Peon Superior (el que va hacia abajo)
movimiento('P',(X,Y),(X,Y2),_) :- Y2 is Y+1.

% Casillas posibles del Peon Inferior (el que va hacia arriba)
movimiento('p',(X,Y),(X,Y2),_) :- Y2 is Y-1.

% Casillas posibles del Rey 
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X-1 , Y2 is Y+1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X , Y2 is Y+1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'K'; F = 'k') , X2 is X+1 , Y2 is Y+1.

% FIXME hay que tener en cuenta que si hay una ficha en su trayectoria no puede continuar...
% Casillas posibles del Alfil
movimiento(F,(X,Y),(X2,Y2),L) :- (F = 'B'; F = 'b') , restarUnoOMas(X,X2,N) , restarUnoOMas(Y,Y2,N).
movimiento(F,(X,Y),(X2,Y2),L) :- (F = 'B'; F = 'b') , sumarUnoOMas(X,X2,N) , restarUnoOMas(Y,Y2,N).
movimiento(F,(X,Y),(X2,Y2),L) :- (F = 'B'; F = 'b') , restarUnoOMas(X,X2,N) , sumarUnoOMas(Y,Y2,N).
movimiento(F,(X,Y),(X2,Y2),L) :- (F = 'B'; F = 'b') , sumarUnoOMas(X,X2,N) , sumarUnoOMas(Y,Y2,N).

% Casillas posibles de la Lanza Superior
movimiento('L',(X,Y),(X,Y2),L) :- sumarUnoOMas(Y,Y2,N).

% Casillas posibles de la Lanza Inferior
movimiento('l',(X,Y),(X,Y2),L) :- restarUnoOMas(Y,Y2,N).

% Casillas posibles de la Torre
movimiento(F,(X,Y),(X2,Y),L) :- (F = 'R'; F = 'r') , restarUnoOMas(X,X2,N).
movimiento(F,(X,Y),(X2,Y),L) :- (F = 'R'; F = 'r') , sumarUnoOMas(X,X2,N).
movimiento(F,(X,Y),(X,Y2),L) :- (F = 'R'; F = 'r') , restarUnoOMas(Y,Y2,N).
movimiento(F,(X,Y),(X,Y2),L) :- (F = 'R'; F = 'r') , sumarUnoOMas(Y,Y2,N).

% Casillas posibles del Oro
movimiento('G',(X,Y),(X2,Y2),_) :- X2 is X-1 , Y2 is Y-1.
movimiento('G',(X,Y),(X2,Y2),_) :- X2 is X+1 , Y2 is Y-1.
movimiento('g',(X,Y),(X2,Y2),_) :- X2 is X-1 , Y2 is Y+1.
movimiento('g',(X,Y),(X2,Y2),_) :- X2 is X+1 , Y2 is Y+1.
% Comunes...
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X-1 , Y2 is Y.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X+1 , Y2 is Y.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X-1 , Y2 is Y+1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X , Y2 is Y+1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'G'; F = 'g') , X2 is X+1 , Y2 is Y+1.

% Casillas posibles del Plata
movimiento('S',(X,Y),(X,Y2),_) :- Y2 is Y+1.
movimiento('s',(X,Y),(X,Y2),_) :- Y2 is Y-1.
% Comunes...
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'S'; F = 's') , X2 is X-1 , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'S'; F = 's') , X2 is X+1 , Y2 is Y-1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'S'; F = 's') , X2 is X-1 , Y2 is Y+1.
movimiento(F,(X,Y),(X2,Y2),_) :- (F = 'S'; F = 's') , X2 is X+1 , Y2 is Y+1.

% Casillas posibles del Caballo Superior
movimiento('N',(X,Y),(X,Y2),_) :- X2 is X-1 , Y2 is Y+2.
movimiento('N',(X,Y),(X,Y2),_) :- X2 is X+1 , Y2 is Y+2.

% Casillas posibles del Caballo Inferior
movimiento('n',(X,Y),(X,Y2),_) :- X2 is X-1 , Y2 is Y-2.
movimiento('n',(X,Y),(X,Y2),_) :- X2 is X+1 , Y2 is Y-2.


% ---------------------------------- AUXILIARES ----------------------------------
restarUnoOMas(A,B,1):- B is A-1.
restarUnoOMas(A,B,N):- B is B2-1, restarUnoOMas(A,B,N2), N2 is N+1.

sumarUnoOMas(A,B,1):- B is A+1.
sumarUnoOMas(A,B,N):- B is B2+1, sumarUnoOMas(A,B,N2), N2 is N+1.