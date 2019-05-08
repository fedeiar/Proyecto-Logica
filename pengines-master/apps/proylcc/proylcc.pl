:- module(proylcc,
	[  
		emptyBoard/1,
		goMove/4
	]).

emptyBoard([
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% goMove(+Board, +Player, +Pos, -RBoard)
% R = Row
% C = Column
% RBoard es la configuración resultante de reflejar la movida del jugador Player
% en la posición Pos a partir de la configuración Board.

goMove(Board, Player, [R,C], RBoard):-
	
    replace(Row, R, NRow, Board, RBoard),
    replace("-", C, Player, Row, NRow),
	encerrado(Board , [R,C,Player] , [] , Rta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% encerrado funciona como una cascara para obtener la primer lista de adyacentes en locked.
encerrado(Board,  [R,C,Color] , Aeliminar):-
	adyacentes(Board , [R,C] , Adyacentes),
    locked(Board , Adyacentes , [R,C,Color] , [] , Aeliminar).


locked( _ , [ [_,_,ColorA] | [] ] , [R,C,Color] , Visitados , [ [R,C,Color] | Visitados ] ):-
	not(member([R,C,Color] , Visitados)),
	sonOpuestos(ColorA,Color).

locked( _ , [ [_,_,ColorA] | [] ] , [R,C,Color] , Visitados , Visitados ):-
	member([R,C,Color] , Visitados),
	sonOpuestos(ColorA,Color).

locked(Board , [ [_,_,ColorA] | Adyacentes ] , [R,C,Color] , Visitados , Aeliminar):-
	Adyacentes \= [],
	sonOpuestos(ColorA,Color),
	locked(Board, Adyacentes , [R,C,Color] , Visitados , Aeliminar).
	
 
locked(Board , [ [Ra,Ca,ColorA] | [] ] , [R,C,Color] , Visitados , Aeliminar ):-
	Color = ColorA,
	not(member([Ra,Ca,ColorA] , Visitados)),
	adyacentes(Board , [Ra,Ca] , AdyacentesA),
	locked(Board , AdyacentesA , [Ra,Ca,ColorA] , [ [R,C,Color] | Visitados ] , Aeliminar). %si era el ultimo adyacente que me quedaba ver de la lista,
																							%y encontre que la ult. ficha es de mi mismo color, entonces
																							%me agrego a visitados y pregunto por mi ficha adyacente.
																							
locked( _ , [ [Ra,Ca,ColorA] | [] ] , [R,C,Color] , Visitados , [ [R,C,Color] | Visitados ] ):-
	Color = ColorA,
	member([Ra,Ca,ColorA] , Visitados). %si la ult ficha que vi ya pertenecia a la lista de visitados, simplemente lo ignoro y me
										%agrego a la lista de visitados (si se ejecuta este predicado probablemente sea el ultimo 
										% en llamarse).


locked(Board , [ [Ra,Ca,ColorA] | Adyacentes] , [R,C,Color] , Visitados , Aeliminar):-
	Adyacentes \= [],
	Color = ColorA,
	not(member([Ra,Ca,ColorA] , Visitados)),
	adyacentes(Board , [Ra,Ca] , AdyacentesA),
	locked(Board , AdyacentesA , [Ra,Ca,ColorA] , [ [R,C,Color] | Visitados ] , Aeliminar1), 
	locked(Board , 	Adyacentes , [R,C,Color] , Aeliminar1 , Aeliminar).    

locked(Board , [ [Ra,Ca,ColorA] | Adyacentes] , [R,C,Color] , Visitados , Aeliminar):-
	Adyacentes \= [],
	Color = ColorA,
	member([Ra,Ca,ColorA] , Visitados), %si ya pertenecia a la lista de visitados, lo ignoro y sigo preguntando por el resto de los adyacentes.
	locked(Board , 	Adyacentes , [R,C,Color] , Visitados , Aeliminar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adyacentes(Board,[R,C],Lvalida):-	
	C1 is C-1,
	C2 is C+1,
	R1 is R-1,
	R2 is R+1,
	ficha(Board,R2,C,Fup),
	ficha(Board,R1,C,Fdown),
	ficha(Board,R,C1,Fleft),
	ficha(Board,R,C2,Fright),
	LAdyacentes = [ [R2,C,Fup] , [R1,C,Fdown] , [R,C1,Fleft] , [R,C2,Fright] ],
	adyacentesValidos(LAdyacentes,Lvalida).
	
adyacentesValidos([],[]).
	
adyacentesValidos([ [_,_,Color] | Lad ] , [ [_,_,Color] | LNueva ]):-
	Color \= ".",
	adyacentesValidos(Lad,LNueva).

adyacentesValidos([ [_,_,Color] | Lad ] , LNueva):-
	Color = ".",
	adyacentesValidos(Lad,LNueva).

% Obtiene el elemento del tablero que esta en la posicion [R,C] de la matriz.	
% Si la posicion solicitada esta fuera del limite, la ficha sera un "." indicando que se salio del tablero.
ficha(Board,R,C,Ficha):-
	dentroDelTablero(R,C),
	nth0(R,Board,Fila),
	nth0(C,Fila,Ficha).
	
ficha(_,R,C,Ficha):-
	not(dentroDelTablero(R,C)),
	Ficha = ".".

dentroDelTablero(R,C):-
	R>=0,
	C>=0,
	R<19,
	C<19 .
	
sonOpuestos(Color1,Color2):-
	Color1 = "w",
	Color2 = "b".
	
sonOpuestos(Color1,Color2):-
	Color1 = "b",
	Color2 = "w".


notGuion(Color):-
	Color \= "-".