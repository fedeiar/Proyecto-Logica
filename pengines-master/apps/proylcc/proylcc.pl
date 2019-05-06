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
    replace("-", C, Player, Row, NRow).

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




encerrado(Board, [R,C,Color] , Capturados , [ [R,C,Color] | Capturados ]):-
	adyacentes(Board , [R,C] , Adyacentes),
	forall( ( (member[R1,C1,Color1] , Adyacentes) , not(member( [R1,C1,Color1] , Capturados ) ,
			   sonOpuestos(Color1,Color) ).
	%eliminar la ficha
	
	
encerrado(Board,  [R,C,Color] , Capturados , [ [R,C,Color] | Aeliminar ]):-
	adyacentes(Board , [R,C] , Adyacentes),
	forall(  ( (member[R1,C1,Color1] , Adyacentes) , (not(member([R1,C1,Color],Capturados))) , not(sonOpuestos(Color1,Color))  ,
		  ( notGuion(Color1) , encerrado(Board,[R1,C1,Color1] , [ [R,C,Color] | Capturados ] , Aeliminar ) ), 
	
	%eliminar la ficha









capturar(Board,Player,[R,C],RBoard):-
	adyacentes(Board,[R,C],LAdyacentes),
	nth0(0,LAdyacentes,[R1,C1,Color1]),
	esCapturada(Board,Player,[R1,C1,Color1],RBoard1,[],_).
	
esCapturada(Board,Player,[R,C,Color],RBoard,Capturados,[ [R,C,Color]|CapturadosN ]):-
	sonOpuestos(Player,Color), %Verifica que se este capturando una ficha del color opuesto al que la colocó.
	not(member([R,C,Color],Capturados)),
	adyacentes(Board,[R,C],LAdyacentes),
	
	esCapturada(Board,Player,


%Devuelve en LAdyacentes una lista de las 4 fichas adyacentes a la ficha ubicada en [R,C] 
%Donde el tercer parametro F... de cada sublista representa el color de la ficha.
adyacentes(Board,[R,C],LValida):-
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
	
adyacentesValidos([ [R,C,Color] | Lad ] , [ [R,C,Color] | LNueva ]):-
	Color \= ".",
	adyacentesValidos(Lad,LNueva).

adyacentesValidos([ [R,C,Color] | Lad ] , LNueva):-
	Color = ".",
	adyacentesValidos(Lad,LNueva).

% Obtiene el elemento del tablero que esta en la posicion [R,C] de la matriz.	
% Si la posicion solicitada esta fuera del limite, la ficha sera un "." indicando que se salio del tablero.
ficha(Board,R,C,Ficha):-
	dentroDelTablero(R,C),
	nth0(R1,Board,Fila),
	nth0(C1,Fila,Ficha).
	
ficha(Board,R,C,Ficha):-
	not(dentroDelTablero(R,C)),
	Ficha = ".".

dentroDelTablero(R,C):-
	R>0,
	C>0,
	R<20,
	C<20.
	
sonOpuestos(Color1,Color2):-
	Color1 = w,
	Color2 = b.
	
sonOpuestos(Color1,Color2):-
	Color1 = b,
	Color2 = w.


notGuion(Color):-
	Color \= "-".