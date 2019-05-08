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

goMove(Board, Player, [R,C], Board2):-
    replace(Row, R, NRow, Board, Board1),
    replace("-", C, Player, Row, NRow),
	adyacentes(Board1 , [R,C] , Adyacentes), %pido la lista de adyacentes a la ficha colocada.
	eliminarCapturadosEnAdyacentes(Board1 , Adyacentes , Board2), %verifico si alguien en la lista de adyacentes esta capturado,
																  %y elimino a los grupos de capturados.
	not(encerrado(Board2 ,[R,C,Player] , _ )). %una vez colocada la ficha, verifico que esta no haya quedado encerrada, es decir, que no se haya suicidado.
	
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

eliminarCapturadosEnAdyacentes(Board , [] , Board).

eliminarCapturadosEnAdyacentes(Board , [ [R,C,Color] | Adyacentes ] , Board2):-
	encerrado(Board , [R,C,Color] , Capturados),
	eliminarCapturados(Board , Capturados , Board1),
	eliminarCapturadosEnAdyacentes(Board1 , Adyacentes , Board2).
	
eliminarCapturadosEnAdyacentes(Board , [ [R,C,Color] | Adyacentes ] , Board1):-
	not(encerrado(Board , [R,C,Color] , _ )),
	eliminarCapturadosEnAdyacentes(Board , Adyacentes , Board1).	



eliminarCapturados(Board, [] ,Board).

eliminarCapturados(Board,[ [R,C,Color] | Capturados ],Board2):-
	replace(Row, R, NRow, Board, Board1),
    replace(Color, C, "-", Row, NRow),
	eliminarCapturados(Board1 , Capturados , Board2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% encerrado funciona como una cascara para obtener la primer lista de adyacentes en locked.
encerrado(Board,  [R,C,Color] , Aeliminar):-
	adyacentes(Board , [R,C] , Adyacentes),
    locked(Board , Adyacentes , [R,C,Color] , [] , Aeliminar).


locked( _ , [] , [R,C,Color] , _ , [ [R,C,Color] | [] ] ).


locked(Board , [ [_,_,ColorA] | Adyacentes ] , [R,C,Color] , Visitados , Aeliminar):-
	sonOpuestos(ColorA,Color),
	locked(Board, Adyacentes , [R,C,Color] , Visitados , Aeliminar).


locked(Board , [ [Ra,Ca,ColorA] | Adyacentes] , [R,C,Color] , Visitados , Aeliminar):-
	Color = ColorA,
	not(member([Ra,Ca,ColorA] , Visitados)),
	adyacentes(Board , [Ra,Ca] , AdyacentesA),
	locked(Board , AdyacentesA , [Ra,Ca,ColorA] , [ [R,C,Color] | Visitados ] , Aeliminar1), 
    append(Visitados,Aeliminar1,VisitadosYeliminados),
	locked(Board , 	Adyacentes , [R,C,Color] , VisitadosYeliminados , Aeliminar2),
	append(Aeliminar1,Aeliminar2,Aeliminar).

locked(Board , [ [Ra,Ca,ColorA] | Adyacentes] , [R,C,Color] , Visitados , Aeliminar):-
	Color = ColorA,
	member([Ra,Ca,ColorA] , Visitados), %si ya pertenecia a la lista de visitados, lo ignoro y sigo preguntando por el resto de los adyacentes.
	locked(Board , 	Adyacentes , [R,C,Color] , Visitados , Aeliminar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adyacentes(Board,[R,C],Lvalida):-
	C1 is C-1,
	C2 is C+1,
	R1 is R+1,
	R2 is R-1,
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