:- module(proylcc,
	[  
		emptyBoard/1,
		goMove/4
	]).

emptyBoard([
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","b","b","b","b","b","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","b","w","w","w","w","w","b"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","b","w","-","-","-","w","b"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","b","w","w","w","w","w","b"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","b","b","b","b","b","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","b","b","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","w","b","-","-","-","-","-","-","b","w","-","-"],
		 ["-","-","-","-","-","-","b","b","-","-","-","-","-","-","b","-","-","w","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","b","w","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","b1"]
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
	eliminarCapturadosEnAdyacentes(Board1 , Player , Adyacentes , Board2), %verifico si alguien en la lista de adyacentes esta capturado,
																  %y elimino a los grupos de capturados.
	not(suicidio(Board2 ,[R,C,Player] , _ )). %una vez colocada la ficha, verifico que esta no haya quedado encerrada, es decir, que no se haya suicidado.


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


	
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eliminarCapturadosEnAdyacentes(Board , _ , [] , Board).

eliminarCapturadosEnAdyacentes(Board , Color , [ [R,C,ColorA] | Adyacentes ] , Board2):-
	sonOpuestos(Color,ColorA),
	suicidio(Board , [R,C,ColorA] , Capturados),
	eliminarCapturados(Board , Capturados , Board1),
	eliminarCapturadosEnAdyacentes(Board1 , Color , Adyacentes , Board2).
	
eliminarCapturadosEnAdyacentes(Board , Color , [ [_,_,ColorA] | Adyacentes ] , Board1):-
	Color = ColorA,
	eliminarCapturadosEnAdyacentes(Board , Color , Adyacentes , Board1).

eliminarCapturadosEnAdyacentes(Board , Color , [ [R,C,ColorA] | Adyacentes ] , Board1):-
	not(suicidio(Board , [R,C,ColorA] , _ )),
	eliminarCapturadosEnAdyacentes(Board , Color , Adyacentes , Board1).	



eliminarCapturados(Board, [] ,Board).

eliminarCapturados(Board,[ [R,C,Color] | Capturados ],Board2):-
	replace(Row, R, NRow, Board, Board1),
    replace(Color, C, "-", Row, NRow),
	eliminarCapturados(Board1 , Capturados , Board2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% suicidio funciona como una cascara para obtener la primer lista de adyacentes en locked.
suicidio(Board,  [R,C,Color] , Aeliminar):-
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


%% Cascara para llamar desde el Javascript y devuelve el puntaje de un color.
 
puntajeColor(Board, Color, Puntaje):-
	recorrer(Board, [0,0], Color, Visitados, SinContar, Puntaje).
	
%% Recorre el tablero fijandose que "-" estan encerrados por un color dado.
% Termino de recorrer el tabler, caso base.
recorrer(Board, [R,C], Color, [], [], 0):-
	R ==20,
	C ==20.


% Estas visitando una ficha.
recorrer(Board, [R,C], Color, [ [R,C,Ficha] | Visitados], SinContar, Puntaje):-
	R \= 20,
	C \= 20,
	ficha(Board, R, C, Ficha),
	Ficha \= "-",
	not(member([R,C,Ficha], Visitados)),
	siguiente(R, C, R1, C1),
	recorrer(Board, [R1, C1], Color, Visitados, SinContar, Puntaje).
	
% Estas visitando un "-" que ya lo  habias visitado.
recorrer(Board, [R,C], Color, [ [R,C,Ficha] | Visitados], SinContar, Puntaje):-
	R \= 20,
	C \= 20,
	ficha(Board, R, C, Ficha),
	Ficha == "-",
	member([R,C,Ficha], Visitados),
	siguiente(R, C, RNueva, CNueva),
	recorrer(Board, [RNueva, CNueva], Color, Visitados, SinContar, Puntaje).

% Estas visitando un "-" que no fue visitado, chequear si eta encerrado o no.
recorrer(Board, [R,C], Color, [ [R,C,Ficha] | Visitados], SinContar, Puntaje):-
	R \= 20,
	C \= 20,
	ficha(Board, R, C, Ficha),
	Ficha == "-",
	not(member([R,C,Ficha], Visitados)),
	
	
	siguiente(R, C, RNueva, CNueva),

		
	

%% El metodo devuelve la proxima fila y columna a visitar.
%% Si no hay mas que recorrer devuelve R=20, C=20.	
siguiente(R, C, R1, C1):-
	R < 18,
	C < 18,
	R1 is R+1,
	C1 is C.

siguiente(R, C, R1, C1):-
	R == 18,
	C < 18,
	R1 is 0,
	C1 is C+1.
	
siguiente(R, C, R1, C1):-
	R == 18,
	C == 18,
	R1 is 20,
	C1 is 20.


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