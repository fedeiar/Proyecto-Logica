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
	adyacentes(Board1 , [R,C] , Adyacentes), % Pido la lista de adyacentes a la ficha colocada.
	eliminarCapturadosEnAdyacentes(Board1 , Player , Adyacentes , Board2), % Verifico si alguien en la lista de adyacentes esta capturado,
																		   % y elimino a los grupos de capturados.
	not(suicidio(Board2 ,[R,C,Player] , _ )). % Una vez colocada la ficha, verifico que esta no haya quedado encerrada, es decir, que no se haya suicidado.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace(?X, +XIndex, +Y, +Xs, -XsY)


replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cascara para llamar desde el Javascript y devuelve el puntaje de un color.
 
puntajeColor(Board, Color, Puntaje):-
	not(vacio(Board , [0,0])),
	recorrer(Board , [0,0] , Color , []  , Puntaje).

puntajeColor(Board, _, 0):-
	vacio(Board , [0,0]).
	
	

% Chequea si el tablero esta vacio.
vacio( _ , [R,C]):-
	R = 19,
	C = 19.
	
vacio(Board, [R,C]):-
	ficha(Board, R , C , Ficha),
	Ficha = "-",
	siguiente(R,C,R1,C1),
	vacio(Board , [R1, C1]).
	
	
% Recorre el tablero fijandose que "-" estan encerrados por un color dado.
% Termino de recorrer el tabler, caso base.
recorrer( _ , [R,C] , _ , _ , 0):-
	R = 19,
	C = 19.


% Estas visitando una ficha.
recorrer(Board, [R,C], Color, Visitados , Puntaje):-
	R \= 19,
	C \= 19,
	ficha(Board ,R,C, Ficha),
	sonOpuestos(Color,Ficha),
	siguiente(R,C,R1,C1),
	recorrer(Board , [R1, C1] , Color , Visitados , Puntaje).
	
recorrer(Board, [R,C], Color, Visitados , Puntaje):-
	R \= 19,
	C \= 19,
	ficha(Board, R,C, Ficha),
	Color = Ficha,
	siguiente(R,C,R1,C1),
	recorrer(Board , [R1, C1] , Color , Visitados , NPuntaje),
	Puntaje is NPuntaje + 1.



% Estas visitando un "-" que ya lo  habias visitado.
recorrer(Board, [R,C], Color, Visitados , Puntaje):-
	R \= 19,
	C \= 19,
	ficha(Board, R,C, Ficha),
	Ficha = "-",
	member([R,C,Ficha], Visitados),
	siguiente(R,C,R1,C1),
	recorrer(Board, [R1,C1] , Color , Visitados , Puntaje).

% Estas visitando un "-" que no fue visitado, chequear si esta encerrado o no. 
recorrer(Board, [R,C], Color, Visitados , Puntaje):-
	R \= 19,
	C \= 19,
	ficha(Board, R,C, Ficha),
	Ficha = "-",
	not(member([R,C,Ficha], Visitados)),
	adyacentes(Board,[R,C],Adyacentes),
	territorioCapturado(Board , Color , [R,C,Ficha] , Adyacentes ,  Visitados  , [] , NuevosVisitados , SubPuntajeTerritorio),
	append( Visitados  , NuevosVisitados , VisitadosTotales),
	siguiente(R,C,R1,C1),
	recorrer(Board, [R1,C1] , Color , VisitadosTotales , NPuntaje),
	Puntaje is SubPuntajeTerritorio + NPuntaje.
	

recorrer(Board, [R,C], Color, Visitados , Puntaje):-
	R \= 19,
	C \= 19,
	ficha(Board, R,C, Ficha),
	Ficha = "-",
	not(member([R,C,Ficha], Visitados)),
	adyacentes(Board,[R,C],Adyacentes),
	not(territorioCapturado(Board , Color , [R,C,Ficha] , Adyacentes ,  Visitados  , [] , _ , _)),
	siguiente(R,C,R1,C1),
	recorrer(Board, [R1,C1] , Color , [ [R,C,Ficha] | Visitados ] , Puntaje).
	    

% C.B: Si verifique que soy un guion capturado por fichas del mismo color, entonces sumo 1 al puntaje.
territorioCapturado( _ , _ , [R,C,"-"] , [] , _ , _ , [ [R,C,"-"] | [] ] ,  1). 

territorioCapturado(Board , Color , [R,C,"-"] , [ [_,_,FichaA] | Adyacentes ] , VisitadosPrevios , VisitadosActuales , NuevosVisitados  , SubPuntaje):-
	Color = FichaA,
	territorioCapturado(Board , Color , [R,C,"-"] , Adyacentes , 	VisitadosPrevios , VisitadosActuales , NuevosVisitados , SubPuntaje).



territorioCapturado(Board , Color , [R,C,"-"] , [ [Ra,Ca,FichaA] | Adyacentes ] , VisitadosPrevios , VisitadosActuales , NuevosVisitados , SubPuntaje ):-
	FichaA = "-",
	not(member( [Ra,Ca,FichaA] , VisitadosPrevios ) ),
	member( [Ra,Ca,FichaA] , VisitadosActuales ),
	territorioCapturado(Board , Color , [R,C,"-"] , Adyacentes , VisitadosPrevios , VisitadosActuales , NuevosVisitados , SubPuntaje).



territorioCapturado(Board , Color , [R,C,"-"] , [ [Ra,Ca,FichaA] | Adyacentes ] , VisitadosPrevios , VisitadosActuales , NuevosVisitados , SubPuntaje):-
	FichaA = "-",
	not(member( [Ra,Ca,FichaA] , VisitadosPrevios ) ),
	not(member( [Ra,Ca,FichaA] , VisitadosActuales ) ),
	adyacentes(Board,[Ra,Ca],AdyacentesA),
	territorioCapturado(Board , Color , [Ra,Ca,"-"] , AdyacentesA , VisitadosPrevios , [ [R,C,"-"] | VisitadosActuales ] , VisitadosEncerrados , SubPuntaje1),
	append(VisitadosActuales , VisitadosEncerrados , VisitadosActuales1),
    SubPuntaje1 > 0,
	territorioCapturado(Board , Color , [R,C,"-"] , Adyacentes , VisitadosPrevios , VisitadosActuales1 , VisitadosEncerrados1 , SubPuntaje2),
	append(VisitadosEncerrados,VisitadosEncerrados1,NuevosVisitados),
	SubPuntaje2 > 0,
	SubPuntaje is SubPuntaje1 + SubPuntaje2.


% C.B: si la ficha ya era miembro de la lista original de visitados, o es una ficha del color opuesto al pasado por parametro,
% entonces la ficha en cuestión (es decir, [R,C,"-"]) no forma parte del conjunto de fichas capturadas.
territorioCapturado( _ , _ , [R,C,"-"] , [ [Ra,Ca,FichaA] | _ ] , VisitadosPrevios , _ , [ [R,C,"-"] | [] ] , 0):-
	member( [Ra,Ca,FichaA] , VisitadosPrevios ).
	
territorioCapturado( _ , Color , [R,C,"-"] , [ [_,_,FichaA] | _ ] , _ , _ , [ [R,C,"-"] | [] ] , 0):-
	sonOpuestos( Color , FichaA ).



%% El metodo devuelve la proxima fila y columna a visitar.
%% Si no hay mas que recorrer devuelve R=19, C=19.	
siguiente(R, C, R1, C1):-
	R < 19,
	C < 18,
	R1 is R,
	C1 is C+1.

siguiente(R, C, R1, C1):-
	R < 18,
	C = 18,
	R1 is R+1,
	C1 is 0.
	
siguiente(R, C, R1, C1):-
	R = 18,
	C = 18,
	R1 is 19,
	C1 is 19.
	
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Predicados auxiliares que se usan en los predicados principales


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
