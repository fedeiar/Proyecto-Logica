// Reference to object provided by pengines.js library which interfaces with Pengines server (Prolog-engine)
// by making query requests and receiving answers.
var pengine;
// Bidimensional array representing board configuration.
var gridData;
// Bidimensional array with board cell elements (HTML elements).
var cellElems;
// States if it's black player turn.
var turnBlack = false;
var bodyElem;
var latestStone;
var contadorTurno=0;

var puntajeNegro = -1;
var puntajeBlanco = -1;



/**
* Initialization function. Requests to server, through pengines.js library, 
* the creation of a Pengine instance, which will run Prolog code server-side.
*/

function init() {
    document.getElementById("passBtn").addEventListener('click', () => passTurn());
    bodyElem = document.getElementsByTagName('body')[0];
    createBoard();
    // Creación de un conector (interface) para comunicarse con el servidor de Prolog.
    pengine = new Pengine({
        server: "http://localhost:3030/pengine",
        application: "proylcc",
        oncreate: handleCreate,
        onsuccess: handleSuccess,
        onfailure: handleFailure,
        destroy: false
    });
}

/**
 * Create grid cells elements
 */

function createBoard() {
    const dimension = 19;
    const boardCellsElem = document.getElementById("boardCells");
    for (let row = 0; row < dimension - 1; row++) {
        for (let col = 0; col < dimension - 1; col++) {
            var cellElem = document.createElement("div");
            cellElem.className = "boardCell";
            boardCellsElem.appendChild(cellElem);
        }
    }
    const gridCellsElem = document.getElementById("gridCells");
    cellElems = [];
    for (let row = 0; row < dimension; row++) {
        cellElems[row] = [];
        for (let col = 0; col < dimension; col++) {
            var cellElem = document.createElement("div");
            cellElem.className = "gridCell";
            cellElem.addEventListener('click', () => handleClick(row, col));
            gridCellsElem.appendChild(cellElem);
            cellElems[row][col] = cellElem;
        }
    }
}

/**
 * Callback for Pengine server creation
 */

function handleCreate() {
    pengine.ask('emptyBoard(Board)');
	
}

/**
 * Callback for successful response received from Pengines server.
 */

function handleSuccess(response) {
	
	console.log(response.data[0]);
	
	
	if(response.data[0].Board !== undefined){
		gridData = response.data[0].Board;
		console.log(gridData);
		for (let row = 0; row < gridData.length; row++)
			for (let col = 0; col < gridData[row].length; col++) {
				cellElems[row][col].className = "gridCell" +
					(gridData[row][col] === "w" ? " stoneWhite" : gridData[row][col] === "b" ? " stoneBlack" : "") +
					(latestStone && row === latestStone[0] && col === latestStone[1] ? " latest" : "");
			}
		switchTurn();
		
	}
	else{
		
		if(response.data[0].Puntaje !== undefined){
			
			if(puntajeBlanco === -1){
			
				puntajeBlanco = response.data[0].Puntaje;
				const s = "puntajeColor(" + Pengine.stringify(gridData) + ","+ Pengine.stringify("b")+", Puntaje)";
				pengine.ask(s);
				
			}else{
				
				puntajeNegro = response.data[0].Puntaje;
				
				if(puntajeNegro ==puntajeBlanco){
					alert("La partida finalizo y termino empatada, el puntaje de ambos fue: "+ puntajeBlanco);				
				}
				else {
					var ganador = (puntajeBlanco>puntajeNegro ? "Blanco" : "Negro");
					alert("El ganador es "+ ganador + " \nEl puntaje del Blanco es: "+ puntajeBlanco + " Y el puntaje del negro es :" +  puntajeNegro);
				}
			}
				
			
		}
	}
	
	contadorTurno = 0;
}

/**
 * Called when the pengine fails to find a solution.
 */

function handleFailure() {
    alert("Invalid move!");
}
/**
* Called when one player pass his turn.	
* If both player pass, the match ends.
*/

function passTurn(){
	
	if(contadorTurno == 1){

		const s = "puntajeColor(" + Pengine.stringify(gridData) + ","+ Pengine.stringify("w")+", Puntaje)";
		console.log(s);
		pengine.ask(s);
		
		
		contadorTurno=2;
	}
	else {
		contadorTurno = 1;
		switchTurn();
	}
}


/**
 * Handler for color click. Ask query to Pengines server.
 */

function handleClick(row, col) {
    const s = "goMove(" + Pengine.stringify(gridData) + "," + Pengine.stringify(turnBlack ? "b" : "w") + "," + "[" + row + "," + col + "]" + ",Board)";
    pengine.ask(s);
    latestStone = [row, col];
}

function switchTurn() {
    turnBlack = !turnBlack;
    bodyElem.className = turnBlack ? "turnBlack" : "turnWhite";
}

/**
* Call init function after window loaded to ensure all HTML was created before
* accessing and manipulating it.
*/

window.onload = init;