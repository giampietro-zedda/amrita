package analyzer;
import java.util.ArrayList;
import enums.EnumObject;
/**
 *   Classe contenitore di servizio con le informazioni sui chiamanti
 *   che potenzialmente possono generare valori.<br>
 *   <p>
 *   Si tratta del programma chiamante, della sua codifica e dell'istruzione di 
 *   call/Link/Xctl che indirettamente richiama il programma chiamato.
 *   
 *   Si arriva a codificare oggetti di questa classe nel processo backward
 *   che va dal programma origine ai suoi chiamanti e coì via.
 *   
 *   Per ogni chiamante e ogni punto di chiamata call/Link/Xctl possono
 *   essere individuati uno o più possibili campi di cui provare a 
 *   trovare i valori. Queste informazioni vengono memorizzate.
 *   
 *   La lunghezza dei nuovi campi individuati deve essere la stessa del
 *   di quella impostata nell'ultima assegnazione in esame.
 *   La posizione nei nuovi campi è 1 se questi sono campi elementari
 *   individuati dentro un gruppo o la posizione originaria 
 *   impostata nell'ultima assegnazione se il nuovo campo è un unico campo
 *   elementare.
 *   
 *   Esempio
 *   
 *   PROGRAM-ID. PGMCALLER.
 *   ...
 *   WORKING-STORAGE SECTION.
 *   01 PARM1-USING     PIC X(200).
 *   01 PARM1-USING-RED REDEFINES PARM1-USING.
 *      05 CAMPO1       PIC X.
 *      05 CAMPO2       PIC XXX.
 *      ...
 *   PROCEDURE DIVISION.
 *   ...
 *   ...
 *   CALL "PGMCALLED" USING PARM1-USING.
 *   
 *   Supponendo un'ultima assegnazione con PosRcv=2 il processo spreaded
 *   dovrà cercare valori fra inizio programma e CALL per:
 *   
 *   1) CAMPO2 da posizione 1
 *   2) PARM1-USING da posizione 2
 *   
 *   
 */


public class LogicCallerOrigin  {
	
	// Informazioni sul programma chiamante/chiamato
	String idProgramCaller = "";                                    	// Nome programma chiamante
	String idProgramCalled = "";                                    	// Nome programma chiamato

	EnumObject typeProgramCaller = EnumObject.OBJECT_PGM_COBOL;     	// Tipologia programma
	ProgramCobol programCaller = null; 									// Descrittore programma chiamante
	ProgramCobolEntry<? extends Instruction> entryInstr = null;     	// Entry con istruzione origine in programma chiamante (Call/Link/Xctl)
	int numInstr = 0;                                               	// Numero istruzione origine in programma chiamante
	int lvlCaller = 0;                                              	// 1 = livello primo programma chiamante
	
	// Informazioni ottenute dall'ultima trasformazione non risolvibile nel programma chiamato.
	// Per strutture semplici a 1 livello il programma chiamato coincide con il programma origine.
	// Sono informazioni sui nuovi campi di cui cercare i valori fra inizio pgm e numInstr di call/link/xctl, in pgm caller.
	// Le cinque ArrayList viaggiano in coppia
	ArrayList<DataItemCobolIdentifier> al_dataItemCallerToSolve = null;	// Nuovi campi 
	ArrayList<Integer> al_dataItemCallerToSolvePos = null;             	// Posizione di inizio in nuovi campi
	ArrayList<Integer> al_dataItemCallerToSolveLng = null;             	// Lunghezza utile nel campo
	ArrayList<Integer> al_dataItemCallerToSolvePosInSubField = null;    // Posizione mappata in sottocampo origine
	ArrayList<Integer> al_dataItemCallerToSolveLngInSubField = null;    // Lunghezza mappata in sottocampo origine
	
	
	/*
	 * Costruttore
	 */
	public LogicCallerOrigin() {
		super();
		al_dataItemCallerToSolve = new ArrayList<DataItemCobolIdentifier> (); // Nuovi campi 
		al_dataItemCallerToSolvePos = new ArrayList<Integer> ();              // Posizione di inizio in nuovi campi
		al_dataItemCallerToSolveLng = new ArrayList<Integer> ();              // Lunghezza in sottocampo origine
		al_dataItemCallerToSolvePosInSubField = new ArrayList<Integer> ();    // Posizione in sottocampo origine
		al_dataItemCallerToSolveLngInSubField = new ArrayList<Integer> ();    // Lunghezza in sottocampo origine

	}


	@Override
	public String toString() {
		return " Caller:"+idProgramCaller
		   +   " CallerLvl:"+lvlCaller
		   +   " Called:"+idProgramCalled
		   +   " Instr:"+numInstr
		;
	}
 }