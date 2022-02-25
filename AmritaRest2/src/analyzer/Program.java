package analyzer;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumSymbolType;

/**
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * 
 * <h1>
 * Program 
 * </h1>
 *  <p>
 * Questa classe  descrive un generico programma, modellando le caratteristiche comuni a tutti i linguaggi.
 * Viene gestita una map dei simboli utilizzati nel programma, che ingloba la gestione di tutuuti i tipi di
 * cross reference.<br>
 * Inoltre viene mantenuta una lista di opzioni del programma e il reference alla decrizione del programma
 * come grafo orientato, tramite l'oggetto {@link GraphManager}.
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/04/2010
 * @see DataItem
 * @see Analyzer
 *   
 */

public class Program implements Serializable, AmritaConstants {

	private static final long serialVersionUID = 1L;

	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi centralizzati e a oggetti condivisi                    //                                                        //
    ///////////////////////////////////
	/////////////////////////////////////////////////////////////////////
    
	public transient UserConfiguration sd = null;          	// Defaults e references globali come gestore messaggi, log etc	
    public transient MessagesManager mm = null;         	// Gestore messaggi
    public transient LoggerFacade lf = null;            	// Gestore logging

	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  comuni a tutti i tipi di programma                                                                          
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    // Dati di identificazione programma
	public String programName = "";              // Nome del programma così come richiamato nei programmi coincidente con il nome del source
	public EnumObject programType = null;        // OBJECT_PGM_COBOL, OBJECT_PGM_PL1, ....
    public String sysOwner = "";                 // Sistema proprietario dove il copy è stato analizzato
    public String subSysOwner = "";              // SottoSistema proprietario dove il copy è stato analizzato
    public String path = "";  				     // Path completo programma source su file system          
    public String dir = "";  				     // Directory programma source  su file system          
    
	// Struttura per indicizzazione, ricerca  simboli e unresolved nel programma. 
    // Un simbolo rappresenta tipicamente un nome di campo o una label di procedura o una section.
    // Questa map permette di risolvere le problematiche di indicizzazione dei simboli, per 
    // ottenere immediatamente i puntatori alle strutture di definizione dati e istruzioni di programma che li utilizzano.
    // Inoltre fornisce supporto immediato per sapere se un simbolo è unresolved, ovvero dichiarato in
    // qualche definizione o istruzione, ma non definito nel programma.
    // Il simbolo viene mappato su un array di 6 oggetti che contiene:
    // (0) EnumSymbolType     symbolType per Label, section, definizione dati e statement Copy etc.
    // (1) ArrayList<Integer) al_DefDataDivision       con pointers a definizioni ar_InnerDefinitionEntry
    // (2) ArrayList<Integer) al_DefProcDivision       con pointers a istruzioni  ar_InnerInstructionEntry 
    // (3) ArrayList<Integer) al_XrefInputProcedure    con pointers a istruzioni  ar_InnerInstructionEntry
    // (4) ArrayList<Integer) al_XrefInputData         con pointers a definizioni ar_InnerDefinitionEntry
    // (5) ArrayList<Integer) al_XrefOutputProcedure   con pointers a istruzioni  ar_InnerInstructionEntry
    // (6) ArrayList<Integer) al_DefEnv                con pointers a definizioni ar_InnerEnvironmentEntry
    // (7) ArrayList<Integer) al_XrefInputEnvironment  con pointers a istruzioni  ar_InnerEnvironmentEntry
    // (8) ArrayList<EnumSymbolType) al_SymbolType     con tipologie simboli stesso nome
	public HashMap<String, Object[]> map_Symbol = null;			
    
	// Opzioni programma.
    // Vengono inserite le opzioni che caratterizzano il programa come, per esempio,
	// PGM_UPDATE, PGM_UPDATE, PGM_WITH_I_O_SQL, PGM_WITH_I_O_SQL, PGM_CICS_WITH_SEND_MAP...
	// Tali opzioni vengono inserite in momenti diversi dai vari processi di analisi.
	// Le opzioni vengono memorizzate nella tabella ObjectOption (OBJO)
    public ArrayList<EnumObjectOption> al_Option = null;     
       
	/**
	 * 
	 * Costruttore
	 * 
	 */
	public Program(UserConfiguration sd, String programName) {
		
		// Defaults reference ai servizi condivisi
		this.sd = sd;
		this.mm = sd.getMessagesManager();
		this.lf = sd.getLoggerFacade();
		
		// Nome modulo copy
		this.programName = programName;
		
		// Inizializzazione variabili di istanza
		programType = EnumObject.NOT_ASSIGNED;
		
		// Strutture di gestione
		map_Symbol = new HashMap<String, Object[]>();
		al_Option = new ArrayList<EnumObjectOption>();    

	}

	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                     Metodi pubblici                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
 
	
	
	/**
	 * Restituisce la map integrale dei simboli.<br>
	 * <p>
	 * La key è il nome del simbolo.<br>
	 * I dati sono un array di oggetti di 8 elementi.<br>
	 * <p>
	 * Il primo elemento è una {@link EnumSymbolType} con il tipo simbolo.<br>
     * Un simbolo rappresenta tipicamente un nome di campo o una label di procedura o una section.<br>
     * <p>
	 * I successivi elementi sono array list di definizione e di xref.<br>
     * Questa map permette di risolvere le problematiche di indicizzazione dei simboli, per <br>
     * ottenere immediatamente i puntatori alle strutture di definizione dati <br>
     * e istruzioni di programma che li utilizzano.<br>
     * Inoltre fornisce supporto immediato per sapere se un simbolo è unresolved, <br>
     * ovvero dichiarato in qualche definizione o istruzione, ma non definito nel programma. <br>
     * <p>
     * Il simbolo viene mappato su un array di 6 oggetti che contiene:<br>
     * <p>
     * (0) {@link EnumSymbolType} per Label, section, definizione dati e statement Copy etc.<br>
     * 
     * (1) ArrayList <b>al_Data</b>                  con pointers a definizioni di campi in data division <br>
     * (2) ArrayList <b>al_Instruction</b>           con pointers a istruzioni di procedure division <br>
     * (3) ArrayList <b>al_XrefInputProc</b>         con pointers a utilizzi in procedure division<br>
     * (4) ArrayList <b>al_XrefInputData</b>         con pointers a utilizzi in data division (redefines e renames)<br>
     * (5) ArrayList <b>al_XrefOutputProc</b>        con pointers a istruzioni  ar_InnerInstructionEntry<br>
     * (6) ArrayList <b>al_Env</b>                   con pointers a definizioni in environment division<br>
     * (7) ArrayList <b>al_XrefInputEnv</b>          con pointers a utilizzi in environment division <br> 

	 * @return the map_Symbol
	 */
	public HashMap<String, Object[]> getMapSymbols() {
		return map_Symbol;
	}



	/**
	 * 
	 * Restituisce le opzioni di programma.
	 * 
	 * @return the al_Option
	 */
	public ArrayList<EnumObjectOption> getOptionsProgram() {
		return al_Option;
	}


	/**
	 * 
	 * Imposta le opzioni di programma.
	 * 
	 * @param al_Option the al_Option to set
	 */
	public void setOptionsProgram(ArrayList<EnumObjectOption> al_Option) {
		this.al_Option = al_Option;
	}

	
	/**
	 * 
	 * Inserisce un entry di definizione dati del programma.<br> 
	 * <p>
	 * @param InnerProgramEntry dataDefinitionEntry
	 * @return boolean true se inserimento effettuato correttamente
	 * 
	 */
 	public void addOptionProgram (EnumObjectOption programOption){
 		al_Option.add(programOption);
    	return;
    }


	/**
	 * 
	 * Inserisce un entry nella map dei simboli inizializzaando l'oggetto mappato<br>
	 * <p>
	 * L'oggetto mappato è un array di Object di 6 elementi che contiene: 
	 *  <Ul>
	 * <Li> (0) EnumSymbolType     symbolType               tipologia ultimo simbolo caricato<br>
	 * <Li> (1) ArrayList<Integer> al_DefDataDivision       pointers a definizioni dati in data division <br>
	 * <Li> (2) ArrayList<Integer> al_DefProcDivision       pointers a istruzioni di procedure division<br>
	 * <Li> (3) ArrayList<Integer> al_XrefInputProc         pointers Xref in input a simboli in proc division<br>
	 * <Li> (4) ArrayList<Integer> al_XrefInputData         pointers xref in input a campi in data division<br>
	 * <Li> (5) ArrayList<Integer> al_XrefOutputProc        pointers xref in output a istruzioni in proc division<br>
	 * <Li> (6) ArrayList<Integer> al_DefEnv                con pointers a definizioni in environment division<br>
	 * <Li> (7) ArrayList<Integer> al_XrefInputEnv          pointers xref a istruzioni di environment division<br>
	 * <Li> (8) ArrayList<EnumSymbolType> al_SymbolType     Tipologia simboli definiti con stesso nome<br>
	 * </Ul>
	 * Ogni ArrayList è costituita da Integer.<br>
	 * <p>
	 * I valori dell'elemento [0] dell'array di oggetti mappati indica il tipo di simbolo e puo valere,<br>
	 * come specoficato in {@link EnumSymbolType}:<br>
	 * <p>
	 *  <b>SYMBOL_DATA_ITEM </b>				 L'entry è un campo di data division<br>
	 *  <b>SYMBOL_LITERAL </b>	                 L'entry è una literal numerica/alfanumerica<br>
	 *  <b>SYMBOL_FIGURATIVE </b>			     L'entry è una Costante Figurativa<br>
	 *  <b>SYMBOL_LABEL </b>				 	 L'entry è il nome di una Label<br>
	 *  <b>SYMBOL_PROC_INTERNAL </b>			 L'entry è il nome di una Section<br>
	 *  <b>COBOL_SYMBOL_COPY_NAME_DATA </b>		 L'entry è il nome copy di data division<br>
	 *  <b>COBOL_SYMBOL_COPY_NAME_PROC </b>		 L'entry è il nome copy di procedure division<br>
	 * <p>
	 * Viene creato un entry nella mappa dei simboli con chiave nome simbolo e come valore un array di 5 oggetti<br>
	 * Il primo elemento viene valorizzato con la tipologia fornita in input e i rimanenti 4 oggetti dell'array <br>
	 * vengono valorizzati con degli ArrayList di interi, con il significato indicato sopra.
	 * Se il simbolo esisteva già nella Map non viene effettuata nessuna operazione.
	 * 
	 * @param String symbolName
	 * @param String symbolType
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public void symbolCreate(String symbolName, EnumSymbolType symbolType){
 		
 		Object ar_o[];
 		ArrayList<Integer> al_Definition = null;
 		ArrayList<Integer> al_Instruction = null;
 		ArrayList<Integer> al_XrefInputProcedure = null;
 		ArrayList<Integer> al_XrefInputData = null;
 		ArrayList<Integer> al_XrefOutputProcedure = null;
 	    ArrayList<Integer> al_Environment = null;
	    ArrayList<Integer> al_XrefInputEnvironment = null;
	    ArrayList<EnumSymbolType> al_SymbolType = null;

 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo già presente: aggiorno l'elenco di tipi simboli con lo stesso nome
 		if (ar_o != null) {
  			ArrayList<EnumSymbolType> al_symbolType = (ArrayList<EnumSymbolType>) ar_o[8];
  			if (al_symbolType.contains(symbolType)) {
  				al_symbolType.add(symbolType);
			}
 			return;
		}
 	 
 		// Alloco ArrayList per elementi 1,2,3,4,5,6,7,8
		al_Definition = new ArrayList<Integer> ();
		al_Instruction = new ArrayList<Integer> ();
 		al_XrefInputProcedure = new ArrayList<Integer> ();
 		al_XrefInputData = new ArrayList<Integer> ();
 		al_XrefOutputProcedure = new ArrayList<Integer> ();
 		al_Environment = new ArrayList<Integer> ();
		al_XrefInputEnvironment = new ArrayList<Integer> ();
		al_SymbolType = new ArrayList<EnumSymbolType> ();

 		// Alloco Array di 9 oggetti e lo inserisco in map
 		ar_o = new Object[9];
 		map_Symbol.put(symbolName, ar_o);

 		// Caricamento primo tipo simbolo con il nome fornito
 		al_SymbolType.add(symbolType);
 		
 		// Caricamento reference in elementi di Array di oggetti
 		ar_o[0] = symbolType;
		ar_o[1] = al_Definition;
		ar_o[2] = al_Instruction;
		ar_o[3] = al_XrefInputProcedure;
		ar_o[4] = al_XrefInputData;
		ar_o[5] = al_XrefOutputProcedure;
		ar_o[6] = al_Environment;
		ar_o[7] = al_XrefInputEnvironment;
		ar_o[8] = al_SymbolType;
 		
    	return;
    }

	/**
	 * 
	 * Inserisce un pointer alle definizioni di data division associate al simbolo.<br>
	 * <p>
	 * Il simbolo può essere una definizione dati o un modulo copy o il nome di un file etc.. <br>
	 * Un simbolo di data division può essere definito in punti diversi con lo stesso nome, <br>
	 * come per per i campi cobol definiti sotto gruppi diversi.
	 * Il pointer si riferisce quindi alla struttuura con tutte le definizioni di Data Division<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> <br>
	 * Se il simbole non esisteva e viene prima inserito e restituisce <b>false</b> <br>
	 * 
	 * @param String symbolName
	 * @param int definitionPointer alla struttura dove il simbolo è definito
	 * @param EnumSymbolType symbolType
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean symbolDefinitionAddData(String symbolName, int definitionPointer, EnumSymbolType symbolType){
 		
 		boolean bRet = true;
 		
 		Object ar_o[];
 		ArrayList<Integer> al_Definition = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: viene definito contestualmente
 		if (ar_o == null) {
 			bRet = false;
 			symbolCreate(symbolName, symbolType);
 			ar_o = map_Symbol.get(symbolName);
		}
 		
 		// Inserimento toipo simbolo associato al nome
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(symbolType)) {
 			al_SymbolType.add(symbolType);
		}

 		al_Definition = (ArrayList<Integer>) ar_o[1];
 		al_Definition.add(definitionPointer);
 		
    	return bRet;
    }


	/**
	 * 
	 * Inserisce un pointer alle definizioni di Environment division associate al simbolo.<br>
	 * <p>
	 * Il simbolo può essere uno special name e in generale qualsiasi simbolo che nasce in Environment division <br>
	 * Il simbolo può essere poi referenziato in qualsiasi divisione Cobol <br>
	 * Il pointer si riferisce quindi alla struttuura con tutte le istruzioni di Environment Division<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> <br>
	 * Se il simbole non esisteva e viene prima inserito e restituisce <b>false</b> <br>
	 * 
	 * @param String symbolName
	 * @param int definitionPointer alla struttura dove il simbolo è definito
	 * @param EnumSymbolType symbolType
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean symbolDefinitionAddEnv(String symbolName, int definitionPointer, EnumSymbolType symbolType){
 		
 		boolean bRet = true;
 		
 		Object ar_o[];
 		ArrayList<Integer> al_Environment = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: viene definito contestualmente
 		if (ar_o == null) {
 			bRet = false;
 			symbolCreate(symbolName, symbolType);
 			ar_o = map_Symbol.get(symbolName);
		}
 		
 	    // Inserimento tipo simbolo associato al nome
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(symbolType)) {
 			al_SymbolType.add(symbolType);
		}
 		
 		al_Environment = (ArrayList<Integer>) ar_o[6];
 		al_Environment.add(definitionPointer);
 		
    	return bRet;
    }

 	/**
	 * 
	 * Inserisce un pointer alle istruzioni di procedure division associate al simbolo.<br>
	 * Il simbolo può essere una Section, una label oppure un modulo copy con istruzioni di procedure Division. <br>
	 * Un simbolo di procedure division può essere definito in una sola istruzione con lo stesso nome.<br>
	 * Ovvero non è possibile avere due Section o Label con lo stesso nome ma è possibile avere un numero <br>
	 * illimitato di richiami allo stesso modulo copy.<br>
	 * Il pointer si riferisce quindi alla struttura con tutte le istruzioni di porcedure procedure.<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> <br>
	 * Se il simbole non esisteva e viene prima inserito e restituisce <b>false</b> <br>
	 * 
	 * @param String symbolName
	 * @param String entryType
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean symbolDefinitionAddProc(String symbolName, int definitionPointer, EnumSymbolType symbolType){
 		
  		boolean bRet = true;
 		
 		Object ar_o[];
 		ArrayList<Integer> al_Instruction = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: viene definito contestualmente
 		if (ar_o == null) {
 			bRet = false;
 			symbolCreate(symbolName, symbolType);
 			ar_o = map_Symbol.get(symbolName);
		}
 		
 	    // Inserimento tipo simbolo associato al nome
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(symbolType)) {
 			al_SymbolType.add(symbolType);
		}
 		
 		al_Instruction = (ArrayList<Integer>) ar_o[2];
 		al_Instruction.add(definitionPointer);
 		
    	return bRet;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni di definizione, dove il simbolo è definito, in Environment Division<br> 
	 * <p>
	 * Se il simbolo non è definito restituisce null.<br>
	 * Se il simbolo è definito ma è di procedure division restituisce null.
	 * <p>
	 * @param String symbolName
	 * @return int[] con il numero di definizione in data division
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] getXrefSymbolDefEnv(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		@SuppressWarnings("unused")
		EnumSymbolType symbolType = null; 						// Da elemento (0) mappato in Map simboli
 		ArrayList<Integer> al_PointerToEnvironmentEntry = null;	// Di servizio
		int ar_PointerToEnvironmentEntry[] = null;				// Array di output con i numeri di definizione del simbolo in Environment
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
        
 		// Tipologia simbolo
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Pointers alle istruzioni di definizione del simbolo in environment division
 		al_PointerToEnvironmentEntry = (ArrayList<Integer>) ar_o[6];
 		
 		// Verifica se il simbolo esiste come definizione in Environment
		if (al_PointerToEnvironmentEntry.size() == 0) {
			return null;
		}
		 
		// Converto in array semplice di interi
		ar_PointerToEnvironmentEntry = new int[al_PointerToEnvironmentEntry.size()];
		for (int i = 0; i < ar_PointerToEnvironmentEntry.length; i++) {
			ar_PointerToEnvironmentEntry[i] = al_PointerToEnvironmentEntry.get(i);
		}

		return ar_PointerToEnvironmentEntry;
    }

 	
	/**
	 * 
	 * Restituisce i puntatori alle istruzioni di definizione, dove il simbolo è definito, in data division<br> 
	 * <p>
	 * Per esempio vengono restituiti tutti i data item con lo stesso nome definiti in strutture diverse,
	 * oppure l'istruzione dove è definito un file interno Cobol, etc.<br>
	 * Se il simbolo non è definito restituisce null.<br>
	 * Se il simbolo è definito ma è di procedure division restituisce null.
	 * <p>
	 * @param String symbolName
	 * @return int[] con il numero di definizione in data division
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] getXrefSymbolDefData(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		@SuppressWarnings("unused")
		EnumSymbolType symbolType = null; 						// Da elemento (0) mappato in Map simboli
 		ArrayList<Integer> al_PointerToDefinitionEntry = null;	// Di servizio
		int ar_PointerToDefinitionEntry[] = null;				// Array di output con i numeri di definizione del simbolo
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
        
 		// Tipologia simbolo
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Pointers alle istruzioni di definizione del simbolo in data division
 		al_PointerToDefinitionEntry = (ArrayList<Integer>) ar_o[1];
 		
 		// Verifica se il simbolo esiste come definizione 
		if (al_PointerToDefinitionEntry.size() == 0) {
			return new int[0];
		}
		 
		// Converto in array semplice di interi
		ar_PointerToDefinitionEntry = new int[al_PointerToDefinitionEntry.size()];
		for (int i = 0; i < ar_PointerToDefinitionEntry.length; i++) {
			ar_PointerToDefinitionEntry[i] = al_PointerToDefinitionEntry.get(i);
		}

		return ar_PointerToDefinitionEntry;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni, dove il simbolo è definito, in procedure division<br> 
	 * <p>
	 * Per esempio vengono restituiti tutte le istruzioni dove una literal è definita internamente all'istruzione,
	 * oppure l'istruzione dove è definito un file interno Cobol, etc.
	 * Se il simbolo non è definito restituisce null.<br>
	 * Se il simbolo è definito ma non è di procedure division restituisce null.
	 * <p>
	 * @param String symbolName
	 * @return int[] con il numero di definizione in data division
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] getXrefSymbolDefProc(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		@SuppressWarnings("unused")
 		ArrayList<Integer> al_PointerToInstructionEntry = null;	// Di servizio
		int ar_PointerToInstructionEntry[] = null;				// Array di output con i numeri di definizione del simbolo
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return new int[0];
		}
        
  		// Pointers alle istruzioni di definizione del simbolo in procedure division
 		al_PointerToInstructionEntry = (ArrayList<Integer>) ar_o[2];
 		
 		// Verifica se il simbolo esiste come definizione 
		if (al_PointerToInstructionEntry.size() == 0) {
			return null;
		}
		 
		// Converto in array semplice di interi
		ar_PointerToInstructionEntry = new int[al_PointerToInstructionEntry.size()];
		for (int i = 0; i < ar_PointerToInstructionEntry.length; i++) {
			ar_PointerToInstructionEntry[i] = al_PointerToInstructionEntry.get(i);
		}

		return ar_PointerToInstructionEntry;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni, dove il simbolo è referenziato in output, in procedure division<br> 
	 * <p>
	 * Se il simbolo non è definito restituisce lista null.<br>
	 * Se il simbolo è definito ma non è di procedure division restituisce null.
	 * <p>
	 * @param String symbolName
	 * @return int[] con il numero di definizione in proc division
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] getXrefSymbolOutProc(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		@SuppressWarnings("unused")
		EnumSymbolType symbolType = null; 						// Da elemento (0) mappato in Map simboli
 		ArrayList<Integer> al_PointerToInstructionEntry = null;	// Di servizio
		int ar_PointerToInstructionEntry[] = null;				// Array di output con i numeri di definizione del simbolo
  		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
 			return new int[0];
		}
        
 		// Tipologia simbolo
 		symbolType = (EnumSymbolType) ar_o[0];
 		
 		// Pointers alle istruzioni di assegnazione del simbolo in procedure division
 		al_PointerToInstructionEntry = (ArrayList<Integer>) ar_o[5];
 		
 		// Verifica se il simbolo esiste come definizione 
		if (al_PointerToInstructionEntry.size() == 0) {
			return new int[0];
		}
		 
		// Converto in array semplice di interi
		ar_PointerToInstructionEntry = new int[al_PointerToInstructionEntry.size()];
		for (int i = 0; i < ar_PointerToInstructionEntry.length; i++) {
			ar_PointerToInstructionEntry[i] = al_PointerToInstructionEntry.get(i);
		}

		return ar_PointerToInstructionEntry;
    }

	/**
	 * 
	 * Restituisce i puntatori alle istruzioni, dove il simbolo è referenziato in input, in procedure division<br> 
	 * <p>
	 * Se il simbolo non è definito restituisce lista vuota.<br>
	 * Se il simbolo è definito ma non è di procedure division restituisce lista vuota..
	 * <p>
	 * @param String symbolName
	 * @return int[] con il numero di definizione in proc division
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public int[] getXrefSymbolInpProc(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		@SuppressWarnings("unused")
		EnumSymbolType symbolType = null; 						// Da elemento (0) mappato in Map simboli
 		ArrayList<Integer> al_PointerToInstructionEntry = null;	// Di servizio
		int ar_PointerToInstructionEntry[] = null;				// Array di output con i numeri di definizione del simbolo
  	
		al_PointerToInstructionEntry = new ArrayList<Integer> ();
		ar_o = map_Symbol.get(symbolName); 

		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return new int[0];
		}
        
 		// Pointers alle istruzioni di assegnazione del simbolo in procedure division
 		al_PointerToInstructionEntry = (ArrayList<Integer>) ar_o[3];
 		
 		// Verifica se il simbolo esiste come definizione 
		if (al_PointerToInstructionEntry.size() == 0) {
			return new int[0];
		}
		 
		// Converto in array semplice di interi
		ar_PointerToInstructionEntry = new int[al_PointerToInstructionEntry.size()];
		for (int i = 0; i < ar_PointerToInstructionEntry.length; i++) {
			ar_PointerToInstructionEntry[i] = al_PointerToInstructionEntry.get(i);
		}

		return ar_PointerToInstructionEntry;
    }

	/**
	 * 
	 * Restituisce trur se il simbolo è referenziato in output, in procedure division<br> 
	 * <p>
	 * @param String symbolName
	 * @return true se ssimbolo referenziato in output
	 * 
	 */
	public boolean isSymboltXrefOutProc(String symbolName){		
		int[] ar_o = null;									// Array di oggetti da Map simboli
		ar_o = getXrefSymbolOutProc(symbolName);
		if (ar_o.length == 0) {
			return false;
		}
		return true;
    }

	/**
	 * 
	 * Restituisce trur se il simbolo è referenziato in input, in procedure division<br> 
	 * <p>
	 * @param String symbolName
	 * @return true se ssimbolo referenziato in input
	 * 
	 */
	public boolean isSymboltXrefInpProc(String symbolName){		
		int[] ar_o = null;									// Array di oggetti da Map simboli
		ar_o = getXrefSymbolInpProc(symbolName);
		if (ar_o.length == 0) {
			return false;
		}
		return true;
    }
 	/**
	 * 
	 * Restituisce tutti i tipi simboli {@link EnumSymbolType} utilizzati con lo stesso nome<br> 
	 * <p>
	 * Per esempio un campo può avere lo stesso nome di una label o di un cursore Sql.<br>
	 * <p>
	 * @param String symbolName
	 * @return EnumSymbolType con il tipo di simbolo
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public ArrayList<EnumSymbolType> symbolTypes(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
	   		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
        
  	    // Tipi simbolo associato al nome
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];

 		return al_SymbolType;
    }

	/**
	 * 
	 * Restituisce l'ultimo tipo di simbolo definito, come enumerazione {@link EnumSymbolType}<br> 
	 * <p>
	 * @param String symbolName
	 * @return EnumSymbolType con il tipo di simbolo
	 * 
	 */
 	public EnumSymbolType symbolType(String symbolName){
 		
		Object ar_o[] = null;									// Array di oggetti da Map simboli
		EnumSymbolType symbolType = null;
		
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito 
 		if (ar_o == null) {
			return null;
		}
        
 		symbolType = (EnumSymbolType) ar_o[0];
 		
  		return symbolType;
    }


	/**
	 * 
	 * Inserisce un pointer associato al simbolo, di cross reference su istruzioni di procedure<br>
	 * division, che utilizzano il simbolo in input<br>
	 * <p>
	 * Si tratta di tutti i tipi di simboli escluso <b>ENTRY_PGM_TYPE_DEF_COPY_DATA </b> e il pointer<br>
	 * che viene inserito indica che l'istruzione di procedure division referenzia in <b>input</b> il simbolo.<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> altrimenti <b>false</b>
	 * 
	 * @param String symbolName
	 * @param int instructionPointer
	 * 
	 */
 	@SuppressWarnings({ "unchecked" })
	public boolean symbolAddXrefProcInput(String symbolName, int instructionPointer, EnumSymbolType symbolTypeParm){
 		
 		boolean bRet = true;
 		
 		Object ar_o[];
  		ArrayList<Integer> al_XrefInputProcedure = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: creazione implicita
 		if (ar_o == null) {
			bRet = false;
 			symbolCreate(symbolName, symbolTypeParm);
 			ar_o = map_Symbol.get(symbolName);
		}
 		
 		// Inserisco il pointer di Cross reference in procedure utilizzato in input
 		al_XrefInputProcedure = (ArrayList<Integer>) ar_o[3];
 		al_XrefInputProcedure.add(instructionPointer);
  		
    	return bRet;
    }

	/**
	 * 
	 * Inserisce un pointer associato al simbolo, di cross reference su definizioni di data division,<br>
	 * che utilizzano il simbolo in input<br>
	 * <p>
	 * Si tratta tipicamente di symboli che rappresentano data item ridefiniti o rinominati da altri campi campi<br>
	 * Si tratta di simboli del tipo <b>ENTRY_PGM_TYPE_DEF_DATA_ITEM </b> e il pointer<br>
	 * che viene inserito indica che l'istruzione di data division referenzia in <b>input</b> il simbolo.<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> altrimenti <b>false</b>
	 * 
	 * @param String symbolName
	 * @param int definitionPointer
	 * 
	 */
 	@SuppressWarnings({ "unchecked" })
	public boolean symbolAddXrefDataInput(String symbolName, int definitionPointer, EnumSymbolType symbolTypeParm){
 		
 		boolean bRet = true;
 		
 		Object ar_o[];
  		ArrayList<Integer> al_XrefInputData = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: creazione d'ufficio
 		if (ar_o == null) {
			symbolCreate(symbolName, symbolTypeParm);
 			ar_o = map_Symbol.get(symbolName);
 			bRet = false;
		}
 		
 		
 		// Inserisco il pointer di Cross reference in data division
		al_XrefInputData = (ArrayList<Integer>) ar_o[4];
		al_XrefInputData.add(definitionPointer);
  		
    	return bRet;
    }

	/**
	 * 
	 * Inserisce un pointer associato al simbolo, di cross reference su definizioni di environment division,<br>
	 * che utilizzano il simbolo in input<br>
	 * <p>
	 * Si tratta tipicamente di symboli che rappresentano data item, literal o special names<br>
	 * Il pointer che viene inserito indica che l'istruzione di data division referenzia in <b>input</b> 
	 * il simbolo.<br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> altrimenti <b>false</b>
	 * 
	 * @param String symbolName
	 * @param int definitionPointer
	 * 
	 */
 	@SuppressWarnings({ "unchecked" })
	public boolean symbolAddXrefEnvInput(String symbolName, int definitionPointer, EnumSymbolType symbolTypeParm){
 		
 		boolean bRet = true;
 		
 		Object ar_o[];
  		ArrayList<Integer> al_XrefInputEnvironment = null;
  		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: creazione d'ufficio
 		if (ar_o == null) {
			symbolCreate(symbolName, symbolTypeParm);
 			ar_o = map_Symbol.get(symbolName);
 			bRet = false;
		}
 		
 		// Inserisco il pointer di Cross reference in environment division
 		al_XrefInputEnvironment = (ArrayList<Integer>) ar_o[7];
 		al_XrefInputEnvironment.add(definitionPointer);
  		
    	return bRet;
    }

	/**
	 * 
	 * Inserisce un pointer associato al simbolo, di cross reference su istruzioni di procedure division,<br>
	 * che utilizzano il simbolo in output<br>
	 * <p>
	 * Si tratta di symboli che rappresentano data item, del tipo <b>ENTRY_PGM_TYPE_DEF_DATA_ITEM </b><br>
	 * <p>
	 * Se il simbole esisteva e viene aggiornato restituisce <b>true</b> altrimenti <b>false</b>
	 * 
	 * @param String symbolName
	 * @param int instructionPointer
	 * 
	 */

	@SuppressWarnings({ "unchecked" })
	public boolean symbolAddXrefProcOutput(String symbolName, int instructionPointer, EnumSymbolType symbolTypeParm){
 
		boolean bRet = true;
 		
 		Object ar_o[];
   		ArrayList<Integer> al_XrefOutputProcedure = null;
 		
 		ar_o = map_Symbol.get(symbolName);

 		// Simbolo non definito: no operation
 		if (ar_o == null) {
 			symbolCreate(symbolName, symbolTypeParm);
 			ar_o = map_Symbol.get(symbolName);
			bRet = false;
		}
 		
 		// In procedure division possono essere referenziati in output solo i data item
 		ArrayList<EnumSymbolType> al_SymbolType = (ArrayList<EnumSymbolType>) ar_o[8];
 		if (!al_SymbolType.contains(EnumSymbolType.COBOL_SYMBOL_DATA_ITEM)) {
 			return false;
		}

 		// Inserisco il pointer di Cross reference in procedure division
		al_XrefOutputProcedure = (ArrayList<Integer>) ar_o[5];
		al_XrefOutputProcedure.add(instructionPointer);
  		
    	return bRet;
    }

	/**
	 * 
	 * Restiuisce true se il simbolo è definito nel programma<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste
	 * 
	 */
 	public boolean isSymbolDefined (String symbol){
 		if (map_Symbol.get(symbol) != null) {
			return true;
		} 
    	return false;
    }

	/**
	 * 
	 * Restiuisce true se il simbolo è una definizione dati<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste ed è una definizione dati
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isSymbolDataItem (String symbol){
 		Object ar_o[] = null;
 		
 		ar_o = map_Symbol.get(symbol);  
 		
		// Verifica se il simbolo è definito
 		if (ar_o == null) {
			return false;
		}
 		
 		// Verifica se il simbolo è un data item
		ArrayList<Integer> al_integer = (ArrayList<Integer>) ar_o[1];
  		if (al_integer.size() > 0) {
  			ArrayList<EnumSymbolType> al_symbolType = (ArrayList<EnumSymbolType>) ar_o[8];
  			if (al_symbolType.contains(EnumSymbolType.COBOL_SYMBOL_DATA_ITEM)) {
  				return true;
			}
		}
 		
    	return false;
    }

	/**
	 * 
	 * Restiuisce true se il simbolo è una label di programma<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste ed è una label
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isSymbolLabel (String symbol){
 		
 		Object ar_o[] = null;
  		
 		ar_o = map_Symbol.get(symbol); 
  		
		// Verifica se il simbolo è definito
 		if (ar_o == null) {
			return false;
		}
 		
		// Verifica se il simbolo è una Label
		ArrayList<Integer> al_integer = (ArrayList<Integer>) ar_o[2];
  		if (al_integer.size() > 0) {
  			ArrayList<EnumSymbolType> al_symbolType = (ArrayList<EnumSymbolType>) ar_o[8];
  			if (al_symbolType.contains(EnumSymbolType.COBOL_SYMBOL_LABEL)) {
  				return true;
			}
		}
 		
    	return false;
    }

 	/**
	 * 
	 * Restiuisce true se il simbolo è una procedura interna  di programma<br> 
	 * <p>
	 * @param String symbol name
	 * @return boolean true se il simbolo esiste ed è una Section
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isSymbolProcedureInternal (String symbol){
 		
 		Object ar_o[] = null;
  		
 		ar_o = map_Symbol.get(symbol); 
 		
		// Verifica se il simbolo è definito
 		if (ar_o == null) {
			return false;
		}
 		
 		// Verifica se il simbolo è una Label
		ArrayList<Integer> al_integer = (ArrayList<Integer>) ar_o[2];
  		if (al_integer.size() > 0) {
  			ArrayList<Integer> al_symbol = (ArrayList<Integer>) ar_o[8];
  			if (al_symbol.contains(EnumSymbolType.COBOL_SYMBOL_PROC_INTERNAL)) {
  				return true;
			}
		}
		
     	return false;
    }

	/**
	 * 
	 * Restiuisce true se il simbolo non è referenziato nel programma, è solo definito<br> 
	 * <p>
	 * @param String symbolName
	 * @return boolean true se il simbolo non è referenziato
	 * 
	 */
 	@SuppressWarnings("unchecked")
	public boolean isSymbolUnXref (String symbolName){
		
 		Object ar_o[] = null;
 		ArrayList<Integer> al_Xref = null;
 	
 		ar_o = map_Symbol.get(symbolName); 
 		
		// Verifica se il simbolo è definito
 		if (ar_o == null) {
			return true;
		}
 
 		// Xref procedure in input
 		al_Xref = (ArrayList<Integer>) ar_o[3];
 		if (al_Xref != null) {
			if (al_Xref.size() > 0) {
				return false;
			}
		}
 		
 		// Xref data in input
 		al_Xref = (ArrayList<Integer>) ar_o[4];
 		if (al_Xref != null) {
			if (al_Xref.size() > 0) {
				return false;
			}
		}
 		
		// Xref procedure in output
 		al_Xref = (ArrayList<Integer>) ar_o[5];
 		if (al_Xref != null) {
			if (al_Xref.size() > 0) {
				return false;
			}
		}

    	return true;
    }



	/**
	 * @return the programName
	 */
	public String getProgramName() {
		return programName;
	}



	/**
	 * @param programName the programName to set
	 */
	public void setProgramName(String programName) {
		this.programName = programName;
	}



	/**
	 * @return the programType
	 */
	public EnumObject getProgramType() {
		return programType;
	}



	/**
	 * @param programType the programType to set
	 */
	public void setProgramType(EnumObject programType) {
		this.programType = programType;
	}



	/**
	 * @return the sysOwner
	 */
	public String getSysOwner() {
		return sysOwner;
	}



	/**
	 * @param sysOwner the sysOwner to set
	 */
	public void setSysOwner(String sysOwner) {
		this.sysOwner = sysOwner;
	}



	/**
	 * @return the subSysOwner
	 */
	public String getSubSysOwner() {
		return subSysOwner;
	}



	/**
	 * @param subSysOwner the subSysOwner to set
	 */
	public void setSubSysOwner(String subSysOwner) {
		this.subSysOwner = subSysOwner;
	}


	/**
	 * @return the path  of serialized program
	 */
	public String getPath() {
		return path;
	}


	/**
	 * @param The path of serialized program to set
	 */
	public void setPath(String path) {
		this.path = path;
	}



	/**
	 * @return  the  directory of serialized program
	 */
	public String getDir() {
		return dir;
	}



	/**
	 * @param The directory of serialized program to set
	 */
	public void setDir(String dir) {
		this.dir = dir;
	}


	
}
