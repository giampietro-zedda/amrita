package analyzer;
/**
	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
	 * 
	 * <h1>
	 * AmritaConstants
	 * </h1>
	 * <p>
	 * 
	 * Vengono definite tute le costanti di utilità utilizzate da Amrita che non sono gestite da
	 * enumerazioni specifiche.<br>
	 * Per essere disponibili nelle classi di gestione queste devono implementare questa interfaccia.
	 *
	 * @author Giampietro Zedda
	 * @version 1.0.0
	 * @since 07/apr/2010 
	 *
*/
public interface AmritaConstants {
    
	// Modalità lancio metodi di funzioni/processi applicativi
	public final int LAUNCH_METHOD_STATIC = 0;		        // Metodi applicativi richiamati staticamente
	public final int LAUNCH_METHOD_INVOKE = 1;		        // Metodi applicativi richiamati con reflection Invoke
	public final int LAUNCH_METHOD_BY_THREAD = 2;		    // Metodi applicativi richiamati a fronte di attivazione thread
	
	// Commenti nei sorgenti di configurazione, messaggi e piloti
	public final String TAG_COMMENT = "#";					// Commenti in sources direttive, conf,..
	
	// Tipologie di parametri nei files pilota
	public final String PARM_NUMERIC = "N";					// In estrazione parametri
	public final String PARM_TEXT = "T";				    // In estrazione parametri
    
	// Valori di default sistema/sottosistema (* = tutti)
	public final String DEFAULT_SYS = "*";				    // Default sistema
	public final String DEFAULT_SUB_SYS = "*";				// Default sottosistema
	
	// Costanti per Cobol
	public final int COBOL_OCCURS_ASCENDING_KEY = 0;	    // Valore ascending in clausola Cobol Occurs
	public final int COBOL_OCCURS_DESCENDING_KEY = 1;	    // Valore descending in clausola Cobol Occurs
	public final int COBOL_SIGN_LEADING = 0;	    		// Sign prima del campo
	public final int COBOL_SIGN_TRAILING = 1;	    		// Sign dopo il campo
	public final int COBOL_SYNCRONIZED_LEFT = 0;	        // Sincronized left
	public final int COBOL_SYNCRONIZED_RIGHT = 1;	        // Sincronized right
	public final int COBOL_JUSTIFIED_LEFT = 0;	        	// Justified left
	public final int COBOL_JUSTIFIED_RIGHT = 1;	        	// Justified right
	public final int COBOL_CONDITION_NAME_FROM = 0;	        // 88 cond-name literal A
	public final int COBOL_CONDITION_NAME_TO = 1;	        // 88 cond-name literal A THRU B

	// Tipo utilizzo campo
	public final int COBOL_DATA_ITEM_USED_INPUT = 0;	    // Campo in input
	public final int COBOL_DATA_ITEM_USED_OUTPUT = 1;	    // Campo in output
	public final int COBOL_DATA_ITEM_USED_INPUT_OR_OUTPUT = 2; // Campo in input oppure in output
	// Tipo open di un file
	public final int COBOL_OPEN_INPUT = 0;	    			// Open Input
	public final int COBOL_OPEN_OUTPUT = 1;	    			// Open Ouput
	public final int COBOL_OPEN_I_O = 2;	    			// Open I-O
	public final int COBOL_OPEN_EXTEND = 3;	    			// Open Extend
	// Usate in analisi data division
	public final String COBOL_FD = "FD";	    			// File descriptor
	public final String COBOL_SD = "SD";	    			// Sort/Merge descriptor
    // Numero righe istruzioni sorgente di prefetch  in analisi sorgenti Cobol
	public final int COBOL_PREFETCH_PARSING_ROWS = 3;
	public final int SQL_SCRIPT_PREFETCH_PARSING_ROWS = 30;
		
	// Tipo entry di definizione programma
	public final String ENTRY_PGM_TYPE_DEF_DATA_ITEM = "D"; 	// L'entry è una definizione dati
	public final String ENTRY_PGM_TYPE_DEF_LABEL = "L";	    	// L'entry è una label
	public final String ENTRY_PGM_TYPE_DEF_PROC_INTERNAL = "P";	// L'entry è una proc interna (Come una Section Cobol)
	public final String ENTRY_PGM_TYPE_DEF_COPY_DATA = "CPD";   // L'entry è uno statement Copy
	public final String ENTRY_PGM_TYPE_DEF_COPY_PROC = "CPP";	// L'entry è uno statement Copy

	// Tipo data item da estrarre da un campo di gruppo (metodo ProgramCobol.dataItemsUnderGroup())
	public final int DATA_ITEMS_GROUP_FIELDS = 0;    		// Restituiti solo i campi di gruppo del gruppo
	public final int DATA_ITEMS_ELEMENTARY_FIELDS = 1;   	// Restituiti solo i campi elementari del gruppo  
	public final int DATA_ITEMS_ALL = 2;   				    // Restituiti tutti i campi del gruppo (altri gruppi e dati elementari)

	// Tipo utilizzo data item da istruzione
	public final String INSTR_USE_DATA_ITEM_INPUT = "I";    // L'istruzione usa il data item in input
	public final String INSTR_USE_DATA_ITEM_OUTPUT = "O";   // L'istruzione usa il data item in output
	
	// Costtanti per logic Manager
	
	// Tipo attivazione LogicManager su specifica istruzione dinamica
	public final int LOGIC_ONLY_STATIC = 0;    		        	// Logic manager risolverà solo le istruzioni statiche nello stesso programma
	public final int LOGIC_DYNAMIC_SAME_PGM = 1;    			// Logic manager proverà a risolvere l'istruzione nel pgm e pgm chiamati
	public final int LOGIC_DYNAMIC_SPREADED_PGM = 2;   			// Logic manager proverà a risolvere l'istruzione nel pgm e in quelli chiamanti
	public final int LOGIC_BOTH_STATIC_AND_DYNAMIC_SAME_PGM = 3;// Logic manager proverà a risolvere l'istruzione statica o dinamica nel pgm  

	// Costanti per Viewe Pgm summary
	public final String PGM_SUMMARY_COPY_SOURCE_ROWS ="Y";      // Viewer Righe copy originali incluse 
	public final String PGM_SUMMARY_COPY_SOURCE_CODED ="N";     // Viewer Righe copy codificate incluse
	
	// Tipo istruzione Cics Read da risolvere
	public final String LOGIC_EXEC_CICS_READ_VSAM = "VSM";  // Exec Cics Read, ReadNext, ReadPrev
	public final String LOGIC_EXEC_CICS_READ_TS = "TS";     // Exec Cics Readq TS
	public final String LOGIC_EXEC_CICS_READ_TD = "TD";     // Exec Cics Readq TD
	
	
	// Tipi file per oggetti codificati e serializzati
	public final String SUFFIX_SERIALIZED_SCRIPT_SQL = "scriptSql";  	// file-name.scriptSql
	public final String SUFFIX_SERIALIZED_PGM = "program";  			// file-name.program
	public final String SUFFIX_SERIALIZED_PGM_IDX = "idx";  			// file-name.idx
	public final String SUFFIX_SERIALIZED_COPY = "copy";    			// file-name.copy
	public final String SUFFIX_SERIALIZED_JCL_JOB = "jclJob";      		// file-name.jclJob
	public final String SUFFIX_SERIALIZED_JCL_INCLUDE = "jclInclude";   // file-name.jclInclude
	public final String SUFFIX_SERIALIZED_JCL_PROC = "jclProc";         // file-name.jclProc

	// Sorgenti/oggetti da trattare in ExecutionDispatcher (EnumDirectivesExecution)
	public final String TYPE_OBJECT_ALL = "*ALL*"; 			// Vengono trattai tutti gli oggetti del tipo specificato
 	public final String TYPE_OBJECT_SINGLE = "*SINGLE*";	// Viene trattato solo l'oggetto del tipo specificato

	// Costanti per controllo GrapManager
	public final int MAX_PATHS_GRAPH_SIZE = 1000;          // Numero massimo paths da elaborare

	// Costanti per accesso ai dati con logical data view
	public final String LDV_RULE_TABLE_READ_ITEM_DEFAULT = "LdvReadItemRuleTable";  		// Nome ldv fìdi default per rule tables
	public final String LDV_RULE_TABLE_READ_SET_DEFAULT = "LdvReadSetRuleTable";  			// Nome ldv fìdi default per rule tables
	public final String LDV_RULE_TABLE_READ_SET_DEFAULT_COL_BOUND = "descItem";  			// Campo bound della ldv

	// Varie
	public final String ID_SUBGRAPH_MAINLINE = "== MAINLINE ==";	// Nome sottografo per mainline
	
	// Forward, funzioni di sistema  
	public final String FUNCTION_LOOKUP_RULE_TABLE = "FunctionLookUpRuleTable";				// Nome funzione standard di lookup da tabella
	public final String FUNCTION_SHOW_MESSAGES = "FunctionShowMessages";					// Nome funzione standard di visualizzazione messaggi
	public final String FUNCTION_SHOW_LDV_ACCESS = "FunctionShowLdvAccess";					// Nome funzione standard di visualizzazione dati ultimo accesso a logical data vaiew
	public final String FUNCTION_SHOW_EXCEPTION_FAILURE = "FunctionShowExceptionFailure";	// Nome funzione standard di visualizzazione dati uòtima exception

	
}
