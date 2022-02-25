package analyzer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import entities.EntityDynamicFieldSubWaitExt;
import entities.EntityRelationOrigin;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumObject;


/**
 *   Classe contenitore di servizio con le informazioni valide nel
 *   processo ricorsivo si individuazione dei valori del sottocampo.
 *   Il processo è di tipo backward, inizia con l'istruzione dinamica
 *   e continua percorrendo all'indietro le istruzioni di assegnazione
 */
public class LogicWorkProcess implements Cloneable {
	
	// Necessario per richiamo metodi statici che necessitano di accesso ai dati
	public UserConfiguration ucfg = null;                                                 	// Informazioni di configurazione per accesso ai dati
	public boolean isActivationByAnalysis = false;                                        	// true=attivazione a fronte di Analyzer
	
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Identificazione generale, programma e istruzione origine (nel processo spreaded potrebbe essere um programma chiamante)
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public ProgramCobol programCur = null;													// Descrittore programma corrente chiamante (valido nel processo spreaded)				
	public ProgramCobol programOrigin = null;												// Descrittore programma origine
	public Instruction instrDynamicOrigin = null;											// Istruzione dinamica in programma origine da risolvere, per LogicSamePgm 		
	public LogicDynamicInstruction logicDynamicInstructionOrigin = null;                   	// In processo spreaded, descrittore istruzione dinamica da risolvere
	public LogicDynamicField logicDynamicFieldOrigin = null;        	                    // In processo spreaded, campo da risolvere del programma ORIGINE (da struttura logiche origine)
	public LogicDynamicFieldSubSetting logicDynamicFieldSubOriginLastSet = null;        	// In processo spreaded, ultima assegnazione da risolvere del programma ORIGINE
	public int logicDynamicInstructionOriginNumField = 0;                                  	// In processo spreaded, numero parametro istruzione zero-based
	public int logicDynamicInstructionOriginNumFieldSub = 0;                              	// In processo spreaded, numero sottocampo in parametro istruzione zero-based
	
    //////////////////////////////////////////////////////////////////////////////////////
    // Campo e sottocampi di cui trovare  i valori
	// Sottocampi modellati da LogicDynamicSubField, dentro LogicDynamicField
    /////////////////////////////////////////////////////////////////////////////////////
    
	public LogicDynamicField dynamicFieldToSolve = null;  									// Descrittore campo originario di cui trovare i valori
	public LogicDynamicFieldSub dynamicFieldSubToSolve = null;  							// Descrittore sottocampo corrente in elaborazione	(pointer a dynamicFieldToSolve.al_SubField(n)
	public DataItemCobolIdentifier dataItemFieldToSolve = null;                            	// Identifier Campo in input di cui trovare i valori
	public DataItemCobolIdentifier[] arDataItemFieldSubToSolve = null;                     	// Identifier Sottocampi di cui trovare i valori
	
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Informazioni di controllo per individuazione valori nei processi ricorsivi di ricerca path/set/..
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	// Paths di esecuzione completi da inizio programma a istruzione target
	public ProgramPath[] arPaths = null;
	
	// Origine esecuzione
	public boolean isProcessLogicSpreaded = false;                                         	// True indica esecuzione di processo logiche spreaded
	public boolean isExecLogicSpreaded = false;                                            	// True indica esecuzione di logiche spreaded in processo spreaded
	                                                                                
	// Errore nelle strutture dinamiche e/o nei programmi
	public boolean isAnyErrorDetected = false;                                             	// Indica un errore di programma
	
	// Definizioni di servizio per programm entry e istruzione
	public ProgramCobolEntry<? extends Instruction> cobolProcEntry = null;					// Generico entry di programma
	public InstructionCobolProcedure instrCobolProcSet = null;								// Generica istruzione
	
	// Di Servizio. Definizioni per sender e receiver di trasformaszioni, nel processo di trasformazione campo elementare
	public DataItemCobolIdentifier identifierSender = null;					    			// Identificatore completo Sender in Move
	public DataItemCobolIdentifier identifierReceiver = null;      							// Descruttore campo di cui trovare il valore

	// Definizione di lavoro a fronte di assegnazione in estrazione catene di trasformazione
	public LogicDynamicFieldSubSetting logicDynamicFieldSubSettingWrk = null;			    // Oggetto classe interna trasformazione in logicInfoDynamic

	// Elenco cumulativo Catene di trasformazione sottocampo e catena corrente
	public ArrayList<ArrayList<LogicDynamicFieldSubSetting>> al_al_chainSetSubFieldAll = null;// ArrayList di catene di trasformazioni individuate   
	public ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubFieldCur = null;          	// ArrayList catena corrente trasformazioni di lavoro  
	
	// Elenco Receiver impliciti, da pos, per lng dedotti da sender in singola trasformazione (Move)
	// Le 5 ArrayList successive viaggiano insieme 
	public ArrayList<DataItemCobolIdentifier> al_nextReceiverImplicit = null;			   // Receiver impliciti dedotti dal Sender (le 5 ArrayList viaggiano insieme)
	public ArrayList<Integer> al_nextReceiverImplicitPos = null;                           // Posizione in receiver impliciti
	public ArrayList<Integer> al_nextReceiverImplicitLng = null;                           // Lunghezza in receiver impliciti
	public ArrayList<Integer> al_nextSubFieldImplicitPos = null;                           // Posizione in sottocasmpo origine
	public ArrayList<Integer> al_nextSubFieldImplicitLng = null;                           // Lunghezza in sottocasmpo origine
	
	// Per gestione possibile loop in generazione catene di trasformazione, a fronte di assegnazioni circolari
	public Map<String,DataItemCobolIdentifier> map_identifierReceiverManaged = null;       // Receiver già analizzati generando le trasformazioni di un sottocampo
	
	// Dati di identificazione media per recupero valori esterni al programma (da DynamivValueExt/DynamicCicsMapping)
	public ArrayList<String> al_externalCicsName = null;                                   // Nomi Cics da direttive di esecuzione
	public ArrayList<String> al_externalDDName = null;                                     // DDname in jcl Cics nome file Vsam/coda/ o nome tab Sql, seg DL1,...
	public ArrayList<String> al_externalPhisicalFile = null;                               // File fisici di cui estrarre i valori da media esterni
	public EnumObject externalTypeObject = null;                                           // Tipo oggetto esterno (File, Coda Ts, CicsField ...)
	public EnumDataItemSystemEnvironment externalTypeSystemField = null;                   // Qualifica campo precedente
	public String externalDsname = "";                                                     // File fisico dove cercare i valori esterni
	public String externalCicsName = "";                                                   // Cics per il quale i valori esterni sono validi
	public String externalObjectName = "";                                                 // Nome oggetto fisico esterno (Phisical-file, Ts-Queue, ..)
	public String externalIdFieldColumn = "";                                              // Nome colonna (EIBTRMID,.., Sql-col,..) o spaces
	public int externalPosColumn = 0;                                                      // Posizione colonna in oggetto esterno (1-based)
	public int externalLngColumn = 0;                                                      // lunghezza colonna in oggetto esterno 
	public int externalPosInColumn = 0;                                                    // Posizione valore in colonna (1-based)
	public int externalLngInColumn = 0;                                                    // lunghezza valore in colonna  
	
	// Per gestione ultima assegnazione e varie
	public EnumLogicSetMode curLastSet = null;                                             // Tipologia ultima assegnazione
	public String valueLiteral = "";                                                       // Valore iniziale o literal in Move individuato in ultima assegnazione
	public boolean lastSetManaged = false;												   // true indica assegnazione valida di valore individuata
	
	// Valori correnti istruzione e catena di trasformazione correnti
	public int numInstrSet = 0;									   					    	// Numero istruzione di impostazione sottocampo
	public int numInstrSetOrigin = 0;									   					// Numero istruzione Move origine del receiver esplicito e impliciti
	public int numChainSet = 0;                                                            	// Numero catena di trasformazione corrente
	
	// Posizione e lunghezza espressi esplicitasmente in trasformazione (Move) di sender e receiver
	public int posRcvRefMod = 0;															// Posizione in receiver da ref mod istruzione
	public int lngRcvRefMod = 0;															// Lunghezza in receiver da ref mod istruzione
	public int posSndRefMod = 0;															// Posizione in sender   da ref mod istruzione
	public int lngSndRefMod = 0;															// Lunghezza in sender   da ref mod istruzione		                                                                                // Per esempio MOVE A TO B
    
	// Posizione e lunghezza correnti receivers propagata ricorsivamente backward nel processo di generazione catene di trasformazione
    // per estrarre il valore finale da data item, literal, campo di ioarea ....
    // I campi successivi sono relativi al campo trasformato e assegnato ricorsivamente (identifierReceiver)
	public int posRcv = 1;															        // Posizione corrente in receiver da considerare
	public int lngRcv = 0;															     	// Lunghezza corrente in receiver da considerare 

	// Posizione e lunghezza correnti nel sottocampo origine dell'istruzione dinamica, propagate ricorsivamente
    // I valori estratti saranno assegnati alla area del sottocampo individuata da posizione e lunghezza specificati
	public int posInSubField = 1;															// Posizione corrente in sottocampo origine che si sta trattando
	public int lngInSubField = 0;															// Lunghezza corrente in sottocampo origine che si sta trattando

    // Posizione e lunghezza sender utilizzati nel computo dell'input per un nuovo receiver
	public int posSnd = 1;															        // Posizione corrente in sender da considerare
	public int lngSnd = 0;															     	// Lunghezza corrente in sender da considerare 

    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Altre Informazioni di controllo generali utilizzate sia per le logiche stesso programma che quelle spreaded
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
	// Catene possibili di assegnazioni di trasformazione per un sottocampo
	// La ArrayList di primo livello è relativa al sottocampo
	// La ArrayList di secondo livello è relativa alle sue trasformazioni
	public ArrayList<ArrayList<LogicDynamicFieldSubSetting>> al_al_chainSetSubField = null; 

	// Elenco valori sottocampo e campi da restituire in output  
	public ArrayList<String> al_valuesField = null;									// Servizio ArrayList valori definitivi singolo campo
	public ArrayList<String> al_valueSubField = null;                          		// Servizio ArrayList valori definitivi singolo sottocampo
	
	// Informazioni per gestione ultima assegnazione da campo di linkage section 
	public EnumLogicSetPointerArea typePointerArea = null;							// Tipo area contenente il pointer         a fronte di LAST_SET_BY_COBOL_LINKAGE (T0028)
	public int dspFieldInLinkageArea  = 0;                         					// Displacement campo in area di linkage   a fronte di LAST_SET_BY_COBOL_LINKAGE/TWA/CSA
	public int dspPointerInLinkageArea = 0;						            		// Displacement pointer in tipo area indicata da typePointerArea
	public int numUsingParmPointer = 0;						    					// Numero parametro Using con pointer      a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM
	public int dspPointerInUsingParm = 0;						    				// Displacement pointer in parametro Using a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM
	public InstructionCobolDataItem dataItemPointer = null;							// Definizione  pointer in parametro Using a fronte di LAST_SET_BY_COBOL_LINKAGE e POINTER_INSIDE_USING_PARM
	public ArrayList<EnumLogicSetPointerArea> al_typePointerArea = null;			// Insieme alle ArrayList successive
	public ArrayList<Integer> al_dspPointerInLinkageArea = null;					// Insieme alle ArrayList successive
	public ArrayList<Integer> al_numUsingParmPointer = null;						// Insieme alle ArrayList successive
	public ArrayList<Integer> al_dspUsingParmPointer = null;						// Insieme alle ArrayList successive
	public ArrayList<InstructionCobolDataItem> al_dataItemPointer = null;			// Insieme alle ArrayList successive

	// Di servizio
	public LogicDynamicField dynamicFieldWrk = null;                               	// Descrittore campo dinamico
	public EntityDynamicFieldSubWaitExt entityDynamicFieldSubWaitExtWrk = null;    	// Richiesta di dati esterni da inserire su db
	public ArrayList<LogicDynamicFieldSub>  al_DynamicFieldSubWrk = null;			// Descrittori sottocampi dinamici
	public ArrayList<LogicDynamicFieldSubSetting> al_DynamicFieldSubSetWrk = null;	// Descrittori specifiche trasformazioni sottocampo
	public ArrayList<LogicDynamicFieldSubSetting> al_DynamicFieldSubSetSpreadedWrk = null;// Descrittori assegnazioni acquisite nel processo di soluzione spreaded
	public ArrayList<String> al_ValuesWrk = null;									// Valori sottocampo/campo
	
	
	public LogicCallerOrigin callerOrigin = null;                                  	// Info su istruzione origine in programma chiamante significativo di servizio
	public ArrayList<LogicCallerOrigin> al_callerOrigin = null;                    	// Info su istruzione origine in programma chiamante significativo di servizio
	public EntityRelationOrigin relationOriginSpreaded = null;                 		// Info Origine relazione programma chiamante
	public LogicDynamicFieldSubSetting lastSetSpreaded = null;                 		// Ultima assegnazione da trattare x logiche spreaded
	public LogicDynamicFieldSubSetting lastSetLocal = null;                    		// Ultima assegnazione da trattare x logiche local in update flag istruzione
	public String dynamicFieldName = "";											// Nome campo dinamico da risolvere
	public int numUsingParmFound = 0;                                           	// Numero parametro in procedure division using individuato
	public int dspInUsingParmFound = 0;                                         	// Displacement campo in parametro using individuato
	public int dspInCicsTwaFound = 0;                                         	    // Displacement campo in area Cics Twa individuato
	public int dspInCicsCsaFound = 0;                                         	    // Displacement campo in area Cics Csa individuato
	public int dspInCicsDfhcommareaFound = 0;                                      	// Displacement campo in area Cics Dfhcommarea individuato
	public int numUsingParmNext = 0;                                           		// Numero parametro in procedure division using X successivo caller
	public int dspInUsingParmNext = 0;                                         		// Displacement campo in parametro using X successivo caller
	public int dspInCommareaNext = 0;                                         	 	// Displacement campo in DFHCOMMAREA X successivo caller
	public int posRcvCaller = 0;                                               		// Posizione in nuovo campo in caller in logiche spreaded
	public int lngRcvCaller = 0;                                               		// Lunghezza in nuovo campo in caller in logiche spreaded
	public int sizeSubFieldOrigin = 0;                                         		// Dimensioni sottocampo in pgm origine da risolvere spreaded
	public int posRcvInSubFieldSpreaded = 0;                                       	// Posizione in sottocampo di pgm origine da risolvere spreaded
	public int lngRcvInSubFieldSpreaded = 0;                                       	// Lunghezza in sottocampo di pgm origine da risolvere spreaded
	public int dspLastTransformedDataItem = 0;                                     	// Displacement campo origine trasformazione
		
	// Gestione individuazione data item/pointer appartenente a Cics TWA/CSA
	public Set<Integer> set_setCicsAddressTwaCsa = null;							// Numeri istruzioni EXEC CICS ADDRESS TWA|CSA(pointer)
	public Set<Integer> set_setCicsAddressSet = null;								// Numeri istruzioni EXEC CICS ADDRESS SET(Address Of area) USING(pointer)
	public Set<Integer> set_setCobolAddressOf = null;								// Numeri istruzioni SET ADDRESS OF area TO pointer
	public Set<Integer> set_setCobolToAddressOf = null;								// Numeri istruzioni SET pointer TO ADDRESS OF area  

	// Per gestione ricomposizione valori in logiche spreaded aggregate per pgm/numero istruzione origine caller
	public int lvlPgmCaller = 0;                                               		// Livello programma chiamante in logiche spreaded
	public int lvlPgmSet = 0;                                                  		// Livello programma di assegnazione valore (0=programma origine)
		
	// Flag stato dinamico istruzione per aggiornamento finale istruzione origine
	// true  per attivazione a fronte di soluzione istruzioni dinamiche a fine programma o a fronte di processo spreaded
	// false per attivazione da web service, modulo Logic, per il solo tracciamento delle trasformazioni
	public boolean isDynamicInstrSolving = false;                                   // Flag origine attivazione 	
	
	// Flags istruzione dinamica
	public boolean isInstrDynamicLight = true;							 		    // Con operando/i dinamici con value non movimentato/i
	public boolean isInstrDynamicToSolve = true;							 		// Sicuramente dinamica
	public boolean isInstrDynamicSolved = true;						 				// Con soluzioni parziali individuate
	public boolean isInstrDynamicSolvedFull = true;					 				// Con soluzioni complete individuate 
	public boolean isInstrDynamicSpreaded = false;					 				// Con soluzioni in programmi chiamanti/chiamati
	public boolean isInstrDynamicWaitingForData = false;				 			// In attesa di dati esterni per qualche sottocampo o porzione
   
	// Flag di servizio
	public boolean allSubFieldsWithValues = false;									// Ogni sottocampo del campo dinamico è risolto
	public boolean isOperandLiteral = false;                        				// Operando espresso da literal 
	public boolean isSpreadedSubFielValuePartial = false;                          	// Composizione valori per sottocampo parziale in logiche spreaded
    
	/*
	 * Costruttore
	 */
	public LogicWorkProcess() {
		al_al_chainSetSubFieldAll = new ArrayList<ArrayList<LogicDynamicFieldSubSetting>> ();	// Catene di trasformazioni individuate
		al_chainSetSubFieldCur = new ArrayList<LogicDynamicFieldSubSetting> ();			    // Catena di trasformazioni corrente di lavoro
		al_nextReceiverImplicit = new ArrayList<DataItemCobolIdentifier> ();
		al_nextReceiverImplicitPos = new ArrayList<Integer> ();
		al_nextReceiverImplicitLng = new ArrayList<Integer> ();         
		al_nextSubFieldImplicitPos = new ArrayList<Integer> ();                    
		al_nextSubFieldImplicitLng = new ArrayList<Integer>();  
		al_externalCicsName = new ArrayList<String> ();
		al_externalDDName = new ArrayList<String> ();
		al_externalPhisicalFile = new ArrayList<String> ();
		al_DynamicFieldSubSetSpreadedWrk = new ArrayList<LogicDynamicFieldSubSetting> ();
		al_DynamicFieldSubWrk = new ArrayList<LogicDynamicFieldSub>();
		al_valuesField = new ArrayList<String> ();   
		callerOrigin = new LogicCallerOrigin();
		map_identifierReceiverManaged = new HashMap<String, DataItemCobolIdentifier> ();    // Receiver già analizzati in processo ricorsivo generazione catene trasformazione
		externalTypeObject = EnumObject.NOT_ASSIGNED;
		externalTypeSystemField = EnumDataItemSystemEnvironment.NOT_ASSIGNED;
	    set_setCicsAddressTwaCsa = new HashSet<Integer> ();								 
	    set_setCicsAddressSet = new HashSet<Integer> ();						 
	    set_setCobolAddressOf = new HashSet<Integer> ();								 
	    set_setCobolToAddressOf = new HashSet<Integer> ();								 
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected LogicWorkProcess clone()  {
		LogicWorkProcess ilpwCloned = null;
		try {
			ilpwCloned = (LogicWorkProcess) super.clone();
		} catch (Exception e) {
			return null;
		}
		return ilpwCloned;
	}
}

