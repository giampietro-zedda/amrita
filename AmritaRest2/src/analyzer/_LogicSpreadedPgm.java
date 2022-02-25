package analyzer;
import java.io.File;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import utilities.SystemService;
import analyzer.LogicDynamicFieldSub;
import analyzer.LogicDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumPrecompilerReservedWords;
import exception.ExceptionAmrita;


/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LogicSpreaded
 * </h1>
 * <p>
 * Soluzione istruzioni dinamiche nei programmi chiamanti/chiamati <br>
 * <p>
 *
 * A fronte dell'analisi di un programma contenente istruzioni dinamiche, attraverso LogicSamePgm vengono<br>
 * risolte le istruzioni dinamiche, per quanto possibile, nello stesso programma.
 * <p>
 * Una struttura con tutte le informazioni relative alle istruzioni dinamiche  LogicInfoDynamic, viene<br>
 * memorizzata nell'oggetto ProgramCobol e fa parte integrante dell'analisi del programma.
 * <p>
 * L'oggetto di LogicInfoDynamic oltre a contenere i descrittori delle istruzioni dinamiche del programma,<br>
 * contiene anche i descrittori delle istruzioni da risolvere nei programmi chiamati/chiamanti<br>
 * a partire dall'ultima assegnazione LAST_SET_BY_LINKAGE, LAST_SET_BY_USING_PARM, etc.<br>
 * <p>
 * Questo permette un completo tracciamento delle trasformazioni del dato elementare, partendo dal programma origine<b>
 * attraverso tutte le trasformazioni dei sottocampi elementari.<br>
 * <p>
 * L'analisi delle trasformazione dei campi elementari nei programmi chiamanti/chiamati viene effettuata sempre attraverso
 * LogicSamePgm, che opera sul programma chiamante/chiamato in modo indipendente.<br>
 * Il riferimento di partenza è sempre l'istruzione dinamica del programma origine codigicata dq LogicDynamicInstruction e,
 * quindi, attraverso la codifica dei sottocampi e delle assegnazioni dei vari sottocampi (LogicFieldSubSetting nel programma
 * origine e in quelli chiamanti/chiamati.
 * <p>
 * Gli aggiornamenti su database, attraverso la classe AnalyzerDbinfo, viene effettuata dal processo chiamante a fine
 * elaborazione, popolando le strutture di AnalyzerDbinfo, con le informazioni dinamiche dei programmi chiamanti/chiamati elaborati,
 * memorizzate nell'oggetto LogicInfoDynamic associato al programma origine.
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 02/05/2021
 * @see AnalyzerCobolProgram
 * @see Instruction
 * 
*/

public class _LogicSpreadedPgm extends ExecutionShared implements AmritaConstants {
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali                                  //                                                        //
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// E' il descrittore del programma origine dal quale le logiche sono state originate
	private ProgramCobol program = null;
	
	// Reference al descrittore delle istruzioni dinamiche nel programma origine.
	// Contiene tutte le informazioni per ogni campo di istruzione dinamica.
	// Le informazioni sono eventualmente pronte per essere inserite su db.
	private LogicInfoDynamic logicInfoDynamicOrigin = null;

    // Gestore generalizzato logiche applicative.
    // Utilizzato per determinare i valori dinamici assunti dalle variabili.
    private LogicSamePgm logicSamePgm = null;
    
    

	/**
	 * Costruttore 
	 *  
	 */
	public _LogicSpreadedPgm(UserConfiguration sd, ExecutionDirectives di, ProgramCobol program) {
		super(sd, di);
		this.program = program;
		
		this.logicSamePgm = new LogicSamePgm(ucfg, di, this.program);
		this.logicSamePgm.setLogicInfoDynamic(this.logicInfoDynamicOrigin);
		this.logicInfoDynamicOrigin = program.getLogicInfoDynamic();
		this.logicInfoDynamicOrigin.setProgram(this.program);
		this.logicSamePgm.getLogicTools().setLogicInfoDynamic(this.logicInfoDynamicOrigin);
			
	} 
	
	/**
	 * 
	 * Restituisce il descrittore del programma contenente le istruzioni da risolvere.
	 * 
	 * @return the program
	 */
	public ProgramCobol getProgram() {
		return program;
	}

	/**
	 * 
	 * Imposta il descrittore del programma contenente le istruzioni da risolvere.
	 * 
	 * 
	 * @param program the program to set
	 */
	public void setProgram(ProgramCobol program) {
		this.program = program;
	}

	/**
	 * 
	 * Restituisce l'oggetto descrittore delle istruzioni dinamiche del programma da risolvere.
	 * 
	 * @return the logicInfoDynamic
	 */
	public LogicInfoDynamic getLogicInfoDynamic() {
		return logicInfoDynamicOrigin;
	}


	/**
	 * 
	 * Imposta l'oggetto descrittore delle istruzioni dinamiche del programma da risolvere.
	 * 
	 * @param logicInfoDynamic the logicInfoDynamic to set
	 */
	public void setLogicInfoDynamic(LogicInfoDynamic logicInfoDynamicOrigin) {
		this.logicInfoDynamicOrigin = logicInfoDynamicOrigin;
	}

	/**
	 * Restituisce i valori dinamici del campo di input, generati nel programma origine e/o chiamanti. <br>
	 *  <p>
	 * Viene risolta l'istruzione dinamica a partire dalle ultime impostazioni non risolte di ogni sottocampo.<br>
	 * La ricerca viene estesa ai programmi chiamanti, a qualsiasi livello di richiamo.<br>
	 * Il processo termina quando si risolve il sottocampo in qualche programma chiamante oppure si individua
	 * un media esterno dal quale recuperari i valori.<br>
	 * <p>
	 * Per ogni sottocampo nel programma origine ancora da risolvere, si individuano nuovi campi nei programmi
	 * chiamanti. Per ogni programma chiamante si attiva in modo indipendente e isolato la ricerca valori
	 * standard stesso programma, richiamando LogicSamePgm.dynamcValues(), con specifiche e temporanee
	 * strutture di controllo relative .<br> 
	 * A fronte di questo processo vengono trovati i valori dei sottocampi del
	 * programma origine nei programmi chiamanti, come assegnazioni di nuovi campi.<br>
	 * <p>
	 * Quando tutti i sottocampi costituenti un operando dinamico, risultano risolti, i loro valori vengono
	 * moltiplicati per comporre i valori definitivi dell'operando dinamico.
	 * A questo punti l'istruzione risulta essere completamente risolta.<br>
	 * <p>
	 * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 */
	public ArrayList<String> dynamicValues(
									       ProgramCobol programOrigin				 	 // Descrittore programma origine				
								         , Instruction instrOrigin			             // Istruzione in programma origine da risolvere		
								         , DataItemCobolIdentifier dataItemToSolve       // Operando di cui trovare i valori (campo elementare o di gruppo)
									      ) throws ExceptionAmrita, SQLException {

		LogicWorkProcess ilpw = null;								// Informazioni e reference per il completo controllo del processo normale e ricorsivo
		ArrayList<String> al_pgmNameCaller = null;                  // 
		ArrayList<String> al_valuesFieldOrigin = null;                   // Valori già presenti per il campo 
		int i = 0;
        
		/////////////////////////////////////////////////////////////////////////////////
		// Inizializzazione
		/////////////////////////////////////////////////////////////////////////////////

		// Attività iniziali
		ilpw = new LogicWorkProcess();                     			// Allocazione oggetto di lavoro  
		ilpw.isProcessLogicSpreaded = true;                         // Utilizzato nella composizione dei valori del sottocampo
		ilpw.programOrigin = programOrigin;							// Programma radice con istruzione dinamica da risolvere
		ilpw.instrDynamicOrigin = instrOrigin;			            // istruzione dinamica origine da risolvere
		ilpw.isExecLogicSpreaded = true;                            // Istruzione dinamica con logiche spreaded nei programmi chiamanti
		ilpw.isInstrDynamicToSolve = true;                          // Istruzione dinamica ancora da risolvere
		ilpw.isSpreadedSubFielValuePartial = false;                 // Composizione valori per tutti i sottocampi
		
        // Struttura X istruzioni dinamichq in programma origine
		this.logicInfoDynamicOrigin = programOrigin.getLogicInfoDynamic();
		
		// Individuazione campo da trattare nel programma chiamante.
		// In caso di exec cics send map possono esserci map e mapset
		i = 0;
		ilpw.dynamicFieldName = dataItemToSolve.getNameIdentifier();
		ilpw.logicDynamicInstructionOrigin = this.logicInfoDynamicOrigin.getDynamicInstr(instrOrigin.getNumInstr());
        for (LogicDynamicField logicDynamicField : ilpw.logicDynamicInstructionOrigin.al_dynamicField) {
			if (logicDynamicField.dataItemCobolIdentifier.getNameIdentifier().equals(ilpw.dynamicFieldName)) {
				ilpw.logicDynamicInstructionOriginNumField = i;
				ilpw.logicDynamicFieldOrigin = logicDynamicField;
				break;
			}
		}
	
        // Il processo spreaded deve restituire SOLO i valori trovati nei pgm chiamanti e NON quelli già presenti
        // Il processo di combinazione valori è comune, vengono quindi salvati i valori già presenti ed eliminati a fine processo.
        al_valuesFieldOrigin = new ArrayList<String>();
        al_valuesFieldOrigin.addAll(ilpw.logicDynamicFieldOrigin.al_valuesField);
               
		// Get nomi programmi chiamanti, che chiamano direttamente il programma  
		al_pgmNameCaller = LogicTools.getPgmCallers(programOrigin.getProgramName(), di.systemInput, di.subSystemInput, ucfg);			 
		
		// Nessun chiamante: impossibile risolvere l'istruzione
		if (al_pgmNameCaller.size() == 0) {
			return new ArrayList<String>();
		}
		
		// Scan SOTTOCAMPI campo in programma origine (presente anche in caso di campo non di gruppo)
		ilpw.al_DynamicFieldSubWrk = this.logicInfoDynamicOrigin.getDynamicFieldsSub(instrOrigin.getNumInstr(), ilpw.dynamicFieldName);
		for (LogicDynamicFieldSub logicDynamicFieldSub : ilpw.al_DynamicFieldSubWrk) {

			ilpw.dynamicFieldSubToSolve = logicDynamicFieldSub;
            ilpw.sizeSubFieldOrigin = logicDynamicFieldSub.dataItemIdentifierSubField.getDataItem().getSizeBytes();
            
            // Sottocampo già risolto: non va considerato
            if (logicDynamicFieldSub.isSolved) {
				continue;
			}
            
            // Sottocampo da risolvere o in attesa di dati esterni 
             
			// Scan ultime assegnazioni sottocampo spreaded ancora da risolvere generate a partire programma ORIGINE
            // al_lastSetSpreaded è stato valorizzato a livello di LogicSamePgm
			for (LogicDynamicFieldSubSetting lastSetTotal : logicDynamicFieldSub.al_lastSetTotal) {

				ilpw.lastSetSpreaded = lastSetTotal;                      // Ultima assegnazione da risolvere per il sottocampo in programma chiamante
                  
				// Ultima assegnazione già risolta 
				if (lastSetTotal.lastSetSolved) {
					continue;
				}
                				
				// Ultima assegnazione già risolta 
				if (isLastSetWaitingForData(lastSetTotal)
			    ||  !isLastSetToBeSolvedInCaller(lastSetTotal)) {
					continue;
				}
                				
				// Ultima assegnazione da risolvere, in quanto in attesa di dati esterni o parametro di call using/dfhcommarea/linkage/twa/csa
				// Si individua il campo da risolvere (potrebbero esserci più campi come per cics send map, cancel etc)
				ilpw.logicDynamicFieldSubOriginLastSet = lastSetTotal;
				
				// Soluzione ultima assegnazione sottocampo (o una sua porzione) spreaded nei programmi chiamanti.
				// Viene popolata le strutture del programma origine con i valori individuati.
				// I valori sono individuati da programma, istruzione e livello di assegnazione.
				// In caso di un solo livello di chiamanti le informazioni coincidono.
				dynamicValuesSpreadedExtractor(ilpw
						                     , ilpw.programOrigin             // Descrittore programma origine
					                         , al_pgmNameCaller
											  );	 

			} // end-for last-set

			// N.B. Le assegnazioni spreaded generate per il sottocampo riscontrate nei programmi chiamanti
            // Sono state accodate nella ArrayList cumulativa LogicDynamicFieldSub.al_al_chainSetSubField
						
		} // end-for sottocampi
		
		// I valori estratti nei programmi chiamanti sono in al_valuesField
		ilpw.dynamicFieldToSolve.al_valuesField = ilpw.al_valuesField;
		
		// Rimozione valori NON generati nel corrente processo spreaded
		ilpw.dynamicFieldToSolve.al_valuesField.removeAll(al_valuesFieldOrigin);
		
		// I flag istruzione sono aggiornati dal chiamante attraverso LogicInfoDynamic.putDatabaseInfoAll()
		
		return ilpw.dynamicFieldToSolve.al_valuesField;
	} 

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi privati                                        /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	/* ---------------------------------------------------------------------
	 * Procedura di soluzione sottocampo in programmi chiamanti 
	 * ---------------------------------------------------------------------
	 * 
	 * Si individuano i programmi chiamanti e si considera solo il primo livello 
	 * Si considerano i numeri istruzione origine di call/link/Xctl e, per ognuno di questi:
	 *   Si Individua il nuovo campo di cui cercare i valori da inizio programma
	 *   Si attiva la ricerca valori stesso programma standard per ogni istruzione di richiamo
	 *   Si memorizzano, se trovati, i valori nella struttura del programma origine
	 *   Se non trovati valori la ricerca valori standard inserirà una nuova ultima assegnazione da risolvere
	 *   
	 * Se l'assegnazione dei valori è effettuata non direttamente sul chiamante ma in chiamanti di piu alto livello
	 * è quindi necessario eseguire più volte il processo di esecuzione spreaded fino a individuare il chiamante
	 * che imposta i valori
	 */
	private void dynamicValuesSpreadedExtractor( LogicWorkProcess ilpw						// Stato ricorsivo iniziale sul programma origine
			                                   , ProgramCobol programOrigin		    		// Programma origine
											   , ArrayList<String> al_pgmNameCaller			// Programmi chiamanti
											   ) throws ExceptionAmrita, SQLException {

		LogicWorkProcess ilpwCaller = null;                   			    // Struttura di servizio specifica per ricerca valori in programma chiamante
		ProgramCobol programCaller = null;                                  // Programma chiamante
		Instruction instrCaller = null;                             		// Istruzione origine in programma chiamante 
		DataItemCobolIdentifier dataItemCallerToSolve = null;       		// Nuovo identificatore campo di cui cercare i valori
		ArrayList<DataItemCobolIdentifier> al_dataItemCallerToSolve = null;	// Nuovi identificatori campo di cui cercare i valori
        ArrayList<LogicCallerOrigin> al_callerOrigin = null;        		// Origine in programmi chiamanti con info complete
		ArrayList<String> al_valuesInProgramCaller = null;          		// Valori individuati nel programma chiamante
 		String programNameOrigin = "";                                    	// 
 		
        programNameOrigin = programOrigin.programName;
        
		// Scan nomi programmi chiamanti
		for (String pgmNameCaller : al_pgmNameCaller) {
			
			programCaller = deserializeProgram(pgmNameCaller);                              // Programma chiamante codificato
				
			ilpw.programCur = programCaller;                                               	//
			LogicTools.loadInfoPointersAndAreasAddressed(ilpw);                             // Istruzioni di Set Address, Cics Address etc in strutture di servizio
			al_callerOrigin = LogicTools.getPgmCallerOrigin(pgmNameCaller, programNameOrigin, 1, di.systemInput, di.subSystemInput, ucfg);	// Punti origine relazione caller/called
			ilpwCaller = new LogicWorkProcess();                                          	// Allocazione oggetto di lavoro (allocazioni varie nel cosruttore) 
			ilpwCaller.lastSetSpreaded = ilpw.lastSetSpreaded.clone();                    	// Si risolve l'ultima assegnazione spreaded del chiamato corrente
			
			// Scan punti origine di richiamo nel programma chiamante corrente (call/xctl/link)
			// Lo stesso programma potrebbe essere richiamato più volte generando potenzialmente valori diversi
			for (LogicCallerOrigin logicCallerOrigin : al_callerOrigin) {
    
				ilpw.callerOrigin = logicCallerOrigin;									 	//  
				ilpw.callerOrigin.programCaller = programCaller;                           	//  
				
				// Individuazione nuovo campo di cui trovare i valori, nel programma chiamante corrente
				detectNewFieldsOnProgramCaller(ilpw);										// -> ilpwCalled.callerOrigin. ....
				al_dataItemCallerToSolve = logicCallerOrigin.al_dataItemCallerToSolve;
				
				// Nessun campo individuabile dal punto di chiamata: scarto, Next call/xctl/link  
				if (al_dataItemCallerToSolve.size() == 0) {  
					continue;
				}

				// Per il momento si considera solo il primo campo individuato, 
				// In ogni caso LogicSamePgm() risolverà eventuali gruppi e/o redefines
				dataItemCallerToSolve = al_dataItemCallerToSolve.get(0);
				
				// Non è necessario un nuovo decrittore di informazioni dinamiche per il chiamante, resta valido quello del pgm origine
				// Valorizzazione nuovo descrittore di servizio campo dinamico di cui trovare i valori 
				instrCaller = ilpw.callerOrigin.entryInstr.getInstruction();
				
				// Inizializzazioni specifiche area di lavoro
				initialIlpwForNewCaller(ilpw);
				
				// Inizializzazioni per ricerca valori in nuovo programma chiamante	
                // Impostazione programOrigin, programCaller (in programCur)
				ilpw = LogicTools.dynamicValuesInitial(ilpw, programCaller, programOrigin, instrCaller, dataItemCallerToSolve);
				ilpw.callerOrigin = logicCallerOrigin;	        											//
				ilpw.instrDynamicOrigin = instrCaller;	        											// Istruzione call/xctl/link con campo da risolvere
				
				// Lunghezza in bytes campo in programma origine disponibile in ilpw.sizeSubFieldOrigin
				ilpw.posRcv = 1;	 			                                                            // A partire dalla  posizione nel receiver (1 o quella corrente)							
				ilpw.lngRcv = dataItemCallerToSolve.getDataItem().getSizeBytes();				            //   Per la lunghezza massima del campo in bytes
				ilpw.posInSubField = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField();     // Mappata in sottocampo origine da posizione
				ilpw.lngInSubField = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField();     //   Per lunghezza
								
				// Tentativo di estrazione valori dinamici nel programma chiamante corrente 
				ilpw.isDynamicInstrSolving = true;           												// Attivazione per soluzione istruzione dinamica target
				ilpw.isExecLogicSpreaded = true;            												// Attivazione a fronte di istruzione dinamica in programma caller
				
				// Valori in ilpw.al_valuesField;
				ilpw.al_valuesField = this.logicSamePgm.dynamicValuesExtractor(ilpw, programCaller, instrCaller, dataItemCallerToSolve, ilpw.posInSubField, ilpw.lngInSubField);				
				
				// Individuati valori per il sottocampo nel programma chiamante relativi al sottocampo del programma origine.
				// Struttura sottocampo chiamante aggiornata via LogicSamePgm disponibile per aggiornamento trasformazioni e valori in programma origine..
				// Le assegnazioni e i valori sono da memorizzare in modo cumulativo nella stessa struttura del programma origine
				// Sono importanti programma e istruzione origine nella catena di chiamanti corrente.
				al_valuesInProgramCaller = ilpw.al_valuesField;
				if (al_valuesInProgramCaller.size() > 0) {
					ilpw.lastSetSpreaded.lastSetSolved = true;
					// Ultime assegnazioni per il punto origine da accodare a quelle del sottocampo a fine processo ricorsivo
					updateValuesInSubFieldOrigin(ilpw);
					continue;		
				}
  											
			} // end-for istruzioni call/xctl/link in pgm caller corrente
			
		} // end-for pgms caller
		
	}

    /*-------------------------------------------------------------------------------------
     * Inizializzazioni necessarie a fronte di ricerca valori su ogni proggramma chiamante
     * ------------------------------------------------------------------------------------
     * 
     */
	private void initialIlpwForNewCaller(LogicWorkProcess ilpw) {
		ilpw.al_DynamicFieldSubWrk = new ArrayList<LogicDynamicFieldSub>();
		
	}

	/*
     * -----------------------------------------------------------
     * Aggiorna i valori del sottocampo del programma origine
     * -----------------------------------------------------------
     * 
     * Si utilizzano le informazioni dei valori individuati in un programma
     * chiamante per un sottocampo con tutte le informazioni di valorizzazione.
     * Lo scopo è rendere trasparente la successiva composizione del campo
     * dinamico nel programma origine, arricchita con i valori individuati in un 
     * generico programma chiamante per un suo specifico sottocampo.
     * Si utilizzano, successivamente, esattamente gli stessi metodi di
     * composizione dei valori finali, utilizzati per le logiche di
     * programma local stesso programma.
     * 
     */
    private void updateValuesInSubFieldOrigin(LogicWorkProcess ilpw) {

    	LogicDynamicField logicFieldOrigin = null;              				// Istruzione nel programma origine
    	LogicDynamicFieldSub logicFieldSubOrigin = null;        				// Istruzione nel programma origine
    	LogicDynamicValue logicDynamicFieldSubValueOrigin = null;          		//

    	LogicDynamicFieldSubSetting logicDynamicFieldSubLastSetCaller = null;	//
    	LogicDynamicFieldSubSetting logicDynamicFieldSubSettingWrk = null;		//
    	EntityDynamicFieldSubSetting entityDynamicFieldSubOriginWrk = null;			//
    	EntityDynamicFieldSubSetting entityDynamicFieldSubLastSetCaller = null;	//
    	EntityDynamicFieldSubValue entityDynamicFieldSubValueWrk = null;		//
       	int numChainCaller = 0;
       	int numChainOrigin = 0;
    	int progr = 0;															// Progressivo valore 
     	int idxLastValue = 0;
     	int i = 0;
    	
     	// Campo, sottocampo, ultima assegnazione nel programma origine
      	logicFieldOrigin = ilpw.logicDynamicInstructionOrigin.al_dynamicField.get((ilpw.logicDynamicInstructionOriginNumField));
     	logicFieldSubOrigin = logicFieldOrigin.al_FieldSub.get(ilpw.logicDynamicInstructionOriginNumFieldSub);

     	numChainOrigin = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getNumChain();
     	 
     	// Ultima assegnazione in programma chiamante che ha generato valori
    	i = ilpw.dynamicFieldSubToSolve.al_lastSetTotal.size() - 1;
     	logicDynamicFieldSubLastSetCaller = ilpw.dynamicFieldSubToSolve.al_lastSetTotal.get(i);
     	entityDynamicFieldSubLastSetCaller = logicDynamicFieldSubLastSetCaller.entityDynamicFieldSetting;
     	    	
    	// Accodamento assegnazioni correnti individuate in programma caller corrente in assegnazioni di programma origine
    	numChainCaller = entityDynamicFieldSubLastSetCaller.getNumChain();
    	for (LogicDynamicFieldSubSetting logicFieldSubSettingCaller : ilpw.dynamicFieldSubToSolve.al_al_chainSetSubField.get(numChainCaller)) {
    		
    		// Generazione nuova impostazione su db da inserire nel programma origine
    		entityDynamicFieldSubOriginWrk = setSettingOriginFromCaller(ilpw, logicFieldSubSettingCaller.entityDynamicFieldSetting, logicFieldSubOrigin);
    		
    		// Accodo la trasformazione come se fosse stata generata nel programma origine
    		logicDynamicFieldSubSettingWrk = new LogicDynamicFieldSubSetting(logicFieldSubOrigin.entityDynamicFieldSub.getSystem(), logicFieldSubOrigin.entityDynamicFieldSub.getSubSystem());    		
   		    logicDynamicFieldSubSettingWrk.entityDynamicFieldSetting = entityDynamicFieldSubOriginWrk;    		
    		logicFieldSubOrigin.al_al_chainSetSubField.get(numChainOrigin).add(logicDynamicFieldSubSettingWrk);
		}

     	// Individuo l'ultimo progressivo valore per sottocampo in struttura origine
      	idxLastValue = logicFieldSubOrigin.al_value.size() - 1;
    	if (idxLastValue >= 0) {
    		progr = logicFieldSubOrigin.al_value.get(idxLastValue).entityDynamicValue.getProgr();
		}
        
		// Valori finali codificati del sottocampo individuati nel programma chiamante corrente
    	for (LogicDynamicValue dynamicFieldSubValueCaller : ilpw.dynamicFieldSubToSolve.al_value) {
    		
    		// Generazione nuovo valore su db da inserire nel programma origine
    		entityDynamicFieldSubValueWrk= setValueOriginFromCaller(ilpw, dynamicFieldSubValueCaller.entityDynamicValue, logicFieldSubOrigin, ++progr);
    		
        	// Accodo il valore come se fosse stato generato nel programma origine
       		logicDynamicFieldSubValueOrigin = new LogicDynamicValue(logicFieldSubOrigin.entityDynamicFieldSub.getSystem(), logicFieldSubOrigin.entityDynamicFieldSub.getSubSystem());
    		logicDynamicFieldSubValueOrigin.entityDynamicValue = entityDynamicFieldSubValueWrk;
    		logicFieldSubOrigin.al_value.add(logicDynamicFieldSubValueOrigin);
		}
    	
    	// Valori stringa finali sottocampo in struttura programma origine
    	for (String stringValue : ilpw.dynamicFieldSubToSolve.al_valueComplete) {
    		logicFieldSubOrigin.al_valueComplete.add(stringValue);
		} 
    	
	}
 
    /* --------------------------------------------------------------------------------
     * Imposta entityDynamicFieldSubValue origin da entityDynamicFieldSubValue caller
     * --------------------------------------------------------------------------------
     * 
     */
    private EntityDynamicFieldSubValue setValueOriginFromCaller(LogicWorkProcess ilpw
							    		, EntityDynamicFieldSubValue entityDynamicValueCaller
							    		, LogicDynamicFieldSub logicFieldSubOrigin
							    		, int progr
							    		 ) {
   		
    	EntityDynamicFieldSubValue entityDynamicValueOrigin = null;
    	entityDynamicValueOrigin = new EntityDynamicFieldSubValue(); 	
    	
    	// Primary key
      	entityDynamicValueOrigin.setSystem(logicFieldSubOrigin.entityDynamicFieldSub.getSystem());
      	entityDynamicValueOrigin.setSubSystem(logicFieldSubOrigin.entityDynamicFieldSub.getSubSystem());
      	entityDynamicValueOrigin.setIdObject(logicFieldSubOrigin.entityDynamicFieldSub.getIdObject());   
     	entityDynamicValueOrigin.setTypeObject(logicFieldSubOrigin.entityDynamicFieldSub.getTypeObject());
     	entityDynamicValueOrigin.setNumInstr(logicFieldSubOrigin.entityDynamicFieldSub.getNumInstr());
     	entityDynamicValueOrigin.setIdField(logicFieldSubOrigin.entityDynamicFieldSub.getIdField());
     	entityDynamicValueOrigin.setIdSubField(logicFieldSubOrigin.entityDynamicFieldSub.getIdSubField());
     	entityDynamicValueOrigin.setProgr(progr);
     	
    	// Data
    	entityDynamicValueOrigin.setDefaultValue(entityDynamicValueCaller.getDefaultValue());
    	
    	// Porzione di sottocampo oggetto dell'impostazione 
   	    entityDynamicValueOrigin.setPosInSubField(entityDynamicValueCaller.getPosInSubField());
    	entityDynamicValueOrigin.setLngInSubField(entityDynamicValueCaller.getLngInSubField());
    	entityDynamicValueOrigin.setValue(entityDynamicValueCaller.getValue());
 
    	// Origine valore sottocampo
       	entityDynamicValueOrigin.setTypeObjectFrom(entityDynamicValueCaller.getTypeObject());
       	entityDynamicValueOrigin.setIdObjectFrom(entityDynamicValueCaller.getIdObject());
       	entityDynamicValueOrigin.setNumInstrFrom(entityDynamicValueCaller.getNumInstr());
       	entityDynamicValueOrigin.setIdPgmFrom(entityDynamicValueCaller.getIdObject());

       	return entityDynamicValueOrigin;
 	}

    
	/* --------------------------------------------------------------------------
     * Imposta entityDynamicSetting origin da entityDynamicSetting caller
     * --------------------------------------------------------------------------- 
     */
   	private EntityDynamicFieldSubSetting setSettingOriginFromCaller(LogicWorkProcess ilpw
														   		  , EntityDynamicFieldSubSetting entityFieldSubSettingCaller
														   		  , LogicDynamicFieldSub logicFieldSubOrigin
														   		   ) {

   		EntityDynamicFieldSubSetting entityFieldSubSettingOrigin = null;
   		entityFieldSubSettingOrigin = new EntityDynamicFieldSubSetting(); 	
   		
   		// Primary key
 		entityFieldSubSettingOrigin.setSystem(logicFieldSubOrigin.entityDynamicFieldSub.getSystem());
 		entityFieldSubSettingOrigin.setSubSystem(logicFieldSubOrigin.entityDynamicFieldSub.getSubSystem());
  		entityFieldSubSettingOrigin.setIdObject(logicFieldSubOrigin.entityDynamicFieldSub.getIdObject());           
  		entityFieldSubSettingOrigin.setTypeObject(logicFieldSubOrigin.entityDynamicFieldSub.getTypeObject());       
		entityFieldSubSettingOrigin.setNumInstr(logicFieldSubOrigin.entityDynamicFieldSub.getNumInstr());           
   		entityFieldSubSettingOrigin.setIdField(logicFieldSubOrigin.entityDynamicFieldSub.getIdField());             
  		entityFieldSubSettingOrigin.setIdSubField(logicFieldSubOrigin.entityDynamicFieldSub.getIdSubField());       
 		entityFieldSubSettingOrigin.setIdPgmSet(entityFieldSubSettingCaller.getIdPgmSet());
 		entityFieldSubSettingOrigin.setNumChain(entityFieldSubSettingCaller.getNumChain());
  		entityFieldSubSettingOrigin.setProgr(entityFieldSubSettingCaller.getProgr());
		entityFieldSubSettingOrigin.setNumInstrSet(entityFieldSubSettingCaller.getNumInstrSet());
		
		// Data
   		entityFieldSubSettingOrigin.setNumField(entityFieldSubSettingCaller.getNumField());
		entityFieldSubSettingOrigin.setNumSubField(entityFieldSubSettingCaller.getNumSubField());
		
		// Porzione di sottocampo oggetto dell'impostazione 		
   		entityFieldSubSettingOrigin.setPosInSubField(entityFieldSubSettingCaller.getPosInSubField());
  		entityFieldSubSettingOrigin.setLngInSubField(entityFieldSubSettingCaller.getLngInSubField());
  		
  		// Informazioni di impostazione, programma, path e modalità di assegnazione e valore se disponibile (literal, campo di tabella)
  		entityFieldSubSettingOrigin.setNumInstrOriginSpreaded(entityFieldSubSettingCaller.getNumInstrOriginSpreaded());
 		entityFieldSubSettingOrigin.setIdPgmSet(entityFieldSubSettingCaller.getIdPgmSet());
 		entityFieldSubSettingOrigin.setTypeObjectPgmSet(entityFieldSubSettingCaller.getTypeObjectPgmSet());
		entityFieldSubSettingOrigin.setNumPath(entityFieldSubSettingCaller.getNumPath());
    	entityFieldSubSettingOrigin.setSetMode(entityFieldSubSettingCaller.getSetMode());
   		entityFieldSubSettingOrigin.setNumUsingParm(entityFieldSubSettingCaller.getNumUsingParm());
  		entityFieldSubSettingOrigin.setDspFieldInUsingParm(entityFieldSubSettingCaller.getDspFieldInUsingParm());
 		entityFieldSubSettingOrigin.setDspFieldInLinkageArea(entityFieldSubSettingCaller.getDspFieldInLinkageArea());
 		entityFieldSubSettingOrigin.setTypePointerArea(entityFieldSubSettingCaller.getTypePointerArea());
  		entityFieldSubSettingOrigin.setDspPointerInLinkageArea(entityFieldSubSettingCaller.getDspPointerInLinkageArea());
   		entityFieldSubSettingOrigin.setNumUsingParmPointer(entityFieldSubSettingCaller.getNumUsingParmPointer());
		entityFieldSubSettingOrigin.setDspPointerInUsingParm(entityFieldSubSettingCaller.getDspPointerInUsingParm());
  		entityFieldSubSettingOrigin.setValue(entityFieldSubSettingCaller.getValue());
		 		
  		// Campo input in assegnazione di trasformazione o campo di ultima assegnazione (di Linkage, ioarea file etc.)
  		// Posizione e lunghezza per il sender sono quelli espressi da reference modification (posSnd:lngSnd) se indicato.
  	    // Posizione e lunghezza sono sempre valorizzati e se non presenti sono inizializzati (1:size(campo sender))
 		entityFieldSubSettingOrigin.setFieldSenderId(entityFieldSubSettingCaller.getFieldSenderId());
 		entityFieldSubSettingOrigin.setFieldSenderNum(entityFieldSubSettingCaller.getFieldSenderNum());
   		entityFieldSubSettingOrigin.setFieldSenderPos(entityFieldSubSettingCaller.getFieldSenderPos());
 		entityFieldSubSettingOrigin.setFieldSenderLng(entityFieldSubSettingCaller.getFieldSenderNum());
		  		
 	    // Campo output in assegnazione di trasformazione o campo receiver senza trasformazioni. 
 	    // La posizione è quella iniziale interessata alla trasformazione.
 	    // La lunghezza è quella del sottocampo origine di cui trovare i valori.
 	    // Posizione e lunghezza sono sempre valorizzati ed inizializzati nel processo a 1, size(campo receiver)
 	    // Se l'istruzione Move che ha generato l'assegnazione contiene anche reference modification (pos:lng), l'informazione
 	    // è utilizzata solo per determinare se il receiver è influenzato dalla trasformazione, ma NON viene memorizzata.
		entityFieldSubSettingOrigin.setFieldReceiverId(entityFieldSubSettingCaller.getFieldReceiverId());
   		entityFieldSubSettingOrigin.setFieldReceiverNum(entityFieldSubSettingCaller.getFieldReceiverNum());
  		entityFieldSubSettingOrigin.setFieldReceiverPos(entityFieldSubSettingCaller.getFieldReceiverPos());
  	   	entityFieldSubSettingOrigin.setFieldReceiverLng(entityFieldSubSettingCaller.getFieldReceiverLng());
 		 	
  		// Oggetto alla cui ioarea appartiene il campo ultima trasformazione, di cui trovare i valori esternamente (prima assegnazione nella catena)
   		entityFieldSubSettingOrigin.setIdObjExt(entityFieldSubSettingCaller.getIdObjExt());
		entityFieldSubSettingOrigin.setTypeObjExt(entityFieldSubSettingCaller.getTypeObjExt());
		entityFieldSubSettingOrigin.setObjExtSqlTableColumn(entityFieldSubSettingCaller.getObjExtSqlTableColumn());
  		entityFieldSubSettingOrigin.setObjExtSystem(entityFieldSubSettingCaller.getObjExtSystem());
  		entityFieldSubSettingOrigin.setObjExtColField(entityFieldSubSettingCaller.getObjExtColField());
 		entityFieldSubSettingOrigin.setObjExtIdCopy(entityFieldSubSettingCaller.getObjExtIdCopy());
		entityFieldSubSettingOrigin.setObjExtPosCol(entityFieldSubSettingCaller.getObjExtPosCol());
		entityFieldSubSettingOrigin.setObjExtLengthCol(entityFieldSubSettingCaller.getObjExtLengthCol());
		
		// Indicatori di soluzione e di valori disponibili
        entityFieldSubSettingOrigin.setSolvedObjExt(entityFieldSubSettingCaller.getSolvedObjExt());
        entityFieldSubSettingOrigin.setWaitingForExternalData(entityFieldSubSettingCaller.getWaitingForExternalData());	
        
        return entityFieldSubSettingOrigin;
   	}

	/* ----------------------entityFieldSubSettingOrigin---------------------------------------------
    * Restituisce true se ultima assegnazione in attesa di dati esterni
    * --------------------------------------------------------------------
    * 
    * L0ultima assegnazione fornita in input NON è ancora stata risolta.
    * 
    */
   	private boolean isLastSetWaitingForData(LogicDynamicFieldSubSetting lastSet) {

   		if (lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_READ
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_SQL_SELECT	
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_FILE_GET
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRMID
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_EIBTRNID		
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_VSAM	
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TS_QUEUE	
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_READ_TD_QUEUE	
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_RECEIVE_MAP) {
   			return true;
   		}

   		return false;
   	}

  	/* --------------------------------------------------------------------
    * Restituisce true se ultima assegnazione in attesa di dati esterni
    * --------------------------------------------------------------------
    * 
    * L0ultima assegnazione fornita in input NON è ancora stata risolta.
    * 
    */
   	private boolean isLastSetToBeSolvedInCaller(LogicDynamicFieldSubSetting lastSet) {

   		if (lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM
   		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_TWA
  		||  lastSet.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_CSA) {
   			return true;
   		}

   		return false;
   	}

	/*
     * ----------------------------------------------------------------------------
     * Restituisce i nuovi campi nel programma chiamante, di cui cercare i valori.
     * ----------------------------------------------------------------------------
     * 
     * A fronte di un ultima assegnazione in un programma chiamato si possono individuare
     * + campi nel programmma chiamante candidati ad essere valorizzati.
     * 
     * Per esempio una commarea può essere ridefinita + volte, indivduando campi con
     * nomi diversi, alla stessa posizione che l'ultima assegnazione stabilisce.
     * Ogni nuovo campo deve essere un parametro o sottoparametro di una call using
     * oppure del parametro Commarea() di una Cics Xctl/Link o, ancora un
     * campo di Cics Twa o Cics Csa. 
     * 
     * Esempio
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
	 *   dovrà cercare valori fra inizio programma e CALL per i campi:
	 *   
	 *   1) CAMPO2 da posizione 1
	 *   2) PARM1-USING da posizione 2
	 *   
	 *   
     * Se non si individua un campo in grado di ospitare il campo passato al
     * programma chiamato, si restituisce null.
     * 
     * Dal programma chiamato arriva l'informazione della posizione nel campo
     * 
     * Determina se il programma chiamante non modifica il nuovo campo
     * ma questi è gra i parametri di procedure Usung o di commarea Cics,
     * con necessità di cercare nei programmi chiamanti.
     * 
	 * Imposta tutte le informazioni disponibili su programma chiamante e il
	 * punto di chiamata e campo.
	 * 
	 * Le informazioni sul campo nel pgm chiamato sono in ilpw.lastSetSpreaded
	 * Il programma chiamante è in ilpw.callerSpreaded.programCaller
	 * Le informazioni sull'origine nel chiamante sono in ilpw.relationOriginSpreaded
	 * Le informazioni vengono memorizzate in ilpw.callerSpreaded
	 * 
     */
	private ArrayList<DataItemCobolIdentifier> detectNewFieldsOnProgramCaller(LogicWorkProcess ilpw) {
		
		ArrayList<DataItemCobolIdentifier> al_newField = null;                  // Nuovi campi nel programma chiamante candidati
		DataItemCobolIdentifier newField = null;                                // Nuovo campo di cui cercare i valori nel programma chiamante
		int numInstr;															// Numero istruzione procedure division
 				
		// Entry con istruzione di call/link/xctl e numero istruzione origine
		numInstr = ilpw.callerOrigin.numInstr;
		ilpw.callerOrigin.entryInstr =  ilpw.callerOrigin.programCaller.entryProcedure(numInstr);
		
		// Informazioni sul campo da cercare nel chiamante, a fronte di Call Using parm1, ... parmn
		if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_USING_PARM) {
			al_newField = getNewFieldOnProgramCallerByUsing(ilpw);
			
   		// Informazioni sul campo da cercare nel chiamante, a fronte di Link/Xctl Commarea()
		} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_DFHCOMMAREA) {
			al_newField = getNewFieldOnProgramCallerByDfhcommarea(ilpw);
			
		// Informazioni sul campo da cercare nel chiamante, a fronte di passaggio parametri in cics Twa
   		} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_TWA) {
   			al_newField = getNewFieldOnProgramCallerByCicsTwaCsa(ilpw, "TWA");
   		
   		// Informazioni sul campo da cercare nel chiamante, a fronte di passaggio parametri in cics Csa
   		} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_CICS_CSA) {
   			al_newField = getNewFieldOnProgramCallerByCicsTwaCsa(ilpw, "CSA");
   		
   		// Informazioni sul campo da cercare nel chiamante, a fronte di generica area di linkage indirizzata da pointers
		} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getSetMode() == EnumLogicSetMode.LAST_SET_BY_COBOL_LINKAGE) {
			al_newField = getNewFieldOnProgramCallerByLinkage(ilpw);
		}
		
		return al_newField;
	}




	/* ------------------------------------------------------------------------------
     * Restituisce il parametro di call using nel chiamante di cui trovare i valori
     * ------------------------------------------------------------------------------
     * 
     * Solo se l'ultima assegnazione era LAST_SET_USING_PARM
     * Viene restituita una ArrayList con gli identificatore completodei campi individuati.
     * 
     */
	private ArrayList<DataItemCobolIdentifier> getNewFieldOnProgramCallerByUsing(LogicWorkProcess ilpw) {

		InstructionCobolProcedure instrCobolProc = null;						// Istruzione origine cobol
		ArrayList<DataItemCobolIdentifier> al_newFieldFound = null;             // 
		ArrayList<DataItemCobolIdentifier> al_identifierCallerParmUsing = null;	// Identificatori campi parametri Using
		DataItemCobolIdentifier identifierCallerParmUsing = null;				// Identificatore cobol parametro using
		int usingCalledNumParm = 0;												// Numero parametro using in procedure division using chiamato
	    int usingCalledDspParm = 0;											    // Displacement campo in parametro using 
		
		
		// Numero parametro using in Procedure Division Using del programma chiamato e displacement campo
		usingCalledNumParm = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParm();
		usingCalledDspParm = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInUsingParm();
		
		// L'struzione NON è cobol di procedure ma una probabile exec cics
		if (!(ilpw.callerOrigin.entryInstr.getInstruction() instanceof InstructionCobolProcedure)) {
			return null;
		}
		
		// Parametri call using in programma chiamante
		instrCobolProc = (InstructionCobolProcedure) ilpw.callerOrigin.entryInstr.getInstruction();
		
		// L'istruzione non è una Call Using
		if (instrCobolProc.getTypeInstr() != EnumCobolReservedWords.PROC_CALL
		|| !instrCobolProc.callIsUsingParms()) {
			return null;
		}
		
		// Parametri using
		al_identifierCallerParmUsing = instrCobolProc.callGetUsingParms();			 		 // Identificatori completi parametri call using
 		
		// Call con numero parametri insufficienti
		if (al_identifierCallerParmUsing.size() < usingCalledNumParm) {
			return new ArrayList<DataItemCobolIdentifier>();
		}
		
		// // Identificatore completo Parametro call using da trattare
		identifierCallerParmUsing = al_identifierCallerParmUsing.get(usingCalledNumParm - 1);
 		
 		// Estrazione generalizzata di tutti i campi al displacement di ultima assegnazione
		al_newFieldFound = getNewFieldsOnAreas(ilpw, identifierCallerParmUsing, usingCalledDspParm);

		return al_newFieldFound;
	}


    /* ------------------------------------------------------------------------ 
     * Restituisce il campo passato in TWA o CSA  
     * ------------------------------------------------------------------------- 
     * 
     * A fronte di assegnazioni di campi riconducibili a TWA o CSA
     * Viene restituita una ArrayList con gli identificatore completodei campi individuati.
     * 
     * 1) Si cercano le EXEC CICS ADDRESS TWA(pointer)
     * 2) Si cercano le EXEC CICS ADDRESS SET(Address of Area) USING pointer)
     * 3) Si cercano le SET ADDRESS OF Area TO pointer
     * 4) Si intabellano le aree individuate da 1) 2) 3)
     * 5) Per ogni area si individua, se esiste, il campo alla posizione richiesta
     * 6) Si restituiscono i campi individuati al chiamante (al momento solo il primo)
     * 
     * Ottimizzazione
     * 
     * Si utilizzano nell'area di controllo tre SET di numeri istruzione
     *        
     * 1) set_setCicsAddressTwaCsa
     * 2) set_setCicsAddressSet
     * 3) set_setCobolAddressOf
     * 
     */
    private ArrayList<DataItemCobolIdentifier> getNewFieldOnProgramCallerByCicsTwaCsa(LogicWorkProcess ilpw, String areaTwaCsa) {
		
		InstructionCobolProcedure instrCobol = null;
		InstructionCics instrPrecompilerAddress = null;
		InstructionCics instrPrecompilerSet = null;
		DataItemCobolIdentifier identifierUsing = null;;
		DataItemCobolIdentifier identifierArea = null;
		ArrayList<DataItemCobolIdentifier> al_areaAddressedWork = null;
		ArrayList<DataItemCobolIdentifier> al_areaAddressed = null;
		ArrayList<DataItemCobolIdentifier> al_newFieldFound = null;
		ArrayList<DataItemCobolIdentifier> al_newFieldFoundPartial = null;
		int dspInArea = 0;                                                      //
		boolean isAreaInserted = false;
		
		al_areaAddressed = new ArrayList<DataItemCobolIdentifier> ();
		
		// Scan istruzioni EXEC CICS ADDRESS TWA|CSA presenti nel programma
		// Intabellamento di tutte le possibili aree indirizzate in TWA/TSA
		for (int numInstrAddress : ilpw.set_setCicsAddressTwaCsa) {
			
			instrPrecompilerAddress = (InstructionCics) ilpw.programCur.entryProcedure(numInstrAddress).getInstruction();
			
			// Considero la TWA o la CSA come da parametro in input
			if (!instrPrecompilerAddress.isThereOperand(areaTwaCsa)) {continue;}
			
			// Scan EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)
			for (int i : ilpw.set_setCicsAddressSet) {
				instrPrecompilerSet = (InstructionCics) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrPrecompilerSet.getOperand("USING");
				identifierArea = instrPrecompilerSet.getOperand("SET");
				
				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getOperand(areaTwaCsa).getNumInstr()) {
					al_areaAddressed.add(identifierArea);
				}
			} // end-for EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)

			// Scan SET ADDRESS OF Area TO Pointer
			for (int i : ilpw.set_setCobolAddressOf) {
				
				instrCobol =  (InstructionCobolProcedure) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrCobol.setGetPointer();
				identifierArea = instrCobol.setGetAreaAddressed();
				
				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getNumInstr()) {
					al_areaAddressed.add(identifierArea);
				}
			} // end-for SET ADDRESS OF Area TO Pointer
			
		} // end-for EXEC CICS ADDRESS TWA|CSA 
		
		
		// Non è presente nessuna area mappata in TWA/CSA: campo non individuato
		if (al_areaAddressed.size() == 0) {
			return null;
		}
		
		// Eliminazione duplicati aree
		al_areaAddressedWork = new ArrayList<DataItemCobolIdentifier> ();
        for (DataItemCobolIdentifier areaAddressed : al_areaAddressed) {
        	isAreaInserted = false;
			for (DataItemCobolIdentifier areaAddressedWork : al_areaAddressedWork) {
				if (areaAddressedWork.getNameIdentifier().equals(areaAddressed.getNameIdentifier())) {
					isAreaInserted = true;
					break;
				}
			}
			if (!isAreaInserted) {
				al_areaAddressedWork.add(areaAddressed);
			}
		}
        al_areaAddressed = al_areaAddressedWork;
        
		// Scan aree TWA/CSA candidate 
		al_newFieldFound = new ArrayList<DataItemCobolIdentifier> ();
		for (DataItemCobolIdentifier areaAddressed : al_areaAddressed) {

			// Estrazione generalizzata di tutti i campi al displacement di ultima assegnazione
			dspInArea = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
			al_newFieldFoundPartial = getNewFieldsOnAreas(ilpw, areaAddressed, dspInArea);
			al_newFieldFound.addAll(al_newFieldFoundPartial);
		}
		
		al_newFieldFound = ilpw.callerOrigin.al_dataItemCallerToSolve;

		return al_newFieldFound;
	}


	
    /* ------------------------------------------------------------------------ 
     * Restituisce il campo passato in una generica area di linkage  
     * ------------------------------------------------------------------------- 
     * 
     * Nel programma chiamato deve essere risolto un campo di Linkage section di
     * un area che deve associarsi a un'area di WORKING nel programma chiamante.
     * Nel programma chiamato l'area viene indirizzata con: 
     * 
     *   SET ADDRESS OF LNK-AREA TO Ptr
     * 
     * Dove Ptr è un POINTER che può essere passato in un parametro Using|DFCOMMAREA|TWA|CSA
     * 
     * Nel programma chiamante il pointer con l'indirizzo dell'area di working, poi passata al chiamato
     * in Using|DFCOMMAREA|TWA|CSA, si valorizza con:
     * 
     *   SET ptr TO ADDRESS OF WRK-AREA 
     * 
     * 1) Si recupera il nome del pointer da cercare da Using|DFCOMMAREA|TWA|CSA
     * 2) Si individuano i possibili pointer che indirizzano l'area di working
     *    cercando le SET pointer TO ADDRESS OF area
     * 3) Si intabellano le aree individuate da 2) indirizzate dai pointer di 1) 
     * 4) Per ogni area si individua, se esistono, i campi alla posizione richiesta
      * 
     * Ottimizzazione
     * 
     * Si utilizza set_setCobolToAddressOf con i numeri istruzione SET 
     *        
     * 
     */
	private ArrayList<DataItemCobolIdentifier> getNewFieldOnProgramCallerByLinkage(LogicWorkProcess ilpw) {

		InstructionCobolProcedure instrCobol = null;
		InstructionCobolDataItem dataItemPointer = null;
		DataItemCobolIdentifier identifierPointer = null;
		DataItemCobolIdentifier identifierAreaAddressed = null;
		ArrayList<DataItemCobolIdentifier> al_areaWorking = null;
		ArrayList<InstructionCobolDataItem> al_areaPointerIntoWichSearch = null;
		ArrayList<InstructionCobolDataItem> al_pointerFound = null;
		ArrayList<DataItemCobolIdentifier> al_newFieldFound = null;
		ArrayList<DataItemCobolIdentifier> al_newFieldFoundWork= null;
		
		// Per individuazione ultima (prima) trasformazione pointer da area indicata in last set

		int areaPointerPos = 0;
		int dspInArea = 0;                                                         //

		
		// Allocazione preliminari
		al_areaWorking = new ArrayList<DataItemCobolIdentifier> ();
		al_pointerFound = new ArrayList<InstructionCobolDataItem> ();
		
        
		/////////////////////////////////////////////////////////////////////////////////////////////////
		// (1) Individuazione aree in cui cercare i pointers a fronte di ultima assegnazione da risolvere
		/////////////////////////////////////////////////////////////////////////////////////////////////
		  
        // POINTER_INSIDE_USING_PARM
		if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_USING_PARM) {
			al_areaPointerIntoWichSearch = getAreaOfUsingParm(ilpw, ilpw.callerOrigin.numInstr, ilpw.lastSetSpreaded.entityDynamicFieldSetting.getNumUsingParmPointer());
			if (al_areaPointerIntoWichSearch.size() == 0) {
				return null;
			}
			areaPointerPos = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInUsingParm() + 1;
			
		// POINTER_INSIDE_CICS_TWA
		} else if ((ilpw.lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_CICS_TWA)) {
			al_areaPointerIntoWichSearch = getAreasCicsTwaCsa(ilpw, "TWA");
			if (al_areaPointerIntoWichSearch.size() == 0) {
				return null;
			}
			areaPointerPos = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea() + 1;
			
		// POINTER_INSIDE_CICS_CSA
     	} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_CICS_CSA) {
			al_areaPointerIntoWichSearch = getAreasCicsTwaCsa(ilpw, "CSA");
			if (al_areaPointerIntoWichSearch.size() == 0) {
				return null;
			}
			areaPointerPos = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea() + 1;
			
		// POINTER_INSIDE_CICS_DFHCOMMAREA
		} else if (ilpw.lastSetSpreaded.entityDynamicFieldSetting.getTypePointerArea() == EnumLogicSetPointerArea.POINTER_INSIDE_CICS_DFHCOMMAREA) {
			al_areaPointerIntoWichSearch = getAreasCicsDfhcommarea(ilpw);
			if (al_areaPointerIntoWichSearch.size() == 0) {
				return null;
			}
			areaPointerPos = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspPointerInLinkageArea() + 1;
		}
		
		
		/////////////////////////////////////////////////////////////////////////////////////////////////
		// (2) Individuazione pointers in aree individuate alla posizione di ultima assegnazione
		/////////////////////////////////////////////////////////////////////////////////////////////////

		// TODO gestire in modo semplificato cercand SET pointer TO ADDRESS OF area come fatto in LogicSamePgm
		 		
		////////////////////////////////////////////////////////////////////////////
		// (2) Estrazione aree indirizzate dai pointers individuati
		////////////////////////////////////////////////////////////////////////////
		
		// Scan SET pointer TO ADDRESS OF Area presenti nel programma
		// Intabellamento di tutte le possibili aree indirizzate dal pointer corretto
		for (int numInstrAddress : ilpw.set_setCobolToAddressOf) {
			
			instrCobol =  (InstructionCobolProcedure) ilpw.programCur.entryProcedure(numInstrAddress).getInstruction();
			identifierPointer = instrCobol.setGetPointer();
			
			// E' il pointer che interessa
			if (!identifierPointer.getDataItem().getDataName().equals("")) {
				continue;
			}
			
			
			dataItemPointer = identifierPointer.getDataItem();
						
			// Estrazione e accodamento area indirizzata 
			identifierAreaAddressed = instrCobol.setGetAreaAddressed();
			al_areaWorking.add(identifierAreaAddressed);
			
		} 
		 
		// Non è possibile individuare nessuna area indirizzata dal pointer passato al chiamato: campi non individuabili in aree di working
		if (al_areaWorking.size() == 0) {
			return al_newFieldFound;
		}

	 
		////////////////////////////////////////////////////////////////////////////
		// (3) Individuazione campi validi in aree individuate
		////////////////////////////////////////////////////////////////////////////
		
		dspInArea = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		al_newFieldFound = new ArrayList<DataItemCobolIdentifier> ();
		
		// Scan aree linkage candidate possibili
		for (DataItemCobolIdentifier areaWorking : al_areaWorking) {

			// Estrazione generalizzata di tutti i campi al displacement di ultima assegnazione
			al_newFieldFoundWork = getNewFieldsOnAreas(ilpw, areaWorking, dspInArea);
		}
		
		al_newFieldFound.addAll(al_newFieldFoundWork);

		return al_newFieldFound;
	}
 

	/* -----------------------------------------------------------
	 * Restituisce tutte le aree mappate sulla TWA o CSA del Cics
	 * -----------------------------------------------------------
	 * 
     * In base al paramtero fornito, 'TWA' o 'CSA' si individuano
     * tutte le aree cobol mappate sulla corrispondente area Cics.
     * Queste aree si trovano in Linkage section e possono assumere qualsiasi
     * nome, anche se normalmente si trovano come 01 TWA e 01 CSA.
     * Tali arre possono essere ridefinite e pertamto vengono portate
     * in output anche tutte le ridefinizioni delle stesse.
     * 
     * 1) Si cercano le EXEC CICS ADDRESS TWA(pointer)
     * 2) Si cercano le EXEC CICS ADDRESS SET(Address of Area) USING pointer
     * 3) Si cercano le SET ADDRESS OF Area TO pointer
     * 4) Si intabellano le aree (campi) individuati da 2) e 3)
     * 
     * Ottimizzazione
     * 
     * Si utilizzano nell'area di controllo tre SET di numeri istruzione
     *        
     * 1) set_setCicsAddressTwaCsa
     * 2) set_setCicsAddressSet
     * 3) set_setCobolAddressOf
     * 
	 */
	private ArrayList<InstructionCobolDataItem> getAreasCicsTwaCsa(LogicWorkProcess ilpw, String areaTwaCsa) {
		
		InstructionCobolProcedure instrCobol = null;
		InstructionCics instrPrecompilerAddress = null;
		InstructionCics instrPrecompilerSet = null;
		DataItemCobolIdentifier identifierUsing = null;;
		DataItemCobolIdentifier identifierArea = null;;
		InstructionCobolDataItem dataItemTwaCsaRed = null;
		ArrayList<InstructionCobolDataItem> al_areaMappedOnTwaCsa = null;
		ArrayList<InstructionCobolDataItem> al_areaMappedOnTwaCsaRed = null;
		int[] ar_numDefTwaCsaRed = null;
		int numDefTwaCsa = 0;
		int numDefTwaCsaRed = 0;

		al_areaMappedOnTwaCsa = new ArrayList<InstructionCobolDataItem> ();
		
		
		// Scan istruzioni EXEC CICS ADDRESS TWA|CSA presenti nel programma
		// Intabellamento di tutte le possibili aree indirizzate in TWA/TSA
		for (int numInstrAddress : ilpw.set_setCicsAddressTwaCsa) {
			
			instrPrecompilerAddress = (InstructionCics) ilpw.programCur.entryProcedure(numInstrAddress).getInstruction();
			
			// Considero la TWA o la CSA come da parametro in input
			if (!instrPrecompilerAddress.isThereOperand(areaTwaCsa)) {continue;}
			
			// Scan EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)
			for (int i : ilpw.set_setCicsAddressSet) {
				instrPrecompilerSet = (InstructionCics) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrPrecompilerSet.getOperand("USING");
				identifierArea = instrPrecompilerSet.getOperand("SET");
				
				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getOperand(areaTwaCsa).getNumInstr()) {
					al_areaMappedOnTwaCsa.add(identifierArea.getDataItem());
				}
			} // end-for EXEC CICS ADDRESS SET(Address Of Area) USING(pointer)

			// Scan SET ADDRESS OF Area TO Pointer
			for (int i : ilpw.set_setCobolAddressOf) {
				
				instrCobol =  (InstructionCobolProcedure) ilpw.programCur.entryProcedure(i).getInstruction();
				identifierUsing = instrCobol.setGetPointer();
				identifierArea = instrCobol.setGetAreaAddressed();
				
				// Area indirizzata in TWA/CSA
				if (identifierUsing.getNumInstr() == instrPrecompilerAddress.getNumInstr()) {
					al_areaMappedOnTwaCsa.add(identifierArea.getDataItem());
				}
			} // end-for SET ADDRESS OF Area TO Pointer
			
		} // end-for EXEC CICS ADDRESS TWA|CSA 

		
		// Scan aree di Twa/Csa individuate e accodamento in output redefines
		al_areaMappedOnTwaCsaRed = new ArrayList<InstructionCobolDataItem> ();
		for (InstructionCobolDataItem dataItemTwaCsa : al_areaMappedOnTwaCsa) {
			numDefTwaCsa = dataItemTwaCsa.getNumInstr();
			// Estraggo eventuali redefines di area individuata direttamente 
			ar_numDefTwaCsaRed = ilpw.programCur.dataItemsRedefinePointers(numDefTwaCsa);
			if (ar_numDefTwaCsaRed != null) {
				// Scan redefines
				for (int i : ar_numDefTwaCsaRed) {
					numDefTwaCsaRed = i;
					dataItemTwaCsaRed = ilpw.programCur.dataItemDefinition(numDefTwaCsaRed);
					al_areaMappedOnTwaCsaRed.add(dataItemTwaCsaRed);
				}
			}
		}
		al_areaMappedOnTwaCsa.addAll(al_areaMappedOnTwaCsaRed);

		return al_areaMappedOnTwaCsa;
	}
 


	/* ------------------------------------------------------------------------ 
     * Restituisce i campi di commarea nel chiamante di cui trovare i valori
     * ------------------------------------------------------------------------- 
     * 
     * A fronte di Exec Cics Link/Xctl con parametro COMMAREA.
     * Viene restituito l'identificatore completo del campo o  null.
     * Nel programma chiamato tale campo è riconducibile alla DFHCOMMAREA Cics.
     * 
     * Il campo parametro COMMAREA può essere espresso da:
     *  - Campo di gruppo con sottocampi 
     *    - Si cerca il sottocampo all'interno della pos richiesta
     *    - Se il sottocampo inizia alla pos richiesta si considera quello
     *    - Se la pos richiesta è all'interno del sottocampo si scarta
     *  - Campo singolo elementare
     *    - si considera come nuovo receiver senza ulteriori modifiche,
     *      alla posizione propagata dal chiamato
     *      
     * Si considerano tutte le ridefinizioni del campo COMMAREA 
     * 
     * Utilizza in input: ilpw.callerSpreaded e ilpw.lastSetSpreaded
     */
	private ArrayList<DataItemCobolIdentifier> getNewFieldOnProgramCallerByDfhcommarea(LogicWorkProcess ilpw) {

		InstructionCics instrCics = null;								// Istruzione origine Cics
		DataItemCobolIdentifier oprndCommarea = null;							// Identificatore cobol Commarea in Link/XcTl
		ArrayList<DataItemCobolIdentifier> al_newFieldFound = null;             // Nuovi campi nel programma chiamante di cui trovare i valori
		int dspInArea = 0;                                                      //
		
		
		// L'struzione origine deve essere un'istruzione di precompilatore
		if (ilpw.callerOrigin.entryInstr.getEntryType() != EnumInstrDataCategory.CICS_PRECOMPILER) {
			return null;
		}
		
		instrCics = (InstructionCics) ilpw.callerOrigin.entryInstr.getInstruction();
		
		// L'istruzione può essere solo di link/xctl
		if (instrCics.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.CICS_INSTR_LINK
		&&  instrCics.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.CICS_INSTR_XCTL) {
			return null;
		}
		
		// La Link/XcTl non ha il parametro commarea: il punto di richiamo nel caller non è un candidato
		oprndCommarea = instrCics.getOperand("COMMAREA");
		if (oprndCommarea == null) {
			return null;
		}
		
		// Estrazione generalizzata di tutti i campi al displacement di ultima assegnazione
		dspInArea = ilpw.lastSetSpreaded.entityDynamicFieldSetting.getDspFieldInLinkageArea();
		al_newFieldFound = getNewFieldsOnAreas(ilpw, oprndCommarea, dspInArea);
		
		return al_newFieldFound;
	}

	
	
	/* ----------------------------------------------------------------------------
	 * Restituzione info su nuovi campi da trattare, fornita un area e displacement
	 * ----------------------------------------------------------------------------
	 * 
	 * Vengono individuate tutte le aree che ridefiniscono quella fornita in input.
	 * Per ogni area ridefinita e per quella fornita, si analizzano i singoli campi 
	 * e vengono accodati e restituiti nella struttura di output, quelli che iniziano 
	 * al displacement fornito in input.
	 * 
	 * Per ogni campo individuato si aggiornano in output:
	 * 
	 * 1) Posizione nell'area
	 * 2) Lunghezza
	 * 3) Posizione mappata nel sottocampo
	 * 4) Lunghezza mappata nel sottocampo
	 * 
	 * Nel caso l'area fornita/ridefinita sia un campo di gruppo e il campo
	 * sia individuato alla posizione aspettata, si considera il nuovo campo
	 * a partire da 1.
	 * 
	 * Nel caso invece l'area fornita/ridefinita sia un campo elementare,
	 * allora si considera come nuovo campo l'area stessa, alla stessa
	 * posizione fornita in input.
	 * 
	 * La lunghezza del campo e la lunghezza/posizione mappata nel sottocampo
	 * origine, sono quelle recuperate dall'ultima assegnazione spreaded e
	 * portate in output senza modifiche.
	 * 
	 */
    private ArrayList<DataItemCobolIdentifier> getNewFieldsOnAreas(LogicWorkProcess ilpw
													    		 , DataItemCobolIdentifier areaIntoSearch
																 , int dspInArea) {

    	
		ProgramCobolEntry<? extends Instruction> entryData = null;				// Entry data division
		ProgramCobolEntry<? extends Instruction> entryArea = null;		        // Entry data division
		InstructionCobolDataItem dataItemGeneric = null;						// Data item generico di servizio
		DataItemCobolIdentifier newField = null;                                // Nuovo campo nel programma chiamante di cui trovare i valori
		ArrayList<DataItemCobolIdentifier> al_areaIntoSearch = null;            //
		ArrayList<DataItemCobolIdentifier> al_newFieldFound = null;             // Nuovi campi nel programma chiamante di cui trovare i valori
		int ar_areaRedefines[] = null;                                          //
		int dspCur = 0;                                                         // Displacement corrente campo in area
		int dspArea = 0;                                                        // Displacement area
		int lvlArea = 0;                                                        // Numero livello area

		al_areaIntoSearch = new ArrayList<DataItemCobolIdentifier> ();
		al_newFieldFound = new ArrayList<DataItemCobolIdentifier> ();

		// Si accoda l'area dichiarata esplicitamente e tutte le sue redefines
		al_areaIntoSearch.add(areaIntoSearch);
		ar_areaRedefines = ilpw.programCur.dataItemsRedefinePointers(areaIntoSearch.getNumInstr());
		for (int i = 0; i < ar_areaRedefines.length; i++) {
			int numInstr = ar_areaRedefines[i];
			entryArea = ilpw.programCur.entryDataDivision(numInstr);
			dataItemGeneric = (InstructionCobolDataItem) entryArea.getInstruction();
			newField = new DataItemCobolIdentifier();
			newField.setDataItem((InstructionCobolDataItem) entryArea.getInstruction());
			newField.setIdentifierType(EnumCobolReservedWords.DATA_DIV_DATA_ITEM);
			newField.setNameIdentifier(dataItemGeneric.getDataName());
			newField.setNumInstr(numInstr);
			al_areaIntoSearch.add(newField);
		}
		
		
		// Scan aree da trattare individuate
		for (DataItemCobolIdentifier identifierArea : al_areaIntoSearch) {

		    // L'area è un campo elementare: sarà il nuovo receiver, alla stessa posizione che si aspetta il chiamato
			if (!identifierArea.getDataItem().isGroupField()) {
				// Caricamento info campo
				al_newFieldFound.add(identifierArea);
				ilpw.callerOrigin.al_dataItemCallerToSolve.add(identifierArea);
				ilpw.callerOrigin.al_dataItemCallerToSolvePos.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverPos());
				ilpw.callerOrigin.al_dataItemCallerToSolveLng.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng());
				ilpw.callerOrigin.al_dataItemCallerToSolvePosInSubField.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField());
				ilpw.callerOrigin.al_dataItemCallerToSolveLngInSubField.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField());
				continue;
			}
			
			// Campo di gruppo: ricerca sottocampo compatibile con la posizione richiesta
			entryArea = ilpw.callerOrigin.programCaller.entriesData()[identifierArea.getDataItem().getNumInstr()];
			lvlArea = identifierArea.getDataItem().getLevelNumber();
			dspArea = entryArea.getPos();                 // 0-based
			
	        // Cerco il campo che inizia al displacement previsto, nella commarea passata dal chiamante al chiamato
	 		
	 		// Scan campi sotto area (inclusa)
	 		for (int i = identifierArea.getDataItem().getNumInstr() + 1; i < ilpw.callerOrigin.programCaller.entriesData().length; i++) {
	 			
				entryData = ilpw.callerOrigin.programCaller.entryDataDivision(i);

				// Considero solo le definizioni dati
				if (!(entryData.getInstruction() instanceof InstructionCobolDataItem)) {continue;}
				
		        dataItemGeneric = (InstructionCobolDataItem) entryData.getInstruction();

				// Considero solo le istruzioni dichiarative
				if (dataItemGeneric.getLevelNumber() == 88
			    ||  dataItemGeneric.getLevelNumber() == 66) {
					continue;
				}

				// Campo non individuato
				if (dataItemGeneric.getLevelNumber() == 77
				||  dataItemGeneric.getLevelNumber() <=  lvlArea ) {
					break;
				}

				// Considero solo i campi elementari, non di gruppo
				if (dataItemGeneric.isGroupField()) {continue;}
				
				dspCur = entryData.getPos() - dspArea;
				
				// Displacement coincidente: è il campo cercato nei sottocampi dell'area o di una sua redefines
				if (dspCur == dspInArea) {
					ilpw.posRcvCaller = 1;
	 				// Composizione identificatore completo cobol campo
					newField = new DataItemCobolIdentifier();
					newField.setIdentifierType(EnumCobolReservedWords.DATA_DIV_DATA_ITEM);
					newField.setDataItem(dataItemGeneric);
					newField.setNameIdentifier(dataItemGeneric.getDataName());
					newField.setNumInstr(dataItemGeneric.getNumInstr());
					// Caricamento info campo
					al_newFieldFound.add(newField);
					ilpw.callerOrigin.al_dataItemCallerToSolve.add(newField);
					ilpw.callerOrigin.al_dataItemCallerToSolvePos.add(1);				
					ilpw.callerOrigin.al_dataItemCallerToSolveLng.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getFieldReceiverLng());
					ilpw.callerOrigin.al_dataItemCallerToSolvePosInSubField.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getPosInSubField());
					ilpw.callerOrigin.al_dataItemCallerToSolveLngInSubField.add(ilpw.lastSetSpreaded.entityDynamicFieldSetting.getLngInSubField());
					break;
				}
				
				// Campo sicuramente non presente
				if (dspCur > dspInArea) {
					break;
				}
			
			} // end-for sottocampi area

		} // end-for aree candidate
   	
     	return al_newFieldFound;
	}




	/*
	 * Deserializza il programma corrente di ultima assegnazione e aggiorna la struttura di controllo.
	 */
	private ProgramCobol deserializeProgram(String idProgram) throws ExceptionAmrita {

		SystemService ss = null;                  // Gestore generalizzato servizi di sistema
		ProgramCobol pgmCobol = null; 
		Object objUnserialized = null;                      		// Contiene il programma deserializzato
		
		// Get program
		pgmCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
	    if (pgmCobol != null) {
	    	return pgmCobol;
		}			

	    // Programma da deserializzare
 		
		// Get Program or Copy object
		ss = new SystemService(ucfg, null);
		ss.setSourceObjectContainer(idProgram);

		objUnserialized = ss.getSerialized(ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm(), idProgram, SUFFIX_SERIALIZED_PGM);

		// Programma serializzato non trovato, impossibile produrre documentazione: messaggio già fornito
		if (objUnserialized != null && objUnserialized instanceof ProgramCobol) {
			pgmCobol = (ProgramCobol) objUnserialized;

		}
		
		AmritaStartup.cache.put(idProgram, pgmCobol);
		return pgmCobol;			 
		
	}
	
	

	/* -----------------------------------------------------------
	 * Restituisce l'area di commarea Cics definita in Link/Xctl
	 * -----------------------------------------------------------
	 * 
 	 * Restituisce la definizione del campo COMMAREA dell'istruzione
 	 * chiamante, se questa è una Exec Cics Link/Xctl
	 * La commarea potrebbe essere ridefinita e pertanto vengono restituite
 	 * tutte le possibili aree.
	 * Se non definita restituisce un ArrayList vuoto.
	 */
	private ArrayList<InstructionCobolDataItem> getAreasCicsDfhcommarea(LogicWorkProcess ilpw) {

		ProgramCobolEntry<? extends Instruction> entryCicsInstr = null;
		InstructionCics instrCics = null;
		ArrayList<InstructionCobolDataItem> al_areaDfhcommarea = null;
		InstructionCobolDataItem dataItemDfhconmmarea = null;
		InstructionCobolDataItem dataItemDfhconmmareaRed = null;
		int[] ar_numDefDataItemRed = null;
		int numDefDfhcommarea = 0;
		int numDefDfhcommareaRed = 0;
		
		al_areaDfhcommarea = new ArrayList<InstructionCobolDataItem> ();
		
		// Istruzione di call/link/xctl in programma chiamante
		entryCicsInstr = ilpw.programCur.entryProcedure(ilpw.callerOrigin.numInstr);
		
		// Non è una istruzione cics 
		if (!(entryCicsInstr.getInstruction() instanceof InstructionCics)) {
			return al_areaDfhcommarea;
		}
		
		// Non è una exec cics link/xctl
		instrCics = (InstructionCics) entryCicsInstr.getInstruction();
		if (instrCics.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.CICS_INSTR_LINK
		&&  instrCics.getTypeInstrPrecompiler() != EnumPrecompilerReservedWords.CICS_INSTR_XCTL) {
			return al_areaDfhcommarea;
		}
		
		// Non è presente il parametro COMMAREA
		if (!instrCics.isThereOperand("COMMAREA")) {
			return al_areaDfhcommarea;
		}
		
		// Estrazione campo commarea passato al chiamato, dove è stato memorizzato il pointer di
		// indirizzamento dell'area mappata in linkage section
		// Porto in output la Dfhcommarea esplicitata nell'istruzione
		dataItemDfhconmmarea = instrCics.getOperand("COMMAREA").getDataItem();
		numDefDfhcommarea = dataItemDfhconmmarea.getNumInstr();
		al_areaDfhcommarea.add(dataItemDfhconmmarea);
		
		// Porto in output gli eventuali redefines della dfhcommarea
		ar_numDefDataItemRed = ilpw.programCur.dataItemsRedefinePointers(numDefDfhcommarea);
		if (ar_numDefDataItemRed != null) {
			// Scan redefines
			for (int i : ar_numDefDataItemRed) {
				numDefDfhcommareaRed = ar_numDefDataItemRed[i];
				dataItemDfhconmmareaRed = ilpw.programCur.dataItemDefinition(numDefDfhcommareaRed);
				al_areaDfhcommarea.add(dataItemDfhconmmareaRed);
			}
		}
		
		return al_areaDfhcommarea;
	}

	/* ----------------------------------------------------------------------------
	 * Restituisce l'area di un parametro di Procedure Division Using o Call Using
	 * ----------------------------------------------------------------------------
	 * 
	 * Questo metodo viene chiamato nel processo ricorsivo di soluzione istruzioni
	 * dinamiche spreaded, su un programma chiamante, a qualsiasi livello.
	 * 
 	 * Si verifica l'esistenza del parametro (1-based) in procedure division (num instr=0)
 	 * Si verifica l'esistenza del parametro (1-based) in call using  (num instr>0)
 	 * L'indicazione del numero di istruzione è in ilpw.callerOrigin.numInstr
 	 * Il parametro potrebbe essere ridefinito e pertanto vengono restituite
 	 * tutte le possibili aree.
	 * Se non presente restituisce un ArrayList vuoto.
	 */
	private ArrayList<InstructionCobolDataItem> getAreaOfUsingParm(LogicWorkProcess ilpw, int numInstrCall, int numParmUsing) {

		ArrayList<InstructionCobolDataItem> al_areaParmUsing = null;
    	ProgramCobolEntry<? extends Instruction> cobolEntryProcDiv = null;
     	InstructionCobolProcedure instrCobolProcDiv = null;
    	ArrayList<DataItemCobolIdentifier> al_identifierParmUsing = null;
    	InstructionCobolDataItem dataItemParmUsing = null;
    	InstructionCobolDataItem dataItemParmUsingRed = null;
		int[] ar_numDefParmUsingRed = null;
		int numDefParmUsingRed = 0;
   	    int numDefParmUsing = 0;
    	
    	al_areaParmUsing = new ArrayList<InstructionCobolDataItem> ();
    	 
		// Recupero entry 0 e istruzione di procedure division Using 
    	if (numInstrCall == 0) {
        	cobolEntryProcDiv = ilpw.programCur.entryProcedure(0);
        	instrCobolProcDiv = (InstructionCobolProcedure) cobolEntryProcDiv.getInstruction();
        	// Procedure division senza using: return
         	if (!instrCobolProcDiv.procDivIsUsingParms()) {
    			return al_areaParmUsing;
    		}
        	// Estrazione parametri dichiarati in Procedure Division Using Parm1, Parm2, ... , ParmN
      		al_identifierParmUsing = instrCobolProcDiv.procDivGetUsingParms();
		} else {
        	cobolEntryProcDiv = ilpw.programCur.entryProcedure(numInstrCall);
        	if (cobolEntryProcDiv.getInstruction() instanceof InstructionCobolProcedure) {
            	instrCobolProcDiv = (InstructionCobolProcedure) cobolEntryProcDiv.getInstruction();
     			if (instrCobolProcDiv.getTypeInstr() == EnumCobolReservedWords.PROC_CALL) {
     		       	// Call senza using: return
     	         	if (!instrCobolProcDiv.callIsUsingParms()) {
     	    			return al_areaParmUsing;
     	    		}
     	            // Estrazione parametri dichiarati in Call Pgm Using Parm1, Parm2, ... , ParmN
    				al_identifierParmUsing = instrCobolProcDiv.callGetUsingParms();
    			}
			}
 		} 
		
    	// Nè procedure division e ne calll
    	if (al_identifierParmUsing == null) {
			return al_areaParmUsing;
		}
    	
   		// Procedure division using o Call using con parametri insufficienti
   		if (al_identifierParmUsing.size() < numParmUsing) {
   			return al_areaParmUsing;
		}
   		
		// Porto in output il parametro using
		numDefParmUsing = al_identifierParmUsing.get(0).getNumInstr();
		dataItemParmUsing = ilpw.programCur.dataItemDefinition(numDefParmUsing);
		al_areaParmUsing.add(dataItemParmUsing);
		
		// Porto in output gli eventuali redefines del parametro using
		ar_numDefParmUsingRed = ilpw.programCur.dataItemsRedefinePointers(numDefParmUsing);
		if (ar_numDefParmUsingRed != null) {
			// Scan redefines
			for (int i : ar_numDefParmUsingRed) {
				numDefParmUsingRed = ar_numDefParmUsingRed[i];
				dataItemParmUsingRed = ilpw.programCur.dataItemDefinition(numDefParmUsingRed);
				al_areaParmUsing.add(dataItemParmUsingRed);
			}
		}
		
		return al_areaParmUsing;
	}

}





