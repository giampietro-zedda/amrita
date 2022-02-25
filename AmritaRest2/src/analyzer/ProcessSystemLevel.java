package analyzer;
import java.io.File;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import dao.DAOImplMetricValue;
import dao.DAOImplObject;
import dao.IDAOMetricValue;
import dao.IDAOObject;
import entities.EntityMetricValue;
import enums.EnumDirectivesExecution;
//import entities.EntityMetricPgm;
import enums.EnumMessageType;
import enums.EnumMetricsScope;
import enums.EnumMetricsSqualeRating;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import exception.ExceptionAmrita;
import utilities.SystemService;;
/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ProcessSystemLevel
 * </h1>
 * <p>
 * 
 * Questa classe gestisce tutti i processi a livello di systema/sottosistema. <br>
 * Per processo si intende una elaborazione che aggiunge informazioni in termini di oggetti, 
 * relazioni e conoscenza tecnica del programma, a livello di singolo programma e a livello di
 * sistema e sottosistema.<br>
 * <p>
 * Per esempio una elaborazione a livello di sistema è quella che risolve le logiche dinamiche
 * dei singoli programmi, che non possono essere risolte localmente al programma, ma necessitano
 * di eleborazioni di altri programmi del sistema/sottosistema.<br>
 * <p>
 * Quando questa elaborazione viene attivata, le direttive di esecuzione possono individuare un 
 * insieme di programmi da elaborare. Ogni programma viene elaborato solo se risulta nello stato di
 * analizzato senza errori.<br>
 * Se il programma è nello stato corretto, vengono eseguiti per quel programma tutte le elaborazioni
 * previste nelle direttive di esecuzione e, in dettaglio:<br>
 * <p>
 *     <ul>
 *         <li><strong>OPT_PGM_SOLVE_DYNAMIC_SPREADED</strong> </li><br>
 *         Soluzione istruzioni dinamiche con logiche spalmate (spreaded) nei programmi chiamanti/chiamati.
 *         <li><strong>OPT_DETECT_DATA_MODEL</strong> </li><br>
 *         Ricostruzione modello dati da oggetti e relazioni.
 *         <li><strong>OPT_DETECT_INDIRECT_RELATIONSHIPS</strong> </li><br>
 *         Individuazione relazioni indirette.
 *         <li><strong>OPT_ASSIGN_PHISICAL_TO_LOGICAL_FILE</strong> </li><br>
 *         Assegnazione file fisico in relazioni con logic file.
 *         <li><strong>OPT_CRUD_MATRIX_BUILDING</strong> </li><br>
 *         Costruzione matrice CRUD a livello di sistema .
 *         <li><strong>OOPT_EVALUATE_METRICS_SYSTEM</strong> </li><br>
 *         Valutazione metriche di sistema  (elaborazione di quelle di programma).
 *    </ul>
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0 07/04/2011
 * @since 1.0
 * @see ExecutionStarter
 * @see ExecutionShared
 * @see ExecutionDispatcher
*/
public class ProcessSystemLevel extends ExecutionShared implements Runnable, AmritaConstants{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
	// Gestione sources
	private SourceInput si = null;

	// dati necessari per processo a livello programma
	private ProgramCobol programCobol = null;
	private AnalyzerDbInfo adbi = null;
	private AnalyzerCicsInstr acx = null;
	private LogicSamePgm lgm = null;
	private LogicSpreadedPgm lsm = null;
	private LogicInfoDynamic lid = null;
	
	
    // Attenzione !! 
	// le direttive attive sono disponibili nella variabile di istanza DirectivesInfo di, della superclasse
	// valorizzata dal costruttore, che contiene anche tutte le informazioni di esecuzione.
	
	
	

	
	/**
	 * 
	 * Costruttore 
	 * 
	 * I file di pilot con i sources/directories da trattare e con i processi da attivare, 
	 * si trovano nella directory DirPilotAndFilter che permettono di personalizzare i processi di analisi. 
	 * Nei files di pilot sono presenti paths compleeti di sorgenti e directories da analizzare, 
	 * con eventuali commenti e informazioni di filtro.
	 * 
	 * 
	 */
	public ProcessSystemLevel(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
		super(sd, al_di.get(al_di.size()-1));	
		this.al_di = al_di;
 	}
	
	
	
   /**
    * 
    * Attivato a fronte dell'esecuzione di start() sul thread  da {@link _ExecutionLauncherFunction} <br>
    * 
    */
	public void run() {
		
		this.di.excpOccurred = null;
		this.di.isExecutionWithErrors = false;

		try {
			
			execProcess();
			
		} catch (Exception e) {
			// L'esecuzione è in un thread separato, l'ora di fine elaborazione è già stata impostata da execFunction.
			// Nel caso di eccezione di tipo AmritaException, è già stata loggata e lo stack trace prodotto,
			// altrimenti tutto è a cura del chiamante AnalyzeManagerFunction.
			logMessage(EnumMessageType.ERROR_FATAL, "EF0021", e);
			di.excpOccurred = e;
		}
		
	}
	

	/**
	 * 
	 * Funzione eseguita in modo sincrono o come thread separato.<br>
	 * <p>
	 * Questo metodo di ingresso permette di effettuare operazioni iniziali
	 * e finali relativi alla funzione da eseguire.<br>
	 * Per esempiovengono impostati l'ora di inzio, fine ed elapsed di elaborazione.>br>
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.<br>
	 * <p>
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execProcess() throws Exception  {

		this.di.execMsAllStart = System.currentTimeMillis();
		this.di.isExecutionWithErrors = false;
		
		// Soluzione logiche dinamiche spreaded
		if (this.di.optSolveDynamicSpreaded) {
			execPgmSolveDynamicSpreaded();
		}
		
		// Aggregazione metriche a livello di sistema e sottosistema
		if (this.di.optMetricsSystem) {
			execMetricsSystem();
		}
		
 		this.di.execMsAllEnd = System.currentTimeMillis();
		this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
		this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);			
		return;
	}
	
	

	/**
	 * 
	 * Processo eseguito in modo sincrono o come thread separato<br>
	 * <p>
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execPgmSolveDynamicSpreaded() throws Exception {
		
		ArrayList<InnerDescriptorSource> al_ObjectToAnalize = null;
		EnumObject ar_objectTypeGood[] = null;
		Set<EnumObject> set_objectTypeGood = null;
		EnumObjectStatus ar_objectStatusGood[] = null;
		Set<EnumObjectStatus> set_objectStatusGood = null;
		 		
 		// Impostazione tipi oggetti da elaborare
 		set_objectTypeGood = new HashSet<EnumObject> ();
 		for (EnumDirectivesExecution dirExec : this.di.al_objectTypeToProcess) {
 			set_objectTypeGood.add(dirExec.getSourceType().getObjectType());
		}
 		ar_objectTypeGood = new EnumObject[set_objectTypeGood.size()];
 		ar_objectTypeGood = set_objectTypeGood.toArray(ar_objectTypeGood);
		
 		// Impostazione stati oggetti da elaborare
 		set_objectStatusGood = new HashSet<EnumObjectStatus> ();
 		for (EnumDirectivesExecution dirExec : this.di.al_objectInputStatus) {
 			
  			// Richiesti oggetti in tutti gli stati
 			if (dirExec == EnumDirectivesExecution.INPUT_OBJECTS_STATUS_ALL) {
 				
				// Scan valori enumerazione
 				for (EnumObjectStatus objectStatus : EnumObjectStatus.values()) {
 					
 					// Scarto oggetti non analizzabili
 					if (objectStatus == EnumObjectStatus.NOT_ASSIGNED 
 					&& objectStatus == EnumObjectStatus.SOURCE_MEMBER_TYPE_NOT_DETECTED
 					&& objectStatus == EnumObjectStatus.SOURCE_MEMBER_ACQUIRED) {
 	 					continue;
					}
 					set_objectStatusGood.add(objectStatus);
				}
 				break;
			}
			set_objectStatusGood.add(dirExec.getObjectStatus());
		}
 		ar_objectStatusGood = new EnumObjectStatus[set_objectStatusGood.size()];
 		ar_objectStatusGood = set_objectStatusGood.toArray(ar_objectStatusGood);
			
					
		// Estrazione centralizzata oggetti pgm con codice dinamico spreaded ancora da risolvere
		// Nei pgm con logiche dinamiche risolte lo status è OBJECT_ANALYZED_DYNAMIC_SOLVED
 	    al_ObjectToAnalize = this.getObjectsToProcess(ar_objectTypeGood
		                                            , ar_objectStatusGood
	                                                , new EnumObjectOption[] {}
		                                            );		
	    
	    // Pgm di cui risolvere le logiche spreaded individuati  
	    this.di.execTotObjectToProcess = al_ObjectToAnalize.size();
	    logMessage(EnumMessageType.INFORMATION, "MI0130", al_ObjectToAnalize.size()+"");
	    clearExecInfo();   // Info di esecuzione a livello di utente	    
  	    
	    // Scan per attivazione processi specifici per ogni programma
	    for (InnerDescriptorSource infoObjectToAnalyze : al_ObjectToAnalize) {
	    	
	      	// Il sorgente originale non viene più preso in considerazione
	        
	    	// Valori correnti disponibili da direttiva di esecuzione
	    	this.di.subSystemInput = infoObjectToAnalyze.subSys;               // Potrebbe non essere esplicitato nelle direttive
            this.di.isExecutionWithErrors = false;
            this.di.isExecutionWithException = false;
			this.di.execCntObjectProcessed++;
			this.di.curObjectType = infoObjectToAnalyze.objectType;
	    	this.di.curObjectId = infoObjectToAnalyze.idObject;
	    	this.di.execCurTypeObject = infoObjectToAnalyze.objectType;
	    	this.di.execCurIdObject = infoObjectToAnalyze.idObject;
	    	this.di.pgmName = infoObjectToAnalyze.idObject;
	    	this.di.fileNameCurObj = infoObjectToAnalyze.sourceFileName;
	    	this.di.filePathCurObj = infoObjectToAnalyze.sourcePath;
	    	this.di.libraryCodeCurObj = infoObjectToAnalyze.libraryCode;
	    	this.di.libraryPathCurObj = infoObjectToAnalyze.libraryPath;
	    	this.di.en_CurProcessFunction = di.en_CurProcessFunction; 
			this.di.execMsCurStart = System.currentTimeMillis();
			this.di.execMsCurEnd = 0;
			createProcessLog(al_ObjectToAnalize.size());		// ProcessLog e dati ucfg visibili da web service   	
          
			try {
				
				this.di.execCntObjectProcessed++;
				processPgmSolveDynamicSpreaded(infoObjectToAnalyze, si, this.di);    // Elaborazioni schedulate a livello di programma
				this.di.execCntObjectProcessedNoError++;

	            // Required stop execution: force process stop
	            if (this.ucfg.getExecStopRequired()) {
				   break;
				}

			} catch (Exception e) {
				// Potrebbe non esserci una connessione attiva di cui fare rollback per l'utente
				if (this.ucfg.getDbConn() != null) {
//					this.ucfg.getDbConn().rollback();
					DataBaseConnections.releaseConnection(this.ucfg.getDbConn());
				}								
				this.di.excpOccurred = e;
				this.di.isExecutionWithErrors = true;
				this.di.execCntObjectProcessedExcp++;
				this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
				this.di.execMsCurEnd = System.currentTimeMillis();
				this.di.execMsCurElapsed = (int) (this.di.execMsCurEnd - this.di.execMsCurStart);
				this.di.execMsAllElapsed += this.di.execMsCurElapsed;
				this.di.execMsAvg = this.di.execMsAllElapsed / this.di.execCntObjectProcessed;								 
				this.updateStatusCurrentObject(di, EnumObjectStatus.OBJECT_PROCESSED_WITH_EXCEPTION);
                this.logSystemInfoException();           // Informazioni di debug se presenti
                this.di.excpOccurred = null;
	            this.updateProcessLog();                            // ProcessLog e dati ucfg visibili da web service                              
		           
	            // Required stop execution: force process stop
	            if (this.ucfg.getExecStopRequired()) {
				   break;
	            }				   
			}
	    	
	    } // End-for Source
		
		this.logFinalExecutionStatistics();     // Conteggi, elenco oggetti con errori e in exception
		
 		return;

 	}

	
	/**
	 * Aggregazione  metriche di programma<br>
	 * <p>
	 * Vengono raggruppate le metriche a livello di programma per <br>
	 * systema, poi per sottosistema e infine per sistema/sottosistema<br>
	 * <p>
	 * Per ogni livello di raggruppamento vengono totalizzati  tutti i valori <br>
	 * e vengono ricalcolate le medie.<br>
	 * <p>
	 * Parallelamente a ogni raggruppamento viene generato l'elenco di programmi<br>
	 * corrispondente.<br>
	 * <p>
	 * Questo processo può essere eseguito in modo sincrono o come thread separato<br>
	 * <p>
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execMetricsSystem() throws Exception {
		
		// Metriche a livello globale, di tutti i sistemi e sottosistemi
		EntityMetricValue entityMetricGlobal = null;
		ArrayList<String> al_pgmGlobal = null;
		
		// Map su programmi del raggruppamento
		Map<String, ArrayList<String>> hs_systemPgms = null;				// Key=system, Data=ArrayList<String>
		Map<String, ArrayList<String>> hs_subSystemPgms = null;         	// Key=subSystem, Data=ArrayList<String>
		
		// Map su metriche del raggruppamento
		Map<String, EntityMetricValue> hs_systemMetrics = null;					// Key=system, Data=ArrayList<Metrics>
		Map<String, EntityMetricValue> hs_subSystemMetrics = null;      			// Key=subSystem, Data=ArrayList<Metrics>
		
		// Allocazione map per nomi programma
		hs_systemPgms = new HashMap<String, ArrayList<String>> (); 
		hs_subSystemPgms = new HashMap<String, ArrayList<String>> (); 
		
		// Allocazione map per metriche
		hs_systemMetrics = new HashMap<String, EntityMetricValue> (); 
		hs_subSystemMetrics = new HashMap<String, EntityMetricValue> (); 
		
		// Allocazioni per metriche globali
		entityMetricGlobal = new EntityMetricValue ();
		al_pgmGlobal = new ArrayList<String> ();

		try {
			execMetricsSystemGetSystems(hs_systemPgms);  									// Estrazione sistemi e relativi programmi	
			execMetricsSystemGetSubSystems(hs_subSystemPgms); 								// Estrazione  sottosistemi e relativi programmi	
			execMetricsSystemTotBySystems(hs_systemPgms, hs_systemMetrics);					// Totalizzazone metriche X sistemi 
			execMetricsSystemTotBySubSystems(hs_subSystemPgms, hs_subSystemMetrics); 		// Totalizzazone metriche X sottosistemi 
			execMetricsSystemTotGlobal(entityMetricGlobal, al_pgmGlobal); 					// Totalizzazoni globali
			execMetricsSystemUpdateDb(hs_systemPgms, hs_subSystemPgms, hs_systemMetrics, hs_subSystemMetrics, entityMetricGlobal, al_pgmGlobal);			 
		} catch (Exception e) {
			this.di.excpOccurred = e;
			this.di.isExecutionWithErrors = true;
			this.di.execCntObjectProcessedExcp++;
			this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
			this.logApplicationInfoException();		 	// Informazioni applicative
            this.logSystemInfoException();           	// Informazioni di debug se presenti
            this.di.excpOccurred = null;
    		return;

		}
	    	
		this.logFinalExecutionStatistics();     		// Conteggi, elenco oggetti con errori e in exception
		
 		return;

 	}
	


    /* -----------------------------------
     * Popolamento oggetto EntityMetric 
     * -----------------------------------
     * 
     * Per ogni colonna nel resultset si valorizza il corrispondente campo in Metrics
     * 
     */
    private void execMetricsSystemPopulateEntityFromResultset(EntityMetricValue entityMetric, ResultSet rs) throws SQLException {
    	
		// Misure di conteggio sorgenti
       	entityMetric.setCntPgmAnalyzed(rs.getLong("CNTRCPGA"));   		// Numero programmi analizzati nel sistema
       	entityMetric.setCntCopyDefined(rs.getLong("TOTLCCPD"));   		// Numero copy definiti
		
		// Misure dimensionali sorgenti
       	entityMetric.setSizeLinesCodeLogical(rs.getLong("TOTLSLCL"));   // Numero linee di codice logiche, includenti istruzioni, senza commenti e righe a blank
       	entityMetric.setSizeLinesCodePhisical(rs.getLong("TOTLSLCP")); 	// Numero linee di codice fisiche, includenti istruzioni, commenti e righe a blank
       	entityMetric.setSizeLinesBlank(rs.getLong("TOTLSLCB")); 		// Numero linee a blank
       	entityMetric.setSizeLinesBlankProc(rs.getLong("TOTLSLPB")); 	// Numero linee a blank in procedure division
       	entityMetric.setSizeLinesBlankData(rs.getLong("TOTLSLDB")); 	// Numero linee a blank in data division
       	entityMetric.setSizeLinesComment(rs.getLong("TOTLSLCC")); 		// Numero linee di commento
       	entityMetric.setSizeLinesCommentProc(rs.getLong("TOTLSLCI")); 	// Numero linee di commento in procedure division
       	entityMetric.setSizeLinesCommentData(rs.getLong("TOTLSLCD")); 	// Numero linee di commento in data division
       	entityMetric.setSizeInstr(rs.getLong("TOTLSINS")); 				// Numero istruzioni in procedure division
		
		// Misure stimate con sizeLinesCodeLogical
       	entityMetric.setBackFiredFunctionPoint(rs.getLong("TOTLBFFP")); // Function point stimati in base al numero logico di loc (sizeLinesCodeLogical)
       	entityMetric.setTimeDevelopment(rs.getLong("TOTLTMDV")); 		// Tempo di sviluppo in giorni stimato in base alla produttività media giornaliera
		
		// Misure definizione dati
       	entityMetric.setDefFields(rs.getLong("TOTLDFLD")); 				// Numero campi definiti
       	entityMetric.setDefFieldsInCopy(rs.getLong("TOTLDFCP")); 		// Numero campi definiti dentro moduli copy
       	entityMetric.setDefLiterals(rs.getLong("TOTLDLIT")); 			// Numero literal definite
		
		// Misure di codice dinamico
		entityMetric.setDynamicPgm(rs.getLong("TOTLDPGM")); 			// Numero di programmi con codice dinamico
		entityMetric.setDynamicInstr(rs.getLong("TOTLDINT")); 			// Numero istruzioni dinamiche totali
		entityMetric.setDynamicInstrLight(rs.getLong("TOTLDINL")); 		// Numero istruzioni dinamiche light, con soli campi con value
		entityMetric.setPercDynamicInstr(rs.getDouble("RATEDINT")); 	// % Istruzioni dinamiche per istruzione
		entityMetric.setPercDynamicInstrLight(rs.getDouble("RATEDINL"));// % Istruzioni dinamiche light per istruzione
		
		// Misure violazioni
		entityMetric.setViolations(rs.getLong("TOTLVCNT")); 			   		// Numero violazioni individuate
		entityMetric.setPercViolationsByLogical(rs.getDouble("RATEVLCL")); 		// % Violazioni rispetto alle righe sorgente logiche, con istruzioni
		entityMetric.setPercViolationsByPhisical(rs.getDouble("RATEVLCP"));		// % Violazioni rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		entityMetric.setPercViolationsByInstruction(rs.getDouble("RATEVINS"));	// % Violazioni per istruzione

		// Misure di documentazione
		entityMetric.setPercComByLogical(rs.getDouble("RATERCML")); 	 // % commenti rispetto alle righe sorgente logiche, con istruzioni
		entityMetric.setPercComByPhisical(rs.getDouble("RATERCMP")); 	 // % commenti rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		entityMetric.setPercComByInstruction(rs.getDouble("RATERCMI"));  // % commenti per istruzione
		entityMetric.setPercBlankByPhisical(rs.getDouble("RATERBKP")); 	 // % righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		entityMetric.setPercBlankByInstruction(rs.getDouble("RATERBKI"));// % righe a blank per istruzione
		
		// Misure di codice morto
       	entityMetric.setDeadFields(rs.getLong("TOTLXFLD")); 			// Numero campi definiti non utilizzati
       	entityMetric.setDeadSubGraph(rs.getLong("TOTLXSBG")); 			// Numero sottografi sconnessi non richiamati, in cobol si tratta di section non referenziate
       	entityMetric.setDeadInstr(rs.getLong("TOTLXINS")); 				// Numero istruzioni definite e non referenziate (includono eventuali label)
       	entityMetric.setDeadLabels(rs.getLong("TOTLXLAB")); 			// Numero label definite e non referenziate
       	entityMetric.setDeadCopyData(rs.getLong("TOTLXCPD")); 			// Numero copy definiti in data division e non utilizzati
       	entityMetric.setDeadCopyProc(rs.getLong("TOTLXCPP")); 			// Numero copy definiti in proc division e non utilizzati
		
		// Misure di complessità strutturale e tecnica
       	entityMetric.setStructFanIn(rs.getLong("TOTLSFAI")); 			// Numero programmi chiamanti con Call, Cics Link, Cics Xctl
       	entityMetric.setStructFanOut(rs.getLong("TOTLSFAO")); 			// Numero programmi chiamanti con Call, Cics Link, Cics Xctl
       	entityMetric.setStructSections(rs.getLong("TOTLSSEC")); 		// Numero section nel programma
       	entityMetric.setStructParagraphs(rs.getLong("TOTLSPAR")); 		// Numero paragrafi nel programma
		
		// Misure di complessità funzionale generiche
       	entityMetric.setFuncObjects(rs.getLong("TOTLFOBJ")); 			// Numero oggetti trattati 
       	entityMetric.setFuncRelations(rs.getLong("TOTLFREL")); 			// Numero relazioni fra oggetti trattati 
		entityMetric.setFuncTranInternal(rs.getLong("TOTLFTRI")); 		// Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid
		entityMetric.setFuncTranExternal(rs.getLong("TOTLFTRE")); 		// Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid
		entityMetric.setFuncMap(rs.getLong("TOTLFMAP")); 				// Numero mappe video utilizzate
		entityMetric.setFuncCallInternal(rs.getLong("TOTLFCAI")); 		// Numero call a moduli interni
		entityMetric.setFuncCallExternal(rs.getLong("TOTLFCAE")); 		// Numero call a moduli esterni
		entityMetric.setFuncAccEntityInternal(rs.getLong("TOTLFAEI")); 	// Numero accessi a entity (tabelle db) interni
		entityMetric.setFuncAccEntityExternal(rs.getLong("TOTLFAEE")); 	// Numero accessi a entity (tabelle db) esterni
		entityMetric.setFuncAccMediaInternal(rs.getLong("TOTLFAMI")); 	// Numero accessi a files sequenziali/Vsam/code ts/.. interni
		entityMetric.setFuncAccMediaExternal(rs.getLong("TOTLFAME"));	// Numero accessi a files sequenziali/Vsam/code ts/.. esterni
		
		// Misure di complessità funzionale Function Point
		entityMetric.setFpExternalOutputEO(rs.getLong("TOTLPXEO")); 	// Funzionalità utente (transazione o job) con output generati da un ILF o EIF
		entityMetric.setFpExternalInputEI(rs.getLong("TOTLPXIO")); 		// Funzionalità utente (transazione o job) con add, change,delete di un ILF
		entityMetric.setFpExternalInquiryEQ(rs.getLong("TOTLPXEQ")); 	// Funzionalità utente (transazione o job) di sola read da ILF o EIF
		entityMetric.setFpInternalLogicalFileILF(rs.getLong("TOTLPILF"));	// Tabelle/files definite e gestite dentro il sistema/sottosistema
		entityMetric.setFpExternalInterfaceFileEIF(rs.getLong("TOTLPEIF")); // Tabelle/files definite fuori dal sistema/sottosistema acceduti in read/update
		
		// Misure di complessità funzionale/tecnica per rehosting 
		entityMetric.setRhRateObjectRelation(rs.getDouble("TOTLHROR")); // ????? Rapporto tra il numero di oggetti e numero di relazioni double 
		entityMetric.setRhObjectsInternal(rs.getLong("TOTLHINT")); 		// ????? Numero di oggetti interni al perimetro   
		entityMetric.setRhObjectsInternal(rs.getLong("TOTLHEXT")); 		// ????? Numero di oggetti esterni al perimetro   
		entityMetric.setRhObjectsUnportable(rs.getLong("TOTLHUNP")); 	// Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...) 
		entityMetric.setRhFilesBynary(rs.getLong("TOTLHBYN")); 			// Numero di files/tabelle contenenti campi binari 
		
		// Misure di complessità ciclomatica
		entityMetric.setMcCabeArcs(rs.getLong("TOTLMARC")); 			// Numero archi di programma
		entityMetric.setMcCabeNodes(rs.getLong("TOTLMNOD")); 			// Numero nodi di programma
		entityMetric.setMcCabeGraphConn(rs.getLong("TOTLMGCN")); 		// Numero di sottografi connessi, in cobol sono section/paragrafi richiamati
		entityMetric.setMcCabeOperatorsOrAnd(rs.getLong("TOTLMOPC")); 	// Numero operatori condizionali OR AND per calcolo esteso
		
		// Misure di complessità di Halstead (o Software Science)
		entityMetric.setHalsteadOperators(rs.getLong("TOTLHOPT")); 		// Numero operatori distinti in un programma (n1)
		entityMetric.setHalsteadOperands(rs.getLong("TOTLHOPN")); 		// Numero operandi distinti in un programma  (n2)
		entityMetric.setHalsteadOperatorsOcc(rs.getLong("TOTLHPTO")); 	// Numero occorrenze di operatori (N1)
		entityMetric.setHalsteadOperandsOcc(rs.getLong("TOTLHPNO")); 	// Numero occorrenze di operandi (N2)
		entityMetric.setHalsteadLengthPgm(rs.getLong("TOTLHLNP")); 		// Lunghezza programma
		entityMetric.setHalsteadVocabularyPgm(rs.getLong("TOTLHVBP")); 	// Vocabolario programma
		entityMetric.setHalsteadVolumePgm(rs.getDouble("TOTLHVLP")); 	// Volume programma
		entityMetric.setHalsteadDifficultPgm(rs.getDouble("TOTLHDFP")); // Difficoltà programma
		entityMetric.setHalsteadEffortPgm(rs.getDouble("TOTLHEFP")); 	// Sforzo programma
		entityMetric.setHalsteadTimeWriting(rs.getLong("TOTLHTMW")); 	// Tempo stimato di scrittura programma in secondi
		
		// Indici di complessita/manutenibilità/Testabilità totali (somma di tutte le procedure interne)
		entityMetric.setIdxMITot(rs.getDouble("TOTLIMIT")); 			// Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		entityMetric.setIdxFPTot(rs.getDouble("TOTLIFPT")); 			// Indice di punti funzione
		entityMetric.setIdxMcCabeTot(rs.getDouble("TOTLIMCT")); 		// Indice di complessità ciclomatica di McCabe con il seguente significato
		entityMetric.setIdxReHostingTot(rs.getDouble("TOTLIRET")); 		// Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità massimi
		entityMetric.setIdxMIHigh(rs.getDouble("MAXIMIH")); 			// Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		entityMetric.setIdxFPHigh(rs.getDouble("MAXIFPH")); 			// Indice di punti funzione
		entityMetric.setIdxMcCabeHigh(rs.getDouble("MAXIMCH")); 		// Indice di complessità ciclomatica di McCabe con il seguente significato
		entityMetric.setIdxReHostingHigh(rs.getDouble("MAXIREH")); 		// Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità minimi
		entityMetric.setIdxMILow(rs.getDouble("MINIMIL")); 				// Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		entityMetric.setIdxFPLow(rs.getDouble("MINIFPL")); 				// Indice di punti funzione
		entityMetric.setIdxMcCabeLow(rs.getDouble("MINIMCL")); 			// Indice di complessità ciclomatica di McCabe con il seguente significato
		entityMetric.setIdxReHostingLow(rs.getDouble("MINIREL")); 		// Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità medio
		entityMetric.setIdxMIAvg(rs.getDouble("AVGIMIA")); 				// Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		entityMetric.setIdxFPAvg(rs.getDouble("AVGIFPA")); 				// Indice di punti funzione
		entityMetric.setIdxMcCabeAvg(rs.getDouble("AVGIMCA")); 			// Indice di complessità ciclomatica di McCabe con il seguente significato
		entityMetric.setIdxReHostingAvg(rs.getDouble("AVGIREA")); 		// Indice della complessità e sforzo di rehosting di una applicazione.

		// Sistema di qualità SQUALE, numero violazioni per categoria gravità
		entityMetric.setSqualeViolationsBlocker(rs.getLong("TOTLSVBK")); // Numero violazioni bloccanti
		entityMetric.setSqualeViolationsCritical(rs.getLong("TOTLSVCR"));// Numero violazioni critiche
		entityMetric.setSqualeViolationsCritical(rs.getLong("TOTLSVMJ"));// Numero violazioni maggiori
		entityMetric.setSqualeViolationsMinor(rs.getLong("TOTLSVMN")); 	 // Numero violazioni minori
		entityMetric.setSqualeViolationsInfo(rs.getLong("TOTLSVIN")); 	 // Numero violazioni informative
		
		// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
		entityMetric.setSqualeSQTI(rs.getLong("TOTLSQTI")); 			// Testability 		index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQRI(rs.getLong("TOTLSQRI")); 			// Reliability 		index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQCI(rs.getLong("TOTLSQCI")); 			// Changeability	index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQEI(rs.getLong("TOTLSQEI")); 			// Efficiency 		index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQSI(rs.getLong("TOTLSQSI")); 			// Security 		index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQMI(rs.getLong("TOTLSQMI")); 			// Maintenability 	index (somma costi remediation per questa caratteristica)
		entityMetric.setSqualeSQPI(rs.getLong("TOTLSQPI")); 			// Portability 		index (somma costi remediation per questa caratteristica)
    
    }




    /* -------------------------------------------------------------
     * Totalizza le metriche per ogni sistema fornito 
     * -------------------------------------------------------------
     * 
     * Per ogni sistema:
     * 
     * Recupera i programmi
     * Recupera le metriche di ogni programma
     * Totalizza le metriche
     * 
     * Infine:
     * 
     * Memorizza le metriche totalizzate nella struttura fornita
     * 
     */
	private void execMetricsSystemTotBySystems(Map<String, ArrayList<String>> hs_systemPgms, Map<String, EntityMetricValue> hs_systemMetrics) throws SQLException, ExceptionAmrita {
		
		EntityMetricValue entityMetricSystem = null;
		Metrics metricsSystem = null;
	    Map<String, EnumMetricsSqualeRating> hs_squaleRating = null;
		ResultSet rs = null;
		String system = "";
		String strSql = "";
		String strSqlSum = "";
		boolean isSystemWithMetrics = false;
		
	    Connection conn = DataBaseConnections.getConnection();
		IDAOMetricValue eoDAOMetricValue = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false,false, ucfg);

		strSqlSum = execMetricsSystemSqlSumBody();					// Restituisce il corpo della select di somma, conteggio e medie
		metricsSystem = new Metrics();
		hs_squaleRating = new HashMap<String, EnumMetricsSqualeRating> ();
		
    	// Squale rating
    	for (EnumMetricsSqualeRating enumSqualeRating  : EnumMetricsSqualeRating.values()) {
    		hs_squaleRating.put(enumSqualeRating.toString(), enumSqualeRating);
    	}

		// Scan map con sistemi da gestire
		for (Entry<String, ArrayList<String>> entry_systemPgms : hs_systemPgms.entrySet()) {
			
			system = entry_systemPgms.getKey();
			isSystemWithMetrics = false;
			
			// Totali metriche programma del sistema
			strSql =    "SELECT  sys,   "; 
			strSql = strSql + strSqlSum;					// COUNT/SUM/AVG
			strSql = strSql + " FROM MetricValue"; 
			strSql = strSql + " WHERE  sys = '"  + system + "'";
			strSql = strSql + "   AND  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
		    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
		    strSql = strSql + "   AND  section = '*'";
		    strSql = strSql + " GROUP BY sys";
	 		
			// Esecuzione query e produzione  ResultSet
			rs = eoDAOMetricValue.execSqlGeneric(strSql);
			
            // Lettura resultset
			if (rs.next()) {
				entityMetricSystem = new EntityMetricValue();
				isSystemWithMetrics = true;
				
				// Popolamento entityMetric da resultset
				execMetricsSystemPopulateEntityFromResultset(entityMetricSystem, rs);
				
				// Completamento dati chiave
				entityMetricSystem.setSystem(system);
				entityMetricSystem.setSubSystem("*");
				entityMetricSystem.setTypeObject(EnumObject.OBJECT_SYS);
				entityMetricSystem.setScope(EnumMetricsScope.SCOPE_LEVEL_SYSTEM);
				entityMetricSystem.setIdObject("");
				entityMetricSystem.setSection("");

				// Popolamento oggetto Metric 
				metricsSystem.populateFromDbEntityMetric(entityMetricSystem);
				
				// Calcoli su valori totalizzati  
				metricsSystem.evaluateBackFiredFunctionPoint();
				metricsSystem.evaluateDynamicCodePercent();
				metricsSystem.evaluateTimeDevelopment();
				metricsSystem.evaluateViolationsPercent();

				// Calcolo indici Squale 
				metricsSystem.evaluateSqualeQualityAbsoluteIndex();
				metricsSystem.evaluateSqualeQualityConsolidateIndex();
				metricsSystem.evaluateSqualeRatingIndex(hs_squaleRating);
				metricsSystem.evaluateSqualeRuleComplianceIndex();
				metricsSystem.evaluateSqualeDensityQualityIndex();
				
				// Restore entity per update su db
				metricsSystem.dbPopulateMetric(entityMetricSystem);

				// Update map con metric pronta per update su db
				hs_systemMetrics.put(system, entityMetricSystem);
			}
			rs.close();
			
			// Nessun pgm analizzato con metriche nel sistema
			if (!isSystemWithMetrics) {
				entityMetricSystem = new EntityMetricValue();
				
				// Completamento dati chiave
				entityMetricSystem.setSystem(system);
				entityMetricSystem.setSubSystem("*");
				entityMetricSystem.setTypeObject(EnumObject.OBJECT_SYS);
				entityMetricSystem.setScope(EnumMetricsScope.SCOPE_LEVEL_SYSTEM);
				entityMetricSystem.setIdObject("");
				entityMetricSystem.setSection("");
				
				// Update map con metric pronta per update su db
				hs_systemMetrics.put(system, entityMetricSystem);
			}
		}
		
		DataBaseConnections.releaseConnection(conn);
		eoDAOMetricValue.setConn(null);
		return;
	}
	
	/* ------------------------------------------------------------------
     * Valorizza le metriche totalizzate per ogni sottosistema fornito 
     * ------------------------------------------------------------------
     * 
     * Per ogni sistema:
     * 
     * Recupera i programmi
     * Recupera le metriche di ogni programma
     * Totalizza le metriche
     * 
     * Infine:
     * 
     * Memorizza le metriche totalizzate nella struttura fornita
     * 
    */
	private void execMetricsSystemTotBySubSystems(Map<String, ArrayList<String>> hs_subSystemPgms, Map<String, EntityMetricValue> hs_subSystemMetrics) throws SQLException, ExceptionAmrita {
		
		EntityMetricValue entityMetricSubSystem = null;
		Metrics metricsSubSystem = null;
	    Map<String, EnumMetricsSqualeRating> hs_squaleRating = null;
		ResultSet rs = null;
		String subSystem = "";
		String strSql = "";
		String strSqlSum = "";
		boolean isSubSystemWithMetrics = false;
		
		strSqlSum = execMetricsSystemSqlSumBody();					// Restituisce il corpo della select di somma, conteggio e medie
		metricsSubSystem = new Metrics();
		hs_squaleRating = new HashMap<String, EnumMetricsSqualeRating> ();

	    Connection conn = DataBaseConnections.getConnection();
		IDAOMetricValue eoDAOMetricValue = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false,false, ucfg);
		
    	// Squale rating
    	for (EnumMetricsSqualeRating enumSqualeRating  : EnumMetricsSqualeRating.values()) {
    		hs_squaleRating.put(enumSqualeRating.toString(), enumSqualeRating);
    	}
		
		// Scan map con sottosistemi da gestire
		for (Entry<String, ArrayList<String>> entry_subSystemPgms : hs_subSystemPgms.entrySet()) {
			
			subSystem = entry_subSystemPgms.getKey();
			isSubSystemWithMetrics = false;
			
			// Totali metriche programma del sistema
			strSql =    "SELECT  subSys,   "; 
			strSql = strSql + strSqlSum;					// COUNT/SUM/AVG
			strSql = strSql + " FROM MetricValue"; 
			strSql = strSql + " WHERE  sys = '"  + subSystem + "'";
			strSql = strSql + "   AND  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
		    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
		    strSql = strSql + "   AND  section = '*'";
		    strSql = strSql + " GROUP BY subSys";
	 		
			// Esecuzione query e produzione  ResultSet
			rs = eoDAOMetricValue.execSqlGeneric(strSql);

			// Lettura resultset
			if (rs.next()) {
				entityMetricSubSystem = new EntityMetricValue();
				isSubSystemWithMetrics = true;
				
				execMetricsSystemPopulateEntityFromResultset(entityMetricSubSystem, rs);
				
				// Completamento dati chiave
				entityMetricSubSystem.setSystem("*");
				entityMetricSubSystem.setSubSystem(subSystem);
				entityMetricSubSystem.setTypeObject(EnumObject.OBJECT_SUBSYS);
				entityMetricSubSystem.setScope(EnumMetricsScope.SCOPE_LEVEL_SUBSYSTEM);
				entityMetricSubSystem.setIdObject("");
				entityMetricSubSystem.setSection("");

				// Popolamento oggetto Metric 
				metricsSubSystem.populateFromDbEntityMetric(entityMetricSubSystem);
				
				// Calcoli su valori totalizzati  
				metricsSubSystem.evaluateBackFiredFunctionPoint();
				metricsSubSystem.evaluateDynamicCodePercent();
				metricsSubSystem.evaluateTimeDevelopment();
				metricsSubSystem.evaluateViolationsPercent();
				
				// Calcolo indici Squale 
				metricsSubSystem.evaluateSqualeQualityAbsoluteIndex();
				metricsSubSystem.evaluateSqualeQualityConsolidateIndex();
				metricsSubSystem.evaluateSqualeRatingIndex(hs_squaleRating);
				metricsSubSystem.evaluateSqualeRuleComplianceIndex();
				metricsSubSystem.evaluateSqualeDensityQualityIndex();
				
				// Restore entity per update su db
				metricsSubSystem.dbPopulateMetric(entityMetricSubSystem);

				// Update map con metric pronta per update su db
				hs_subSystemMetrics.put(subSystem, entityMetricSubSystem);
			}
			rs.close();
			
			// Nessun pgm analizzato con metriche nel sistema
			if (!isSubSystemWithMetrics) {
				entityMetricSubSystem = new EntityMetricValue();
				
				// Completamento dati chiave
				entityMetricSubSystem.setSystem("*");
				entityMetricSubSystem.setSubSystem(subSystem);
				entityMetricSubSystem.setTypeObject(EnumObject.OBJECT_SUBSYS);
				entityMetricSubSystem.setScope(EnumMetricsScope.SCOPE_LEVEL_SUBSYSTEM);
				entityMetricSubSystem.setIdObject("");
				entityMetricSubSystem.setSection("");
				
				// Update map con metric pronta per update su db
				hs_subSystemMetrics.put(subSystem, entityMetricSubSystem);
			}

		}
		
		DataBaseConnections.releaseConnection(conn);
		eoDAOMetricValue.setConn(null);

		return;
	}


	/* ----------------------------------------------------------------------------
     * Valorizza le metriche totalizzate globali, per tutti i programmi analizzati
     * ----------------------------------------------------------------------------
     * 
     * Per tutti i sistemi e sottosistemi
     * 
     * Recupera i programmi
     * Recupera le metriche di ogni programma
     * Totalizza le metriche
     * 
     * Infine:
     * 
     * Memorizza le metriche totalizzate nella struttura fornita
     * 
    */
	private void execMetricsSystemTotGlobal(EntityMetricValue entityMetricGlobal, ArrayList<String> al_pgmGlobal) throws SQLException, ExceptionAmrita {
		
		Metrics metricsGlobal = null;
	    Map<String, EnumMetricsSqualeRating> hs_squaleRating = null;
		ResultSet rs = null;
		String strSql = "";
		String strSqlSum = "";

	    Connection conn = DataBaseConnections.getConnection();
		IDAOMetricValue eoDAOMetricValue = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false,false, ucfg);
		
		strSqlSum = execMetricsSystemSqlSumBody();					// Restituisce il corpo della select di somma, conteggio e medie
		metricsGlobal = new Metrics();
		hs_squaleRating = new HashMap<String, EnumMetricsSqualeRating> ();
		
    	// Squale rating
    	for (EnumMetricsSqualeRating enumSqualeRating  : EnumMetricsSqualeRating.values()) {
    		hs_squaleRating.put(enumSqualeRating.toString(), enumSqualeRating);
    	}		
		// Totali metriche programma del sistema
		strSql =    "SELECT    "; 
		strSql = strSql + strSqlSum;					// COUNT/SUM/AVG
		strSql = strSql + " FROM MetricValue"; 
		strSql = strSql + " WHERE  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
	    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
	    strSql = strSql + "   AND  section = '*'";
 		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAOMetricValue.execSqlGeneric(strSql);

		// Lettura resultset
		if (rs.next()) {
             
			execMetricsSystemPopulateEntityFromResultset(entityMetricGlobal, rs);
			
			// Completamento dati chiave
			entityMetricGlobal.setSystem("*");
			entityMetricGlobal.setSubSystem("*");
			entityMetricGlobal.setTypeObject(EnumObject.OBJECT_SYS_SUBSYS_GLOBAL);
			entityMetricGlobal.setScope(EnumMetricsScope.SCOPE_LEVEL_GLOBAL);
			entityMetricGlobal.setIdObject("");
			entityMetricGlobal.setSection("");

			// Popolamento oggetto Metric 
			metricsGlobal.populateFromDbEntityMetric(entityMetricGlobal);
			
			// Calcoli su valori totalizzati  
			metricsGlobal.evaluateBackFiredFunctionPoint();
			metricsGlobal.evaluateDynamicCodePercent();
			metricsGlobal.evaluateTimeDevelopment();
			metricsGlobal.evaluateViolationsPercent();
			
			// Calcolo indici Squale 
			metricsGlobal.evaluateSqualeQualityAbsoluteIndex();
			metricsGlobal.evaluateSqualeQualityConsolidateIndex();
			metricsGlobal.evaluateSqualeRatingIndex(hs_squaleRating);
			metricsGlobal.evaluateSqualeRuleComplianceIndex();
			metricsGlobal.evaluateSqualeDensityQualityIndex();
			
			// Restore entity per update su db
			metricsGlobal.dbPopulateMetric(entityMetricGlobal);

		}
		rs.close();
		
		// Programmi
		strSql =    "SELECT idObject   "; 
		strSql = strSql + " FROM MetricValue"; 
		strSql = strSql + " WHERE  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
	    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
	    strSql = strSql + "   AND  section = '*'";
 		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAOMetricValue.execSqlGeneric(strSql);
		
        // Lettura resultset
		while (rs.next()) {
			al_pgmGlobal.add(rs.getString(1));
		}
		rs.close();

		DataBaseConnections.releaseConnection(conn);
		eoDAOMetricValue.setConn(null);

	}



	/* ------------------------------------------------------------
     * Valorizza tutti i sistemi presenti con i relativi programmi
     * ------------------------------------------------------------
     * 
     */
	private void execMetricsSystemGetSystems(Map<String, ArrayList<String>> hs_systemPgms) throws SQLException, ExceptionAmrita {

		ResultSet rs = null;
		ArrayList<String> al_pgm = null;
		String system = "";
		String pgmName = "";
		String strSql = "";

	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAOObject = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		IDAOMetricValue eoDAOMetricValue = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql = "SELECT sys FROM Object"; 
		strSql = strSql + " WHERE  typeObject = "  + EnumObject.OBJECT_SYS.ordinal();
	    strSql = strSql + "   AND  sys <> '*'";
 		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAOObject.execSqlGeneric(strSql);

		// Scan sistemi trovati
		while (rs.next()) {
			
			system = rs.getString(1);
			al_pgm = hs_systemPgms.get(system);
			if (al_pgm == null) {
				al_pgm = new ArrayList<String> ();
				hs_systemPgms.put(system, al_pgm);
			}
		}
		rs.close();

		// Scan map con sistemi per recupero programmi analizzati con metriche
		for (Entry<String, ArrayList<String>> entry_systemPgms : hs_systemPgms.entrySet()) {
			
			strSql =    "SELECT idObject   "; 
			strSql = strSql + " FROM MetricValue"; 
			strSql = strSql + " WHERE  sys = '" + entry_systemPgms.getKey() + "'";
			strSql = strSql + "   AND  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
		    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
		    strSql = strSql + "   AND  section = '*'";

			// Esecuzione query e produzione  ResultSet
			rs = eoDAOMetricValue.execSqlGeneric(strSql);

			al_pgm = entry_systemPgms.getValue();

			// Scan sistemi trovati
			while (rs.next()) {
				pgmName = rs.getString(1);
				al_pgm.add(pgmName);
			}
			rs.close();
		}
		
		DataBaseConnections.releaseConnection(conn);
		eoDAOObject.setConn(null);
		eoDAOMetricValue.setConn(null);
		return;
	}


	/* ------------------------------------------------------------
     * Valorizza tutti i sottosistemi presenti con i relativi programmi
     * ------------------------------------------------------------
     * 
     */
	private void execMetricsSystemGetSubSystems(Map<String, ArrayList<String>> hs_subSystemPgms) throws SQLException, ExceptionAmrita {
		
		ResultSet rs = null;
		ArrayList<String> al_pgm = null;
		String subsystem = "";
		String pgmName = "";
		String strSql = "";

	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAOObject = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		IDAOMetricValue eoDAOMetricValue = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false,false, ucfg);

		// Composizione Select di lettura EntityObject
		strSql = "SELECT OBJTSUBS FROM OBJT"; 
		strSql = strSql + " WHERE  OBJTTYPO = "  + EnumObject.OBJECT_SUBSYS.ordinal();
	    strSql = strSql + "   AND  OBJTSUBS <> '*'";
 		
		// Esecuzione query e produzione  ResultSet
		rs = eoDAOObject.execSqlGeneric(strSql);

		// Scan sistemi trovati
		while (rs.next()) {
			
			subsystem = rs.getString(1);
			al_pgm = hs_subSystemPgms.get(subsystem);
			if (al_pgm == null) {
				al_pgm = new ArrayList<String> ();
				hs_subSystemPgms.put(subsystem, al_pgm);
			}
		}
		rs.close();

		// Scan map con sistemi per recupero programmi analizzati con metriche
		for (Entry<String, ArrayList<String>> entry_systemPgms : hs_subSystemPgms.entrySet()) {
			
			strSql =    "SELECT idObject   "; 
			strSql = strSql + " FROM MetricValue"; 
			strSql = strSql + " WHERE  subSys = '" + entry_systemPgms.getKey() + "'";
			strSql = strSql + "   AND  scope = "  + EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal();
		    strSql = strSql + "   AND  typeObject = "  + EnumObject.OBJECT_PGM_COBOL.ordinal();
		    strSql = strSql + "   AND  section = '*'";

			// Esecuzione query e produzione  ResultSet
			rs = eoDAOMetricValue.execSqlGeneric(strSql);

			al_pgm = entry_systemPgms.getValue();

			// Scan sistemi trovati
			while (rs.next()) {
				pgmName = rs.getString(1);
				al_pgm.add(pgmName);
			}
			rs.close();
		}

		DataBaseConnections.releaseConnection(conn);
		eoDAOObject.setConn(null);
		eoDAOMetricValue.setConn(null);
		return;
	}



	/* ---------------------------------------------------------------------------------------
     * Restituisce il corpo dello statement sql di conteggio, totalizzazione e media metriche
     * ---------------------------------------------------------------------------------------
     * 
     * Metodo richiamato a fronte di sistema o di sottosistema come livelli di aggregazione
     * 
     */
	private String execMetricsSystemSqlSumBody() {
		StringBuffer strSql = null;
		strSql = new StringBuffer();
		
		// Misure di conteggio sorgenti
		strSql.append("  COUNT(*)      AS CNTRCPGA"); // Numero programmi analizzati nel sistema
		strSql.append(", SUM(cntCopyDefined) AS TOTLCCPD"); // Numero copy definiti
		
		// Misure dimensionali sorgenti
		strSql.append(", SUM(sizeLinesCodeLogical) AS TOTLSLCL"); // Numero linee di codice logiche, includenti istruzioni, senza commenti e righe a blank
		strSql.append(", SUM(sizeLinesCodePhisical) AS TOTLSLCP"); // Numero linee di codice fisiche, includenti istruzioni, commenti e righe a blank
		strSql.append(", SUM(sizeLinesBlank) AS TOTLSLCB"); // Numero linee a blank
		strSql.append(", SUM(sizeLinesBlankProc) AS TOTLSLPB"); // Numero linee a blank in procedure division
		strSql.append(", SUM(sizeLinesBlankData) AS TOTLSLDB"); // Numero linee a blank in data division
		strSql.append(", SUM(sizeLinesComment) AS TOTLSLCC"); // Numero linee di commento
		strSql.append(", SUM(sizeLinesCommentProc) AS TOTLSLCI"); // Numero linee di commento in procedure division
		strSql.append(", SUM(sizeLinesCommentData) AS TOTLSLCD"); // Numero linee di commento in data division
		strSql.append(", SUM(sizeInstr) AS TOTLSINS"); // Numero istruzioni in procedure division
		
		// Misure stimate con sizeLinesCodeLogical
		strSql.append(", SUM(backFiredFunctionPoint) AS TOTLBFFP"); // Function point stimati in base al numero logico di loc (sizeLinesCodeLogical)
		strSql.append(", SUM(timeDevelopment) AS TOTLTMDV"); // Tempo di sviluppo in giorni stimato in base alla produttività media giornaliera
		
		// Misure definizione dati
		strSql.append(", SUM(defFields) AS TOTLDFLD"); // Numero campi definiti
		strSql.append(", SUM(defFieldsInCopy) AS TOTLDFCP"); // Numero campi definiti dentro moduli copy
		strSql.append(", SUM(defLiterals) AS TOTLDLIT"); // Numero literal definite
		
		// Misure di documentazione
		strSql.append(", (TOTLSLCI * 100) / TOTLSLCL AS RATERCML"); // % commenti rispetto alle righe sorgente logiche, con istruzioni
		strSql.append(", (TOTLSLCI * 100) / TOTLSLCP AS RATERCMP"); // % commenti rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		strSql.append(", (TOTLSLCI * 100) / TOTLSINS AS RATERCMI"); // % commenti per istruzione
		strSql.append(", (TOTLSLPB * 100) / TOTLSLCP AS RATERBKP"); // % righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		strSql.append(", (TOTLSLPB * 100) / TOTLSINS AS RATERBKI"); // % righe a blank per istruzione
		
		// Misure di codice dinamico
		strSql.append(", SUM(dynamicPgm) AS TOTLDPGM"); 				// Numero di programmi con codice dinamico
		strSql.append(", SUM(dynamicInstr) AS TOTLDINT"); 				// Numero istruzioni dinamiche totali
		strSql.append(", SUM(dynamicInstrLight) AS TOTLDINL"); 				// Numero istruzioni dinamiche light, con soli campi con value
		strSql.append(", (TOTLDINT * 100) / TOTLSINS AS RATEDINT"); // % Istruzioni dinamiche per istruzione
		strSql.append(", (TOTLDINL * 100) / TOTLSINS AS RATEDINL"); // % Istruzioni dinamiche light per istruzione
		
		// Misure violazioni
		strSql.append(", SUM(violations) AS TOTLVCNT"); 				// Numero violazioni individuate
		strSql.append(", (TOTLVCNT * 100) / TOTLSLCL AS RATEVLCL"); // % Violazioni rispetto alle righe sorgente logiche, con istruzioni
		strSql.append(", (TOTLVCNT * 100) / TOTLSLCP AS RATEVLCP"); // % Violazioni rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
		strSql.append(", (TOTLVCNT * 100) / TOTLSINS AS RATEVINS"); // % Violazioni per istruzione
		
		// Misure di codice morto
		strSql.append(", SUM(deadFields) AS TOTLXFLD"); // Numero campi definiti non utilizzati
		strSql.append(", SUM(deadSubGraph) AS TOTLXSBG"); // Numero sottografi sconnessi non richiamati, in cobol si tratta di section non referenziate
		strSql.append(", SUM(deadInstr) AS TOTLXINS"); // Numero istruzioni definite e non referenziate (includono eventuali label)
		strSql.append(", SUM(deadLabels) AS TOTLXLAB"); // Numero label definite e non referenziate
		strSql.append(", SUM(deadCopyData) AS TOTLXCPD"); // Numero copy definiti in data division e non utilizzati
		strSql.append(", SUM(deadCopyProc) AS TOTLXCPP"); // Numero copy definiti in proc division e non utilizzati
		
		// Misure di complessità strutturale e tecnica
		strSql.append(", SUM(structFanIn) AS TOTLSFAI"); // Numero programmi chiamanti con Call, Cics Link, Cics Xctl
		strSql.append(", SUM(structFanOut) AS TOTLSFAO"); // Numero programmi chiamanti con Call, Cics Link, Cics Xctl
		strSql.append(", SUM(structSections) AS TOTLSSEC"); // Numero section nel programma
		strSql.append(", SUM(structParagraphs) AS TOTLSPAR"); // Numero paragrafi nel programma
		
		// Misure di complessità funzionale generiche
		strSql.append(", SUM(funcObjects) AS TOTLFOBJ"); // ????? Numero oggetti trattati 
		strSql.append(", SUM(funcRelations) AS TOTLFREL"); // Numero relazioni fra oggetti trattati 
		strSql.append(", SUM(funcTranInternal) AS TOTLFTRI"); // Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid
		strSql.append(", SUM(funcTranExternal) AS TOTLFTRE"); // Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid
		strSql.append(", SUM(funcMap) AS TOTLFMAP"); // Numero mappe video utilizzate
		strSql.append(", SUM(funcCallInternal) AS TOTLFCAI"); // Numero call a moduli interni
		strSql.append(", SUM(funcCallExternal) AS TOTLFCAE"); // Numero call a moduli esterni
		strSql.append(", SUM(funcAccEntityInternal) AS TOTLFAEI"); // Numero accessi a entity (tabelle db) interni
		strSql.append(", SUM(funcAccEntityExternal) AS TOTLFAEE"); // Numero accessi a entity (tabelle db) esterni
		strSql.append(", SUM(funcAccMediaInternal) AS TOTLFAMI"); // Numero accessi a files sequenziali/Vsam/code ts/.. interni
		strSql.append(", SUM(funcAccMediaExternal) AS TOTLFAME"); // Numero accessi a files sequenziali/Vsam/code ts/.. esterni
		
		// Misure di complessità funzionale Function Point
		strSql.append(", SUM(fpExternalOutputEO) AS TOTLPXEO"); // Funzionalità utente (transazione o job) con output generati da un ILF o EIF
		strSql.append(", SUM(fpExternalInputEI) AS TOTLPXIO"); // Funzionalità utente (transazione o job) con add, change,delete di un ILF
		strSql.append(", SUM(METRPXEQ) AS TOTLPXEQ"); // Funzionalità utente (transazione o job) di sola read da ILF o EIF
		strSql.append(", SUM(fpExternalInquiryEQ) AS TOTLPILF"); // Tabelle/files definite e gestite dentro il sistema/sottosistema
		strSql.append(", SUM(fpExternalInterfaceFileEIF) AS TOTLPEIF"); // Tabelle/files definite fuori dal sistema/sottosistema acceduti in read/update
		
		// Misure di complessità funzionale/tecnica per rehosting 
		strSql.append(", SUM(rhRateObjectRelation) AS TOTLHROR"); // ????? Rapporto tra il numero di oggetti e numero di relazioni double 
		strSql.append(", SUM(rhObjectsInternal) AS TOTLHINT"); // ????? Numero di oggetti interni al perimetro   
		strSql.append(", SUM(rhObjectsExternal) AS TOTLHEXT"); // ????? Numero di oggetti esterni al perimetro   
		strSql.append(", SUM(rhObjectsUnportable) AS TOTLHUNP"); // Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...) 
		strSql.append(", SUM(rhFilesBynary) AS TOTLHBYN"); // Numero di files/tabelle contenenti campi binari 
		
		// Misure di complessità ciclomatica
		strSql.append(", SUM(mcCabeArcs) AS TOTLMARC"); // Numero archi di programma
		strSql.append(", SUM(mcCabeNodes) AS TOTLMNOD"); // Numero nodi di programma
		strSql.append(", SUM(mcCabeGraphConn) AS TOTLMGCN"); // Numero di sottografi connessi, in cobol sono section/paragrafi richiamati
		strSql.append(", SUM(mcCabeOperatorsOrAnd) AS TOTLMOPC"); // Numero operatori condizionali OR AND per calcolo esteso
		
		// Misure di complessità di Halstead (o Software Science)
		strSql.append(", SUM(halsteadOperators) AS TOTLHOPT"); // Numero operatori distinti in un programma (n1)
		strSql.append(", SUM(halsteadOperands) AS TOTLHOPN"); // Numero operandi distinti in un programma  (n2)
		strSql.append(", SUM(halsteadOperatorsOcc) AS TOTLHPTO"); // Numero occorrenze di operatori (N1)
		strSql.append(", SUM(halsteadOperandsOcc) AS TOTLHPNO"); // Numero occorrenze di operandi (N2)
		strSql.append(", SUM(halsteadLengthPgm) AS TOTLHLNP"); // Lunghezza programma
		strSql.append(", SUM(halsteadVocabularyPgm) AS TOTLHVBP"); // Vocabolario programma
		strSql.append(", SUM(halsteadVolumePgm) AS TOTLHVLP"); // Volume programma
		strSql.append(", SUM(halsteadDifficultPgm) AS TOTLHDFP"); // Difficoltà programma
		strSql.append(", SUM(halsteadEffortPgm) AS TOTLHEFP"); // Sforzo programma
		strSql.append(", SUM(halsteadTimeWriting) AS TOTLHTMW"); // Tempo stimato di scrittura programma in secondi
		
		// Indici di complessita/manutenibilità/Testabilità totali (somma di tutte le procedure interne)
		strSql.append(", SUM(idxMITot) AS TOTLIMIT"); // Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		strSql.append(", SUM(idxFPTot) AS TOTLIFPT"); // Indice di punti funzione
		strSql.append(", SUM(idxMcCabeTot) AS TOTLIMCT"); // Indice di complessità ciclomatica di McCabe con il seguente significato
		strSql.append(", SUM(idxReHostingTot) AS TOTLIRET"); // Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità massimi
		strSql.append(", MAX(idxMIHigh) AS MAXIMIH"); // Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		strSql.append(", MAX(idxFPHigh) AS MAXIFPH"); // Indice di punti funzione
		strSql.append(", MAX(idxMcCabeHigh) AS MAXIMCH"); // Indice di complessità ciclomatica di McCabe con il seguente significato
		strSql.append(", MAX(idxReHostingHigh) AS MAXIREH"); // Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità minimi
		strSql.append(", MIN(idxMILow) AS MINIMIL"); // Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		strSql.append(", MIN(idxFPLow) AS MINIFPL"); // Indice di punti funzione
		strSql.append(", MIN(idxMcCabeLow) AS MINIMCL"); // Indice di complessità ciclomatica di McCabe con il seguente significato
		strSql.append(", MIN(idxReHostingLow) AS MINIREL"); // Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Indici di complessita/manutenibilità/Testabilità medio
		strSql.append(", (SUM(idxMITot) / COUNT(*)) AS AVGIMIA"); // Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
		strSql.append(", (SUM(idxFPTot) / COUNT(*)) AS AVGIFPA"); // Indice di punti funzione
		strSql.append(", (SUM(idxMcCabeTot) / COUNT(*)) AS AVGIMCA"); // Indice di complessità ciclomatica di McCabe con il seguente significato
		strSql.append(", (SUM(idxReHostingTot) / COUNT(*)) AS AVGIREA"); // Indice della complessità e sforzo di rehosting di una applicazione.
		
		// Sistema di qualità SQUALE, numero violazioni per categoria gravità
		strSql.append(", SUM(squaleViolationsBlocker) AS TOTLSVBK"); 	// Squale numero violazioni bloccanti
		strSql.append(", SUM(squaleViolationsCritical) AS TOTLSVCR"); 	// Squale numero violazioni critiche
		strSql.append(", SUM(squaleViolationsMajor) AS TOTLSVMJ"); 	// Squale numero violazioni maggiori
		strSql.append(", SUM(squaleViolationsMinor) AS TOTLSVMN"); 	// Squale numero violazioni minori
		strSql.append(", SUM(squaleViolationsInfo) AS TOTLSVIN"); 	// Squale numero violazioni informative
		
		// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
		strSql.append(", SUM(squaleSQTI) AS TOTLSQTI"); 	// Squale     Testability 		index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQRI) AS TOTLSQRI"); 	// Squale     Reliability 		index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQCI) AS TOTLSQCI"); 	// Squale     Changeability	    index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQEI) AS TOTLSQEI"); 	// Squale     Efficiency 		index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQSI) AS TOTLSQSI"); 	// Squale     Security 		    index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQMI) AS TOTLSQMI"); 	// Squale     Maintenability 	index (somma costi remediation per questa caratteristica)
		strSql.append(", SUM(squaleSQPI) AS TOTLSQPI"); 	// Squale     Portability 		index (somma costi remediation per questa caratteristica)

		return strSql.toString();
	}

	
	/* -----------------------------------------------------------------------------------
	 * Inserimento sul database delle metriche raggruppate e dei programmi di ogni gruppo
	 * -----------------------------------------------------------------------------------
	 * 
	 * Per ogni sistema, sottosistema  
	 * 
	 * 1) Si inseriscono su db le righe della tabella METR
	 * 2) Si inseriscono su db le righe della tabella METP
	 * 
	 * La key delle map in input è il livello di aggregazione.
	 * Per sistema/sottosistema i due valori sono separati da spazio.
	 */
	private void execMetricsSystemUpdateDb(Map<String, ArrayList<String>> hs_systemPgms
										 , Map<String, ArrayList<String>> hs_subSystemPgms
										 , Map<String, EntityMetricValue> hs_systemMetrics
										 , Map<String, EntityMetricValue> hs_subSystemMetrics
										 , EntityMetricValue entityMetricGlobal
										 , ArrayList<String> al_pgmGlobal) throws SQLException, ExceptionAmrita {
		

/*		
		EntityMetric entityMetric = null;
		EntityMetricPgm entityMetricPgm = null;
		ArrayList<String> al_pgm = null;
		String system = "";
		String subSystem = "";
		
		// Scan entries map sistemi
		for (Entry<String, ArrayList<String>> entry_systemPgms : hs_systemPgms.entrySet()) {
			
			// EntityMetric con totali del sistema
			system =  entry_systemPgms.getKey(); 
			al_pgm = entry_systemPgms.getValue();
			entityMetric = hs_systemMetrics.get(system);
			adbi.al_DbMetric.add(entityMetric);
			
			// Programmi cobol nel sistema
			for (String pgmName : al_pgm) {
				entityMetricPgm = new EntityMetricPgm();
				entityMetricPgm.setSystem(system);
				entityMetricPgm.setSubSystem(entityMetric.getSubSystem());
				entityMetricPgm.setScope(entityMetric.getScope());
				entityMetricPgm.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
				entityMetricPgm.setIdObject(pgmName);
				adbi.al_DbMetricPgm.add(entityMetricPgm);
			}
		}
		
		// Scan entries map sotto sistemi
		for (Entry<String, ArrayList<String>> entry_subSystemPgms : hs_subSystemPgms.entrySet()) {
			
			// Programmi e EntityMetric con totali del sistema
			subSystem =  entry_subSystemPgms.getKey(); 
			al_pgm = entry_subSystemPgms.getValue();
			entityMetric = hs_subSystemMetrics.get(subSystem);
			adbi.al_DbMetric.add(entityMetric);
			
			// Programmi nel sottosistema
			for (String pgmName : al_pgm) {
				entityMetricPgm = new EntityMetricPgm();
				entityMetricPgm.setSystem(entityMetric.getSystem());
				entityMetricPgm.setSubSystem(subSystem);
				entityMetricPgm.setScope(entityMetric.getScope());
				entityMetricPgm.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
				entityMetricPgm.setIdObject(pgmName);
				adbi.al_DbMetricPgm.add(entityMetricPgm);
			}
		}
		
		// Valori totalizzati su tutti i sistemi e tutti i sottosistemi
		adbi.al_DbMetric.add(entityMetricGlobal);
		for (String pgmName : al_pgmGlobal) {
			entityMetricPgm = new EntityMetricPgm();
			entityMetricPgm.setSystem(entityMetricGlobal.getSystem());
			entityMetricPgm.setSubSystem(entityMetricGlobal.getSubSystem());
			entityMetricPgm.setScope(entityMetricGlobal.getScope());
			entityMetricPgm.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
			entityMetricPgm.setIdObject(pgmName);
			adbi.al_DbMetricPgm.add(entityMetricPgm);
		}
		
    	// Updates su db
    	adbi.sqlDeleteGenericLevel(true, false);		// Delete generiche da eseguire prima impostate dai chiamanti								
     	adbi.sqlDeleteSystemLevelMetrics(false, false);	// Delete specifici per il processo metriche a livello sistema
     	adbi.update(false, true, "", false, true);  	// Aggiornamento effettivo con info da processo di soluzione istruzioni dinamiche
*/
	}



	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
    /*
     * 
     * Elaborazione processi schedulati per il tipo programma.
     * 
     * 
     */
	private void processPgmSolveDynamicSpreaded(InnerDescriptorSource objectToAnalyze, SourceInput si, ExecutionDirectives diCur) throws ExceptionAmrita, SQLException  {
		
    	SystemService ss;
    	
		// Selezione analizzatore da attivare
    	switch (objectToAnalyze.objectType) {
    	        
				case OBJECT_PGM_COBOL: 
					
					// Creazione istanza di analyzer e caricamento oggetti.
					// Viene simulata la situazione a fine analisi preliminare programma.
			        // La soluzione del codice dinamico stesso programma seve già stata essere effettuata.
			        // Le strutture di definizione del codice dinamico sono pertanto già disponibili.
					AnalyzerCobol acp = new AnalyzerCobol(this.ucfg, diCur, programCobol);
					adbi = acp.getAnalyzerDbInfo();
					
					// Deserializzazione oggetto descrittore programma
					ss = new SystemService(ucfg, null);
				  	programCobol = (ProgramCobol) ss.getSerialized(ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm(), objectToAnalyze.idObject, SUFFIX_SERIALIZED_PGM);  	        					
									        
					acp.setProgramCobol(programCobol);
					
					lid = programCobol.getLogicInfoDynamic();
					lsm = new LogicSpreadedPgm(ucfg, di, programCobol);
					lsm.setLogicInfoDynamic(lid);
					lsm.setAnalyzerDbInfo(adbi);
					lgm = new LogicSamePgm(ucfg, di, programCobol);
					lgm.setLogicInfoDynamic(lid);
					lgm.setAnalyzerDbInfo(adbi);
					acp.setPgmNameUnderParsing(objectToAnalyze.idObject);
					acp.setProcessSystemLevel(true);
					acp.setLogicInfoDynamic(lid);
  				    acp.setLogicSamePgm(lgm);
  				    acp.setLogicSpreadedPgm(lsm);
					acp.setInitialContextAnalysis();
					acp.di.userExitInfoPgm.setSystem(programCobol.sysOwner);
					acp.di.userExitInfoPgm.setSubSystem(programCobol.subSysOwner);
					acx = new AnalyzerCicsInstr(ucfg, di, acp);
 					acp.setAnalyzerCics(acx);
					
					
					// Soluzione istruzioni dinamiche esternamente al programma
					if (diCur.optSolveDynamicSpreaded) {
						
						acp.solveInstructionsDynamicSpreaded();
						
						// Persistenza su file system, serializzazione oggetto descrittore programma + info logiche dinamiche
						try {
							acp.putSerialized(ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm(), SUFFIX_SERIALIZED_PGM, objectToAnalyze.idObject, programCobol);
						} catch (ExceptionAmrita e) {
				            logMessage(EnumMessageType.WARNING, "MI0108");  			// Elaborazione continua con altro programma
				            return;  													// Errore di serializzazione, log messaggio già effettuato: exit
						}
						
				        // Persistenza su database, aggiornamenti complessivi effettivi base dati
				        if (this.di.optUpdateDb) {
				        	// Delete generiche da eseguire prima impostate dai chiamanti
				        	adbi.sqlDeleteGenericLevel();	
				        	adbi.sqlUpdateGenericLevel();
				        	adbi.update(false, true, "", null);
						}
						break;
					}
					
					 
				default:
					break;
		}
    	
 	}
	
  /* 
   * Lettura istruzioni dinamiche spreaded programma da DynamicField 
   * e popolamento struttura di AnalyzerDbInfo.
   * Scopo è la completa simulazione delle condizioni che si
   * avrebbero analizzando il programma.
   * La struttura su AnalyzerDbInfo con i campi dinamici viene
   * utilizzata per l'aggiornamento finale sul database
  */
	/*
   private void populateDbInfoDynamicFieldsSpreaded(InnerDescriptorSource objectToAnalyze, AnalyzerDbInfo adbi) throws Exception {
	   
		String sqlWhere = ""; 
		String sqlOrderBy = ""; 
		EntityDynamicField entityDynamicField= null;
		List<EntityDynamicField> ar_object = null;

	    Connection conn = DataBaseConnections.getConnection();
		IDAODynamicField eoDAODynamicField = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false,false, ucfg);
		
		// Composizione Where di accesso a DFLD per istruzioni dinamiche spreaded da risolvere
		sqlWhere = sqlWhere + "          sys = '" + this.di.systemInput    + "'";
		sqlWhere = sqlWhere +	  " AND  subsys = '" + this.di.subSystemInput + "'";
		sqlWhere = sqlWhere +     " AND  idObject = '" + objectToAnalyze.idObject  + "'";
		sqlWhere = sqlWhere +     " AND  typeObject = "  + objectToAnalyze.objectType.ordinal();
		sqlWhere = sqlWhere +     " AND  typeObject = "  + objectToAnalyze.objectType.ordinal();
		sqlWhere = sqlWhere +     " AND  solved = false "  ;
		sqlWhere = sqlWhere +     " AND  spreaded = true "  ;

		sqlOrderBy =  "   ORDER BY numInstr ";
		
		ar_object =  eoDAODynamicField.readSetEntityWhere(sqlWhere, sqlOrderBy);
        
		// Scan righe
		for (EntityDynamicField obj_entityDynamicField : ar_object) {
			entityDynamicField = obj_entityDynamicField;
			this.adbi.addObjEntityDynamicField(entityDynamicField);
		}
		
		DataBaseConnections.releaseConnection(conn);
		eoDAODynamicField.setConn(null);

	}

*/

}	

