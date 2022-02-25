package analyzer;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import dao.DAOImplDynamicField;
import dao.IDAODynamicField;
import entities.EntityDynamicField;
import enums.EnumDirectivesExecution;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import exception.ExceptionAmrita;;
/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ProcessPgmLevel
 * </h1>
 * <p>
 * 
 * Questa classe gestisce tutti i processi a livello di programma. <br>
 * Per processo si intende una elaborazione che aggiunge informazioni in termini di oggetti, 
 * relazioni e conoscenza tecnica del programma,  metriche, 
 * soluzione di istruzioni dinamiche e così via.<br>
 * <p>
 * Quando questa elaborazione viene attivata, le direttive di esecuzione individuano un insieme
 * di programmi da elaborare. Ogni programma viene elaborato solo se risulta nello stato di
 * analizzato senza errori o in attesa di dati esterni.<br>
 * Se il programma è nello stato corretto, vengono eseguiti per quel programma tutte le elaborazioni
 * previste nelle direttive di esecuzione e, in dettaglio:<br>
 * <p>
 *     <ul>
 *         <li><strong>OPT_PGM_SOLVE_CODE_STATIC</strong> </li><br>
 *         Soluzione istruzioni statiche quali call 'program'

 *         <li><strong>OPT_PGM_SOLVE_CODE_DYNAMIC_LOCAL</strong> </li><br>
 *         Soluzione istruzioni dinamiche quali call PGM-FIELD all'interno<br>
 *         dello stesso programma. La ricerca di soluzione si ferma se il valore
 *         attribuito al campo è nello stesso programma, assegnato esplicitamente
 *         o all'interno di un file, coda di Ts etc., i cui valori sono stati resi
 *         disponibili.
 *         
 *         <li><strong>OPT_PGM_SOLVE_CODE_CICS</strong> </li><br>
 *         Soluzione istruzioni Cics quali Exec Cics Link, Exec Cics Send Map,
 *         etc.
 *         
 *         <li><strong>OPT_PGM_SOLVE_DYNAMIC_SPREADED</strong> </li><br>
 *         Soluzione contestuale istruzioni dinamiche spreaded (spalmate) su programmi 
 *         chiamanti e/o chiamati.<br>
 *         Quando una istruzione dinamica non può essere risolta localmente, con questa
 *         opzione viene tentata automaticamente la soluzione esternamente al programma
 *         corrente, nei programmi chiamanti e/o chiamati, senza necessità di eseguire
 *         elaborazioni a livelolo di sistema.<br>
 *         I programmi esterni in cui cercare i valori dinamici devono essere stati già
 *         analizzati, risultare privi di errori e con il grafo di programma creato,
 *         per essere utilizzati.<br>
 *         Attualmente questa opzione non è attivata.
 *         
 *         <li><strong>OPT_PGM_SOLVE_DML_SQL_ENABLED</strong> </li><br>
 *         Soluzione istruzioni DML Sql.<br>
 *         Attualmente questa opzione non è attivata.
 *         
 *         <li><strong>OPT_PGM_SOLVE_DML_DL1</strong> </li><br>
 *         Soluzione istruzioni DML DL1.<br>
 *         Attualmente questa opzione non è attivata.
 *         
 *         <li><strong>OPT_PGM_SOLVE_DML_ADABAS</strong> </li><br>
 *         Soluzione istruzioni DML Adabas.<br>
 *         Attualmente questa opzione non è attivata.
 *         
 *         <li><strong>OPT_PGM_GRAPH_CREATION</strong> </li><br>
 *         Viene creato il grafo di programma delle istruzioni eseguibili.<br>
 *         Il grafo viene serializzato insieme all'oggetto {@link Program} che descrive
 *         analiticamente il programma ed è un requisito per le successive elaborazioni.
 *         
 *         <li><strong>OPT_PGM_DETECT_DEAD_CODE</strong> </li><br>
 *         Viene marcato il codice morto nel programma.<br>
 *         Alcune forme di codice morto, come label e section non referenziate, sono
 *         già deducibili dall'analisi del programma, interrogando l'oggetto {@link Program}.<br>
 *         Altre forme di codice morto, come codice irraggiungibile, fra <code>goto</code>
 *         incondizionato e label, dopo <code>stop run</code>, <code>Exec Cics Xctl</code>
 *         etc., sono individuabili solo visitando il grafo di programma.<br>
 *         Operativamente vengono marcate tutte le istruzioni non raggiungibili<br>
 *         interrogando l'oggetto {@link Program} è possibile ottenere l'elenco di tutte le 
 *         istruzioni non raggiungibili.
 *         
 *         <li><strong>OPT_PGM_EVALUATE_METRICS</strong> </li><br>
 *         Vengono calcolate tutte le metriche a livelolo di programma e memorizzate
 *         direttamente nell'oggetto {@link Program} e nel data base.<br>
 *         Le metriche sono dimensionali, come numero righe, commenti, righe vuote <br>
 *         Oppure sono di complessita ciclomatica di McAbe o di healstead<br>
 *         Oppure sono di complessità funzionale come Function Point o altra metrica custom.<br>
 *         Attualmente questa opzione non è attivata.
 *         
 *    </ul>
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0 10/70/2010
 * @since 1.0
 * @see ExecutionStarter
 * @see ExecutionShared
 * @see ExecutionDispatcher
*/
public class ProcessPgmLevel extends ExecutionShared implements Runnable, AmritaConstants{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
	// Gestione sources
	private SourceInput si = null;

	// Dati necessari per processo a livello programma
	private ProgramCobol programCobol = null;
	private AnalyzerDbInfo adbi = null;
	private AnalyzerCicsInstr acx = null;
	private LogicSamePgm lgm = null;
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
	public ProcessPgmLevel(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
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
		
		execProcessPgmLevel();
		
		this.di.execMsAllEnd = System.currentTimeMillis();
		this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
	    this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
		return;
	}
	
	

	/**
	 * 
	 * Processo eseguita in modo sincrono o come thread separato<br>
	 * <p>
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execProcessPgmLevel() throws Exception {
		
		// Gestione intabellamento nomi programmi da processare
		ArrayList<InnerDescriptorSource> al_ObjectToAnalize = null;
		ExecutionDirectives diCur = null;
				
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
 		
 		
		// Estrazione centralizzata oggetti da trattare del tipo, stato e con opzioni richieste
 	    al_ObjectToAnalize = this.getObjectsToProcess(new EnumObject[]{EnumObject.OBJECT_PGM_COBOL}
		                                            , ar_objectStatusGood
	                                                , new EnumObjectOption[] {}
		                                            );			
		
	    
	    // Pgm da analizzare individuati  
	    this.di.execTotObjectToProcess = al_ObjectToAnalize.size();
	    logMessage(EnumMessageType.INFORMATION, "MI0130", al_ObjectToAnalize.size()+"");

	     
	    // Scan per attivazione processi specifici per ogni programma
	    for (InnerDescriptorSource infoObjectToAnalyze : al_ObjectToAnalize) {

	    	diCur = infoObjectToAnalyze.di;
	    	
	      	// Il sorgente originale non viene più preso in considerazione
	        
	    	// Valori correnti disponibili da direttiva di esecuzione
	    	this.di.curObjectId = infoObjectToAnalyze.idObject;
	        diCur.pgmName = infoObjectToAnalyze.idObject;
	        diCur.fileNameCurObj = infoObjectToAnalyze.sourceFileName;
	        diCur.filePathCurObj = infoObjectToAnalyze.sourcePath;
	        diCur.libraryCodeCurObj = infoObjectToAnalyze.libraryCode;
	        diCur.libraryPathCurObj = infoObjectToAnalyze.libraryPath;
            diCur.en_CurProcessFunction = di.en_CurProcessFunction; 
            this.di.isExecutionWithErrors = false;
            
			try {
				
				this.di.execCntObjectProcessed++;
				processPgmLevel(infoObjectToAnalyze, si, diCur);    // Elaborazioni schedulate a livello di programma

			} catch (Exception e) {
				this.di.excpOccurred = e;
				this.di.isExecutionWithErrors = true;
				this.di.execCntObjectProcessedExcp++;
				this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
				this.logApplicationInfoException();		 // Informazioni applicative
                this.logSystemInfoException();           // Informazioni di debug se presenti
                this.updateStatusCurrentObject(di, EnumObjectStatus.OBJECT_PROCESSED_WITH_EXCEPTION);
                this.di.excpOccurred = null;
  			}
	    	
	    } // End-for Source

		this.logFinalExecutionStatistics();     // Conteggi, elenco oggetti con errorori e in exception
		
 		return;

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
	private void processPgmLevel(InnerDescriptorSource objectToAnalyze, SourceInput si, ExecutionDirectives diCur) throws ExceptionAmrita, SQLException  {
		
		UserExitInfo userExitInfoPgm = null;
		boolean isToUpdateSerialized = false;
		
    	// Selezione analizzatore da attivare
    	switch (objectToAnalyze.objectType) {
    	        
				case OBJECT_PGM_COBOL: 
					
					AnalyzerCobol acp = new AnalyzerCobol(this.ucfg, diCur, programCobol);
					
					// Deserializzazione oggetto descrittore programma
			        try {
			  	        programCobol = (ProgramCobol) acp.getSerialized(this.ucfg.getDirCobolObjPgm(), objectToAnalyze.idObject, SUFFIX_SERIALIZED_PGM);
			        } catch (ExceptionAmrita e) {
				        return;  												// Errore di deserializzazione, log messaggio già effettuato: exit
			        }
			        
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(diCur, programCobol.programName, "", "");
					
		            // Valide in caso di exception, per update status e altre info
					diCur.curObjectId = programCobol.programName;
					diCur.curObjectType = EnumObject.OBJECT_PGM_COBOL;
					
					// Creazione istanza di analyzer e caricamento oggetti
					// Viene simulata la situazione a fine analisi preliminare programma
			        programCobol.sd = this.ucfg;
					acp.setProgramCobol(programCobol);
					acp.setUserExitInfoPgm(userExitInfoPgm);
					adbi = acp.getDbInfo();
					lid = new LogicInfoDynamic();
//					lgm = new LogicManager(ucfg, diCur, programCobol);
//					lgm.setAnalyzerDbInfo(adbi);
//					lgm.setLogicInfoDynamic(lid);
					programCobol.setLogicInfoDynamic(lid);
					acp.setPgmNameUnderParsing(objectToAnalyze.idObject);
					acp.setProcessPgmLevel(true);
					acp.setLogicInfoDynamic(lid);
					acp.setLogicSamePgm(lgm);
					acp.setInitialContextAnalysis();
					acx = new AnalyzerCicsInstr(ucfg, diCur, acp);
 					acp.setAnalyzerCics(acx);
 					acp.setForProcessPgmLevel();					// Impostazioni map e riallocazioni
 										
					// Soluzione istruzioni dinamiche localmente al programma
					// Utilizza anche optPgmSolveCodeCics
					if (diCur.optSolveDynamicLocal || diCur.optSolveDynamicLocalLight) {
						try {
							populateDbInfoDynamicFields(objectToAnalyze, adbi);                             // Per update stato istruzione a fine elaborazione
						} catch (Exception e) {
				            logMessage(EnumMessageType.INFORMATION, "EI0032", e, objectToAnalyze.idObject); // Stack trace in log
				            logMessage(EnumMessageType.INFORMATION, "MI0110", objectToAnalyze.idObject);    // Elaborazione continua con altro programma
				            this.di.isExecutionWithErrors = true;
				            return;  											    						 
						}
						acp.solveInstructionsDynamicLocally();
						isToUpdateSerialized = true;
					}
					// Soluzione istruzioni Sql
					if (diCur.optSolveDmlSql) {
						isToUpdateSerialized = true;
					}
					
					// Soluzione istruzioni Dl1
					if (diCur.optSolveDmlDl1) {
						isToUpdateSerialized = true;
					}
					
					// Soluzione istruzioni Adabas
					if (diCur.optSolveDmlAdabas) {
						isToUpdateSerialized = true;
					}
					
					// Intercettazione dead code 
					if (diCur.optDetectDeadCode) {
						isToUpdateSerialized = true;
						acp.detectDeadCode();
						isToUpdateSerialized = true;
					}

					// Metriche base a livello di programma, data e proc
					if (diCur.optMetricsAll || diCur.optMetricsBasic) {
						acp.metricsMeasurePgmDimensionalDataBasic(); 			// Misure dimensionali definizioni dati		
						acp.metricsMeasurePgmDimensionalProcBasic(this.programCobol.getMetricsProgram(), 0, this.programCobol.entriesProcedure().length - 1); 
						isToUpdateSerialized = true;
					}

					// Metriche di dettaglio mainline, section/paragrafi e totali su programma.
					if (diCur.optMetricsAll || diCur.optMetricsBasic) {
						acp.metricsMeasurePgmProcDetail();    					// Calcolo metriche di dettaglio per mainline e ogni section/paragrafo richiamato
						isToUpdateSerialized = true;
					}

					// Violazioni alle Metriche 
					if (diCur.optMetricsAll
					||	diCur.optMetricsSqualeDetectViolation) {
						acp.metricsDetectViolation();
						isToUpdateSerialized = true;
					}

					// Metriche addOn calcolabili solo a valle dell'analisi
					if (diCur.optMetricsAll
					||	diCur.optMetricsComplexityStructure
					||	diCur.optMetricsComplexityFunction
					||	diCur.optMetricsFunctionPoint 
					||	diCur.optMetricsRehosting ) {
						acp.metricsMeasurePgmAddOn();
						isToUpdateSerialized = true;
					}

					// Persistena su file system, serializzazione oggetto descrittore programma
					if (isToUpdateSerialized) {
						try {
							acp.putSerialized(ucfg.getDirCobolObjPgm(), SUFFIX_SERIALIZED_PGM, objectToAnalyze.idObject, programCobol);
						} catch (ExceptionAmrita e) {
				            logMessage(EnumMessageType.WARNING, "MI0108");  			// Elaborazione continua con altro programma
				            return;  													// Errore di serializzazione, log messaggio già effettuato: exit
						}
					}
					
			    	// Salvataggio metriche di programm/mainline/dettaglio per ogni section/paragrafo X update finale su db
					acp.storeMetricsOnDbStructure(diCur.optMetricsBasic, diCur.optMetricsDetail && diCur.optMetricsPgmDetailOnDb);

			        // Persistenza su database, aggiornamenti complessivi effettivi base dati
			        if (this.di.optUpdateDb) {
			        	// Delete generiche da eseguire prima impostate dai chiamanti
			        	adbi.sqlDeleteGenericLevel();									
			        	// Delete dei soli oggetti/relazioni/.. associati a istruzioni dinamiche
			        	if (this.di.optSolveDynamicLocal || this.di.optSolveDynamicLocalLight) {
//				        	adbi.sqlDeleteProgramLevelDynamic(objectToAnalyze.idObject, EnumObject.OBJECT_PGM_COBOL, true, false, false, false);	
						}
			        	// Delete metriche
			        	if (this.di.optMetricsAll || this.di.optMetricsBasic || this.di.optMetricsDetail) {
			        		adbi.sqlDeleteProgramLevelMetrics(programCobol.programName, false, false);
						}	
			        	// Delete violazioni alle metriche
			        	if (this.di.optMetricsSqualeDetectViolation) {
			        		adbi.sqlDeleteProgramLevelMetricsViolations(programCobol.programName, false, false);
						}
			        	// Aggiornamento effettivo con info istruzioni dinamiche
			        	adbi.update(false, false, "", null);
					}
					break;
					 
	
				default:
					break;
		}
    	
 	}
	
  /* 
   * Lettura istruzioni dinamiche stesso programma da DynamicField e popolamento
   * struttura di AnalyzerDbInfo.
   * Scopo è la completa simulazione delle condizioni che si
   * avrebbero analizzando il programma origine.
   * La struttura su AnalyzerDbInfo con i campi dinamici viene
   * utilizzata per l'aggiornamento finale sul database
  */
   private void populateDbInfoDynamicFields(InnerDescriptorSource objectToAnalyze, AnalyzerDbInfo adbi) throws Exception {
		String sqlWhere = ""; 
		String sqlOrderBy = ""; 
		EntityDynamicField entityDynamicField= null;
		List<EntityDynamicField> ar_object = null;

	    Connection conn = DataBaseConnections.getConnection();
		IDAODynamicField eoDAO = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false,false, ucfg);
		
		// Vengono estratti TUTTI i campi di istruzioni dinamiche da trattare 
		// in questo tipo di elaborazione.
		// Ovvero campi di istruzioni dinamiche NON risolte, che potrebbero essere anche spreaded
		
		
		// Composizione Where di accesso a DFLD
		sqlWhere = sqlWhere + "          sys = '" + this.di.systemInput    + "'";
		sqlWhere = sqlWhere +	  " AND  subSys = '" + this.di.subSystemInput + "'";
		sqlWhere = sqlWhere +     " AND  idObject = '" + objectToAnalyze.idObject  + "'";
		sqlWhere = sqlWhere +     " AND  typeObject = "  + objectToAnalyze.objectType.ordinal();
		sqlWhere = sqlWhere +     " AND  solved = false "  ;

		sqlOrderBy = "   ORDER BY numInstr ";
		
		ar_object =  eoDAO.readSetEntityWhere(sqlWhere, sqlOrderBy);
        
		// Scan righe
		for (Object obj_entityDynamicField : ar_object) {
			entityDynamicField = (EntityDynamicField) obj_entityDynamicField;
			this.adbi.addObjEntityDynamicField(entityDynamicField);
		}
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
	}



}	

