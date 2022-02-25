package analyzer;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import enums.EnumDirectivesExecution;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumSourceType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ProcessAnalyzeSource
 * </h1>
 * <p>
 * 
 * Analisi sorgenti specificati dal pilota dei sorgenti  e dei filtri.
 * Vengono analizzati tutti i tipi di sorgente impostati da setObjectTypeToAnalyze().
 * Nel caso non siano stati impostati tipi di sorgente da analizzare, vengono, analizzate tutte
 * le tipologie incontrate.<br>
 * In base al tipo di sorgente viene attivato il tipo di analizzatore specifico, lanciato
 * in un thread separato.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 10/04/2010
 * @see ExecutionStarter
 * @see ExecutionShared
 * @see ExecutionLancherFunction
 * @see ExecutionLancherProcess
*/
public class ProcessAnalyzeSource extends ExecutionShared implements Runnable, AmritaConstants{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
    // Attenzione !! 
	// le direttive attive sono disponibili nella variabile di istanza DirectivesInfo di, della superclasse
	// valorizzata dal costruttore, che contiene anche tutte le informazioni di esecuzione.
		
	// Gestione sources
	private SourceManager sm = null; 
	private SourceInput si = null;
	
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
	public ProcessAnalyzeSource(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
		super(sd, al_di.get(al_di.size()-1));	
		this.al_di = al_di;
 	}
	
	
   /**
    * 
    * Attivato a fronte dell'esecuzione di start() sul thread  da {@link ExecutionDispatcher} <br>
 	 * 
	 * Funzione eseguita in modo sincrono o come thread separato.<br>
	 * <p>
	 * Questo metodo di ingresso permette di effettuare operazioni iniziali
	 * e finali relativi alla funzione da eseguire.<br>
	 * Per esempiovengono impostati l'ora di inzio, fine ed elapsed di elaborazione.<br>
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.<br>
	 * <p>
	 * @throws Exception 
   */
	public void run() {
		
		this.di.excpOccurred = null;
		this.di.isExecutionWithErrors = false;

		try {
			
			this.di.execMsAllStart = System.currentTimeMillis();
			this.ucfg.setExecMsAllEnd(this.di.execMsAllStart);
			this.ucfg.setExecStopRequired(false);
			this.di.execProcessRunning = true;
			this.di.execStopRequired = false;
			this.di.isExecutionWithErrors = false;
			this.di.isExecutionWithException = false;
			this.di.execCntObjectProcessedNoError = 0;
			this.ucfg.setExecStopRequired(false);
			this.ucfg.setExecProcessRunning(true);
			this.ucfg.setExecMsAllEnd(0);
			this.ucfg.setExecMsAllElapsed(0);
			
			execAnalyzeSources();
				
			this.di.execProcessRunning = false;
			this.di.execStopRequired = false;
			this.di.execMsAllEnd = System.currentTimeMillis();
			this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
			this.ucfg.setExecProcessRunning(false);
			this.ucfg.setExecMsAllEnd(this.di.execMsAllEnd);
			this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			return;
			
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
	 * Processo di analisi dei sorgenti eseguito in modo sincrono o come thread separato.<br>
	 * <p>
	 * Dopo aver recuperato i nomi dei sorgenti da analizzare, l'analisi effettiva viene
	 * effettuata dalle classi specializzate {@link AnalyzerCobol}, {@link AnalyzerCicsInstr}
	 * {@link AnalyzerSqlDml} e così via per ogni tipo di sorgente.<br>
	 * La classe {@link Analyzer} funge da classe madre di tutte le classi di analisi e contiene tutte le strutture dati
	 * e i metodi comuni a tutti i processi di analisi. In particicolare vengono gestiti in modo univoco e centralizzato
	 * il reperimento dei sorgenti, dei loro commenti e di tutte le strutture dati per identificarli.<br>
	 * <p>
	 * @throws Exception 
	 * 
	 */
	public void execAnalyzeSources() throws Exception {
		
		// Gestione intabellamento nomi sorgenti da analizzare
		ArrayList<InnerDescriptorSource> al_ObjectToAnalize = null;
		
		
		EnumObject ar_objectTypeGood[] = null;
		Set<EnumObject> set_objectTypeGood = null;
		EnumObjectStatus ar_objectStatusGood[] = null;
		Set<EnumObjectStatus> set_objectStatusGood = null;
		int cntSource = 0;
		
	    // Allocazione gestore sources e impostazione tipologie soorgenti valide da trattare
 		sm = new SourceManager(ucfg);
 		
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
 		
 		
		// Estrazione centralizzata oggetti da trattare
 	    al_ObjectToAnalize = this.getObjectsToProcess(ar_objectTypeGood
		                                            , ar_objectStatusGood
	                                                , new EnumObjectOption[] {}
		                                            );
	    

	    // Sorgenti da analizzare individuati per la direttiva OBJECT_IDENTIFICATION_UNIT corrente
	    this.di.execTotObjectToProcess = al_ObjectToAnalize.size();     
	    logMessage(EnumMessageType.INFORMATION, "MI0130", al_ObjectToAnalize.size()+"");	    	    
	    clearExecInfo();   // Info di esecuzione a livello di utente	    

	    // Scan per attivazione analizzatori specifici  
	    for (InnerDescriptorSource infoObjectToAnalyze : al_ObjectToAnalize) {

	      	// Recupero il sorgente da analizzare
	    	this.si = sm.getSource(infoObjectToAnalyze.sourcePath, false, false);
	    	
			// Sorgente non trovato nella directory specificata nell'oggetto: logging e continue  next source
	        if (si == null) {
	            logMessage(EnumMessageType.WARNING, "MW0006", infoObjectToAnalyze.idObject, infoObjectToAnalyze.sourcePath);
	        	continue;
			}
	        this.di.subSystemInput = infoObjectToAnalyze.subSys;
	        this.di.fileNameCurObj = infoObjectToAnalyze.sourceFileName;
	        this.di.filePathCurObj = infoObjectToAnalyze.sourcePath;
	        this.di.libraryCodeCurObj = infoObjectToAnalyze.libraryCode;
	        this.di.libraryPathCurObj = infoObjectToAnalyze.libraryPath;
            this.di.isExecutionWithErrors = false;
            this.di.isExecutionWithException = false;
            this.si.setSystemOwner(infoObjectToAnalyze.sys);
            this.si.setSubSystemOwner(infoObjectToAnalyze.subSysOwner);
            this.si.setLibraryCode(infoObjectToAnalyze.libraryCode);
            this.si.setLibraryPath(infoObjectToAnalyze.libraryPath);
            this.si.setPathComplete(infoObjectToAnalyze.sourcePath);	        
            this.si.setSourceType(getSourceTypeFromObjectType(infoObjectToAnalyze.objectType));
			this.di.execCntObjectProcessed++;
			this.di.curObjectType = infoObjectToAnalyze.objectType;
			this.di.curObjectId = si.getIdSource();
			this.di.execCurIdObject = this.di.curObjectId;
			this.di.execCurTypeObject = this.di.curObjectType;
			this.di.execMsCurStart = System.currentTimeMillis();
			this.di.execMsCurEnd = 0;
			this.createProcessLog(al_ObjectToAnalize.size());		// ProcessLog e dati ucfg visibili da web service   	
			
			try {				
				
				activationSpecificAnalyzer(infoObjectToAnalyze, si, ++cntSource, al_ObjectToAnalize.size());  // Attivazione analizzatore specifico 							                                                      // Consolidamento updates
				
				this.di.execCntObjectProcessedNoError++;
				this.di.execMsCurEnd = System.currentTimeMillis();
				this.di.execMsCurElapsed = (int) (this.di.execMsCurEnd - this.di.execMsCurStart);
				this.di.execMsAllElapsed += this.di.execMsCurElapsed;
				this.di.execMsAvg = this.di.execMsAllElapsed / this.di.execCntObjectProcessed;								 
	            updateProcessLog();                                 // ProcessLog e dati ucfg visibili da web service          
	           
	            // Required stop execution: force process stop
	            if (this.ucfg.getExecStopRequired()) {
				   break;
				}
	            
			} catch (Exception e) {
				// Potrebbe non esserci una connessione attiva di cui fare rollback per l'utente
				if (this.ucfg.getDbConn() != null) {
					this.ucfg.getDbConn().rollback();
					DataBaseConnections.releaseConnection(this.ucfg.getDbConn());
				}
				this.di.excpOccurred = e;
				this.di.isExecutionWithErrors = false;            // Eventualmente acceso in updateOnDbPendingErrorsWarningForException()
				this.di.isExecutionWithException = true;
				this.di.execCntObjectProcessedExcp++;
				this.di.exec_alObjectNameExcp.add(this.di.curObjectId);
				this.di.execMsCurEnd = System.currentTimeMillis();
				this.di.execMsCurElapsed = (int) (this.di.execMsCurEnd - this.di.execMsCurStart);
				this.di.execMsAllElapsed += this.di.execMsCurElapsed;
				this.di.execMsAvg = this.di.execMsAllElapsed / this.di.execCntObjectProcessed;								 
				this.updateStatusCurrentObject(di, EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION);
				this.logApplicationInfoException();			       // Informazioni applicative su log
                this.logSystemInfoException();           	       // Informazioni di debug se presenti su log
                this.updateOnDbPendingErrorsWarningForException(); // Per ogni istruzione con errori/warning + errori/warning non legati a istruzioni
                this.di.excpOccurred = null;
	            this.updateProcessLog();                            // ProcessLog e dati ucfg visibili da web service                              
	           
	            // Required stop execution: force process stop
	            if (this.ucfg.getExecStopRequired()) {
				   break;
				}	            
 			}
			
	    } // End-for Source
				
		this.logFinalExecutionStatistics();     // Conteggi, programmi in errore e in exception, update valori in ucfg (exec)
  		return;

 	}

	/*
	 * Restituzione elenco oggetti da analizzare
	 */
	public ArrayList<InnerDescriptorSource> getObjectsToAnalyze() throws Exception {
		
		// Gestione intabellamento nomi sorgenti da analizzare
		ArrayList<InnerDescriptorSource> al_ObjectToAnalize = null;
		EnumObject ar_objectTypeGood[] = null;
		Set<EnumObject> set_objectTypeGood = null;
		EnumObjectStatus ar_objectStatusGood[] = null;
		Set<EnumObjectStatus> set_objectStatusGood = null;
		int cntSource = 0;
		
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
 				
		// Estrazione centralizzata oggetti da trattare
 	    al_ObjectToAnalize = this.getObjectsToProcess(ar_objectTypeGood
		                                            , ar_objectStatusGood
	                                                , new EnumObjectOption[] {}
		                                            );		
 	    
 	    return al_ObjectToAnalize;
	}

	/*
     * In base al tipo oggetto restituisce il tipo source
     */
	private EnumSourceType getSourceTypeFromObjectType(EnumObject objectType) {
		switch (objectType) {
		case OBJECT_COPY_COBOL_DATA:		
			return EnumSourceType.COBOL_COPY_DATA;
		case OBJECT_COPY_COBOL_PROC:
			return EnumSourceType.COBOL_COPY_PROC;
		case OBJECT_PGM_COBOL:
			return EnumSourceType.COBOL_PROGRAM;
		case OBJECT_CICS_BMS_SOURCE:
			return EnumSourceType.CICS_BMS;
		default:
			break;
		}
		return EnumSourceType.NOT_ASSIGNED;
	}


	/*
     * 
     * Attivazione analizzatore specifico in base al tipo oggetto
     * 
     */
	private void activationSpecificAnalyzer(InnerDescriptorSource objectToAnalyze, SourceInput si, int cntSource, int totSource) throws Exception {

		UserExitInfo userExitInfoPgm = null;
		AnalyzerCobol acc = null;
		
    	// Selezione analizzatore da attivare
    	switch (objectToAnalyze.objectType) {
    	
				case OBJECT_PGM_COBOL: 
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente TODO (Inutile, già disponibile)
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(this.di.systemInput); 			// Sistema corrente del programma dall'oggetto estratto
			        this.di.userExitInfoPgm.setSubSystem(this.di.subSystemInput); 		// sottosistema corrente del programma dall'oggetto estratto
 	
					// Attivazione analizzatore programmi cobol
					AnalyzerCobol acp = new AnalyzerCobol(this.ucfg, this.di);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_PGM_COBOL;
					this.di.execCurIdObject = this.di.curObjectId;
					this.di.execCurTypeObject = EnumObject.OBJECT_PGM_COBOL;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0111", this.di.curObjectId, cntSource+"", totSource+"");
					acp.analyzeProgram(si, objectToAnalyze);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;
					
				case OBJECT_COPY_COBOL_DATA:
					
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput =  si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

			        // Attivazione analizzatore moduli copy di data division
			        acc = new AnalyzerCobol(this.ucfg, this.di);
			        this.di.curObjectId = objectToAnalyze.sourceFileName;
			        this.di.curObjectType = EnumObject.OBJECT_COPY_COBOL_DATA;
			        this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0306", this.di.curObjectId, cntSource+"", totSource+"");
			        acc.analyzeCopySource(si, objectToAnalyze, EnumSourceType.COBOL_COPY_DATA);
			        if (this.di.curObjectWithErrors) {
			        	this.di.isExecutionWithErrors = true;
			        	this.di.execCntObjectProcessedError++;
			        	this.di.exec_alObjectNameError.add(this.di.curObjectId);
			        }
			        break;
 
				case OBJECT_COPY_COBOL_PROC:		
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0306", this.di.curObjectId, cntSource+"", totSource+"");
			        acc = new AnalyzerCobol(this.ucfg, this.di);
			        this.di.curObjectId = objectToAnalyze.sourceFileName;
			        this.di.curObjectType = EnumObject.OBJECT_COPY_COBOL_PROC;
			        this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
		            acc.analyzeCopySource(si, objectToAnalyze, EnumSourceType.COBOL_COPY_PROC);
			        if (this.di.curObjectWithErrors) {
			        	this.di.isExecutionWithErrors = true;
			        	this.di.execCntObjectProcessedError++;
			        	this.di.exec_alObjectNameError.add(this.di.curObjectId);
			        }
			        break;
			        
				case OBJECT_JCL_JOB:
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(this.di.systemInput); 			// Sistema corrente del programma in analisi
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Attivazione analizzatore jcl Mvs
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0308", this.di.curObjectId, cntSource+"", totSource+"");
					AnalyzerJclMvs ajclJob = new AnalyzerJclMvs(this.ucfg, this.di, EnumObject.OBJECT_JCL_JOB);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_JCL_JOB;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					ajclJob.analyzeJclJob(si, objectToAnalyze.sourceFileName);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;
					
				case OBJECT_JCL_PROC:
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(this.di.systemInput); 			// Sistema corrente del programma in analisi
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Attivazione analizzatore jcl Mvs
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0308", this.di.curObjectId, cntSource+"", totSource+"");
					AnalyzerJclMvs ajclProc = new AnalyzerJclMvs(this.ucfg, this.di, EnumObject.OBJECT_JCL_PROC);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_JCL_PROC;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					ajclProc.analyzeJclProc(si, objectToAnalyze.sourceFileName);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;
					
				case OBJECT_JCL_INCLUDE:
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Attivazione analizzatore jcl Mvs
					AnalyzerJclMvs ajclInclude = new AnalyzerJclMvs(this.ucfg, this.di, EnumObject.OBJECT_JCL_INCLUDE);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_JCL_INCLUDE;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0308", this.di.curObjectId, cntSource+"", totSource+"");
					ajclInclude.analyzeJclInclude(si, objectToAnalyze.sourceFileName);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;
					
				case OBJECT_SQL_SCRIPT:
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Attivazione analizzatore sql 
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0307", this.di.curObjectId, cntSource+"", totSource+"");
					AnalyzerSql asql = new AnalyzerSql(this.ucfg, this.di, objectToAnalyze.sourceFileName);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_JCL_INCLUDE;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					asql.analyzeSqlScript(si, objectToAnalyze.sourceFileName);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;

				case OBJECT_CICS_BMS_SOURCE:
					// Individuazione corretto sistema/sottosistema di appartenenza del sorgente
					userExitInfoPgm = userExitGetSystemSubsystem(di, objectToAnalyze.sourceFileName, "", "");
					this.di.userExitInfoPgm = userExitInfoPgm;
			        this.di.userExitInfoPgm.setSystem(si.getSystemOwner()); 			// Sistema corrente del programma in analisi da descrittore source
			        this.di.userExitInfoPgm.setSubSystem(si.getSubSystemOwner()); 		// sottosistema corrente del programma in analisi da descrittore source
			        this.di.subSystemInput = si.getSubSystemOwner(); 				    // sottosistema corrente proprietario del programma in analisi da descrittore source       

					// Attivazione analizzatore Cics Bms 
					// Messaggio di inizio analisi
					logMessage(EnumMessageType.INFORMATION, "MI0316", this.di.curObjectId, cntSource+"", totSource+"");
					AnalyzerCicsBms abms = new AnalyzerCicsBms(this.ucfg, this.di, objectToAnalyze.sourceFileName);
					this.di.curObjectId = objectToAnalyze.sourceFileName;
					this.di.curObjectType = EnumObject.OBJECT_JCL_INCLUDE;
					this.di.curObjectWithErrors = false;
					this.di.isExecutionWithErrors = false;
					this.di.isExecutionWithException = false;
					abms.analyzeCicsBms(si, objectToAnalyze.sourceFileName);
					if (this.di.curObjectWithErrors) {
						this.di.isExecutionWithErrors = true;
						this.di.execCntObjectProcessedError++;
						this.di.exec_alObjectNameError.add(this.di.curObjectId);
					}
					break;

				default:
					break;
		}
    	
    	si = null;			// Per garbage
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                      Metodi privati                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
}

