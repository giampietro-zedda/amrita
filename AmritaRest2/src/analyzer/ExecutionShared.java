package analyzer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import dao.IDAOObject;
import dao.IDAOObjectOption;
import dao.IDAOProcessLog;
import dao.MySQLDAOFactory;
import dao.DAOFactory;
import dao.DAOImplObject;
import dao.DAOImplObjectOption;
import dao.DAOImplProcessLog;
import dao.DAOImplSqlGeneric;
import utilities.DateTimeService;
import utilities.ReflectionManager;
import entities.EntityObject;
import entities.EntityObjectAnalysisError;
import entities.EntityObjectAnalysisInfo;
import entities.EntityObjectOption;
import entities.EntityProcessLog;
import enums.EnumAmritaExceptionError;
import enums.EnumDirectivesExecution;
import enums.EnumInstrDataCategory;
import enums.EnumMessageType;
import enums.EnumModule;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumProcessStatus;
import enums.EnumSourceType;
import enums.EnumTypeProcessAnalysis;
import enums.EnumUserExit;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ExecutionShared 
 * </h1>
 * <p>
 * 
 * Questa classe modella le funzionalità comuni sia alle classi di gestioni dei processi sia
 * a quelle di gestione delle funzioni.<br>
 * Le classi di gestione di processi e funzioni ereditano da questa classe.<br>
 * Sono presenti funzionalità comuni di garbage memoria, logging informazioni
 * applicative e di sistema come stack trace in caso di abend, attivazione della
 * user exit applicativa e altro ancora.<br>
 * <p>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 1/04/2010
 * @see ProcessAnalyzeSource
 * @see ProcessPgmLevel
 * @see ProcessSystemLevel
 * @see FunctionLibraryScan
 * @see FunctionPgmSummary
 * @see FunctionSystemSummary
*/
public class ExecutionShared implements AmritaConstants {

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi centralizzati e a oggetti condivisi                    //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
 
	public UserConfiguration ucfg = null;          		    // Defaults e references globali come gestore messaggi, log etc	
	public UserExit ue = null;                               // User exit il cui nome è specificato in configurazione
 
    // ArrayList con descrittori completi esecuzione: l'ultimo elemento è FUNCTION_SOURCE_DETECTED
    public ExecutionDirectives di = null;                        // Direttiva PROCESS/FUNCTION corrente per il processo/funzione in esecuzione
    public ArrayList<ExecutionDirectives> al_di = null;          // Insieme di direttive correnti
	
    
    ///////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza comuni a tutti i processi/funzioni
	///////////////////////////////////////////////////////////////////////////////////
		
	// Varie
	public String arParm[] = null;					// Parametri messaggi
	

	
	/**
	 * 
	 * Costruttore vuoto
	 */
	public ExecutionShared()  {
		super();
		this.ue = new UserExitDefault();
 	}

	
	/**
	 * 
	 * Costruttore 1
	 * 
	 * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public ExecutionShared(UserConfiguration ucfg)  {
		// Defaults reference ai servizi condivisi
		this.ucfg = ucfg;
		this.ue = new UserExitDefault();
 	}

	/**
	 * 
	 * Costruttore 2
	 * 
	 * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public ExecutionShared(UserConfiguration sd, ExecutionDirectives di)  {
		this(sd);
		this.di = di;
 	}

	
	/*
	 *  Logging informazioni sulla memoria disponibile
	 * 		
     */
	public void loggingMemoryStatus() {
		Runtime r = Runtime.getRuntime();
		Long freeMemory = r.freeMemory();
		Long maxMemory = r.maxMemory();
		Long totalMemory = r.totalMemory();
		String[] arParm = new String[3];
		arParm[0] = freeMemory.toString();
		arParm[1] = totalMemory.toString();
		arParm[2] = maxMemory.toString();
		// Memoria disponibile ...
		AmritaStartup.lf.writeRow(EnumMessageType.DEBUG, "DG0001", arParm, null); 
	}

	/*
	 *  Free memory  
	 * 		
     */
	public void freeMemory(Long thresold) {
		
		Runtime r = Runtime.getRuntime();
		
		if (thresold == 0 ) {return;}
		Long freeMemory = r.freeMemory();
		if (freeMemory >= thresold) {return;}  
		
		// Rilascio memoria
		r.gc();
		AmritaStartup.lf.writeRow(EnumMessageType.DEBUG, "DG0002", null, null); 

	}

	/*
	 *  Servizio centralizzato di logging messaggio senza produzione di printStackTrace
	 * 		
     */
	public void logMessage(EnumMessageType messageType, String messageKey, String ... ar_Parm) {

		if (ar_Parm.length == 0) {
			AmritaStartup.lf.writeRow(messageType, messageKey, null, null); 
		} else {
			AmritaStartup.lf.writeRow(messageType, messageKey, ar_Parm, null); 
		}
	}
    
	/*
	 *  Restituisce il messaggio attualizzato come verrebbe scritto nel log
	 * 		
     */
	public String logMessageToString(EnumMessageType messageType, String messageKey, String ... ar_Parm) {      
		return AmritaStartup.lf.getMsgActualized(messageType, messageKey, ar_Parm);
	}
  
	/*
	 *  Servizio centralizzato di logging messaggio CON produzione di printStackTrace
	 * 		
     */
	public void logMessage(EnumMessageType messageType, String messageKey, Exception excp, String ... ar_Parm) {
 
		if (ar_Parm == null || ar_Parm.length == 0) {
			AmritaStartup.lf.writeRow(messageType, messageKey, null, excp); 
		} else {
			AmritaStartup.lf.writeRow(messageType, messageKey, ar_Parm, excp); 
		}
	}
	
	/*
	 *  Restitisce un messaggio in lingua 
	 * 		
     */
	public String getMessage(EnumModule module, EnumMessageType messageType, String keyMessage) {
		return AmritaStartup.mm.getMessage(module, messageType, keyMessage);
	}
	
	/*
	 *  Restitisce un messaggio in lingua attualizzato con parametri
	 * 		
     */
	public String getMessageAtualized(EnumModule module, EnumMessageType messageType, String keyMessage, String ... ar_Parm) {
		return AmritaStartup.mm.getMessageActualized(module, messageType, keyMessage, ar_Parm);
	}
	
	/**
	 * Restituisce true se il nome soddisfa i criteri di filtro.
	 * Il nome del file deve iniziare per una delle stringhe in array di input
	 * @throws Exception  
	 */
	public boolean isNameMatchingFilterActive(ExecutionDirectives di, String nameToEvaluate) throws Exception  {
		
		UserExitInfo userExitinfo = null;
		boolean bRet = false;                  // Default NO match e file da NON considerare
		boolean bFilterAnd = true;             // True indica filtri superati: sorgente da considerare
		int posFilter = 0;					   //	
		String valueFilterToMatch = "";		   //
		
		
		// Gestione filtri disabilitata: return
		if (di.en_filterType == EnumDirectivesExecution.FILTER_DISABLED) {
			return true;
		}

		// Posizione e valore Filtro nel nome file gestito da exit parametrica esterna
		if (di.en_filterType == EnumDirectivesExecution.FILTER_BY_EXIT) {
			userExitinfo = userExitEvaluateFilters(di, nameToEvaluate);
			return userExitinfo.isMatchingFilter();
		}
		
		// Filtri gestiti da posizione e valore.
		// Gli entry dello stesso filtro sono in AND mentre gli entry sono in OR
        // Verifico se il nome sorgente passa i criteri di filtro
		if (di.en_filterType == EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_POS_VALUE) {
			// Scan filtri
			for (FilterEntry ife : di.al_filterEntry) {
				
				bFilterAnd = true; 
				
				// Scan entry filtro corrente
				for (int i = 0; i < ife.arPosFilter.length; i++) {
					
					posFilter = ife.arPosFilter[i];
					valueFilterToMatch = ife.arValueFilterToMath[i].trim();
					
					// Test validità dati di filtro in input
				    if ((valueFilterToMatch.length() + posFilter - 1) > nameToEvaluate.length()) {
					   logMessage(EnumMessageType.INFORMATION, "MW0003", nameToEvaluate, "" + posFilter, valueFilterToMatch);
				       break; 					// Applicazione filtro successivo
				    }
				    
			    	// Test applicazione filtro su name
			    	if (!nameToEvaluate.startsWith(valueFilterToMatch, posFilter - 1)) {
			    		bFilterAnd = false; 
			    		break;
					}
			    	
			    	// Analizzo successivo filtro in AND
			    	
				} // end-for
				
				// Tutti i filtri in AND sul nome sono stati superati: sorgente da considerare
				if (bFilterAnd) {
					bRet = true;
					break;
				}
				
				// Analizzo successivo filtro in OR
				
			} // end-for
			return bRet;
		}

		// Filtro come range di valori.
		// Se non sono specificati i limiti di range il filtro non ha effetto.
		if (di.en_filterType == EnumDirectivesExecution.FILTER_ON_OBJECT_NAME_RANGE) {
			// Il nome sorgente passa
			if (di.filterRangeObjectNameFrom.equals("") 
			||  di.filterRangeObjectNameTo.equals("")) {
				return true;
			}
			
			// Nome da valutare nel range di filtro
			if (nameToEvaluate.compareToIgnoreCase(di.filterRangeObjectNameFrom) >= 0
			&&  nameToEvaluate.compareToIgnoreCase(di.filterRangeObjectNameTo) <= 0) {
				return true;
			}
		}
		
		return bRet;
	}


	/**
	 * Restituisce true se il nome soddisfa i criteri di esclusione singola e di range.
	 * 
	 */
	public boolean isNameToExclude(ExecutionDirectives di, String nameToEvaluate)  {
		
		// Esclusione singola
		if (di.al_excludeObjectName.size() > 0 
		&&  di.al_excludeObjectName.contains(nameToEvaluate) ) {
			return true;
		}
 
		// Nessun criterio di esclusione di range specificato
		if (di.excludeRangeObjectNameFrom.equals("") 
		||  di.excludeRangeObjectNameTo.equals("")) {
			return false;
		}
		
		// Nome da valutare nel range di esclusione
		if (nameToEvaluate.compareToIgnoreCase(di.excludeRangeObjectNameFrom) >= 0
		&&  nameToEvaluate.compareToIgnoreCase(di.excludeRangeObjectNameTo) <= 0) {
			return true;
		}
		return false;
	}

	/** 
	 *  Restituisce l'enumerazione {@link EnumDirectivesExecution} che identifica
	 *  il processo o la funzine in esecuzione.
	 * 
	 */
	public EnumDirectivesExecution getRunningProcessFunction() {

		return this.di.en_CurProcessFunction;
	}
		


	/*
	 * 
	 * Attivazione user exit  per verifica se il nome supera i criteri di filtro
	 * 
	 */
	public UserExitInfo userExitEvaluateFilters(ExecutionDirectives di, String nameToEvaluate) throws ExceptionAmrita {
		
		Object[] ar_Object = null;
		Class[] ar_Class = null;
		ExceptionAmrita excp = null;
		UserExitInfo userExitInfo = null;
		
		// Recupero filtro
		if (di.en_filterType == EnumDirectivesExecution.FILTER_BY_EXIT) {
			userExitInfo = new UserExitInfo();
			userExitInfo.setRunningProcessFunction(getRunningProcessFunction());
			userExitInfo.setCustomerCode(di.curCustomerCode);
			userExitInfo.setCustomerInfo(di.curCustomerInfo);
			userExitInfo.setNameToEvaluate(nameToEvaluate);
			userExitInfo.setUserExitOperation(EnumUserExit.FILTER_ON_OBJECT_NAME_EVALUATE);
			ar_Class = new Class[1];
			ar_Class[0] = UserConfiguration.class;
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			// Recupero oggetto istanza di classe con user exit
			if (ue == null) {
				ue = (UserExit) userExitGetInstance();
			}
			// Gestione errore di invocazione, errore di caricamento
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
			    logMessage(EnumMessageType.ERROR_INTERNAL, "ET0006", excp, ucfg.getUserExitClass(), "executeExit");
			    throw excp;
			}
			AmritaStartup.rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);			
		}
		return userExitInfo;
	}
	
	/*
	 * 
	 * Attivazione user exit  per recupero sistema/sottosistema custom
	 * 
	 */

	public UserExitInfo userExitGetSystemSubsystem(ExecutionDirectives di, String nameToEvaluate, String sqlOwner, String sqlEntityName) throws ExceptionAmrita {
		
		Object[] ar_Object = null;
		Class[] ar_Class = null;
		ExceptionAmrita excp = null;
		UserExitInfo userExitInfo = null;
		int iStart = 0;
		int iEnd = 0;
		
		userExitInfo = new UserExitInfo();
		
		userExitInfo.setRunningProcessFunction(this.di.en_CurProcessFunction);
		userExitInfo.setCustomerCode(di.curCustomerCode);
		userExitInfo.setCustomerInfo(di.curCustomerInfo);
		userExitInfo.setLibraryCode(di.libraryCodeCurObj);
		userExitInfo.setNameToEvaluate(nameToEvaluate);
		userExitInfo.setSqlOwner(sqlOwner);
		userExitInfo.setSqlTableName(sqlEntityName);
		
        // System via user exit
		if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_BY_EXIT) {
			// Attivazione exit per recupero sistema e sottosistema
			userExitInfo.setUserExitOperation(EnumUserExit.GET_SYSTEM_OWNER);
			ar_Class = new Class[1];
			ar_Class[0] = userExitInfo.getClass();
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			
			// Recupero oggetto istanza di classe con user exit
			if (ue == null) {
				ue = (UserExit) userExitGetInstance();
				// Gestione errore di invocazione
				if (ue == null) {
			       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
				    logMessage(EnumMessageType.ERROR_INTERNAL, "ET0006", excp, ucfg.getUserExitClass(), "executeExit");
				    throw excp;
				}
			}
			AmritaStartup.rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
		}
		
        // System by Value
		if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_INPUT) {
			userExitInfo.setSystem(di.systemInput);
		}
		
        // System sul nome
		if (di.en_activeObjectSystem == EnumDirectivesExecution.SYSTEM_ON_NAME) {
			iStart = di.curSystemPos - 1;
			iEnd = iStart + di.curSystemLng;
			if (iEnd <= nameToEvaluate.length()) {
				userExitInfo.setSystem(nameToEvaluate.substring(iStart, iEnd));
			}
		}
		
		
        // Subsystem via user exit
		if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_BY_EXIT) {
			// Attivazione exit per recupero sistema e sottosistema
			userExitInfo.setUserExitOperation(EnumUserExit.GET_SUB_SYSTEM_OWNER);
			ar_Class = new Class[1];
			ar_Class[0] = userExitInfo.getClass();
			ar_Object = new Object[1];
			ar_Object[0] = userExitInfo;
			AmritaStartup.rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
			// Gestione errore di invocazione
			if (ue == null) {
		       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
			    logMessage(EnumMessageType.ERROR_INTERNAL, "ET0006", excp, ucfg.getUserExitClass(), "executeExit");
			    throw excp;
			}
		}
		
        // SubSystem by value
		if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_INPUT) {
			userExitInfo.setSubSystem(di.subSystemInput);
		}

        // SubSystem sul nome
		if (di.en_activeObjectSubSystem == EnumDirectivesExecution.SUB_SYSTEM_ON_NAME) {
			iStart = di.curSubSystemPos - 1;
			iEnd = iStart + di.curSubSystemLng;
			if (iEnd <= nameToEvaluate.length()) {
				userExitInfo.setSubSystem(nameToEvaluate.substring(iStart, iEnd));
			}
		}

		return userExitInfo;
	}

	/**
	 * 
	 * Attivazione generica user exit.<br>
	 * <p>
	 * Viene fornito l'oggetto UserExitInfo con le informazioni di attivazione.<br>
	 * Tale oggetto viene restituito aggiornato al chiamante con status etc. <br>
	 */
	public void userExitGenericActivate(UserExitInfo userExitInfo) throws ExceptionAmrita {
		
		Object[] ar_Object = null;
		Class[] ar_Class = null;
		ExceptionAmrita excp = null;

		// Recupero oggetto istanza di classe con user exit
		if (ue == null) {
			ue = (UserExit) userExitGetInstance();
		}
		
		// Completamento informazioni correnti di esecuzione
		userExitInfo.setRunningProcessFunction(this.di.en_CurProcessFunction);
		userExitInfo.setSystem(this.di.systemInput); 
		userExitInfo.setSubSystem(this.di.subSystemInput); 
		userExitInfo.setCustomerCode(this.di.curCustomerCode);
		userExitInfo.setCustomerInfo(this.di.curCustomerInfo);
		
		// Funzione impostata dal chiamante in userExitInfo
		ar_Class = new Class[1];
		ar_Class[0] = userExitInfo.getClass();
		ar_Object = new Object[1];
		ar_Object[0] = userExitInfo;
		AmritaStartup.rm.invokeMethod(ue, "executeExit", ar_Class, ar_Object);
		// Gestione errore di invocazione
		if (ue == null) {
	       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, null);
		    logMessage(EnumMessageType.ERROR_INTERNAL, "ET0006", excp, ucfg.getUserExitClass(), "executeExit");
		    throw excp;
		}
		return;
	}

	
	 /*
	  * Recupero una istanza dei UserExit
	  */
	private Object userExitGetInstance() throws ExceptionAmrita {

		Object ue = null;
			
		String classModel = "";
		String classExitNameComplete = "";
		ExceptionAmrita excp = null;
        if (AmritaStartup.rm == null) {
        	AmritaStartup.rm = new ReflectionManager();
		}
		// Determino il nome completo della classe, prendendo a modello questa corrente
		classModel = this.getClass().getName();
		int iStart = classModel.indexOf(this.getClass().getSimpleName());
		classExitNameComplete = classModel.substring(0, iStart) + this.di.userExitClass;

		ue = AmritaStartup.rm.newInstance(classExitNameComplete, null, null);
	 	ue = new analyzer.UserExitDefault();
		// Gestione errore di istanziazione		 	
		if (ue == null) {
	       	excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_REFLECTION_NEW_INSTANCE, null);
		    logMessage(EnumMessageType.ERROR_INTERNAL, "ET0005", excp, classExitNameComplete);
   		    throw excp;
		}

		return ue;
	}


	/**
     * Log informazioni di debug se presenti.
     * 
     * 
     */
	public void logApplicationInfoException() {
		
		AnalyzerCobol.InnerContextAnalysis ictx = null;
		Instruction instruction = null;
		
		// Nessuna informazione disponibile
		if (di.excpInfo == null) {
			return;
		}
		
		// Parsing in corso
		if (di.excpInfo instanceof AnalyzerCobol.InnerContextAnalysis) {
			ictx = (AnalyzerCobol.InnerContextAnalysis) di.excpInfo;
			if (ictx.objectInstr != null && ictx.objectInstr instanceof Instruction) {
				instruction = (Instruction) ictx.objectInstr;
				if (di.curProgramCobol != null) {
					logMessage(EnumMessageType.ERROR_INPUT, "EI0030", "Program:", di.curObjectId, instruction.getRowStartSource() + "", instruction.getSourceInstr());
				} else {
					logMessage(EnumMessageType.ERROR_INPUT, "EI0030", "Copy:", di.curObjectId, instruction.getRowStartSource() + "", instruction.getSourceInstr());
					
				}
			}
		}
	}

	/**
     * Memorizzazione errori/warning istruzioni su db
     * A fronte di exception
     * 
     */
	public void updateOnDbPendingErrorsWarningForException() {
		
		EntityObjectAnalysisError eoae = null;
		EntityObjectAnalysisInfo eoai = null;
		String sourceInstrNormalized = "";
		String stackTraceNormalized = "";
        String msg = "";

        // Durata analisi/processo
		this.di.curTimeMsEnd = System.currentTimeMillis();
		this.di.curTimeMsElapsed = this.di.curTimeMsEnd - this.di.curTimeMsStart;
		
		// UpdateOggetto db con info di analisi/process con dati correnti 
		// Elaborazione potrebbe non essere ancora iniziata per il primo oggetto
		if (this.di.curAnalyzerDbInfo == null) {
			return;
		}

		eoai=new EntityObjectAnalysisInfo();
		eoai = this.di.curAnalyzerDbInfo.getObjAnalysisInfo();
	   	updateOnDbPendingErrorsWarningDateTimeElapsed(eoai);
      
        // Conterrà l'entry con le informazioni di errore al momento dell'exception
	   	// Per tutti i tipi di elaborazione
		eoae = new EntityObjectAnalysisError();
        
		////////////////////////////////////////////////////////////////////////////////////////
		// Inserimento eventuali errori incontrati in analisi istruzioni in ObjectAnalysisError
		// Precedentemente all'exception
		///////////////////////////////////////////////////////////////////////////////////////
		
		// Info a fronte di analisi sorgenti
		if (this.di.en_CurProcessFunction == EnumDirectivesExecution.PROCESS_ANALYZE_SOURCES) {

			// Analisi di programma
			if (this.di.curProgramCobol != null) {
				storeOnDbParsingErrorsWarningDivision(this.di.curProgramCobol, this.di.curAnalyzerDbInfo, eoai);
			// Analisi di copy
			} else {
				eoae.setCopyName(this.di.curCopyCobol.getCopyName());				// Data analisi, ora di inizio/fine + elapsed in ms
				storeOnDbParsingErrorsWarningEntries(this.di.curCopyCobol.getEntries(), this.di.curCopyUnderAnalysis, this.di.curObjectType , this.di.curAnalyzerDbInfo, eoai);
			}   
		}

		////////////////////////////////////////////////////////////////////////////////////////
		// Inserimento errore dovuto a exception corrente
		///////////////////////////////////////////////////////////////////////////////////////		

		eoae.setSystem(this.di.systemInput);
		eoae.setSubSystem(this.di.subSystemInput);
		eoae.setIdObject(this.di.curObjectId);
		eoae.setTypeObject(this.di.curObjectType);
		
		// Info generali di exception comuni a tutte le elaborazioni
	   	eoae.setExcp(true);
		eoae.setTypeProcessAnalysis(this.di.curTypeProcessAnalysis);
	   	eoae.setExcpDescription(this.di.excpOccurred.toString());
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		this.di.excpOccurred.printStackTrace(pw);
		stackTraceNormalized = sw.toString();
		stackTraceNormalized = stackTraceNormalized.replace("\n", "@");
		stackTraceNormalized = stackTraceNormalized.replace("\r", "#");
		stackTraceNormalized = stackTraceNormalized.replace("\t", "!");
		stackTraceNormalized = stackTraceNormalized.replace("'", " ");
		eoae.setStackTrace(stackTraceNormalized);
		eoae.setExcpDescription(eoae.getExcpDescription().replace("'", " "));
		
	   	// Info specifiche a fronte di analisi sorgenti
		if (this.di.en_CurProcessFunction == EnumDirectivesExecution.PROCESS_ANALYZE_SOURCES) {
			
			// Informazioni istruzione sotto analisi NON disponibili
			if (this.di.curInstr == null) {
			   return;

			}
			
			// Update tipologia errore in entityObjectInfo  
			if (this.di.curInstr.getTypeInstrCategory() == EnumInstrDataCategory.COBOL_INSTRUCTION) {
				eoai.setWithAnyCobolParsingErrors(true);
				this.di.isExecutionWithErrors = true;
			} else if (this.di.curInstr.getTypeInstrCategory() == EnumInstrDataCategory.SQL_PRECOMPILER) {
				eoai.setWithAnySqlParsingErrors(true);
				this.di.isExecutionWithErrors = true;
			} else if (this.di.curInstr.getTypeInstrCategory() == EnumInstrDataCategory.CICS_PRECOMPILER) {
				eoai.setWithAnyCicsParsingErrors(true);
				this.di.isExecutionWithErrors = true;
			}

			// Info disponibili in fase di normalizzazione sorgente e analisi
	        eoae.setRowNum(this.di.curNumRowSource);

		   	// Info disponibili a fronte di analisi istruzione
			if (this.di.curTypeProcessAnalysis == EnumTypeProcessAnalysis.PARSING_SOURCE) {
				if (!this.di.curCopyUnderAnalysis.equals("")) {
					eoae.setExcpWhenCopyAnalysis(true);
				}
				eoae.setTypeInstr(this.di.curTypeInstr);
				eoae.setNumInstr(this.di.curNumInstr);
				eoae.setActiveCobolDivision(this.di.curCobolDivision);
				eoae.setActiveCobolSection(this.di.curCobolSection);
				sourceInstrNormalized = this.di.curInstr.getSourceInstr().replace("'", "''");
				eoae.setSourceInstr(sourceInstrNormalized);
				if (this.di.lastInstrGood != null) {
					sourceInstrNormalized = this.di.lastInstrGood.getSourceInstr().replace("'", "''");
					eoae.setSourceInstrLastGood(sourceInstrNormalized);
				}
				eoae.setRowNumInstrBegin(this.di.curInstr.getRowStartSource());
				eoae.setRowNumInstrFinal(this.di.curInstr.getRowEndSource());
				eoae.setSourceInstr(sourceInstrNormalized);
				eoae.setTokenError(this.di.curInstr.getTokenInError());
				eoae.setRowNumInstrBegin(this.di.curInstr.getRowStartSource());
				eoae.setRowNumInstrFinal(this.di.curInstr.getRowEndSource());
				eoae.setMsgType(this.di.curInstr.getMsgType());
				eoae.setMsgCode(this.di.curInstr.getMsgCode());
				msg = getMessageAtualized(EnumModule.ANALYZER, this.di.curInstr.getMsgType(), this.di.curInstr.getMsgCode(), this.di.curInstr.getMsgParm());
				eoae.setMsgText(msg);
				eoae.setParsingError(this.di.curInstr.isParsingError());
				eoae.setSemanticError(this.di.curInstr.isSemanticError());
				eoae.setWarning(this.di.curInstr.isWarning());
				eoae.setCopyName("");
				
				// Inserimento in struttura db X update a fine elaborazione
				this.di.curAnalyzerDbInfo.getObjsAnalysisError().add(eoae);
			}
			
			// Inserimento in struttura db X update a fine elaborazione
			this.di.curAnalyzerDbInfo.getObjsAnalysisError().add(eoae);

		}
		
		
		
		//////////////////////////////////////////////////////////////
		// Update effettivo db
		//////////////////////////////////////////////////////////////
	 
		// Esecuzione a fronte di exception
		if (this.di.excpOccurred != null) {
		    try {
		    	this.di.curAnalyzerDbInfo.sqlDeletePgmCopyLevelErrors(this.di.curObjectId);
				this.di.curAnalyzerDbInfo.updateInfoAndErrors(true, null);
			} catch (Exception e) {
				this.di.isRecursiveException = true;
			}
			return;
		}
		
  	}
    
	/**
	 * Update data analisi, ora di inizio/fine ed elapsed.<br>
	 * <p>
	 * Si aggiorna direttamete l'oggetto database con le info di analisi e dimensionali.<br>
	 * Metodo richiamato a fronte di exception e in fase normale di update db, afine analisi.<br>
	 * L'ora di ini/fine e l'elapsed time sono già disponibili denne direttive di esecuzione.<br>
	 */
	public void updateOnDbPendingErrorsWarningDateTimeElapsed(EntityObjectAnalysisInfo eoai) {
		
		Date dt = null;
		
    	eoai.setDtAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
    	dt = new Date(this.di.curTimeMsStart);
	   	eoai.setTmStartAnalysis(DateTimeService.getTimeFormatted(dt, "hhmmssSS"));
	   	dt = new Date(this.di.curTimeMsEnd);
	   	eoai.setTmEndAnalysis(DateTimeService.getTimeFormatted(dt, "hhmmssSS"));
		eoai.setMsElapsedTot((int) this.di.curTimeMsElapsed);
	}


	/*
	 * Inserimento informazioni su istruzioni errate con tutte le informazioni disponibili
	 * al momento del parsing, su db, per tutte le divisioni cobol
	 * 
	 * Questo metodo vieene richiamato anche a fronte di exception e le informazioni
	 * sono disponibili nelle direttive di esecuzione correnti.
	 */
	public void storeOnDbParsingErrorsWarningDivision(ProgramCobol programCobol
										    , AnalyzerDbInfo analyzerDbInfo
											, EntityObjectAnalysisInfo objDbAnalysisInfo
										     ) {
		
		ProgramCobolEntry<? extends Instruction>[] ar_entries = null;
		
		// Identification Division
		ar_entries = programCobol.entriesIdentification();
		storeOnDbParsingErrorsWarningEntries(ar_entries, programCobol.programName, EnumObject.OBJECT_PGM_COBOL,  analyzerDbInfo, objDbAnalysisInfo);

		// Environment Division
		ar_entries = programCobol.entriesEnvironment();
		storeOnDbParsingErrorsWarningEntries(ar_entries, programCobol.programName, EnumObject.OBJECT_PGM_COBOL,  analyzerDbInfo, objDbAnalysisInfo);
		

		// Data Division
		ar_entries = programCobol.entriesData();
		storeOnDbParsingErrorsWarningEntries(ar_entries, programCobol.programName, EnumObject.OBJECT_PGM_COBOL,  analyzerDbInfo, objDbAnalysisInfo);
		
		// Procedure Division
		ar_entries = programCobol.entriesProcedure();
		storeOnDbParsingErrorsWarningEntries(ar_entries, programCobol.programName, EnumObject.OBJECT_PGM_COBOL,  analyzerDbInfo, objDbAnalysisInfo);
		
	}
	

  
	/*
	 *  Logging istruzioni errate o con warning di specifica divisione cobol.
	 *  Riceve in input un array di ProgramCobolEntry<> con gli entry di una specifica divisione Cobol
	 *  o di un modulo copy di procedure division/data division
	 */
	private void storeOnDbParsingErrorsWarningEntries(ProgramCobolEntry<? extends Instruction>[] ar_entries
												 	, String sourceIdUnderParsing
												 	, EnumObject typeObjectUnderParsing
													, AnalyzerDbInfo analyzerDbInfo
													, EntityObjectAnalysisInfo objDbAnalysisInfo
												     ) {
		
		EntityObjectAnalysisError oae = null;
		Instruction instrGeneric = null;
        String msg = "";
		String instrNormalized = "";
        
		// Scan Istruzioni  
		for (ProgramCobolEntry<? extends Instruction> entryInstrucion : ar_entries) {
			
		    oae = null;
			instrGeneric = entryInstrucion.getInstruction();
			
			// Istruzione parsata e analizzata senza errori: continue
			if (!instrGeneric.isParsingError() 
			&&  !instrGeneric.isSemanticError()
			&&  !instrGeneric.isWarning()) {
				continue;
			}
			
			// Errore di parsing/analisi
			
			// Inserimento informazioni da istruzione
			oae = new EntityObjectAnalysisError();
			
			// Update primary key ai valori del programma sotto analisi
			oae.setSystem(this.di.systemInput);
			oae.setSubSystem(this.di.subSystemInput);
			oae.setIdObject(sourceIdUnderParsing);
			oae.setTypeObject(typeObjectUnderParsing);
			oae.setRowNum(instrGeneric.getRowStartSource());

			// Update informazioni correnti errore
			oae.setExcp(false);
			oae.setTypeProcessAnalysis(EnumTypeProcessAnalysis.PARSING_SOURCE);
			oae.setActiveCobolDivision(entryInstrucion.getProgramDivision());
			oae.setActiveCobolSection(entryInstrucion.getProgramSection());
			oae.setTypeInstr(instrGeneric.getTypeInstrCategory());
			instrNormalized = instrGeneric.getSourceInstr().replace("'", "''");
			oae.setNumInstr(instrGeneric.getNumInstr());
			oae.setSourceInstr(instrNormalized);
			oae.setTokenError(instrGeneric.getTokenInError());
			oae.setRowNumInstrBegin(instrGeneric.getRowStartSource());
			oae.setRowNumInstrFinal(instrGeneric.getRowEndSource());
			oae.setMsgType(instrGeneric.getMsgType());
			oae.setMsgCode(instrGeneric.getMsgCode());
			msg = getMessageAtualized(EnumModule.ANALYZER, instrGeneric.getMsgType(), instrGeneric.getMsgCode(), instrGeneric.getMsgParm());
			oae.setMsgText(msg);
			oae.setParsingError(instrGeneric.isParsingError());
			oae.setSemanticError(instrGeneric.isSemanticError());
			oae.setWarning(instrGeneric.isWarning());
			oae.setCopyName(entryInstrucion.getUnderCopyName());

			// Update tipologia errore in entityObjectInfo  
			if (instrGeneric.getTypeInstrCategory() == EnumInstrDataCategory.COBOL_INSTRUCTION) {
				objDbAnalysisInfo.setWithAnyCobolParsingErrors(true);
			} else if (instrGeneric.getTypeInstrCategory() == EnumInstrDataCategory.SQL_PRECOMPILER) {
				objDbAnalysisInfo.setWithAnySqlParsingErrors(true);
			} else if (instrGeneric.getTypeInstrCategory() == EnumInstrDataCategory.CICS_PRECOMPILER) {
				objDbAnalysisInfo.setWithAnyCicsParsingErrors(true);
			} else if (instrGeneric.getTypeInstrCategory() == EnumInstrDataCategory.DL1_PRECOMPILER) {
				objDbAnalysisInfo.setWithAnyDL1ParsingErrors(true);
			}
			
			// Inserimento in struttura db X update a fine elaborazione
		    analyzerDbInfo.getObjsAnalysisError().add(oae);
			
		} // end-for
	}


	
    /**
     * Produzione informazioni specifiche tecniche abend, tra cui lo stack trace
     * 
     */
    public void logSystemInfoException() {
    	
    	if (this.di.excpOccurred == null) {
			return;
		}
    	
		// Exception lanciata in modo controllato dalla funzione eseguita
        if (this.di.excpOccurred instanceof ExceptionAmrita ) {
			arParm = new String[2];
			arParm[0] = this.di.curObjectId;
			arParm[1] = this.di.toString();
			// Exception intercettata e gestita, stack trace già prodotto: l'elaborazione continua con la prossima funzione
			AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0019", arParm, this.di.excpOccurred);
			return;
		}
		 
        // Exception non prevista e non gestita: rilanciata al chiamante di esecuzione processi e funzioni
		arParm = new String[3];
		arParm[0] = "";
		arParm[1] = this.di.curObjectId;
		arParm[2] = this.di.excpOccurred.toString();
		// Exception non gestita: l'elaborazione interrotta
		if (this.di.curProgramCobol != null) {
			arParm[0] = "Program:";
		} else {
			arParm[0] = "Copy:";
		}  
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0018", arParm, this.di.excpOccurred);
	}

    /**
     * Produzione informazioni finali di esecuzione.<br>
     * <p>
     * Vengono loggate le seguenti informazioni relative alla direttiva
     * di esecuzione corrente:<br>
     * <p>
     * Counter oggetti trattati, con errori e in exception<br>
     * Elenco oggeetti con errori<br>
     * Elenco oggetti in exception<br>
     * <p>
     * 
     */
    public void logFinalExecutionStatistics() {
    	
        logMessage(EnumMessageType.INFORMATION, "MI0128", this.di.execCntObjectProcessed+"", this.di.execCntObjectProcessedError+"", this.di.execCntObjectProcessedExcp+"");

        ucfg.setExecCntObjectProcessed(this.di.execCntObjectProcessed);
        ucfg.setExecCntObjectProcessedError(this.di.execCntObjectProcessedError);
        ucfg.setExecCntObjectProcessedExcp(this.di.execCntObjectProcessedExcp);
        
        // Log elenco programmi analizzati con errori
        if (this.di.execCntObjectProcessedError > 0) {
        	logMessage(EnumMessageType.INFORMATION, "MI0131");
    		for (String pgmNameError : this.di.exec_alObjectNameError) {
    			logMessage(EnumMessageType.INFORMATION, "MI0133", pgmNameError);
    		}
    	}
        
        // Log elenco programmi analizzati terminati in exception
        if (this.di.execCntObjectProcessedExcp > 0) {
        	logMessage(EnumMessageType.INFORMATION, "MI0132");
    		for (String pgmNameExcp : this.di.exec_alObjectNameExcp) {
    			logMessage(EnumMessageType.INFORMATION, "MI0133", pgmNameExcp);
    		}
    	}

    }
    	
    
    /**
     * Restituisce un ArrayList di oggetti da processare a fronte delle direttive di esecuzione<br>
     * <p>
     * Si analizzanto le direttive di esecuzione e si considerano solo i tipi di oggetti forniti<br>
     * in input. Le direttive di esecuzione elencano tipologie di oggetti e specifici oggetti di<br>
     * una determinata tipologia. Tipologie di oggetti presenti nelle direttive e non dichiarati<br>
     * fra quelli da considerare validi, vengono segnalati.<br>
     * <p>
     * Ogni elemento restituito è descritto dalla classe interna {@link InnerDescriptorSource}
     * <p>
     * @throws Exception 
     * 
     */
    public ArrayList<InnerDescriptorSource> getObjectsToProcess(EnumObject ar_typeObjectGood[]
    	                                                      , EnumObjectStatus ar_typeObjectStatusGood[]
    				                                          , EnumObjectOption ar_objectOption[]                                            
    		) throws Exception {

    	ExecutionDirectives diCur = null;                        // Direttiva per il programma corrente

    	// Informazioni sorgente  
    	EnumObject objectTypeToProcess = null;
    	String objectModeToProcess = null;
    	String objectNameToProcess = null;
    	boolean isThereOption = false;
    	int i = 0;
    	int j = 0;
    	int k = 0;

    	
    	// Gestione intabellamento nomi sorgenti da analizzare
    	ArrayList<InnerDescriptorSource> al_ObjectToProcess = null;
    	ArrayList<InnerDescriptorSource> al_ObjectToProcessMassive = null;
    	InnerDescriptorSource innerSourceToAnalyze = null;


    	al_ObjectToProcess = new ArrayList<InnerDescriptorSource>();

    	// Scan ArrayList con direttive di esecuzione.
    	// Nel caso più semplice cè una sola direttiva OBJECTS_IDENTIFICATION_UNIT seguita dalla direttiva PROCESS_.....
    	// Nel caso più generale sono presenti più direttive di tipo OBJECTS_IDENTIFICATION_UNIT con eventuali informazioni di classificazione
    	// diverse, come sistema, sottosistema e filtri.

    	// Scan direttive singole unità di esecuzione  
    	for (k = 0; k < this.al_di.size(); k++) {

    		diCur = this.al_di.get(k);

    		if (diCur.en_CurProcessFunction == EnumDirectivesExecution.OBJECTS_IDENTIFICATION_UNIT) {


    			// Scan oggetti singoli e massivi di cui analizzare i sorgenti
    			for (i = 0; i < this.di.al_objectTypeToProcess.size(); i++) {

    				objectTypeToProcess = diCur.al_objectTypeToProcess.get(i).getSourceType().getObjectType();
    				objectModeToProcess = diCur.al_objectModeToProcess.get(i);
    				objectNameToProcess = diCur.al_objectNameToProcess.get(i);

    				// Controllo che siano tipi oggetti previsti
    				for (j = 0; j < ar_typeObjectGood.length; j++) {
    					if (objectTypeToProcess == ar_typeObjectGood[j]) {
    						break;
    					}
    				}
    				// Tipo oggetto selezionato non previsto da processo/funzione
    				if (j >= ar_typeObjectGood.length) {
    					logMessage(EnumMessageType.WARNING, "MW0012", objectNameToProcess, objectTypeToProcess.toString(), diCur.systemInput, diCur.subSystemInput);
    					continue;
    				}


    				// Oggetto singolo
    				if (objectModeToProcess.equals(TYPE_OBJECT_SINGLE)) {

    					innerSourceToAnalyze = getSourceSingleInfo(diCur, objectNameToProcess, objectTypeToProcess);

    					// L'oggetto da analizzare non è ancora stato classificato: logging e skip
    					if (innerSourceToAnalyze == null) {
    						logMessage(EnumMessageType.WARNING, "MW0005", objectNameToProcess, objectTypeToProcess.toString(), diCur.systemInput, diCur.subSystemInput);
    						continue;
    					}

    					// Controllo che lo stato dell'oggetto sia fra quelli previsti per il processo/funzione
    					for (j = 0; j < ar_typeObjectGood.length; j++) {
    						if (innerSourceToAnalyze.objectStatus == ar_typeObjectStatusGood[j]) {
    							break;
    						}
    					}
    					// Stato oggetto non permette elaborazioni: skip
    					if (j >= ar_typeObjectStatusGood.length) {
    						logMessage(EnumMessageType.WARNING, "MW0013", objectNameToProcess, objectTypeToProcess.toString(), innerSourceToAnalyze.idObject, diCur.systemInput, diCur.subSystemInput);
    						continue;
    					}

    					// Controllo presenza opzioni oggetto
    					if (ar_objectOption.length > 0) {
    						isThereOption = false;
    						for (int l = 0; l < ar_objectOption.length; l++) {
    							for (EnumObjectOption objectOption : innerSourceToAnalyze.ar_objectOption) {
    								if (ar_objectOption[l] == objectOption) {
    									isThereOption = true;
    									break;
    								}
    							}
    						}		    		
    						// Opzione oggetto richiesta non presente per l'oggetto selezionato: skip
    						if (!isThereOption) {
    							continue;
    						}
    					}


    					// Intabello informazioni programma da elaborare
    					al_ObjectToProcess.add(innerSourceToAnalyze);
    					continue;
    				} 

    				// Richiesta massiva oggetti dello stesso tipo
    				if (objectModeToProcess.equals(TYPE_OBJECT_ALL)) {

    					al_ObjectToProcessMassive = getSourceAllInfo(diCur, i, ar_typeObjectStatusGood);

    					// Intabello informazioni sorgente da analizzare
    					al_ObjectToProcess.addAll(al_ObjectToProcessMassive);
    				} 

    			} // end-for
    			
    		} // end if

    	} // end-for Scan direttive singole unità di esecuzione 

    	return al_ObjectToProcess;

    }
    
    /**
     * Aggiornamento stato oggetto con quello fornito
     * @throws ExceptionAmrita 
     * @throws SQLException 
     * @throws  
     * 
     * 
     */
    public void updateStatusCurrentObject(ExecutionDirectives di, EnumObjectStatus newStatus) throws  ExceptionAmrita, SQLException {
		
    	EntityObject entityObject = null;
    	boolean isCreatedOk = false;
    	
    	entityObject = new EntityObject();
    	
		// Primary key
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    						 
		entityObject.setIdObject(di.curObjectId);    						 
		entityObject.setTypeObject(di.curObjectType);				 
		entityObject.setTypeSource(getSourceType(entityObject.getTypeObject()));		 
		
		// Provo a inserire, potrebbe essere una analisi di un programma inesistente
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,true, ucfg);
		isCreatedOk = eoDAO.create(entityObject);
		if (isCreatedOk) {
			return;
		}
		
		// Lettura Object
		eoDAO.read(entityObject);
        entityObject.setStatus(newStatus);
        eoDAO.update(entityObject);
        
       conn.commit();
       DataBaseConnections.releaseConnection(conn);
       eoDAO.setConn(null);
    }
    
    
    /*
     * 
     * Lettura oggetti su Object per recupero nome sorgente e altre informazioni
     * 
    */
	private InnerDescriptorSource getSourceSingleInfo(ExecutionDirectives di, String objectNameToProcess, EnumObject objectTypeToProcess ) throws ExceptionAmrita, SQLException {
		
		// Entities, contenitori dei dati
		EntityObject entityObject = null;
		EntityObjectOption entityObjectOption = null;
		List<EntityObjectOption> al_objEntitityOption = null;
		List<EntityObject> al_objEntityObject = null;
		InnerDescriptorSource innerPgmToDoSummary = null;
        String libraryCode = "";
        String libraryPath = "";
        String sourceFileName = "";
        String suffixFileSource = "";
        @SuppressWarnings("unused")
		String sourceFilePath = "";
		EnumObjectStatus status = null;
    	String dtFirstAnalysis = "";	    		// Data prima analisi AAAAMMGG
    	String tmFirstAnalysis = "";	    		// Ora  prima analisi HHMMSSCC	
    	String dtLastAnalysis = "";	        		// Data ultima analisi AAAAMMGG
    	String tmLastAnalysis = "";	        		// Ora  ultima analisi HHMMSSCC	

    	Connection conn = DataBaseConnections.getConnection();
    	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
    	IDAOObjectOption eooDAO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false, false, ucfg);
    	
		// Lettura oggetto su Object per recupero path e nome sorgente
    	entityObject = new EntityObject();	
    	
		// Lettura Object: un oggetto con lo stesso nome può essere di un solo sottosistema   	
		al_objEntityObject = eoDAO.findProgramsByName(di.systemInput, objectNameToProcess, objectTypeToProcess);		
        if (al_objEntityObject.size() == 0) {
         	DataBaseConnections.releaseConnection(conn);
         	eoDAO.setConn(null);
         	return null;
		}
        
        entityObject=al_objEntityObject.get(0);
 		libraryCode = entityObject.getLibrarySourceObject();
		libraryPath = entityObject.getLibrarySource();
		sourceFileName = entityObject.getFileSource();
		suffixFileSource = entityObject.getSuffixFileSource();
		sourceFilePath = entityObject.getLibrarySource() + entityObject.getFileSource();
		status = entityObject.getStatus();
		dtFirstAnalysis = entityObject.getDtFirstAnalysis();
		tmFirstAnalysis = entityObject.getTmFirstAnalysis();
		dtLastAnalysis = entityObject.getDtLastAnalysis();
		tmLastAnalysis = entityObject.getTmLastAnalysis();
		
		
        innerPgmToDoSummary = new InnerDescriptorSource();
        innerPgmToDoSummary.sys=di.systemInput;
        innerPgmToDoSummary.subSys=entityObject.getSubSystem();
        innerPgmToDoSummary.subSysOwner=entityObject.getSubSystemOwner();
        innerPgmToDoSummary.di = di;
        innerPgmToDoSummary.idObject = objectNameToProcess;
        innerPgmToDoSummary.objectType = objectTypeToProcess;
		innerPgmToDoSummary.objectStatus = status;
        innerPgmToDoSummary.sourceFileName = sourceFileName;
        innerPgmToDoSummary.suffixFileSource = suffixFileSource;
 		innerPgmToDoSummary.sourcePath = libraryPath + File.separator + sourceFileName + (suffixFileSource.equals("") ? "" : "." + suffixFileSource);

		innerPgmToDoSummary.libraryCode = libraryCode;
		innerPgmToDoSummary.libraryPath = libraryPath;
		innerPgmToDoSummary.dtFirstAnalysis = dtFirstAnalysis;
		innerPgmToDoSummary.tmFirstAnalysis = tmFirstAnalysis;
		innerPgmToDoSummary.dtLastAnalysis = dtLastAnalysis;
		innerPgmToDoSummary.tmLastAnalysis = tmLastAnalysis;

		
		// Lettrura opzioni
   	    entityObjectOption = new EntityObjectOption();	
      	String whereCondition = "";
      
     	// Composizione Where di lettura opzioni oggetto
       	whereCondition =                  "     sys = '"        + this.di.systemInput    + "'";
     	whereCondition = whereCondition + " AND subSys = '"     + entityObject.getSubSystem() + "'";
     	whereCondition = whereCondition + " AND idObject = '"   + objectNameToProcess       + "'";
     	whereCondition = whereCondition + " AND typeObject =  " + objectTypeToProcess.ordinal();
    	
     	al_objEntitityOption =  eooDAO.findAllWhere(whereCondition, ""); 

     	innerPgmToDoSummary.ar_objectOption = new EnumObjectOption[al_objEntitityOption.size()];
     	 
     	// Scan option oggetto e caricamento in struttura
     	for (int i = 0; i < innerPgmToDoSummary.ar_objectOption.length; i++) {
     		entityObjectOption = (EntityObjectOption) al_objEntitityOption.get(i);
      		innerPgmToDoSummary.ar_objectOption[i] = entityObjectOption.getOption();
		}
     	
     	DataBaseConnections.releaseConnection(conn);
     	eoDAO.setConn(null);
     	eooDAO.setConn(null);
		return innerPgmToDoSummary;
	}

	/*
	 * Restituisce il tipo sorgente a fronte del tipo oggetto
	 */
    public EnumSourceType getSourceType(EnumObject typeObject) {
    	EnumSourceType sourceType = null;
		switch (typeObject) {
		case OBJECT_PGM_COBOL:
			sourceType = EnumSourceType.COBOL_PROGRAM;
			break;
		case OBJECT_COPY_COBOL_DATA:
			sourceType = EnumSourceType.COBOL_COPY_DATA;
			break;
		case OBJECT_COPY_COBOL_PROC:
			sourceType = EnumSourceType.COBOL_COPY_PROC;
			break;
		case OBJECT_CICS_BMS_SOURCE:
			sourceType = EnumSourceType.CICS_BMS;
			break;
		case OBJECT_JCL_JOB:
			sourceType = EnumSourceType.JCL_MVS_JOB;
			break;
		case OBJECT_JCL_INCLUDE:
			sourceType = EnumSourceType.JCL_MVS_INCLUDE;
			break;
		case OBJECT_JCL_PROC:
			sourceType = EnumSourceType.JCL_MVS_PROC;
			break;
		default:
			sourceType = EnumSourceType.NOT_ASSIGNED;
			break;
		}
		return sourceType;
	}


	/* ----------------------------------------------------------------------------------------
     * Lettura oggetti massivi su Object per recupero nome sorgente e file completo per analisi.
     * ----------------------------------------------------------------------------------------
     * 
     * Vengono considerati solo gli oggetti negli stati previsti dai parametri di direttiva.
     * Il metodo viene chiamato per uno specifico tipo oggetto
     * Una volta individuati gli oggetti negli stati previsti si applicano eventuali filtri attivi sui nomi degli oggetti.
     * Il sistema e il sottosistema di input su cui operare sono parametri di direttiva di esecuzione principali e forniti.
     * 
    */
	private ArrayList<InnerDescriptorSource> getSourceAllInfo(ExecutionDirectives diCur, int iTypeObject, EnumObjectStatus[] ar_typeObjectStatusGood) throws Exception {
		
		ResultSet rs = null;
		InnerDescriptorSource innerPgmToDoSummary = null;
		ArrayList<InnerDescriptorSource> al_ObjectToProcessMassive = null;
		EntityObjectOption entityObjectOption = null;
		List<EntityObjectOption> al_objEntitityOption = null;
		EnumObject objectType = null;
        String strSql = "";
        String whereCondition = "";
        String sys="";
        String subSys="";
        String sysOwner="";
        String subSysOwner="";
        String idObject = "";
        String libraryCode = "";
        String libraryPath = "";
        String sourceFileName = "";
        String suffixFileSource = "";
        EnumObjectStatus status = null;
    	String dtFirstAnalysis = "";	    		// Data prima analisi AAAAMMGG
    	String tmFirstAnalysis = "";	    		// Ora  prima analisi HHMMSSCC	
    	String dtLastAnalysis = "";	        		// Data ultima analisi AAAAMMGG
    	String tmLastAnalysis = "";	        		// Ora  ultima analisi HHMMSSCC	
    	String strOr = "";
    	int statusNumeric = 0;
		int statusOrdinal = 0;
    	int i = 0;
		
    	
    	// Il sistema e il sottosistema degli oggetti da recuperare, o tutti se *ALL*.
    	// è specificato nel pilot di esecuzione
     	
 		al_ObjectToProcessMassive = new ArrayList<InnerDescriptorSource>();
		
		// Composizione Select di lettura EntityObject
		strSql = "SELECT sys, subSys, idObject, librarySourceObject, librarySource, fileSource, suffixFileSource, statusObject, dtFirstAnalysis, tmFirstAnalysis, dtLastAnalysis, tmLastAnalysis, sysOwner, subSysOwner "; 
		strSql = strSql + "  FROM Object WHERE   ";
		
		// Condizione sys/subsys
		strSql = strSql +	  " sys = '" + diCur.systemInput + "'";
		
		// Un solo subsys dichiarato
		if (diCur.ar_subSystemInput.length == 1) {
			// Si richiede solo un subSys
			if (!diCur.subSystemInput.equals( "*ALL*")) {
				strSql = strSql +	  " AND subSys = '" + diCur.ar_subSystemInput[0] + "'";
				strSql = strSql +	  " AND subSys = subSysOwner ";
			}
			// Si sono richiesti TUTTI i subSys
		// Richiesti subsys multipli
		} else {
			strSql = strSql +	  " AND (subSys  = '"  +  diCur.ar_subSystemInput[0] + "'";
			for (i = 1; i < diCur.ar_subSystemInput.length; i++) {
				strSql = strSql +	  " OR  (subSys = '"  +  diCur.ar_subSystemInput[i] + "'  AND AND subSys = subSysOwner ";
			}
			strSql = strSql +	  " )";
		}
		
		// Condizione tipo oggetto
		objectType = diCur.al_objectTypeToProcess.get(iTypeObject).getSourceType().getObjectType();
		strSql = strSql + " AND  (typeObject = "  + objectType.ordinal() + ")";
		
		// Condizioni stato oggetto
		strSql = strSql + "  AND  (";
		for (EnumObjectStatus statusGood : ar_typeObjectStatusGood) {
			for (EnumObjectStatus statusEnum : EnumObjectStatus.values()) {
				if (!statusGood.toString().equals(statusEnum.toString()) ) {continue;}
				statusNumeric = statusEnum.ordinal();
				strSql = strSql + strOr + " statusObject = " + statusNumeric + " ";
				strOr = " OR ";
			}
 		}
		strSql = strSql + ")  ORDER BY idObject ASC;";
			
		// Esecuzione query e produzione  ResultSet
	    Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAOSqlGeneric = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false,false, this.ucfg);
		IDAOObjectOption eoDAOObjectOption = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false,false, this.ucfg);
       
		rs = eoDAOSqlGeneric.execSqlGeneric(strSql);

        // Scan valori trovati
		while (rs.next()) {
			
			sys = rs.getString(1);
			subSys = rs.getString(2);
			idObject = rs.getString(3);
			libraryCode = rs.getString(4);
			libraryPath = rs.getString(5);
			sourceFileName = rs.getString(6);
			suffixFileSource = rs.getString(7);
			statusOrdinal = rs.getInt(8);
			status = EnumObjectStatus.NOT_ASSIGNED;
            for (EnumObjectStatus innerStatus : EnumObjectStatus.values()) {
				if (statusOrdinal == i) {
					status = innerStatus;
					break;
				}
				i++; 
			}
			dtFirstAnalysis = rs.getString(9);
			tmFirstAnalysis = rs.getString(10);
			dtLastAnalysis = rs.getString(11);
			tmLastAnalysis = rs.getString(12);
			sysOwner = rs.getString(13);
			subSysOwner = rs.getString(14);

			
			// Il nome oggetto non soddisfa i criteri di filtro: skip
			// Si utilizza il metodo standard di ExecutionShared
			if (!this.isNameMatchingFilterActive(diCur, idObject)) {
				continue;
			}
			
			// Il nome oggetto soddisfa i criteri di esclusione: skip
			// Si utilizza il metodo standard di ExecutionShared
			if (this.isNameToExclude(diCur, idObject)) {
				continue;
			}

			// Sorgente oggetto da analizzare
			innerPgmToDoSummary = new InnerDescriptorSource();
			innerPgmToDoSummary.di = diCur;
			innerPgmToDoSummary.sys = sys;
			innerPgmToDoSummary.subSys = subSys;
			innerPgmToDoSummary.sysOwner = sysOwner;
			innerPgmToDoSummary.subSysOwner = subSysOwner;
			innerPgmToDoSummary.idObject = idObject;
			innerPgmToDoSummary.objectType = objectType;
			innerPgmToDoSummary.objectStatus = status;
			innerPgmToDoSummary.sourceFileName = sourceFileName;
			innerPgmToDoSummary.suffixFileSource = suffixFileSource;
			innerPgmToDoSummary.sourcePath = libraryPath + File.separator + idObject + (suffixFileSource.equals("") ? "" : "." + suffixFileSource);
			innerPgmToDoSummary.libraryCode = libraryCode;
			innerPgmToDoSummary.libraryPath = libraryPath;
			innerPgmToDoSummary.dtFirstAnalysis = dtFirstAnalysis;
			innerPgmToDoSummary.tmFirstAnalysis = tmFirstAnalysis;
			innerPgmToDoSummary.dtLastAnalysis = dtLastAnalysis;
			innerPgmToDoSummary.tmLastAnalysis = tmLastAnalysis;

			// Lettura opzioni
	   	    entityObjectOption = new EntityObjectOption();	
	      	whereCondition = "";
	      
	     	// Composizione Where di lettura opzioni oggetto
	       	whereCondition =                  "     sys = '" + sys    + "'";
	     	whereCondition = whereCondition + " AND subSys = '" + subSys + "'";
	     	whereCondition = whereCondition + " AND idObject = '" + idObject                  + "'";
	     	whereCondition = whereCondition + " AND typeObject =  " + objectType.ordinal();
	      	
	     	al_objEntitityOption = eoDAOObjectOption.readSetEntityWhere(whereCondition, "");
	     	innerPgmToDoSummary.ar_objectOption = new EnumObjectOption[al_objEntitityOption.size()];
	     	
	     	// Scan option oggetto e caricamento in struttura
	     	for (i = 0; i < innerPgmToDoSummary.ar_objectOption.length; i++) {
	     		entityObjectOption = (EntityObjectOption) al_objEntitityOption.get(i);
	      		innerPgmToDoSummary.ar_objectOption[i] = entityObjectOption.getOption();
			}
			
			// Accodamento oggetto da elaborare con tutte le sue caratteristiche
			al_ObjectToProcessMassive.add(innerPgmToDoSummary);
			
		} // end-while
		
		rs.close();
		
		DataBaseConnections.releaseConnection(conn);
		eoDAOSqlGeneric.setConn(null);
		eoDAOObjectOption.setConn(null);

		
		return al_ObjectToProcessMassive;
	}

	 /**
	 * 
	 * Serializzazione oggetto.<br>
	 * <p>
	 * L'oggetto  viene memorizzato su disco in formato serializzato.
	 * 
	 * @param String dirFileSerialized 		Directory file di output 
	 * @param String suffixFile 			Suffisso file di output (SUFFIX_SERIALIZED_COPY, SUFFIX_SERIALIZED_PGM, ..)
	 * @param String idObject 				Nome file di output 
	 * @param String objectToSerialize 		Istanza oggetto da serializzare
	 * @throws ExceptionAmrita 
	 */
	public void  putSerialized(String dirFileSerialized         				// Recuperata dal chiamante da SystemDefaults
			                 , String suffixFile                				// Passata dal chiamante da AmritaConstants
			                 , String idObject                  				// Nome Copy, programma, script sql..
			                 , Object objectToSerialize        					// Oggetto da serializzare
			                 , EnumAmritaExceptionError typeExeceptionError     // Tipo errore in exception
			                 , String codeMsgError								// Codice messaggio di errore
			                 , String codeMsgInfo								// Codice messaggio informativo
			                 ) throws ExceptionAmrita {
		
		ExceptionAmrita excp = null;
		ObjectOutputStream oos = null;
		String fileName = null;
		
		fileName = dirFileSerialized + idObject + "." + suffixFile;
		
		try {
			// Open del file
			oos = new ObjectOutputStream(new FileOutputStream(fileName));
			// Scrittura su file oggetto graph corrente
			oos.writeObject (objectToSerialize);
			// Chiususa file
			oos.close ();
 		} catch (Exception e) {
        	logMessage(EnumMessageType.ERROR_INTERNAL, codeMsgError, e, idObject, objectToSerialize.getClass().getName(), fileName);
			excp = new ExceptionAmrita(typeExeceptionError, e);
 			// Il main gestisce l'eccezione, fornisce info supplementari e decide se continuare l'elaborazione
            throw excp;
		}	
 		
 		// Serializzazione completata: log messaggio informativo
		logMessage(EnumMessageType.INFORMATION, codeMsgInfo, idObject, fileName, objectToSerialize.getClass().getName());
		return;
	}
	
	
	/*
	 * Inizializzazione informazioni di esecuzione 
	 */
    public void clearExecInfo() {
	    ucfg.setExecProcess(this.di.en_activeObjectSubSystem);
	    ucfg.setExecMsCurExpectedEnd(0);
	    ucfg.setExecMsAllExpectedEnd(0);
	    ucfg.setExecMsAvg(0);
	    ucfg.setExecMsMin(0);
	    ucfg.setExecMsMax(0);
	    ucfg.setExecCurIdObject("");
	    ucfg.setExecTotObjectToProcess(0);
	    ucfg.setExecCntObjectProcessed(0);
	}

    /*
     * Inserimento riga in Process Log
     */
	public void createProcessLog(int totObjectToAnalize) throws SQLException, ExceptionAmrita {
		Date dto = null;
		String dts = "";
		String tms = "";
        @SuppressWarnings("unused")
		boolean isCreateOK = false;
        
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOProcessLog eoDAO =  (DAOImplProcessLog) sqlFactory.getDAOProcessLog(conn, true, true, ucfg);
		EntityProcessLog pl = new EntityProcessLog();
		
		// PK
		pl.setUser(ucfg.getUser());
		pl.setSystem(di.systemInput);
		pl.setSubSystem(this.di.subSystemInput);
		pl.setIdObject(this.di.execCurIdObject);
		pl.setTypeObject(this.di.execCurTypeObject);
		pl.setTypeProcess(this.di.en_CurProcessFunction);
		dto = DateTimeService.getDate();
	    dts = DateTimeService.getDateFormatted(dto, DateTimeService.FORMAT_YYYYMMDD);
	    tms = DateTimeService.getTimeFormatted(dto, DateTimeService.TIME_DISPLAY_FORMAT_HHmmss);
		pl.setDtStartProcess(dts);
		pl.setTmStartProcess(tms);
		this.di.execCurDtStartProcess = dts;
		this.di.execCurTmStartProcess = tms;
		
		// Data
		pl.setStatusProcess(EnumProcessStatus.RUNNING);
		pl.setTmEndProcess("");
		pl.setMsDuration(0);
		pl.setMsgError("");
		pl.setIdExcpError("");
		pl.setIdExcpError("");
		pl.setExcpStackTrace("");
		pl.setThreadNameError("");
		pl.setJavaClassError("");

		isCreateOK = eoDAO.create(pl);
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		this.ucfg.setExecCurTypeObject(this.di.curObjectType);
		this.ucfg.setExecCurIdObject(this.di.curObjectId);
		this.ucfg.setExecCntObjectProcessed(this.di.execCntObjectProcessed);
		this.ucfg.setExecTotObjectToProcess(totObjectToAnalize);
		this.ucfg.setExecMsCurElapsed(0);
		this.ucfg.setExecMsCurExpectedEnd(0);
	}
		
	/*
     * Update riga in Process Log
     */
	public void updateProcessLog() throws SQLException, ExceptionAmrita {
		Date dto = null;
		String tms = "";
        @SuppressWarnings("unused")
		boolean isReadOK = false;
		@SuppressWarnings("unused")
		boolean isUpdateOK = false;
        
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOProcessLog eoDAO =  (DAOImplProcessLog) sqlFactory.getDAOProcessLog(conn, true, true, ucfg);
		EntityProcessLog pl = new EntityProcessLog();
		
		// PK
		pl.setUser(ucfg.getUser());
		pl.setSystem(di.systemInput);
		pl.setSubSystem(this.di.subSystemInput);
		pl.setIdObject(this.di.execCurIdObject);
		pl.setTypeObject(this.di.execCurTypeObject);
		pl.setTypeProcess(this.di.en_CurProcessFunction);
		pl.setDtStartProcess(this.di.execCurDtStartProcess);
		pl.setTmStartProcess(this.di.execCurTmStartProcess);
		
		isReadOK=eoDAO.read(pl);
		
		// Data
		pl.setStatusProcess(EnumProcessStatus.ENDED_WITH_NO_ERRORS);
		if (this.di.isExecutionWithErrors || this.di.isExecutionWithException) {
			pl.setStatusProcess(EnumProcessStatus.ENDED_WITH_ERRORS);
		}
		dto = DateTimeService.getDate();
	    tms = DateTimeService.getTimeFormatted(dto, DateTimeService.TIME_DISPLAY_FORMAT_HHmmss);
		pl.setTmEndProcess(tms);
		pl.setMsDuration(this.di.execMsCurElapsed);
		pl.setMsgError("");
		pl.setIdExcpError("");
		pl.setIdExcpError("");
		pl.setExcpStackTrace("");
		pl.setThreadNameError("");
		pl.setJavaClassError("");

		isUpdateOK = eoDAO.update(pl);
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);	
		
		//
		// Update valori ucfg visibili da web service ms cur,max,min, avg
		//

		this.ucfg.setExecProcessRunning(this.di.execProcessRunning);
		this.ucfg.setExecCntObjectProcessedNoError(this.di.execCntObjectProcessedNoError);
		this.ucfg.setExecCntObjectProcessedError(this.di.execCntObjectProcessedError);
		this.ucfg.setExecCntObjectProcessedExcp(this.di.execCntObjectProcessedExcp);
		this.ucfg.setExecMsCurElapsed(this.di.execMsCurElapsed);
		if (this.di.execMsCurElapsed > this.ucfg.getExecMsMax()) {
			this.ucfg.setExecMsMax(this.di.execMsCurElapsed);
			this.ucfg.setExecMsMaxIdObject(this.di.curObjectId);
		}
		if (this.di.execMsCurElapsed < this.ucfg.getExecMsMin() || this.ucfg.getExecMsMin() == 0) {
			this.ucfg.setExecMsMin(this.di.execMsCurElapsed);
			this.ucfg.setExecMsMinIdObject(this.di.curObjectId);
		}
		this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);	
		this.ucfg.setExecMsAvg(this.di.execMsAllElapsed / this.di.execCntObjectProcessed);
		this.ucfg.setExecCntObjectProcessedError(this.di.execCntObjectProcessedError);
		this.ucfg.setExecMsAllExpectedEnd((this.di.execMsAllElapsed / this.di.execCntObjectProcessed)
				                          * (this.di.execTotObjectToProcess - this.di.execCntObjectProcessed)
				);
		
		
	}
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
	/*
	 *   Classe contenitore di servizio con l'oggetto programma/copy e le informazioni per recuperare
	 *   il sorgente
	 *   
	 */
	public class InnerDescriptorSource {
		SourceInput sourceInput=null;              // Descrittore sorgente con path, sys, subsys rtc
		String sys="";                              // Sistema 
		String subSys="";                           // Sottosistema 
		String sysOwner="";                         // Sistema proprietario 
		String subSysOwner="";                      // Sottosistema 
		ExecutionDirectives di = null;				    // Direttiva di esecuzione attiva
		EnumObject objectType = null;				// Tipologia oggetto sorgente da analizzare
		EnumObjectStatus objectStatus = null;       // Status oggetto
		EnumObjectOption ar_objectOption[] = null;  // Opzioni oggetto
		String idObject = "";						// Nome oggetto (può coincidere con il nome sorgente senza postfisso)
		String sourceFileName = "";					// Nome sorgente completo senza eventuale postfisso
		String suffixFileSource = "";				// Suffisso dopo .
		String sourcePath = "";						// Path completo sorgente
        String libraryPath = "";                    // Dir libreria sorgente dove il source è stato individuato (LIBS di OBJT)
        String libraryCode = "";                    // Nome libreria (LIBO di OBJT)
    	String dtFirstAnalysis = "";	    		// Data prima analisi AAAAMMGG
    	String tmFirstAnalysis = "";	    		// Ora  prima analisi HHMMSSCC	
    	String dtLastAnalysis = "";	        		// Data ultima analisi AAAAMMGG
    	String tmLastAnalysis = "";	        		// Ora  ultima analisi HHMMSSCC	
	}
	

}
