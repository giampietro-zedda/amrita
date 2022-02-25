package analyzer;

import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import dao.DAOFactory;
import dao.IDAOImpactObject;
import dao.IDAOImpactPlan;
import dao.IDAOSqlGeneric;
import dao.MySQLDAOFactory;
import entities.EntityImpactObject;
import entities.EntityImpactPlan;
import entities.EntitySqlGeneric;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;
import utilities.SystemService;


/**
 * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ProcessImpactAnalysis
 * </h1>
 * <p>
 * 
 * Elaborazione analisi di impatto attivata via web service,
 * Vengono analizzate le operazione previste per l'impact plan memorizzate in ImpactPlan e si
 * individuano gli oggetti da modificare, a quali istruzioni e i pgm da ricompilare
 * 
 * L'elaborazione avviene in uno specifico thread.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 26/05/2021
 * @see ExecutionStarter
*/
public class ProcessImpactAnalysis extends ExecutionShared implements Runnable, AmritaConstants{
	String sys = "";                     	// System
	String idPlan = "";                     // Impact Plan da elaborare
	
	/**
	 * 
	 * Costruttore 
	 * 
	 * Le direttive di esecuzione, comuni a tutti i processi non sono al momento gestite. 
	 * Le informazioni generali si trovano nella UserConfiguration. 
	 * Il processo per partire ha bisogno solo dell'impact plan 
	 * 
	 */
	public ProcessImpactAnalysis(UserConfiguration ucfg, String idPlan, ExecutionDirectives di) {
		super(ucfg);	
		this.di = di;
		this.idPlan = idPlan;
		this.sys = ucfg.getSystemOwner();
 	}
	
	
   /**
    * 
    * Attivato a fronte dell'esecuzione di start() sul thread  da {@link RestImpactService} <br>
 	 * 
	 * Funzione eseguita come thread separato.<br>
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
		
		this.ucfg.setExcpOccurred(false);
		this.ucfg.setExecExcp(null);

		try {
			
			this.di.execProcessRunning = true;
			this.di.execStopRequired = false;
			this.di.isExecutionWithErrors = false;
			this.di.execMsAllStart = System.currentTimeMillis();
			this.ucfg.setExecMsAllStart(this.di.execMsAllStart);
			this.ucfg.setExecMsAllEnd(this.di.execMsAllStart);
			this.ucfg.setExecStopRequired(false);;
			this.ucfg.setExecProcessRunning(true);
			this.ucfg.setExecMsAllEnd(0);
			this.ucfg.setExecMsAllElapsed(0);
			
			execImpactAnalysis(sys, idPlan);
				
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
			this.ucfg.setExcpOccurred(true);
			this.ucfg.setExecExcp(e);
		}
		
	}

	/* Analisi programmi dell'impact plan */
	private void execImpactAnalysis(String sys, String idPlan) throws SQLException, ExceptionAmrita {
		List<EntityImpactObject> ls_impactObject = null;
		List<EntityImpactPlan> ls_impactPlan = null;
		EntityImpactPlan eoImpactPlan = null;
		ProgramCobol programCobol = null;
		String idPgmOld = "";
		String fieldColumn = "";
	    int rowStart = 0;
	    int rowEnd = 0;
	    int numInstr = 0;
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactObject daoImpactObject = (IDAOImpactObject) AmritaStartup.sqlFactory.getDAOImpactObject(conn, false,false, ucfg);
		IDAOImpactPlan daoImpactPlan = (IDAOImpactPlan) AmritaStartup.sqlFactory.getDAOImpactPlan(conn, false,false, ucfg);
	    
		ls_impactObject=daoImpactObject.findAll(sys, idPlan);
		
		this.ucfg.setExecTotObjectToProcess(0);
		idPgmOld = "";
		
		// Scan oggetti impact plan per trovare il numero di programmi coinvolti
		for (EntityImpactObject eo : ls_impactObject) {		
		    if (!idPgmOld.equals(eo.getIdObjectTarget())) {
		    	idPgmOld = eo.getIdObjectTarget();
		    	this.ucfg.setExecTotObjectToProcess(this.ucfg.getExecTotObjectToProcess() + 1);	    	
			}
		}	
		
		// Scan oggetti impact plan
		this.ucfg.setExecCntObjectProcessed(0);
		for (EntityImpactObject eoImpactObject : ls_impactObject) {
			
			// Richiesta di
			
			
		    if (!idPgmOld.equals(eoImpactObject.getIdObjectTarget())) {
		    	idPgmOld = eoImpactObject.getIdObjectTarget();
		    	this.ucfg.setExecCurIdObject(idPgmOld);
		    	this.ucfg.setExecCntObjectProcessed(this.ucfg.getExecCntObjectProcessed() + 1);
		    	
		    	// Recupero change operation se non già disponibile
		    	if (eoImpactPlan == null || (eoImpactPlan != null && eoImpactPlan.getNumOp() != eoImpactObject.getNumOp()))  {
			    	ls_impactPlan = daoImpactPlan.findAll(sys, idPlan, eoImpactObject.getNumOp());
			    	eoImpactPlan = ls_impactPlan.get(0);
				}
		    	
		    	// Get program serialized
		    	programCobol = getProgram(ucfg.getUser(), idPgmOld);	        		    	
			}	
		    
		    // Impostazioni per analisi supplementari
		    fieldColumn = eoImpactPlan.getFieldColumn();
		    rowStart = eoImpactObject.getRowStart();
		    rowEnd = eoImpactObject.getRowEnd();
		    numInstr = eoImpactObject.getNumInstr();
		    
		    // Attivazione analisi supplementare where used campo
		    execImpactAnalysisDetail(programCobol, fieldColumn, rowStart, rowEnd, numInstr);
		}
		
		DataBaseConnections.releaseConnection(conn);
		daoImpactObject.setConn(null);
		daoImpactPlan.setConn(null);
	}
	
	/*
	 * Analisi di dettaglio impatti supplementari campo/colonna a fronte di whereUsed a riga/col/istr fornito
	 * 
	 * - Per ogni where used si seguono le catene di trasformazione
	 *   - Per ogni trasformazione si verifica che il change(es allargamento campo) sia compatibile con la trasformazione.
	 *     - Se anche il campo trasformato deve essere modificato in size/int/dec viene aggiunto al change operation summary (impactPlan)
	 *     - Se il campo trasformato è dentro un copy si aggiunge il copy al change operation summary (impactPlan)
	 *   - Se l'ultima trasformazione è un campo di mappa/copy/tabella sql e necessita di change viene aggiunto al change operation summary (impactPlan)
	 */
	private void execImpactAnalysisDetail(ProgramCobol programCobol, String fieldColumn, int rowStart, int rowEnd, int numInstr) {
		ProgramCobolEntry<? extends Instruction>[] arProcEntries = null;
//		arProcEntries = programCobol.entriesProcedure();
		
		// TODO  
		
	}


	/*
	 * Recupero programma java serializzato da directory a livello di user
	 * 
	 */
	public ProgramCobol getProgram(String user, String idPgm) throws ExceptionAmrita  {
		UserConfiguration ucfg = null;
		SystemService ss = null;                  // Gestore generalizzato servizi di sistema
		ProgramCobol pgmCobol = null; 
		Object objUnserialized = null;

		
    	// Get program da cache se già trattato
		pgmCobol = (ProgramCobol) AmritaStartup.cache.get(idPgm);
        if (pgmCobol != null) {
        	return pgmCobol;
        }
         		
		// User mast be logged in
		// Login should be done	
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
			ucfg = new UserConfiguration(user);
		}

		// Get Program or Copy object
		ss = new SystemService(ucfg, null);
		ss.setSourceObjectContainer(idPgm);

		objUnserialized = ss.getSerialized(ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm(), idPgm, SUFFIX_SERIALIZED_PGM);

		// Programma serializzato non trovato, impossibile produrre documentazione: messaggio già fornito
		if (objUnserialized != null && objUnserialized instanceof ProgramCobol) {
			pgmCobol = (ProgramCobol) objUnserialized;

		}
		
		// Pgm in cache
		AmritaStartup.cache.put(idPgm, pgmCobol);
		return pgmCobol;

	}

	
}

