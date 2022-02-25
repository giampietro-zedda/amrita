package analyzer;

import java.io.File;
import java.sql.SQLException;
import enums.EnumAmritaExceptionError;
import enums.EnumMessageType;
import exception.ExceptionAmrita;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ExecutionStarterWeb
 * </h1>
 * <p>
 * Questa classe rappresenta il programma di partenza per i servizi di impact analysis
 * richiamati dall'esterno, richiamata via WebService da web. 
 * L'unico metodo eseguibile è <strong>exec</strong> che attiva il dispatcher di elaborazione.
 * 
 * Il web service <b>executionImpactStart</b> viene richiamato da Impact.html con login già effettuato.
 * Le informazioni essenziali dell'utente necessarie all'elaborazione, sono presenti nell'oggetto UserConfiguration, passato al metodo di esecuzione.
 * Tutte le informazioni applicative, le opzioni etc, sono passate nell'oggetto DirectiveExecution con impostazioni che arrivano dal client.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 20/02/2010
 * @see Analyzer
 * @see ExecutionDispatcher
 * 
*/
public class ExecutionStarterImpactWeb {
	
	// Oggetti di gestione
	private ExecutionDispatcherWeb ed = null;			// Gestore attivazione process e funzioni
	private SourceManager sm = null;					// Gestore lettura/scrittura files sources 
	private SourceInput siPilotExecution = null;		// Container righe file pilot processi 
	
	// Tempi di esecuzione
	private long timeMsStart = 0;						// Inizio elaborazione
	private long timeMsEnd = 0;							// Fine elaborazione
	private long timeMsTotal = 0;						// Durata in millisecondi
	
	// Di servizio
	private String[] arParm = null;						// Parametri per errori
	private String[] arRowSourcePilot = null;			// Righe sorgente file pilota
	

	
	/**
	 * @param pilotName name of pilot including suffix
	 * @throws SQLException 
	 * @throws Exception 
	 * 
	 */
	public  void exec(UserConfiguration ucfg, String pilotName) throws ExceptionAmrita, SQLException {
        
		sm = new SourceManager(ucfg);
		
		//////////////////////////////////////////////////////////////////////////
        // Gestione pilot esecuzione
		//////////////////////////////////////////////////////////////////////////
        
		siPilotExecution = sm.getSource(ucfg.getPathUser() + File.separator + ucfg.getDirPilot() + File.separator +  pilotName, false, true);
		
		// File di pilot di esecuzione da eseguire inesistente
		if (siPilotExecution == null) {
			arParm = new String[1];
			arParm[0] = pilotName;
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0005", arParm, null);
			throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PILOT_PROCESS_FILE_NOT_FOUND, null);
		}
       
		//////////////////////////////////////////////////////////////////////////
        // Scrittura su log informazioni di ambiente e di inizio
		//////////////////////////////////////////////////////////////////////////
        
		// File pilota di esecuzione da eseguire
		arParm = new String[1];
		arParm[0] = siPilotExecution.getPathComplete();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0008", arParm, null);

		// Scan rughe file pilota di esecuzione
		arRowSourcePilot = siPilotExecution.getArrayRowSource();
		for (int i = 0; i < arRowSourcePilot.length; i++) {
			arParm = new String[1];
			arParm[0] = arRowSourcePilot[i];
			AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0011", arParm, null);
		}

		
		//////////////////////////////////////////////////////////////////////////
        // Attivazione gestore generalizzato processi e funzioni 
		//////////////////////////////////////////////////////////////////////////
		
		timeMsStart = System.currentTimeMillis();
		ucfg.setExecMsAllElapsed(0);
		ucfg.setExecMsAllStart(timeMsStart);
		ed = new ExecutionDispatcherWeb(ucfg, siPilotExecution);
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0013", null, null); // Inizio elaborazione processi ...
		
		try {
			ed.processMain();
		} catch (SQLException e) {
			// Errori già gestiti
		}
		
		timeMsEnd = System.currentTimeMillis();
		timeMsTotal = timeMsEnd - timeMsStart;
		
		ucfg.setExecMsAllEnd(timeMsEnd);
		ucfg.setExecMsAllElapsed(timeMsTotal);
		if (ucfg.getExecCntObjectProcessed() > 0) {
			ucfg.setExecMsAvg((int)timeMsTotal / ucfg.getExecCntObjectProcessed()); 
		} else {
			ucfg.setExecMsAvg(0); 
		}
		
		
		//////////////////////////////////////////////////////////////////////////
        // Scrittura su log informazioni di chiusura e consuntivo
		//////////////////////////////////////////////////////////////////////////
        
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0014", null, null); // Fine elaborazione processi ...
		Long timeElapsed = new Long(timeMsTotal);
		arParm = new String[1];
		arParm[0] = timeElapsed.toString();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0015", arParm, null); // Elapsed totale
		
		
	}
	
}
