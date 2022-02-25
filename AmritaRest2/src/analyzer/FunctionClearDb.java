package analyzer;
import java.util.ArrayList;
import enums.EnumMessageType;


/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ProcessClearDb
 * </h1>
 * <p>
 * 
 * Pulizia tabelle sistema/sottosistema
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 10/04/2010
 * @see ExecutionStarter
 * @see ExecutionShared
 * @see ExecutionLancherFunction
 * @see ExecutionLancherProcess
*/
public class FunctionClearDb extends ExecutionShared implements Runnable, AmritaConstants{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
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
	public FunctionClearDb(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
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
			
			exec();
			
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
	 * Per esempiovengono impostati l'ora di inzio, fine ed elapsed di elaborazione.<br>
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.<br>
	 * <p>
	 * @throws Exception 
	 * 
	 */
	public void exec() throws Exception {

		this.di.execMsAllStart = System.currentTimeMillis();
		this.di.isExecutionWithErrors = false;
		
		execClearDb();
			
		this.di.execMsAllEnd = System.currentTimeMillis();
		this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
		this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			
		return;
	}
	
	


	/**
	 * 
	 * Pulizia tabelle sistema/sottosistema<br>
	 * <p>
	 * Vengono svuotate tutte le tabelle del sistema/sottosistema attraverso la classe
	 * dedicata agli aggiornamenti {@link AnalyzeDbInfo}<br>
	 * <p>
	 * @throws Exception 
	 * 
	 */
	public void execClearDb() throws Exception {
		AnalyzerDbInfo adbi = new AnalyzerDbInfo(ucfg, di, "");
		
		adbi.sqlClearDb(di.systemInput, di.ar_subSystemInput, false, true);
		String x = "";
		for (String y : di.ar_subSystemInput) {
			x = x + y + " ";
		}
		// Elenco Sottosistemi processati
		logMessage(EnumMessageType.INFORMATION, "MI0303", this.di.systemInput, x);
		
  		return;

 	}


	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                      Metodi privati                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
}

