package analyzer;

import java.util.ArrayList;

/**
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * FunctionSystemSummary
 * </h1>
 * <p>
 * 
 * Vengono letti su OBJT gli oggetti i cui tipi sono specificati nella richiesta e filtrati dal file di filtro.
 * Per ogni oggetto vengono restituite due gruppi di informazione.
 * Il primo gruppo di informazioni è relativo a:
 * <p>
 * <Ul>
 * <Li> Data creazione sorgente		 
 * <Li> Data ultima modifica
 * <Li> Path sorgente
 * <Li> Nome sorgente
 * <Li> Tipo sorgente
 * <Li> Tipo oggetto
 * </Ul>
 * 
 * Il secondo gruppo di informazioni è relativo a:
 * <p>
 * <Ul>
 * <Li> Elenco completo relazioni, incluse quelle con i files in READ, UPDATE etc	
 * <Li> Elenco completo opzioni
 * <Li> Elenco sezioni/istruzioni di codice morto, non referenziate
 * </Ul>
 * 
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
public class FunctionSystemSummary extends ExecutionShared implements Runnable{

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza specifiche                                                                    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
 

	
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
	public FunctionSystemSummary(UserConfiguration sd, ArrayList<ExecutionDirectives> al_di) {
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
			
			execFunction();
			
		} catch (Exception e) {
			// L'esecuzione è in un thread separato, l'ora di fine elaborazione è già stata impostata da execFunction.
			// Nel caso di eccezione di tipo AmritaException, è già stata loggata e lo stack trace prodotto,
			// altrimenti tutto è a cura del chiamante AnalyzeManagerFunction.
			di.excpOccurred = e;
		}
		
	}
	

	/**
	 * 
	 * Funzione eseguita in modo sincrono o come thread separato.
	 * Questo metodo di ingresso permette di effettuare operazioni iniziali
	 * e finali relativi alla funzione da eseguire.
	 * Per esempiovengono impostati l'ora di inzio, fine ed elapsed di elaborazione.
	 * In caso di eccezione viene impostato il flag di funzione eseguita con errori
	 * nell'oggetto di direttiva di esecuzione DirectivesInfo.
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execFunction() throws Exception  {

		this.di.execMsAllStart = System.currentTimeMillis();
		this.di.isExecutionWithErrors = false;
		
		try {
			
			execObjectSummary();
			
			this.di.execMsAllEnd = System.currentTimeMillis();
			this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
			this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			
		} catch (Exception e) {
			this.di.excpOccurred = e;
			this.di.isExecutionWithErrors = true;
			this.di.execMsAllEnd = System.currentTimeMillis();
			this.di.execMsAllElapsed = (int) (this.di.execMsAllEnd - this.di.execMsAllStart);
			this.ucfg.setExecMsAllElapsed(this.di.execMsAllElapsed);
			// Se la funzione è stata lanciata in modo statico bisogna rilanciarla al chiamante
			// Se invece la funzione è stata chiamata dinamicamente con Invoke, il controllo torna
			// in ogni caso all'istruzione dopo la Invoke, dove l'oggetto DirectivesInfo di è disponibile
			// e contiene questa exception, che viene quindi gestita in quel punto (_ExecutionLauncherFunction)
			if (this.di.launchMethodMode == AmritaConstants.LAUNCH_METHOD_STATIC) {
				throw e;
			}
		}
		return;
	}
	
	

	/**
	 * 
	 * Processo eseguita in modo sincrono o come thread separato
	 *  
	 * @throws Exception 
	 * 
	 */
	public void execObjectSummary() throws Exception {
		
	
 		return;

 	}

	

	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	
}	
