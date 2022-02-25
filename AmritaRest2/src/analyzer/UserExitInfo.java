package analyzer;

import enums.EnumCobolReservedWords;
import enums.EnumDirectivesExecution;
import enums.EnumObject;
import enums.EnumUserExit;

/**
* Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda Turin (ITALY)
* 
* <h1>
* UserExitInfo
* </h1>
*  <p>
* Vengono definite le informazioni fornite e restituite dalla exit applicativa codificata nella classe {@link UserExit}.<br>
* Si tratta di una classe contenitore.
* 
* 
* @author Giampietro Zedda
* @version 1.0.0
* @since 07/04/2010
* @see Analyzer
* @see EnumDirectivesSource
* @see EnumDirectivesProcess
* @see EnumUserExit
* @see AmritaConstants
*/

public class UserExitInfo {

	static final int USER_EXIT_RETURN_OK = 0;						// Dati restituiti senza errori, controlli superati senza errori
	static final int USER_EXIT_RETURN_ERROR = 1;					// Errori dettagliati dal messaggio
    
	// Info di servizio per il controllo
	private ProgramCobol programCobol = null;                       // Descrittore completo programma cobol
	private EnumUserExit userExitOperation = null;					// Tipo specifico operazione exit
	private EnumDirectivesExecution runningProcessFunction = null;  // Tipo processo o funziane in esecuzione
	private EnumObject activeObjectType = null;        				// Tipo oggetto attivo per processo o funzione
	private EnumCobolReservedWords activeCobolDivision  = null;     // Divisione cobol attiva
	private EnumCobolReservedWords activeCobolSection  = null;      // Sezione cobol attiva
	private String activeSectionName = "";                          // Nome section attiva sotto controllo 
	private String activeParagraphName = "";                        // Nome paragrafo attivo sotto controllo
	
	// Info applicative per il controllo
	private String customerCode = ""; 								// Identificativo cliente
	private String customerInfo = ""; 								// Info specifiche cliente presenti nel pilot di esecuzione
	private String libraryCode = "";								// Codice libreria dal quale proviene l'oggetto di cui valutare il nome
	private String scriptSqlName = "";								// Nome script sql come da file system
	private String nameToEvaluate = "";								// Nome sorgente/campo/label/.. o oggetto da cui estrarre sistema/sottosistema, criteri di filtro etc.
	private String system = "";			  							// Systema identificato dal nome programma 
	private String subSystem = "";    								// Sottosistema identificato dal nome programma  
	private String systemOwner = "";			  					// Systema proprietario, identificato in SCAN-LIBRARY o a fronte di analisi
	private String subSystemOwner = "";    							// Sottosistema proprietario, identificato in SCAN-LIBRARY  o a fronte di analisi
	private String sqlDbName = "";    				    			// Database name 
	private String sqlTablespaceName = "";    						// Tablespace name 
	private String sqlTableName = "";    				    		// Table name 
	private String sqlOwner = "";    					    		// Owner
	private boolean isMatchingFilter = false;   					// True significa filtro verificato: l'oggetto deve essere preso in considerazione
	private int returnCode = 0;    			    					// Return code  
	private String msgCodeError = "";    	        				// Eventuale codice messaggio di errore     
    
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public  UserExitInfo() {
		userExitOperation = EnumUserExit.NOT_ASSIGNED;
		runningProcessFunction = EnumDirectivesExecution.NOT_ASSIGNED;
		activeObjectType = EnumObject.NOT_ASSIGNED;
		activeCobolDivision = EnumCobolReservedWords.NOT_ASSIGNED;
		activeCobolSection = EnumCobolReservedWords.NOT_ASSIGNED;
	}

	/**
	 * Restituisce il tipo di operazione richiesta alla user exit.
	 * <p>
	 * @return the userExitOperation
	 */
	public EnumUserExit getUserExitOperation() {
		return userExitOperation;
	}

	/**
	 * Imposta il tipo di operazione richiesta alla user exit.
	 * <p>
	 * @param userExitOperation the userExitOperation to set
	 */
	public void setUserExitOperation(EnumUserExit userExitOperation) {
		this.userExitOperation = userExitOperation;
	}

	
	/**
	 * Restituisce il descrittore del programma Cobol.<br>
	 * <p>
	 * Attraverso il descrittore cobol è possibile risolvere qualsiasi 
	 * esigenza di controllo su campi, label etc.
	 * <br>
	 * @return the programCobol
	 */
	public ProgramCobol getProgramCobol() {
		return programCobol;
	}

	/**
	 * Imposta il descrittore del programma Cobol.<br>
	 * <p>
	 * Attraverso il descrittore cobol è possibile risolvere qualsiasi 
	 * esigenza di controllo su campi, label etc.
	 * <br>
	 * @param programCobol the programCobol to set
	 */
	public void setProgramCobol(ProgramCobol programCobol) {
		this.programCobol = programCobol;
	}

	/**
	 * Restituisce il tipo di processo o di funzione in esecuzione.
	 * <br>
	 * @return the runningProcessFunction
	 */
	public EnumDirectivesExecution getRunningProcessFunction() {
		return runningProcessFunction;
	}

	/**
	 * Imposta il tipo di processo o di funzione in esecuzione.
	 * <br>
	 * @param runningProcessFunction the runningProcessFunction to set
	 */
	public void setRunningProcessFunction(EnumDirectivesExecution runningProcessFunction) {
		this.runningProcessFunction = runningProcessFunction;
	}

	/**
	 * Restituisce il tipo di oggetto in corso di analisi/elaborazione da process o funzione in esecuzione.
	 * <br>
	 * @return the activeObjectType
	 */
	public EnumObject getActiveObjectType() {
		return activeObjectType;
	}

	/**
	 * Imposta il tipo di oggetto in corso di analisi/elaborazione da process o funzione in esecuzione.
	 * <br>
	 * @param activeObjectType the activeObjectType to set
	 */
	public void setActiveObjectType(EnumObject activeObjectType) {
		this.activeObjectType = activeObjectType;
	}

	/**
	 * Restituisce il nome, tipicamente il nome di un programma, 
	 * che deve soddisfare i criteri applicativi di filtro.<br>.
	 * Se i criteri di filtro sono validi e il nome deve considerarsi <br>
	 * valido, isMatchingFilter() deve restituire true.<br>
	 * <p>
	 * @return the nameToEvaluate
	 */
	public String getNameToEvaluate() {
		return nameToEvaluate;
	}

	/**
	 * Imposta il nome, tipicamente il nome di un programma, 
	 * che deve soddisfare i criteri applicativi di filtro.<br>.
	 * Se i criteri di filtro sono validi e il nome deve considerarsi <br>
	 * valido, isMatchingFilter() deve restituire true.<br>
	 * <p>
	 * @param nameToEvaluate the nameToEvaluate to set
	 */
	public void setNameToEvaluate(String nameToEvaluate) {
		this.nameToEvaluate = nameToEvaluate;
	}

	/**
	 * Restituisce il codice del livello di aggregazione degli oggetti più
	 * alto, ovvero il <b>sistema</b> <br> 
	 * <p>
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}

	/**
	 * Imposta il codice del livello di aggregazione degli oggetti più
	 * alto, ovvero il <b>sistema</b> <br> 
	 * <p>
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}

	/**
	 * Restituisce il codice del livello di aggregazione degli oggetti più
	 * alto, ovvero il <b>sistema</b> proprietario<br> 
	 * <p>
	 * @return the system
	 */
	public String getSystemOwner() {
		return systemOwner;
	}

	/**
	 * Imposta il codice del livello di aggregazione degli oggetti più
	 * alto, ovvero il <b>sistema</b> proprietario <br> 
	 * <p>
	 * @param systemOwner the systemOwner to set
	 */
	public void setSystemOwner(String systemOwner) {
		this.systemOwner = systemOwner;
	}

	/**
	 * Restituisce il codice del livello di aggregazione degli oggetti all'interno
	 * di un sistema, ovvero il <b>sottosistema</b> <br> 
	 * <p>
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}

	/**
	 * Imposta il codice del livello di aggregazione degli oggetti all'interno
	 * di un sistema, ovvero il <b>sottosistema</b> <br> 
	 * <p>
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}

	/**
	 * Restituisce il codice del livello di aggregazione degli oggetti all'interno
	 * di un sistema, ovvero il <b>sottosistema</b> proprietario<br> 
	 * <p>
	 * @return the subSystemOwner
	 */
	public String getSubSystemOwner() {
		return subSystemOwner;
	}

	/**
	 * Imposta il codice del livello di aggregazione degli oggetti all'interno
	 * di un sistema, ovvero il <b>sottosistema</b> proprietario<br> 
	 * <p>
	 * @param subSystemOwner the subSystemOwner to set
	 */
	public void setSubSystemOwner(String subSystemOwner) {
		this.subSystemOwner = subSystemOwner;
	}

	/**
	 * Restituisce il nome del database corrente, se disponibile<br> 
	 * <p>
	 * @return the sqlDbName
	 */
	public String getSqlDbName() {
		return sqlDbName;
	}

	/**
	 * Imposta il nome del database corrente<br> 
	 * <p>
	 * @param sqlDbName the sqlDbName to set
	 */
	public void setSqlDbName(String sqlDbName) {
		this.sqlDbName = sqlDbName;
	}

	/**
	 * Restituisce il nome del tablespace corrente, se disponibile<br> 
	 * <p>
	 * @return the sqlTablespaceName
	 */
	public String getSqlTablespaceName() {
		return sqlTablespaceName;
	}

	/**
	 * Imposta il nome del tablespace corrente<br> 
	 * <p>
	 * @param sqlTablespaceName the sqlTablespaceName to set
	 */
	public void setSqlTablespaceName(String sqlTablespaceName) {
		this.sqlTablespaceName = sqlTablespaceName;
	}

	
	/**
	 * Restituisce il nome della tablee corrente, se disponibile<br> 
	 * <p>
	 * @return the sqlTableName
	 */
	public String getSqlTableName() {
		return sqlTableName;
	}

	/**
	 * Imposta il nome della table corrente<br> 
	 * <p>
	 * @param sqlTableName the sqlTableName to set
	 */
	public void setSqlTableName(String sqlTableName) {
		this.sqlTableName = sqlTableName;
	}

	
	/**
	 * Restituisce l'owner attivo per l'oggetto sql.<br>
	 * <p>
	 * @return the sqlOwner
	 */
	public String getSqlOwner() {
		return sqlOwner;
	}

	/**
	 * Imposta l'owner attivo per l'oggetto sql.<br>
	 * <p>
	 * @param sqlOwner the sqlOwner to set
	 */
	public void setSqlOwner(String sqlOwner) {
		this.sqlOwner = sqlOwner;
	}

	/**
	 * Restituisce true se il nome fornito soddisfa i criteri di filtro.<br>
	 * <p>
	 * Il criterio di filtro viene valutato dalla funzione FILTER_EVALUATE.<br>
	 * <p>
	 * 
	 * @return the isMatchingFilter
	 */
	public boolean isMatchingFilter() {
		return isMatchingFilter;
	}

	/**
	 * Imposta se il nome fornito soddisfa i criteri di filtro.<br>
	 * <p>
	 * Il criterio di filtro viene valutato dalla funzione FILTER_EVALUATE.<br>
	 * <p>
	 * 
	 * @param isMatchingFilter the isMatchingFilter to set
	 */
	public void setMatchingFilter(boolean isMatchingFilter) {
		this.isMatchingFilter = isMatchingFilter;
	}
	
	

	/**
	 * Restituisce il return code di esecuzione della exit.<br>
	 * <p>
	 * 
	 * @return the returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * Imposta il return code di esecuzione della exit.<br>
	 * <p>
	 * 
	 * @param returnCode the returnCode to set
	 */
	public void setReturnCode(int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * Restituisce il il codice di errore di esecuzione della exit.<br>
	 * <p>
	 * 
	 * @return the msgCodeError
	 */
	public String getMsgCodeError() {
		return msgCodeError;
	}

	/**
	 * Imposta il il codice di errore di esecuzione della exit.<br>
	 * <p>
	 * @param msgCodeError the msgCodeError to set
	 */
	public void setMsgCodeError(String msgCodeError) {
		this.msgCodeError = msgCodeError;
	}

	/**
	 * Restituisce il codice cliente sul quale attivare logiche specifiche.<br>
	 * <p>
	 * @return the customerCode
	 */
	public String getCustomerCode() {
		return customerCode;
	}

	/**
	 * Imposta il codice cliente sul quale attivare logiche specifiche.<br>
	 * <p>
	 * @param customerCode the customerCode to set
	 */
	public void setCustomerCode(String customerCode) {
		this.customerCode = customerCode;
	}

	
	
	/**
	 * Restituisce le informazioni specifiche del cliente codificate<br>
	 * nel pilot di esecuzione dalla direttiva <tt>CUSTOMER_INFO</tt>
	 * <p>
	 * @return the customerInfo
	 */
	public String getCustomerInfo() {
		return customerInfo;
	}

	/**
	 * Imposta le informazioni specifiche del cliente codificate<br>
	 * nel pilot di esecuzione dalla direttiva <tt>CUSTOMER_INFO</tt>
	 * <p>
	 * @param customerInfo the customerInfo to set
	 */
	public void setCustomerInfo(String customerInfo) {
		this.customerInfo = customerInfo;
	}

	/**
	 * Restituisce il codice di libreria dal quale proviene l'oggetto<br>
	 * di cui si stavalutando il nome.<br>
	 * <p>
	 * Valido per funzione di <tt>LibraryScan</tt><br>
	 * <p>
	 * @return the libraryCode
	 */
	public String getLibraryCode() {
		return libraryCode;
	}

	/**
	 * Imposta il codice di libreria dal quale proviene l'oggetto<br>
	 * di cui si stavalutando il nome.<br>
	 * <p>
	 * Valido per funzione di <tt>LibraryScan</tt><br>
	 * <p>
	 * @param libraryCode the libraryCode to set
	 */
	public void setLibraryCode(String libraryCode) {
		this.libraryCode = libraryCode;
	}

	/**
	 * Restituisce il nome dello script Sql dal quale è stata estratta l'istruzione
	 * sotto analisi, di cui devono essere identificati sistema/sottosistema.
	 * 
	 * @return the scriptSqlName
	 */
	public String getScriptSqlName() {
		return scriptSqlName;
	}

	/**
	 * Imposta il nome dello script Sql dal quale è stata estratta l'istruzione
	 * sotto analisi, di cui devono essere identificati sistema/sottosistema.
	 * 
	 * @param scriptName the scriptName to set
	 */
	public void setScriptSqlName(String scriptSqlName) {
		this.scriptSqlName = scriptSqlName;
	}

	/**
	 * Restituisce la divisione cobol attiva al momento del'attivazione della exit.<br>
	 * <p>
	 * @return the activeCobolDivision
	 */
	public EnumCobolReservedWords getActiveCobolDivision() {
		return activeCobolDivision;
	}

	/**
	 * Imposta la divisione cobol attiva al momento del'attivazione della exit.<br>
	 * <p>
	 * @param activeCobolDivision the activeCobolDivision to set
	 */
	public void setActiveCobolDivision(EnumCobolReservedWords activeCobolDivision) {
		this.activeCobolDivision = activeCobolDivision;
	}

	/**
	 * Restituisce la sezione cobol attiva al momento del'attivazione della exit.<br>
	 * <p>
	 * @return the activeCobolSection
	 */
	public EnumCobolReservedWords getActiveCobolSection() {
		return activeCobolSection;
	}

	/**
	 * Imposta la sezione cobol attiva al momento del'attivazione della exit.<br>
	 * <p>
	 * @param activeCobolSection the activeCobolSection to set
	 */
	public void setActiveCobolSection(EnumCobolReservedWords activeCobolSection) {
		this.activeCobolSection = activeCobolSection;
	}

	/**
	 * Restituisce il nome della section sotto verifica.<br>
	 * <p>
	 * @return the activeSectionName
	 */
	public String getActiveSectionName() {
		return activeSectionName;
	}

	/**
	 * Imposta il nome della section sotto verifica.<br>
	 * <p>
	 * @param activeSectionName the activeSectionName to set
	 */
	public void setActiveSectionName(String activeSectionName) {
		this.activeSectionName = activeSectionName;
	}

	/**
	 * Restituisce il nome del paragrafo sotto verifica.<br>
	 * <p>
	 * @return the activeParagraphName
	 */
	public String getActiveParagraphName() {
		return activeParagraphName;
	}

	/**
	 * Imposta il nome del paragrafo sotto verifica.<br>
	 * <p>
	 * @param activeParagraphName the activeParagraphName to set
	 */
	public void setActiveParagraphName(String activeParagraphName) {
		this.activeParagraphName = activeParagraphName;
	}

	
}
