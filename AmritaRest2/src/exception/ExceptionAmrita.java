package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmrita
 * </h1>
 * <p>
 * Questa classe identifica una exception lanciata a fronte di un errore di elaborazione o in
 * generale a fronte di specifiche condizioni applicative.  Tutte le eccezioni del progetto Amrita ereditano 
 * da questa classe. Le classi di eccezione che ereditano da questa classe definiscono già, di per sè, 
 * la natura dell'anomalia.<br>
 * L'oggetto di questa classe memorizza informazioni sullo stato dettagliato dell'ultimo accesso Sql,
 * una enumerazione con una eventuale qualificazione ulteriore dell'errore e un eventiuale oggetto Exception 
 * intercettato origine dell'anomalia scatenante.<br>
 * le suddette informazioni possono ovviamente non essere valorizzate e in questo caso restano impostate a null.<br>
 * <p>
 * A fronte di una eccezione intercettata e gestita, viene inviato su log un messaggio informativo di generica
 * ExceptioAmrita lanciata, con il completo e contestuale PrintStackTrace.
 * Successivamente vengono loggati messaggi su log indicanti l'eventuale eccezione origine, la descrizione
 * qualificata del messaggio e, se disponibili, tutte le informazioni di accesso Sql.
 * <p>
 * Il metodo toString() viene ridefinito per mostrare la seguente stringa di identificazione: <br>
 * Nome exception lanciata
 * (messaggio qualificato)
 * Messaggio supplementare
 * "Originated By "  
 * Nome exception origine del malfunzionamento
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseManager
 * @see DataBaseEntityInterface
 * @see ExceptionAmritaAnnotationMissing
 * @see ExceptionAmritaReflectionError
 * @see ExceptionAmritaSqlAccessDuplicate
 * @see ExceptionAmritaSqlAccessNotfound
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns
*/

public class ExceptionAmrita extends Exception {

	private static final long serialVersionUID = 1L;		 
	public DataBaseStatusDetailed dbsd = null;				// Descrittore stato esecuzione statement sql
    public EnumAmritaExceptionError qualifiedError = null;  // Errore qualificato    
    public Exception excpOrigin = null;   					// Exception intercettata origine dell'anomalia
    public String msg = "";   					            // Messaggio supplementare impostato via setter
	
    // Informazioni sul thread in esecuzione
    public String threadGroupName = "";					    // Nome del gruppo dei thread da direttive processo/funzione
    public String threadName = "";							// Nome thread da direttive processo/funzione

    /**
	 * Costruttore vuoto
	 */
	public ExceptionAmrita() {
		super();
	}
	
	/**
	 * Costruttore con informazioni di accesso al database, errore qualificato e exception origine
	 */
	public ExceptionAmrita(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super();
		this.dbsd = dbsd;
		this.qualifiedError = qualifiedError;
		this.excpOrigin = excpOrigin;
	}

	/**
	 * Costruttore con errore qualificato e exception origine
	 */
	public ExceptionAmrita(EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super();
		this.qualifiedError = qualifiedError;
		this.excpOrigin = excpOrigin;
	}

	/**
	 * Costruttore con il solo errore qualificato 
	 */
	public ExceptionAmrita(EnumAmritaExceptionError qualifiedError) {
		super();
		this.qualifiedError = qualifiedError;
	}

	/**
	 * @return the DataBaseStatusDetailed of Sql operation 
	 */
	public DataBaseStatusDetailed getInfoSql() {
		return dbsd;
	}

	/**
	 * @param dbsd the dbsd to set
	 */
	public void setInfoSql(DataBaseStatusDetailed dbsd) {
		this.dbsd = dbsd;
	}

	/**
	 * @return the qualifiedError
	 */
	public EnumAmritaExceptionError getQualifiedError() {
		return qualifiedError;
	}

	/**
	 * @param qualifiedError the qualifiedError to set
	 */
	public void setQualifiedError(EnumAmritaExceptionError qualifiedError) {
		this.qualifiedError = qualifiedError;
	}

	/**
	 * @return the excpOrigin
	 */
	public Exception getExcpOrigin() {
		return excpOrigin;
	}

	/**
	 * @param excpOrigin the excpOrigin to set
	 */
	public void setExcpOrigin(Exception excpOrigin) {
		this.excpOrigin = excpOrigin;
	}

	/**
	 * @return the msg
	 */
	public String getMsg() {
		return msg;
	}

	/**
	 * @param msg the msg to set
	 */
	public void setMsg(String msg) {
		this.msg = msg;
	}

	/**
	 * @return the threadGroupName
	 */
	public String getThreadGroupName() {
		return threadGroupName;
	}

	/**
	 * @param threadGroupName the threadGroupName to set
	 */
	public void setThreadGroupName(String threadGroupName) {
		this.threadGroupName = threadGroupName;
	}

	/**
	 * @return the threadName
	 */
	public String getThreadName() {
		return threadName;
	}

	/**
	 * @param threadName the threadName to set
	 */
	public void setThreadName(String threadName) {
		this.threadName = threadName;
	}

	/* (non-Javadoc)
	 * @see java.lang.Throwable#toString()
	 */
	@Override
	public String toString() {
		String toStringMsg = "";
		
		// Nome ExceptionAmrita o da questa derivate
		toStringMsg = super.toString();
		
		// Qualified message
		if (qualifiedError != null) {
			toStringMsg = toStringMsg + " (" + qualifiedError.toString() + ")";
		}
		
		// Messaggio supplementare
		if (!msg.equals("")) {
			toStringMsg = toStringMsg + " " + msg;
		}

		// Exception origine
		if (excpOrigin != null) {
			toStringMsg = toStringMsg + " Originated By " + excpOrigin.toString();
		}
		return toStringMsg;
	}

	
}
