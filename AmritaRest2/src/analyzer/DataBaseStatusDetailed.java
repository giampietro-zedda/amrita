package analyzer;
import java.sql.SQLWarning;

import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseStatusDetailed
 * </h1>
 * <p>
 * Questa classe descrive le informazioni a fronte di una qualsiasi operazione di accesso
 * al database effettuata con {@link DataBaseManager}.
 * Ogni SqlException viene intercettata da DataBaseManager e al chiamante viene sempre restituito 
 * un oggetto di questa classe con l'esito dell'operazione, le informazioni di accesso quali il
 * numero totale di righe lette o altro.
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/mar/2010 
 * @see ClassName
 *
*/

public class DataBaseStatusDetailed {
	
	private EnumDataBaseOperation typeOperation = null;			// Operazione database richiesta
	private EnumDataBaseOperationStatus statusOperation = null; // Stato generale operazione eseguita
	private String entityName = "";								// Nome java POJO entity name associato alla tabella Sql 
	private String tableName = "";								// Nome tabella 
	private int sqlErrorCode = 0;								// Error code operazione Sql vVendor dependent. 0 = OK
	private String sqlStatus = "";								// Status operazione Sql (vendor e User)
	private String sqlString = "";      						// Stringa Sql eseguita nella grammatica del driver
	private String sqlStringOrigin = "";      			        // Stringa Sql originale da convertire in base al database
	private String jdbcCommand = "";      			            // Comando jdbc eseguito
	private int updateCount =0;      					    	// Numero aggiornamenti effettuati 
	private String warningMessage = "";      					// messaggio di warning localizzato
	private String errorCause = "";      					    // Cause generating the error
	private String sqlMessage = "";      					    // Message sql error from exception
	private String sqlLocalizedMessage = "";      				// Message sql error from exception localized
	private SQLWarning sqlWarning =  null;                  	// Oggetto Completo
	private Exception excpOrigin =  null;                  	    // Exception eventualmente scatenante l'errore
	/*
	 * 
	 * Costruttore 
	 * 
	 */
	public DataBaseStatusDetailed() {
		super();
		typeOperation = EnumDataBaseOperation.NOT_ASSIGNED;
		statusOperation = EnumDataBaseOperationStatus.NOT_ASSIGNED;
	}

	
	
	/**
	 * @return the typeOperation
	 */
	public EnumDataBaseOperation getTypeOperation() {
		return typeOperation;
	}



	/**
	 * @return the statusOperation
	 */
	public EnumDataBaseOperationStatus getStatusOperation() {
		return statusOperation;
	}



	/**
	 * @param statusOperation the statusOperation to set
	 */
	public void setStatusOperation(EnumDataBaseOperationStatus statusOperation) {
		this.statusOperation = statusOperation;
	}



	/**
	 * @param typeOperation the typeOperation to set
	 */
	public void setTypeOperation(EnumDataBaseOperation typeOperation) {
		this.typeOperation = typeOperation;
	}



	/**
	 * @return the sqlErrorCode
	 */
	public int getSqlErrorCode() {
		return sqlErrorCode;
	}

	/**
	 * @param sqlErrorCode the sqlErrorCode to set
	 */
	public void setSqlErrorCode(int sqlErrorCode) {
		this.sqlErrorCode = sqlErrorCode;
	}

	/**
	 * @param entityName the entityName to set
	 */
	public void setEntityName(String entityName) {
		this.entityName = entityName;
	}



	/**
	 * @return the entityName
	 */
	public String getEntityName() {
		return entityName;
	}



	/**
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}

	/**
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	/**
	 * @return the sqlStatus
	 */
	public String getSqlStatus() {
		return sqlStatus;
	}

	/**
	 * @param sqlStatus the sqlStatus to set
	 */
	public void setSqlStatus(String sqlStatus) {
		this.sqlStatus = sqlStatus;
	}

	/**
	 * @return the sqlString
	 */
	public String getSqlString() {
		return sqlString;
	}

	/**
	 * @param sqlString the sqlString to set
	 */
	public void setSqlString(String sqlString) {
		this.sqlString = sqlString;
	}



	/**
	 * @return the sqlStringOrigin
	 */
	public String getSqlStringOrigin() {
		return sqlStringOrigin;
	}



	/**
	 * @param sqlStringOrigin the sqlStringOrigin to set
	 */
	public void setSqlStringOrigin(String sqlStringOrigin) {
		this.sqlStringOrigin = sqlStringOrigin;
	}



	/**
	 * @return the jdbcCommand
	 */
	public String getJdbcCommand() {
		return jdbcCommand;
	}



	/**
	 * @param jdbcCommand the jdbcCommand to set
	 */
	public void setJdbcCommand(String jdbcCommand) {
		this.jdbcCommand = jdbcCommand;
	}



	/**
	 * @return the updateCount
	 */
	public int getUpdateCount() {
		return updateCount;
	}



	/**
	 * @param updateCount the updateCount to set
	 */
	public void setUpdateCount(int updateCount) {
		this.updateCount = updateCount;
	}



	/**
	 * @return the warningMessage
	 */
	public String getWarningMessage() {
		return warningMessage;
	}



	/**
	 * @return the errorCause
	 */
	public String getErrorCause() {
		return errorCause;
	}



	/**
	 * @param errorCause the errorCause to set
	 */
	public void setErrorCause(String errorCause) {
		this.errorCause = errorCause;
	}



	/**
	 * @param warningMessage the warningMessage to set
	 */
	public void setWarningMessage(String warningMessage) {
		this.warningMessage = warningMessage;
	}



	/**
	 * @return the sqlWarning
	 */
	public SQLWarning getSqlWarning() {
		return sqlWarning;
	}



	/**
	 * @param sqlWarning the sqlWarning to set
	 */
	public void setSqlWarning(SQLWarning sqlWarning) {
		this.sqlWarning = sqlWarning;
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
	 * @return the sqlMessage
	 */
	public String getSqlMessage() {
		return sqlMessage;
	}



	/**
	 * @param sqlMessage the sqlMessage to set
	 */
	public void setSqlMessage(String sqlMessage) {
		this.sqlMessage = sqlMessage;
	}


	public String getLocalizedMessage() {
		return this.sqlLocalizedMessage;
		
	}

	public void setLocalizedMessage(String localizedMessage) {
		this.sqlLocalizedMessage = localizedMessage;
		
	}


	public void clear() {
    	this.typeOperation = null;			 
    	this.statusOperation = null; 
    	this.entityName = "";								 
    	this.tableName = "";								 
    	this.sqlErrorCode = 0;								 
    	this.sqlStatus = "";								 
    	this.sqlString = "";      						 
    	this.sqlStringOrigin = "";      			         
    	this.jdbcCommand = "";      			            
    	this.updateCount =0;      					    	 
    	this.warningMessage = "";      					 
    	this.errorCause = "";      					    
    	this.sqlWarning =  null;  
    	this.sqlMessage =  null;   
    	this.excpOrigin =  null;                  	    
    	   	
    }


}
