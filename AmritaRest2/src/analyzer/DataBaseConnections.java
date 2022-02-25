package analyzer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import enums.EnumAmritaExceptionError;
import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;

/**
 * copyright (c) 2009-2020 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseConnection
 * </h1>
 * <p>
 * Questa classe descrive tutte le operazioni di connessione/disconnessione al db.
 * La struttura con le connessioni si trava come variabile statica in AmritaStartup,
 * attivata allo startup di Tomcat
 * 
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 20/dic/2020
*/

public class DataBaseConnections {

	// Are used those by config.properties, just samples
	static int MAX_DB_CONN = 3;
	static String DRIVER = "com.mysql.cj.jdbc.Driver";   // la classe driver */
	static String DBURL = "jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC&rewriteBatchedStatements=true"; 	//* L'url al database */	
	static String USER = "GZEDDA";   					 // Lo username per le operazioni sul DB   	
	static String PWD = "giampietro4";                   // La password per le operazioni sul DB */

	/**
	 * Creazione pool di connessioni
	 * Richiamato da AmritaStartup
	 * 
	 */
	@SuppressWarnings("deprecation")
	public static void createConnectionsPool(UserConfiguration ucfg)  {		
        String[] arParm=null;
        
		// Messaggio di inizializzazione data base in corso
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0025", null, null); 
		
		//////////////////////////////////////////////////////////////////
		// Caricamento driver 
		//////////////////////////////////////////////////////////////////
        
		try {
			Class.forName(ucfg.getDataBaseDriver());
			
		} catch (ClassNotFoundException e1) {
			// Logging errore di driver non trovato
			arParm = new String[1];
			arParm[0] = ucfg.getDataBaseDriver();
			AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0007", arParm, e1); 
			try {
				throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DATA_BASE_DRIVER, e1);
			} catch (ExceptionAmrita e) {
				return;
			}
		}
		
		//////////////////////////////////////////////////////////////////
		// Creazione pool di connessioni non assegnate
		// Array di connessioni già allocato staticamente in AmritaStartup
        //////////////////////////////////////////////////////////////////
		
		// Open all pool connections
		for (int i = 0; i < AmritaStartup.arDbConnection.length; i++) {

			DataBaseConnection dataBaseConnection = new DataBaseConnection();
			
			// Creazione connessione
            try {
				Connection dbConn = DriverManager.getConnection(ucfg.getDataBaseUrl(), ucfg.getDataBaseUser(), ucfg.getDataBasePwd());
				dbConn.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
				dataBaseConnection.setDbConn(dbConn);
				dataBaseConnection.setConnActive(true);
				dataBaseConnection.setConnFree(true);
				AmritaStartup.arDbConnection[i] = dataBaseConnection;

			} catch (SQLException e) {
				// Errore sql in connessione
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0008", null, null); 
				// Dati Sql errore
				arParm = new String[2];
				arParm[0] = e.getSQLState();
				arParm[1] = new Integer(e.getErrorCode()).toString();
				AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0009", arParm, e); 
				try {
					throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DATA_BASE_GET_CONNECTION, e);
				} catch (ExceptionAmrita e1) {
					return;
				}
			}		
		}

		//////////////////////////////////////////////////////////////////
		// Pool di connessioni creato
        //////////////////////////////////////////////////////////////////
		
		// Messaggio creazione pool di n connessioni terminata
		arParm = new String[1];
		arParm[0] = new Integer(AmritaStartup.arDbConnection.length).toString();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0024", arParm, null); 

		// Messaggio di inizializzazione database terminata con successo
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0026", null, null); 

	}
	
	
	/**
	 * 
	 * EnumDataBaseOperation.DB_GET_CONNECTION
	 * 
	 * Recupera una connessione dal pool, e la restituisce al chiamante.
	 * 
	 * return Connection stabilita
	 * @throws ExceptionAmritaSqlError 
	 * @throws SQLException 
	 */
	public static synchronized Connection getConnection() throws ExceptionAmritaSqlError, SQLException {		
        DataBaseStatusDetailed dbs = null;

		// Scan pool di connessionin definite
		for (DataBaseConnection dbConn : AmritaStartup.arDbConnection) {
			
			if (dbConn.isConnFree()) {
				dbConn.setConnFree(false);
				dbConn.setConnActive(true);
				return dbConn.getDbConn();
			}
		}

		// Connessione rifiutata: raggiunto il massimo definito
		AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0020", null, null); 
		dbs = new DataBaseStatusDetailed();
		dbs.setTypeOperation(EnumDataBaseOperation.DB_ACQUIRE_CONNECTION);
		throw new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_DATA_BASE_ACQUIRE_CONNECTION, null);
	}

	/**
	 * 
	 * EnumDataBaseOperation.DB_CLOSE_CONNECTION<br>
	 * <p>
	 * Rilascio connessione specifica. Può essere usata da un altro
	 * cliente.
	 * @throws SQLException 
	 */
	public static synchronized void releaseConnection(Connection dbConnToRelease)  {		

		// Fornita connessione nulla: nessuna operazione
		if (dbConnToRelease == null) {
			return;
		}
		
		// Scan pool di connessioni
		for (DataBaseConnection dbConn : AmritaStartup.arDbConnection) {
			
			// E' la connessione in input
			if (dbConn.getDbConn() == dbConnToRelease) {
				dbConn.setConnFree(true);
				dbConn.setConnActive(true);
				return;
			}
		}
	}

	/**
	 * 
	 * EnumDataBaseOperation.DB_CLOSE_CONNECTION<br>
	 * <p>
	 * Rilascio tutte le connessioni attive
	 * Richiamato da AmritaStartup a chiusura Tomcat
	 * cliente.
	 */
	public static synchronized void releaseAllConnection()  {		

		String arParm[] = null;

		// Messaggio di chiusura connessioni in corso Da' inspiegabilmente errore
//		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0313", null, null); 
		

		// Scan pool di connessioni
		for (DataBaseConnection dbConn2 : AmritaStartup.arDbConnection) {
				dbConn2.setConnFree(true);
				dbConn2.setConnActive(false);
				try {
					dbConn2.getDbConn().close();
				} catch (SQLException e) {
					// Errore sql in chiusura connessione
					AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0022", null, null); 
					// Dati Sql errore
					arParm = new String[2];
					arParm[0] = e.getSQLState();
					arParm[1] = new Integer(e.getErrorCode()).toString();
					AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0023", arParm, e); 
                    break;
				}
		}
		// Messaggio di fine chiusura connessioni
//		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0314", null, null); 
		
	}

    /* 
	 * @throws ExceptionAmritaSqlError 
    * 
    */
	public static synchronized void closeConnection(Connection dbConn, DataBaseStatusDetailed dbs) throws ExceptionAmritaSqlError {		

		dbs.clear();
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_CONNECTION);
		
		// Scan pool di connessioni
		for (DataBaseConnection innerDbConn : AmritaStartup.arDbConnection) {
			
			if (innerDbConn.getDbConn() == dbConn && innerDbConn.isConnActive()) {
				try {
					dbConn.close();
					innerDbConn.setConnFree(true);
					innerDbConn.setConnActive(false);
                    dbs.setJdbcCommand("dbConn.close()");
					return;
				} catch (SQLException e) {
					dbs.setSqlStatus(e.getSQLState());
					dbs.setSqlErrorCode(e.getErrorCode());
					dbs.setWarningMessage(e.getLocalizedMessage());
					dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_CONNECTION_CLOSE);
					// Errore di disconnessione
					AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0018", null, null); 
					innerDbConn.setConnFree(true);
					innerDbConn.setConnActive(false);

				}
			}
		}
	}

	    			  

	/**
    * 
    * EnumDataBaseOperation.DB_CLOSE_CONNECTIONS
    * 
    * Chiusura di tutte le connessioni attive
	* @throws ExceptionAmritaSqlError 
    * 
    */
	public static synchronized void closeConnections(DataBaseStatusDetailed dbs) throws ExceptionAmritaSqlError {		

		dbs.clear();
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_CONNECTIONS);

		// Scan pool di connessioni
		for (DataBaseConnection innerDbConn : AmritaStartup.arDbConnection) {
			
			if (innerDbConn.isConnActive()) {
				try {
					innerDbConn.getDbConn().close();
					innerDbConn.setConnActive(false);
					dbs.setJdbcCommand("dbConn.close()");
				} catch (SQLException e) {
					dbs.setSqlStatus(e.getSQLState());
					dbs.setSqlErrorCode(e.getErrorCode());
					dbs.setWarningMessage(e.getLocalizedMessage());
					// Errore di disconnessione
					AmritaStartup.lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0018", null, null); 
					throw new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_DATA_BASE_DROP_CONNECTION, e);
				}
			}
		}
		return;
	}
	
	
	
	
	
	/*
	 * Generazione entry su log con informazioni istruzione Sql eseguita se previsto
	 * dai parametri di configurazione. 
	 * Sul log vengono scritti messaggi di tipo INFO e se dovesse verificarsi qualche exceptio sql, 
	 * verrebbero loggati anche i relativi messaggi di tipo DEBUG.
	 * 
	*/
	private void logSqlInstructionInfo(UserConfiguration ucfg, DataBaseStatusDetailed dbs) {
		
		String arParm[] = null;
		
		// Nessun errore e nessuna direttiva di log per ogni istruzione sql
		if (!ucfg.isDataBaseLogAnySql()) {
			return;
		}

		// Messaggio esito operazione Sql 
		arParm = new String[6];
		arParm[0] = dbs.getTypeOperation().toString();
		arParm[1] = dbs.getStatusOperation().toString();
		arParm[2] = dbs.getSqlStatus().toString();
		arParm[3] = new Integer(dbs.getSqlErrorCode()).toString();
		arParm[4] = dbs.getTableName();
		arParm[5] = dbs.getWarningMessage();
		AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0027", arParm, null); 

        // Messaggio stringa Sql 
		if (!dbs.getSqlString().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getSqlString();
			AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0028", arParm, null);
		}
		
		// Comando Jdbc 
		if (ucfg.isLogVerbose() && !dbs.getJdbcCommand().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getJdbcCommand();
			AmritaStartup.lf.writeRow(EnumMessageType.INFORMATION, "MI0029", arParm, null);
		} 
	
	}

	
}
