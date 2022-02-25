package analyzer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;

import utilities.ReflectionManager;

import enums.EnumAmritaExceptionError;
import enums.EnumDataBase;
import enums.EnumDataBaseJdbcSqlType;
import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;
import enums.EnumMessageType;
import enums.EnumSourceType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaReflectionError;
import exception.ExceptionAmritaSqlError;

/**
  * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
  * 
 * <h1>
 * DataBaseManager  
 * </h1>
 *  <p>
 * Questa classe funge da interfaccia fisica per tutte le operazione sul data base.
 * Vengono centralizzati tutti i metodi di interrogazione della struttura del database come
 * nomi tabelle, indici, campi e formato campi, oltre ai metodi di esecuzione di generiche
 * istruzioni Sql di inserimento, aggiornamento e delete, navigazione etc.
 * <p>
 * Quuesta classe è anche la base, trasparente, per l'utilizzo  dell'interfaccia di accesso 
 * CRUD {@link DataBaseEntityInterface}. Vengono gestite le operazioni CRUD passando ai metodi
 * preposti un descrittore dei campi interessati e vengono pertanto generate automaticamente
 * le istruzioni sql necessarie.<br>
 * Tuttavia il mapping completo e trasparente ORM è effettuato dalla classe {@link DataBaseEntityInterface},
 * che popola automaticamente il descrittore dei campi e delle colonne con le informazioni 
 * recuperate dalle annotazioni in source della classe di gestione dell'Entity, 
 * richiamando poi i metodi di questa classe.<br>
 * <p>
 * Viene gestito un pool di connessioni,  l'apertura di una connessione, la sua chiusura, 
 * l'utilizzo di statement prepared per l'ottimizzazione e tutte le operazioni fisiche 
 * di accesso al db. <br>
 * L'accesso fisico al database viene effettuato attraverso l'nterfaccia standard Java
 * Jdbc JConnector oppure attraverso Jdbc Odbc, come nel caso di MS Access.
 * I drivers utilizzati sono indicati nel file di configiurazione e sono, normalmente:
 * <p>
 * ODBC_DRIVER_SUN = "sun.jdbc.odbc.JdbcOdbcDriver";
 * JDBC_DRIVER_MYSQL = "com.mysql.jdbc.Driver";
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/03/2010
 * @see DataBaseManager
 */

@SuppressWarnings("unused")
public class DataBaseManager  {

    private LoggerFacade lf = null;					// Gestore centralizzato log	
 	private UserConfiguration sd = null;		    // Defaults di sistema
	private ReflectionManager rm = null;            // Gestore generalizzato refection
	private UserConfiguration ucfg = null;          // Configurazione utente
	
	////////////////////////////////////////////////////////////////
	// Variabili di istanza                                       //
	////////////////////////////////////////////////////////////////
	
	// Campi per connessione con ODBC driver or JAVA JDBC JConnector
	private EnumDataBase dbType = null;             // Database type
	private String driverName = ""; 		        // Driver di accesso al database
    private String dbName = ""; 		    		// Database name o data source ODBC
    private String url = ""; 		    		    // Url completa di accesso
    private String user;   						    // User name
    private String pwdUser;      					// Password
    private String connString = "";     			// Stringa di connessione

    // Pool di connessioni predefinite
    static InnerDbConnection  arDbConnection[] = null;
    
 	// Gestione esecuzione batch di insiemi di istruzioni multiple Sql
    Statement stmtBatch = null;					    // Statement da eseguire Batch
    
    
	/**
	 * Costruttore
	 * 
	 * @throws ExceptionAmrita 
	 */
	public DataBaseManager(	UserConfiguration ucfg
						   ,EnumDataBase dbType      // Tipo database
						   ,String driverName        // Driver da utilizzare
					       ,String url               // Url completa di data base o data source finale
					       ,String dbName            // Nome data base o dataSource se Db Access
						   ,String user 			 // User se richiesto per il database
					 	   ,String pwdUser  		 // Pwd se richiesta per il database
						   ,int numConnPool  		 // Numero connessioni da generare in pool di inizializzazione
						) throws ExceptionAmrita {

		
		this.sd = ucfg;
		this.dbType = dbType;
		this.driverName = driverName;
		this.url = url;
		this.dbName = dbName;
		this.user = user;
		this.pwdUser = pwdUser;
		
		boolean bParmOk = false;
		String arParm[] = null;

		rm = new ReflectionManager();
		DataBaseStatusDetailed dbs = new DataBaseStatusDetailed() ;

		lf = ucfg.getLoggerFacade();
		
		dbs.setTypeOperation(EnumDataBaseOperation.DB_OPEN_SESSION);
		clearDatabaseStatusStructure(dbs);
		
		// Messaggio di inizializzazione data base in corso
		lf.writeRow(EnumMessageType.INFORMATION, "MI0025", null, null); 
		
		//////////////////////////////////////////////////////////////////
		// Controlli sui dati in input
		//////////////////////////////////////////////////////////////////

		bParmOk = areParmConstructorGood(dbType, driverName, url, dbName, user, pwdUser);		 
		if (!bParmOk) {
			return;
		}
		
		
		//////////////////////////////////////////////////////////////////
		// Caricamento driver 
		//////////////////////////////////////////////////////////////////
        
		try {
			Class.forName(driverName);
			dbs.setJdbcCommand("Class.forName(driverName)");
			logSqlInstructionInfo(dbs);
		} catch (ClassNotFoundException e1) {
			// Logging errore di driver non trovato
			arParm = new String[1];
			arParm[0] = driverName;
			lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0007", arParm, e1); 
			throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DATA_BASE_DRIVER, e1);
		}

		//////////////////////////////////////////////////////////////////
		// Composizione stringa di connessione in base al db
		// potrebbero esserci differenze fra i vari dbms
        //////////////////////////////////////////////////////////////////
		
		// Protocol jdbc, Subprotocol odbc, data source ODBC|database name
		switch (dbType) {
				case DB_MS_ACCESS:
					connString = url;    	// jdbc:odbc:DbAmrita
					break;
				case DB_MYSQL:
					connString = url;		// jdbc:mysql://localhost:3306/DbAmrita
					break;
				default:
					break;
		}
		
		//////////////////////////////////////////////////////////////////
		// Creazione pool di connessioni  
        //////////////////////////////////////////////////////////////////

		if (arDbConnection != null) {
	//		ucfg.setDataBaseManager(this);	   // Obsoleto
            return;
		}
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_GET_CREATE_CONNECTIONS);
		
		arDbConnection = new InnerDbConnection[numConnPool];
		
		// Scan pool
		for (int i = 0; i < arDbConnection.length; i++) {

			InnerDbConnection innerDbConnection = new InnerDbConnection();
			
			// Creazione connessione: user e pwd possono essere ""
            try {
				Connection dbConn = DriverManager.getConnection(connString, user, pwdUser);
				innerDbConnection.dbConn = dbConn;
				innerDbConnection.isConnActive = true;
				innerDbConnection.isConnFree = true;
				arDbConnection[i] = innerDbConnection;
				dbs.setJdbcCommand("DriverManager.getConnection(" + connString + "," + user +"," + pwdUser + ")");
			//  logSqlInstructionInfo(dbs);
			} catch (SQLException e) {
				// Errore sql in connessione
				lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0008", null, null); 
				// Dati Sql errore
				arParm = new String[2];
				arParm[0] = e.getSQLState();
				arParm[1] = new Integer(e.getErrorCode()).toString();
				lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0007", arParm, e); 
				throw new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DATA_BASE_GET_CONNECTION, e);
			}		
		}
		
		
		// Messaggio creazione pool di n connessioni terminata
		arParm = new String[1];
		arParm[0] = new Integer(arDbConnection.length).toString();
		lf.writeRow(EnumMessageType.INFORMATION, "MI0024", arParm, null); 

		// Messaggio di inizializzazione database terminata con successo
		lf.writeRow(EnumMessageType.INFORMATION, "MI0026", null, null); 
		
		// Disponibile per tutte le applicazioni
//		ucfg.setDataBaseManager(this);	     // Obsoleto
	}

	/**
	 * Costruttore semplificato per compatibilità con il nuovo sistema di accesso dati
	 */
	public DataBaseManager(	UserConfiguration ucfg) {
       this.ucfg = ucfg;
	
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
	public synchronized Connection getConnection(DataBaseStatusDetailed dbs) throws ExceptionAmritaSqlError, SQLException {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_ACQUIRE_CONNECTION);

		// Scan pool di connessionin definite
		for (InnerDbConnection dbConn : arDbConnection) {
			
			if (dbConn.isConnFree) {
				dbConn.isConnFree = false;
				dbConn.isConnActive = true;
			 	dbConn.dbConn.setAutoCommit(false);
				return dbConn.dbConn;
			}
		}

		// Connessione rifiutata: raggiunto il massimo definito
		lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0020", null, null); 
		throw new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_DATA_BASE_ACQUIRE_CONNECTION, null);
	}

	/**
     * 
     * EnumDataBaseOperation.DB_CLOSE_CONNECTION<br>
     * <p>
     * Rilascio connessione specifica. Può essere usata da un altro
     * cliente.
     * 
     * 
     */
	public synchronized void releaseConnection(Connection dbConn, DataBaseStatusDetailed dbs)  {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_CONNECTION);
		
		// Fornita connessione nulla: nessuna operazione
		if (dbConn == null) {
			return;
		}
		
		// Scan pool di connessioni
		for (InnerDbConnection innerDbConn : arDbConnection) {
			
			// E' la connessione in input
			if (innerDbConn.dbConn == dbConn) {
				innerDbConn.isConnFree = true;
				innerDbConn.isConnActive = false;
				return;
			}
		}
	}


	/**
     * 
     * EnumDataBaseOperation.DB_CLOSE_CONNECTION
     * 
     * Chiusura connessione specifica.
     * 
     * 
	 * @throws ExceptionAmritaSqlError 
     * 
     */
	public synchronized void closeConnection(Connection dbConn, DataBaseStatusDetailed dbs) throws ExceptionAmritaSqlError {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_CONNECTION);
		
		// Scan pool di connessioni
		for (InnerDbConnection innerDbConn : arDbConnection) {
			
			if (innerDbConn.dbConn == dbConn && innerDbConn.isConnActive) {
				try {
					dbConn.close();
					innerDbConn.isConnFree = true;
					innerDbConn.isConnActive = false;
                    dbs.setJdbcCommand("dbConn.close()");
					logSqlInstructionInfo(dbs);
					return;
				} catch (SQLException e) {
					dbs.setSqlStatus(e.getSQLState());
					dbs.setSqlErrorCode(e.getErrorCode());
					dbs.setWarningMessage(e.getLocalizedMessage());
					dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_CONNECTION_CLOSE);
					// Errore di disconnessione
					lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0018", null, null); 
					logSqlInstructionError(dbs, e);
					innerDbConn.isConnFree = true;
					innerDbConn.isConnActive = false;

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
	public synchronized void closeConnections(DataBaseStatusDetailed dbs) throws ExceptionAmritaSqlError {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_CONNECTIONS);

		// Scan pool di connessioni
		for (InnerDbConnection innerDbConn : arDbConnection) {
			
			if (innerDbConn.isConnActive) {
				try {
					innerDbConn.dbConn.close();
					innerDbConn.isConnActive = false;
					dbs.setJdbcCommand("dbConn.close()");
					logSqlInstructionInfo(dbs);
				} catch (SQLException e) {
					dbs.setSqlStatus(e.getSQLState());
					dbs.setSqlErrorCode(e.getErrorCode());
					dbs.setWarningMessage(e.getLocalizedMessage());
					// Errore di disconnessione
					lf.writeRow(EnumMessageType.ERROR_FATAL, "EF0018", null, null); 
					logSqlInstructionError(dbs, e);
					throw new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_DATA_BASE_DROP_CONNECTION, e);
				}
			}
		}
		return;
	}

	/**
     * 
     * Inzio transazione. Viene forzata una commit al database 
     * 
     */
	public synchronized void  beginTransaction(Connection dbConn
			           			 ,DataBaseStatusDetailed dbs
			                    ) {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_BEGIN_TRANSACTION);
		
		try {
			dbConn.commit();
			dbs.setJdbcCommand("dbConn.commit()");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return;
	}
	
	
	/**
     * 
     * Commit aggiornamenti effettuati e non consolidati
     * 
     */
	public synchronized void  commit(Connection dbConn
			           ,DataBaseStatusDetailed dbs
			           ) {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_COMMIT);
		
		try {
			dbConn.commit();
			dbs.setJdbcCommand("dbConn.commit()");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return;
	}
	


	/**
     * 
     * Rollback aggiornamenti effettuati e non committati
     * fino al punto di commit impostato precedentemente.
     * Se non era stato impostato un punto di commit tutti gli aggiornamenti
     * vengono controaggiornati.
     * 
     */
	public synchronized void  rollback(Connection dbConn 
						 ,DataBaseStatusDetailed dbs 
						 ,Savepoint commitPoint
						 ) {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_ROLLBACK);

		try {
			if (commitPoint == null) {
				dbConn.rollback();
				dbs.setJdbcCommand("dbConn.rollback()");
				logSqlInstructionInfo(dbs);
			} else {
				dbConn.rollback(commitPoint);
				dbs.setJdbcCommand("dbConn.rollback(commitPoint)");
				logSqlInstructionInfo(dbs);
			}
			
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return;
	}

	/**
     * 
     * Rollback aggiornamenti effettuati e non committati
     * fino all'ultima commit effettuata.
     * 
     */
	public synchronized void  rollback(Connection dbConn 
						 ,DataBaseStatusDetailed dbs 
						 ) {		

		
		rollback(dbConn, dbs, null);
		return;
	}

	/**
     * 
     * Imposta un punto di commit con un nome.
     * Successivi Rollback elimineranno gli aggiornamenti solo fina questo
     * punto.
     * 
     */
	public Savepoint setCommitPoint(Connection dbConn
								   ,DataBaseStatusDetailed dbs
								   ,String commitPoint
								   ) {		

        Savepoint commitPointName = null;
        
        clearDatabaseStatusStructure(dbs);
        dbs.setTypeOperation(EnumDataBaseOperation.DB_SET_COMMIT_POINT);

        try {
			commitPointName = dbConn.setSavepoint(commitPoint);
			dbs.setJdbcCommand("dbConn.setSavepoint(commitPoint)");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return commitPointName;
	}


	/**
     * 
     * EnumDataBaseOperation.DB_CRUD_CREATE <br>
     * 
     * Effettua un inserimento CRUD della riga nella tabella specificata.
     * Le colonne e i valori da inserire sono codificati nell'array in
     * input di descrittori {@link DataBaseItemDescriptor}.
	 * @throws ExceptionAmrita 
     * 
     * 
     */
	public synchronized void crudCreate(Connection dbConn
						               ,DataBaseStatusDetailed dbs
									   ,String dbTableName
									   ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
									   ) throws ExceptionAmrita  {		
		
		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
		String sqlString = "";              // Istruzione nella grammatica ANSI comune
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_CREATE);

		// Aggiornamento valori key Sql colonne
		updateDbColumnsValue(dbs, arDataBaseItemDescriptor);

		// Creazione generalizzata statement crud Insert 
		sqlString = createSqlStringCrud("INSERT", dbTableName, arDataBaseItemDescriptor);
		
		sqlString=sqlString.replace("\\", "\\\\");	  // GPZ
		
        // Esecuzione statement Sql Insert
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			dbs.setJdbcCommand("dbConn.createStatement()");
			Statement stmt = dbConn.createStatement();
			dbs.setJdbcCommand("stmt.executeUpdate(sqlStringDriverGrammar)");
			stmt.executeUpdate(sqlStringDriverGrammar);
			if (stmt.getWarnings() != null) {
				dbs.setWarningMessage(stmt.getWarnings().toString());
			}
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setExcpOrigin(e);
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			// Error code = 1062 significa Duplicate in MySql
			if (dbs.getSqlErrorCode() != 0 && dbs.getSqlErrorCode() != 1062 ) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
				logSqlInstructionError(dbs, e);
			} else {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_DUPLICATE);
			}
		}
		return;
	}
    

    			  

	/**
     * 
     * EnumDataBaseOperation.DB_CRUD_READ <br>
     * 
     * Effettua una lettura CRUD della riga di Entity specificata.
     * La lettura è intesa di una riga identificata univocamente dalla chiave primaria, pertanto il
     * risultato di questa query dovrebbe essere di una sola riga restituita.
      * Le colonne con le colonne e i valori da inserire sono codificati nell'array in
     * input di descrittori {@link DataBaseItemDescriptor}.
     * 
     * @return ResultSet con riga tdati tabella
	 * @throws ExceptionAmrita 
     * 
     */
	public synchronized ResultSet crudRead(Connection dbConn
						 				  ,DataBaseStatusDetailed dbs
									      ,String dbTableName
									      ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
										  ) throws ExceptionAmrita  {		
		
		ResultSet rs = null;				// ResultSet con la riga letta 
		String sqlString = "";          	// Conterrà Select ...
		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
		boolean recordFound = true; 
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_READ);
        
		// Aggiornamento valori key Sql colonne con input dall'oggetto entity fornito
		updateDbColumnsValue(dbs, arDataBaseItemDescriptor);

		// Creazione generalizzata statement crud Select 
		sqlString = createSqlStringCrud("SELECT", dbTableName, arDataBaseItemDescriptor);
		
		// Lettura riga tabella
		try {
//?			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente (da' problemi)
			sqlStringDriverGrammar = sqlString;
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			dbs.setJdbcCommand("dbConn.createStatement()");
			Statement stmt = dbConn.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
			dbs.setJdbcCommand("stmt.executeQuery(sqlStringDriverGrammar)");
			//sqlStringDriverGrammar = "SELECT OBJTSYST, OBJTSUBS, OBJTIDOB, OBJTTYPO, OBJTIDOE, OBJTLIBS, OBJTFILS, OBJTTYPS, OBJTLDIR, OBJTSTAT, OBJTAUTH, OBJTSYOW, OBJTSSOW, OBJTDTWR, OBJTPDOC, OBJTDTFA, OBJTTMFA, OBJTDTLA, OBJTTMLA FROM OBJT WHERE OBJTSYST = 'A1' AND OBJTSUBS = 'SU' AND OBJTIDOB = 'AK210' AND OBJTTYPO = 0";
			rs = stmt.executeQuery(sqlStringDriverGrammar);  
			if (stmt.getWarnings() != null) {
				dbs.setWarningMessage(stmt.getWarnings().toString());
			}
			logSqlInstructionInfo(dbs);
			recordFound = rs.first();
			if (recordFound) {
				updateJavaFieldsValue(rs, arDataBaseItemDescriptor); // Update valori letti colonne in campi Java di array descrittore items
 			} else {
                dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);          
			}
			dbs.setJdbcCommand("rs.close()");
			rs.close();
			dbs.setJdbcCommand("stmt.close()");
			stmt.close();
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			// Error code = 0 significa Notfound
			if (dbs.getSqlErrorCode() != 0) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
				logSqlInstructionError(dbs, e);
			}
		}
		
		return rs;
	}
	


	/**
     * 
     * EnumDataBaseOperation.DB_CRUD_UPDATE
     * 
     * Effettua un aggiornamento CRUD di una riga della tabella specificata.
     * Non è fornito il ResultSet ma i nomi e i valori java delle colonne.
     * Pertanto l'esecuzione di questo metodo provoca il recupero di un Resultset
     * che dovrebbe essere di una riga soltanto con il successivo aggiornamento.
     * Le colonne e i valori da inserire sono codificati nell'array in
     * input di descrittori {@link DataBaseItemDescriptor}.
	 * @throws ExceptionAmrita 
     * 
     */
	public synchronized void crudUpdate(Connection dbConn 
						  ,DataBaseStatusDetailed dbs
					      ,String dbTableName
					      ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
			              ) throws ExceptionAmrita  {	
		
		ResultSet rs = null;				// ResultSet con la riga letta 
		String sqlString = "";          	// Conterrà Update ...
		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
        
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_UPDATE);
        
		// Aggiornamento valori colonne da aggiornare con valori campi java
		updateDbColumnsValue(dbs, arDataBaseItemDescriptor);

		// Creazione generalizzata statement Update
		sqlString = createSqlStringCrud("UPDATE", dbTableName, arDataBaseItemDescriptor);
		sqlString=sqlString.replace("\\", "\\\\");	  // GPZ

		// Update riga tabella
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			dbs.setJdbcCommand("dbConn.createStatement()");
			Statement stmt = dbConn.createStatement();
            dbs.setJdbcCommand("stmt.executeUpdate(sqlStringDriverGrammar)");
			stmt.executeUpdate(sqlStringDriverGrammar);
			if (stmt.getWarnings() != null) {
				dbs.setWarningMessage(stmt.getWarnings().toString());
			}
			if (stmt.getUpdateCount() == 0) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);
			}
            logSqlInstructionInfo(dbs);
			dbs.setJdbcCommand("stmt.close()");
			stmt.close();
		} catch (SQLException e) {
			// Errore Sql o di conversione dati: ritorno al chiamante
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			// Error code = 0 significa Notfound
			if (dbs.getSqlErrorCode() != 0) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
				logSqlInstructionError(dbs, e);
			}
		}
		return;
	}
	

	/**
     * 
     * EnumDataBaseOperation.DB_CRUD_DELETE
     * 
     * Delete riga di Entity.
     * Effettua un delete CRUD di una riga della tabella specificata.
     * Non è fornito il ResultSet ma i nomi e i valori java delle colonne.
     * Viene generato ed eseguito uno statement Sql di Delete con la
     * condizione di Where impostata con i valori delle colonne chiave
     * fornite in input.
     * Le colonne e i valori chiave da cercare sono codificati nell'array in
     * input di descrittori {@link DataBaseItemDescriptor}.
	 * @throws ExceptionAmrita 
     * 
     */
	public synchronized void crudDelete(Connection dbConn 
						  ,DataBaseStatusDetailed dbs
					      ,String dbTableName
					      ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
			            ) throws ExceptionAmrita  {	


		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
		String sqlString = "";
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_DELETE);

		// Aggiornamento valori colonne da aggiornare con valori campi java
		updateDbColumnsValue(dbs, arDataBaseItemDescriptor);

	    sqlString = createSqlStringCrud("DELETE", dbTableName, arDataBaseItemDescriptor);
	    
	    // Esecuzione statement Sql Delete
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			dbs.setJdbcCommand("dbConn.createStatement()");
			Statement stmt = dbConn.createStatement();
			dbs.setJdbcCommand("stmt.execute(sqlStringDriverGrammar)");
			stmt.executeUpdate(sqlStringDriverGrammar);
			dbs.setUpdateCount(stmt.getUpdateCount());
			if (stmt.getWarnings() != null) {
				dbs.setWarningMessage(stmt.getWarnings().toString());
			}
			if (stmt.getUpdateCount() == 0) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_NOTFOUND);
			}
			logSqlInstructionInfo(dbs);
			dbs.setJdbcCommand("stmt.close()");
			stmt.close();
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			// Error code = 0 significa Notfound
			if (dbs.getSqlErrorCode() != 0) {
				dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
				logSqlInstructionError(dbs, e);
			}
		}

		return ;
	}


	
	/**
     * 
     * 
     * Viene chiuso il Resultset precedentemente  ottenuto da
     * una operazione Sql.
     * 
     */
	public void closeResultSet(ResultSet rs
						      ,DataBaseStatusDetailed dbs
			                  )  {	


		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CLOSE_RESULTSET);

		try {
			rs.close();
			dbs.setJdbcCommand("rs.close()");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return ;
	}

	/**
     * 
     * Dbms prepare di una istruzione, per ottimizzazione.
     * I parametri placeholder saranno valorizzati con i valori correnti nelle istruzioni
     * di esecuzione dell'istruzione preparata. 
     * Si tratta di colonne il cui valore viene stabilito solo al momento dell'esecuzione.
     * E' possibile che l'istruzione generi un ResultSet, nel caso di Select.
     * 
     * return ResultSet con i dati, se Select 
     */
	public PreparedStatement prepareStatement(Connection dbConn
									 		 ,String sqlStringToPrepare
									 		 ,DataBaseStatusDetailed dbs
							  				 )  {		

		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
		PreparedStatement prepStat = null;

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_PREPARE);
  	
		// Esecuzione prepare
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlStringToPrepare); // Trasformazione secondo grammatica del driver corrente
			prepStat = dbConn.prepareStatement(sqlStringDriverGrammar);  
			dbs.setJdbcCommand("dbConn.prepareStatement(sqlStringDriverGrammar)");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return prepStat;
	}


	/**
     * 
     * Esecuzione istruzione preparata con Sql Prepare.
     * Vengono forniti i valori dei placeholder impostati nell'istruzione.
     * 
     * return ResultSet con i dati, se Select 
	 * @throws ExceptionAmrita 
     */
	public ResultSet execPreparedStatement(Connection dbConn
				                          ,PreparedStatement preparedStatement
										  ,DataBaseStatusDetailed dbs
										  ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
								  	      ) throws ExceptionAmrita  {		

		
		ResultSet rs = null;
		boolean isStatementUpdate = false;
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_PREPARED_EXEC);
  	
		try {
			// Esecuzione istruzione e impostazione messaggi
			updatePlaceHolders(preparedStatement, arDataBaseItemDescriptor);
			
			isStatementUpdate = preparedStatement.execute();  
            logSqlInstructionInfo(dbs);
            
			// Eventuali warning 
			if (preparedStatement.getWarnings() != null) {
				dbs.setSqlWarning(preparedStatement.getWarnings());
				dbs.setWarningMessage(preparedStatement.getWarnings().getLocalizedMessage());
				dbs.setSqlErrorCode(preparedStatement.getWarnings().getErrorCode());
				dbs.setSqlStatus(preparedStatement.getWarnings().getSQLState());
			}
			
			// Operazioni in base al tipo di istruzione eseguita
			if (isStatementUpdate) {
				dbs.setUpdateCount(preparedStatement.getUpdateCount());
			} else {
                rs = preparedStatement.getResultSet();
                dbs.setJdbcCommand("preparedStatement.getResultSet()");
			}
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		
		return rs;
	}

  			  
	/**
     * 
     * Creazione di un batch attraverso accodamento di comandi Sql per successiva esecuzione cumulatava.
     * Le unità di esecuzione separate vengono fornite nell'array di inità di 
     * esecuzione batch in input.
     * Per ogni riga dell'array di comandi batch, viene eseguito, sullo statement
     * di esecuzione batch, il metodo addBatch.
     * 
     */
	public Statement batchCreate(Connection dbConn
			                    ,DataBaseBatchExecUnit arBatchExecUnit[]
					            ,DataBaseStatusDetailed dbs
					            )  {		

		
		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_BATCH_CREATE);
		
		try {
			// creazione oggetto Statement con comandi cumulativi
			if (stmtBatch == null) {
				stmtBatch = dbConn.createStatement();
				dbs.setJdbcCommand("dbConn.createStatement()");
				logSqlInstructionInfo(dbs);
			}
			
			// Scan singoli comandi Sql
			for (DataBaseBatchExecUnit batchExecUnit : arBatchExecUnit) {
				sqlStringDriverGrammar = dbConn.nativeSQL(batchExecUnit.getSqlCommand()); // Trasformazione secondo grammatica del driver corrente
				stmtBatch.addBatch(sqlStringDriverGrammar);
				dbs.setJdbcCommand("stmtBatch.addBatch(sqlStringDriverGrammar)");
				batchExecUnit.setCountUpdates(0);
				logSqlInstructionInfo(dbs);
			}
			
			// Eventuali warning 
			if (stmtBatch.getWarnings() != null) {
				dbs.setSqlWarning(stmtBatch.getWarnings());
				dbs.setWarningMessage(stmtBatch.getWarnings().getLocalizedMessage());
				dbs.setSqlErrorCode(stmtBatch.getWarnings().getErrorCode());
				dbs.setSqlStatus(stmtBatch.getWarnings().getSQLState());
			}
			
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		
		return stmtBatch;
	}


	/**
     * 
     * Eliminazione statements Sql inserite con batchAdd()
     * 
     */
	public void batchClear(DataBaseStatusDetailed dbs)  {		

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_BATCH_CLEAR);
		
		try {
			this.stmtBatch.clearBatch();
			this.stmtBatch = null;
			dbs.setJdbcCommand("stmtBatch.clearBatch()");
			logSqlInstructionInfo(dbs);
			
			// Eventuali warning 
			if (stmtBatch.getWarnings() != null) {
				dbs.setSqlWarning(stmtBatch.getWarnings());
				dbs.setWarningMessage(stmtBatch.getWarnings().getLocalizedMessage());
				dbs.setSqlErrorCode(stmtBatch.getWarnings().getErrorCode());
				dbs.setSqlStatus(stmtBatch.getWarnings().getSQLState());
			}
			
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		
		return;
	}


	/**
     * 
     * Eliminazione statements Sql inserite con batchAdd()
     * 
     */
	public void batchExec(DataBaseBatchExecUnit arBatchExecUnit[]
					     ,DataBaseStatusDetailed dbs
					      )  {		
        
		int arRetCountUpdates[] = null;
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_BATCH_EXEC);
		
		try {
			arRetCountUpdates = stmtBatch.executeBatch();
			dbs.setJdbcCommand("stmtBatch.executeBatch()");
			logSqlInstructionInfo(dbs);

			// Eventuali warning 
			if (stmtBatch.getWarnings() != null) {
				dbs.setSqlWarning(stmtBatch.getWarnings());
				dbs.setWarningMessage(stmtBatch.getWarnings().getLocalizedMessage());
				dbs.setSqlErrorCode(stmtBatch.getWarnings().getErrorCode());
				dbs.setSqlStatus(stmtBatch.getWarnings().getSQLState());
			}
			
			// Scan comandi Sql e update contatore updates
			for (int i = 0; i < arBatchExecUnit.length; i++) {
				arBatchExecUnit[i].setCountUpdates(arRetCountUpdates[i]);
			}
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return;
	 }





	/**
	 * 
	 * Esecuzione di generico DML sql.
	 * Può contenere qualsiasi istruzione Sql di Select, Insert, Update o Delete.
	 * Restituisce un resultset diverso da null in caso di istruzione Select.
	 * In caso di Select non ci sono limiti al numero di righe da restituire.
	 * @throws SQLException 
	 * 
	 */
	public ResultSet execSqlGeneric(Connection dbConn
						           ,String sqlString 
								   ,DataBaseStatusDetailed dbs
							  	   ) throws SQLException  {		
		ResultSet rs =  null;
		rs = execSqlGenericCommon(dbConn, sqlString, dbs, 0);		// Nessun limite alle righe da recuperare
		return rs;
	}

	/**
	 * 
	 * Esecuzione di generico DML sql.
	 * Può contenere qualsiasi istruzione Sql di Select, Insert, Update o Delete.
	 * Restituisce un resultset diverso da null in caso di istruzione Select.
	 * In caso di Select non ci sono limiti al numero di righe da restituire.
	 * @throws SQLException 
	 * 
	 */
	public ResultSet execSqlGeneric( Connection dbConn
						           , String sqlString 
								   , DataBaseStatusDetailed dbs
								   , int maxRows
							  	   ) throws SQLException  {		
		ResultSet rs =  null;
		rs = execSqlGenericCommon(dbConn, sqlString, dbs, maxRows);		// Limite alle righe da recuperare
		return rs;
	}

	
	/*
	 * 
	 * Esecuzione di generico DML sql.
	 * Può contenere qualsiasi istruzione Sql di Select, Insert, Update o Delete.
	 * Restituisce un resultset diverso da null in caso di istruzione Select.
	 * @throws SQLException 
	 * 
	 */
	private ResultSet execSqlGenericCommon(Connection dbConn
						           ,String sqlString 
								   ,DataBaseStatusDetailed dbs
								   , int maxRows
							  	   ) throws SQLException  {		
		
		ResultSet rs =  null;
		Statement stmt = null;
		String sqlStringDriverGrammar = ""; // Istruzione nella grammatica del driver corrente
		boolean sqlSelect = false;

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_GENERIC_SQL);
		
        // Esecuzione statement Sql 
		try {
     		sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			// esecuzione istruzione
			stmt = dbConn.createStatement();
			if (maxRows > 0) {
				stmt.setMaxRows(maxRows);		// limit		
			}
 			dbs.setJdbcCommand("dbConn.createStatement()");
			dbs.setSqlString(sqlString);
			logSqlInstructionInfo(dbs);
			dbs.setJdbcCommand("stmt.execute(sqlString)");
			sqlSelect = stmt.execute(sqlString);
			logSqlInstructionInfo(dbs);
			
			// Se non è un'istruzione di aggiornamento è stato prodotto un ResultSet
			if (sqlSelect) {
				rs = stmt.getResultSet();
				dbs.setJdbcCommand("stmt.getResultSet()");
				logSqlInstructionInfo(dbs);
			}
			
			// Eventuali warning 
			if (stmt.getWarnings() != null) {
				dbs.setSqlWarning(stmt.getWarnings());
				dbs.setWarningMessage(stmt.getWarnings().getLocalizedMessage());
				dbs.setSqlErrorCode(stmt.getWarnings().getErrorCode());
				dbs.setSqlStatus(stmt.getWarnings().getSQLState());
			}
			logSqlInstructionInfo(dbs);
			
		} catch (SQLException e) {
			dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			dbs.setExcpOrigin(e);
			logSqlInstructionError(dbs, e);
			throw e;
		}

		return rs;
	}

    /**
	 * 
	 * Creazione singola tabella su database.
	 * Le singole colonne da inserire sono descritte dall'array di
	 * DataBaseItemDescriptor in input.
	 * 
	 */
	public void tableCreate(Connection dbConn 
			               ,String dbTableName
			               ,DataBaseItemDescriptor arTableItem[]
					       ,DataBaseStatusDetailed dbs
	                       )  {	
		
		String sqlString = "";
		String sqlStringDriverGrammar = "";
		String commaToPut = "";
		boolean areTherePrumaryKeyColumns = false;
	    
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_CREATE);

		////////////////////////////////////////////////////////////////////
		// Composizione istruzione sql di Create                          //
		////////////////////////////////////////////////////////////////////	
		
		sqlString =  "CREATE TABLE" +  dbTableName;
		sqlString =  sqlString +  " (";
		
		// Scan colonne 
		for (DataBaseItemDescriptor tableItem : arTableItem) {
			// Nome
			sqlString =  sqlString +  tableItem.getDbColumn() +  " "; 
			// Tipo
			sqlString =  sqlString +  tableItem.getDbColumnType().toString() +  " "; 
			// Lunghezza  
			sqlString =  sqlString +  "(";
			sqlString =  sqlString +  tableItem.getDbColumnLength() +  " "; 
			if (tableItem.getDbColumnType() == EnumDataBaseJdbcSqlType.DECIMAL) {
				sqlString =  sqlString +  ",";
				sqlString =  sqlString +  tableItem.getDbColumnDec();
			}
			sqlString =  sqlString +  ")";
			// Null
			if (!tableItem.isDbNullable()) {
				sqlString =  sqlString +  "NOT NULL ";
			}
			// Indicatore di campi Primary Key
			if (tableItem.isPrimaryKey()) {
				areTherePrumaryKeyColumns = true;
			}
		}

		// Primary Key
		if (areTherePrumaryKeyColumns) {
			sqlString = "CONSTRAINT pk-" + dbTableName + " PRIMARY KEY (";
			// Scan colonne 
			for (int i = 0; i < arTableItem.length; i++) {
				if (arTableItem[i].isPrimaryKey()) {
					sqlString = sqlString + commaToPut;
					commaToPut = ",";
					areTherePrumaryKeyColumns = true;
					sqlString = sqlString + arTableItem[i].getDbColumn();
				}
			}
			sqlString = sqlString + " )"; // Chiusura parentesi Primary Key
		}
		
		sqlString =  sqlString +  ")";     // Chiusura parentesi colonne

		
		////////////////////////////////////////////////////////////////////
		// Esecuzione istruzione sql di Create                            //
		////////////////////////////////////////////////////////////////////	
		
		
		// Esecuzione effettiva statement
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			Statement stmt = dbConn.createStatement();  
			stmt.execute(sqlString);							  // Esecuzione
			dbs.setJdbcCommand("stmt.execute(sqlString)");
			logSqlInstructionInfo(dbs);
			dbs.setUpdateCount(stmt.getUpdateCount());
			stmt.close();
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return;
	}

	/**
	 * 
	 * Drop tabella su database
	 * 
	 */
	public void tableDrop(Connection dbConn 
            			 ,String dbTableName
            			 ,DataBaseStatusDetailed dbs
                         )  {	

		String sqlString = "";
		String sqlStringDriverGrammar = "";
	    
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_DROP);

		// Istruzione sql di Drop                            
		sqlString =  "DROP TABLE" +  dbTableName;
		
		// Esecuzione effettiva statement
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			Statement stmt = dbConn.createStatement();
			stmt.execute(sqlString);
			dbs.setJdbcCommand("stmt.execute(sqlString)");
			logSqlInstructionInfo(dbs);
			// Esecuzione
			dbs.setUpdateCount(stmt.getUpdateCount());
			stmt.close();
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return;
	}

	/**
	 * 
	 * Fornisce L'elenco delle tabelle definite
	 * 
	 */
	public DataBaseMetaTable[] tableInfo(
			              Connection dbConn 
			             ,String catalog			    // "" tabelle senza catalogo, null nessun controllo
			             ,String schemaPattern          // "" tabelle senza schema, null nessun controllo
			             ,String tableNamePattern       // Pattern nomi tabelle da estrarre
			             ,String[] tableTypes           // Null nessun controllo sul tipo tabella (Table, view,..)
						 ,DataBaseStatusDetailed dbs
			            )  {	

		DatabaseMetaData dbmd = null;
		DataBaseMetaTable dbmt = null;
		DataBaseMetaTable ar_dbmt[] = null;
		ArrayList<DataBaseMetaTable> al_dbmt = null;
		ResultSet rs = null;
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_INFO);
		al_dbmt  = new ArrayList<DataBaseMetaTable>();
		
		try {
			dbmd = dbConn.getMetaData();  
			rs = dbmd.getTables(catalog, schemaPattern, tableNamePattern, tableTypes);
			dbs.setJdbcCommand("dbConn.getMetaData().getTables(catalog, schemaPattern, tableNamePattern, tableTypes)");
            logSqlInstructionInfo(dbs);
            
			// Scan Resultset
			while (rs.next()) {
				
				 // estraggo da resultset e carico in array di meta dati
				 dbmt = new DataBaseMetaTable();
				 dbmt.setCatalog(rs.getString(1));
				 dbmt.setSchema(rs.getString(2));
				 dbmt.setTableName(rs.getString(3));
				 dbmt.setRemarks(rs.getString(5));
				 al_dbmt.add(dbmt);
			}
			// Converto Array list in array
			ar_dbmt = new DataBaseMetaTable[al_dbmt.size()];
			ar_dbmt = al_dbmt.toArray(ar_dbmt);
			
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return ar_dbmt; 
	}


	/**
	 * 
	 * Fornisce L'elenco delle colonne di una tabella con tutte le informazioni 
	 * recuperate attraverso il driver sql corrente.
	 * 
	 */
	public DataBaseMetaTableColumn[] tableInfoColumns(
					            Connection dbConn 
					           ,String tableName	 // "" tabelle senza catalogo, null nessun controllo
							   ,DataBaseStatusDetailed dbs
					          )  {	

		DatabaseMetaData dbmd = null;
		DataBaseMetaTableColumn dbmtc = null;
		DataBaseMetaTableColumn ar_dbmtc[] = null;
		ArrayList<DataBaseMetaTableColumn> al_dbmtc = null;
		ResultSet rs = null;
		String columnNamePK = "";
		
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_INFO_COLUMN);
		al_dbmtc  = new ArrayList<DataBaseMetaTableColumn>();
		
		try {
			dbmd = dbConn.getMetaData();  
			rs = dbmd.getColumns(null, null, tableName, null);
			dbs.setJdbcCommand("dbConn.getMetaData().getColumns(null, null, tableName, null)");
			logSqlInstructionInfo(dbs);
            
			// Scan Resultset con le colonne della tabella
			while (rs.next()) {
				
				// estraggo da resultset e carico in array di meta dati
				dbmtc = new DataBaseMetaTableColumn();
				dbmtc.setTableName(tableName);
				dbmtc.setColumnName(rs.getString("COLUMN_NAME"));
				dbmtc.setDataType(rs.getInt("DATA_TYPE"));
				dbmtc.setColumnSize(rs.getInt("COLUMN_SIZE"));
				dbmtc.setDecimalDigits(rs.getInt("DECIMAL_DIGITS"));
				dbmtc.setRemarks(rs.getString("REMARKS"));
				dbmtc.setDefaultValue(rs.getString("COLUMN_DEF"));
				if (rs.getString("IS_NULLABLE").equals("YES")) {
					dbmtc.setNullable(true);
				} else {
					dbmtc.setNullable(true);
				}
				if (rs.getString("IS_AUTOINCREMENT").equals("YES")) {
					dbmtc.setAutoIncrement(true);
				} else {
					dbmtc.setAutoIncrement(true);
				}
				al_dbmtc.add(dbmtc);
			}
			rs.close();
				
			// Converto Array list in array
			ar_dbmtc = new DataBaseMetaTableColumn[al_dbmtc.size()];
			ar_dbmtc = al_dbmtc.toArray(ar_dbmtc);
			
			// Lettura colonne primary key per la tabella in questione
			rs = dbmd.getPrimaryKeys(null, null, tableName);
			
			// Scan Resultset con le colonne primary key della tabella
			while (rs.next()) {
				columnNamePK = rs.getString(4);
				// Scan colonne tabella
				for (DataBaseMetaTableColumn dataBaseMetaTableColumn : ar_dbmtc) {
					if (dataBaseMetaTableColumn.getColumnName().equals(columnNamePK)) {
						dataBaseMetaTableColumn.setPrimaryKey(true);
					}
				}
			}
			rs.close();
			
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return ar_dbmtc;
	}



	/**
	 * 
	 * Crea un indice in una tabella
	 * 
	 */
	public void tableIndexCreate(Connection dbConn 
							    ,String dbTableName
							    ,String dbTableIndexName
							    ,String ar_colIndex[]
							    ,String ar_ascDesc[]  
							    ,boolean uniqueIndex                
							    ,DataBaseStatusDetailed dbs
				                )  {	


		String sqlString = "";
		String sqlStringDriverGrammar = "";
		String commaToPut = "";
	    
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_INDEX_CREATE);

		////////////////////////////////////////////////////////////////////
		// Composizione istruzione sql di Create                          //
		////////////////////////////////////////////////////////////////////	
		
		sqlString =  "CREATE ";
		if (uniqueIndex) {
			sqlString =  sqlString +  "UNIQUE ";  
		}
		sqlString =  sqlString +  "INDEX " + dbTableIndexName;
		sqlString =  sqlString +  "ON " + dbTableName;
		
		sqlString =  sqlString +  "( ";
		
		// Scan colonne indice
		for (int i = 0; i < ar_colIndex.length; i++) {
			sqlString =  commaToPut + sqlString + ar_colIndex[i];
			sqlString =  sqlString + " " + ar_ascDesc[i];
			commaToPut = ",";
		}
		sqlString =  sqlString +  ")";     // Chiusura parentesi colonne indice

		
		////////////////////////////////////////////////////////////////////
		// Esecuzione istruzione sql di Create  Index                     //
		////////////////////////////////////////////////////////////////////	
		
		
		// Esecuzione effettiva statement
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			Statement stmt = dbConn.createStatement();
			stmt.execute(sqlString);							  // Esecuzione
			dbs.setJdbcCommand("stmt.execute(sqlString)");
			dbs.setUpdateCount(stmt.getUpdateCount());
			stmt.close();
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

		return ;
	}


	/**
	 * 
	 * Elimina un indice da una tabella
	 * 
	 */
	public void tableIndexDrop(Connection dbConn 
							  ,String dbTableName
							  ,String dbTableIndexName
							  ,DataBaseStatusDetailed dbs
				              )  {	

		String sqlString = "";
		String sqlStringDriverGrammar = "";
	    
		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_INDEX_DROP);

		// Istruzione sql di Drop                            
		sqlString =  "DROP INDEX " +  dbTableIndexName + " ON " + dbTableName;
		
		// Esecuzione effettiva statement
		try {
			sqlStringDriverGrammar = dbConn.nativeSQL(sqlString); // Trasformazione secondo grammatica del driver corrente
			dbs.setTableName(dbTableName);
			dbs.setSqlString(sqlStringDriverGrammar);
			Statement stmt = dbConn.createStatement();
			stmt.execute(sqlString);							  // Esecuzione
			dbs.setUpdateCount(stmt.getUpdateCount());
			stmt.close();
			dbs.setJdbcCommand("stmt.execute(sqlString)");
			logSqlInstructionInfo(dbs);
		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}
		return;
	}

	/**
	 * 
	 * Fornisce informazioni sugli indici di una tabella
	 * 
	 */
	public DataBaseMetaTableColumn[] tableIndexInfo(
						            Connection dbConn 
						           ,DataBaseMetaTable dbmt	   // "" tabelle senza catalogo, null nessun controllo
								   ,DataBaseStatusDetailed dbs
						          )  {	

		DatabaseMetaData dbmd = null;
		DataBaseMetaTableColumn dbmtc = null;
		DataBaseMetaTableColumn ar_dbmtc[] = null;
		ArrayList<DataBaseMetaTableColumn> al_dbmtc = null;
		ResultSet rs = null;

		clearDatabaseStatusStructure(dbs);
		dbs.setTypeOperation(EnumDataBaseOperation.DB_TABLE_INDEX_INFO_COLUMN);
		al_dbmtc  = new ArrayList<DataBaseMetaTableColumn>();

		try {
			dbmd = dbConn.getMetaData();  
			rs = dbmd.getIndexInfo(dbmt.getCatalog(), dbmt.getSchema(), dbmt.getTableName(), false, false);
			dbs.setJdbcCommand("dbConn.getMetaData().getIndexInfo(dbmt.getCatalog(), dbmt.getSchema(), dbmt.getTableName(), false, false)");
			logSqlInstructionInfo(dbs);
            
			// Scan Resultset con le colonne della tabella
			while (rs.next()) {

				// estraggo da resultset e carico in array di meta dati
				dbmtc = new DataBaseMetaTableColumn();
				dbmtc.setTableName(dbmt.getTableName());
				dbmtc.setIndexName(rs.getString(6));
				dbmtc.setColumnName(rs.getString(9));
				if (rs.getBoolean(4)) {
					dbmtc.setIndexUnique(false);
				} else {
					dbmtc.setIndexUnique(true);
				}
				if (rs.getString(10).equals("A")) {
					dbmtc.setIndexAscending(true);
				} else {
					dbmtc.setIndexAscending(false);
				}
				al_dbmtc.add(dbmtc);
			}
			// Converto Array list in array
			ar_dbmtc = new DataBaseMetaTableColumn[al_dbmtc.size()];
			ar_dbmtc = al_dbmtc.toArray(ar_dbmtc);

		} catch (SQLException e) {
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			logSqlInstructionError(dbs, e);
		}

	return ar_dbmtc;
}


   
		 
	///////////////////////////////////////////////////////////////////////////////////////////////////		 
    ///   Getter e Setter                                                                          ////		 
    ///////////////////////////////////////////////////////////////////////////////////////////////////		 

    /**
	 * @return the logger facade
	 */
	public LoggerFacade getLf() {
		return lf;
	}

	/**
	 * @param logger facade the lf to set
	 */
	public void setLf(LoggerFacade lf) {
		this.lf = lf;
	}


	/**
	 * @return the dbType
	 */
	public EnumDataBase getDbType() {
		return dbType;
	}


	/**
	 * @param dbType the dbType to set
	 */
	public void setDbType(EnumDataBase dbType) {
		this.dbType = dbType;
	}


	/**
	 * @return the dbName
	 */
	public String getDbName() {
		return dbName;
	}


	/**
	 * @param dbName the dbName to set
	 */
	public void setDbName(String dbName) {
		this.dbName = dbName;
	}


	/**
	 * @return the user
	 */
	public String getUser() {
		return user;
	}


	/**
	 * @param user the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}


	/**
	 * @return the pwd
	 */
	public String getPwdUser() {
		return this.pwdUser;
	}


	/**
	 * @param pwd the pwd to set
	 */
	public void setPwdUse(String pwdUser) {
		this.pwdUser = pwdUser;
	}

	/**
	 *  Update valori campi Java nel descrittore colonne, con valori correnti del resultSet.
	 * 
	 */
	public void updateJavaFieldsValue(ResultSet rs, DataBaseItemDescriptor[] arDataBaseItemDescriptor) throws SQLException, ExceptionAmrita  {

		// Scan colonne presenti nel ResultSet (definite nel descrittore)
		for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
			
			if (dbid.getJavaFieldClassNameSimple().equals("String")) {
				dbid.setJavaFieldValue(rs.getString(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("char")) {
				dbid.setJavaFieldValue(rs.getString(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("byte")) {
				dbid.setJavaFieldValue(rs.getByte(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("boolean")) {
				dbid.setJavaFieldValue(rs.getBoolean(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("int")) {
				dbid.setJavaFieldValue(rs.getInt(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("long")) {
				dbid.setJavaFieldValue(rs.getLong(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("double")) {
				dbid.setJavaFieldValue(rs.getDouble(dbid.getDbColumn()));
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("float")) {
				dbid.setJavaFieldValue(rs.getFloat(dbid.getDbColumn()));
				continue;
			}	

			// Enumerazione: generazione oggetto Enum da numero ordinale espresso come int
			if (dbid.isJavaEnumeration()) {
				int ordinal = rs.getInt(dbid.getDbColumn());
				Enum<?> e = retrieveEnumFromOrdinal(dbid.getJavaFieldClass(), ordinal);
				dbid.setJavaFieldValue(e);
				continue;
			}	

			// Tipo campo non gestito: logging e lancio eccezione generale gestita
			ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PROGRAM_INTERNAL, null);
			logMessage(EnumMessageType.ERROR_INTERNAL, "EI0011", excp, dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable() );
			logMessage(EnumMessageType.INFORMATION, "MI0045", null, dbid.getJavaFieldClassName());
			throw excp;
			
		}
	}
    

	 
	///////////////////////////////////////////////////////////////////////////////////////////////////		 
    ///   Metodi privati                                                                           ////		 
    ///////////////////////////////////////////////////////////////////////////////////////////////////		 

	
	/*
     * 
     * Controllo parametri in input
     * 
     */
	private boolean areParmConstructorGood( EnumDataBase dbType,
											String driverName, 
											String ipAddress, 
											String dbName, 
											String user,
											String pwdUser) {
		
		boolean bCtrlOK = true;

		return bCtrlOK;
	}


	/*
     * 
     * Creazione string Sql per generica istruzione crud insert/select/update/delete
     * 
     */
	private String createSqlStringCrud(String sqlInstr
			                          ,String dbTableName
							          ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
								      ) {
		String sqlString = "";
		String comma = "";
        
		// SELECT/DELETE/INSERT/UPDATE
		sqlString = sqlInstr + " ";
		
		// Colonne subito dopo Select
		if (sqlInstr.equals("SELECT")) {
			comma = "";
			// Tuttele colonne
			sqlString = sqlString + " * ";
		}
		
		// FROM dopo campi Select o dopo DELETE
		if (sqlInstr.equals("SELECT") || sqlInstr.equals("DELETE")) {
     		sqlString = sqlString + " FROM ";
		}
		
		// INTO dopo INSERT
		if (sqlInstr.equals("INSERT")) {
     		sqlString = sqlString + " INTO ";
		}

		// Tabella dopo FROM di Select/Delete o INTO di Insert o Update
    	sqlString = sqlString + dbTableName;

		// Set colonne per UPDATE
		if (sqlInstr.equals("UPDATE")) {
     		sqlString = sqlString + " SET ";
			comma = "";
			// Colonne = valore
			for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
				if (dbid.isPrimaryKey()) {
					continue;
				}
				sqlString = sqlString + comma ;
				sqlString = sqlString + dbid.getDbColumn();
				sqlString = sqlString + " = ";
				sqlString = sqlString + dbid.getDbColumnValue();
				comma = ",";
			}
		}
		
		// (Col,..) e Values(val,..) per INSERT
		if (sqlInstr.equals("INSERT")) {
			comma = "";
			// Colonne 
			sqlString = sqlString + " (";
			for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
				sqlString = sqlString + comma + dbid.getDbColumn();
				comma = ",";
			}
			sqlString = sqlString + ")";
			// Valori da inserire per ogni colonna
			sqlString = sqlString + " VALUES(";
			comma = "";
	        for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
	        	sqlString = sqlString + comma + dbid.getDbColumnValue();
	        	comma = ",";
			}
	        sqlString = sqlString + ")";
		}
		
		// Where se non INSERT
		if (!sqlInstr.equals("INSERT")) {
			sqlString = sqlString + " WHERE "
					  + insertWhereConditionPrimaryKey(arDataBaseItemDescriptor);
		}
		return sqlString.trim();
	}

	
	/*
     * 
     * Creazione stringa di Where sui campi di chiave primaria
     * 
     */
	private String insertWhereConditionPrimaryKey(DataBaseItemDescriptor[] arDataBaseItemDescriptor) {
		
		String sqlString = "";

		// Where condition
		for (int i = 0; i < arDataBaseItemDescriptor.length; i++) {
			DataBaseItemDescriptor dbid = arDataBaseItemDescriptor[i];
			if (dbid.isPrimaryKey()) {
	        	sqlString = sqlString + dbid.getDbColumn() + " = " + dbid.getDbColumnValue();
	        	if (i+1 < arDataBaseItemDescriptor.length) {
					if (arDataBaseItemDescriptor[i + 1].isPrimaryKey()) {
						sqlString = sqlString + " AND ";
					}
				}
			}
		}
		return sqlString;
	}



	/*
	 * 
	 *  Update colonne in riga corrente result set con i valori java orrenti
	 * 
	 */
	private void updateResultSetColumns(ResultSet rs, DataBaseItemDescriptor[] arDataBaseItemDescriptor) throws SQLException, ExceptionAmrita {
		
		// Scan colonne da aggiornare
		for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
			
			Type javaFieldTypeClass = dbid.getJavaFieldType();
			String javaFieldType = javaFieldTypeClass.toString().substring(10);
			
			if (dbid.getJavaFieldClassNameSimple().equals("String")) {
				rs.updateString(dbid.getDbColumn(), (String) dbid.getJavaFieldValue());	
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("char")) {
				rs.updateString(dbid.getDbColumn(), (String) dbid.getJavaFieldValue());	
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("byte")) {
				rs.updateByte(dbid.getDbColumn(), (Byte) dbid.getJavaFieldValue());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("boolean")) {
				rs.updateBoolean(dbid.getDbColumn(), (Boolean) dbid.getJavaFieldValue());  
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("int")) {
				rs.updateInt(dbid.getDbColumn(), (Integer) dbid.getJavaFieldValue()); 
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("long")) {
				rs.updateLong(dbid.getDbColumn(), (Long) dbid.getJavaFieldValue());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("double")) {
				rs.updateDouble(dbid.getDbColumn(), (Double) dbid.getJavaFieldValue());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("float")) {
				rs.updateFloat(dbid.getDbColumn(), (Float) dbid.getJavaFieldValue()); 
				continue;
			}
			// Enumerazione: estrazione numero ordinale e trattamento come int
			if (dbid.isJavaEnumeration()) {
				Enum e = (Enum) dbid.getJavaFieldValue();
				rs.updateInt(dbid.getDbColumn(), e.ordinal());   // Autoboxing evita di usare Integer
				continue;
			}	

			// Tipo campo non gestito: logging e lancio eccezione generale gestita
			ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PROGRAM_INTERNAL, null);
			logMessage(EnumMessageType.ERROR_INTERNAL, "EI0011", excp, dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable() );
			logMessage(EnumMessageType.INFORMATION, "MI0045", null, dbid.getJavaFieldClassName());
			throw excp;
			
		}
	}

	/*
	 * 
	 *  Update valori colonne da inserire nel database, a parire dai valori Java, esattamente come
	 *  devono essere inseriti nelle istruzioni Insert e Update
	 * 
	 */
	private void updateDbColumnsValue(DataBaseStatusDetailed dbs, DataBaseItemDescriptor[] arDataBaseItemDescriptor) throws ExceptionAmrita {

		// Scan colonne da inserire/aggiornare
		for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {
			
			// Tipi campi elementari
			if (dbid.getJavaFieldClassNameSimple().equals("String")) {
				dbid.setDbColumnValue("'" + (String) dbid.getJavaFieldValue() + "'");
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("char")) {
				dbid.setDbColumnValue("'" + (String) dbid.getJavaFieldValue() + "'");
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("byte")) {
				dbid.setDbColumnValue("'" + (String) dbid.getJavaFieldValue() + "'");
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("boolean")) {
				dbid.setDbColumnValue("0");
				if ((Boolean) dbid.getJavaFieldValue()) {dbid.setDbColumnValue("1");}
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("int")) {
				dbid.setDbColumnValue(((Integer) dbid.getJavaFieldValue()).toString());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("long")) {
				dbid.setDbColumnValue(((Long) dbid.getJavaFieldValue()).toString());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("double")) {
				dbid.setDbColumnValue(((Double) dbid.getJavaFieldValue()).toString());
				continue;
			}
			if (dbid.getJavaFieldClassNameSimple().equals("float")) {
				dbid.setDbColumnValue(((Float) dbid.getJavaFieldValue()).toString());
				continue;
			}	
			
			// Enumerazione: estrazione numero ordinale e trattamento come int
			if (dbid.isJavaEnumeration()) {
				@SuppressWarnings("rawtypes")
				Enum e = (Enum) dbid.getJavaFieldValue();
				// Campo enumerazione a null: logging e lancio eccezione generale gestita
				if (e == null) {
					ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_DB_COLUMN_MAPPING_NULL, null);
					logMessage(EnumMessageType.ERROR_INTERNAL, "EI0012", excp, dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable(), dbid.getDbColumn(), dbs.getEntityName());
					throw excp;
				}
				dbid.setDbColumnValue(new Integer(e.ordinal()).toString());
				continue;
			}	

			// Tipo campo non gestito: logging e lancio eccezione generale gestita
			ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PROGRAM_INTERNAL, null);
			logMessage(EnumMessageType.ERROR_INTERNAL, "EI0011", excp, dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable() );
			logMessage(EnumMessageType.INFORMATION, "MI0045", null, dbid.getJavaFieldClassName());
			throw excp;
		}
	}


	/*
	 * Vengono aggiornati i parametri variabili dell'istruzione preparata.
	 * I parametri devono essere nell'istruzione Sql preparata, nello stesso
	 * ordine di quello definito nell'array fornito di DataBaseItemDescriptor[].
	*/
	@SuppressWarnings("unchecked")
	private void updatePlaceHolders(PreparedStatement preparedStatement,
			                        DataBaseItemDescriptor[] arDataBaseItemDescriptor
			                        ) throws SQLException, ExceptionAmrita {
		
        int numPlaceHolder = 0;
		
		// Scan colonne presenti nel descrittore di item  
		for (DataBaseItemDescriptor dbid : arDataBaseItemDescriptor) {

			Type javaFieldTypeClass = dbid.getJavaFieldType();
			String javaFieldType = javaFieldTypeClass.toString().substring(10);

			if (javaFieldType.equals("String")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setString(numPlaceHolder, (String) dbid.getJavaFieldValue());
					continue;
				}
				
			}
			if (javaFieldType.equals("char")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setString(numPlaceHolder, (String) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("byte")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setByte(numPlaceHolder, (Byte) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("boolean")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setBoolean(numPlaceHolder, (Boolean) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("int")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setInt(numPlaceHolder, (Integer) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("long")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setLong(numPlaceHolder, (Long) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("double")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setDouble(numPlaceHolder, (Long) dbid.getJavaFieldValue());
					continue;
				}
			}
			if (javaFieldType.equals("float")) {
				if (dbid.isPreparedPlaceHolder()) {
					numPlaceHolder++;
					preparedStatement.setFloat(numPlaceHolder, (Float) dbid.getJavaFieldValue());
					continue;
				}
			}
			
			// Enumerazione: generazione numero ordinale espresso come int da oggetto Enum 
			if (dbid.isJavaEnumeration()) {
				numPlaceHolder++;
				Enum e = (Enum) dbid.getJavaFieldValue();
				preparedStatement.setInt(numPlaceHolder, e.ordinal());
				continue;
			}	

			// Tipo campo non gestito: logging e lancio eccezione generale gestita
			ExceptionAmrita excp = new ExceptionAmrita(null, EnumAmritaExceptionError.ERROR_PROGRAM_INTERNAL, null);
			logMessage(EnumMessageType.ERROR_INTERNAL, "EI0011", excp, dbid.getJavaFieldClassName(), dbid.getJavaFieldName(), dbid.getDbTable() );
			logMessage(EnumMessageType.INFORMATION, "MI0045", null, dbid.getJavaFieldClassName());
			throw excp;
				
		}
	
	}

	/*
	 * 
	 * Data un oggetto classe di una enumerazione e il numero ordinale, fornisce l'oggetto Enum appropriato
	 * 
	 */
	private Enum retrieveEnumFromOrdinal(Class javaFieldClass, int ordinal) {
		
		Enum ar_enum[] =  null;
 		ar_enum = (Enum[]) rm.invokeMethodStatic(javaFieldClass, "values", null, null);
		  
 		// Se errore logging e lancio exception
 		if (ar_enum == null) {
			// TODO
 			
		}
 		
 		
		return ar_enum[ordinal];
	}


	
	/*
	 * 
	 * Invio su log messaggio con numero arbitrario di parametri.
	 */
	private void logMessage(EnumMessageType messageType
			               ,String messageCode
			               ,ExceptionAmrita excp
			               ,String ...errorPlaceHolders
			               ) {
	    String arParm[] = null;
		
	    // Parametri da inserire nel messaggio di log
		if (errorPlaceHolders.length == 0) {
			arParm = null;
		} else {
			arParm = errorPlaceHolders;
		}
		
		// Log messaggio: viene prodotto solo il messaggio con o senza PrintStackTrace in base a excp
        lf.writeRow(messageType, messageCode, arParm, excp); 
	}
	
	
	
	/*
	 * Generazione entry su log con informazioni istruzione Sql eseguita se previsto
	 * dai parametri di configurazione. 
	 * Sul log vengono scritti messaggi di tipo INFO e se dovesse verificarsi qualche exceptio sql, 
	 * verrebbero loggati anche i relativi messaggi di tipo DEBUG.
	 * 
	*/
	private void logSqlInstructionInfo(DataBaseStatusDetailed dbs) {
		
		String arParm[] = null;
		
		// Nessun errore e nessuna direttiva di log per ogni istruzione sql
		if (!sd.isDataBaseLogAnySql()) {
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
        lf.writeRow(EnumMessageType.INFORMATION, "MI0027", arParm, null); 

        // Messaggio stringa Sql 
		if (!dbs.getSqlString().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getSqlString();
			lf.writeRow(EnumMessageType.INFORMATION, "MI0028", arParm, null);
		}
		
		// Comando Jdbc 
		if (sd.isLogVerbose() && !dbs.getJdbcCommand().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getJdbcCommand();
			lf.writeRow(EnumMessageType.INFORMATION, "MI0029", arParm, null);
		} 
	
	}


	/*
	 * Generazione entry su log con informazioni istruzione Sql se previsto
	 * dai parametri di configurazione. Sul log vengono scritti messaggi di tipo
	 * INFO e se dovesse verificarsi qualche exception sql, verrebbero loggati
	 * anche i relativi messaggi di tipo DEBUG.
	 * Viene anche generata una Exception che viene passata al gestore del log.
	 * In questo modo viene stampato il PrintStacktrace dell'applicazione fino
	 * a questo momento.
	 * 
	*/
	private void logSqlInstructionError(DataBaseStatusDetailed dbs, SQLException excpSql) {
		
		String arParm[] = null;
		ExceptionAmritaSqlError excp = null;
		
		// Messaggio con printStackTrace  
		arParm = new String[6];
		arParm[0] = dbs.getTypeOperation().toString();
		arParm[1] = dbs.getStatusOperation().toString();
		arParm[2] = dbs.getSqlStatus().toString();
		arParm[3] = new Integer(dbs.getSqlErrorCode()).toString();
		arParm[4] = dbs.getTableName();
		arParm[5] = dbs.getWarningMessage();
		
		// Messaggio con PrintStackTrace solo se SqlErrorCode <> 0 e non è una disconnesssione dal db
		// Altrimenti significa Duplicate su insert o Notfound su Read/Update/Delete
        if (dbs.getSqlErrorCode() != 0 && dbs.getTypeOperation() != EnumDataBaseOperation.DB_CLOSE_CONNECTION) {
			lf.writeRow(EnumMessageType.INFORMATION, "EF0013", arParm, excpSql);
		} else {
			lf.writeRow(EnumMessageType.INFORMATION, "EF0013", arParm, null);
		}
		// Messaggio stringa Sql  (senza printStackTrace)
    	if (!dbs.getSqlString().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getSqlString();
			lf.writeRow(EnumMessageType.INFORMATION, "EF0014", arParm, null);
		} 
    	
		// Comando Jdbc
		if (!dbs.getJdbcCommand().equals("")) {
			arParm = new String[1];
			arParm[0] = dbs.getJdbcCommand();
			lf.writeRow(EnumMessageType.INFORMATION, "EF0015", arParm, null);
		} 
	}



	
	
	/*
	 * 
	 * Inizializzazione struttura con stato e parametri operazione Sql
	 * 
	*/
	private void clearDatabaseStatusStructure(DataBaseStatusDetailed dbs) {
		
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		dbs.setExcpOrigin(null);
		dbs.setSqlErrorCode(0);
		dbs.setSqlStatus("");
		dbs.setTableName("");
		dbs.setWarningMessage("");
        dbs.setSqlString("");
        dbs.setSqlStringOrigin("");
        dbs.setJdbcCommand("");
	}
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

	/*
	 *   CLASSE INTERNA DI SERVIZIO con tutte le informazioni relative a una connessione. 
	 *   Si tratta di tutte le informazioni statiche di inizializzazione e di tutte quelle
	 *   dinamiche, di accesso e di errore. 
	 */
	private class InnerDbConnection {
		Connection dbConn = null;		  // Connessione al data base			
	    boolean isConnActive = false;	  // True = connessione attiva e assegnabile
	    boolean isConnFree = true;		  // True = connessione rassegnabile
        String idAssigned = "";			  // identificativo richiedente
        String errorCodeSQL = "";         // 
        String errorStateSQL = "";        //
	}	
}
