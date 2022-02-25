package analyzer;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import enums.EnumAmritaExceptionError;
import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaAnnotationMissing;
import exception.ExceptionAmritaReflectionError;
import exception.ExceptionAmritaSqlAccessDuplicate;
import exception.ExceptionAmritaSqlAccessNotfound;
import exception.ExceptionAmritaSqlError;
import utilities.ReflectionManager;
import utilities.StringService;

/**
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseEntityInterface  
 * </h1>
 *  <p>
 * Questa classe funge da interfaccia per tutte le operazioni <b>CRUD</b> di accesso al database, 
 * quali lettura/scrittura oggetti, relazioni etc., oltre ad accessi selettivi. <br>
 * <p>
 * Le operazioni di gestione fisica del database vengono effettuate dalla classe {@link DataBaseManager}
 * che attraverso Jdbc interrroga e aggiorna il database in linea.<br>
 * L'utilizzo applicativo è elementare e si limita a fornire come parametro, per ogni operazione, 
 * il solo oggetto POJO della classe che implementa la gestione dell'Entity. <br>
 * <p>
 * Ciò avviene nei seguenti casi:<br>
 * - Accessi CRUD
 *   La chiave primaria deve essere valorizzata nell'oggetto POJO che descrive l'entity,
 * - Accessi generici di lettura a singola entity con condizioni di Where fornite.<br>
 *   Le condizioni di Where fornite ossono essere di qualsiasi tipo e complessità pur rispettando il linguaggio Sql.
 *   Viene restituito un array di oggetti POJO per ogni riga restituita.<br>
 * - Accessi generici di lettura a fronte di Select comunque complessa.<br>
 *   Viene fornito un oggetto POJO conforme agli standard Amrita come descrittore di entity.
 *   Devono essere descritte le singole colonne della Select, codificate con l'attribut AS nameColumn.
 *   Il nome della colonna viene quindi valorizzato nell'annotazione {@link DataBaseMappedColumns} mentre l'annotazione
 *   {@link DataBaseMappedTable} viene ignorata.<br>
 *   Il meccanismo diventa quindi lo stesso e viene resituito un array di oggetti POJO con le righe risultato
 * <p> 
 * L'oggetto POJO può essere un normale descrittore di entiy come <b>EntityObject</b> oppure un qualsiasi
 * oggetto conforme agli standard Amrita come descrittore di entity<br>
 * Per la realizzazione generalizzata e trasparente della mappatura dell'Entity con la tabella fisica, 
 * e dei campi dell'Entity con le colonne della tabella, vengono sfruttate massicciamente le <b>Annotation</b> 
 * e i meccanismi di <b>Reflection</b> realizzando di fatto viene realizzato una completa mappatura <b>ORM</b> <br>
 * L'unica attività applicativa da effettuare, per gli aggiornamenti <b>CRUD</b>, è quella di inserire, 
 * per ogni classe POJO di gestione Entity, le annotazioni relative al nome tabella e ai nomi delle colonne e dei campi chiave<br>
 * {@link DataBaseMappedTable} e {@link DataBaseMappedColumns}. <br>
 * Compito di {@link DataBaseManager} è quello, oltre che di eseguire la richiesta attraverso il driver
 * Jdbc corrente, di caricare dinamicamente con Reflection i campi dell'oggetto POJO rappresentante
 * l'Entity con i valori da database, e di effettuare l'operazione inversa.<br> 
 * <p>
 * Tutte le operazioni CRUD potrebbero essere effettuate direttamente utilizzando un oggetto della classe
 * {@link DataBaseManager}, che ha  tra l'altro il compito di loggare, in caso di errore Sql oppure nel caso di
 * esplicita richiesta di logging di tutte le istruzioni Sql, tutte le informazioni disponibili sulla richiesta
 * Sql e sul suo esito.<br>
 * L'utilizzo della classe DataBaseEntityInterface implica invece un'esecuzione ancora più <b>semplice</b> nelle operazioni CRUD, 
 * permettendo di concentrarsi sulle sole operazioni da eseguire (Create, Read, Updsate e Delete) sull'entity,
 * senza preoccuparsi della gestione dell'esito dell'operazione e degli status e return code fisici Sql restituiti.<br>
 * Inoltre le operazioni di consolidamento degli aggiornamenti (commit) sono rese trasparenti ed effettuate
 * automaticamente dopo il numero di aggiornamenti stabilito nel file di configurazione attraverso il parametro <b>DataBaseCommitBlockUpdates</b><br>
 * In ogni caso è possibile specificare un valore diverso a livello di esecuzione processo/funzione attraverso la direttiva
 * di processo. Il programma chiamante deve effettuare una commit al termine dell'elaborazione per committare
 * gli aggiornamenti residui.
 * <p>
 * A questo scopo sono disponibili eccezioni specifiche, che ereditano dall'eccezione generale di Amrita
 * {@link ExceptionAmrita} e che vengono lanciate sostanzialmente a fronte di riga sulla tabella Sql
 * non trovata (NOTFOUND), quando avrebbe dovuto esserci, oppure trovata (DUPLICATE), quando non avrebbe
 * dovuto esserci. Nel caso di errore Sql viene lanciata una eccezione specifica. <br>
 * A fronte di queste queste eccezioni generali è possibile accedere a tutte le informmazioni relative
 * all'operazione Sql (già loggate da DataBaseManager), attraverso il metodo <b>getInfoSql()</b>.
 * <p>
 * Questa classe deve essere utilizzata con una sessione con il database già aperta dal DataBaseManager e con
 * una connessione già aperta e disponibile, sempre ottenuta dal DataBaseManager.
 * <p>
 * <b> Esempio di Create </b>
 * <p>
 *  
 * 			dbm = new DataBaseManager(sd, database, driver, url, dbname, user,pwd, 3);<br>
 *			dbConn = dbm.getConnection(dbs);<br>
 *			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);<br>	
 *			<p> <b>
 *			entityObject.setSystem("A1");   	<br>					 
 *			entityObject.setSubSystem("SU");  <br>						  
 *          <p>
 *		 
 *			dbei.create(entityObject);<br>	</b>
 *			<p>
 *			dbei.commit(dbConn);<br>
 *			dbm.closeConnection(**dbConn, dbs);<br>
 *
 *		} catch (ExceptionAmritaSqlAccessDuplicate e) {<br>
 *			 
 *			e.printStackTrace();<br>
 *		} catch (ExceptionAmritaSqlError e) {<br>
 *			 
 *			e.printStackTrace();<br>
 *		} catch (ExceptionAmritaAnnotationMissing e) {<br>
 *			 
 *			e.printStackTrace();<br>
 *		} catch (ExceptionAmritaReflectionError e) {<br>
 *			 
 *			e.printStackTrace();<br>
 *		} catch (ExceptionAmrita e) {<br>
 *			e.printStackTrace();<br>
 *		}<br>
 *
 * 
 * 
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseEntityInterface
 *   
 */

public class DataBaseEntityInterface {
	
	// Reference a classi di utilità esterne
    @SuppressWarnings("unused")
	private UserConfiguration sd = null;				// Defaults di sistema
	private ReflectionManager rm = null;            // Gestore centralizzato reflection Java
	private LoggerFacade lf = null;                 // Gestore centralizzato logging
	
	// Campi di servizio per x connessione attiva, status, ...
	Connection dbConn = null;						// Connessione attiva
    DataBaseManager dbm = null;						// Gestore database
    DataBaseStatusDetailed dbs = null;				// Status ultima operazione effettuata
    ExceptionAmrita excp = null;                    // Exception lanciata al chiamante

	// Informazioni di database mapping da annotazioni classe gestione entity
	DataBaseItemDescriptor[] arDataBaseItemDescriptor = null;
	private String tableName = "";				    // Tabella db estratta da annotazione
	private String[] ar_ColumnMapping = null;       // Riga con campi java/db estratti da annotazione

	// Informazioni di controllo
	private int dataBaseCommitBlockUpdates = 100;   // Numero istruzioni da committare in blocco (da default o direttive)
	private int cntUpdatedDone = 0;                 // Contatore aggiornamenti effettuati
	private boolean autoCommitBlockUpdates = true;  // Al raggiungimento del numero di updates stabilito, commit automatica
	
	/**
	 * Costruttore
	 */
	public DataBaseEntityInterface(UserConfiguration sd, DataBaseManager dbm, Connection dbConn) {
 		this.sd = sd;
		this.dbm = dbm;
		this.dbConn = dbConn;
		this.lf = sd.getLoggerFacade();
		this.dbs = new DataBaseStatusDetailed();
		this.rm = new ReflectionManager();
		this.dataBaseCommitBlockUpdates = sd.getDataBaseCommitBlockUpdates();
	}

	/**
	 * Costruttore
	 */
	public DataBaseEntityInterface(UserConfiguration sd, DataBaseManager dbm) {
 		this.sd = sd;
		this.dbm = dbm;
		this.lf = sd.getLoggerFacade();
		this.dbs = new DataBaseStatusDetailed();
		this.rm = new ReflectionManager();
		this.dataBaseCommitBlockUpdates = sd.getDataBaseCommitBlockUpdates();
	}

	
	/**
	 * Gets the active connection.
	 * <br>
	 * @return the dbConn
	 */
	public Connection getDbConn() {
		return dbConn;
	}

	/**
	 * Sets the active connection.
	 * <br>
	 * @param dbConn the dbConn to set
	 */
	public void setDbConn(Connection dbConn) {
		this.dbConn = dbConn;
	}

	/**
	 * 
	 * Funzionalità CRUD create generica su una Entity<br>
	 * 
	 * I campi chiave della tabella sql associata all'entitity sono recuperati dalle annotazioni
	 * associate alla classe di gestione dell'Entity.
	 * Se viene restituito normalmente il controllo al chiamante significa riga inserita correttamente
	 * nella tabella specificata.<br>
	 * In caso di riga già presente oppure di errore Sql nell'esecuzione dell Insert viene
	 * invece lanciata l'opportuna exception. 
	 * In questo caso sono disponibili i metodi del parametro DataBaseStatusDetailed dbs
	 * per ottenere informazioni sull'esito generale dell' operazione tramite il metodo
	 * <b> getStatusOperation()</b> oppure di dettaglio come getSqlErrorCode()  getSqlStatus() etc.
	 * <p>
	 * Nel caso più normale l'esito dell'operazione potrà essere:<br>
	 * 
	 * 	<b> DB_OPERATION_OK</b>  Se Operazione senza errori e riga inserita  <br>  			  
	 *  <b> DB_DUPLICATE </b>	 Se Operazione senza errori e riga non inserita  <br>
	 *  <b> DB_ERROR_SQL </b>	 Se Operazione con errori Sql<br>
	 * <p>
	 * Se DB_OPERATION_OK e DB_DUPLICATE il controllo viene restituito al chiamante senza lanciare eccezioni.
	 * Se il valore restituito è true significa DB_OPERATION_OK e se false significa DB_DUPLICATE.
	 * 
	 * 
	 * @param Object entityObjectClass	
	 * @param DataBaseStatusDetailed dbs
	 * @throws ExceptionAmritaSqlError 
	 * @throws ExceptionAmritaSqlAccessDuplicate 
	 * @throws ExceptionAmritaAnnotationMissing 
	 * @throws ExceptionAmritaReflectionError 
	 * @throws ExceptionAmrita 
   */
	public boolean create(Object entityObjectClass )
			            throws ExceptionAmritaSqlError
					          ,ExceptionAmritaAnnotationMissing
					          ,ExceptionAmritaReflectionError
					          ,ExceptionAmrita
			          {
		
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_CREATE);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
		// Recupero nome tabella sql e righe con mapping campi java da annotation 
		this.ar_ColumnMapping = getInfoMappingFromAnnotations(entityObjectClass);   // -> this.tableName 
		dbs.setTableName(this.tableName);
		
		// Caricamento array descrittore campi per esecuzione dinamica
		this.arDataBaseItemDescriptor = new DataBaseItemDescriptor[this.ar_ColumnMapping.length];
		loadDataBaseItemDescriptor(entityObjectClass, this.arDataBaseItemDescriptor, this.ar_ColumnMapping, this.tableName);
		
		// Esecuzione DataBaseManager                                           
		dbm.crudCreate(dbConn, dbs, this.tableName, arDataBaseItemDescriptor);
		
		// Esito operazione OK: Riga inserita nel database e nessun errore fisico 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			ManageAutoCommit();
			return true;
		}
		
		//////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Riga duplicata nel database o errore fisico Sql //
		//////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante se errore sql
		if (isSqlErrorVendor(dbs)) {
			excp = new ExceptionAmritaSqlError(dbs, null, dbs.getExcpOrigin());
			throw excp;
		} 

		// Duplicate
		return false;
	}
    

	/**
	 * Funzionalità CRUD read generica su una Entity
	 * 
	 * I campi chiave della tabella sql associata all'entitity sono recuperati dalle annotazioni
	 * associate alla classe di gestione dell'Entity.
	 * Viene restituito un valore booleano e true significa riga entity letta correttamente
	 * dalla tabella specificata attraverso l'oggetto di gestione Entity fornito..<br>
	 * Se viene restituito <b>false</b> sicuramente la riga non è stata restituita. 
	 * In questo caso sono disponibili i metodi del parametro DataBaseStatusDetailed dbs
	 * per ottenere informazioni sull'esito generale dell' operazione <b> getStatusOperation()</b>
	 * oppure di dettaglio come <b>getSqlErrorCode()</b>  <b>getSqlStatus()</b> etc.
	 * <p>
	 * Nel caso più normale l'esito dell'operazione potrà essere:<br>
	 * 	<b> DB_OPERATION_OK</b>  Se Operazione senza errori e riga trovata  <br>  			  
	 *  <b> DB_NOTFOUND </b>	 Se Operazione senza errori e riga non trovata  <br>
	 *  <b> DB_ERROR_SQL </b>	 Se Operazione con errori Sql<br>
	 * 
	 * 
	 * 
	 * @param Object entityObjectClass	
	 * @param DataBaseStatusDetailed dbs
	 * @return boolean create row ok on database
	 * @throws ExceptionAmritaSqlAccessNotfound 
	 * @throws ExceptionAmritaSqlError 
	 * @throws ExceptionAmritaAnnotationMissing 
	 * @throws ExceptionAmritaReflectionError 
	 * @throws ExceptionAmrita 
	 */
	public boolean read(Object entityObjectClass)
					     throws ExceptionAmritaSqlError
							   ,ExceptionAmritaAnnotationMissing
							   ,ExceptionAmritaReflectionError
							   ,ExceptionAmrita
                         {

		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_READ);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
		// Recupero nome tabella sql e righe con mapping campi java da annotation 
		this.ar_ColumnMapping = getInfoMappingFromAnnotations(entityObjectClass);   // -> this.tableName
		dbs.setTableName(this.tableName);
	
		// Caricamento array descrittore campi per esecuzione dinamica
		arDataBaseItemDescriptor = new DataBaseItemDescriptor[this.ar_ColumnMapping.length];
		loadDataBaseItemDescriptor(entityObjectClass, arDataBaseItemDescriptor, this.ar_ColumnMapping, this.tableName);
		
		// Esecuzione DataBaseManager                                           
		dbm.crudRead(dbConn, dbs, this.tableName, arDataBaseItemDescriptor);

		// Esito operazione di lettura OK 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			// Aggiornamento campi entity con valori campi letti da DataBaseManager, con reflection e Invoke dinamiche
			updateEntityObjectFields(entityObjectClass, dbs, arDataBaseItemDescriptor);
			// Nessuna exception lanciata: nessun errore in fase di caricamento dati riga, return al chiamante
			if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
				return true;
			}
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Riga non presente nel database o errore fisico Sql   //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante se errore Sql
		if (isSqlErrorVendor(dbs)) {
			excp = new ExceptionAmritaSqlError(dbs, null, dbs.getExcpOrigin()); 
		}
		
		// Notfound
		return false;
	}
	
	
	/**
	 * Funzionalità CRUD update generica su una Entity
	 * 
	 * I campi chiave della tabella sql associata all'entitity sono recuperati dalle annotazioni
	 * associate alla classe di gestione dell'Entity.
	 * Viene restituito un valore booleano e true significa riga entity aggiornata correttamente
	 * dalla tabella specificata attraverso l'oggetto di gestione Entity fornito..<br>
	 * Se viene restituito false sicuramente la riga non è stata aggiornata. 
	 * In questo caso sono disponibili i metodi del parametro DataBaseStatusDetailed dbs
	 * per ottenere informazioni sull'esito generale dell' operazione <b> getStatusOperation()</b>
	 * oppure di dettaglio come <b>getSqlErrorCode()</b>  <b>getSqlStatus()</b> etc.
	 * <p>
	 * Nel caso più normale l'esito dell'operazione potrà essere:<br>
	 * 	<b> DB_OPERATION_OK</b>  Se Operazione senza errori e riga trovata  <br>  			  
	 *  <b> DB_NOTFOUND </b>	 Se Operazione senza errori e riga non trovata  <br>
	 *  <b> DB_ERROR_SQL </b>	 Se Operazione con errori Sql<br>
	 * 
	 * <p>
	 * Se DB_OPERATION_OK e DB_NOTFOUND il controllo viene restituito al chiamante senza lanciare eccezioni.
	 * Se il valore restituito è true significa DB_OPERATION_OK e se false significa DB_NOTFOUND.
     *
	 * 
	 * @param Object entityObjectClass	
	 * @param DataBaseStatusDetailed dbs
	 * @return boolean create row ok on database
	 * @throws ExceptionAmritaSqlAccessNotfound 
	 * @throws ExceptionAmritaSqlError 
	 * @throws ExceptionAmritaAnnotationMissing 
	 * @throws ExceptionAmritaReflectionError 
	 * @throws ExceptionAmrita 
	 */
	public boolean update(Object entityObjectClass)
						   throws ExceptionAmritaSqlError
						 		 ,ExceptionAmritaAnnotationMissing
						 		 ,ExceptionAmritaReflectionError
						 		 ,ExceptionAmrita
						 
						 {
		
		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_UPDATE);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
		// Recupero nome tabella sql e righe con mapping campi java da annotation 
		this.ar_ColumnMapping = getInfoMappingFromAnnotations(entityObjectClass);   // -> this.tableName
		dbs.setTableName(this.tableName);
	
		// Caricamento array descrittore campi per esecuzione dinamica
		arDataBaseItemDescriptor = new DataBaseItemDescriptor[this.ar_ColumnMapping.length];
		loadDataBaseItemDescriptor(entityObjectClass, arDataBaseItemDescriptor, this.ar_ColumnMapping, this.tableName);
		
		// Esecuzione DataBaseManager                                           
		dbm.crudUpdate(dbConn, dbs, this.tableName, arDataBaseItemDescriptor);
		
		// Esito operazione OK: Riga aggiornata nel database e nessun errore fisico 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			ManageAutoCommit();
			return true;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Riga non presente nel database o errore fisico Sql   //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante se errore Sql
		if (isSqlErrorVendor(dbs)) {
			excp = new ExceptionAmritaSqlError(dbs, null, dbs.getExcpOrigin());
		} 
		
		// Notfound
		return false;
	}
		

	/**
	 * Funzionalità CRUD delete generica su una Entity
	 * 
	 * I campi chiave della tabella sql associata all'entitity sono recuperati dalle annotazioni
	 * associate alla classe di gestione dell'Entity.
	 * Viene restituito un valore booleano e true significa riga entity deletata correttamente
	 * dalla tabella specificata attraverso l'oggetto di gestione Entity fornito..<br>
	 * Se viene restituito false sicuramente la riga non è stata deletata. 
	 * In questo caso sono disponibili i metodi del parametro DataBaseStatusDetailed dbs
	 * per ottenere informazioni sull'esito generale dell' operazione <b> getStatusOperation()</b>
	 * oppure di dettaglio come <b>getSqlErrorCode()</b>  <b>getSqlStatus()</b> etc.
	 * <p>
	 * Nel caso più normale l'esito dell'operazione potrà essere:<br>
	 * 	<b> DB_OPERATION_OK</b>  Se Operazione senza errori e riga trovata  <br>  			  
	 *  <b> DB_NOTFOUND </b>	 Se Operazione senza errori e riga non trovata  <br>
	 *  <b> DB_ERROR_SQL </b>	 Se Operazione con errori Sql<br>
	 * <p>
	 * Se DB_OPERATION_OK e DB_NOTFOUND il controllo viene restituito al chiamante senza lanciare eccezioni.
	 * Se il valore restituito è true significa DB_OPERATION_OK e se false significa DB_NOTFOUND.
	 * 
	 * 
	 * @param Object entityObjectClass	
	 * @param DataBaseStatusDetailed dbs
	 * @return boolean create row ok on database
	 * @throws ExceptionAmrita 
	 */
	public boolean delete(Object entityObjectClass) 
							throws ExceptionAmrita {

		dbs.setTypeOperation(EnumDataBaseOperation.DB_CRUD_DELETE);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		
		// Recupero nome tabella sql e righe con mapping campi java da annotation 
		this.ar_ColumnMapping = getInfoMappingFromAnnotations(entityObjectClass);   // -> this.tableName
		dbs.setTableName(this.tableName);

		// Caricamento array descrittore campi per esecuzione dinamica
		arDataBaseItemDescriptor = new DataBaseItemDescriptor[this.ar_ColumnMapping.length];
		loadDataBaseItemDescriptor(entityObjectClass, arDataBaseItemDescriptor, this.ar_ColumnMapping, this.tableName);
		
		// Esecuzione DataBaseManager                                           
		dbm.crudDelete(dbConn, dbs, this.tableName, arDataBaseItemDescriptor);
		
		// Esito operazione OK: Riga deletata dal database e nessun errore fisico 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			ManageAutoCommit();
			return true;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Riga non presente nel database o errore fisico Sql   //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante se errore sql
		if (isSqlErrorVendor(dbs)) {
			excp = new ExceptionAmritaSqlError(dbs, null, dbs.getExcpOrigin());
			throw excp;
		} 
		
		// Notfound
		return false;
	}
	

	/**
	 * Restituisce un array di oggetti applicativi Entity, mappato sulle righe del resultSet risultato
	 * della  Select sull'entity specifico, con la condizione di where specificata. <br>
	 * <p>
	 * L'array di oggetti Entity restituiti sono dello stesso tipo dell'oggetto Entity fornito in input,
	 * utilizzato per il mapping via annotation e reflection dei nomi delle colonne con i campi java della classe.
	 * <p>
	 * Operativamente l'applicazione utilizzerà {@link DataBaseManager} per instaurare una connessione con il db
	 * e per eseguire una generica istruzione Sql, con Select, Where o altro tramite il metodo <br>
	 * <b>execSqlGeneric(dbConn, sqlString, dbs)</b>.<br>
	 * Testando i valori dell'oggetto dbs si verifica l'esito generale dell'operazione.
	 * Se l'operazione è andata a buon fine viene restituito un array di oggetti applicativi Entity dove 
	 * ogni elemento rappresenta una riga formattata del resultSet generato..<br>
	 * Se il criterio di where non individua nessuna riga si restituisce una array vuoto.<br>
	 * Se ci sono stati errori viene lanciata una exception.
	 * <p>
	 * Esempio di utilizzo<br>
	 * <p>
	 *  Object ar_objRelation[] = null;
     *	EntityRelation entityRelation = null;
     *	String whereCondition = "";
     *	
     *  // Operazioni per accesso al databsae
     *	dbs = new DataBaseStatusDetailed();
     *	dbConn = dbm.getConnection(dbs);
     *	dbei = new DataBaseEntityInterface(sd, dbm, dbConn);	
     *	
     *	entityRelation = new EntityRelation();
     *	
     *       
     * 
     *	// Composizione Where di lettura EntityRelation
     *	whereCondition = "SELECT OBJTIDOB, OBJTLIBO, OBJTLIBS, OBJTFILS FROM OBJT"; 
     *	whereCondition = whereCondition + " WHERE OBJTSYST = '" + this.di.curSystemOwner + "'";
     *	whereCondition = whereCondition +	  " AND OBJTSUBS = '" + this.di.curSubSystemOwner + "'";
     *	whereCondition = whereCondition +     " AND  OBJTTYPO = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
     * 	
     *	ar_objRelation = dbei.readSetEntity(entityRelation, whereCondition);
     *	
     *	
     *	for (Object objRelation : ar_objRelation) {
     *		
     * 	}
     *
     *	dbei.commit();
     *	dbm.releaseConnection(dbConn, dbs);
	 * 
	 * 
	 * 
	 * @param Object entityObjectClass	
	 * @param String con condizione di where
	 * @param String con order by
	 * @return Object[] entityObject
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 * 
	 */
	public Object[] readSetEntity(Object entityObjectClass, String whereCondition, String orderBy) throws ExceptionAmrita, SQLException {
            
		dbs.setTypeOperation(EnumDataBaseOperation.DB_READ_SET_ENTITY);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
		ResultSet rs = null;
		Object[] ar_objectEntity = null;
		ArrayList<Object> al_objectEntity = null;
		Object objectEntity = null;
		String sqlString = "";
		
		al_objectEntity = new ArrayList<Object> ();
		
		// Recupero nome tabella sql e righe con mapping campi java da annotation 
		this.ar_ColumnMapping = getInfoMappingFromAnnotations(entityObjectClass);   // -> this.tableName
		dbs.setTableName(this.tableName);

		// Caricamento array descrittore campi per esecuzione dinamica
		this.arDataBaseItemDescriptor = new DataBaseItemDescriptor[this.ar_ColumnMapping.length];
		loadDataBaseItemDescriptor(entityObjectClass, this.arDataBaseItemDescriptor, this.ar_ColumnMapping, this.tableName);
		
		// Statemnt Select con Where condition order by
		sqlString = "SELECT * FROM ";
    	sqlString = sqlString + this.tableName + " ";
    	// Potrebbe non eseere stata specificata la where
    	if (!whereCondition.equals("")) {
          	sqlString = sqlString + " WHERE " + whereCondition;
		}
      	sqlString = sqlString + orderBy;
		
     	
		// Esecuzione DataBaseManager                                           
		rs = dbm.execSqlGeneric(dbConn, sqlString, dbs);

		// Esito operazione di lettura OK 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {

			// Scan record set e valorizzazione campi oggetto entity di riga
			while (rs.next()) {
				objectEntity = rm.newInstance(entityObjectClass.getClass(), null, null);	
				dbm.updateJavaFieldsValue(rs, arDataBaseItemDescriptor);                    // Update campi descrittore da resultset
				updateEntityObjectFields(objectEntity, dbs, arDataBaseItemDescriptor);      // Aggiornamento campi java entity da descrittore
				al_objectEntity.add(objectEntity);
			}
			ar_objectEntity = new Object[al_objectEntity.size()];
			ar_objectEntity = al_objectEntity.toArray(ar_objectEntity);
			
			return ar_objectEntity;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: errore fisico Sql                                    //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante se errore Sql
		if (isSqlErrorVendor(dbs) || dbs.getExcpOrigin() != null) {
			excp = new ExceptionAmritaSqlError(dbs, null, dbs.getExcpOrigin()); 
			throw excp;
		}
		
		ar_objectEntity = new Object[0];
		
		return ar_objectEntity;
	}
	



	
	/**
	 * Consolidamento aggiornamenti sul database.
	 * Viene lanciata la commit gestita da {@link DataBaseManager}
	 * 
	 * @throws ExceptionAmrita 
	 */
	public void  commit() throws ExceptionAmrita {		

		dbs.setTypeOperation(EnumDataBaseOperation.DB_COMMIT);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		// Esecuzione DataBaseManager                                           
		dbm.commit(this.dbConn, this.dbs);
		
		// Esito operazione OK: Riga deletata dal database e nessun errore fisico 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			return;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Aggiornasmenti non consolidati                       //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante
		excp = new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_SQL_COMMIT, dbs.getExcpOrigin());
		throw excp;
	}
	
	/**
	 * Controaggiornamneti effettuati sul database fino all'ultimo
	 * punto di Commit.
	 * 
	 * @throws ExceptionAmrita 
	 */
	public void  rollback() throws ExceptionAmrita {		

		dbs.setTypeOperation(EnumDataBaseOperation.DB_ROLLBACK);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		// Esecuzione DataBaseManager                                           
		dbm.rollback(this.dbConn, this.dbs, null);
		
		// Esito operazione OK: Riga deletata dal database e nessun errore fisico 
		if (dbs.getStatusOperation() == EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			return;
		}
		
		///////////////////////////////////////////////////////////////////////////////////
		// Esito operazione NOT OK: Controaggiornamenti                       			 //
		///////////////////////////////////////////////////////////////////////////////////
		
		// Logging già effettuato da DataBaseManager
		// Lancio Exception per il chiamante
		excp = new ExceptionAmritaSqlError(dbs, EnumAmritaExceptionError.ERROR_SQL_ROLLBACK, dbs.getExcpOrigin());
		throw excp;
	}

	/**
	 * Gets all <code>@DataBaseMappedTable</code> declared by the entity name.<br>
	 * <p>
	 * This annotation holds the name of the Sql table implementing the entity.<br>
	 * <p>
	 * If no annotation has been found, a null value will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the annotation or null
	 */
	public Annotation  getMappedTableAnnotation(Class<?> entityClass)  {	
		
		DataBaseMappedTable dbAnnoTable = null;			 													// Annotazione su  Entity... con nome tabella
		dbAnnoTable = (DataBaseMappedTable) rm.getAnnotation(entityClass, "analyzer.DataBaseMappedTable"); 	// Recupero annotazioni da oggetto Class per la classe Entity...
		if (rm.getException() != null) {
			logExceptionOnAnyError(rm.getException(), "EF0017", "DataBaseMappedTable", entityClass.getClass().getName());
			return null;
		}
		return dbAnnoTable;
	}
	
	/**
	 * Gets all <code>@DataBaseMappedColumns</code> declared by the entity name.<br>
	 * <p>
	 * If no annotation has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the annotation or null
	 */
	public Annotation  getMappedColumnsAnnotation(Class<?> entityClass) {		
		
		DataBaseMappedColumns dbAnnoColumn = null;       														// Annotazione su  Entity... con mapping campi java/db column
		dbAnnoColumn = (DataBaseMappedColumns) rm.getAnnotation(entityClass, "analyzer.DataBaseMappedColumns");
		if (rm.getException() != null) {
			logExceptionOnAnyError(rm.getException(), "EF0017", "DataBaseMappedColumns", entityClass.getClass().getName());
			return null;
		}
		return dbAnnoColumn;
	}
	

	
	/**
	 * Gets all <code>@DataBaseMappedRuleTable</code> declared by the entity name.<br>
	 * <p>
	 * They're annotations describing the access to rule & configuration tables,<br>
	 * managed by means of Forward generalized tables system.<br>
	 * If no annotation has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return an array of {@link DataBaseMappedRuleTable} annotation
	 */
	public DataBaseMappedRuleTable[]  getRuleTableAnnotations(Class<?> entityClass) {		
		DataBaseMappedRuleTables annotationRuleTables = null;
		DataBaseMappedRuleTable[] annotationsOut = null;
		ArrayList<DataBaseMappedRuleTable> al_annotations = null;
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationRuleTables = entityClass.getAnnotation(DataBaseMappedRuleTables.class);
		if (annotationRuleTables == null) {
			return new DataBaseMappedRuleTable[0];
		}
		
		al_annotations = new ArrayList<DataBaseMappedRuleTable>();
		
		// Scan array di annotazioni 
		for (DataBaseMappedRuleTable dataBaseMappedRuleTable : annotationRuleTables.tablesRule()) {
			al_annotations.add(dataBaseMappedRuleTable);
		}
		annotationsOut = new DataBaseMappedRuleTable[al_annotations.size()];
		annotationsOut = al_annotations.toArray(annotationsOut);
		return annotationsOut;
	}
	

	/**
	 * Gets the <code>DataBaseMappedRuleTable</code> declared by the entity name with the specified reference.<br>
	 * <p>
	 * If no annotation found with the input reference a null value will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @param ruleTableRef the reference to the declared rule table
	 * @return the {@link DataBaseMappedRuleTable} annotation or null
	 */
	public DataBaseMappedRuleTable  getRuleTableAnnotation(Class<?> entityClass, String ruleTableRef) {		
		DataBaseMappedRuleTables annotationRuleTables = null;
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationRuleTables = entityClass.getAnnotation(DataBaseMappedRuleTables.class);
		if (annotationRuleTables == null) {
			return null;
		}
		
		// Scan array di annotazioni 
		for (DataBaseMappedRuleTable dataBaseMappedRuleTable : annotationRuleTables.tablesRule()) {
			if (dataBaseMappedRuleTable.tabRef().equals(ruleTableRef)) {
				return dataBaseMappedRuleTable;
			}
		}
		return null;
	}
	
	/**
	 * Gets all entity names in FOR_ANY() relationship declared by the input entity name by annotation {@link DataBaseMappedForAnys}<br>
	 * <p>
	 * If no relationship has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the entity names list
	 */
	public String[]  getForAnyEntityNames(Class<?> entityClass) {		
		DataBaseMappedForAnys annotationForAnys = null;
		String[] ar_entityName = null;
		ArrayList<String> al_entityName = null;
		al_entityName = new ArrayList<String>();
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationForAnys = entityClass.getAnnotation(DataBaseMappedForAnys.class);
		if (annotationForAnys == null) {
			return new String[0];
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForAny dataBaseMappedForAny : annotationForAnys.forAny()) {
			al_entityName.add(dataBaseMappedForAny.entity());
		}
		
		ar_entityName = new String[al_entityName.size()];
		ar_entityName = al_entityName.toArray(ar_entityName);
		return ar_entityName;
	}


	/**
	 * Gets all entity names in FOR_EACH() relationship declared by the input entity name by annotation {@link DataBaseMappedForEachs}<br>
	 * <p>
	 * If no relationship has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the entity names list
	 */
	public String[]  getForEachEntityNames(Class<?> entityClass) {		
		DataBaseMappedForEachs annotationForEachs = null;
		String[] ar_entityName = null;
		ArrayList<String> al_entityName = null;
		al_entityName = new ArrayList<String>();
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle in forEach
		annotationForEachs = entityClass.getAnnotation(DataBaseMappedForEachs.class);
		if (annotationForEachs == null) {
			return new String[0];
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForEach dataBaseMappedForEach : annotationForEachs.forEach()) {
			al_entityName.add(dataBaseMappedForEach.entity());
		}
		
		ar_entityName = new String[al_entityName.size()];
		ar_entityName = al_entityName.toArray(ar_entityName);
		return ar_entityName;
	}


	
	/**
	 * Gets all <code>@DataBaseMappedForAny</code> declared by the entity name.<br>
	 * <p>
	 * If no annotation has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the data base item descriptor list
	 */
	public DataBaseMappedForAny[]  getForAnyAnnotations(Class<?> entityClass) {		
		DataBaseMappedForAnys annotationForAnys = null;
		DataBaseMappedForAny[] annotationsOut = null;
		ArrayList<DataBaseMappedForAny> al_annotations = null;
		al_annotations = new ArrayList<DataBaseMappedForAny>();
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationForAnys = entityClass.getAnnotation(DataBaseMappedForAnys.class);
		if (annotationForAnys == null) {
			return new DataBaseMappedForAny[0];
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForAny annotation : annotationForAnys.forAny()) {
			al_annotations.add(annotation);
		}
		
		annotationsOut = new DataBaseMappedForAny[al_annotations.size()];
		annotationsOut = al_annotations.toArray(annotationsOut);
		return annotationsOut;
	}
	
	/**
	 * Gets all <code>@DataBaseMappedForEach</code> declared by the entity name.<br>
	 * <p>
	 * If no annotation has been found, an empty array will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @return the data base item descriptor list
	 */
	public DataBaseMappedForEach[]  getForEachAnnotations(Class<?> entityClass) {		
		DataBaseMappedForEachs annotationForEachs = null;
		DataBaseMappedForEach[] annotationsOut = null;
		ArrayList<DataBaseMappedForEach> al_annotations = null;
		al_annotations = new ArrayList<DataBaseMappedForEach>();
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationForEachs = entityClass.getAnnotation(DataBaseMappedForEachs.class);
		if (annotationForEachs == null) {
			return new DataBaseMappedForEach[0];
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForEach annotation : annotationForEachs.forEach()) {
			al_annotations.add(annotation);
		}
		
		annotationsOut = new DataBaseMappedForEach[al_annotations.size()];
		annotationsOut = al_annotations.toArray(annotationsOut);
		return annotationsOut;
	}
	
	/**
	 * Gets the <code>@DataBaseMappedForAny</code> declared in the entityClass by the entity name.<br>
	 * <p>
	 * If no annotation has been found, a null value will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @param forAnyEntity the entity name of witch to get the annotation describing the relationship
	 * @return the {@link DataBaseMappedForAny} annotation or null
	 */
	public DataBaseMappedForAny getForAnyAnnotation(Class<?> entityClass, String forAnyEntity) {		
		DataBaseMappedForAnys annotationForAnys = null;
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle
		annotationForAnys = entityClass.getAnnotation(DataBaseMappedForAnys.class);
		if (annotationForAnys == null) {
			return null;
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForAny dataBaseMappedForAny : annotationForAnys.forAny()) {
			if (dataBaseMappedForAny.entity().equals(forAnyEntity)) {
				return dataBaseMappedForAny;
			}
		}
		return null;
	}

	/**
	 * Gets the <code>@DataBaseMappedForEach</code> declared in the entityClass by the entity name.<br>
	 * <p>
	 * If no annotation has been found, a null value will be returned.<br>
	 * <p>
	 * @param entityClass the class that manages the entity as a been
	 * @param forEachEntity the entity name of witch to get the annotation describing the relationship
	 * @return the {@link DataBaseMappedForEach} annotation or null
	 */
	public DataBaseMappedForEach getForEachAnnotation(Class<?> entityClass, String forEachEntity) {		
		DataBaseMappedForEachs annotationForEachs = null;
		
		// Recupero annotazione contenente tutte le annotazioni di tabelle in forEach
		annotationForEachs = entityClass.getAnnotation(DataBaseMappedForEachs.class);
		if (annotationForEachs == null) {
			return null;
		}

		// Scan array di annotazioni 
		for (DataBaseMappedForEach dataBaseMappedForEach : annotationForEachs.forEach()) {
			if (dataBaseMappedForEach.entity().equals(forEachEntity)) {
				return dataBaseMappedForEach;
			}
		}
		return null;
	}

	
	/**
	 * Gets a <code>DataBaseItemDescriptor</code> list for the entity primary key fields.<br>
	 * <p>
	 * @param @entityClass the class that manages the entity as a been
	 * @return the data base item descriptor list
	 */
	public DataBaseItemDescriptor[]  getDescriptorsPK(Object entityClassObject) {	
		DataBaseItemDescriptor[] ar_dataBaseItemDescriptor = null;
		ar_dataBaseItemDescriptor = getDescriptorsFields(entityClassObject, true, false);
		return ar_dataBaseItemDescriptor;
	}
	
	
	/**
	 * Gets a <code>DataBaseItemDescriptor</code> list for the entity not primary key fields.<br>
	 * <p>
	 * @param @entityClass the class that manage the entity as a been
	 * @return the data base item descriptor list
	 */
	public DataBaseItemDescriptor[]  getDescriptorsNotPK(Object entityClassObject) {	
		DataBaseItemDescriptor[] ar_dataBaseItemDescriptor = null;
		ar_dataBaseItemDescriptor = getDescriptorsFields(entityClassObject, false, true);
		return ar_dataBaseItemDescriptor;
	}
	
	
	/**
	 * Gets a <code>@DataBaseItemDescriptor</code> list for all entity fields.<br>
	 * <p>
	 * If no {@link DataBaseMappedTable} or no DataBaseMappedColumns has been found, an empty array will be returned.<br>
	 * <p>
	 * @param @entityClass the class that manages the entity as a been
	 * @return the data base item descriptor list
	 */
	public DataBaseItemDescriptor[]  getDescriptorsAllFields(Object entityClassObject) {	
		DataBaseItemDescriptor[] ar_dataBaseItemDescriptor = null;
		ar_dataBaseItemDescriptor = getDescriptorsFields(entityClassObject, true, true);
		return ar_dataBaseItemDescriptor;
	}
	
	

	
	

	/**
	 * @return the autoCommitBlockUpdates
	 */
	public boolean isAutoCommitBlockUpdates() {
		return autoCommitBlockUpdates;
	}

	/**
	 * @param autoCommitBlockUpdates the autoCommitBlockUpdates to set
	 */
	public void setAutoCommitBlockUpdates(boolean autoCommitBlockUpdates) {
		this.autoCommitBlockUpdates = autoCommitBlockUpdates;
	}


	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////  Metodi privati                                     /////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////	
	////////////////////////////////////////////////////////////////////////////////////////////





	/*
	 * Restituisce il descrittore con il nome della colonna o null se non trovato.
	 * Il nome della colonna deve essere uguale al nome di un campo java definito 
	 * e NON aal corrispondente nome db2
	 */
	@SuppressWarnings("unused")
	private DataBaseItemDescriptor searchColumnDescriptor(String columnName, DataBaseItemDescriptor[] ar_dataBaseItemDescriptor) {
		for (int i = 0; i < ar_dataBaseItemDescriptor.length; i++) {
			if (ar_dataBaseItemDescriptor[i].getJavaFieldName().equals(columnName)) {
				return ar_dataBaseItemDescriptor[i];
			}
		}
		return null;
	}


	
	/*
	 * 
	 * In caso di exception su una classe di servizio chiamata, viene gestita la segnalazione sul log 
	 * con il messaggio richiesto e con il printStackTrace effettuato sull'eccezione scatenante.
	 * L'exception fornita come parametro è quella originaria scatenante l'anomalia.
	 * 
	 */
	private void logExceptionOnAnyError(Exception excpOrigin
			                           ,String messageCode
			                           ,String ...errorPlaceHolders
			                           ) {
	    String arParm[] = null;
		
	    // Parametri da inserire nel messaggio di log
		if (errorPlaceHolders.length == 0) {
			arParm = null;
		} else {
			arParm = errorPlaceHolders;
		}
		
		// Log messaggio: viene prodotto il printStackTrace dall'eccezione origine scatenante l'anomalia
        lf.writeRow(EnumMessageType.ERROR_FATAL, messageCode, arParm, excpOrigin); 
	}
	

	
    /*
     * 
     * Recupero nome tabella e mapping campi java/colonne db
     * dalle annotazioni della classe di gestione dell'Entity
     * 
     */
	private String[] getInfoMappingFromAnnotations(Object entityObjectClass )  throws  ExceptionAmritaAnnotationMissing
			                                           							     , ExceptionAmritaReflectionError {
		
		// Campi estrazione e gestione annotation
		DataBaseMappedTable dbAnnoTable = null;			 // Annotazione su  Entity... con nome tabella
		DataBaseMappedColumns dbAnnoColumn = null;       // Annotazione su  Entity... con mapping campi java/db column
		
		// Recupero annotazioni da oggetto Class per la classe Entity...
		dbAnnoTable = (DataBaseMappedTable) rm.getAnnotation(entityObjectClass, "analyzer.DataBaseMappedTable");
		if (rm.getException() != null) {
			logExceptionOnAnyError(rm.getException(), "EF0017", "DataBaseMappedTable", entityObjectClass.getClass().getName());
			throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_GET_INFO, rm.getException());
		}
		
		dbAnnoColumn = (DataBaseMappedColumns) rm.getAnnotation(entityObjectClass, "analyzer.DataBaseMappedColumns");
		if (rm.getException() != null) {
			logExceptionOnAnyError(rm.getException(), "EF0017", "DataBaseMappedColumns", entityObjectClass.getClass().getName());
			throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_GET_INFO, rm.getException());
		}
		
		// Annotazioni obbligatorie non definite: logging e lancio exception
		if (dbAnnoTable == null) {
			logExceptionOnAnyError(null, "EF0010", "DataBaseMappedTable", entityObjectClass.getClass().getName());
			throw new ExceptionAmritaAnnotationMissing(null, EnumAmritaExceptionError.ERROR_ANNOTATION_ENTITY, null);
		}
		if (dbAnnoColumn == null) {
			logExceptionOnAnyError(null, "EF0010", "DataBaseMappedColumns", entityObjectClass.getClass().getName());
			throw new ExceptionAmritaAnnotationMissing(null, EnumAmritaExceptionError.ERROR_ANNOTATION_ENTITY, null);
		}
		
		// Recupero nome tabella/view db e colonne
		// Ogni colonna contiene: nome java, nome db, PK|NK
		this.tableName = dbAnnoTable.value();
		
		return dbAnnoColumn.java_db_col() ;
	}

	/*
	 * 
	 * Caricamento struttura descrittore campi entity java e colonne di database
	 * per esecuzione dinamica e generalizzata con DataBaseManager
	 * 
	 */
	private void loadDataBaseItemDescriptor(Object entityObjectClass
										  , DataBaseItemDescriptor[] arDataBaseItemDescriptor
										  , String[] ar_columnMapping
										  , String tableName
										   ) throws ExceptionAmritaReflectionError  {

		// Entity POJO name 
		dbs.setEntityName(entityObjectClass.getClass().getSimpleName());
				
		// Campi estrazione e gestione annotation
		DataBaseMappedEnumeration dbMappedEnumeration = null; // Annotazione su  Enumeration utilizzata come campo

		// Campi per caratteristiche campo/colonna di data base
		String javaField = "";
		Type javaFieldType = null;
		Class<?> javaFieldClass = null;
		String javaFieldClassName = "";
		String javaFieldClassNameSimple = "";
		String dataBaseColumn = "";
		boolean primaryKey = false;
		boolean isFieldEnumeration = false;

		// Campi di servizio per x connessione, reference, ...
		StringService ss = null;

		// Campi per utilizzo reflection su classi e annotazioni
		Field fieldReflection = null;
		Object objectReflection = null;
		String methodNameReflection = "";
        String txtFieldType = "";

		// Caricamento struttura per esecuzione DataBaseManager          
		for (int i = 0; i < ar_columnMapping.length; i++) {
			
			// estrazione valori da riga in annotation
			ss = new StringService(ar_columnMapping[i]);
			javaField = ss._word(1);					// javaFieldName
			dataBaseColumn = ss._word(2);				// dbColumn
			primaryKey = false;					    	// No primary key
			
			// Solo se presenti + di due valori, l'esecuzione potrebbe essere a fronte di ldv di forward
			if (ss._wordsCount() > 2) {
				if (ss._word(3).equals("PK")) {
					primaryKey = true;					// primaryKey
				}
			}
             
			// Recupero via reflection oggetto Field da oggetto Entity....
			fieldReflection = rm.getField(entityObjectClass, javaField);
			if (rm.getException() != null) {
				logExceptionOnAnyError(rm.getException(), "EF0011", javaField, entityObjectClass.getClass().getName());
				throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_GET_INFO, rm.getException());
			}
			
			// Tipo campo, nome classe e oggetto Class  
			javaFieldType =  fieldReflection.getGenericType();
			txtFieldType = javaFieldType.toString();
			isFieldEnumeration = false;
			javaFieldClass = getClassFromFieldType(txtFieldType);
			
			// Tipo campo non riconosciuto: errore interno oppure Entity con tipi campi di classe  non permessa
			if (javaFieldClass == null) {
				logExceptionOnAnyError(rm.getException(), "ET0019", entityObjectClass.getClass().getName(), javaField, txtFieldType) ;
				throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_DB_COLUMN_TYPE_DETECTING, null);
			}
			
			// Tipo campo riconosciuto
			if (txtFieldType.startsWith("class")) {
				javaFieldClassName = javaFieldClass.getName();
				javaFieldClassNameSimple = javaFieldClass.getSimpleName();

				// Recupero eventuale annotazione della classe di Enumeration, se campo di enumerazione
				dbMappedEnumeration = (DataBaseMappedEnumeration) rm.getAnnotation(javaFieldClass, "analyzer.DataBaseMappedEnumeration");
				if (rm.getException() != null) {
					logExceptionOnAnyError(rm.getException(), "EF0016", javaField, entityObjectClass.getClass().getName());
					throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_GET_INFO, rm.getException());
				}
				if (dbMappedEnumeration != null) {
					isFieldEnumeration = true;
				}
			} else {
				javaFieldClassName = txtFieldType;
				javaFieldClassNameSimple = txtFieldType;
			}
			
            
			// Invoke via reflection metodo getter getFieldname() per caricamento valore campo in struttura
			methodNameReflection = "get" + javaField.substring(0, 1).toUpperCase() + javaField.substring(1);
			objectReflection = rm.invokeMethod(entityObjectClass, methodNameReflection, null, null);
			if (rm.getException() != null) {
				logExceptionOnAnyError(rm.getException(), "EF0012", methodNameReflection, entityObjectClass.getClass().getName());
				throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, rm.getException());
			}
			
			arDataBaseItemDescriptor[i] = new DataBaseItemDescriptor();
		   
			// Valorizzo campi descrittore item anche con dati anche da reflection
			arDataBaseItemDescriptor[i].setDbTable(tableName);
			arDataBaseItemDescriptor[i].setDbColumn(dataBaseColumn);
			arDataBaseItemDescriptor[i].setJavaEnumeration(isFieldEnumeration);
			arDataBaseItemDescriptor[i].setJavaFieldType(javaFieldType);
			arDataBaseItemDescriptor[i].setJavaFieldName(javaField);
			arDataBaseItemDescriptor[i].setJavaFieldClass(javaFieldClass);
			arDataBaseItemDescriptor[i].setJavaFieldClassName(javaFieldClassName);
			arDataBaseItemDescriptor[i].setJavaFieldClassNameSimple(javaFieldClassNameSimple);
			arDataBaseItemDescriptor[i].setPrimaryKey(primaryKey);
			arDataBaseItemDescriptor[i].setJavaFieldValue(objectReflection);
		}
	}

	/*
	 * 
     * Aggiornamento campi riga tabella in oggetto Entity.. utilizzando i metodi setter standard
     * attivati con Invoke via reflection. Vengono valorizzati i campi java dell'Entity con i valori
     * java memorizzati nel descrittore, già caricati da database da DataBaseManager
	 * 
	 */
	private void updateEntityObjectFields(Object entityObjectClass
										 ,DataBaseStatusDetailed dbs
										 ,DataBaseItemDescriptor[] arDataBaseItemDescriptor
										 ) throws ExceptionAmritaReflectionError  {
		
		
		// Campi per caratteristiche campo/colonna di data base
        DataBaseItemDescriptor dbid = null;
		
		// Campi per utilizzo reflection su classi e annotazioni
		Object ar_ObjectInvoke[] = null;
		Class<?> ar_ObjectClassInvoke[] = null;
		String methodNameReflection = "";


		// Scan descrittore colonne 
		for (int i = 0; i < this.ar_ColumnMapping.length; i++) {
			
			dbid = arDataBaseItemDescriptor[i];
			
			// Imposto metodo setter
			methodNameReflection = "set" + dbid.getJavaFieldName().substring(0, 1).toUpperCase() + dbid.getJavaFieldName().substring(1);
			
			// Invoke via reflection metodo setter setFieldName(value)
			ar_ObjectInvoke = new Object[1];;
			ar_ObjectClassInvoke = new Class[1];
			ar_ObjectInvoke[0] = arDataBaseItemDescriptor[i].getJavaFieldValue();
			ar_ObjectClassInvoke[0] = dbid.getJavaFieldClass();
			
			rm.invokeMethod(entityObjectClass, methodNameReflection, ar_ObjectClassInvoke, ar_ObjectInvoke);
			if (rm.getException() != null) {
				logExceptionOnAnyError(rm.getException(), "EF0012", methodNameReflection, entityObjectClass.getClass().getName());
				throw new ExceptionAmritaReflectionError(null, EnumAmritaExceptionError.ERROR_REFLECTION_INVOKE, rm.getException());
			}
		}
	}


	/*
	 * 
	 * Restituisce true se l'error code è un errore Sql, in base al tipo
	 * di database utilizzato. Ogni dbms ha codici di ritorno diversi da zero
	 * per indicare una riga trovata a fronte di una select oppure una riga
	 * già presente a fronte di una Insert.
	 * Di default, viene restituito false se l'error code è zero,
	 * altrimenti true.
	 * 
	 */
	private boolean isSqlErrorVendor(DataBaseStatusDetailed dbs) {
		// 10
		if (dbs.getSqlErrorCode() == 0 || dbs.getSqlErrorCode() == 1062) {
			return false;
		} else {
			return true;
		}
	}

	/*
	 * 
	 * Gestione commit, nel caso sia stato superato il numero di istruzioni
	 * stabilite per il blocco di aggiornamenti
	 * 
	 */	
	private synchronized void ManageAutoCommit() throws ExceptionAmrita {
		
		// Solo se abilitato
		if (!isAutoCommitBlockUpdates()) {
			return;
		}
		
		cntUpdatedDone++;
		if (cntUpdatedDone >= dataBaseCommitBlockUpdates) {
			commit();
			cntUpdatedDone = 0;
		}
	}

    /*
     * restituisce un oggetto class a partire dal tipo di dato
     * 
     */
	private Class<?> getClassFromFieldType(String fieldTypeFull) {
       
		String fieldType = "";
        
       // Tipo campo oggetto: può essere solo String o enum
       if (fieldTypeFull.startsWith("class")) {
        	fieldType = fieldTypeFull.substring(6);
    		// Tipo campo oggetto String
    		if (fieldType.equals("java.lang.String") || fieldType.startsWith("enums.") ) {
    			return rm.getClass(fieldType);
    		}
        	return null;
        }
        
        fieldType = fieldTypeFull;
		
		// Tipi campi primitivi
		if (fieldType.equals("char")) {
			return Character.TYPE;
		}
		if (fieldType.equals("byte")) {
			return Byte.TYPE;
		}
		if (fieldType.equals("boolean")) {
			return Boolean.TYPE;
		}
		if (fieldType.equals("int")) {
			return Integer.TYPE;
		}
		if (fieldType.equals("long")) {
			return Long.TYPE;
		}
		if (fieldType.equals("double")) {
			return Double.TYPE;
		}
		if (fieldType.equals("float")) {
			return Float.TYPE;
		}

		return null;
	}

	
	/*
	 * Gets a <code>DataBaseItemDescriptor</code> list for the entity primary key fields.<br>
	 * <p>
	 * If no {@link DataBaseMappedTable} or no DataBaseMappedColumns has been found, an empty array will be returned.<br>
	 * <p>
	 * @param @entityClass the class that manages the entity as a been
	 * @return the data base item descriptor list
	 */
	private DataBaseItemDescriptor[]  getDescriptorsFields(Object entityClassObject, boolean isPkFieldToGet, boolean isNotPkFieldToGet) {	
		DataBaseItemDescriptor[] ar_dataBaseItemDescriptor = null;
		ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
		String[] ar_columnMapping = null;
		String tableName = "";
		
		// Recupero mapping colonne da annotation 
		try {
			ar_columnMapping = getInfoMappingFromAnnotations(entityClassObject);   								// -> this.tableName
			tableName = this.tableName;
			ar_dataBaseItemDescriptor = new DataBaseItemDescriptor[ar_columnMapping.length];  				// Caricamento array descrittore campi per esecuzione dinamica
			loadDataBaseItemDescriptor(entityClassObject, ar_dataBaseItemDescriptor, ar_columnMapping, tableName);//
		} catch (ExceptionAmritaAnnotationMissing e) {
			return new DataBaseItemDescriptor[0];
		} catch (ExceptionAmritaReflectionError e) {
			return new DataBaseItemDescriptor[0];
		}
			
		// Store descrittori soli campi chiave
		al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor>();
		for (DataBaseItemDescriptor dataBaseItemDescriptor : ar_dataBaseItemDescriptor) {
			if (dataBaseItemDescriptor.isPrimaryKey() && isPkFieldToGet) {
				al_dataBaseItemDescriptor.add(dataBaseItemDescriptor);
			}
			if (!dataBaseItemDescriptor.isPrimaryKey() && isNotPkFieldToGet) {
				al_dataBaseItemDescriptor.add(dataBaseItemDescriptor);
			}
		}
		
		// Conversione a array
		ar_dataBaseItemDescriptor = new DataBaseItemDescriptor[al_dataBaseItemDescriptor.size()];
		ar_dataBaseItemDescriptor = al_dataBaseItemDescriptor.toArray(ar_dataBaseItemDescriptor);
		return ar_dataBaseItemDescriptor;
	}

}


/*
			// Tipi campi primitivi
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

*/