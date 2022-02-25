package forward;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.Set;
import utilities.ReflectionManager;
import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseItemDescriptor;
import analyzer.DataBaseManager;
import analyzer.DataBaseMappedEnumeration;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedRuleTable;
import analyzer.DataBaseMappedTable;
import analyzer.DataBaseMetaTableColumn;
import analyzer.DataBaseStatusDetailed;
import analyzer.ExecutionDirectives;
import analyzer.LoggerFacade;
import analyzer.UserConfiguration;
import entities.EntityMetricValue;
import entities.EntityTableData;
import entities.EntityTableStructure;
import enums.EnumDataBaseJdbcSqlType;
import enums.EnumDataBaseOperation;
import enums.EnumDataBaseOperationStatus;
import enums.EnumDataItemGeneric;
import enums.EnumForwardLogicalDataViewControl;
import enums.EnumForwardScope;
import enums.EnumLanguage;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;


/**
 * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda   Turin (Itali)
 * 
 * <h1>
 * ForwardLogicalDataView
 * </h1>
 * Describe a logical data view to access data organized by entities in a concise, formal and general way<br>
 * independent by phisical data base and Sql language.<br>
 * <p>
 * <code>ForwardLogicalDataView</code> is based on the Amrita data access infrastructure, that allows to access to<br>
 * any database table through an object, a bean describing the entity like, for example, to {@link EntityMetric},<br>
 * just mapping phisical and database related informations, using plain methods like <code>readEntity()</code> and so on.<br>
 * Single entity fields values are managed by simple get and set methods.<br>
 * <p>
 * <code>ForwardLogicalDataView</code>, like <code>F.S.D.</code> (Function Specification & Documentation), allow you<br>
 * to declare data access through a declaring  model, against technical and procedural code.<br>
 * They are integrated in the same declaration, the access to database entities with the access to any<br>
 * non legacy rule table too. Rule and/or configuration tables are managed by means of the centralized forward table manager,<br>
 * phisically in the same sql table, with own columns virtually defined. <br>
 * Rule tables are used by application for rule controls or to show a description multilanguage<br>
 * Configuration tables hold typical non legacy application dependent configuration data, identified by a table code and a name (like a column).<br>
 * <p>
 * <code>ForwardLogicalDataView</code> has been designed to access to database data with three level of technology independendence,<br>
 * from the higher to lower level, that is a native Sql Select statement.<br>
 * For all access levels will be returned to the application a set of rows to get with a set of standard methods like<br>
 * <code>read(), readNext(), readPrev(), readFirst(), readLast().<br>
 * Phisically there will be not any overhead as all work is made directly on original resultset.<br>
 * When the row columns data are available, the application can get specific columns by getting the value of<br>
 * variables bound to any columns, by means of methods like <code>geValueString(), geValueInt(), geValueDouble(), geValueFloat(),
 * geValueDate(), geValueTime().<br>
 * As general design guidelines, Amrita Forward normally goes towards not to yield exceptions. Thus, when a value will be asked for<br>
 * an undefined column or with a wrong type, will be returnd an object, initialized according to the requested type, with no exeception.<br>
 * As further feature, it's possible to get all object entities and then to obtain current values by getter method of any column.<br>
 * In this modality are available methods like <code>getEntitiesNames(), getEntity()</code><br>.
 * <p>
 * At the first level you use only straightforward model declaring specifications <code>FOR_ANY</code> and <code>FOR_EACH</code> to get data. <br>
 * <code>FOR_ANY</code> declaration means to use a many to one or one to one relatiosnhip to get entity data. <br>
 * <code>FOR_EACH</code> declaration means to use a one to many to get entity data. <br>
 * When <code>FOR_ANY</code> is the first directive in the logical data view this does means to identify a specific row of the <br>
 * entity, identified by its primary key, from which to start the navigation of entities tree.<br>
 * Column values for the entity starting declared by first <code>FOR_ANY</code>, must be set by means of column variables, by setValue..() methods,
 * by application caller or directly in logical data view.<br>
 * <p>
 * Actually this means to follow all relationships between entitities defined by means of the relational database logical data model.<br>
 * You must be not concerned for about entities primary and foreign keys, matching of column names between entities and <br>
 * any technical issue, because all necessary informations have been declared by annotation at entity java class definition.<br>
 * <p>
 * At the second level, for particular application needs, it's also possible to add specific native Sql code to <br>
 * <code>FOR_ANY</code> and <code>FOR_EACH</code> declaration, to be considered in <code>AND</code> mode.<br>
 * For example your'e allowed a condition like <code>EXISTS (Select * FROM @entity WHERE @col = :varCol ... )</code>
 * Additional conditions and sql construcs can refers to variables to be solved runtime or to any other column declared too.<br>
 * Variables to be solved runtime must be identified by the name starting with a semicolon (:), like host sql variables.<br>
 * Columns names, entities names and correlation names must start with at sign symbol (@).<br>
 * Leading special characters are necessary for an easier validation of sql condition in the logical data view context.<br>
 * <code>validate()</br> method will verify, using the standard Amrita Assesment Sql analyzer, if the condition is well coded<br>
 * in the current logical data view context.<br>
 * <p>
 * At the third level you are allowed to specify a normal and completely free sql select statement.<br>
 * What's really important, from the logical data view, is to obtain a resultset where each column name has been assigned<br>
 * as a unique name. Then operations to get row column values are the same.<br>
 * <p>
 * With all of capabilities explained above, the logical data view meets the need of a full scalability specification of <br>
 * data access issues, from a pure model specification to a native sql statements.<br>
 * In any case is maintained the same interface towards the final application, that can be a normal java program or a <br>
 * Amrita forward application.
 * <br>
 * With first and second level, to use a <code>ForwardLogicalDataView</code> to access data, it's necessary to have defined <br>
 * before all entities classes, declaring mapping informations by {@link DataBaseMappedForAny}, {@link DataBaseMappedForEach} <br>
 * and {@link DataBaseMappedRuleTable} annotations.<br>
 * <code>ForwardLogicalDataView</code> starts from here to offer a way to declare a truly data access strategy at any level<br>
 * of deep and complexity, using just two factory directives:<p>
 * <ul>
 * <li><b>FOR_ANY()</b></li>
 * <li><b>FOR_EACH()</b></li>
 * </ul>
 * Here is the simpler example of declaration:<pre>
 * <b>FOR_ANY(new(EntitityMetric))</b>  
 *   <b>FOR_EACH(new(EntityMetricViolation) </b>
 *     <b>FOR_ANY("T001") </b>
 * </pre>
 * 
 * Here is a more complex example of declaration and the the sql query generated:<pre>
 *
 *<b>
 * FOR_ANY(EntitityMetric.class, "E1")
 *     FOR_ANY("T001", "T1")
 *     FOR_ANY("T045", "T2")
 *     FOR_ANY(EntitityMetricViolation.class, "E2")
 *     FOR_ANY(EntityObject.class, "E3")
 *     FOR_EACH(EntityRelation.class, "E4")
 *     	FOR_ANY("T001", "T3") 
 *
 *
 SELECT 
	E1.METRIDOB AS idObject, 
	T1.TBDTRDAT AS typeObject,
	E1.METRSCOP AS scope, 
	T2.TBDTRDAT AS scopeDescription,
	E3.OBJTLIBO AS library, 
	E2.METVORGN AS origin, 
	E4.RELAIDOB AS idObjectRelated,
	T3.TBDTRDAT AS typeObjectRelated
	FROM
	(((((METR AS E1 LEFT JOIN TBDT AS T1 ON     E1.METRT001 = T1.TBDTTTAB  AND  CSTR(E1.METRTYPO) = T1.TBDTKVAL)
	                LEFT JOIN TBDT AS T2 ON     E1.METRT045 = T2.TBDTTTAB  AND  CSTR(E1.METRSCOP) = T2.TBDTKVAL)                      
				    LEFT JOIN METV AS E2 ON     E1.METRSECT = E2.METVSECT
	                                       AND  E1.METRIDOB = E2.METVIDOB
	                                       AND  E1.METRTYPO = E2.METVTYPO
	                                       AND  E1.METRSUBS = E2.METVSUBS
	                                       AND  E1.METRSYST = E2.METVSYST)
	                LEFT JOIN OBJT AS E3  ON    E1.METRIDOB = E3.OBJTIDOB
	                                       AND  E1.METRTYPO = E3.OBJTTYPO
	                                       AND  E1.METRSUBS = E3.OBJTSUBS
	                                       AND  E1.METRSYST = E3.OBJTSYST) 
	                INNER JOIN RELA AS E4 ON    E1.METRIDOB = E4.RELAIDOA
	                                       AND  E1.METRSUBS = E4.RELASUBS
	                                       AND  E1.METRSYST = E4.RELASYST)
	                LEFT  JOIN TBDT AS T3 ON    E4.RELAT001 = T3.TBDTTTAB  AND  CSTR(E4.RELATYPB) = T3.TBDTKVAL;  
 * </b>
 * </pre>
 * 
 * You are noticed that the same rule table, in the example "T001", has been declared to be accessed in the same query,<br>
 * with different key values, obtained from different entities.<br>
 * <p>
 * 
 * Here is another example of declaration and the the sql query generated:
 * <pre>
 * <b>FOR_ANY(new(EntitityMetric))</b>  
 *     <b>FOR_ANY("T001") </b>
 *     <b>FOR_ANY("T041") </b>
 *  <b>
 *	 SELECT
		E1.METRSYST,
		E1.METRSUBS,
		E1.METRSCOP,
		E1.METRTYPO,
		E1.METRIDOB,
		E1.METRSECT,
		T1.TBDTRDAT AS DESC_PGM,
		T2.TBDTRDAT AS DESC_SCOPE
	FROM (METR AS E1 LEFT JOIN TBDT AS T1 ON E1.METRT001 = T1.TBDTTTAB  AND CSTR(E1.METRTYPO) = T1.TBDTKVAL)
                                      LEFT JOIN TBDT AS T2 ON E1.METRT041 = T2.TBDTTTAB
	WHERE T1.TBDTSUBS = '*'  
	</b>
 </pre>     
 * This <code>ForwardLogicalDataView</code> makes available to the application all fields of entity <code>EntitityMetric</code>,<br>
 * <code>EntitityMetricViolation</code> and rule table <code>T001</code>
 * <p>
 * <code>ForwardLogicalDataView</code> has so been designed to achieve the goals below:<br>
 * <ul>
 * <li><b>Declaration of strategy data access</b></li>
 * <li><b>Full integration with Amrita database access system mapping</b></li>
 * <li><b>Capability to be used out of Amrita and forward context</b></li>
 * <li><b>Scalability from model declaration to native sql statement</b></li>
 * <li><b>Use through both entity been objects get() and direct column variable querying</b></li>
 * <li><b>Full integration with forward function desktop and web variables system</b></li>
 * <li><b>Sql optimization with use of only really used columns</b></li>
 * <li><b>Availability of plain methods to do all operations</b></li>
 * </ul>
 * For any further information and example please refer to the documentation of any available method.
 * <br>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/march/2012 
 * @see ForwardFunction
 * @see ForwardMonitorDesktop
*/ 

public abstract class ForwardLogicalDataView {

	// Generali di servizio e di accesso ai dati
	private UserConfiguration sd = null;          					// Defaults e references globali come gestore messaggi, log etc	
    private LoggerFacade lf = null;            					// Gestore logging
    private DataBaseManager dbm = null;							// Gestore database
    private Connection dbConn = null;							// Connessione con il database	
    private DataBaseEntityInterface dbei = null;				// Interfaccia per operazioni CRUD su entities			     
    private DataBaseStatusDetailed dbs = null;					// Status esito operazione dettagliato
	private ReflectionManager rm = null;            			// Gestore generalizzato operazioni di reflection
	
	// Specifici di istanza
	private String name = "";									// Nome logical data view
	private ArrayList<InnerForAnyEach> al_forAnyEach = null;	// Sequenza di costrutti FOR_ANY e FOR_EACH, dichiarati dall'applicazione
	private ArrayList<InnerForAnyEach> al_forInsUpdDel = null;	// Sequenza di costrutti INSERT(), DELETE() e UPDATE, dichiarati dall'applicazione
	private Map<String, InnerVar> hm_var = null;				// Variabili utilizzate dalla logical data view e risolte runtime
    private boolean isRunningMonitor = false;               	// Logical data View istanziata dal forward Monitor
    private ForwardSystem forwardSystem = null;             	// L'oggetto ForwardSystem quando l'attivazione è effettuata dal monitor
    private String sqlSelectString = "";                    	// Select sql che implementa la logical data view
 	private int limitRows = 0;									// Limite numero righe da leggere, -1 = no limit
	private int pageRows = 0;									// Numero righe per pagina da restituire all'applicazione, -1 = no limit
	private int returnCode = 0;									// Return code
	private EnumLanguage ruleTableLanguage = null;				// Linguaggio da utilizzare per accedere alle ruleTable, defauly inglese
	private boolean isEntitiesAvailable = false;            	// True rende disponibili le singole entity che compongono la logical data view
	private boolean isReadSetRuleTable = false;            		// True indica logical data view di una sola for_each per accesso a rule table 
	private boolean isReadItemRuleTable = false;            	// True indica logical data view di una sola for_any per accesso a rule table 
	private boolean isValidateToDo = true;            			// True indica validate da eseguire. Non è possibile eseguire la logical data view.
	private boolean isForSql = false;            				// True indica che la logical data view è composta da una sola FOR_SQL() declarative
	private boolean isDbAutoCommit = true;                      // True indica commit automatica dopo ogni insert/update/delete
	private boolean isDbAutoConnection = true;               	// True indica connessione da acquisire/rilasciare a fronte di insert/delete/update
	
 	// Dichiarazione parametri richiesti/restituiti alla/dalla funzione se richiamata/richiama da altra funzione
	private ArrayList<String> al_parmRequired = null;			// Parametri richiesti dal chiamante (nome variabili)

    // Array di array con valori righe e colonne dati output di select
    private ArrayList<ArrayList<Object>> al_al_rowColumn = null;                            
    private Map<String, InnerAsColumn> hm_columnAsName = null; 	// Key = nome colonna (AS), data=idx entity owner e idx colonna in recordset + ..         
    
    // Working & state
    Class<?> curEntityClass = null;                         	// Classe entity corrente
    String curEntityName = "";									// Entity currently declared by FOR_ANY or FOR_EACH
    String strSqlOrderByColumn = "";  							// Stringa di order by accodata alla select
    String strSqlWhereCondition = "";  							// Where di completamento condizioni di JOIN, tipicamente per accesso a tabelle di dominio con aggiunta condizioni specifiche applicative
    String[] ar_orderByColumn = null;  							// Order by in formato sql
    int curRowNumber = 0;										// Numero corrente riga
    int cntColumnExposed = 0;									// Counter corrente colonne view
    boolean isErrorFound = false;								// Error: no operation
    
	/**
	 * Creates a {@link ForwardLogicalDataView} object
	 */
	public ForwardLogicalDataView() {
		super();
		ruleTableLanguage = EnumLanguage.ENGLISH;
		name = this.getClass().getSimpleName();
		al_forAnyEach = new ArrayList<InnerForAnyEach> ();
		al_forInsUpdDel = new ArrayList<InnerForAnyEach> ();
		hm_var = new HashMap<String, InnerVar>();
		hm_columnAsName = new HashMap<String, InnerAsColumn>();
		al_al_rowColumn = new ArrayList<ArrayList<Object>> ();
		ar_orderByColumn = new String[0];
		al_parmRequired = new ArrayList<String> ();
		initial();
	}
	
	/**
	 * Creates a {@link ForwardLogicalDataView} object with the database manager set.<br>
	 * <p>
	 */
	public ForwardLogicalDataView(DataBaseManager dataBaseManager) {
		super();
		this.dbm = dataBaseManager;
		al_forAnyEach = new ArrayList<InnerForAnyEach>();
		this.hm_var = new HashMap<String, InnerVar>();
		initial();
	}
	
	/**
	 * <h3>Run the declare logical data view specification</h3>
	 * <br>
	 * This method is intended for exclusive internal use by forward monitor, for a correct exception management<br>
	 * <p>
	 */
	public  int runDeclare(ExecutionDirectives di) { 
		
		try {
			declare();
			return this.returnCode;
			
		} catch (Exception e) {
			di.excpOccurred = e;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_EXCEPTION.ordinal();
		}
	}

	
	/**
	 * <h3>Declare logical data view specification</h3>
	 * <br>
	 * <code>declare()</code> contains calls declaring logical data view  methods.<br>
	 * <br>
	 * Applications function classes must extends {@link ForwardLogicalDataView}, an abstract class that define<br>
	 * just the abstract method <code>declare()</code>.<br>
	 * Forward monitor calls <code>declare()</code> in the initialization process to build logical data view structures.<br>
	 * <p>
	 * The declare() method must be filled by user  with the only declaratives:<br>
	 *  FOR_ANY(), FOR_EACH(), FOR_SQL(), INSERT(), UPDATE() and DELETE()<br>
	 * <p>
	 * After the execution of the declare() method the logical data view is not still executable.<br>
	 * The validate() method mustr be called to make the logical data view executable.<br>
	 * <p>
	 */
	public abstract void declare();

	
	/**
	 * Gets system defaults.<br>
	 * <p>
	 * @return the sd
	 */
	public UserConfiguration getSd() {
		return sd;
	}

	/**
	 * Sets system defaults.<br>
	 * <p>
	 * @param sd the sd to set
	 */
	public void setSd(UserConfiguration sd) {
		this.sd = sd;
	}

	/**
	 * @return the lf
	 */
	public LoggerFacade getLf() {
		return lf;
	}

	/**
	 * @param lf the lf to set
	 */
	public void setLf(LoggerFacade lf) {
		this.lf = lf;
	}

	/**
	 * Gets the data base manager.<br>
	 * <p>
	 * That's the low level data base access object.<br>
	 * <p>
	 * @return the dbm
	 */
	public DataBaseManager getDbm() {
		return dbm;
	}

	/**
	 * Sets the data base manager.<br>
	 * <p>
	 * That's the low level data base access object.<br>
	 * <p>
	 * @param dbm the dbm to set
	 */
	public void setDbm(DataBaseManager dbm) {
		this.dbm = dbm;
	}

	/**
	 * Gets a new data base connection used to access to data base.<br>
	 * <p>
	 * @return the dbConn
	 */
	public int getDbConnection() {
		dbs = new DataBaseStatusDetailed();
		try {
			dbConn = dbm.getConnection(dbs);
		} catch (ExceptionAmritaSqlError e) {
			// TODO Auto-generated catch block
			EnumForwardLogicalDataViewControl.LDV_ERROR_DB_CONNECTION.ordinal();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			EnumForwardLogicalDataViewControl.LDV_ERROR_DB_CONNECTION.ordinal();
		}
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}

	/**
	 * Release the current data base connection used to access to data base.<br>
	 * <p>
	 * @return the dbConn
	 */
	public int releaseDbConnection() {
		dbm.releaseConnection(dbConn, dbs);
		dbConn = null;
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
  
	/**
	 * Sets the data base connection used to access to data base.<br>
	 * <p>
	 * @param dbConn the dbConn to set
	 */
	public void setDbConnection(Connection dbConn) {
		this.dbConn = dbConn;
	}

	
	/**
	 * Commits updates on the current data base connection.<br>
	 * <p>
	 * @return the logical data view return code
	 */
	public int dbCommit() {
		dbs.setTypeOperation(EnumDataBaseOperation.DB_COMMIT);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		// Esecuzione DataBaseManager                                           
		dbm.commit(this.dbConn, this.dbs);
		
		// Esito operazione NOT OK: Aggiornasmenti non consolidati        
		if (dbs.getStatusOperation() != EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_COMMIT.ordinal();
		}
		
		// Esito operazione NOT OK: aggiornamenti non consolidati                       
		
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
 
	
	/**
	 * Rollbacks updates on the current data base connection.<br>
	 * <p>
	 * @return the logical data view return code
	 */
	public int dbRollback() {
		
		dbs.setTypeOperation(EnumDataBaseOperation.DB_COMMIT);
		dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);

		// Esecuzione DataBaseManager                                           
		dbm.rollback(this.dbConn, this.dbs);
		
		// Esito operazione NOT OK: Aggiornasmenti non consolidati        
		if (dbs.getStatusOperation() != EnumDataBaseOperationStatus.DB_OPERATION_OK) {
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ROLLBACK.ordinal();
		}
		
		// Esito operazione NOT OK: aggiornamenti non consolidati                       
		
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
 
	
	/**
	 * Gets the data base entity interface object to access to data base entities.<br>
	 * <p>
	 * That's the intermediate level data base access to an entity, by CRUD modality.<br>
	 * <p>
	 * @return the dbei
	 */
	public DataBaseEntityInterface getDbei() {
		return dbei;
	}

	/**
	 * Sets the data base entity interface object to access to data base entities.<br>
	 * <p>
	 * That's the intermediate level data base access to an entity, by CRUD modality.<br>
	 * <p>
	 * @param dbei the dbei to set
	 */
	public void setDbei(DataBaseEntityInterface dbei) {
		this.dbei = dbei;
	}

	/**
	 * Gets the data base status detailed of the last data base operation.<br>
	 * <p>
	 * Any failed database access operation doesn't cause an exception but an appropriate<br>
	 * return code will be returned to the application with setting of a {@link DataBaseStatusDetailed}<br>
	 * object too. The application can then query this object to obtain nicely all informations it needs.<br>
	 * <p>
	 * @return the dbs
	 */
	public DataBaseStatusDetailed getDbs() {
		return dbs;
	}

	/**
	 * Sets the data base status detailed of the last data base operation.<br>
	 * <p>
	 * Any failed database access operation doesn't cause an exception but an appropriate<br>
	 * return code will be returned to the application with setting of a {@link DataBaseStatusDetailed}<br>
	 * object too. The application can then query this object to obtain nicely all informations it needs.<br>
	 * <p>
	 * @param dbs the dbs to set
	 */
	public void setDbs(DataBaseStatusDetailed dbs) {
		this.dbs = dbs;
	}

	/**
	 * Gets the {@link ForwardSystem} object active.<br>
	 * <p>
	 * When the caller application is not the Forward monitor, a null value will be returned.<br>
	 * <p>
	 * @return the forwardSystem
	 */
	public ForwardSystem getForwardSystem() {
		return forwardSystem;
	}

	/**
	 * Sets the {@link ForwardSystem} object active.<br>
	 * <p>
	 * This method is intended for only internal use<br>
	 * <p>
	 * @param forwardSystem the forwardSystem to set
	 */
	public void setForwardSystem(ForwardSystem forwardSystem) {
		this.forwardSystem = forwardSystem;
	}

	/**
	 * Sets the sql select string representing the sequence of FOR_ANY() and/or FOR_EACH() of the logical data view <br>
	 * <p>
	 * That's an sqk select that codes a set of LEFT OUTER JOIN.<br>
	 * <p>
	 * @param sqlSelectString the sqlSelectString to set
	 */
	public void setSqlSelectString(String sqlSelectString) {
		this.sqlSelectString = sqlSelectString;
	}

	/**
	 * Return the name of the logical data view.<br>
	 * <p>
	 * That's the simple name of this class, with no packages path.<br>
	 * <p>
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets if is running the forward monitor, to say that the caller is not a stand alone java application.<br>
	 * <p>
	 * @return the isRunningMonitor
	 */
	public boolean isRunningMonitor() {
		return isRunningMonitor;
	}

	/**
	 * Sets if is running the forward monitor, to say that the caller is not a stand alone java application.<br>
	 * <p>
	 * @param isRunningMonitor the isRunningMonitor to set
	 */
	public void setRunningMonitor(boolean isRunningMonitor) {
		this.isRunningMonitor = isRunningMonitor;
	}

	/**
	 * Gets if single entity object beans are available for the application caller.<br>
	 * <p>
	 * Logical data view is elementary data driven, to say that application referes only
	 * to single fields, regardless from which entity the column field comes from.<br>
	 * <p>
	 * A true value means that when logical data view row data are made available, for<br>
	 * each entity will be created and populated the bean object <br>
	 * Because may be tha not all entity fields are to be managed, some fields could be set<br>
	 * to the type default value.<br>
	 * <p>
	 * @return the isEntitiesAvailable
	 */
	public boolean isEntitiesBeanAvailable() {
		return isEntitiesAvailable;
	}

	/**
	 * Sets if single entity object beans are available for the application caller.<br>
	 * <p>
	 * Logical data view is elementary data driven, to say that application referes only
	 * to single fields, regardless from which entity the column field comes from.<br>
	 * <p>
	 * A true value means that when logical data view row data are made available, for<br>
	 * each entity will be created and populated the bean object <br>
	 * Because may be tha not all entity fields are to be managed, some fields could be set<br>
	 * to the type default value.<br>
	 * <p>
	 * @param isEntitiesAvailable the isEntitiesAvailable to set
	 */
	public void setEntitiesAvailable(boolean isEntitiesAvailable) {
		this.isEntitiesAvailable = isEntitiesAvailable;
	}

	/**
	 * Gets if the logical data view is of generic access to a rule table.<br>
	 * <p>
	 * This type of logical data view declare just one FOR_EACH() for a rule table.<br>
	 * <p>
	 * @return the isReadSetRuleTable
	 */
	public boolean isReadSetRuleTable() {
		return isReadSetRuleTable;
	}

	/**
	 * Sets if the logical data view is of generic access to a rule table.<br>
	 * <p>
	 * This type of logical data view declare just one FOR_EACH() for a rule table.<br>
	 * <p>
	 * @param isReadSetRuleTable the isReadSetRuleTable to set
	 */
	public void setReadSetRuleTable(boolean isReadSetRuleTable) {
		this.isReadSetRuleTable = isReadSetRuleTable;
	}

	/**
	 * Gets if the logical data view is of generic access to a rule table.<br>
	 * <p>
	 * This type of logical data view declare just one FOR_ANY() for a rule table.<br>
	 * <p>
	 * @return the isReadSetRuleTable
	 */
	public boolean isReadItemRuleTable() {
		return isReadItemRuleTable;
	}

	/**
	 * Sets if the logical data view is of generic access to a rule table.<br>
	 * <p>
	 * This type of logical data view declare just one FOR_ANY() for a rule table.<br>
	 * <p>
	 * @param isReadItemRuleTable the isReadItemRuleTable to set
	 */
	public void setReadItenRuleTable(boolean isReadItemRuleTable) {
		this.isReadItemRuleTable = isReadItemRuleTable;
	}

	
	/**
	 * Gets if the validate() method must be executed.<br>
	 * <p>
	 * The logical data view can be executed only after a succesfully validation.<br>
	 * The validation is executed normally after the creation and before the execution.<br>
	 * However the execution of methods like <code>setPrimaryKeyToExclude()</code> <br>
	 * and <code>setColumnsToExpose()</code> invalidate the logical data view, that can't<br>
	 * be executed until a <code>validate()</code> will be succesfully executed.<br>
	 * <p>
	 * @return the isValidateToDo
	 */
	public boolean isValidateToDo() {
		return isValidateToDo;
	}

	/**
	 * Sets if the validate() method must be executed.<br>
	 * <p>
	 * The logical data view can be executed only after a succesfully validation.<br>
	 * The validation is executed normally after the creation and before the execution.<br>
	 * However the execution of methods like <code>setPrimaryKeyToExclude()</code> <br>
	 * and <code>setColumnsToExpose()</code> invalidate the logical data view, that can't<br>
	 * be executed until a <code>validate()</code> will be succesfully executed.<br>
	 * <p>
	 * @param isValidateToDo the isValidateToDo to set
	 */
	public void setValidateToDo(boolean isValidateToDo) {
		this.isValidateToDo = isValidateToDo;
	}

	
	/**
	 * Gets if the logical data view is a <code>FOR_SQL()</code> type, completely user coded.<br>
	 * <p>
	 * When the logical data view ia a <code>FOR_SQL()</code>, all internal descriptors are loaded<br>
	 * at the first execution time, getting informations directly from metadata, available with jdbc.<br>
	 * So are retrieved, for each column, the name of the column exposed by means of the AS clause,
	 * the database column name and type, the database table name and the internal name used.<br>
	 * <br>
	 * To make the sql manual coding effective, you must always code all of columns you are interested in, <br>
	 * because no descriptor is available with java logical names.<br>
	 * When a normal <code>FOR_ANY()</code> and <code>FOR_EACH()</code> is used, the class implementing the<br>
	 * bean for the entity, describes, with the annotations, all informations here queried runtime to the database.<br>
	 * @return the isForSql
	 */
	public boolean isForSql() {
		return isForSql;
	}

	/**
	 * Sets if the logical data view is a <code>FOR_SQL()</code> type, completely user coded.<br>
	 * <p>
	 * When the logical data view ia a <code>FOR_SQL()</code>, all internal descriptors are loaded<br>
	 * at the first execution time, getting informations directly from metadata, available with jdbc.<br>
	 * So are retrieved, for each column, the name of the column exposed by means of the AS clause,
	 * the database column name and type, the database table name and the internal name used.<br>
	 * <br>
	 * To make the sql manual coding effective, you must always code all of columns you are interested in, <br>
	 * because no descriptor is available with java logical names.<br>
	 * When a normal <code>FOR_ANY()</code> and <code>FOR_EACH()</code> is used, the class implementing the<br>
	 * bean for the entity, describes, with the annotations, all informations here queried runtime to the database.<br>
	 * @param isForSql the isForSql to set
	 */
	public void setForSql(boolean isForSql) {
		this.isForSql = isForSql;
	}

	
	
	/**
	 * Gets if an automatic commit has to be executed after any insert, delete or update.<br>
	 * <p>
	 * When no automatic commit is effective, the open and close data base connection must be <br>
	 * executed explicitely by getConnection() and releaseConnection() methods.
	 * <br>
	 * The default is automatic commit to be executed.
	 * <p>
	 * @return the isDbAutoCommit
	 */
	public boolean isDbAutoCommit() {
		return this.isDbAutoCommit;
	}

	/**
	 * Sets if an automatic commit has to be executed after any insert, delete or update.<br>
	 * <p>
	 * When no automatic commit is effective, the open and close data base connection must be <br>
	 * executed explicitely by getConnection() and releaseConnection() methods.
	 * <br>
	 * The default is automatic commit to be executed.
	 * <p>
	 * @param isDbAutoCommit the isDbAutoCommit to set
	 */
	public void setDbAutoCommit(boolean isDbAutoCommit) {
		this.isDbAutoCommit = isDbAutoCommit;
	}

	/**
	 * Get if the connection to the data base must be acquired and then released autonmatically.<br>
	 * <p>
	 * The default value is true. When a false value is set the application must execute explicitely<br>
	 * the getConnection() before and releeaseConnnection() after.<br>
	 * <p>
	 * @return the isDbAutoConnection
	 */
	public boolean isDbAutoConnection() {
		return isDbAutoConnection;
	}

	/**
	 * Sets if the connection to the data base must be acquired and then released autonmatically.<br>
	 * <p>
	 * The default value is true. When a false value is set the application must execute explicitely<br>
	 * the getConnection() before and releeaseConnnection() after.<br>
	 * <p>
	 * @param isDbAutoConnection the isDbAutoConnection to set
	 */
	public void setDbAutoConnection(boolean isDbAutoConnection) {
		this.isDbAutoConnection = isDbAutoConnection;
	}

	/**
	 * Gets the limit row property.<br>
	 * <p>
	 * That's the max number of rows that can be read from database.<br>
	 * <p>
	 * @return the limitRows
	 */
	public int getLimitRows() {
		return limitRows;
	}

	/**
	 * Sets the limit row property.<br>
	 * <p>
	 * That's the max number of rows that can be read from database.<br>
	 * <p>
	 * @param limitRows the limitRows to set
	 */
	public void setLimitRows(int limitRows) {
		this.limitRows = limitRows;
	}

	/**
	 * Gets the number of rows in a page.<br>
	 * <p>
	 * @return the pageRows
	 */
	public int getPageRows() {
		return pageRows;
	}

	/**
	 * Sets the number of rows in a page.<br>
	 * <p>
	 * @param pageRows the pageRows to set
	 */
	public void setPageRows(int pageRows) {
		this.pageRows = pageRows;
	}


	/**
	 * Gets the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}<br>
	 * <p>
	 * @return the returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * Sets the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}<br>
	 * <p>
	 * @param returnCode the returnCode to set
	 */
	public void setReturnCode(int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * Gets the language to be used to access to rule tables.<br>
	 * <p>
	 * The language is a key field in the forward generalized tables system and<br>
	 * must be set in the sql select created using FOR_ANY() and FOR_EACH() declaration.<br>
	 * <p>
	 * The default value is <code>ENGLISH</code> but the application function can set a new<br>
	 * language before the logical data view execution.<br>
	 * If a specified rule table is not in the data base in a specific language,<br>
	 * the logical data view will return initialized columns instead.<br>
	 * <p>
	 * @return the ruleTableanguage
	 */
	public EnumLanguage getRuleTableLanguage() {
		return ruleTableLanguage;
	}

	/**
	 * Sets the language to be used to access to rule tabòes.<br>
	 * <p>
	 * The language is a key field in the forward generalized tables system and<br>
	 * must be set in the sql select created using FOR_ANY() and FOR_EACH() declaration.<br>
	 * <p>
	 * The default value is <code>ENGLISH</code> but the application function can set a new<br>
	 * language before the logical data view execution.<br>
	 * If a specified rule table is not in the data base in a specific language,<br>
	 * the logical data view will return initialized columns instead.<br>
	 * <p>
	 * @param ruleTableLanguage the ruleTableLanguage to set
	 */
	public void setRuleTableLanguage(EnumLanguage ruleTableLanguage) {
		this.ruleTableLanguage = ruleTableLanguage;
	}

	/**
	 * Gets if any error occurred calling the logical data view methods<br>
	 * <p>
	 * @return the isErrorFound
	 */
	public boolean isErrorFound() {
		return isErrorFound;
	}

	/**
	 * sets if any error occurred calling the logical data view methods<br>
	 * <p>
	 * @param isErrorFound the isErrorFound to set
	 */
	public void setErrorFound(boolean isErrorFound) {
		this.isErrorFound = isErrorFound;
	}

	/**
	 * Creates explicitely a variable in the logical data view linked to a column exposed.<br>
	 * <p>
	 * Normally all variables are automatically created for each column exposed by logical data view<br>
	 * and can so be set directly by application caller and are mainly primary ke columns.<br>
	 * For documentation purposes it's possible explicitely to declare column variable.<br>
	 * <p>
	 * If a variable with the same name already exists, it will be replaced.<br>
	 * The variables logical data view system has been designed to share variables values<br>
	 * used runtime, like host variables and variables automatically created for any logical<br>
	 * data view data column.<br>
	 * <p>
	 * Variables used runtime like host variables, are used to solve dynamic expressions <br>
	 * in <code>FOR_ANY</code><br> and <code>FOR_EACH</code>. A substitution of names starting <br>
	 * with : (semicolon) with the current variable value, will be done in the sql query <br>
	 * statement building process.<br>
	 * This type of variable should be created by a PARM_REQUIRED() declaration.<br>
	 * <p>
	 * Variables for each data column returned by logical data view are automatically created too.<br>
	 * Always automatically data column variables are set when data rows are available by<br>
	 * read(), readNext(), readPrev(), readFirst(), readLast() methods.<br>
	 * <br>
	 * Variables are set by forward monitor to let the automatic application data access<br>
	 * and can be set by any normal java application using a logical data view too.<br>
	 * Convenient methods to set variables values are available.<br>
	 * <p>
	 * @see isVarDeclared()
	 * @see setValue()
	 * @see setValueString()
	 * @see setValueBoolean()
	 * @see setValueInt()
	 * @see setValueDouble()
	 * @see setValueFloat()
	 * @see setValueDate()
	 * 
	 */
	public void VAR(String varName, Object valueInitial) {
		InnerVar innerVar;
		innerVar = new InnerVar();
		innerVar.varName = varName;
		innerVar.varType = valueInitial.getClass();
		innerVar.varScope = EnumForwardScope.NOT_ASSIGNED;
		innerVar.varInitial = valueInitial;
		innerVar.varObject = valueInitial;
		innerVar.varObjectPrev = valueInitial;
		this.hm_var.put(varName, innerVar);
		return;
	}


	/**
	 * Declare an entity data related many to one to a previous declared entity with minimal specification.<br>
	 * <p>
	 * When a <code>FOR_ANY</code> declaration is the first, all starting entity <br>
	 * primary key values are obtained by variables automatically set for primary <br>
	 * key columns.<br> All of these variables values must be set externally,<br>
	 * by forward monitor or by a java caller application.<br>
	 * <P>
	 * 
	 * @param entityClass the the class that declare the entity
	 */
	public void FOR_ANY(Class<?> entityClass) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_ANY, "", null, true, null, new String[]{}, "");
	}
	
	/**
	 * Declare an entity data related many to one to a previous declared entity.<br>
	 * <p>
	 * When a <code>FOR_ANY</code> declaration is the first, all starting entity <br>
	 * primary key values are obtained by variables automatically set for primary <br>
	 * key columns.<br> All of these variables values must be set externally,<br>
	 * by forward monitor or by a java caller application.<br>
	 * <br>
	 * 
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated
	 */
	public void FOR_ANY(Class<?> entityClass, String entityNameAs, String whereSql) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_ANY, entityNameAs, null, true, null, new String[]{}, whereSql);
	}
	
	/**
	 * Declare an entity data related many to one to a previous declared entity with full options.<br>
	 * <p>
	 * When a <code>FOR_ANY</code> declaration is the first, all starting entity <br>
	 * primary key values are obtained by variables automatically set for primary <br>
	 * key columns.<br> All of these variables values must be set externally,<br>
	 * by forward monitor or by a java caller application.<br>
	 * <P>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated. May be an empty string.
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by entity and the second with the name to be returned to the application.<br>
	 * @param primaryKeysExclude as a string array of primary key column names. When the FOR_ANY() is the first declared by<br>
	 * logical data view the columns to exclude will be not included in the the where condition of the sql select generated<br>
	 * A null value means no exclusion to do. A no null value can be coded to solve specific needs or with a not completely<br>
	 * relational database design.
	 * @param areIncludedAllFields a boolean true means all columns included getting entity data, false only those declared in input
	 * @param fieldsToIncludeExclude a string array of column names (as java field names). When all columns are included lists the column names to exclude<br>
	 *                     whereas not all columns are included lists the columns names to include<br>
	 *                     The string array should be not empty when colsIncludedExcluded is set to false.<br>
	 */
	public void FOR_ANY(Class<?> entityClass, String entityNameAs, String whereSql, String[] asClauses, String[] primaryKeysExclude, boolean areIncludedAllFields, String ... fieldsToIncludeExclude) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_ANY, entityNameAs, primaryKeysExclude, areIncludedAllFields, fieldsToIncludeExclude,  (asClauses == null) ? new String[]{} : asClauses, whereSql);
	}
	    
	  
	/** 
	 * Declare a rule table entity data related many to one to a previous declared entity with minimal specification.<br>
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of previous FOR_ANY() and FOR_EACH() declaration, or by externally variables setting.<br>
	 * Normally in this kind of access it's enaugh to get the table code and the table key field. <br>
	 * Rule tables are classified by a numeric sequence whereas the key element is a string value. <br>
	 * When the same rule tabe is used more then once by an entity, it's necessary to specify explicitely<br>
	 * key values names to use.<br>
	 * <br>
	 * 
	 * @param ruleTableNum the number of the rule table in the forward generalyzed tables system
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param relatedKeysName a list of names of fields, declared by previous FOR_ANY() and FOR_EACH() to be used to complete the rule table key.<br>
	 * They must be specified when the same rule table is used by the same entity with different key values.<br>
	 * When a rule table is used once by a table no key name must be specified. 
	 */
	public void FOR_ANY(int ruleTableNum, String entityNameAs, String ... relatedKeysName) {
		InnerForAnyEach innerForAnyEach = null;
		
		if (this.getReturnCode() > 0) {return;}
		innerForAnyEach = forAnyEachSqlCommon(EntityTableData.class, EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE, entityNameAs, new String[]{"keySeq"}, true, null, new String[]{}, "");
		innerForAnyEach.ruleTableNum = ruleTableNum;
		innerForAnyEach.ruleTableId = ruleTableNum + "";
		if (relatedKeysName != null && relatedKeysName.length >= 1) {
			innerForAnyEach.ruleTableRelatedKeysName = relatedKeysName;
		} else {
			innerForAnyEach.ruleTableRelatedKeysName = new String[0];
		}
	}

	
	/**
	 * Declare a rule table entity data related one to one or many to one to a previous declared entity.<br>
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of previous FOR_ANY() and FOR_EACH() declaration, or by externally variables setting.<br>
	 * Normally in this kind of access it's enaugh, to get data, the table code and the table key field. <br>
	 * Rule tables are classified by a numeric key sequence whereas the key element is a string value. <br>
	 * Just only one key field, apart the number of table and the language identifies a table entry.<br>
	 * When the same rule tabe is used more then once by an entity, it's necessary to specify explicitely<br>
	 * key values names to use.<br>
	 * <p>
	 * 
	 * @param ruleTableNum the number of the rule table in the forward generalyzed tables system
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by rule table and the second with the name to be returned to the application.<br>
	 * @param relatedKeysName a list of names of fields, declared by previous FOR_ANY() and FOR_EACH() to be used to complete the rule table key.<br>
	 * They must be specified when the same rule table is used by the same entity with different key values.<br>
	 * When a rule table is used once by a table no key name must be specified. 
	 */
	public void FOR_ANY(int ruleTableNum, String entityNameAs, String whereSql, String[] asClauses, String ... relatedKeysName) {
		InnerForAnyEach innerForAnyEach = null;
		
		if (this.getReturnCode() > 0) {return;}
		innerForAnyEach = forAnyEachSqlCommon(EntityTableData.class, EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE, entityNameAs, new String[]{"keySeq"}, false, new String[]{"rowData"}, (asClauses == null) ? new String[]{} : asClauses, whereSql);

		innerForAnyEach.ruleTableNum = ruleTableNum;
		innerForAnyEach.ruleTableId = ruleTableNum + "";
		if (relatedKeysName != null && relatedKeysName.length >= 1) {
			innerForAnyEach.ruleTableRelatedKeysName = relatedKeysName;
		} else {
			innerForAnyEach.ruleTableRelatedKeysName = new String[0];
		}
	}

	/**
	 * Declare a generic rule table entity data not related to any previous declared entity, with minimal specification.<br>
	 * <p>
	 * Because this declaration doesn't contain the rule table number, it can be used in a shared generalized logical data view to access<br>
     * to any rule table, only to get a description of the item key or to control the existence of a specific item key.<br>
	 * <p>
	 * All of necessary key fields to access to the rule table, must be set before the execution by means of application, <br>
	 * by externally variables setting.<br>
	 * Kes fields to be set externally are <code>numTable</code>, <code>keyVal</code> and <code>language</code>.<br>
	 * Key fiels <code>system</code> and <code>subSystem</code> must be set directly by the logical data to the <code>'*'</code> value.<br>
	 * <br>
	 * Application function must so set <b>numTable, keyVal, language</b> column name directly in a reusable application method by means of:<pre>
	 *   setValue("numTable", intValue) 
	 *   setValue("keyVal", stringValue) 
	 *   setValue("language", EnumLanguage.ENGLISH|ITALIAN)</pre>
	 *   
	 * or at declaration level by <b>actions</b>:<pre>
	 *   DO_LDV_SET_FIELD("LdvName", "numTable", intValue)
	 *   DO_LDV_SET_FIELD("LdvName", "keyVal", stringValue)
	 *   DO_LDV_SET_LANGUAGE("LdvObjects", EnumLanguage.ENGLISH|ITALIAN)</pre>
	 * <p>
	 * @param ruleTableNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by rule table and the second with the name to be returned to the application.<br>
	 */
	public void FOR_ANY(String ruleTableNameAs, String[] asClauses) {
		InnerForAnyEach innerForAnyEach = null;
        String whereSql = "";
		
		if (this.getReturnCode() > 0) {return;}
		innerForAnyEach = forAnyEachSqlCommon(EntityTableData.class, EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE, ruleTableNameAs, new String[]{"keySeq"}, false, new String[]{"keyVal", "rowData"}, (asClauses == null) ? new String[]{} : asClauses, whereSql);
		innerForAnyEach.ruleTableNum = -1;						// Impostato esternamente al popolamento
		innerForAnyEach.ruleTableRelatedKeysName = new String[0];
	}


	/**
	 * Declare a specific rule table entity data not related to any previous declared entity.<br>
	 * <p>
	 * This declaration identifies a set of rows with the rule table content.
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of application, by externally variables setting.<br>
	 * In this kind of access it's enaugh to set the table code, because all of others keys, such as system, subsystem and language, <br>
	 * are automatically set with current value, but any key value can be overrided too.<br>
	 * The key element of returned rows, will be different for each row and identifies the rule table row element.<br>
	 * Rule tables are classified by a numeric sequence whereas the key element is a string value. <br>
	 * <br>
	 * Because this declaration doesn't contain the rule table number, it can be used as a shared logical data view to acces<br>
     * to all rule tables used only to get a description of the rule, to say exposing just only a description column.<br>
     * <p>
	 * Application function must so set <b>numTable</b> column name to the table number desired:<pre>
	 *   setValue("numTable", intValue)</pre>
	 * <p>
	 * @param ruleTableNum the number of the rule table in the forward generalyzed tables system
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by rule table and the second with the name to be returned to the application.<br>
	 */
	public void FOR_EACH(int ruleTableNum, String entityNameAs, String whereSql, String[] asClauses) {
		InnerForAnyEach innerForAnyEach = null;

		if (this.getReturnCode() > 0) {return;}
		innerForAnyEach = forAnyEachSqlCommon(EntityTableData.class, EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE, entityNameAs, new String[]{"keySeq", "keyVal"}, false, new String[]{"keyVal", "rowData"}, (asClauses == null) ? new String[]{} : asClauses, whereSql);
		innerForAnyEach.ruleTableNum = ruleTableNum;
		innerForAnyEach.ruleTableRelatedKeysName = new String[0];
	}

	/**
	 * Declare a generic rule table entity data not related to any previous declared entity, with minimal specification.<br>
	 * <p>
	 * This declaration identifies a set of rows with the rule table content and the rule table number must be set externally.<br>
	 *
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of application, by externally variables setting.<br>
	 * In this kind of access it's enaugh to set the table code, directly in the population action, because all of others keys, <br>
	 * such as system, subsystem and language, are automatically set with current value, but any key value can be overrided too.<br>
	 * The key element of returned rows, will be different for each row and identifies the rule table row element.<br>
	 * Rule tables are classified by a numeric sequence whereas the key element is a string value. <br>
	 * <br>
	 * Because this declaration doesn't contain the rule table number, it can be used as a shared logical data view to acces<br>
     * to all rule tables used only to get a description of the rule, to say exposing just only a description column.<br>
     * <p>
	 * Application function must so set <b>numTable</b> column name to the table number desired:<pre>
	 *   setValue("numTable", intValue)</pre>
	 * <p>
	 * 
	 * @param ruleTableNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by rule table and the second with the name to be returned to the application.<br>
	 */
	public void FOR_EACH(String ruleTableNameAs, String whereSql, String[] asClauses) {
		InnerForAnyEach innerForAnyEach = null;

		if (this.getReturnCode() > 0) {return;}
		innerForAnyEach = forAnyEachSqlCommon(EntityTableData.class, EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE, ruleTableNameAs, new String[]{"keySeq", "keyVal"}, false, new String[]{"keySeq", "keyVal", "rowData"}, (asClauses == null) ? new String[]{} : asClauses, whereSql);
		innerForAnyEach.ruleTableNum = -1;						// Impostato esternamente al popolamento
		innerForAnyEach.ruleTableRelatedKeysName = new String[0];
	}

	  
	
	/**
	 * Declare an entity data related one (a previous declared entity) to many with minimal specification.<br>
	 * <p>
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of previous FOR_ANY() and FOR_EACH() declaration, or by externally variables setting.<br>
	 * All informations to match all relationship fields must be specified in the entity class annotations.<br>
	 * <p>
	 * @param entityClass the the class that declare the entity
	 */
	public void FOR_EACH(Class<?> entityClass) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_EACH,  "", null, true, null, new String[]{}, "");
	}

	/**
	 * Declare an entity data related one (a previous declared entity) to many.<br>
	 * <p>
	 * <p>
	 * All of necessary key fields necessary to access to the rule table, should be been defined<br>
	 * by means of previous FOR_ANY() and FOR_EACH() declaration, or by externally variables setting.<br>
	 * All informations to match all relationship fields must be specified in the entity class annotations.<br>
	 * <p>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated
	 */
	public void FOR_EACH(Class<?> entityClass, String entityNameAs, String whereSql) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_EACH, entityNameAs, null, true, null, new String[]{}, whereSql);
	}

	/**
	 * Declare an entity data related one to many to a previous declared entity.<br>
	 * <p>
	 * When a <code>FOR_ANY</code> declaration is the first, all starting entity <br>
	 * primary key values are obtained by variables automatically set for primary <br>
	 * key columns.<br> All of these variables values must be set externally,<br>
	 * by forward monitor or by a java caller application.<br>
	 * <P>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated. May be an empty string.
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by entity and the second with the name to be returned to the application.<br>
	 * @param areIncludedAllFields a boolean true means all columns included getting entity data, false only those declared in input
	 * @param fieldsToIncludeExclude a string array of column names (as java field names). When all columns are included lists the column names to exclude<br>
	 *                     whereas not all columns are included lists the columns names to include<br>
	 *                     The string array should be not empty when areIncludedAllFields is set to false.<br>
	 */
	public void FOR_EACH(Class<?> entityClass, String entityNameAs, String whereSql, String[] asClauses, boolean areIncludedAllFields, String ... fieldsToIncludeExclude) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_EACH, entityNameAs, null, areIncludedAllFields, fieldsToIncludeExclude, (asClauses == null) ? new String[]{} : asClauses, whereSql);
	}
	
	 
	/**
	 * Declare an entity data related one to many to a previous declared entity.<br>
	 * <p>
	 * When a <code>FOR_EACH</code> declaration is the first, all starting entity <br>
	 * primary key values are obtained by variables automatically set for primary <br>
	 * key columns.<br> All of these variables values must be set externally,<br>
	 * by forward monitor or by a java caller application.<br>
	 * <P>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param whereSql the sql condition added in AND in the sql statement generated. May be an empty string.
	 * @param asClauses as a String array where each string element contains two tokens, the first with the <br>
	 *          java column name declared by entity and the second with the name to be returned to the application.<br>
	 * @param primaryKeysExclude as a string array of primary key column names. When the FOR_EACH() is the first declared by<br>
	 * logical data view the columns to exclude will be not included in the the where condition of the sql select generated<br>
	 * A null value means no exclusion to do.
	 * @param areIncludedAllFields a boolean true means all columns included getting entity data, false only those declared in input
	 * @param fieldsToIncludeExclude a string array of column names (as java field names). When all columns are included lists the column names to exclude<br>
	 *                     whereas not all columns are included lists the columns names to include<br>
	 *                     The string array should be not empty when colsIncludedExcluded is set to false.<br>
	 */
	public void FOR_EACH(Class<?> entityClass, String entityNameAs, String whereSql, String[] asClauses, String[] primaryKeysExclude, boolean areIncludedAllFields, String ... fieldsToIncludeExclude) {
		if (this.getReturnCode() > 0) {return;}
		forAnyEachSqlCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_FOR_EACH, entityNameAs, primaryKeysExclude,  areIncludedAllFields, fieldsToIncludeExclude, (asClauses == null) ? new String[]{} : asClauses, whereSql);
	}
	 
	/**
	 * Declare the order by criterias to be applyied to the sql select generated by means of the logical data view.<br>
	 * <p>
	 * The <code>ORDER_BY()</code> can be declared in any point and must be coded just one time.<br>
	 * Further ordering declaration will be ignored.<br>
	 * <p>
	 * Ordering informations must be coded as a string array where each element in sequence is the column to be ordered followed,
	 * like sql notation, by nothing or <code>ASC</code> for ascending ordering or by <code>DESC</code>, for descending ordering.<br>
	 * <P>
	 * Notice the the column name is not the database column but the java name coded by the entity bean class for the entity,<br>
	 * mapped on the column db.<br>
	 * 
	 * @param orderByColumns the string array with columns to be ordered, in sequence
	 */
	public void ORDER_BY(String ... orderByColumns) {
		this.ar_orderByColumn = orderByColumns;
		return;
	}

	/**
	 * Declare a parameter required to the logical data view execution<br>
	 * <p>
	 * Declare a parameter name required for the correct running of the function<br>
	 * when the function is started by another function.<br>
	 * <p>
	 * A parameter name is the name assigned to a fuction variable declared with the same name.<br> 
	 * The variable will be automatically created as it was explicitely created by VAR() declaration.<br>
	 * <br>
	 * A parameter could be, for example a customer code, to be assigned to a GUI control as default,<br>
	 * or to access data using a logical data view.<br>
	 * <p>
	 * @param parmNameRequired 
	 * @param parmInitial the parameter object with initial value
	 */
	public void PARM_REQUIRED(String parmNameRequired, Object parmInitial) {
		VAR(parmNameRequired, parmInitial);
		this.al_parmRequired.add(parmNameRequired);
		return;
	}

	/**
	 * Gets all parameters names declared as required to the logical data view to be replaced as <code>HostVar</code> starting by : (semicolon).
	 * <br>
	 * @return the parameters required
	 */
	public ArrayList<String> getParmNamesRequired() {
		return this.al_parmRequired;
	}

	/**
	 * Declare the updated of an entity allowed.<br>
	 * <p>
	 * When a <CODE>UPDATE()</code> is declared the <code>update()</code> method will cause the update of the entity specified.<br>
	 * Primary key values to identify the row to update are obtained by variables, automatically set for primary <br>
	 * key columns, currently active.<br> All of these variables values can be set externally, by forward monitor, <br>
	 * by a java caller application or be the values of the current row.<br>
	 * <P>
	 * To prevent undesiderable updates of entity columns, its possible specify a names list with just only the columns to be updated.<br>
	 * All others entity columns of logical data view, that can be showed by user interface, will be not updated.<br>
	 * As normal approach a UI controls, when declared with the same name of a column in the logical data view, will cause<br>
	 * the update of the database column.br>
	 * In any case, if the entity to be updatabele has been declared in any previous FOR_ANY() or FROR_EACH(), only exposed<br>
	 * column will be updatable.<br>
	 * <p>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 * @param areUpdatableAllFields as true if all entity fields will be updatable with any eventually exclusion by fieldsToBeIncludedExcludedAsUpd array
	 * @param fieldsToBeIncludedExcludedAsUpd as a string array of column names (as java field names) to be updated 
	 *                  with current variables values or to be excluded from update. <br> 
	 *                  When areUpdatableAllFields is true, that's the list of columns to exclude from update.<br>
	 *                  When areUpdatableAllFields is false, that's the list of columns to be considered updatable at all.<br>
	 * @param ar_asClause as a variable string array of javaColumnName AS newName clause. New name lets to use a java variable name specific for an entity column
	 */
	public void UPDATE(Class<?> entityClass, String entityNameAs, boolean areUpdatableAllFields, String[] fieldsToBeIncludedExcludedAsUpd, String ... ar_asClause) {
		
		InnerForAnyEach forUpdInsDel = null;
		ArrayList<String> al_includedExcludedField = null;
		
		if (this.getReturnCode() > 0) {return;}

		al_includedExcludedField = new ArrayList<String> ();
		if (fieldsToBeIncludedExcludedAsUpd != null) {
			for (String fieldToBeIncluded : fieldsToBeIncludedExcludedAsUpd) {
				al_includedExcludedField.add(fieldToBeIncluded);
			}
		}
		forUpdInsDel = updInsDelCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_UPDATE, entityNameAs, "", areUpdatableAllFields, al_includedExcludedField, ar_asClause);
		
		forUpdInsDel.areUpdatableAllFields = areUpdatableAllFields;
		
		// Append values
		if (fieldsToBeIncludedExcludedAsUpd != null) {
			for (String fieldToBeIncludedExcludedAsUpd : fieldsToBeIncludedExcludedAsUpd) {
				forUpdInsDel.al_includedExcludedFieldUpdatable.add(fieldToBeIncludedExcludedAsUpd);
			}
		}
	}
	 
	
	/**
	 * Declare the delete of an entity allowed.<br>
	 * <p>
	 * When a <CODE>DELETE()</code> is declared the <code>delete()</code> method will cause the delete of the entity specified.<br>
	 * Primary key values to identify the row to delete are obtained by variables, automatically set for primary <br>
	 * key columns, currently active.<br> All of these variables values can be set externally, by forward monitor, <br>
	 * by a java caller application or be the values of the current row.<br>
	 * <P>
	 * If the entity has beeen not declared, no action will be taken.<br>
	 * <p>
	 * @param entityClass the the class that declare the entity
	 * @param entityNameAs the correlation name used in the sql statement by the AS clause or empty string
	 */
	public void DELETE(Class<?> entityClass, String entityNameAs) {
		
		ArrayList<String> al_includedExcludedField = null;
		String[] ar_asClause = null;
		String whereSql = "";
		boolean areIncludedAllFields = true;
		
		if (this.getReturnCode() > 0) {return;}

		al_includedExcludedField = new ArrayList<String> ();
		ar_asClause = new String[0];
		
		updInsDelCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_DELETE, entityNameAs, whereSql, areIncludedAllFields, al_includedExcludedField, ar_asClause);
	}
	

	/**
	 * Declare the delete of an entity allowed.<br>
	 * <p>
	 * When a <CODE>DELETE()</code> is declared the <code>delete()</code> method will cause the delete of the entity specified.<br>
	 * Primary key values to identify the row to delete are obtained by variables, automatically set for primary <br>
	 * key columns, currently active.<br> All of these variables values can be set externally, by forward monitor, <br>
	 * by a java caller application or be the values of the current row.<br>
	 * <P>
	 * If the entity has beeen not declared, no action will be taken.<br>
	 * <p>
	 * @param entityClass the class that declare the entity
	 * @param entityNameAs the table correlation name to refer entity columns in the where clause
	 * @param whereSql the sql where clause to be added to primary key constraints conditions
	 */
	public void DELETE(Class<?> entityClass, String entityNameAs, String whereSql) {
		
		ArrayList<String> al_includedExcludedField = null;
		String[] ar_asClause = null;
		boolean areIncludedAllFields = true;
		
		if (this.getReturnCode() > 0) {return;}

		al_includedExcludedField = new ArrayList<String> ();
		ar_asClause = new String[0];
		
		updInsDelCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_DELETE, entityNameAs, whereSql, areIncludedAllFields, al_includedExcludedField, ar_asClause);
	}
	

	/**
	 * Declare the insert of an entity allowed.<br>
	 * <p>
	 * When a <CODE>INSERT()</code> is declared the <code>insert()</code> method will cause the insert of the entity specified.<br>
	 * Primary key values to identify the row to insert are obtained by variables, automatically set for primary <br>
	 * key columns, currently active. All others field will be obtained in the same way too.<br>
	 * All of these variables values can be set externally, by forward monitor, <br>
	 * by a java caller application or be the values of the current row.<br>
	 * <P>
	 * If the entity has beeen not declared, no action will be taken.<br>
	 * <p>
	 * @param entityClass the class that declare the entity
	 * @param entityNameAs the table correlation name to refer entity columns in the where clause
	 */
	public void INSERT(Class<?> entityClass, String entityNameAs) {
		
		ArrayList<String> al_includedExcludedField = null;
		String[] ar_asClause = null;
		String whereSql = "";
		boolean areIncludedAllFields = true;
		
		if (this.getReturnCode() > 0) {return;}

		al_includedExcludedField = new ArrayList<String> ();
		ar_asClause = new String[0];
		
		updInsDelCommon(entityClass, EnumForwardLogicalDataViewControl.LDV_INSERT, entityNameAs, whereSql, areIncludedAllFields, al_includedExcludedField, ar_asClause);
	}
	 

	/*
	 * Parte common UPDATE(), INSERT(), DELETE()
	 * 
	 * Viene caricato il descrittore dell'entity a partire dalle annotation.
	 *
	 */
	private InnerForAnyEach updInsDelCommon(Class<?> entityClass
										  , EnumForwardLogicalDataViewControl typeEntry
										  , String entityNameAs
										  , String whereSql
										  , boolean areIncludedAllFields
										  , ArrayList<String> al_includedExcludedField
										  , String[] ar_asClause
										   ) {
		
		DataBaseMappedTable dbTableAnnotation = null;			// Annotations con nome sql tabella
		InnerForAnyEach innerUpdInsDel = null;					// Strutture interne di servizio
		Object entityClassObject = null;
		String varName = "";
		
		// Istanziazione oggetto entity di modello
		entityClassObject = this.rm.newInstance(entityClass, null, null);
		
		// New entry per UPDATE(), INSERT() e DELETE()
		innerUpdInsDel = new InnerForAnyEach();
		this.al_forInsUpdDel.add(innerUpdInsDel);
		if (entityClass != null) {innerUpdInsDel.entityName = entityClass.getSimpleName();}
		innerUpdInsDel.entityObject = entityClassObject;
		innerUpdInsDel.indexEntry = this.al_forInsUpdDel.size() - 1;
		innerUpdInsDel.typeEntry = typeEntry;
		innerUpdInsDel.entityNameAs = entityNameAs;
		innerUpdInsDel.whereSql = whereSql;
	    dbTableAnnotation = entityClass.getAnnotation(DataBaseMappedTable.class);
	    if (dbTableAnnotation != null) {innerUpdInsDel.entityNameDb = dbTableAnnotation.value();}
		
		// Info descrittive complete campi chiave dell'entity in UPDATE(), INSERT() o DELETE()
	    innerUpdInsDel.ar_primarKey = dbei.getDescriptorsPK(entityClassObject);
	    innerUpdInsDel.ar_notPrimarKey = dbei.getDescriptorsNotPK(entityClassObject);

		// Nomi colonne di entity da esporre e caricamento struttura generale di controllo colonne di istanza
		populateEntityColumnsAs(innerUpdInsDel, ar_asClause, this.al_forInsUpdDel.size() - 1);

		// Creazione variabili di colonna per tutti i campi primary key
		for (DataBaseItemDescriptor descriptorColumn : innerUpdInsDel.ar_primarKey) {
			varName = descriptorColumn.getAsName();
			if (varName.equals("")) {
				varName = descriptorColumn.getJavaFieldName();
			}
			if (isVarDeclared(varName)) {continue;} 						// Variabile creata da probabile FOR_ANY() o FOR_EACH() precedente: nessuna operazione
			createVar(descriptorColumn);							     	// Creazione variabile nella logical data view					
		}
  
		// In ottica CRUD i campi NON key non interessano per DELETE()
		if (typeEntry == EnumForwardLogicalDataViewControl.LDV_DELETE) {
			return innerUpdInsDel;
		}
		
		// Creazione variabili di colonna per tutti i campi NOT primary key
		for (DataBaseItemDescriptor descriptorColumn : innerUpdInsDel.ar_notPrimarKey) {
			varName = descriptorColumn.getAsName();
			if (varName.equals("")) {
				varName = descriptorColumn.getJavaFieldName();
			}
			if (isVarDeclared(varName)) {continue;} 						// Variabile creata da probabile FOR_ANY() o FOR_EACH() precedente: nessuna operazione
			createVar(descriptorColumn);							     	// Creazione variabile nella logical data view					
		}
		
		return innerUpdInsDel;
	}
		


	/**
	 * Declare a native sql select statement to realize the logical data view.<br>
	 * <p>
	 * This is the lower level to specify the data access without entities bean classes<br>
	 * describing a sql table, using thus directly names defined in sql DDL.<br>
	 * The sql select can be written with no constaints.<br>
	 * <p>
	 * Host variables, substituted runtime before the execution, must be coded as <br>
	 * host variables starting with a : (semicolon). Such a host variables must be <br>
	 * declared by <code>VAR()</code> declarations and must be set externally, by  forward <br>
	 * monitor or by java application caller. <bt>
	 * <p>
	 * At the first execution, for each column declared by sql select statent, will be <br>
	 * created a variable to host row column values. If a column AS sql clause has been declared<br>
	 * the variable name will be that specified in the AS clause.<br>
	 * <p>
	 * When data rows are available you will be allowed to navigate with the same methods
	 * used by logical data view created by means of FOR_ANY() and FOR_EACH().<br>
	 * Application caller can get all current row column values always calling the methods<br>
	 * getValueString(), getValueBoolean(), getValueInt(), getValueDouble(), getValueFloat(), <br>
	 * getValueDate() <br>
	 * <p>
	 * 
	 * @param selectSql the complete sql select statement string
	 */
	public void FOR_SQL(String selectSql) {
		InnerForAnyEach innerForAnyEach = null;
		innerForAnyEach = forAnyEachSqlCommon(null, EnumForwardLogicalDataViewControl.LDV_FOR_SQL, "", new String[0], true, new String[0], new String[0],  "");
		innerForAnyEach.selectSql = selectSql;
		this.setForSql(true);
	}

	/*
	 * Parte common FOR_ANY() e FOR_EACH() e FOR_SQL()
	 */
	private InnerForAnyEach forAnyEachSqlCommon(Class<?> entityClass, EnumForwardLogicalDataViewControl typeEntry, String entityNameAs, String ar_primaryKeyExclude[], boolean areIncludedAllFields, String ar_fieldsToIncludeExclude[], String[] ar_asClause, String whereSql) {
		
		Scanner scn = null;
		Object entityClassObject = null;
		Object entityClassForAnyObject = null;
		Object entityClassForEachObject = null;
		
		// Annotations
		DataBaseMappedTable dbTableAnnotation = null;
		DataBaseMappedForAny forAnyAnnotation = null;
//		DataBaseMappedForEach forEachAnnotation = null;
		DataBaseMappedRuleTable[] ar_usableRuleTable = null;
		
		// Descrittori campi
		DataBaseItemDescriptor[] ar_descriptorFieldForAny = null;
		DataBaseItemDescriptor columnDescriptor = null;
		DataBaseItemDescriptor columnRelatedDescriptor = null;
		
		// Coppie di descrittori colonne
		ArrayList<ForwardColumnPair> al_forwardColumnsPair = null;
		ForwardColumnPair[] ar_forwardColumnsPair = null;
		ForwardColumnPair forwardColumnsPair = null;
		
		// Strutture interne di servizio
		InnerForAnyEach innerForAnyEach = null;
		InnerUsableRuleTable innerUsableRuleTable = null;
		
		// Classi bean 
		Class<?> classForAny = null;
		Class<?> classForEach = null;
		
		// Nomi entries e colonne
		String[] ar_entityNames = null;
		String columnName = "";
		String columnRelatedName = "";
		String varName = "";
		
		// Istanziazione oggetto entity di modello
		entityClassObject = this.rm.newInstance(entityClass, null, null);
		
		
		// New entry per FOR_ANY FOR_EACH FOR_SQL
		innerForAnyEach = new InnerForAnyEach();
		this.al_forAnyEach.add(innerForAnyEach);
		if (entityClass != null) {
			innerForAnyEach.entityName = entityClass.getSimpleName();
		    dbTableAnnotation = entityClass.getAnnotation(DataBaseMappedTable.class);
		}
		innerForAnyEach.entityObject = entityClassObject;
		innerForAnyEach.indexEntry = this.al_forAnyEach.size() - 1;
		innerForAnyEach.typeEntry = typeEntry;
		innerForAnyEach.entityNameAs = entityNameAs;
		innerForAnyEach.whereSql = whereSql;
	    if (dbTableAnnotation != null) {
	    	innerForAnyEach.entityNameDb = dbTableAnnotation.value();
		}
		
	    // Impostazione colonne PK da escludere e colonne NOT PK da includere/escludere
		innerForAnyEach.ar_primaryKeyExclude = ar_primaryKeyExclude;
		if (ar_primaryKeyExclude == null) {
			innerForAnyEach.ar_primaryKeyExclude = new String[0];
		}
		innerForAnyEach.areIncludedAllFields = areIncludedAllFields;
		for (String fieldToIncludeExclude : ar_fieldsToIncludeExclude) {
			innerForAnyEach.al_includedExcludedField.add(fieldToIncludeExclude);
		}

		
		// FOR_SQL()
	    // Le informazioni sulle colonne, AS clause, primary key e NOT primary key
	    // saranno valorizzate al momento della prima esecuzione della select sql
	    // quando dal recordset si potranno recuperare via metaData le informazioni
	    if (typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_SQL) {
			return innerForAnyEach;
		}
		

	    /////////////////////////////////////////////////////////////////////////
		// Comune a FOR_ANY() e FOR_EACH()
		/////////////////////////////////////////////////////////////////////////
	    
		// Info descrittive complete campi chiave dell'entity in FOR_ANY() o FOR_EACH()
		innerForAnyEach.ar_primarKey = dbei.getDescriptorsPK(entityClassObject);
		innerForAnyEach.ar_notPrimarKey = dbei.getDescriptorsNotPK(entityClassObject);

		// Nomi colonne di entity da esporre e caricamento struttura generale di controllo colonne di istanza
		populateEntityColumnsAs(innerForAnyEach, ar_asClause, this.al_forAnyEach.size() - 1);
		
		// Creazione variabili di colonna per tutti i campi primary key
		for (DataBaseItemDescriptor descriptorColumn : innerForAnyEach.ar_primarKey) {
			varName = descriptorColumn.getAsName();
			if (varName.equals("")) {
				varName = descriptorColumn.getJavaFieldName();
			}
			descriptorColumn.setAsName(varName);
			if (isVarDeclared(varName)) {continue;} 						 
			createVar(descriptorColumn);
		}

		// Accesso a RULE table
		// Nel caso di rule table i nomi delle colonne NON PK sono virtuali, definiti da EntityTableStructure
		// In questo caso devono essere estratti con substr() nella select sql da comporre (se presenti + campi).
		// Se non è definita una struttura della tabella, si tratta di tabella di trascodifica e vale il campo rowData (TBDTRDAT)
		// Il matching con i campi key applicativi a partire dall'ultimo campo chiave della tabella
		if (typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
		||  typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
			this.returnCode = getRuleTableNotPKFields(innerForAnyEach);
		    if (this.returnCode > 0) {return innerForAnyEach;}
			innerForAnyEach.ruleTableFirstKeyColToBound = innerForAnyEach.ar_primarKey[innerForAnyEach.ar_primarKey.length - 1].getJavaFieldName();
		}
		
		//  Creazione variabili di colonna per tutti i campi NOT primary key, reali o virtuali (tabelle di dominio) che siano
		for (DataBaseItemDescriptor descriptorColumn : innerForAnyEach.ar_notPrimarKey) {
			varName = descriptorColumn.getAsName();
			if (varName.equals("")) {
				varName = descriptorColumn.getJavaFieldName();
			}
			descriptorColumn.setAsName(varName);
			if (isVarDeclared(varName)) {continue;} 						 
			createVar(descriptorColumn);
		}

		// Accesso a rule table: fine operazioni
		if (typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE 
		||  typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
			return innerForAnyEach;
		}
		
		// Accesso NON a rule table
		
		// Estrazione info tabelle di dominio RULE tables accedibili con dati (key) da entity corrente (al momento 1 solo campo chiave)
		ar_usableRuleTable = dbei.getRuleTableAnnotations(entityClass);
		for (DataBaseMappedRuleTable usableRuleTable : ar_usableRuleTable) {
			innerUsableRuleTable = new InnerUsableRuleTable();
			innerUsableRuleTable.tabNumCol = usableRuleTable.tabNumCol();
			innerUsableRuleTable.tabRef = usableRuleTable.tabRef();
			innerUsableRuleTable.tabId = usableRuleTable.tabId();
			innerUsableRuleTable.tabSystem = usableRuleTable.tabSystem();
			innerUsableRuleTable.tabKeyColsBound = usableRuleTable.tabKeyColsBound();
			innerForAnyEach.hm_usableRuleTable.put(usableRuleTable.tabRef(), innerUsableRuleTable);
		}


		/////////////////////////////////////////////////////////////////////////
		// Estrazione info entities in FOR_ANY() di entities
		/////////////////////////////////////////////////////////////////////////
		
			
		// Scan entities a loro volta in FOR_ANY() con l'entity corrente dichiarata
		ar_entityNames = dbei.getForAnyEntityNames(entityClass);
		for (String forAnyEntity : ar_entityNames) {
			
			// Annotation descriptor della FOR_ANY() 
			forAnyAnnotation = dbei.getForAnyAnnotation(entityClass, forAnyEntity);

			// Se Classe entity in ForAny non presente skip
			classForAny = rm.getClass(forAnyEntity, entityClassObject);
			if (classForAny == null) {continue;}

			// Istanziazione oggetto entity in FOR_ANY()
			entityClassForAnyObject = this.rm.newInstance(classForAny, null, null);
			
			// Descrittori di tutti i campi dichiarati nell'entity relazionata
			ar_descriptorFieldForAny = dbei.getDescriptorsAllFields(entityClassForAnyObject);
			
			// Costruzione coppie descrittori campi di collegamento fra le due entità
			// Il primo valore è relativo all'entity dichiarata che si sta trattando
			// Il secondo valore è relativo all'entity relazionata
			al_forwardColumnsPair = new ArrayList<ForwardColumnPair> ();
			
			// Scan campi di correlazione fra le due entità, come dichiarato nella annotation
			for (String colsPair : forAnyAnnotation.colsBound()) {
				scn = new Scanner(colsPair);
				
				// Nomi colonne in relazione
				columnName = scn.next();
				columnRelatedName = scn.next();
				
				// Descrittore colonna in relazione e relazionata
				columnDescriptor = findColumnDescriptor(columnName, innerForAnyEach.ar_primarKey);
				if (columnDescriptor == null) {
					columnDescriptor = findColumnDescriptor(columnName, innerForAnyEach.ar_notPrimarKey);
				}
				columnRelatedDescriptor = findColumnDescriptor(columnRelatedName, ar_descriptorFieldForAny);

				// Errore nella codifica dell'annotazione: colonna o colonna relazionata non definite nell'entity/entity relazionata
				// L'associazione viene scartata
				if (columnDescriptor == null) {continue;}
				
                // Composizione coppia di colonne in relazione fra le due tabelle e accodamento
				forwardColumnsPair = new ForwardColumnPair(columnDescriptor, columnRelatedDescriptor);
				al_forwardColumnsPair.add(forwardColumnsPair);
			}
			
			// Conversione a Array e inserimento entry in map
			ar_forwardColumnsPair = new ForwardColumnPair[al_forwardColumnsPair.size()];
			ar_forwardColumnsPair = al_forwardColumnsPair.toArray(ar_forwardColumnsPair);
			innerForAnyEach.hm_forAny.put(forAnyEntity, ar_forwardColumnsPair);
		}

		
		/////////////////////////////////////////////////////////////////////////
		// Estrazione info entities in FOR_EACH()
		/////////////////////////////////////////////////////////////////////////
	
		/*	
		// Scan entities a loro volta in FOR_EACH() con l'entity corrente dichiarata
		ar_entityNames = dbei.getForEachEntityNames(entityClass);
//		for (String forEachEntity : ar_entityNames) {
			
			// Annotation descriptor della FOR_EACH() 
//			forEachAnnotation = dbei.getForEachAnnotation(entityClass, forEachEntity);
			
			// Se Classe entity in ForAny non presente skip
			classForEach = rm.getClass(forEachEntity, entityClassObject);
			if (classForEach == null) {continue;}

			// Istanziazione oggetto entity in FOR_EACH()
			entityClassForEachObject = this.rm.newInstance(classForEach, null, null);

			// Descrittori di tutti i campi dichiarati nell'entity relazionata
			ar_descriptorFieldForAny = dbei.getDescriptorsAllFields(entityClassForEachObject);
			
			// Costruzione coppie descrittori campi di collegamento fra le due entità
			// Il primo valore è relativo all'entity dichiarata che si sta trattando
			// Il secondo valore è relativo all'entity relazionata
			al_forwardColumnsPair = new ArrayList<ForwardColumnPair> ();
			
			// Scan campi di correlazione fra le due entità, come dichiarato nella annotation
			/*
			for (String colsPair : forEachAnnotation.colsBound()) {
				scn = new Scanner(colsPair);
				
				// Nomi colonne in relazione
				columnName = scn.next();
				columnRelatedName = scn.next();
				
				// Descrittore colonna in relazione e relazionata
				columnDescriptor = findColumnDescriptor(columnName, innerForAnyEach.ar_primarKey);
				if (columnDescriptor == null) {
					columnDescriptor = findColumnDescriptor(columnName, innerForAnyEach.ar_notPrimarKey);
				}
				columnRelatedDescriptor = findColumnDescriptor(columnRelatedName, ar_descriptorFieldForAny);

				// Errore nella codifica dell'annotazione: colonna o colonna relazionata non definite nell'entity/entity relazionata
				// L'associazione viene scartata
				if (columnDescriptor == null) {continue;}
				
                // Composizione coppia di colonne in relazione fra le due tabelle e accodamento
				forwardColumnsPair = new ForwardColumnPair(columnDescriptor, columnRelatedDescriptor);
				al_forwardColumnsPair.add(forwardColumnsPair);
			}
			
			// Conversione a Array e inserimento entry in map
			ar_forwardColumnsPair = new ForwardColumnPair[al_forwardColumnsPair.size()];
			ar_forwardColumnsPair = al_forwardColumnsPair.toArray(ar_forwardColumnsPair);
			innerForAnyEach.hm_forEach.put(forEachEntity, ar_forwardColumnsPair);
		}
*/
//		return innerForAnyEach;
			return null;
	}


	/*  -----------------------------------------------------------------
	 *  Estrazione di tutte le colonne da portare in output per l'entity.
	 *  -----------------------------------------------------------------
	 *  
	 *  Viene accodato il columnAsName all'elenco colonne dell'entity, nella struttura forAnyEach
	 *    - Se non è presente la clausola AS il nome è lo stesso del campo java definito nell'entity
	 *  Viene inserita la colonna nella map generale dei campi esposti dalla view, con index forAnyEach e index colonna 
	 *  
	 */
	private void populateEntityColumnsAs(InnerForAnyEach forAnyEach, String[] ar_asClause, int indexForAnyEach) {
		
		DataBaseItemDescriptor dbItemDescriptor = null;
		String asColumnToExpose = "";
		String fieldNameDb = "";
		String fieldNameJava = "";
		
		
		// Sono inclusi SOLO i campi esplicitamente forniti, se non definiti vengono ignorati
		if (!forAnyEach.areIncludedAllFields) {
			for (String columnNameJava : forAnyEach.al_includedExcludedField) {
				dbItemDescriptor = getDataBaseItemDescriptor(forAnyEach, columnNameJava);
				if (dbItemDescriptor == null) {continue;}
				fieldNameDb = dbItemDescriptor.getDbColumn();
				asColumnToExpose = getAsColumnToExpose(columnNameJava, ar_asClause);
				dbItemDescriptor.setAsName(asColumnToExpose);
				forAnyEach.al_columnAsName.add(asColumnToExpose);
				putAsColumnInStructure(forAnyEach, indexForAnyEach, columnNameJava, fieldNameDb, asColumnToExpose);  // Inserimento colonna esposta in struttura di istanza
				if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY
				||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH
				||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
				||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
					dbItemDescriptor.setDbColumnIndex(this.cntColumnExposed - 1);
				} else {
					dbItemDescriptor.setDbColumnIndex(forAnyEach.cntColumnInsUpd - 1);
				}
			}
			return;
		}
		
		// Sono inclusi TUTTI i campi MENO quelli da escludere
		
		// Column primary key
		for (DataBaseItemDescriptor fieldPK : forAnyEach.ar_primarKey) {
			if (forAnyEach.al_includedExcludedField.contains(fieldPK.getJavaFieldName())) {continue;}
			fieldNameDb = fieldPK.getDbColumn();
			fieldNameJava = fieldPK.getJavaFieldName();
			asColumnToExpose = getAsColumnToExpose(fieldNameJava, ar_asClause);
			fieldPK.setAsName(asColumnToExpose);
			forAnyEach.al_columnAsName.add(asColumnToExpose);
			putAsColumnInStructure(forAnyEach, indexForAnyEach, fieldNameJava, fieldNameDb, asColumnToExpose);  // Inserimento colonna esposta in struttura di istanza
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
				fieldPK.setDbColumnIndex(this.cntColumnExposed - 1);
			} else {
				fieldPK.setDbColumnIndex(forAnyEach.cntColumnInsUpd - 1);
			}
		}
		
		// Column dati
		for (DataBaseItemDescriptor fieldNotPK : forAnyEach.ar_notPrimarKey) {
			if (forAnyEach.al_includedExcludedField.contains(fieldNotPK.getJavaFieldName())) {continue;}
			fieldNameDb = fieldNotPK.getDbColumn();
			fieldNameJava = fieldNotPK.getJavaFieldName();
			asColumnToExpose = getAsColumnToExpose(fieldNameJava, ar_asClause);
			fieldNotPK.setAsName(asColumnToExpose);
			forAnyEach.al_columnAsName.add(asColumnToExpose);
			putAsColumnInStructure(forAnyEach, indexForAnyEach, fieldNameJava, fieldNameDb, asColumnToExpose);  // Inserimento colonna esposta in struttura di istanza
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
				fieldNotPK.setDbColumnIndex(this.cntColumnExposed - 1);
			} else {
				fieldNotPK.setDbColumnIndex(forAnyEach.cntColumnInsUpd - 1);
			}
		}
		return;
	}

	/*
	 * Restituisce il descrittore completo della colonna di data base dell'entity in input
	 * I descrittori sono già disponibile per PK e non PK
	 * Se non è definita una colonna con il nome java fornito restituisce null
	 */
	private DataBaseItemDescriptor getDataBaseItemDescriptor(InnerForAnyEach forAnyEach, String columnNameJava) {
		for (DataBaseItemDescriptor dataBaseItemDescriptor : forAnyEach.ar_primarKey) {
			if (dataBaseItemDescriptor.getJavaFieldName().equals(columnNameJava)) {
				return dataBaseItemDescriptor;
			}
		}
		for (DataBaseItemDescriptor dataBaseItemDescriptor : forAnyEach.ar_notPrimarKey) {
			if (dataBaseItemDescriptor.getJavaFieldName().equals(columnNameJava)) {
				return dataBaseItemDescriptor;
			}
		}
		return null;
	}


	/*
	 * Restituisce il nome java della colonna o quello fornito dalla clausola AS, se presente
	 */
	private String getAsColumnToExpose(String fieldNameJava, String[] ar_asClause) {
		Scanner scn = null;
		String token = "";
        String asColumnToExpose = "";
		
        asColumnToExpose = fieldNameJava;
        	
		// Scan elenco di nomeJava AS newName
		for (String asClause : ar_asClause) {
			scn = new Scanner(asClause);
			token = scn.next();
			if (!token.equals(fieldNameJava)) {continue;}
			if (!scn.hasNext()) {continue;}
			token = scn.next();
			if (!token.toUpperCase().equals("AS"))  {continue;}
			if (!scn.hasNext()) {continue;}
			token = scn.next();
			asColumnToExpose = token;
			break;
		}
		return asColumnToExpose;
	}

    /*
     * Inserimento AS column (tutte le colonne vengono dichiarate nella select con AS)
     * nella Map di istanza e update contatore generale.
     * Struttura map di ottimizzazione e accesso veloce alle colonne interessate.
     */
	private InnerAsColumn putAsColumnInStructure(InnerForAnyEach forAnyEachInsUpdDel, int indexForAnyEach, String fieldNameJava, String fieldNameDb, String asColumnToExpose) {
		InnerAsColumn innerAsColumn = null;
		
		// Allocazione e inizializzazionw struttura
		innerAsColumn = new InnerAsColumn();
		innerAsColumn.entityOwnerIndex = indexForAnyEach;
		innerAsColumn.columnNameJava = fieldNameJava;
		innerAsColumn.columnNameDb = fieldNameDb;
		innerAsColumn.columnNameAs = asColumnToExpose;
		if (this.hm_columnAsName.get(asColumnToExpose) == null) {
			this.hm_columnAsName.put(asColumnToExpose, innerAsColumn); 
		}
		
		// Incremento counter generale di istanza colonne esposte a fronte di FOR_ANY() o FOR_EACH().
		if (forAnyEachInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY
		||  forAnyEachInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH
		||  forAnyEachInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
		||  forAnyEachInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
			innerAsColumn.columnIndex = this.cntColumnExposed;
			this.cntColumnExposed++;
			return innerAsColumn;
		}
		
		// Incremento counter specifico in direttiva LDV_INSERT, DELETE o UPDATE
		innerAsColumn.columnIndex = forAnyEachInsUpdDel.cntColumnInsUpd;
		forAnyEachInsUpdDel.cntColumnInsUpd++;
		
		return innerAsColumn;
	}


	/* ------------------------------------------------------------------------
	 * Completamento attributi NON chiave di tabella di dominio/configurazione
	 * ------------------------------------------------------------------------
	 * 
	 * I campi NON sono definiti come campi effettivi di EntityTableData, ma virtualmente dentro il solo campo rowData (TBDTSDAT).
	 * Il tracciato che definisce rowData, con i nomi dei campi, è definito da EntityTableStructure.
	 * Quindi si effettuano le seguenti operazioni:
	 * - Lettura descrittori campi tabella, con posizione, lunghezza e tipo
	 * - Aggiornamento colonne non PK in struttura FOR_ANY() a simulazione di una qualsiasi entity
	 * - Se la struttura NON è definita, si considera la rule table di solo dominio e si considera una solo colonna
	 *   virtuale con nome TnnnDESC. L'appplicazione potrà ricevere il nome desiderato con la clausola AS
	 * - Se esiste un tracciato per la rule table, ovvero è stata definita, vengono valorizzati le colonne con nome e tipo
	 */
    private int getRuleTableNotPKFields(InnerForAnyEach innerForAnyEach) {
    	
    	EntityTableStructure entityTableItemDefinition = null;
    	Object[] ar_objEntityTableItemDefinition = null;
    	DataBaseItemDescriptor dataBaseItemDescriptor = null;
    	String whereCondition = "";
    	String curSystem = "*";
    	String curSubSystem = "*";
       	String columnName = "";
       	String strWrk = "";
    	
		entityTableItemDefinition = new EntityTableStructure();
     	
        // Composizione Where di lettura definizioni campi tabella
        whereCondition =                  "     TBSTSYST = '" + curSystem    + "'";
      	whereCondition = whereCondition + " AND TBSTSUBS = '" + curSubSystem + "'";
      	whereCondition = whereCondition + " AND TBSTTTAB =  " + innerForAnyEach.ruleTableNum;

    	try {
			dbs = new DataBaseStatusDetailed();
			dbConn = dbm.getConnection(dbs);
			dbei = new DataBaseEntityInterface(sd, dbm, dbConn);
			ar_objEntityTableItemDefinition = dbei.readSetEntity(entityTableItemDefinition, whereCondition, "");
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
		} catch (ExceptionAmritaSqlError e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (SQLException e) {
			dbm.releaseConnection(dbConn, dbs);
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmrita e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		}	
      	
      	// La tabella non è definita come struttura.
    	// Suppongo sia di sola trascodifica con un solo campo da utilizzare come descrizione che è tutto il campo RDAT
    	// Rimane valida la struttora non PK già impostata con il solo campo descrizione rowData (TBDTRDAT)
    	if (ar_objEntityTableItemDefinition == null 
    	|| (ar_objEntityTableItemDefinition != null && ar_objEntityTableItemDefinition.length == 0)) {
      		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
		}
    	
    	
        // Tabella strutturata in n campi ospitati nella colonna db fisica TBDTRDAT
     	// Scan campi definiti nella tabella
    	innerForAnyEach.ar_notPrimarKey = new DataBaseItemDescriptor[ar_objEntityTableItemDefinition.length];
      	for (int i = 0; i < ar_objEntityTableItemDefinition.length; i++) {
      		
      		entityTableItemDefinition = (EntityTableStructure) ar_objEntityTableItemDefinition[i];
      		dataBaseItemDescriptor = new DataBaseItemDescriptor();
      		strWrk = "000" + innerForAnyEach.ruleTableNum;
      		columnName = "T" + strWrk.substring(strWrk.length() - 3) + "DESC";
      		dataBaseItemDescriptor.setDbColumn(columnName);
     		dataBaseItemDescriptor.setDbColumnLength(entityTableItemDefinition.getLngBytes());
      		dataBaseItemDescriptor.setDbColumnType(EnumDataBaseJdbcSqlType.VARCHAR);
      		dataBaseItemDescriptor.setDbTable("TBDT");
      		
      		// Valore alfanumerico
      		if (entityTableItemDefinition.getTypeItem() == EnumDataItemGeneric.DATA_ITEM_TEXT) {
          		dataBaseItemDescriptor.setJavaFieldClassName(String.class.getName());
         		dataBaseItemDescriptor.setJavaFieldClassNameSimple(String.class.getSimpleName());
        		dataBaseItemDescriptor.setJavaFieldType(String.class);
         		dataBaseItemDescriptor.setJavaFieldClass(String.class);
         		innerForAnyEach.ar_notPrimarKey[i] = dataBaseItemDescriptor;
         		continue;
			}
      		
      		// Valore numerico
      		if (entityTableItemDefinition.getTypeItem() == EnumDataItemGeneric.DATA_ITEM_NUMERIC) {
          		dataBaseItemDescriptor.setJavaFieldClassName(int.class.getName());
         		dataBaseItemDescriptor.setJavaFieldClassNameSimple(int.class.getSimpleName());
        		dataBaseItemDescriptor.setJavaFieldType(int.class);
         		dataBaseItemDescriptor.setJavaFieldClass(int.class);
         		innerForAnyEach.ar_notPrimarKey[i] = dataBaseItemDescriptor;
         		continue;
			}
      		
      		// Valore booleano true o false
      		if (entityTableItemDefinition.getTypeItem() == EnumDataItemGeneric.DATA_ITEM_CONDITION) {
          		dataBaseItemDescriptor.setJavaFieldClassName(boolean.class.getName());
         		dataBaseItemDescriptor.setJavaFieldClassNameSimple(boolean.class.getSimpleName());
        		dataBaseItemDescriptor.setJavaFieldType(boolean.class);
         		dataBaseItemDescriptor.setJavaFieldClass(boolean.class);
         		dataBaseItemDescriptor.setDbColumnLength(5);
         		innerForAnyEach.ar_notPrimarKey[i] = dataBaseItemDescriptor;
         		continue;
			}
   		}

       	return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}

	/*
     * Ricerca colonna in elenco fornito
     */
	private DataBaseItemDescriptor findColumnDescriptor(String columnName, DataBaseItemDescriptor[] ar_columnDescriptor) {
		for (DataBaseItemDescriptor dataBaseItemDescriptor : ar_columnDescriptor) {
			if (dataBaseItemDescriptor.getJavaFieldName().equals(columnName)) {
				return dataBaseItemDescriptor;
			}
		}
		return null;
	}



	/**
	 * Validates the logical data view.<br>
	 * <p>
	 * When the logical data view is a <code>FOR_SQL()</code> type, the validation<br>
	 * process is quite different, because there is not a descriptor but only a user coded<br>
	 * sql statement.<br>
	 * In this case the sql select will be executed just one time to get and store all informations
	 * querying directly the database through jdbc metadata requests.<br>
	 * 
	 * @param entityName the name of the class that declare the entity
	 */
	public int validate() {
		
		InnerForAnyEach forAnyEach = null;
		InnerForAnyEach forAnyEachPrec = null;
		ForwardColumnPair[] ar_columnPair = null;
		ArrayList<InnerUsableRuleTable> al_usableRuleTable = null;
		String columnNameToMatch = "";
		StringBuffer sbOrderByColumn = null;
		Scanner sc = null;
		InnerAsColumn innerAsColumn =null;
		String columnToOrder = "";
		String ascDesc = "";								// Ordinamento ascendente o decrescente
		String comma = "";									// Di servizio
		boolean isRelationshipFound = false;
		boolean isRuleTableFound = false;
		int indexEntity = 0;
		int i = 0;
		int k = 0;
		int retValidate = 0;
		
		
		// Logical data view con sql embedded.
		// Deve esserci una sola FOR_SQL() valida
		if (isForSql()) {
			if (this.al_forAnyEach.size() > 1) {return EnumForwardLogicalDataViewControl.LDV_ERROR_FOR_SQL_VALIDATE.ordinal();}
			forAnyEach = this.al_forAnyEach.get(0);
			retValidate = validateForSql(forAnyEach);
			return retValidate;
		}
		
		// Impostazione flag di logical data view di accesso generico a rule table
		if (this.al_forAnyEach.size() == 1) {
			forAnyEach = this.al_forAnyEach.get(0);
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
				this.setReadSetRuleTable(true);}
		}
		
		// Scan declaratives & validation
		for (i = 0; i < this.al_forAnyEach.size(); i++) {
			forAnyEach = this.al_forAnyEach.get(i);
			
			validateWhereSql(forAnyEach);			// Se presente una where imposta i nomi di colonna db qualificati
			
			// Prima dichiarazione
			if (i == 0  ) {
				// FOR_ANY(), deve esserci una primary key
				if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY && forAnyEach.ar_primarKey.length == 0) {
					return EnumForwardLogicalDataViewControl.LDV_ERROR_NO_PK.ordinal();
				}
				// FOR_EACH() di rule table: deve essere la sola dichiarativa nella logical data view
				if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE && this.al_forAnyEach.size() > 1) {
					return EnumForwardLogicalDataViewControl.LDV_ERROR_FOR_EACH_RULE_TABLE_NOT_UNIQUE.ordinal();
				}
				// FOR_SQL(), attivazione analisi assesment per analisi istruzione
				if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_SQL) {
					return analyzeSqlSelect(forAnyEach);
				}
				continue;
			}
			
			isRelationshipFound = false;
			
			// Scan dichiarazioni precedenti
			for (int j = i - 1; j >= 0; j--) {
				
				forAnyEachPrec = this.al_forAnyEach.get(j);
				
				// Relazione fra due entities in FOR_ANY() o FOR_EACH() 
				if ((forAnyEach.typeEntry     == EnumForwardLogicalDataViewControl.LDV_FOR_ANY || forAnyEach.typeEntry     == EnumForwardLogicalDataViewControl.LDV_FOR_EACH)
				&& 	(forAnyEachPrec.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY || forAnyEachPrec.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH)) {
					// Non è relazionata in FOR_ANY() o FOR_EACH() a quella corrente: skip
					if (!forAnyEachPrec.getForAnyEntityNames().contains(forAnyEach.entityName) && !forAnyEachPrec.getForEachEntityNames().contains(forAnyEach.entityName)) {continue;}
					// Individuata l'entity relazionata con quella corrente
					forAnyEach.entityNameRelatedWith = forAnyEachPrec.entityName;
					ar_columnPair = forAnyEachPrec.hm_forAny.get(forAnyEach.entityName);
					if (ar_columnPair == null) {
						ar_columnPair = forAnyEachPrec.hm_forEach.get(forAnyEach.entityName);
					}
					if (ar_columnPair.length > 0) {
						// Verifica se tutti i campi hanno corrispondenza, il secondo campo deve essere definito nell'entity corrente relazionata
						for (ForwardColumnPair columnPair : ar_columnPair) {
							columnNameToMatch = columnPair.getColumnRelated().getJavaFieldName();
							if (!forAnyEach.isColumnDeclared(columnNameToMatch)) {
								this.isErrorFound = true;
								return EnumForwardLogicalDataViewControl.LDV_ERROR_COLUMN_MATCHING.ordinal();
							}
						} // end-for
						isRelationshipFound = true;
						break;
					} // end-if
				} // end-if
				
				// Relazione fra due entities individuata
				if (isRelationshipFound) {
					break;
				}
				
				// Relazione fra entity in FOR_ANY() o FOR_EACH() e FOR_ANY() di rule table
				if ((forAnyEach.typeEntry  == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE)
				&& 	(forAnyEachPrec.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY || forAnyEachPrec.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH)) {
					
					al_usableRuleTable = forAnyEachPrec.getUsableRuleTables(forAnyEach.ruleTableId);
					
					// Rule table NON dichiarate nell'entity in corso di analisi: continua la ricerca backward
					if (al_usableRuleTable.size() == 0) {continue;}
					isRelationshipFound = true;
					forAnyEach.entityNameRelatedWith = forAnyEachPrec.entityName;
					
					// Indicazione se rule table di sistema (sistema e sottosistema saranno forzati a '*')
					forAnyEach.ruleTableSystem = al_usableRuleTable.get(0).tabSystem;
					forAnyEach.ruleTableRef = al_usableRuleTable.get(0).tabRef;

					// RuleTableKeyBound NON esplicitata nella dichiarazione: prendo la prima dichiarazione della rule table per buona
					if (forAnyEach.ruleTableRelatedKeysName.length == 0) {
						forAnyEach.ruleTableRelatedKeysName = new String[al_usableRuleTable.get(0).tabKeyColsBound.length];
						k = 0;
						for (String KeyColBound : al_usableRuleTable.get(0).tabKeyColsBound) {
							forAnyEach.ruleTableRelatedKeysName[k] = KeyColBound;
							k++;
						}
                        break;
					}
					
					// RuleTableKeyBound ESPLICITATA nella dichiarazione FOR_ANY() rule table: si cerca il reference corretto alla rule table in forAnyEachPrec
					
					// Scan dichiarazioni nell'entity precedente alla stessa rule table
					for (InnerUsableRuleTable usableRuleTable : al_usableRuleTable) {
						// Scan ruleTableKeysBound esplicitate
						for (String ruleTableRelatedKeyName : forAnyEach.ruleTableRelatedKeysName) {
							isRuleTableFound = false;
							// Scan key bound di accesso alla rule table
							for (String tabKeyColBound : usableRuleTable.tabKeyColsBound) {
								if (ruleTableRelatedKeyName.equals(tabKeyColBound)) {
									isRuleTableFound = true;
									break;
								}
							}
							if (!isRuleTableFound) {break;}
							// Match keys rule table
							forAnyEach.ruleTableRef = usableRuleTable.tabRef;
							break;
						}
						if (isRuleTableFound) {break;}
					}
					
					// Keys rule table NON matchano con entity precedente: errore
					if (!isRuleTableFound) {
						this.isErrorFound = true;
						return EnumForwardLogicalDataViewControl.LDV_ERROR_RULE_TABLE_KEYS_MATCH.ordinal();
					}
					
					// Relazione fra entity e rule table individuata
					isRelationshipFound = true;
					break;
				}

			} // end-for backward
			
			// Nessuna entity precedente in relazione con quella corrente
			if (!isRelationshipFound) {
				this.isErrorFound = true;
				return EnumForwardLogicalDataViewControl.LDV_ERROR_NO_RELATIONSHIP.ordinal();
			}
			
		} // end-for forward
		
		// OrderBy assente: fine operazioni
		if (this.ar_orderByColumn.length == 0) {
			this.setValidateToDo(false);
			return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
		}
		
 		// Validazione Order By e preparazione stringa Sql pronta da accodare 
		// La colonna deve essere espressa con il nome esposto da una clausola AS seguito da ASC o DESC
		sbOrderByColumn = new StringBuffer();
		for (String orderByColumn : this.ar_orderByColumn) {
			
			if (orderByColumn.equals("")) {continue;}
			sc = new Scanner(orderByColumn);
			columnToOrder = sc.next();
			ascDesc = "ASC";
			if (sc.hasNext()) {
				ascDesc = sc.next();
				if (!ascDesc.equals("ASC") && !ascDesc.equals("DESC")) {return EnumForwardLogicalDataViewControl.LDV_ERROR_ORDER_BY.ordinal();}
			}	
			
			// Composizione ordinamento colonna
			innerAsColumn = this.hm_columnAsName.get(columnToOrder);
			if (innerAsColumn == null) {return EnumForwardLogicalDataViewControl.LDV_ERROR_ORDER_BY.ordinal();}
			indexEntity = innerAsColumn.entityOwnerIndex;
			sbOrderByColumn.append(comma);
			sbOrderByColumn.append(this.al_forAnyEach.get(indexEntity).entityNameAs + "." + innerAsColumn.columnNameDb + " " + ascDesc); 
			comma = ", ";
		}
		
		// Order by string utilizzata alla sql create
		this.strSqlOrderByColumn = sbOrderByColumn.toString();
		this.setValidateToDo(false);
 		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	
	/*
	 * Validazione where.
	 * Se un token è il nome di una colonna, si sostituisce con il nome della colonna db, qualificato.
	 */
	private void validateWhereSql(InnerForAnyEach forAnyEach) {
		InnerAsColumn innerAsColumn = null;
		Scanner sc = null;
		StringBuffer sbWhereNormalized = null;
		String token = "";
		
		// Validazione where.
		sc = new Scanner(forAnyEach.whereSql);
		sbWhereNormalized = new StringBuffer();
		// Scan token where
		while (sc.hasNext()) {
			token = sc.next();
			innerAsColumn = this.hm_columnAsName.get(token);
			// Non è una colonna, accodo normalmente
			if (innerAsColumn == null) {
				sbWhereNormalized.append(" " + token + " ");
				continue;
			}
			// Accodo il nome db qualificato
			sbWhereNormalized.append(" " + forAnyEach.entityNameAs + "." + innerAsColumn.columnNameDb + " ");
		}
		// Restore normalized
		forAnyEach.whereSql = sbWhereNormalized.toString();
	}

	/*
	 * Validazione FOR_SQL()
	 * ---------------------
	 * 
	 * 1) Attivazione anayzer SQL per verifica formale statement
	 * 2) Esecuzione select per recupero in formazioni metadata, da resultset, impostazione numero righe da restituire a 1
	 * 3) Se esecuzione senza exception
	 *    - Recupero informazioni da resultset come nomi colonne, tipi, nome tabella etc
	 *    - Caricamento descrittori standardi di forAnyEach
	 * 4) Se punto 3) completato interrogazione database per recupero informazioni sulle PK delle tabelle coinvolte
	 *   
	 */
	private int validateForSql(InnerForAnyEach forAnyEach) {
		
		ResultSet rs = null;
		ResultSetMetaData metaData = null;
		DataBaseMetaTableColumn[] ar_tableColumnDescriptor = null;
		Set<String> hs_table = null;
		Map<String, DataBaseMetaTableColumn[]> hm_tableDescriptor = null;
		String tableName = "";
		String columnNameDb = "";
		String columnNameAs = "";
		int columnSqlType = 0;
		int retCode = 0;
		int columnCount = 0;
		
		// Attivazione ed esecuzione analyzer SQL
		// TODO
		 
		
		
		// Esecuzione select per interrogazione resultset, con limitazione a 1 riga restituita
		try {
			dbs = new DataBaseStatusDetailed();
			dbConn = dbm.getConnection(dbs);
			rs = dbm.execSqlGeneric(dbConn, forAnyEach.selectSql, dbs, 1);
			metaData = rs.getMetaData();							// Serve anche per il conteggio colonne
			rs.close();
			
			hs_table = new HashSet<String> ();
			hm_tableDescriptor = new HashMap<String, DataBaseMetaTableColumn[]> ();

			columnCount =  metaData.getColumnCount();
			for (int i = 1; i <= columnCount; i++) {
				
				// Informazioni base da metadata recordset
				columnNameDb = metaData.getSchemaName(i);
				columnNameAs = metaData.getColumnLabel(i);
				columnSqlType = metaData.getColumnType(i);			// Costante java.sql.Types.
				tableName = metaData.getTableName(i);
				
				// Recupero descrittore completo campi tabella se non ancora effettuato
				hs_table.add(tableName);
				if (hm_tableDescriptor.get(tableName) == null) {
					ar_tableColumnDescriptor = dbm.tableInfoColumns(dbConn, tableName, dbs);
					hm_tableDescriptor.put(tableName, ar_tableColumnDescriptor);
				}
				ar_tableColumnDescriptor = hm_tableDescriptor.get(tableName);
				
				// A questo punto sono disponibili tutte le informazioni.
				// Informazioni base della colonna come esposte nello statement sql
				// Informazioni complete di tutti i campi della tabella che lo contiene
				forAnyEach.al_columnAsName.add(columnNameAs);
				
			}
			
			
		} catch (SQLException e) {
			retCode = EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmritaSqlError e) {
			retCode = EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} finally  {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
		}
		return retCode;
	}
 

	/*
	 * Creates the executable sql select string.<br>
	 * <p>
	 * 
	 */
	private EnumForwardLogicalDataViewControl createSql() {
		
		InnerForAnyEach forAnyEach = null;
		InnerForAnyEach forAnyEachPreviousRelated = null;
		EnumForwardLogicalDataViewControl retCode = EnumForwardLogicalDataViewControl.LDV_OK;
		StringBuffer sbSqlString = null;
		StringBuffer sbSqlFrom = null;
		StringBuffer sbSqlWhere = null;
		String sqlString = null;
		String sqlJoinCondition = null;
		String comma = "";
		String and = "";
		
		sbSqlString = new StringBuffer();
		this.sqlSelectString = "";
		this.strSqlWhereCondition = "";
		 
		sbSqlString.append("SELECT ");
		
		// Scan entities
		for (InnerForAnyEach forAnyEachLoop : this.al_forAnyEach) {
			sqlString = extractColumnsWithAsClause(forAnyEachLoop);
			if (sqlString.equals("")) {continue;}
			sbSqlString.append(comma);
			sbSqlString.append(sqlString);
			comma = ", ";
		}
		
		sbSqlString.append(" FROM ");
		
		sbSqlFrom = new StringBuffer();
		forAnyEach = this.al_forAnyEach.get(0);
		sbSqlFrom.append(forAnyEach.entityNameDb + " AS " + forAnyEach.entityNameAs + " ");
		 
		// Scan entities a partire dalla seconda.
		// Composizione join.
		for (int i = 1; i < this.al_forAnyEach.size(); i++) {
			forAnyEach = this.al_forAnyEach.get(i);										// FOR_ANY() FOR_EACH() corrente
			forAnyEachPreviousRelated = getPreviousForAnyEachRelated(forAnyEach);		// FOR_ANY() FOR_EACH() precedente
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY 
			||  forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {
				sbSqlFrom.append(" LEFT JOIN "  + forAnyEach.entityNameDb + " AS " + forAnyEach.entityNameAs + " ");
			} else {
				sbSqlFrom.append(" INNER JOIN " + forAnyEach.entityNameDb + " AS " + forAnyEach.entityNameAs + " ");
			}
			sbSqlFrom.append(" ON ");
			sqlJoinCondition = compoundJoinCondition(forAnyEachPreviousRelated, forAnyEach);
			sbSqlFrom.append(sqlJoinCondition);
			sbSqlFrom.insert(0, "(");
			sbSqlFrom.append(")");
		}
		
		// Sql Select completa fino a clausola FROM inclusa 
		sbSqlString.append(sbSqlFrom.toString());
		
		// Inizio composizione Where
		sbSqlWhere = new StringBuffer();
		
		// Se la view inizia con FOR_ANY() o FOR_EACH() la condizione sulla chiave primaria si
		// specifica come prima condizione di where.
		// Nel descrittore sono presenti, per le rule table, le colonne PK da NON considerare
		// Le column PK devono essere uguali ai valori correnti delle variabili corrispondenti
		if (this.al_forAnyEach.get(0).typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY
		||  this.al_forAnyEach.get(0).typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH
		||  this.al_forAnyEach.get(0).typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE
		||  this.al_forAnyEach.get(0).typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH_RULE_TABLE) {
			appendWhereConditionPK(this.al_forAnyEach.get(0), sbSqlWhere);
			and = " AND ";
		}
		
		// Accodamento condizioni di where generate per realizzare le LEFT e INNER JOIN di rule tables
		if (!this.strSqlWhereCondition.equals("")) {
			sbSqlWhere.append(and);
			sbSqlWhere.append(this.strSqlWhereCondition);
			and = " AND ";
		}
				
		// Accodamento eventuali condizioni di Where applicative supplementari, per ogni FOR_ANY() o FOR_EACH() dichiarata
		for (InnerForAnyEach forAnyEachLoop : this.al_forAnyEach) {
			if (!forAnyEachLoop.whereSql.equals("")) {
				sbSqlWhere.append(and);
				sbSqlWhere.append(getSqlStringWithHostVarActualized(forAnyEachLoop.whereSql));
				and = " AND ";
			} 
		}
		
		// Composizione finale Select sql
		
		// Where da inserire
		if (!sbSqlWhere.toString().trim().equals("")) {
			sbSqlString.append(" WHERE ");
			sbSqlString.append(sbSqlWhere);
		}
		
		// Order By da inserire
		if (!this.strSqlOrderByColumn.equals("")) {
			sbSqlString.append(" ORDER BY ");
			sbSqlString.append(this.strSqlOrderByColumn);
		}
		
		
		// Chiusura
		sbSqlString.append(";");
		
		// Impostazione variabile di istanza
		this.sqlSelectString = sbSqlString.toString();
		
		return retCode;
	}
	
	
	/* ---------------------------------------------------
	 * Accodamento condizione di Where sulla primary key
	 * ---------------------------------------------------
	 * 
	 * Vengono escluse le colonne PK dichiarate da escludere per l'entity.
	 * Tipicamente ciò serve quando una FOR_ANY() o FOR_EACH() è codificata come prima
	 * dichiarazione e NON tutti i campi PK sono significativi.
	 * Ciò vale sia per entities sia per rule tables.
	 * Tuttavia, per esigenze specifiche, potrebbe essere necessario escludere dei campi
	 * chiave anche in dichiarazioni successive alla prima.
	 */
	private void appendWhereConditionPK(InnerForAnyEach innerForAnyEach, StringBuffer sbSqlWhere) {

		InnerAsColumn innerAsColumn = null;
        String varName = "";
        String javaFieldName = "";
        String varValue = "";
        String and = "";
        int i = 0;
        boolean isPKColumnToDiscard = false;
        
		// Scan PK columns
		for (DataBaseItemDescriptor columnPK : innerForAnyEach.ar_primarKey) {
			
			// Nome colonna (java field name) o nome esposto come AS newName
			varName = columnPK.getAsName();
            if (varName.equals("")) {
            	varName = columnPK.getJavaFieldName();
			}
            
            // Se la colonna è esposta dalla logical data view, serve il nome della variabile java dell'entity bean
            innerAsColumn = this.hm_columnAsName.get(varName);
            javaFieldName = columnPK.getAsName();
            if (innerAsColumn != null) {
            	javaFieldName = innerAsColumn.columnNameJava;
			}	
            
			// Non devono essere considerate le PK richieste in FOR_ANY() e FOR_EACH()
			isPKColumnToDiscard = false;
			for (i = 0; i < innerForAnyEach.ar_primaryKeyExclude.length; i++) {
				if (javaFieldName.equals(innerForAnyEach.ar_primaryKeyExclude[i])) {
					isPKColumnToDiscard = true;
					break;
				}
			}
			
			// PK column da NON considerare
			if (isPKColumnToDiscard) {
				continue;
			}
			
			// Reperimento valore variabile e composizione condition
			varValue = getValueStringForVar(varName);
			sbSqlWhere.append(and);
			sbSqlWhere.append(innerForAnyEach.entityNameAs + ".");
			sbSqlWhere.append(columnPK.getDbColumn());
			sbSqlWhere.append(" = ");
			sbSqlWhere.append(varValue);
			and = " AND ";
		}
		
	}

	/*
	 * Restituisce la stringa con le variabili attualizzate con il loro valore corrente
	 * Le variabili sono in notazione host variable del tipo :hostVarName
	 */
	private String getSqlStringWithHostVarActualized(String sqlString) {
        StringBuffer sbStrOut = null;
        String varName = "";
        String varValue = "";
        int i;
        int iStart = 0;
         
        sbStrOut = new StringBuffer();
        
        // Scan condizione
        for (i = 0; i < sqlString.length(); i++) {
        	
           	// Sicuramente da portare in output
    		if (sqlString.charAt(i) != ':') {
    			sbStrOut.append(sqlString.charAt(i));
    			continue;
    		}
    		
        	// Inizia una variabile host
    		
			// Trova primo crt no space
			for (i = i+1; i < sqlString.length(); i++) {
				if (sqlString.charAt(i) != ' ') {break;}
			}
			
			// Fine: stringa errata terminata da : non seguiti da nome variabile, skip
			if (i >=  sqlString.length()) {
				break;
			}
			
			iStart = i;

			// Trova primo crt space o fine stringa
			for (; i < sqlString.length(); i++) {
				if (sqlString.charAt(i) != ' ') {break;}
			}
			
			// Fine: la variabile host era l'ultima
			if (i >= sqlString.length()) {
				varName = sqlString.substring(iStart).trim();
				if (isVarDeclared(varName)) {
					varValue = getValueStringForVar(varName);
					sbStrOut.append(" " + varValue);
					break;
				}
				// TODO Gestione variabile non dichiarata
				break;
			}
			
			varName = sqlString.substring(iStart, i);
			varValue = getValueStringForVar(varName);
			sbStrOut.append(" " + varValue + " ");

		}
		return sbStrOut.toString();
	}
	
	/*
	 * Restituisce il valore stringa della variabile.
	 * Nel caso di string la restituisce fra apici singoli.
	 */
	private String getValueStringForVar(String varName) {
		InnerVar innerVar = null;
		innerVar = getVar(varName);
		
		// Stringa fra apici
		if (innerVar.varType == String.class) {
			return "'" + getValueString(varName) + "'";
		}
		
		// Valore stringa degli altri tipi di dato
		
		return getValue(varName).toString();
	}

	/*  --------------------------------------------------------------------
	 *  Estrazione stringa con colonne da portare in output per l'entity.
	 *  --------------------------------------------------------------------
	 *  
	 *  Le colonne vengono portate in output solo se NON sono da escludere
	 *  Le colonne vengono qualificate con il nome AS dell'entity
	 *  
	 *  Nel caso di entity la forma è    : dbTableName.dbColName1 AS javaName, dbTableName.dbColName2, ... , ..
	 *  Nel caso di rule table la forma è: SUBSTR(TBDT.TBDTRDAT, pos, lng) AS javaName1 ... , ..
	 *  Di solito si ha                  : TBDT.TBDTRDAT AS TnnnDESC quando c'è solo il campo di descrizione
	 */
	private String extractColumnsWithAsClause(InnerForAnyEach forAnyEach) {
		
		InnerAsColumn innerAsColumn = null;
		StringBuffer sb_asColumn = null;
		String asColumnToExpose = "";
		String comma = "";
		String fieldNameDb = null;
		String fieldNameAs = null;
		
		sb_asColumn = new StringBuffer ();
		
		// Scan colonne da esporre
		for (String columnAsName : forAnyEach.al_columnAsName) {
			
			// Colonna da esporre, può anche essere una colonna PK
			innerAsColumn = this.hm_columnAsName.get(columnAsName);
			fieldNameDb = innerAsColumn.columnNameDb;
			fieldNameAs = innerAsColumn.columnNameAs;
			asColumnToExpose = comma + " " + forAnyEach.entityNameAs + "." + fieldNameDb + " AS " +  fieldNameAs;
			sb_asColumn.append(asColumnToExpose);
			comma = ", ";
		}
				
		return sb_asColumn.toString();
	}

	/* --------------------------------------------------------------------------------------
	 * Restituisce il decrittore FOR_ANY o FOR_EACH precedente con il quale c'è la relazione
	 * --------------------------------------------------------------------------------------
	 * 
	 * Viene fornito l'entry dell'entity di cui trovare l'entity relazionata con
	 * Restituisce l'entity in relazione con quella fornita.
	 * Se non si trova restituisce null ma NON deve succedere, questo metodo è attivabile
	 * solo dopo validate() che fa questi controlli
	 */
	private InnerForAnyEach getPreviousForAnyEachRelated(InnerForAnyEach forAnyEach) {
		InnerForAnyEach forAnyEachPrev = null;
		
		// Scan reverse le entities dichiarate
		for (int i = forAnyEach.indexEntry - 1; i >= 0; i--) {
			forAnyEachPrev = this.al_forAnyEach.get(i);
			if (forAnyEachPrev.entityName.equals(forAnyEach.entityNameRelatedWith)) {
				return forAnyEachPrev;
			}
		}
		return null;
	}

	/* ---------------------------------------------
	 * Composizione condizione di join nella forma:
	 * ---------------------------------------------
	 * 
	 *  	dbTableNamePrev.dbColumnPrev1 = dbTableName.dbColumn1
	 *      ....
	 * AND 	dbTableNamePrev.dbColumnPrevN = dbTableName.dbColumnN
	 */
	private String compoundJoinCondition(InnerForAnyEach forAnyEachPreviousRelated, InnerForAnyEach forAnyEach) {
		
		InnerUsableRuleTable innerUsableRuleTable = null;
		StringBuffer sb_onCondition = null;
		StringBuffer sb_whereCondition = null;
		ForwardColumnPair[] ar_columnPairToMatch = null;
		sb_onCondition = new StringBuffer ();
		String tabKeyColBound = "";
		String andString = "";
		String tabNumColJava = "";											// Numero
		String tabNumColDb = "";											// Numero
		String tabKeyJavaFieldNameBoundDb = "";
		Class<?> tabKeyJavaFieldClass = null;
		String ruleTableFirstKeyColToBoundDb = "";
		
		// Impostazione corretto array coppie di colonne di join
		if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY) {
			ar_columnPairToMatch = forAnyEachPreviousRelated.hm_forAny.get(forAnyEach.entityName);
		}
		if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH) {
			ar_columnPairToMatch = forAnyEachPreviousRelated.hm_forEach.get(forAnyEach.entityName);
		}
		
		// Relazione fra entity
		if ((forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY || forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH)
		&&  (forAnyEachPreviousRelated.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY || forAnyEachPreviousRelated.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_EACH)) {
			// Scan coppie di colonne in join
			for (ForwardColumnPair columnPair : ar_columnPairToMatch) {
				sb_onCondition.append(andString);
				sb_onCondition.append(forAnyEachPreviousRelated.entityNameAs);
				sb_onCondition.append(".");
				sb_onCondition.append(columnPair.getColumn().getDbColumn());
				sb_onCondition.append(" = ");
				sb_onCondition.append(forAnyEach.entityNameAs);
				sb_onCondition.append(".");
				sb_onCondition.append(columnPair.getColumnRelated().getDbColumn());
				andString = " AND ";
			}
			return sb_onCondition.toString();
		}
		
		// Relazione fra entity e rule table.
		// Le chiavi per accedere alla tabella sono codificate nel descrittore della FOR_ANY() della rule table.
		// Qui viene valorizzato il solo campo chiave (TBDTKVAL) che qualifica l'accesso alla tabella.
		// Il nome di questo campo chiave viene caricato nel descrittore al momento della dichiarazione.
		// Potrebbero essere presenti + campi chiave, al momento NON gestiti e nemmeno definiti.
		// Il campo chiave è quello presente in forAnyEachPreviousRelated che può essere numerico o alfanumerico.
		// Se alfanumerico viene caricato direttamente, se numerico (come una enumerazione) si carica il valore stringa numerica.
		// Devono inoltre essere inserite le condizioni di Where sui valori NON presenti quali: sistema , sottosistema, linguaggio e numero tabella.
		// Tali condizioni vengono accodate a fine query nella WHERE, qui vengono solo preparate.
		
		// Recupero descrittore rule table accedibile e quindi nome campo da usare come key per la rule table e campo contenente il numero di tabella.
		for (Entry<String, InnerUsableRuleTable> entryUsableRuleTable : forAnyEachPreviousRelated.hm_usableRuleTable.entrySet()) {
			innerUsableRuleTable = entryUsableRuleTable.getValue();
			if (!innerUsableRuleTable.tabId.equals(forAnyEach.ruleTableId)) {continue;}    	// Non è la tabella relazionata
			if (!innerUsableRuleTable.tabRef.equals(forAnyEach.ruleTableRef)) {continue;}   // Non è la tabella relazionata con il giusto campo
			tabKeyColBound = innerUsableRuleTable.tabKeyColsBound[0];						// Al momento solo una colonna key specificabile
			tabNumColJava = innerUsableRuleTable.tabNumCol;									// Colonna in entity contenente fisicamente il numero di rule table
//			for (DataBaseItemDescriptor PKItemDescriptor : forAnyEachPreviousRelated.ar_primarKey) {
//				if (PKItemDescriptor.getJavaFieldName().equals(tabNumColJava)) {
//					tabNumColDb = PKItemDescriptor.getDbColumn();
//					break;
//				}
//			}
// GPZ			
			break;
		}
		
		tabKeyJavaFieldNameBoundDb = "";
		tabNumColDb = "";
				
		// Recupero nome colonna db da usare come ultima parte di chiave di accesso alla tabella
		// Viene inoltre recuperato il nome della colonna contenente il numero tabella, per la JOIN ON condition
		for (DataBaseItemDescriptor notPKItemDescriptor : forAnyEachPreviousRelated.ar_notPrimarKey) {
			if (notPKItemDescriptor.getJavaFieldName().equals(tabKeyColBound.trim())) {
				tabKeyJavaFieldNameBoundDb = notPKItemDescriptor.getDbColumn();
				tabKeyJavaFieldClass = notPKItemDescriptor.getJavaFieldClass();
			}
			if (notPKItemDescriptor.getJavaFieldName().equals(tabNumColJava)) {
				tabNumColDb = notPKItemDescriptor.getDbColumn();
				break;
			}
		}
		
		// Il campo non è stato trovato: potrebbe essere fra i campi chiave
		if (tabKeyJavaFieldNameBoundDb.equals("")) {
			for (DataBaseItemDescriptor PKItemDescriptor : forAnyEachPreviousRelated.ar_primarKey) {
				if (PKItemDescriptor.getJavaFieldName().equals(tabKeyColBound.trim())) {
					tabKeyJavaFieldNameBoundDb = PKItemDescriptor.getDbColumn();
					tabKeyJavaFieldClass = PKItemDescriptor.getJavaFieldClass();
					break;
				}
			}
		}
		
		// Il campo non è stato trovato del tutto: errore nelle annotation dell'entity
		if (tabKeyJavaFieldNameBoundDb.equals("")) {
			// TODO segnalazione
		    lf.writeRow(EnumMessageType.ERROR_INPUT, "ET0207", new String[]{forwardSystem.getMonitorDesktop().mcb.function.getClass().getSimpleName()
		    		                                                       ,this.getClass().getSimpleName()
		    		                                                       ,forAnyEachPreviousRelated.entityName
		    		                                                       ,forAnyEach.ruleTableId
		    		                                                       ,tabKeyColBound
		    		                                                       }
		    															   , null);

			return null;
		}
		
		// Recupero il nome della colonna db dell'entity di gestione tabelle, ultimo campo chiave
		for (DataBaseItemDescriptor PKItemDescriptor : forAnyEach.ar_primarKey) {
			if (PKItemDescriptor.getJavaFieldName().equals(forAnyEach.ruleTableFirstKeyColToBound)) {
				ruleTableFirstKeyColToBoundDb = PKItemDescriptor.getDbColumn();
				break;
			}
		}
		
		// Composizione ON condition del campo chiave presente nell'entity e il campo con il numero tabella

		sb_onCondition.append(forAnyEach.entityNameAs + '.' + ruleTableFirstKeyColToBoundDb);
		sb_onCondition.append(" = ");

		// Il campo chiave è stringa: nessun intervento
		if (tabKeyJavaFieldClass == String.class) {
			sb_onCondition.append(forAnyEachPreviousRelated.entityNameAs);
			sb_onCondition.append(".");
			sb_onCondition.append(tabKeyJavaFieldNameBoundDb);
		} else 
		// Il campo chiave è convertibile in stringa: function standard di conversione sql
		if (tabKeyJavaFieldClass == Integer.class 
	    ||  tabKeyJavaFieldClass == int.class 
	    ||  tabKeyJavaFieldClass.getAnnotation(DataBaseMappedEnumeration.class) != null) {
			sb_onCondition.append("CSTR(");
			sb_onCondition.append(forAnyEachPreviousRelated.entityNameAs);
			sb_onCondition.append(".");
			sb_onCondition.append(tabKeyJavaFieldNameBoundDb);
			sb_onCondition.append(")");
		// Campo chiave non convertibile: segnalazione
		} else {
			//TODO
		}

		// Se non è stata dichiarata nessuna colonna dell'entity come contenente fisicamente il numero di rule table,
		// la condizione per il numero di tabella viene messa nella where, in fondo alla select e NON è efficiente
		// Se invece il campo è dichiarato ed esiste viene messo direttamente nell ON condition di LEFT JOIN (molto + efficiente)
		// Questa attività viene fatta in questo metodo, successivamente
		if (!tabNumColDb.equals("")) {
			sb_onCondition.append(" AND ");
			sb_onCondition.append(forAnyEachPreviousRelated.entityNameAs);
			sb_onCondition.append(".");
			sb_onCondition.append(tabNumColDb);
			sb_onCondition.append(" = ");
			sb_onCondition.append(forAnyEach.entityNameAs + ".TBDTTTAB");
			
		}
		
		// Composizione condizione di where sugli altri campi chiave tabella da accodare nella WHERE finale
		// Si tratta di systema, sottosistema, numero tabella, language
		sb_whereCondition = new StringBuffer();
		
		// Tabella di sistema condivisa NON applicativa: sistema e sottosistema valgono '*' fisso
		if (forAnyEach.ruleTableSystem) {
			sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTSYST");
			sb_whereCondition.append(" = ");
			sb_whereCondition.append("'*'");
			sb_whereCondition.append(" AND ");
			sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTSUBS");
			sb_whereCondition.append(" = ");
			sb_whereCondition.append("'*'");
			
		// Tabella APPLICATIVA: sistema e sottosistema devono essere quelli della tabella contenente la chiave
		} else {
			sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTSYST");
			sb_whereCondition.append(" = ");
			sb_whereCondition.append(forAnyEachPreviousRelated.entityNameAs + "." + forAnyEachPreviousRelated.getSystemColumnDb());
			sb_whereCondition.append(" AND ");
			sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTSUBS" );
			sb_whereCondition.append(" = ");
			sb_whereCondition.append(forAnyEachPreviousRelated.entityNameAs + "." + forAnyEachPreviousRelated.getSubSystemColumnDb());
		}
		
		// Colonna per numero tabella NON prevista nell'entity e non inseribile nella JOIN
		//  Si inserisce la condizione come normale condizione in WHERE
		if (tabNumColDb.equals("")) {
			sb_whereCondition.append(" AND ");
			sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTTTAB");
			sb_whereCondition.append(" = ");
			sb_whereCondition.append(forAnyEach.ruleTableNum);
		}
		
		// Linguaggio
		sb_whereCondition.append(" AND ");
		sb_whereCondition.append(forAnyEach.entityNameAs + ".TBDTLANG");
		sb_whereCondition.append(" = ");
		sb_whereCondition.append(this.getRuleTableLanguage().ordinal());
		
		
		// Accodamento Where generale
		if (this.strSqlWhereCondition.equals("")) {
			this.strSqlWhereCondition = sb_whereCondition.toString();
		} else {
			this.strSqlWhereCondition+= " AND " + sb_whereCondition.toString();
		}
		
		return sb_onCondition.toString();
	}



	/**
	 * The logical data view, declared and validated, will be executed.<br>
	 * <p>
	 * When the logical data view contains just only a FOR_SQL() declaration,<br>
	 * the user can code any type of sql statement select, specifying an AS clause,<br>
	 * for any column to be exposed to the application. The name exposed must be the
	 * name of an application variable or of a GUI control.<br>
	 * At the first execution, for each column declared, using JDBC, the descriptor will be<br>
	 * got to know the sql table owner and if the column is a primary key.<br>
	 * All of these informations let to  make available all of logical data view infrastructure,
	 * for custom sql select too.
	 * <p>
	 * In a custom sql statement coded in a FOR_SQL() directive, can be coded variables to be actualizated<br>
	 * runtime, before the execuution. These variables must be coded as <b>:varName</b> and must be specified<br>
	 * by a <code>VAR()</code> declaration. Application function can set the value of any variable.<br>
	 * <p>
	 * The original resultset originated by data base access interface, <br>
	 * will be used to load internal structures of the logical data view.<br>
	 * To navigate in the recordset use read() readFirst(), readLast() and readPrev() methods.<br>
	 * <p>
	 */
	public int execute() {
		
		ResultSet rs = null;
		ResultSetMetaData metaData = null;
		ArrayList<Object> al_columnObject = null;
		Object columnObject = null;
		EnumForwardLogicalDataViewControl retCode = null;
		int retCodeNum = 0;
		
		// Logical data view da validare: NON può essere eseguita senza validazione
		if (this.isValidateToDo()) {
			retCodeNum = validate();
			if (retCodeNum > 0) {return retCodeNum;}
		}
		
		// Impostazione numero tabella e linguaggio in variabile di colonna se accesso generico a rule table
		if (this.isReadSetRuleTable()) {
			this.setValue("numTable", this.getRuleTableNum());
			this.setValue("language", this.getRuleTableLanguage().ordinal());
		}
		
        // Generazione statement Sql
		retCode = createSql();					// -> this.sqlSelectString
		if (retCode.ordinal() > 0) {
			return retCode.ordinal();
		}

		
		// Esecuzione query e produzione  ResultSet
		try {
			dbs = new DataBaseStatusDetailed();
//			dbs.setTypeOperation(EnumDataBaseOperation.DB_EXEC_LDV_FORWARD);
			dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_OPERATION_OK);
			dbs.setSqlString(this.sqlSelectString);
			dbConn = dbm.getConnection(dbs);
			
			rs = dbm.execSqlGeneric(dbConn, this.sqlSelectString, dbs);
			metaData = rs.getMetaData();							// Serve per il conteggio colonne
			al_al_rowColumn = new ArrayList<ArrayList<Object>> ();
			
			// Scan valori trovati
			while (rs.next()) {
				
				al_columnObject = new ArrayList<Object> ();
				// Scan colonne definite
				for (int i = 1; i <= metaData.getColumnCount(); i++) {
					columnObject = rs.getObject(i);
					al_columnObject.add(columnObject);
				}
	            al_al_rowColumn.add(al_columnObject);
				
			} // end-while
			
			rs.close();
			
		} catch (SQLException e) {
			dbs.setExcpOrigin(e);
			dbs.setSqlStatus(e.getSQLState());
			dbs.setSqlErrorCode(e.getErrorCode());
			dbs.setWarningMessage(e.getLocalizedMessage());
			dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmritaSqlError e) {
			dbs.setExcpOrigin(e);
			dbs.setWarningMessage(e.getLocalizedMessage());
			dbs.setStatusOperation(EnumDataBaseOperationStatus.DB_ERROR_SQL);
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} finally  {
			dbm.releaseConnection(dbConn, new DataBaseStatusDetailed());
			dbConn = null;
		}
		
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	

	/**
	 * Makes available data of a specific row of the logical data view resultset.<br>
	 * <p>
	 * All column variables automatically defined at decaration time, will be updated.<br>
	 * Application can query the logical data view to get current column values by <br>
	 * <code>getValueXXX()</code> methods.<br>
	 * <p>
	 * If the row number is out of range, no action will be taken.
	 * 
	 * @param numRow the number of row 0-based to make avaliable
	 */
	public int read(int numRow) {
		if (numRow >= al_al_rowColumn.size()) {return EnumForwardLogicalDataViewControl.LDV_ERROR_INDEX_OUT_OF_BOUND.ordinal();}
		setColumnVariables(numRow);
		this.curRowNumber = numRow;
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	
	/**
	 * Makes available data of the first row of the logical data view resultset.<br>
	 */
	public int readFirst() {
		if (this.al_al_rowColumn.size() == 0) {
			return EnumForwardLogicalDataViewControl.LDV_EOF.ordinal();
		}
		this.curRowNumber = 0;
		setColumnVariables(this.curRowNumber);
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	
	/**
	 * Makes available data of the next row of the logical data view resultset.<br>
	 */
	public int readNext() {
		this.curRowNumber++;
		if (this.curRowNumber >= this.al_al_rowColumn.size()) {
			return EnumForwardLogicalDataViewControl.LDV_EOF.ordinal();
		}
		setColumnVariables(this.curRowNumber);
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	
	/**
	 * Makes available data of the previous row of the logical data view resultset.<br>
	 */
	public int readPrev() {
		if (this.curRowNumber == 0) {
			return EnumForwardLogicalDataViewControl.LDV_EOF.ordinal();
		}
		this.curRowNumber--;
		setColumnVariables(this.curRowNumber);
		return 0;
	}
	
	/**
	 * Makes available data of the last row of the logical data view resultset.<br>
	 */
	public int readLast() {
		if (this.al_al_rowColumn.size() == 0) {
			return EnumForwardLogicalDataViewControl.LDV_EOF.ordinal();
		}
		this.curRowNumber = this.al_al_rowColumn.size() - 1;
		setColumnVariables(this.curRowNumber);
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
	
	
	/**
	 * Gets the count of rows got by logical data view resultset.<br>
	 */
	public int getCountRows() {
		return this.al_al_rowColumn.size();
	}

	/**
	 * Erase all data rows stored in the logical data view.<br>
	 * No changes will be done to internal strictures and logical data view descriptors.
	 */
	public void clear() {
		this.al_al_rowColumn.clear();
		return;
	}

	/**
	 * Gets the count of rows by the execution of a SELECT COUNT(*) FROM select-statemnt<br>
	 * <p>
	 * An sql statement will be executed.<br>
	 * The sql select statement representing the logical data view must already have been created,<br>
	 * otherwise a 0 value will be returned<br>
	 * <p>
	 */
	public long getCountRowsBySql() {
		ResultSet rs = null;
		String sqlSelectCount = "";
		Object countObject = null;
		int i = 0;
		
		// Select sql non ancora generata
		if (this.sqlSelectString.equals("")) {
			return 0;
		}
		
		i = this.sqlSelectString.indexOf(" FROM ", 0);
		sqlSelectCount = "SELECT COUNT(*) " + this.sqlSelectString.substring(i);
		this.setErrorFound(false);
		
		// Esecuzione query e produzione  ResultSet
		try {
			dbConn = dbm.getConnection(dbs);
			dbs = new DataBaseStatusDetailed();
			rs = dbm.execSqlGeneric(dbConn, sqlSelectCount, dbs);
			rs.next();
			countObject = rs.getObject(1);
			rs.close();
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			if (countObject instanceof Integer) {
				return ((Integer)countObject).intValue();
			}
			if (countObject instanceof Long) {
				return ((Long)countObject).longValue();
			}
			return 0;
			
			
		} catch (ExceptionAmritaSqlError e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
			
		} catch (SQLException e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			this.setErrorFound(true);
			return 0;
		}	
	}

	/**
	 * Gets all columns exposed by the logical data view<br>
	 * <p>
	 * All columns, in the select statement generated, are coded with the AS clause.<br>
	 * The new name in the AS clause should be the java name of variables and GUI application controls.<br>
	 * <p>
	 */
	public ArrayList<String> getColumnNamesExposed() {
		ArrayList<String> al_columnAs = null;
		
		al_columnAs = new ArrayList<String> ();
		
		if (this.al_forAnyEach.size() == 1 
		&& this.al_forAnyEach.get(0).typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_SQL ) {
			return al_columnAs;
		}
		
		// Scan FOR_ANY() e FOR_EACH() definite
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			al_columnAs.addAll(forAnyEach.al_columnAsName);
		}
		
		return al_columnAs;
	}

	

	/**
	 * Updates all entities current row of the logical data view with changed values.<br>
	 * <p>
	 * Only entities declared by UPDATE() will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 */
	public int update() {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_UPDATE)) {continue;}							// Entity NON in UPDATE
			retCode = updateCommon(forInsUpdDel);
		}
		
		return retCode;
	}

	/**
	 * Updates the entity current row of the logical data view with changed values.<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityNameAs the name of the entity internal reference
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int update(String entityNameAs) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (!forInsUpdDel.entityNameAs.equals(entityNameAs)) {continue;}													// NON è l'entity cercata
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_UPDATE)) {continue;}							// Entity NON in UPDATE
			retCode = updateCommon(forInsUpdDel);
		}
		 
		return retCode;
	}

	/**
	 * Updates the entity current row of the logical data view with changed values.<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityClass the name of the entity bean class
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int update(Class<?> entityClass) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (forInsUpdDel.entityObject.getClass() != entityClass) {continue;}												// NON è l'entity che interessa
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_UPDATE)) {continue;}							// Entity NON in UPDATE
			retCode = updateCommon(forInsUpdDel);
		}
		
		return retCode;
	}

	
	/**
	 * Insert a row in the entity specified, on database, with the current value of column variables<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityNameAs the name of the entity internal reference
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int insert(String entityNameAs) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (!forInsUpdDel.entityNameAs.equals(entityNameAs)) {continue;}													// NON è l'entity cercata
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_INSERT)) {continue;}							// Entity NON in INSERT
			retCode = insertCommon(forInsUpdDel);
		}
		
		return retCode;
	}

	/**
	 * Insert a row in the entity specified, on database, with the current value of column variables<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityClass the name of the entity bean class
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int insert(Class<?> entityClass) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (forInsUpdDel.entityObject.getClass() != entityClass) {continue;}												// NON è l'entity che interessa
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_INSERT)) {continue;}							// Entity NON in INSERT
			retCode = insertCommon(forInsUpdDel);
		}
		
		return retCode;
	}

	/**
	 * Delete a row in the entity specified, on database, with the current value of primary key column variables<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityNameAs the name of the entity internal reference
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int delete(String entityNameAs) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (!forInsUpdDel.entityNameAs.equals(entityNameAs)) {continue;}													// NON è l'entity cercata
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_DELETE)) {continue;}							// Entity NON in DELETE
			retCode = deleteCommon(forInsUpdDel);
		}
		return retCode;
		
	}

	/**
	 * Delete a row in the entity specified, on database, with the current value of primary key column variables<br>
	 * <p>
	 * Only the entity input, if it's marked as updatable will be updated.<br>
	 * In this case will be considered all columns or only columns marked as updatable.<br>
	 * It will be read the object entity bean with keys identified by logical data view,<br>
	 * and any column value to be updated will be replaced with th new value, using standard<br>
	 * setXXX methods of the entity object bean.<br>
	 * At the end, the entity will be updated in CRUD modality, just as using the normal<br>
	 * {@link DataBaseEntityInterface} object to perform manually CRUD entitties operations. <br>
	 * 
	 * @param entityClass the name of the entity bean class
	 * @param the return code as an ordinal of {@link EnumForwardLogicalDataViewControl}.<br>
	 * LDV_OK is the ordinal zero value 
	 */
	public int delete(Class<?> entityClass) {
		int retCode = 0;
		
		// Scan entities
		for (InnerForAnyEach forInsUpdDel : al_forInsUpdDel) {
			if (forInsUpdDel.entityObject.getClass() != entityClass) {continue;}												// NON è l'entity che interessa
			if (!(forInsUpdDel.typeEntry == EnumForwardLogicalDataViewControl.LDV_DELETE)) {continue;}							// Entity NON in DELETE
			retCode = deleteCommon(forInsUpdDel);
		}
		return retCode;
		
	}

	/*
	 * -----------------------------------
	 * Parte comune update singola entity.
	 * -----------------------------------
	 * 
	 * - L'oggetto entity è stato istanziato in fase di declare UPDATE()
	 * - Viene letta in modalità CRUD la riga della tabella identificata dalla primary key dell'entity
	 * - Si aggiornano via reflection con setXXX i campi dell'entity, se il valore letto è diverso da quello
	 *   dell logical data view. Inoltre i campi vengono aggiornati se NON presenti nella lista di esclusione
	 * - Quindi si aggiorna l'entity in modalità CRUD
	 * 
	 * In questo modo vengono fisicamente aggiornati sempre tutti i campi dell'entity, eventualmente con i 
	 * nuovi valori impostati per la logical data view.
	 */
	private int updateCommon(InnerForAnyEach forAnyEach) {
		Object entityObject = null;
		int rcSetPk = 0;
		boolean isEntityRowFound = false;
		boolean isEntityRowUpdated = false;
		 
		entityObject = forAnyEach.entityObject;
		
		// Impostazione PK di accesso
		rcSetPk = setEntityFieldPrimaryKey(entityObject, forAnyEach);
		if (rcSetPk > 0) {
			return rcSetPk;
		}

    	try {
			dbs = new DataBaseStatusDetailed();
			if (isDbAutoConnection) {dbConn = dbm.getConnection(dbs);}
			if (this.dbei == null) {this.dbei = new DataBaseEntityInterface(sd, dbm);}
			dbei.setDbConn(dbConn);
			
			isEntityRowFound = dbei.read(entityObject);
			
			// Riga Non trovata con i valori chiave correnti: NOTFOUND
			if (!isEntityRowFound) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
				return EnumForwardLogicalDataViewControl.LDV_ERROR_NOTFOUND.ordinal();
			}
			
			// Impostazione campi NON PK e update fisico
			setEntityFieldNotPrimaryKey(entityObject, forAnyEach);
			
			// Updated fisico riga in modalità CRUD
			isEntityRowUpdated = dbei.update(entityObject);

			// Riga Non trovata con i valori chiave correnti: NOTFOUND
			if (!isEntityRowUpdated) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
				return EnumForwardLogicalDataViewControl.LDV_ERROR_NOTFOUND.ordinal();
			}

			// Update andato a buon fine
			if (isDbAutoCommit) {
				dbei.commit();
			}
			if (isDbAutoConnection) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
			}
			
		} catch (ExceptionAmritaSqlError e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (SQLException e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmrita e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		}
    	
    	// Riga trovata e aggiornata senza errori fisici
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();	
	}

	/*
	 * -----------------------------------
	 * Parte comune insert singola entity.
	 * -----------------------------------
	 * 
	 * - L'oggetto entity è stato istanziato in fase di declare INSERT()
	 * - Si aggiornano via reflection con setXXX i campi dell'entity
	 * - Quindi si inserisce l'entity in modalità CRUD
	 * 
	 */
	private int insertCommon(InnerForAnyEach forAnyEach) {
		Object entityObject = null;
		
		boolean isEntityRowInserted = false;
		 
		entityObject = forAnyEach.entityObject;
    	try {
			dbs = new DataBaseStatusDetailed();
			if (isDbAutoConnection) {dbConn = dbm.getConnection(dbs);}
			if (this.dbei == null) {this.dbei = new DataBaseEntityInterface(sd, dbm);}
			dbei.setDbConn(dbConn);
			
			// Aggiornamento colonne chiave e accesso fisico in lettura
			// Impostazione campi PK e non PK
			setEntityFieldPrimaryKey(entityObject, forAnyEach);
			setEntityFieldNotPrimaryKey(entityObject, forAnyEach);
			
			// Inserimento riga
			isEntityRowInserted = dbei.create(entityObject);
			
			// Riga Non trovata con i valori chiave correnti: NOTFOUND
			if (!isEntityRowInserted) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
				return EnumForwardLogicalDataViewControl.LDV_ERROR_DUPLICATE.ordinal();
			}
			
			// Insert andato a buon fine
			if (isDbAutoCommit) {
				dbei.commit();
			}
			if (isDbAutoConnection) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
			}
			
		} catch (ExceptionAmritaSqlError e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (SQLException e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmrita e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		}
    	
    	// Riga trovata e aggiornata senza errori fisici
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();	
	}

	/*
	 * -----------------------------------
	 * Parte comune update singola entity.
	 * -----------------------------------
	 * 
	 * - L'oggetto entity è stato istanziato in fase di declare DELETE()
	 * - Si aggiornano via reflection con setXXX i campi dell'entity 
	 * - Quindi si deleta l'entity in modalità CRUD
	 * 
	 * In questo modo vengono fisicamente aggiornati sempre tutti i campi dell'entity, eventualmente con i 
	 * nuovi valori impostati per la logical data view.
	 */
	private int deleteCommon(InnerForAnyEach forAnyEach) {
		Object entityObject = null;
		
		boolean isEntityDeleteOk = false;
		 
		entityObject = forAnyEach.entityObject;
    	try {
			dbs = new DataBaseStatusDetailed();
			if (isDbAutoConnection) {dbConn = dbm.getConnection(dbs);}
			if (this.dbei == null) {this.dbei = new DataBaseEntityInterface(sd, dbm);}
			dbei.setDbConn(dbConn);
			
			// Aggiornamento colonne chiave e accesso fisico in lettura
			// Impostazione PK di accesso
			setEntityFieldPrimaryKey(entityObject, forAnyEach);

			// Updated fisico riga in modalità CRUD
			isEntityDeleteOk = dbei.delete(entityObject);
			
			// Riga Non trovata con i valori chiave correnti: NOTFOUND
			if (!isEntityDeleteOk) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
				return EnumForwardLogicalDataViewControl.LDV_ERROR_NOTFOUND.ordinal();
			}
			
			// Delete andato a buon fine
			if (isDbAutoCommit) {
				dbei.commit();
			}
			if (isDbAutoConnection) {
				dbm.releaseConnection(dbConn, dbs);
				dbConn = null;
			}

		} catch (ExceptionAmritaSqlError e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (SQLException e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		} catch (ExceptionAmrita e) {
			dbm.releaseConnection(dbConn, dbs);
			dbConn = null;
			return EnumForwardLogicalDataViewControl.LDV_ERROR_DB_ACCESS.ordinal();
		}
    	
    	// Riga trovata e aggiornata senza errori fisici
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();	
	}

	
	/* ------------------------------------------
	 * Set campi primary key nell'entity bean
	 * ------------------------------------------
	 * 
	 * -Vengono aggiornati via reflection setXXX() i campi PK dell'entity brean utilizzando
	 * le variabili della logical data view.
	 * -Se le variabili NON sono mai state impostate, significa che la colonna NON interessa
	 *  e quindi noviene aggiornato il bean
	 */
    private int setEntityFieldPrimaryKey(Object entityObject, InnerForAnyEach forAnyEach) {
    	InnerVar innerVar = null;
    	int rcSet = 0;
    	
		// Scan campi PK
		for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
			innerVar = getVar(columnDescriptor.getAsName());									// La variabile deve esistere
			
			// Update campo via reflection
			rcSet = updateEntityObjectField(forAnyEach, innerVar, columnDescriptor.getJavaFieldName());	
			if (rcSet != EnumForwardLogicalDataViewControl.LDV_OK.ordinal()) {
				return rcSet;
			}
		}
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}
    
	/* ---------------------------------------------
	 * Set campi Non primary key nell'entity bean
	 * ---------------------------------------------
	 * 
	 * -Vengono aggiornati via reflection setXXX() i campi non PK dell'entity brean utilizzando
	 * le variabili della logical data view.
	 * -Se le variabili NON sono mai state impostate, significa che la colonna NON interessa
	 *  e quindi noviene aggiornato il bean
	 */
    private void setEntityFieldNotPrimaryKey(Object entityObject, InnerForAnyEach forAnyEach) {
    	InnerVar innerVar = null;
    	
		// Scan campi PK
		for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
			innerVar = getVar(columnDescriptor.getAsName());									// La variabile deve esistere
			
			// Update campo via reflection
			updateEntityObjectField(forAnyEach, innerVar, columnDescriptor.getJavaFieldName());	 
		}
		
	}

	/*
     * Aggiornamento via reflection dell'oggetto entityBean con il valore corrente della variabile colonna
     * Il nome della variabile potrebbe NON essere il nome del campo java dichiarato nell'entity a causa di AS clause.
     * In ogni caso sono stati memorizzati nell'entry FOR_ANY(), FOR_EACH(), UPDATE(), INSERT() e DELETE() 
     * tutte le colonne esposte per l'entity.
     * Se per le colonne è stata dichiarata una clausola AS newName, allora è memorizzato il newName, altrimenti  
     * è memorizzato il nome java originale definito nell'entity.
     */
	private int updateEntityObjectField(InnerForAnyEach forAnyEach, InnerVar innerVar, String fieldNameJava) {
		Object[] ar_statusInvoke = null;
		Object ar_ObjectInvoke[] = null;
		Class<?> ar_ObjectClassInvoke[] = null;
		Class<?> fieldClass = null;
		Object[] ar_enumValue = null;
		Field field = null;
		String methodName = "";
		int enumOrdinal = 0;
		
		// Composizine nome metodo ed esecuzione
		methodName = "set" + fieldNameJava.substring(0, 1).toUpperCase() + fieldNameJava.substring(1);
		
		// Invoke via reflection metodo setter setFieldName(value)
		ar_ObjectInvoke = new Object[1];;
		ar_ObjectClassInvoke = new Class[1];
		ar_ObjectClassInvoke[0] = innerVar.varObject.getClass();
		
		field = rm.getField(forAnyEach.entityObject, fieldNameJava);
		fieldClass = field.getType();
		ar_enumValue = fieldClass.getEnumConstants();

		if (ar_enumValue != null) {
			enumOrdinal = (Integer) innerVar.varObject;
			ar_ObjectClassInvoke[0] = fieldClass;
			ar_ObjectInvoke[0] = ar_enumValue[enumOrdinal];
		} else {
			ar_ObjectInvoke[0] = innerVar.varObject;
		}
		
		ar_statusInvoke = rm.invokeMethodWithStatus(forAnyEach.entityObject, methodName, ar_ObjectClassInvoke, ar_ObjectInvoke);
		if (ar_statusInvoke[0] != null) {
			return EnumForwardLogicalDataViewControl.LDV_ERROR_SET_ENTIITY_FIELD.ordinal();
		}
		return EnumForwardLogicalDataViewControl.LDV_OK.ordinal();
	}

	
	/**
	 * Gets the name of entities declared by FOR_ANY() and FOR_EACH().<br>
	 * The entity name is the name of the bean class for the entity.<br>
	 * <p>
	 * @return an array of entities names
	 */
	public String[] getEntityNames() {
        ArrayList<String> al_entityNames = null;
        String[] ar_entityNames = null;
        al_entityNames = new  ArrayList<String> ();
        
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			// Interessano solo le for-any e le foe-each
			if (forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_ANY 
			&&  forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_EACH) {
				continue;
			}
			
			al_entityNames.add(forAnyEach.entityName);
		}
		ar_entityNames = new String[al_entityNames.size()];
		ar_entityNames = al_entityNames.toArray(ar_entityNames);
		return ar_entityNames;
	}
	
	/**
	 * Gets the name of entities declared by FOR_ANY() and FOR_EACH().<br>
	 * The entity name is the internal AS name assigned to the entity.<br>
	 * <p>
	 * @return an array of entities names
	 */
	public String[] getEntityNamesAs() {
        ArrayList<String> al_entityNames = null;
        String[] ar_entityNames = null;
        al_entityNames = new  ArrayList<String> ();
        
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			// Interessano solo le for-any e le foe-each
			if (forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_ANY 
			&&  forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_EACH) {
				continue;
			}
			
			al_entityNames.add(forAnyEach.entityNameAs);
		}
		ar_entityNames = new String[al_entityNames.size()];
		ar_entityNames = al_entityNames.toArray(ar_entityNames);
		return ar_entityNames;
	}
	
	/**
	 * Gets the name of entities declared by INSERT(), UPDATE(), DELETE()<br>
	 * The entity name is the name of the bean class for the entity.<br>
	 * <p>
	 * @return an array of entities names
	 */
	public String[] getEntityNamesInsUpdDel() {
        ArrayList<String> al_entityNames = null;
        String[] ar_entityNames = null;
        al_entityNames = new  ArrayList<String> ();
        
        // Scan dichiarative
		for (InnerForAnyEach forInsUpdDel : this.al_forInsUpdDel) {
			al_entityNames.add(forInsUpdDel.entityName);
		}
		ar_entityNames = new String[al_entityNames.size()];
		ar_entityNames = al_entityNames.toArray(ar_entityNames);
		return ar_entityNames;
	}
	
	/**
	 * Gets the name of entities declared by INSERT(), UPDATE(), DELETE()<br>
	 * The entity name is the internal AS name assigned to the entity.<br>
	 * <p>
	 * @return an array of entities names
	 */
	public String[] getEntityNamesInsUpdDelAs() {
        ArrayList<String> al_entityNames = null;
        String[] ar_entityNames = null;
        al_entityNames = new  ArrayList<String> ();
        
        // Scan dichiarative
		for (InnerForAnyEach forInsUpdDel : this.al_forInsUpdDel) {
			al_entityNames.add(forInsUpdDel.entityNameAs);
		}
		ar_entityNames = new String[al_entityNames.size()];
		ar_entityNames = al_entityNames.toArray(ar_entityNames);
		return ar_entityNames;
	}
	
	/**
	 * Gets the numbers of rule tables declared by logical data view<br>
	 * <br>
	 * @return an array of rule table numers
	 */
	public Integer[] getRuleTableNames() {
		ArrayList<Integer> al_ruleTableNumber = null;
        Integer[] ar_ruleTableNumber = null;
        al_ruleTableNumber = new  ArrayList<Integer> ();
        
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}		// interessano solo le FOR_ANY() di tabelle di configurazione
			al_ruleTableNumber.add(forAnyEach.ruleTableNum);
		}
		ar_ruleTableNumber = new Integer[al_ruleTableNumber.size()];
		ar_ruleTableNumber = al_ruleTableNumber.toArray(ar_ruleTableNumber);
		return ar_ruleTableNumber;
	}
	
	/**
	 * Gets all defined fields names of all entities declared by logical data view.<br>
	 * <p>
	 * They will be returned all field names defined in the entity bean class, for each entity, <br>
	 * as coded by annotations. They're java field name.<br>
	 * <p>
	 * @return a String array with entities field names
	 */
	public String[] getEntitiesDefinedFieldNames() {
    	
		String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	al_entitiesFieldName = new ArrayList<String>();
     	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			
			// Scan colonne di primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
			
			// Scan colonne NON primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
		}
		
		// Comversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}
	

	/**
	 * Gets the Sql select generated by the succesfully validate() method call.<br>
	 * <p>
	 * @return the sql string
	 */
	public String getSql() {
		return this.sqlSelectString;
	}


	/**
	 * Gets all fields names visible to the application, of all entities declared by logical data view.<br>
	 * <p>
	 * They will be returned all field names defined in the entity bean class, for each entity, <br>
	 * as coded by annotations, that will be exposed to the application.<br>
	 * <br>
	 * @return a String array with entities field names
	 */
	public String[] getEntitiesAvailableFieldNames() {
    	
		String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	al_entitiesFieldName = new ArrayList<String>();
     	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			
			// Scan campi esposti disponibili per l'applicazione, con il nome eventualmente specificato da AS newName
			for (String columnAsName : forAnyEach.al_columnAsName) {
				al_entitiesFieldName.add(columnAsName);
			}
		}
		
		// Comversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}
	
	/**
	 * Gets all fields names of a specific entity defined by logical data view<br>
	 * <p>
	 * It's returned a list of java column names as defined by entity bean class definition.<br>
	 * When the logical data view entity declaration declare a column to be exposed with<br>
	 * the AS clasuse, the specified AS name will be returned instead.<br>
	 * <p>
	 * If the entity has been not declared an empty array will be returned.<br>
	 * <p>
	 * @param entityNameAs the name of the entity reference
	 * @return a String array with entity field names
	 */
	public String[] getEntityDefinedFieldNames(String entityNameAs) {
	   	
		String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	al_entitiesFieldName = new ArrayList<String>();
     	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			if (!entityNameAs.equals(forAnyEach.entityNameAs))									  {continue;}	// Interessa solo l'entry con l'entity in input
			
			// Scan colonne di primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
			
			// Scan colonne NON primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
			break;
		}
		
		// Comversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}
	
	/**
	 * Gets all primary key fields names of a specific entity declared by logical data view<br>
	 * <p>
	 * The entity can be declared by a FOR_ANY(), FOR_EACH(), INSERT(), UPDATE() or DELETE().<br>
	 * It's returned a list of java primary key column names as defined by entity bean class definition.<br>
	 * When the logical data view entity declaration declare a column to be exposed with<br>
	 * the AS clasuse, the specified AS name will be returned instead.<br>
	 * <p>
	 * If the entity has been not declared an empty array will be returned.<br>
	 * <p>
	 * @param entityNameAs the name of the entity reference
	 * @return a String array with entity field names
	 */
	public String[] getEntityPKFieldNames(String entityNameAs) {
	   	
		String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	boolean isEntityFound = false;

     	al_entitiesFieldName = new ArrayList<String>();

        // Scan dichiarative FOR_ANY() e FOR_EACH()
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			if (!entityNameAs.equals(forAnyEach.entityNameAs)) 	 	  							  {continue;}	// Interessa solo l'entry con l'entity in input
			
			isEntityFound = true;
			
			// Scan colonne di primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
		}
		
		// Entity non trovata in FOR_ANY() e FOR_EACH(), provo in eventuali dichiarative INSERT(), UPDATE() e DELETE()
		if (!isEntityFound) {
			for (InnerForAnyEach forInsUpdDel : this.al_forInsUpdDel) {
				if (!entityNameAs.equals(forInsUpdDel.entityNameAs))  {continue;}	// Interessa solo l'entry con l'entity in input
				
				// Scan colonne di primary key
				for (DataBaseItemDescriptor columnDescriptor : forInsUpdDel.ar_primarKey) {
					al_entitiesFieldName.add(columnDescriptor.getAsName());
				}
			}
		}
		
		// Comversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}

	/**
	 * Gets all primary key fields names of a specific entity declared by logical data view<br>
	 * <p>
	 * It's returned a list of java primary key column names as defined by entity bean class definition.<br>
	 * When the logical data view entity declaration declare a column to be exposed with<br>
	 * the AS clasuse, the specified AS name will be returned instead.<br>
	 * <p>
	 * If the index is out of bound, an empty array will be returned
	 * <p>
	 * @param index the index 0-based of the entity in the declaration sequence
	 * @return a String array with entity field names
	 */
	public String[] getEntityPKFieldNames(int index) {
		
		InnerForAnyEach forAnyEach = null;
	   	String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	
     	// Out of bound
     	if (index >= this.al_forAnyEach.size()) {
			return new String[0];
		}
     	
     	al_entitiesFieldName = new ArrayList<String>();
     	forAnyEach = this.al_forAnyEach.get(index);
     	
		// Scan colonne di primary key
		for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
			al_entitiesFieldName.add(columnDescriptor.getAsName());
		}
		
		// Conversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}

	/**
	 * Gets all not primary key fields names of a specific entity declared by logical data view<br>
	 * <p>
	 * It's returned a list of java primary key column names as defined by entity bean class definition.<br>
	 * When the logical data view entity declaration declare a column to be exposed with<br>
	 * the AS clasuse, the specified AS name will be returned instead.<br>
	 * <p>
	 * If the index is out of bound, an empty array will be returned
	 * <p>
	 * @param entityNameAs the name of the entity reference
	 * @return a String array with entity field names
	 */
	public String[] getEntityNotPKFieldNames(String entityNameAs) {
		
	   	String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
    	boolean isEntityFound = false;

     	al_entitiesFieldName = new ArrayList<String>();
     	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			if (!entityNameAs.equals(forAnyEach.entityNameAs)) 									  {continue;}	// Interessa solo l'entry con l'entity in input
			
			isEntityFound = true;
			
			// Scan colonne NON primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
			break;
		}
		
		// Entity non trovata in FOR_ANY() e FOR_EACH(), provo in eventuali dichiarative INSERT(), UPDATE() e DELETE()
		if (!isEntityFound) {
			for (InnerForAnyEach forInsUpdDel : this.al_forInsUpdDel) {
				if (!entityNameAs.equals(forInsUpdDel.entityNameAs))  {continue;}	// Interessa solo l'entry con l'entity in input
				
				// Scan colonne non primary key
				for (DataBaseItemDescriptor columnDescriptor : forInsUpdDel.ar_notPrimarKey) {
					al_entitiesFieldName.add(columnDescriptor.getAsName());
				}
			}
		}
		
		// Conversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}

	/**
	 * Gets all not primary key fields names of a specific entity declared by logical data view<br>
	 * <p>
	 * If the index is out of bound, an empty array will be returned
	 * <p>
	 * @param index the index 0-based of the entity in the declaration sequence
	 * @return a String array with entity field names
	 */
	public String[] getEntityNotPKFieldNames(int index) {
		InnerForAnyEach forAnyEach = null;
	   	String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	
     	// Out of bound
     	if (index >= this.al_forAnyEach.size()) {
			return new String[0];
		}
     	
     	al_entitiesFieldName = new ArrayList<String>();
     	forAnyEach = this.al_forAnyEach.get(index);
     	
		// Scan colonne di primary key
		for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
			al_entitiesFieldName.add(columnDescriptor.getAsName());
		}
		
		// Conversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}

	
	/**
	 * Gets Primary Key fields to exclude<br>
	 * <p>
	 * If the entity name has not been declared, no action will be taken and will be returned<br>
	 * an empty string array.<br>
	 * <p>
	 * @param entityNameAs the service name of the entity
	 * @return a String array with primary entity field names to exclude for the entity
	 */
	public String[] getPrimaryKeysToExclude(String entityNameAs) {
		
		// Scan declarations
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (!forAnyEach.entityNameAs.equals(entityNameAs)) {continue;}
			return forAnyEach.ar_primaryKeyExclude;
		}
		return new String[0];
	}
 
	 
	/**
	 * Sets Primary Key fields to exclude<br>
	 * <p>
	 * @param entityNameAs the service name of the entity
	 * @param primaryKeysExclude as a string array of primary key column names. When the FOR_ANY() is the first declared by<br>
	 * logical data view the columns to exclude will be not included in the the where condition of the sql select generated<br>
	 * A null value means no exclusion to do. A no null value can be coded to solve specific needs or with a not completely<br>
	 * relational database design.
	 */
	public void setPrimaryKeysToExclude(String entityNameAs, String[] primaryKeysExclude) {
		
		// Scan declarations
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			if (!forAnyEach.entityNameAs.equals(entityNameAs)) {continue;}
			
			// Impostazione colonne PK da escludere
			if (primaryKeysExclude == null) {
				forAnyEach.ar_primaryKeyExclude = new String[0];
				break;
			}
			forAnyEach.ar_primaryKeyExclude = primaryKeysExclude;
			this.setValidateToDo(true);
			break;
		}
		return;
	}
 
	
	/**
	 * Gets column names exposed to the application.<br>
	 * <p>
	 * @param entityNameAs the service name of the entity
	 * @return a String array with primary entity field names to expose
	 */
	public String[] getColumnNamesExposed(String entityNameAs) {
		
		String[] ar_columnAsName = null;
		
		// Scan declarations
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			if (!forAnyEach.entityNameAs.equals(entityNameAs)) {continue;}
		    
			// Impostazione colonne NOT PK da includere/escludere
			ar_columnAsName = new String[forAnyEach.al_columnAsName.size()];
			ar_columnAsName = forAnyEach.al_columnAsName.toArray(ar_columnAsName);
			return ar_columnAsName;
		}
		return null;
	}
 
	   
	/**
	 * Sets column names to expose to the application.<br>
	 * <p>
	 * @param entityNameAs the service name of the entity
	 * @param areIncludedAllFields a boolean true means all columns included getting entity data, false only those declared in input
	 * @param fieldsToIncludeExclude a string array of column names (as java field names). When all columns are included lists the column names to exclude<br>
	 *                     whereas not all columns are included lists the columns names to include<br>
	 *                     The string array should be not empty when colsIncludedExcluded is set to false.<br>
	 */
	public void setColumnsToExpose(String entityNameAs, boolean areIncludedAllFields, String ... fieldsToIncludeExclude) {

		// Scan declarations
		for (InnerForAnyEach innerForAnyEach : this.al_forAnyEach) {
			
			if (!innerForAnyEach.entityNameAs.equals(entityNameAs)) {continue;}
			
		    // Impostazione colonne NOT PK da includere/escludere
			innerForAnyEach.areIncludedAllFields = areIncludedAllFields;
			for (String fieldToIncludeExclude : fieldsToIncludeExclude) {
				innerForAnyEach.al_includedExcludedField.add(fieldToIncludeExclude);
			}
			this.setValidateToDo(true);
			break;
		}
		return;
	}
 
	 

	/**
	 * Gets all available fields names of a specific entity declared by logical data view<br>
	 * <p>
	 * @param entityNameAs the name of the entity reference
	 * @return a String array with entity field names
	 */
	public String[] getEntityAvailableFieldNames(String entityNameAs) {
		
	   	String[] ar_entitiesFieldName = null;
      	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			if (!entityNameAs.equals(forAnyEach.entityNameAs)) {continue;}										// Interessa solo l'entry con l'entity in input
			
			// Conversione in array  campi esposti disponibili per l'applicazione, con il nome eventualmente specificato da AS newName
			ar_entitiesFieldName = new String[forAnyEach.al_columnAsName.size()];
			ar_entitiesFieldName = forAnyEach.al_columnAsName.toArray(ar_entitiesFieldName);

			break;
		}
		return ar_entitiesFieldName;
	}
	
	/**
	 * Gets the fields names of the rule table declared by logical data view<br>
	 */
	public String[] getRuleTableFieldNames(String ruleTableNameAs) {
		
    	String[] ar_entitiesFieldName = null;
    	ArrayList<String> al_entitiesFieldName = null;
     	al_entitiesFieldName = new ArrayList<String>();
     	
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			
			// Interessano solo le FOR_ANY() di tabelle di dominio
			if (forAnyEach.typeEntry != EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}
			
			
			
			// Scan colonne di primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_primarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
			// Scan colonne NON primary key
			for (DataBaseItemDescriptor columnDescriptor : forAnyEach.ar_notPrimarKey) {
				al_entitiesFieldName.add(columnDescriptor.getAsName());
			}
		}
		
		// Conversione in array
		ar_entitiesFieldName = new String[al_entitiesFieldName.size()];
		ar_entitiesFieldName = al_entitiesFieldName.toArray(ar_entitiesFieldName);
		return ar_entitiesFieldName;
	}
	
	/**
	 * Gets the entity bean object that manages the entity name, used by the logical data view<br>
	 * <p>
	 * The returned object is loaded with data of the current logical data view row,<br>
	 * and the application must casts it to the correct variable type.<br>
	 * <p>
	 * If the entity name is a wrong name, a null value will be returned.
	 * <p>
	 * @param entityName the entity name of which the bean class is required
	 * @return the entity bean object
	 */
	public Object getEntityObject(String entityName) {
		
        // Scan dichiarative
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {continue;}	// NON interessano le FOR_ANY() di tabelle di dominio
			if (!entityName.equals(forAnyEach.entityName)) {continue;}	
			return forAnyEach.entityObject;
		}
		return null;
	}
	
	/**
	 * Gets the type of the logical data view entry<br>
	 * <p>
	 * Valid values for the enumeration {@link EnumForwardLogicalDataViewControl} returned are:<pre>
		LDV_FOR_ANY 
		LDV_FOR_ANY_RULE_TABLE 
		LDV_FOR_EACH 
		LDV_FOR_SQL 
	  </pre>
	  If the input index is out of range <code>LDV_RETURN_INDEX_OUT_OF_BOUND</code> will be returned
	 * @param index the index 0-based of the entry
	 * @return the entity bean object
	 */
	public EnumForwardLogicalDataViewControl getEntryType(int index) {
		
		if (index >= this.al_forAnyEach.size()) {
			return EnumForwardLogicalDataViewControl.LDV_ERROR_INDEX_OUT_OF_BOUND;
		}
		return this.al_forAnyEach.get(index).typeEntry;
	}
	
	/**
	 * Gets the number of read declaratives FOR_ANY/EACH in the logical data view<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @return the number of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public int getForAnyEachCount() {
		return this.al_forAnyEach.size();
	}
	
	/**
	 * Gets the internal descriptor of a FOR_ANY/EACH declaratives<br>
	 * <p>
	 * If the input index is out of range a null value will be returned.<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @return the internal descriptor of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public InnerForAnyEach getForAnyEach(int index) {
		InnerForAnyEach innerForAnyEach = null;
		if (index >= getForAnyEachCount() - 1) {return null;}
		innerForAnyEach = this.al_forAnyEach.get(index);
		return innerForAnyEach;
	}
	
	/**
	 * Gets the internal descriptor of a FOR_ANY/EACH declaratives<br>
	 * <p>
	 * If the the entry is not found a null value will be returned.<br>
	 * <p>
	 * @param entityNameAs the internal name identifier of the entry
	 * @return the internal descriptor of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public InnerForAnyEach getForAnyEach(String entityNameAs) {
		
		// Scan entries in lettura
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.entityNameAs.equals(entityNameAs)) {
				return forAnyEach;
			}
		}
		
		// Non trovato, verifica se entry di update
		for (InnerForAnyEach forInsUpdDel : this.al_forInsUpdDel) {
			if (forInsUpdDel.entityNameAs.equals(entityNameAs)) {
				return forInsUpdDel;
			}
		}
		
		// Entry non trovata in struttura per lettura e in quella di aggiornamento
		return null;
	}
	
	/**
	 * Sets the Sql where condition of a FOR_ANY/EACH declarative<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can dynamically invoke this <br>
	 * method to set it.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the declaration index is wrong, no action will be taken.<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @param whereSql the sql where condition
	 * @return the internal descriptor of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public InnerForAnyEach setWhereSql(int index, String whereSql) {
		InnerForAnyEach innerForAnyEach = null;
		innerForAnyEach = getForAnyEach(index);
		if (innerForAnyEach == null) {return null;}
		innerForAnyEach.whereSql = whereSql;
		return innerForAnyEach;
	}
	
	/**
	 * Sets the Sql where condition of a FOR_ANY/EACH declarative<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can dynamically invoke this <br>
	 * method to set it.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the declaration entity identifier name is wrong, no action will be taken.<br>
	 * <p>
	 * @param entityNameAs the internal name identifier of the entry
	 * @param whereSql the sql where condition
	 * @return the internal descriptor of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public InnerForAnyEach setWhereSql(String entityNameAs, String whereSql) {
		InnerForAnyEach innerForAnyEach = null;
		innerForAnyEach = getForAnyEach(entityNameAs);
		if (innerForAnyEach == null) {return null;}
		innerForAnyEach.whereSql = whereSql;
		return innerForAnyEach;
	}
	

	/**
	 * Gets the internal descriptor of a FOR_ANY/EACH declarative<br>
	 * <p>
	 * If the input index is out of range a null value will be returned.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @return an ArraList of declaratives FOR_ANY(), FOR_EACH() or FOR_ANY_RULE_TABLE()
	 */
	public ArrayList<InnerForAnyEach> getForAnyEachAll() {
		return this.al_forAnyEach;
	}
	
	/**
	 * Gets the number of INSERT/UPDATE/DELETE declaratives in the logical data view<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @return the number of declaratives INSERT(), UPDATE() or DELETE()
	 */
	public int getForInsUpdDelCount() {
		return this.al_forAnyEach.size();
	}
	
	/**
	 * Gets the internal descriptor of a INSERT/UPDATE/DELETE declaratives<br>
	 * <p>
	 * If the input index is out of range a null value will be returned.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @return the internal descriptor of declaratives INSERT(), UPDATE() or DELETE()
	 */
	public InnerForAnyEach getForInsUpdDel(int index) {
		InnerForAnyEach innerForAnyEach = null;
		if (index >= getForAnyEachCount() - 1) {return null;}
		innerForAnyEach = this.al_forInsUpdDel.get(index);
		return innerForAnyEach;
	}
	
	/**
	 * Gets the all {@link DataBaseItemDescriptor} objects describing the column java and sql informations<br>
	 * <p>
	 * <p>
	 * @return an ArrayList of {@link DataBaseItemDescriptor} object with all informations
	 */
	public ArrayList<DataBaseItemDescriptor> getColumnDescriptors() {
		ArrayList<DataBaseItemDescriptor> al_wrkDataBaseItemDescriptor = null;
		ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
		
		al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
		
		for (InnerForAnyEach innerForAnyEach : this.al_forAnyEach) {
			al_wrkDataBaseItemDescriptor = innerForAnyEach.getDataBaseItemDescriptors();
			al_dataBaseItemDescriptor.addAll(al_wrkDataBaseItemDescriptor);
		}
		return al_dataBaseItemDescriptor;
	}
	
	/**
	 * Gets the all {@link DataBaseItemDescriptor} objects describing the primary key column java and sql informations<br>
	 * <p>
	 * @return an ArrayList of {@link DataBaseItemDescriptor} object with all informations
	 */
	public DataBaseItemDescriptor getColumnDescriptorsPK() {
		ArrayList<DataBaseItemDescriptor> al_wrkDataBaseItemDescriptor = null;
		ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
		DataBaseItemDescriptor dataBaseItemDescriptor = null;
		
		al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
		
		for (InnerForAnyEach innerForAnyEach : this.al_forAnyEach) {
			al_wrkDataBaseItemDescriptor = innerForAnyEach.getPkDataBaseItemDescriptors();
			al_dataBaseItemDescriptor.addAll(al_wrkDataBaseItemDescriptor);
		}
		return dataBaseItemDescriptor;
	}

	/**
	 * Gets the all {@link DataBaseItemDescriptor} objects describing the not primary key column java and sql informations<br>
	 * <p>
	 * @return an ArrayList of {@link DataBaseItemDescriptor} object with all informations
	 */
	public DataBaseItemDescriptor getColumnDescriptorsNotPK() {
		ArrayList<DataBaseItemDescriptor> al_wrkDataBaseItemDescriptor = null;
		ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
		DataBaseItemDescriptor dataBaseItemDescriptor = null;
		
		al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
		
		for (InnerForAnyEach innerForAnyEach : this.al_forAnyEach) {
			al_wrkDataBaseItemDescriptor = innerForAnyEach.getNotPkDataBaseItemDescriptors();
			al_dataBaseItemDescriptor.addAll(al_wrkDataBaseItemDescriptor);
		}
		return dataBaseItemDescriptor;
	}

	/**
	 * Gets the {@link DataBaseItemDescriptor} describing the column java and sql informations<br>
	 * <p>
	 * It's required the column name with the name exposed by logical data view.<p>
	 * <br>
	 * If the column name is not declared, a null value will be returned.<br>
	 * <p>
	 * @param columnNameAs the column name as exposed by logical data view
	 * @return the {@link DataBaseItemDescriptor} object with all informations
	 */
	public DataBaseItemDescriptor getColumnDescriptor(String columnNameAs) {
		DataBaseItemDescriptor dataBaseItemDescriptor = null;
		InnerAsColumn innerColumn = null;
		
		innerColumn = this.hm_columnAsName.get(columnNameAs);
		if (innerColumn == null) {return null;}
		
		for (DataBaseItemDescriptor dataBaseItemDescriptorLoop : getColumnDescriptors()) {
			if (!dataBaseItemDescriptorLoop.getJavaFieldName().equals(innerColumn.columnNameJava)) {continue;}
			dataBaseItemDescriptor = dataBaseItemDescriptorLoop;
			break;
		}
		return dataBaseItemDescriptor;
	}
	
	/**
	 * Gets the internal descriptor of a INSERT/UPDATE/DELETE declarative<br>
	 * <p>
	 * If the input index is out of range a null value will be returned.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @param index the index 0-based of the entry
	 * @returnan ArraList of declarative INSERT(), UPDATE() or DELETE()
	 */
	public ArrayList<InnerForAnyEach> getForInsUpdDelAll() {
		return this.al_forInsUpdDel;
	}
	
	
	/**
	 * Returns true if the variable is declared in the variables system of the logical data view.<br>
	 * <p>
	 * If the variable is not declared returns false.<br>
	 * <p>
	 * @param varName
	 * @return true if the variable has been declared, false if not
	 */
	public boolean isVarDeclared(String varName) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return false;}
		return true;
	}


	/**
	 * Gets the rule table number to be accessed by logical data view.<br>
	 * <p>
	 * The logical data view must access to the forward generalized rule table system and so<br>
	 * must declare only a <code>FOR_EACH()</code> of a rule table.<br>
	 * <p>
	 * If the logical data view is not for a rule table a -1 value will be returned<br>
	 * <p>
	 * <p>
	 * @return the rul table number
	 */
	public int getRuleTableNum() {
		InnerForAnyEach forAnyEach = null;
		if (!this.isReadSetRuleTable) {return -1;}
		forAnyEach = this.al_forAnyEach.get(0);
		return forAnyEach.ruleTableNum;
	}
    
	/**
	 * Sets the rule table number to be accessed by logical data view.<br>
	 * <p>
	 * The logical data view must access to the forward generalized rule table system and so<br>
	 * must declare only a <code>FOR_EACH()</code> or a <code>FOR_ANY()</code> of a rule table.<br>
	 * <p>
	 * If the logical data view is not for a rule table no action will be taken.<br>
	 * <p>
	 * <p>
	 * @param ruleTableNum the rule table number to be set
	 */
	public void setRuleTableNum(int ruleTableNum) {
		InnerForAnyEach forAnyEach = null;
		forAnyEach = this.al_forAnyEach.get(0);
		forAnyEach.ruleTableNum = ruleTableNum;
		return;
	}
    
	/**
	 * Return the current value of the variable in the logical data view.
	 * <br>
	 * A variabile can be declared by a <code>VAR()</code> directive or to be<br>
	 * automatically declared for each column of the laogical data view.<br>
	 * <p>
	 * This method returns the object containing the value of the variable to be casted invoking it.<br>
	 * <p>
	 * If the variable doesn't exist, a null value will be returned.<br>
	 * <p>
	 * @param varName
	 * @return the object var value
	 */
	public Object getValue(String varName) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return null;}
		return innerVar.varObject;
	}
    
	/**
	 * Set a variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable input value type is not as the defined type, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the object var value
	 */
	public void  setValue(String varName, Object varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (varValue != null && innerVar.varType != varValue.getClass()) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}

	/**
	 * Sets a String variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a String variable, no action will be taken.
	 * if the input variable value is not a String variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the String object var value
	 */
	public void  setValueString(String varName, String varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != String.class) {return;}
		if (varValue.getClass() != String.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}


	/**
	 * Return the current String value of the variable in the logical data view.<br>
	 * <p>
	 * If the variable is not defined or it's defined of a different type, <br>
	 * returns an initialized empty string.<br>
	 * <p>
	 * @param varName
	 * @return the String value
	 */
	public String getValueString(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return "";}
		if (!(objVar instanceof String)) {return "";}
		return (String)objVar ;
	}

	/**
	 * Return the current boolean value of the variable in the logical data view.<br>
	 * <p>
	 * If the variable is not defined or if is defined  of a different type, <br>
	 * returns a false initialized boolean value.<br>
	 * <p>
	 * @param varName
	 * @return the boolean value
	 */
	public boolean getValueBoolean(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return (new Boolean(false)).booleanValue();}
		if (!(objVar instanceof Boolean)) {return new Boolean(false).booleanValue();}
		return ((Boolean)objVar).booleanValue() ;
	}

	/**
	 * Sets a Boolean variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a Boolean variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the boolean object var value
	 */
	public void  setValueBoolean(String varName, boolean varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != Boolean.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}


	/**
	 * Return the current int value of the variable in the logical data view.<br>
	 * <p>
	 * If the variable is not defined it's defined of a different type, <br>
	 * returns a 0 initialized integer value.<br>
	 * <p>
	 * @param varName
	 * @return the int value
	 */
	public int getValueInt(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return (new Integer(0)).intValue();}
		if (!(objVar instanceof Integer)) {return new Integer(0).intValue();}
		return ((Integer)objVar).intValue() ;
	}

	/**
	 * Sets a Integer variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a Integer variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the integer object var value
	 */
	public void  setValueInt(String varName, int varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != Integer.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}


	/**
	 * Return the current float value of the variable in the logical data view.<br>
	 * <br>
	 * If the variable is not defined or it's defined of a different type, <br>
	 * returns a 0.0f initialized float value.<br>
	 * <p>
	 * @param varName
	 * @return the float value
	 */
	public float getValueFloat(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return (new Float(0)).floatValue();}
		if (!(objVar instanceof Float)) {return new Float(0).floatValue();}
		return ((Float)objVar).floatValue() ;
	}

	/**
	 * Sets a Float variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a Float variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the float object var value
	 */
	public void  setValueFloat(String varName, float varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != Float.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}

	/**
	 * Return the current double value of the variable in the logical data view.<br>
	 * <br>
	 * If the variable is not defined or it's defined of a different type, <br>
	 * returns a 0.0d initialized float value.<br>
	 * <p>
	 * @param varName
	 * @return the double value
	 */
	public Double getValueDouble(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return new Double(0).doubleValue();}
		if (!(objVar instanceof Double)) {return (new Double(0)).doubleValue();}
		return ((Double)objVar).doubleValue() ;
	}

	/**
	 * Sets a Double variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a Double variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the double object var value
	 */
	public void  setValueDouble(String varName, double varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != Double.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}

	
	/**
	 * Return the current date value of the variable in the logical data view.<br>
	 * <br>
	 * If the variable is not defined or it's defined of a different type, <br>
	 * returns current date initialized date value.<br>
	 * <p>
	 * @param varName
	 * @return the Date object
	 */
	public Date getValueDate(String varName) {
		Object objVar = null;
		objVar = getValue(varName);
		if (objVar == null) {return new Date();}
		if (!(objVar instanceof Date)) {return new Date();}
		return (Date)objVar ;
	}

	
	/**
	 * Sets a Date variable value, in the logical data view.
	 * <br>
	 * If the variable doesn't exist  no action will be taken.<br>
	 * if the variable is not a Date variable, no action will be taken.
	 * <p>
	 * @param varName
	 * @param varValue the double object var value
	 */
	public void  setValueDate(String varName, Date varValue) {
		InnerVar innerVar = null;
		innerVar = this.hm_var.get(varName);
		if (innerVar == null) {return;}
		if (innerVar.varType != Date.class) {return;}
		innerVar.varObjectPrev = innerVar.varObject;
		innerVar.varObject = varValue;
		innerVar.isAnySetDone = true;
		return;
	}

	

	
	
	/* =========================================================================
	 * Metodi privati
	 * =========================================================================
	 * 
	 */
	private void initial() {
		this.rm = new ReflectionManager();
	}

	/*
	 * Attivazione analizzatore sql per analisi statemet sql select
	 */
	private int analyzeSqlSelect(InnerForAnyEach forAnyEach) {
		// TODO Auto-generated method stub
		return 0;
	}


	/*
	 * Creazione variabile in base al tipo colonna
	 */
	private void createVar(DataBaseItemDescriptor descriptorColumn) {
		
		String varName = "";
		
		// Variabile già creata probabilmente a fronte di VAR() di campo chiave O da precedente tabella correlata
		varName = descriptorColumn.getAsName();
		if (isVarDeclared(varName)) {
			return;
		}
		
		// String
		if (descriptorColumn.getJavaFieldClass() == String.class) {
			VAR(varName, new String(""));
			return;
		}
		
		// Byte
		if (descriptorColumn.getJavaFieldClass() == Byte.class) {
			VAR(varName, new Byte(""));
			return;
		}
		
		// Char
		if (descriptorColumn.getJavaFieldClass() == Character.class || descriptorColumn.getJavaFieldClass() == char.class) {
			VAR(varName, new Character(' '));
			return;
		}
		
		// Boolean
		if (descriptorColumn.getJavaFieldClass() == Boolean.class || descriptorColumn.getJavaFieldClass() == boolean.class) {
			VAR(varName, new Boolean(false));
			return;
		}
		
		// Integer o Enumeration 
		if (descriptorColumn.getJavaFieldClass() == Integer.class || descriptorColumn.getJavaFieldClass() == int.class || descriptorColumn.isJavaEnumeration()) {
			VAR(varName, new Integer(0));
			return;
		}
		
		// Long
		if (descriptorColumn.getJavaFieldClass() == Long.class || descriptorColumn.getJavaFieldClass() == long.class) {
			VAR(varName, new Long(0));
			return;
		}
		
		// Double
		if (descriptorColumn.getJavaFieldClass() == Double.class || descriptorColumn.getJavaFieldClass() == double.class) {
			VAR(varName, new Double(0.0d));
			return;
		}
		
		// Float
		if (descriptorColumn.getJavaFieldClass() == Float.class || descriptorColumn.getJavaFieldClass() == float.class) {
			VAR(varName, new Float(0.0f));
			return;
		}
		
		// Date
		if (descriptorColumn.getJavaFieldClass() == Date.class) {
			VAR(descriptorColumn.getAsName(), new Date());
			return;
		}
		
	}

	/*
	 * Aggiorna le variabili correnti di colonna della logical data view con i valori della riga specificata in input
	 */
	private void setColumnVariables(int numRow) {
		InnerVar innerVar = null;
		ArrayList<Object> al_rowColumn = null;
		String columnName = "";
		Object columnObject = null;
		int iColomun = 0;
		
		al_rowColumn = this.al_al_rowColumn.get(numRow);
		
		// Scan nomi colonne logical data view esposti
		for (Entry<String, InnerAsColumn> entryColumnAsName : this.hm_columnAsName.entrySet()) {
			columnName = entryColumnAsName.getKey();   							// AS columnName
			iColomun = entryColumnAsName.getValue().columnIndex;
			columnObject = al_rowColumn.get(iColomun);							// Integer, Boolean, ..., object
			
			// Se valore colonna è null imposto al valore di default
			if (columnObject == null) {
				columnObject = getDefaultInitialColumnValue(entryColumnAsName.getValue());
			}
			
			// Update variabile con nuovo valore
			innerVar = this.hm_var.get(columnName);
			innerVar.varObjectPrev = innerVar.varObject;
			innerVar.varObject = columnObject;
		}
		
	}

	/*
	 * Restituisce il valore di default del tipo di colonna
	 */
	private Object getDefaultInitialColumnValue(InnerAsColumn innerColumn) {
		InnerForAnyEach forAnyEach = null;
		DataBaseItemDescriptor descriptorColumnFound = null;
		
		forAnyEach = this.al_forAnyEach.get(innerColumn.entityOwnerIndex);
		
		// Scan chiave primaria
		for (DataBaseItemDescriptor descriptorColumn : forAnyEach.ar_primarKey) {
			if (!descriptorColumn.getAsName().equals(innerColumn.columnNameAs)) {continue;}
			descriptorColumnFound = descriptorColumn;
			break;
		}
		
		// Non è una colonna di PK
		if (descriptorColumnFound == null) {
			// Scan chiave primaria
			for (DataBaseItemDescriptor descriptorColumn : forAnyEach.ar_notPrimarKey) {
				if (!descriptorColumn.getAsName().equals(innerColumn.columnNameAs)) {continue;}
				descriptorColumnFound = descriptorColumn;
				break;
			}
		}
		
		// Non trovato: anomalia
		if (descriptorColumnFound == null) {return null;};
		
		// Individuo il default
		
		// String
		if (descriptorColumnFound.getJavaFieldClass() == String.class) {
			return new String("");
		}
		
		// Byte
		if (descriptorColumnFound.getJavaFieldClass() == Byte.class || descriptorColumnFound.getJavaFieldClass() == byte.class) {
			return new Byte("");
		}
		
		// Char
		if (descriptorColumnFound.getJavaFieldClass() == Character.class || descriptorColumnFound.getJavaFieldClass() == char.class) {
			return new Character(' ');
		}
		
		// Boolean
		if (descriptorColumnFound.getJavaFieldClass() == Boolean.class || descriptorColumnFound.getJavaFieldClass() == boolean.class) {
			return new Boolean(false);
		}
		
		// Integer o Enumeration 
		if (descriptorColumnFound.getJavaFieldClass() == Integer.class || descriptorColumnFound.getJavaFieldClass() == int.class || descriptorColumnFound.isJavaEnumeration()) {
			return new Integer(0);
		}
		
		// Long
		if (descriptorColumnFound.getJavaFieldClass() == Long.class || descriptorColumnFound.getJavaFieldClass() == long.class) {
			return new Long(0);
		}
		
		// Double
		if (descriptorColumnFound.getJavaFieldClass() == Double.class || descriptorColumnFound.getJavaFieldClass() == double.class) {
			return new Double(0.0d);
		}
		
		// Float
		if (descriptorColumnFound.getJavaFieldClass() == Float.class || descriptorColumnFound.getJavaFieldClass() == float.class) {
			return new Float(0.0f);
		}
		
		// Date
		if (descriptorColumnFound.getJavaFieldClass() == Date.class) {
			return new Date();
		}
		
		return null;
	}
 
	/*
	 * Restituisce il descrittore della variabile o null se non esiste
	 * <p>
	 */
	private InnerVar getVar(String varName) {
		return this.hm_var.get(varName);
	}
    
	/*
	 * Restituisce la dichiarazione di forAnyEach o null se non definita
	 */
	@SuppressWarnings("unused")
	private InnerForAnyEach getForAnyEachDeclared(Class<?> entityClass) {
		for (InnerForAnyEach forAnyEach : this.al_forAnyEach) {
			if (forAnyEach.entityObject.getClass() == entityClass) {
				return forAnyEach;
			}
		}
		return null;
	}

	
	/* ---------------------------------------------------------------------------------
	 * ---------------------------------------------------------------------------------
	 * ------------------------ Classi interne di servizio -----------------------------
	 * ---------------------------------------------------------------------------------
	 * ---------------------------------------------------------------------------------
	 */
	
	
	
	/* --------------------------------------------------------------------------------------
	 * Contiene tutte le informazioni associate a entity dichiarato in FOR_ANY o FOR_EACH.
	 * --------------------------------------------------------------------------------------
	 * 
	 * Sono informazioni esaustive per ogni successiva elaborazione.
	 * La codifica è la stessa del DataBaseManager, con informazioni su lato java, nome e tipo campo e sql, nome.
	 * Sono codificate in questo modo la chiave primaria dell'entity oggetto di FOR_ANY() o FOR_EACH()
	 * Per ogni annotation forAny e forEachpresente nella classe con il nome dell'entity in oggetto;
	 *  - si popola la map corrispondente a forAny e forEach con key=enome entity
	 *  - si popola per ogni entry inserito un array di descrittori di coppie dei campi delle due entity in relazione
	 */
	class InnerForAnyEach{
		
		// Identificazione entry
		EnumForwardLogicalDataViewControl typeEntry = null;				// Tipo entry
		int indexEntry = 0; 											// Index 0-base nella struttura 
		
		
		// Info nome entity e entity in relazione con
		Object entityObject = null;										// Oggetto bean di gestione entity, come EntityMetric																		
		String entityName = "";											// Entity (nome classe) o nome (o valore numerico testuale) tabella dominio o configurazione
		String entityNameDb = "";										// Entity name come da Create table e in annotation classe di entity
		String entityNameAs = "";                                       // Table correlation name, come da parametro AS sql
		String entityNameRelatedWith = "";								// Entity precedente con la quale è in relazione FOR_ANY() o FOR_EACH() oppure entity 
		                                                                // precedente dove la rule table FOR_ANY(numTb) è definita
		
		
		// Info solo se type entry forAny RULE table.
		// Info parzialmente recuperate in validate() da forAny(entity) con la key necessaria ad accedere a questa rule table.
		// Si utilizzano le stesse strutture usate per descrivere l'accesso alle entity.
		// In fase di dichiarazione le informazioni vengono normalizzate, dal momento che
		// per le rule entities le colonne sono definite virtualmente all'interno di un campo singolo generico.
		int ruleTableNum = 0;                                           // Numero tabella dominio/configurazione se isForAny e isForAnyRuleTable
		String ruleTableId = "";                                        // Stringa numero tabella dominio/configurazione 
		boolean ruleTableSystem = false;   
		// False indica tabella applicativa qualificata applicativamente anche da sistema/sottosistema (invece che '*' fisso)
		String ruleTableRef = "";                                       // Identificatore univoco rule table acceduta (ricavato in dichiarazione dalle keys bound) 
		String ruleTableFirstKeyColToBound = "";                        // Nome prima colonna PK in EntityTableData da collegare alle keys bound.
		                                                                // In questa implementazione si tratta del campo keyVal (TBDTKVAL), unico campo chiave disponibile
		                                                                // per accedere alle rule table. System, subsystem, language, numTable e keySeq sono valorizzati
		                                                                // automaticamente in base ai valori correnti di sistema.
		                                                                // Informazioni completate da validate()
		String ruleTableKeysName[] = null;								// Nomi campi chiave in entityTableData da abbinare a ruleTableRelatedKeysName
		String ruleTableRelatedKeysName[] = null;                       // Nomi campi chiave definiti in una FOR_ANY() e FOR_EACH() precedenti da utilizzare per 
		                                                                // comporre la chiave di accesso a questa rule table. 
		                                                                // Al momento si tratta di un array di un solo elemento.
		
		
		// Condizione sql where supplementare
		String selectSql = "";                                          // Select sql completa di FOR_SQL()
		String whereSql = "";                                           // Condizione where sql supplementare per FOR_ANY() e FOR_EACH()
		
		
		// Info di inclusione/esclusione campi/colonne da portare in select con la clausola AS
		boolean areIncludedAllFields = true;                            // True se tutte le colonne dichiarate sono incluse
		ArrayList<String> al_includedExcludedField = null;              // Se areIncludedAllFields = false sono i campi da includere
		 															    // Se areIncludedAllFields = true sono i campi da escludere
		ArrayList<String> al_columnAsName = null;                       // Colonne, come nomi java da elencare nella Select o di riferimento per Insert/Update/Delete.
																		// Sono già i nomi colonna dopo AS name.
		                                                                // Se non è presente la clausola AS il nome è quello java definito per l'entity
		
		// Info di update e di inclusione/esclusione campi updatabili
		boolean areUpdatableAllFields = true;                           // True indica che tutti i campi sono updatabili
		ArrayList<String> al_includedExcludedFieldUpdatable = null;     // Se isUpdatableAllFields = false sono i campi da updatabili
		 																// Se isUpdatableAllFields = true sono i campi da escludere dall'update
		
		// Info esaustive entity corrente entityName da annotation in classe.
		// Sono descritti tutti i campi, le entities in relazione e le coppie di campi che li definiscono.
		// Sono definiti i nomi dei campi restituiti con un nome diberso (AS vlause)
		// Sono inoltre descritte tutte le tabelle di dominio con la sequenza di campi chiave.
		// I nomi dei campi chiave sono quelli dei campi java direttamente nell'entity, che identificano un'elemento di tabella.
		// I campi chiave come il codice tabella e il codice lingua NON sono considerati.
		// Il o i gruppi di campi chiave hanno un identificatore alfanumerico codificato sempre nella annotation
		// in quanto la stessa tabella di dominio potrebbe essere dichiarata + volte con campi diversi nella stessa entity.
		DataBaseItemDescriptor[] ar_primarKey = null;					// Descrittori campi primary key
		DataBaseItemDescriptor[] ar_notPrimarKey = null;				// Descrittori campi non primary key
		Map<String, ForwardColumnPair[]> hm_forAny = null;				// Key=EntityNameRelatedWith in FOR_ANY con entityName 
																	    // Data=array di oggetti con coppie di descrittori campi
		Map<String, ForwardColumnPair[]> hm_forEach = null;				// Key=EntityNameRelatedWith in FOR_EACH con entityName
																		// Data=array di oggetti con coppie di descrittori campi
		Map<String, InnerUsableRuleTable> hm_usableRuleTable = null;	// Key=id rule table presente nella definizione dell'entity, anche relativa a campi diversi
																		// Data=descrittore campi key bound di accesso a rule table
	    
		
		// Info colonne PK escluse da where a fronte FOR_EACH() o FOR_ANY() come prima dichiarativa.
		// Le colonne specificate NON vengono incluse nelle condizioni di where sulla  primary key.
		// Con FOR_EACH() la condizione è sicuramente parziale sulla primary key o può arrivare ad
		// escludere del tutto la PK utilizzando una condizione di where completamente applicativa
		// FOR_ANY() per rule table deve escludere la colonna PK keySeq
		// FOR_EACH() per rule table deve escludere le colonna PK keySeq e keyVal
		String ar_primaryKeyExclude[] = null;
		
		// Contatore colonne inserite/aggiornate
		int cntColumnInsUpd = 0;
		
		private InnerForAnyEach() {
			typeEntry = EnumForwardLogicalDataViewControl.LDV_OK;
			ar_primarKey = new DataBaseItemDescriptor[0];
			ar_notPrimarKey = new DataBaseItemDescriptor[0];
			ruleTableKeysName = new String[0];
			ruleTableRelatedKeysName = new String[0];
			hm_forAny = new HashMap<String, ForwardColumnPair[]> ();
			hm_forEach = new HashMap<String, ForwardColumnPair[]> ();
			hm_usableRuleTable = new HashMap<String, InnerUsableRuleTable> ();
			al_columnAsName = new ArrayList<String> ();
			al_includedExcludedField = new ArrayList<String> ();
			al_includedExcludedFieldUpdatable = new ArrayList<String> ();
		}

		
		/*
		 * Restituisce un arraylist di oggetti DataBaseItemDescriptor promary key
		 */
		private ArrayList<DataBaseItemDescriptor> getPkDataBaseItemDescriptors() {
			ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
			al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
			for (DataBaseItemDescriptor dataBaseItemDescriptor : ar_primarKey) {
				al_dataBaseItemDescriptor.add(dataBaseItemDescriptor);
			}
			return al_dataBaseItemDescriptor;
		}

		/*
		 * Restituisce un arraylist di oggetti DataBaseItemDescriptor NON promary key
		 */
		private ArrayList<DataBaseItemDescriptor> getNotPkDataBaseItemDescriptors() {
			ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
			al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
			for (DataBaseItemDescriptor dataBaseItemDescriptor : ar_notPrimarKey) {
				al_dataBaseItemDescriptor.add(dataBaseItemDescriptor);
			}
			return al_dataBaseItemDescriptor;
		}

		/*
		 * Restituisce un arraylist di oggetti DataBaseItemDescriptor primary key e NON promary key
		 */
		private ArrayList<DataBaseItemDescriptor> getDataBaseItemDescriptors() {
			ArrayList<DataBaseItemDescriptor> al_dataBaseItemDescriptor = null;
			al_dataBaseItemDescriptor = new ArrayList<DataBaseItemDescriptor> ();
			al_dataBaseItemDescriptor.addAll(getPkDataBaseItemDescriptors());
			al_dataBaseItemDescriptor.addAll(getNotPkDataBaseItemDescriptors());
			return al_dataBaseItemDescriptor;
		}

		
		/*
		 * Restituisce true se la colonna è definita nell'entity.
		 * Si tratta del nome java e NON del nome db2
		 */
		private boolean isColumnDeclared(String columnName) {
			for (DataBaseItemDescriptor columnPK : ar_primarKey) {
				if (columnPK.getJavaFieldName().equals(columnName)) {return true;}
			}
			for (DataBaseItemDescriptor columnNoPK : ar_notPrimarKey) {
				if (columnNoPK.getJavaFieldName().equals(columnName)) {return true;}
			}
			return false;
		}
		
		/*
		 * Restituisce la colonna db del campo system
		 * Si tratta del nome java e NON del nome db2
		 */
		private String getSystemColumnDb() {
			for (DataBaseItemDescriptor columnPK : ar_primarKey) {
				if (!columnPK.getJavaFieldName().equals("system")) {continue;}
				return columnPK.getDbColumn();
			}
			return "";
		}
		
		/*
		 * Restituisce la colonna db del campo subSystem
		 * Si tratta del nome java e NON del nome db2
		 */
		private String getSubSystemColumnDb() {
			for (DataBaseItemDescriptor columnPK : ar_primarKey) {
				if (!columnPK.getJavaFieldName().equals("subSystem")) {continue;}
				return columnPK.getDbColumn();
			}
			return "";
		}
		
		/*
		 * Restituisce i nomi delle entity in relazione FOR_ANY()
		 */
		private Set<String> getForAnyEntityNames() {
			Set<String> setKeysForAny = hm_forAny.keySet();
			return setKeysForAny;
		}
		
		/*
		 * Restituisce i nomi delle entity in relazione FOR_EACH()
		 */
		private Set<String> getForEachEntityNames() {
			Set<String> setKeysForEach = hm_forEach.keySet();
			return setKeysForEach;
		}
		
		/*
		 * Restituisce gli id di tutte le rule table accedute.
		 */
		@SuppressWarnings("unused")
		private ArrayList<String> getUsableRuleTablesId() {
			ArrayList<String> al_usableRuleTableId = null;
			al_usableRuleTableId = new ArrayList<String> ();
			// Scan rule table accedibili
			for (Entry<String, InnerUsableRuleTable> entryRuleTable : hm_usableRuleTable.entrySet()) {
				al_usableRuleTableId.add(entryRuleTable.getValue().tabId);
			}
			return al_usableRuleTableId;
		}
		
		/*
		 * Restituisce il descrittore di una specifica rule table usabile fornito l'id della tabella
		 */
		@SuppressWarnings("unused")
		private InnerUsableRuleTable getUsableRuleTable(String tabId) {
			// Scan rule table accedibili
			for (Entry<String, InnerUsableRuleTable> entryRuleTable : hm_usableRuleTable.entrySet()) {
				if (entryRuleTable.getValue().tabId.equals(tabId)) {
					return entryRuleTable.getValue();
				}
			}
			return null;
		}
		
		/*
		 * Restituisce i descrittori di una rule table usabile fornito l'id della tabella.
		 * La stessa tabella può essere usata con chiavi diverse nella stessa entity
		 */
		private ArrayList<InnerUsableRuleTable> getUsableRuleTables(String tabId) {
			ArrayList<InnerUsableRuleTable> al_innerUsableRuleTable = null;
			
			al_innerUsableRuleTable = new ArrayList<InnerUsableRuleTable> ();
			
			// Scan rule table accedibili
			for (Entry<String, InnerUsableRuleTable> entryRuleTable : hm_usableRuleTable.entrySet()) {
				if (entryRuleTable.getValue().tabId.equals(tabId)) {
					al_innerUsableRuleTable.add(entryRuleTable.getValue());
				}
			}
			return al_innerUsableRuleTable;
		}
		
		/*
		 * Restituisce il descrittore di una specifica rule table usabile forniti il tabId e i campi chiave
		 * o null se non c'è matching
		 */
		@SuppressWarnings("unused")
		private InnerUsableRuleTable getUsableRuleTable(String tabId, String ar_tabKeyInput[]) {
			boolean keyFound = false;
			// Scan rule table accedibili
			for (Entry<String, InnerUsableRuleTable> entryRuleTable : hm_usableRuleTable.entrySet()) {
				if (!entryRuleTable.getValue().tabId.equals(tabId)) {continue;}
				// Scan campi key bound dichiarati in FOR_ANY()
				for (String tabKeyInput : ar_tabKeyInput) {
					keyFound = false;
					// Scan campi key bound dichiarati in annotation di entity
					for (String tabKeyColBound : entryRuleTable.getValue().tabKeyColsBound) {
						if (tabKeyInput.equals(tabKeyColBound)) {
							keyFound = true;
							break;
						}
					}
					if (!keyFound) {return null;}
				}
				return entryRuleTable.getValue();
			}
			return null;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return this.typeEntry.toString()+ " "+this.entityNameAs;
		}
		
	}
		

	/* ------------------------------------------------------------------------------------------
	 * Descrittore singola rule table accedibile da entity dichiarata con FOR_ANY() o FOR_EACH()
	 * ------------------------------------------------------------------------------------------
	 * 
	 * Sono informazioni recuperate dall'annotation @DataBaseMappedRuleTable nell'annotation @DataBaseMappedRuleTables,
	 * per TUTTE le rule table accedibili dall'entity.
	 * Si tratta del nome/numero rule table e dei campi chiave (normalmente uno solo) necessari per accedere alla rule table.
	 * 
	 * 
	 */
	class InnerUsableRuleTable{
		String tabNumCol = "";                                          // Nome colonna (nome campo java) dell'entity con il numero della tabella, per il JOIN
		String tabRef = "";                                          	// Identificatore univoco tabella nell'ambito dell'entity
		String tabId = "";                                           	// Key tabella o numero come stringa numerica
		boolean tabSystem = false;                                      // False se tabella applicativa con sistema e sottosistema significativi
		String[] tabKeyColsBound = null;        						// Campi chiave di accesso alla tabella senza id tabella e language  
	}

	
	/* ------------------------------------------------------------------------------------------
	 * Identificazione singola colonna esposta come AS column in FOR_ANY() o FOR_EACH()
	 * ------------------------------------------------------------------------------------------
	 * 
	 * Oltre agli indici di identificazione viene preparata memorizzata la stringa già pronta del tipo:
	 * 
	 * columnNameDb AS columnNameJava
	 * 
	 */
	class InnerAsColumn{
		int entityOwnerIndex = 0;                                       // Index 0-based entity di appartenenza colonna in struttura interna 
		int columnIndex = 0;        									// Index 0-based progressivo colonna esposta a livello di view
        String columnNameDb = "";										// Nome colonna definita su db
        String columnNameJava = "";										// Nome campo java definito su entity per la colonna 
        String columnNameAs = "";										// Nome campo java definito nella view con AS name                                                                       
        																// Se assente clausola AS coincide con il nome java definito su entity per la colonna 
	}	
}
