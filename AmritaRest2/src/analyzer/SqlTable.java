
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorTable
 * </h1>
 * <p>
 * Descrive tutte le informazioni di una tabella Sql, a fronte di uno statement CREATE TABLE.br>
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/lug/2011 
 * @see SqlDescriptorTableColumn
*/

/**
 * @author Amrita
 *
 */
public class SqlTable implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Caratteristiche generali tabella
    //////////////////////////////////////////////////////////////////////////
    
	private String tableFullName = "";                           		  			// Nome completo qualificato
	private String db2LocationName = "";                           		  			// Location db2 qualificazione su 3 campi
	private String tableOwner = "";                           		  			    // Nome owner se qualificazione su 3 o 2 campi
	private String tableName = "";                           		  			    // Nome tabella applicativo
	private String likeTableView = "";                                              // Nome tabella o view di LIKE
	private String databaseName = "";												// Nome database in cui è la tabella
	private String tablespaceName = "";										        // Nome tablespace in cui è la tabella
	private String editprocProgramName = "";										// Proc di edit
	private String validprocProgramName = "";										// Proc di validazione
	private String audit = "";														// NONE, CHANGES, ALL
	private String dataCapture = "";                                                // NONE, CHANGES
	private String ccsId = "";                                                      // ASCII, EBCDIC, UNICODE
	private int obid = 0;                                                           // OBID identifier
	private boolean isWithRestrictOnDrop = false;                                   // Opzione WITH RESTRICT ON DROP
	private boolean isCopyIncludingIdentityColumnAttributes = false;                // Opzione LIKE tb INCLUDING IDENTITY COLUMN ATTRIBUTES
	private boolean isCopyIncludingColumnDefaults = false;                			// Opzione LIKE tb INCLUDING COLUMN DEFAULTS
	private boolean isCopyIncludingRowChangeTimestamp = false;                		// Opzione LIKE tb INCLUDING ROW CHANGE TIMESTAMP
	private boolean isCopyUsingTypeDefaults = false;                		        // Opzione LIKE tb USING TYPE DEFAULTS
	private boolean isCopyExcludingIdentityColumnAttributes = false;                // Opzione LIKE tb EXCLUDING IDENTITY COLUMN ATTRIBUTES
	private boolean isCopyExcludingColumnDefaults = false;                			// Opzione LIKE tb EXCLUDING COLUMN DEFAULTS
	private boolean isCopyExcludingRowChangeTimestamp = false;                		// Opzione LIKE tb EXCLUDING ROW CHANGE TIMESTAMP
	private boolean isCopyExcludingXmlTypeModifiers = false;                		// Opzione LIKE tb EXCLUDING XML TYPE MODIFIERS
	private boolean isPeriodSystemTime = false;                                     // PERIOD SYSTEM_TIME
	private boolean isPeriodBusinessTime = false;                                   // PERIOD BUSINESS_TIME
	private ArrayList<SqlTableColumn> al_column = null;                   			// Colonne tabella 

	
	//////////////////////////////////////////////////////////////////////////
    // Vincoli (costraints)
    //////////////////////////////////////////////////////////////////////////
    
    // Opzioni vincoli
	private boolean isConstraintPrimaryKey = false;               						// Chiave primaria definita
	private boolean isConstraintUnique = false;                   						// Chiave/i univoche definita/e  
	private boolean isConstraintForeignKey = false;               						// Chiave/i foreign definita/e
	private boolean isConstraintCheck = false;                    						// Vincolo/i di check definito/i
	
	// Valori associati a opzioni vincoli relativi a tutta la tabella
	private String constraintUniquePrimaryKeyName = "";           						// CONSTRAINT name UNIQUE|PRIMARY KEY
	
	// Valori associati a opzioni vincoli di + occorrenze
	private ArrayList<String> al_constraintUniquePrimaryKeyCols = null;  				// CONSTRAINT name UNIQUE|PRIMARY KEY (cols)
	// Viaggiano insieme
	private ArrayList<String> al_constraintForeignKeyName = null;          				// CONSTRAINT name FOREIGN KEY
	private ArrayList<String> al_constraintForeignKeyReferenceTable = null;				// CONSTRAINT name FOREIGN KEY (cols)   REFERENCES table-name
	private ArrayList<ArrayList<String>> al_constraintForeignKeyCols = null;			// CONSTRAINT name FOREIGN KEY (cols) 
	private ArrayList<ArrayList<String>> al_constraintForeignKeyReferenceCols = null; 	// CONSTRAINT name FOREIGN KEY (cols)   REFERENCES table-name cols)  
	private ArrayList<String> al_constraintForeignOnDelete = null;         	            // ... REFERENCES table-name cols) ... ON DELETE RESTRICT|NO ACTION|CASCADE|SET NULL
	// Viaggiano insieme                                                                
	private ArrayList<String> al_constraintCheckName = null;                  	  		// CONSTRAINT name
	private ArrayList<String>  al_constraintCheckCondition = null;                 		// CONSTRAINT name CHECK (condition)

	//////////////////////////////////////////////////////////////////////////
    // Informazioni fisiche e di allocazione e varie
    //////////////////////////////////////////////////////////////////////////
    
	// Partizioni e hashing
	private SqlPartitions partitions = null;                                   			  // PARTITION BY RANGE ... PARTITION 1 ENDING AT (MINVALUE'), ...
	private ArrayList<String> al_columnOrganizeByHashUnique = null;                      // ORGANIZE BY HASH (col1, .. coln)
    private int organizeByHashUniqueSpaceValue = 0;                                      // ORGANIZE BY HASH UNIQUE (cols) HASH SPACE integer
    private String organizeByHashUniqueSpaceType = "";                                   // ORGANIZE BY HASH UNIQUE (cols) HASH SPACE integer K|M|G
	private ArrayList<String> al_periodColumn = null;          							 // PERIOD ..... (cols)

	
	/**
	 * Costruttore vuoto
	 */
	public SqlTable() {
		super();
		al_column = new  ArrayList<SqlTableColumn> ();
		al_constraintUniquePrimaryKeyCols = new ArrayList<String> ();  				 
		al_constraintForeignKeyName = new ArrayList<String> ();          				 
		al_constraintForeignKeyReferenceTable = new ArrayList<String> ();				 
		al_constraintForeignKeyCols = new ArrayList<ArrayList<String>> ();			 
		al_constraintForeignKeyReferenceCols = new ArrayList<ArrayList<String>> (); 	 
		al_constraintForeignOnDelete = new ArrayList<String> ();          
		al_constraintCheckName = new ArrayList<String> ();                  	  			 
		al_constraintCheckCondition = new ArrayList<String> ();  
		al_columnOrganizeByHashUnique = new ArrayList<String> ();
		al_periodColumn = new ArrayList<String>();
	}


	/**
	 * Restituisce il nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @return the tableFullName
	 */
	public String getTableFullName() {
		return tableFullName;
	}


	/**
	 * Imposta il nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @param tableFullName the tableFullName to set
	 */
	public void setTableFullName(String tableFullName) {
		this.tableFullName = tableFullName;
	}


	/**
	 * Restituisce il server-id del nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @return the db2LocationName
	 */
	public String getDb2LocationName() {
		return db2LocationName;
	}


	/**
	 * Imposta il server-id del nome qualificato della tabella,
	 * come <tt>serverId.Owner.tableName</tt>
	 * 
	 * @param db2LocationName the db2LocationName to set
	 */
	public void setDb2LocationName(String db2LocationName) {
		this.db2LocationName = db2LocationName;
	}


	/**
	 * Restituisce l'owner della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br>
	 * <p>
	 * 
	 * @return the tableOwner
	 */
	public String getTableOwner() {
		return tableOwner;
	}


	/**
	 * Imposta l'owner della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br>
	 * <p>
	 * 
	 * @param tableOwner the tableOwner to set
	 */
	public void setTableOwner(String tableOwner) {
		this.tableOwner = tableOwner;
	}


	/**
	 * Restituisce il nome  della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br> oppure di
	 * tableName per nome non qualificato.
	 * <p>
	 * 
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}


	/**
	 * Imposta il nome  della tabella, a fronte
	 * di <tt>serverId.Owner.tableName</tt> oppure di
	 * <tt>Owner.tableName</tt><br> oppure di
	 * tableName per nome non qualificato.
	 * <p>
	 * 
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}


	/**
	 * Restituisce il nome  del database, a fronte di<br>
	 * <tt>IN DATABASE database-name</tt> oppure di <br>
	 * <tt>IN database-name.tablespace.name</tt> oppure di<br> 
	 * <p>
	 * 
	 * @return the databaseName
	 */
	public String getDatabaseName() {
		return databaseName;
	}


	/**
	 * Imposta il nome  del database, a fronte di<br>
	 * <tt>IN DATABASE database-name</tt> oppure di <br>
	 * <tt>IN database-name.tablespace.name</tt> oppure di<br> 
	 * <p>
	 * 
	 * @param databaseName the databaseName to set
	 */
	public void setDatabaseName(String databaseName) {
		this.databaseName = databaseName;
	}


	/**
	 * Restituisce il nome  del tablespace in cui è memorizzata
	 * la tabella, a fronte di<br>
	 * <tt>IN database-name.tablespace.name</tt><br> 
	 * <p>
	 * 
	 * @return the tablespaceName
	 */
	public String getTablespaceName() {
		return tablespaceName;
	}


	/**
	 * Imposta il nome  del tablespace in cui è memorizzata
	 * la tabella, a fronte di<br>
	 * <tt>IN database-name.tablespace.name</tt><br> 
	 * <p>
	 * 
	 * @param tablespaceName the tablespaceName to set
	 */
	public void setTablespaceName(String tablespaceName) {
		this.tablespaceName = tablespaceName;
	}


	/**
	 * Restituisce la EDITPROC procedure.<br>
	 * <p>
	 * 
	 * @return the editprocProgramName
	 */
	public String getEditprocProgramName() {
		return editprocProgramName;
	}


	/**
	 * Imposta la EDITPROC procedure.<br>
	 * <p>
	 * 
	 * @param editprocProgramName the editprocProgramName to set
	 */
	public void setEditprocProgramName(String editprocProgramName) {
		this.editprocProgramName = editprocProgramName;
	}


	/**
	 * Restituisce la VALIDPROC procedure.<br>
	 * <p>
	 * 
	 * @return the validprocProgramName
	 */
	public String getValidprocProgramName() {
		return validprocProgramName;
	}


	/**
	 * Imposta la VALIDPROC procedure.<br>
	 * <p>
	 * 
	 * @param validprocProgramName the validprocProgramName to set
	 */
	public void setValidprocProgramName(String validprocProgramName) {
		this.validprocProgramName = validprocProgramName;
	}


	/**
	 * Restituisce il valore del parametro AUDIT<br>
	 * <p>
	 * 
	 * @return the audit
	 */
	public String getAudit() {
		return audit;
	}


	/**
	 * Imposta il valore del parametro AUDIT<br>
	 * <p>
	 * 
	 * @param audit the audit to set
	 */
	public void setAudit(String audit) {
		this.audit = audit;
	}


	/**
	 * Restituisce il valore del parametro DATA CAPTURE<br>
	 * <p>
	 * 
	 * @return the dataCapture
	 */
	public String getDataCapture() {
		return dataCapture;
	}


	/**
	 * Imposta il valore del parametro DATA CAPTURE<br>
	 * <p>
	 * 
	 * @param dataCapture the dataCapture to set
	 */
	public void setDataCapture(String dataCapture) {
		this.dataCapture = dataCapture;
	}


	/**
	 * Restituisce il valore del parametro CCSID<br>
	 * <p>
	 * 
	 * @return the ccsId
	 */
	public String getCcsId() {
		return ccsId;
	}


	/**
	 * Imposta il valore del parametro CCSID<br>
	 * <p>
	 * 
	 * @param ccsId the ccsId to set
	 */
	public void setCcsId(String ccsId) {
		this.ccsId = ccsId;
	}


	/**
	 * Restituisce true se  WITH RESTRICT ON DROP<br>
	 * <p>
	 * 
	 * @return the isWithRestrictOnDrop
	 */
	public boolean isWithRestrictOnDrop() {
		return isWithRestrictOnDrop;
	}


	/**
	 * Imposta se esiste  WITH RESTRICT ON DROP<br>
	 * <p>
	 * 
	 * @param isWithRestrictOnDrop the isWithRestrictOnDrop to set
	 */
	public void setWithRestrictOnDrop(boolean isWithRestrictOnDrop) {
		this.isWithRestrictOnDrop = isWithRestrictOnDrop;
	}


	/**
	 * Restituisce il parametro OBID<br>
	 * <p>
	 * 
	 * @return the obid
	 */
	public int getObid() {
		return obid;
	}


	/**
	 * Imposta il parametro OBID<br>
	 * <p>
	 * 
	 * @param obid the obid to set
	 */
	public void setObid(int obid) {
		this.obid = obid;
	}



	/**
	 * Restituisce il nome della tabella o della view codificate in<br>
	 * LIKE table/view INCLUDING IDENTITY COLUMN ATTRIBUTES.<br>
	 * <p>
	 * 
	 * 
	 * @return the likeTableView
	 */
	public String getLikeTableView() {
		return likeTableView;
	}


	/**
	 * Restituisce se presente l'opzione INCLUDING IDENTITY COLUMN ATTRIBUTES <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyIncludingIdentityColumnAttributes
	 */
	public boolean isCopyIncludingIdentityColumnAttributes() {
		return isCopyIncludingIdentityColumnAttributes;
	}


	/**
	 * Imposta se presente l'opzione INCLUDING IDENTITY COLUMN ATTRIBUTES <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyIncludingIdentityColumnAttributes the isCopyIncludingIdentityColumnAttributes to set
	 */
	public void setCopyIncludingIdentityColumnAttributes(
			boolean isCopyIncludingIdentityColumnAttributes) {
		this.isCopyIncludingIdentityColumnAttributes = isCopyIncludingIdentityColumnAttributes;
	}


	/**
	 * Restituisce se presente l'opzione INCLUDING COKUMN DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyIncludingColumnDefaults
	 */
	public boolean isCopyIncludingColumnDefaults() {
		return isCopyIncludingColumnDefaults;
	}


	/**
	 * Imposta se presente l'opzione INCLUDING COLUMN DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyIncludingColumnDefaults the isCopyIncludingColumnDefaults to set
	 */
	public void setCopyIncludingColumnDefaults(boolean isCopyIncludingColumnDefaults) {
		this.isCopyIncludingColumnDefaults = isCopyIncludingColumnDefaults;
	}


	/**
	 * Restituisce se presente l'opzione INCLUDING ROW CHANGE TIMESTAMP <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyIncludingRowChangeTimestamp
	 */
	public boolean isCopyIncludingRowChangeTimestamp() {
		return isCopyIncludingRowChangeTimestamp;
	}


	/**
	 * Imposta se presente l'opzione INCLUDING ROW CHANGE TIMESTAMP <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyIncludingRowChangeTimestamp the isCopyIncludingRowChangeTimestamp to set
	 */
	public void setCopyIncludingRowChangeTimestamp(
			boolean isCopyIncludingRowChangeTimestamp) {
		this.isCopyIncludingRowChangeTimestamp = isCopyIncludingRowChangeTimestamp;
	}


	/**
	 * Restituisce se presente l'opzione USING TYPE DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyUsingTypeDefaults
	 */
	public boolean isCopyUsingTypeDefaults() {
		return isCopyUsingTypeDefaults;
	}


	/**
	 * Imposta se presente l'opzione USING TYPE DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyUsingTypeDefault the isCopyUsingTypeDefault to set
	 */
	public void setCopyUsingTypeDefaults(boolean isCopyUsingTypeDefaults) {
		this.isCopyUsingTypeDefaults = isCopyUsingTypeDefaults;
	}


	/**
	 * Restituisce se presente l'opzione EXCLUDING IDENTITY COLUMN ATTRIBUTES <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyExcludingIdentityColumnAttributes
	 */
	public boolean isCopyExcludingIdentityColumnAttributes() {
		return isCopyExcludingIdentityColumnAttributes;
	}


	/**
	 * Imposta se presente l'opzione EXCLUDING IDENTITY COLUMN ATTRIBUTES <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyExcludingIdentityColumnAttributes the isCopyExcludingIdentityColumnAttributes to set
	 */
	public void setCopyExcludingIdentityColumnAttributes(
			boolean isCopyExcludingIdentityColumnAttributes) {
		this.isCopyExcludingIdentityColumnAttributes = isCopyExcludingIdentityColumnAttributes;
	}


	/**
	 * Restituisce se presente l'opzione EXCLUDING COLUMN DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyExcludingColumnDefaults
	 */
	public boolean isCopyExcludingColumnDefaults() {
		return isCopyExcludingColumnDefaults;
	}


	/**
	 * Imposta se presente l'opzione EXCLUDING COLUMN DEFAULTS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyExcludingColumnDefaults the isCopyExcludingColumnDefaults to set
	 */
	public void setCopyExcludingColumnDefaults(boolean isCopyExcludingColumnDefaults) {
		this.isCopyExcludingColumnDefaults = isCopyExcludingColumnDefaults;
	}


	/**
	 * Restituisce se presente l'opzione EXCLUDING ROW CHANGE TIMESTAMP <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyExcludingRowChangeTimestamp
	 */
	public boolean isCopyExcludingRowChangeTimestamp() {
		return isCopyExcludingRowChangeTimestamp;
	}


	/**
	 * Imposta se presente l'opzione EXCLUDING ROW CHANGE TIMESTAMP <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyExcludingRowChangeTimestamp the isCopyExcludingRowChangeTimestamp to set
	 */
	public void setCopyExcludingRowChangeTimestamp(
			boolean isCopyExcludingRowChangeTimestamp) {
		this.isCopyExcludingRowChangeTimestamp = isCopyExcludingRowChangeTimestamp;
	}


	/**
	 * Restituisce se presente l'opzione EXCLUDING XML TYPE MODIFIERS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @return the isCopyExcludingXmlTypeModifiers
	 */
	public boolean isCopyExcludingXmlTypeModifiers() {
		return isCopyExcludingXmlTypeModifiers;
	}


	/**
	 * Imposta se presente l'opzione EXCLUDING XML TYPE MODIFIERS <br>
	 * su LIKE table/view.<br>
	 * <p>
	 * @param isCopyExcludingXmlTypeModifiers the isCopyExcludingXmlTypeModifiers to set
	 */
	public void setCopyExcludingXmlTypeModifiers(
			boolean isCopyExcludingXmlTypeModifiers) {
		this.isCopyExcludingXmlTypeModifiers = isCopyExcludingXmlTypeModifiers;
	}

	/**
	 * Restituisce se è dichiarata l'opzione <tt>PERIOD SYSTEM_TIME</tt>
	 * <p>
	 * 
	 * @return the isPeriodSystemTime
	 */
	public boolean isPeriodSystemTime() {
		return isPeriodSystemTime;
	}

	/**
	 * Imposta se è dichiarata l'opzione <tt>PERIOD SYSTEM_TIME</tt>
	 * <p>
	 * 
	 * @param isPeriodSystemTime the isPeriodSystemTime to set
	 */
	public void setPeriodSystemTime(boolean isPeriodSystemTime) {
		this.isPeriodSystemTime = isPeriodSystemTime;
	}

	/**
	 * Restituisce se è dichiarata l'opzione <tt>PERIOD BUSINESS_TIME</tt>
	 * <p>
	 * 
	 * @return the isPeriodBusinessTime
	 */
	public boolean isPeriodBusinessTime() {
		return isPeriodBusinessTime;
	}

	/**
	 * Imposta se è dichiarata l'opzione <tt>PERIOD BUSINESS_TIME</tt>
	 * <p>
	 * 
	 * @param isPeriodBusinessTime the isPeriodBusinessTime to set
	 */
	public void setPeriodBusinessTime(boolean isPeriodBusinessTime) {
		this.isPeriodBusinessTime = isPeriodBusinessTime;
	}


	/**
	 * Imposta il nome della tabella o della view codificate in<br>
	 * LIKE table/view INCLUDING IDENTITY COLUMN ATTRIBUTES.<br>
	 * <p>
	 * 
	 * @param likeTableView the likeTableView to set
	 */
	public void setLikeTableView(String likeTableView) {
		this.likeTableView = likeTableView;
	}


	/**
	 * Restituisce le colonne definite per la tabella,
	 * come ArrayList di {@link SqlTableColumn}
	 * 
	 * @return the al_column
	 */
	public ArrayList<SqlTableColumn> getColumns() {
		return al_column;
	}


	/**
	 * Imposta le colonne definite per la tabella,
	 * come ArrayList di {@link SqlTableColumn}
	 * 
	 * @param al_column the al_column to set
	 */
	public void setColumns(ArrayList<SqlTableColumn> al_column) {
		this.al_column = al_column;
	}

	/**
	 * Restituisce true se la colonna è dichiarata <tt>PRIMARY KEY</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @return the isConstraintPrimaryKey
	 */
	public boolean isConstraintPrimaryKey() {
		return isConstraintPrimaryKey;
	}

	/**
	 * Imposta se la colonna è dichiarata <tt>PRIMARY KEY</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @param isConstraintPrimaryKey the isConstraintPrimaryKey to set
	 */
	public void setConstraintPrimaryKey(boolean isConstraintPrimaryKey) {
		this.isConstraintPrimaryKey = isConstraintPrimaryKey;
	}

	/**
	 * Restituisce true se la colonna è dichiarata <tt>UNIQUE</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @return the isConstraintUnique
	 */
	public boolean isConstraintUnique() {
		return isConstraintUnique;
	}

	/**
	 * Imposta se la colonna è dichiarata <tt>UNIQUE</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @param isConstraintUnique the isConstraintUnique to set
	 */
	public void setConstraintUnique(boolean isConstraintUnique) {
		this.isConstraintUnique = isConstraintUnique;
	}

	/**
	 * @return the isConstraintForeignKey
	 */
	public boolean isConstraintForeignKey() {
		return isConstraintForeignKey;
	}

	/**
	 * @param isConstraintForeignKey the isConstraintForeignKey to set
	 */
	public void setConstraintForeignKey(boolean isConstraintForeignKey) {
		this.isConstraintForeignKey = isConstraintForeignKey;
	}

	/**
	 * Restituisce true se è dichiarata  la clausola<tt>CHECK</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @return the isConstraintCheck
	 */
	public boolean isConstraintCheck() {
		return isConstraintCheck;
	}

	/**
	 * Imposta se è dichiarata  la clausola<tt>CHECK</tt>
	 * con specificazione del nome del vincolo.<br>
	 * <p>
	 * @param isConstraintCheck the isConstraintCheck to set
	 */
	public void setConstraintCheck(boolean isConstraintCheck) {
		this.isConstraintCheck = isConstraintCheck;
	}

	/**
	 * Restituisce il nome del vincolo di <tt>PRIMARY KEY</tt>
	 * o <tt>UNIQUE</tt><br>
	 * <p>
	 * @return the constraintUniquePrimaryKeyName
	 */
	public String getConstraintUniquePrimaryKeyName() {
		return constraintUniquePrimaryKeyName;
	}

	/**
	 * Imposta il nome del vincolo di <tt>PRIMARY KEY</tt>
	 * o <tt>UNIQUE</tt><br>
	 * <p>
	 * @param constraintUniquePrimaryKeyName the constraintUniquePrimaryKeyName to set
	 */
	public void setConstraintUniquePrimaryKeyName(String constraintUniquePrimaryKeyName) {
		this.constraintUniquePrimaryKeyName = constraintUniquePrimaryKeyName;
	}


	/**
	 * Imposta i nomi dei vincolo di <tt>CHECK</tt><br>
	 * <p>
	 * @return the constraintCheckName
	 */
	public ArrayList<String> getConstraintCheckNames() {
		return al_constraintCheckName;
	}

	/**
	 * Restituisce il nomi del dei vincoli di <tt>CHECK</tt><br>
	 * <p>
	 * @param constraintCheckName the constraintCheckName to set
	 */
	public void setConstraintCheckNames(ArrayList<String> al_constraintCheckName) {
		this.al_constraintCheckName = al_constraintCheckName;
	}

	/**
	 * Restituisce i vincoli di <tt>CHECK</tt><br>
	 * <p>
	 * Ogni vincolo è associato al corrispondente nome ottenuto da getConstraintCheckNames().<br>
	 * Si può trattare di una full select o altro.<br>
	 * @return the constraintCheckCondition
	 */
	public ArrayList<String> getConstraintCheckCondition() {
		return al_constraintCheckCondition;
	}

	/**
	 * Imposta i vincoli di <tt>CHECK</tt><br>
	 * <p>
	 * Ogni vincolo è associato al corrispondente nome ottenuto da getConstraintCheckNames().<br>
	 * Si può trattare di una full select o altro.<br>
	 * @param constraintCheckCondition the constraintCheckCondition to set
	 */
	public void setConstraintCheckCondition(ArrayList<String> al_constraintCheckCondition) {
		this.al_constraintCheckCondition = al_constraintCheckCondition;
	}

	/**
	 * Restituisce il descrittore delle partizioni della  tabella.<br>
	 * <p>
	 * @return the partitions
	 */
	public SqlPartitions getPartitionsDescriptor() {
		return partitions;
	}


	/**
	 * Imposta il descrittore delle partizioni della  tabella.<br>
	 * <p>
	 * @param partitions the partitions to set
	 */
	public void setPartitionsDescriptor(SqlPartitions partitions) {
		this.partitions = partitions;
	}
		
	
	/**
	 * Restituisce le colonne espresse da <tt>ORGANIZE BY HASH (col1, .. coln)</tt>.<br>
	 * <p>
	 * @return the al_columnOrganizeByHashUnique
	 */
	public ArrayList<String> getColumnsOrganizeByHashUnique() {
		return al_columnOrganizeByHashUnique;
	}


	/**
	 * Imposta le colonne espresse da <tt>ORGANIZE BY HASH (col1, .. coln)</tt>.<br>
	 * <p>
	 * @param al_columnOrganizeByHashUnique the al_columnOrganizeByHashUnique to set
	 */
	public void setColumnOrganizeByHashUnique(ArrayList<String> al_columnOrganizeByHashUnique) {
		this.al_columnOrganizeByHashUnique = al_columnOrganizeByHashUnique;
	}


	/**
	 * Restituisce il valore espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @return the hashSpaceValue
	 */
	public int getOrganizeByHashUniqueSpaceValue() {
		return organizeByHashUniqueSpaceValue;
	}


	/**
	 * Imposta il valore espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @param organizeHashSpaceValue the organizeHashSpaceValue to set
	 */
	public void setOrganizeByHashUniqueSpaceValue(int organizeByHashUniqueSpaceValue) {
		this.organizeByHashUniqueSpaceValue = organizeByHashUniqueSpaceValue;
	}


	/**
	 * Restituisce il tipo espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @return the organizeByHashUniqueSpaceType
	 */
	public String getOrganizeByHashUniqueSpaceType() {
		return organizeByHashUniqueSpaceType;
	}


	/**
	 * Imposta il tipo espresso da <tt>HASH VALUE int-value K|M|G</tt><br>
	 * <p>
	 * @param organizeByHashUniqueSpaceType the organizeByHashUniqueSpaceType to set
	 */
	public void setOrganizeByHashUniqueSpaceType(String organizeByHashUniqueSpaceType) {
		this.organizeByHashUniqueSpaceType = organizeByHashUniqueSpaceType;
	}



	/**
	 * Restituisce i nomi delle colonne di <tt>PRIMARY KEY</tt><br>
	 * <p>
	 * @return the constraintUniquePrimaryKeyCols
	 */
	public ArrayList<String> getConstraintUniquePrimaryKeyCols() {
		return al_constraintUniquePrimaryKeyCols;
	}

	/**
	 * Imposta i nomi delle colonne di <tt>PRIMARY KEY</tt><br>
	 * <p>
	 * @param constraintUniquePrimaryKeyCols the constraintUniquePrimaryKeyCols to set
	 */
	public void setConstraintUniquePrimaryKeyCols(ArrayList<String> al_constraintUniquePrimaryKeyCols) {
		this.al_constraintUniquePrimaryKeyCols = al_constraintUniquePrimaryKeyCols;
	}

	/**
	 * Restituisce i nomi dei vincoli di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * @return the constraintForeignKeyName
	 */
	public ArrayList<String> getConstraintForeignKeyNames() {
		return al_constraintForeignKeyName;
	}

	/**
	 * Imposta i nomi dei vincoli di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * @param constraintForeignKeyName the constraintForeignKeyName to set
	 */
	public void setConstraintForeignKeyNames(ArrayList<String> al_constraintForeignKeyName) {
		this.al_constraintForeignKeyName = al_constraintForeignKeyName;
	}

	/**
	 * Restituisce i nomi delle colonne di <tt>FOREIGN KEY</tt>, per ogni colonna<br>
	 * <p>
	 * L'arrayList di primo livello ha tanti elementi quante le foreign keys.<br>
	 * <p>
	 * @return the constraintForeignKeyCols
	 */
	public ArrayList<ArrayList<String>> getConstraintForeignKeyCols() {
		return al_constraintForeignKeyCols;
	}

	/**
	 * Imposta i nomi delle colonne di <tt>FOREIGN KEY</tt>, per ogni colonna<br>
	 * <p>
	 * L'arrayList di primo livello ha tanti elementi quante le foreign keys.<br>
	 * <p>
	 * @param constraintForeignKeyCols the constraintForeignKeyCols to set
	 */
	public void setConstraintForeignKeyCols(ArrayList<ArrayList<String>> al_constraintForeignKeyCols) {
		this.al_constraintForeignKeyCols = al_constraintForeignKeyCols;
	}

	/**
	 * Restituisce i nomi della tabelle referenziate di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * @return the constraintForeignKeyReferenceTable
	 */
	public ArrayList<String> getConstraintForeignKeyReferenceTables() {
		return al_constraintForeignKeyReferenceTable;
	}

	/**
	 * Imposta i nomi della tabelle referenziate di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * @param constraintForeignKeyReferenceTable the constraintForeignKeyReferenceTable to set
	 */
	public void setConstraintForeignKeyReferenceTables(ArrayList<String> al_constraintForeignKeyReferenceTable) {
		this.al_constraintForeignKeyReferenceTable = al_constraintForeignKeyReferenceTable;
	}

	/**
	 * Restituisce i nomi delle colonne della tabella referenziata di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * L'arrayList di primo livello indica la foreign key a cui si riferiscono le colonne.<br>
	 * <p>
	 * @return the constraintForeignKeyReferenceCols
	 */
	public ArrayList<ArrayList<String>> getConstraintsForeignKeyReferenceCols() {
		return al_constraintForeignKeyReferenceCols;
	}

	/**
	 * Imposta i nomi delle colonne della tabella referenziata di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * L'arrayList di primo livello indica la foreign key a cui si riferiscono le colonne.<br>
	 * <p>
	 * @param constraintForeignKeyReferenceCols the constraintForeignKeyReferenceCols to set
	 */
	public void setConstraintsForeignKeyReferenceCols(ArrayList<ArrayList<String>> al_constraintForeignKeyReferenceCols) {
		this.al_constraintForeignKeyReferenceCols = al_constraintForeignKeyReferenceCols;
	}

	/**
	 * Restituisce il tipo di operazione da effettuare a fronte di <tt>DELETE</tt>
	 * delle tabelle referenziate di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * Ogni elemento dell'array list corrisponde alla foreign key a cui si riferiscono le operazioni ON DELETE.<br>
	 * <p>
	 * Si tratta di operazioni quali:<br>
	 * <p>
	 * <tt>RESTRICT</tt><br>
	 * <tt>NO ACTION</tt><br>
	 * <tt>CASCADE</tt><br>
	 * <tt>SET NULL</tt><br>
	 * <p>
	 * 
	 * 
	 * @return the constraintForeignOnDelete
	 */
	public ArrayList<String> getConstraintsForeignOnDelete() {
		return al_constraintForeignOnDelete;
	}

	/**
	 * Imposta il tipo di operazione da effettuare a fronte di <tt>DELETE</tt>
	 * delle tabelle referenziate di <tt>FOREIGN KEY</tt><br>
	 * <p>
	 * Ogni elemento dell'array list corrisponde alla foreign key a cui si riferiscono le operazioni ON DELETE.<br>
	 * <p>
	 * Si tratta di operazioni quali:<br>
	 * <p>
	 * <tt>RESTRICT</tt><br>
	 * <tt>NO ACTION</tt><br>
	 * <tt>CASCADE</tt><br>
	 * <tt>SET NULL</tt><br>
	 * <p>
	 * 
	 * 
	 * @param constraintForeignOnDelete the constraintForeignOnDelete to set
	 */
	public void setConstraintsForeignOnDelete(ArrayList<String> al_constraintForeignOnDelete) {
		this.al_constraintForeignOnDelete = al_constraintForeignOnDelete;
	}

	/**
	 * Restituisce le colonne specificate da tt>PERIOD SYSTEM_TIME|BUSINESS_TIME (col1, col2, ... coln)</tt></tt>
	 * <p>
	 * @return the al_periodColumn
	 */
	public ArrayList<String> getPeriodColumns() {
		return al_periodColumn;
	}

	/**
	 * Imposta le colonne specificate da tt>PERIOD SYSTEM_TIME|BUSINESS_TIME (col1, col2, ... coln)</tt></tt>
	 * <p>
	 * @param alPeriodColumn the al_periodColumn to set
	 */
	public void setPeriodColumns(ArrayList<String> al_periodColumn) {
		this.al_periodColumn = al_periodColumn;
	}

	
}
