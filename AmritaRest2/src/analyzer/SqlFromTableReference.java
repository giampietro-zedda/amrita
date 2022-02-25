
package analyzer;
import java.io.Serializable;
import java.util.ArrayList;
import enums.EnumSqlTableReferenceType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFromTableReference
 * </h1>
 * <p>
 * Descrive la clausola FROM nelle istruzioni sql dove prevista.<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement
 * @see SqlFullSelect
*/

public class SqlFromTableReference implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private EnumSqlTableReferenceType typeTableReference = null;       		// Tipologia table reference
 
	// Campi per table reference SINGLE_TABLE 
	private String singleTableEntityName = "";                      		// tableView name eventualmente qualificato
	private ArrayList<SqlPeriodSpecification> al_singleTablePeriod = null;  // FOR SYSTEM_TIME|BUSINESS_TIME ....
	
	// Campi per table reference NESTED_TABLE_EXPRESSION 
	private SqlFullSelect nestedTableFullSelect = null;						// |TABLE (full-select) corr-clause
	
	// Campi per table reference TABLE_FUNCTION_REFERENCE 
    private String tableFunctionName = "";                                 	// TABLE (function-name(expr|TABLE transiction-table-name, ... ) CARDINALITY|CARDINALITY MULTIPLIER num
	private ArrayList<SqlFromTableReferenceTableFunction> al_tableFunction = null;
    private int tableFunctionCardinalityNumber = 0;							// CARDINALITY num
    private int tableFunctionCardinalityMultiplierNumber = 0;				// CARDINALITY MULTIPLIER num
	private boolean isThereTableFunctionCardinality = false;                // CARDINALITY|CARDINALITY MULTIPLIER num

	// Campi per table reference DATA_CHANGE_TABLE_REFERENCE 
	private boolean isDataChangeFinal = false;                              // FINAL TABLE (INSERT|UPDATE|MERGE statement) 
	private boolean isDataChangeOld = false;                                // OLD   TABLE (UPDATE|DELETE statement) 
	private InstructionSql dataChangeStmt = null;							// FINAL|OLD TABLE (INSERT|UPDATE|DELETE|MERGE statement) AS ..	
	
	// Campi per table reference TABLE_LOCATOR_REFERENCE 
    private String tableLocatorHostVar  = "";                             	// TABLE(table-locator-variabile LIKE table-name) |correlation-name
    private String tableLocatorEntityNameLike = "";                         // TABLE(table-locator-variabile LIKE table-name) |correlation-name
    private String tableLocatorCorrelationName = "";                        // TABLE(table-locator-variabile LIKE table-name) |correlation-name
	
	// Campi per table reference XMLTABLE_EXPRESSION 
    private String xmltableExpressionValue = "";                            // XMLTABLE( .. value ..) correlation-clause
   
    // Campi per table reference JOINED_TABLE 
    private SqlFromTableReferenceJoinedTable joinedTable = null;			// table-reference INNER|LEFT|RIGHT|FULL |OUTER JOIN table-reference ON join-condition
    
    // Campi per correlation-clause valida tranne che per joined-table
	private SqlCorrelationClause correlationClause; 						// |AS |corr-name |(new-col-name1, ..., new-col-namen)
	private boolean isThereCorrelationClause = false;                   	//

	
	
	/**
	 * Costruttore vuoto
	 */
	public SqlFromTableReference() {
		super();
		al_singleTablePeriod = new  ArrayList<SqlPeriodSpecification> ();
		al_tableFunction = new ArrayList<SqlFromTableReferenceTableFunction> ();
		correlationClause = new SqlCorrelationClause();
	}

	/**
	 * Restituisce il tipo di table-reference codificato nella clausola <tt>FROM</tt><br>
	 * <p>
	 * @return the typeTableReference
	 */
	public EnumSqlTableReferenceType getTypeTableReference() {
		return typeTableReference;
	}

	/**
	 * Imposta il tipo di table-reference codificato nella clausola <tt>FROM</tt><br>
	 * <p>
	 * @param typeTableReference the typeTableReference to set
	 */
	public void setTypeTableReference(EnumSqlTableReferenceType typeTableReference) {
		this.typeTableReference = typeTableReference;
	}

	/**
	 * Restituisce il nome della tabella o della view per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Viene restituito il nome della tabella eventualmente qualifificato.
	 * <p>
	 * @return the singleTableEntityName
	 */
	public String getSingleTableEntityNameQualified() {
		return singleTableEntityName;
	}

	/**
	 * Imposta il nome della tabella o della view per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Viene restituito il nome della tabella eventualmente qualifificato.<br>
	 * <p>
	 * @param singleTableEntityName the singleTableEntityName to set
	 */
	public void setSingleTableEntityNameQualified(String singleTableEntityName) {
		this.singleTableEntityName = singleTableEntityName;
	}

	
	/**
	 * Restituisce l'owner di qualificazione della tabella o una stringa vuota se nopn presente.<br>
	 * <p>
	 * @return the singleTableOwner
	 */
	public String getSingleTableEntityOwner() {
		String owner = "";
		owner = getTableOwner(this.singleTableEntityName);
		return owner;
	}


	/**
	 * Restituisce il nome della tabella, codificata o meno con l'owner di qualificazione.<br>
	 * <p>
	 * @return the tableName
	 */
	public String getSingleTableEntityName() {
		String tableName = "";
		tableName = getTableName(this.singleTableEntityName);
		return tableName;
	}


	/**
	 * Restituisce le specificazioni di periodo per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <ul>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| AS OF VALUE</tt></li>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| FROM value1 TO value2</tt></li>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| BETWEEN value1 TO value2</tt></li>
	 * </ul>
	 * @return the al_singleTablePeriod
	 */
	public ArrayList<SqlPeriodSpecification> getSingleTablePeriod() {
		return al_singleTablePeriod;
	}

	/**
	 * Imposta le specificazioni di periodo per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <ul>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| AS OF VALUE</tt></li>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| FROM value1 TO value2</tt></li>
	 * <li> <tt>SYSTEM_TIME|BUSINESS_TIME| BETWEEN value1 TO value2</tt></li>
	 * </ul>
	 * @param al_singleTablePeriod the al_singleTablePeriod to set
	 */
	public void setSingleTablePeriod(ArrayList<SqlPeriodSpecification> al_singleTablePeriod) {
		this.al_singleTablePeriod = al_singleTablePeriod;
	}

	/**
	 * Restituisce la clausola di correlazione per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>|AS correlation-name </tt> </li>
	 * <li> <tt>|AS correlation-name ( new-col1, new-col2,..., new-coln) </tt> </li>
	 * <p>
	 * @return the correlationClause
	 */
	public SqlCorrelationClause getCorrelationClause() {
		return correlationClause;
	}

	/**
	 * Imposta la clausola di correlazione per table-reference <tt>single-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>|AS correlation-name </tt> </li>
	 * <li> <tt>|AS correlation-name ( new-col1, new-col2,..., new-coln) </tt> </li>
	 * <p>
	 * @param correlationClause the correlationClause to set
	 */
	public void setCorrelationClause(SqlCorrelationClause correlationClause) {
		this.correlationClause = correlationClause;
	}

	/**
	 * Restituisce se presente la clausola di correlazione per table-reference <tt>single-table</tt><br>
	 * <p>
	 * @return the isThereCorrelationClause
	 */
	public boolean isThereCorrelationClause() {
		return isThereCorrelationClause;
	}

	/**
	 * Imposta se presente la clausola di correlazione per table-reference <tt>single-table</tt><br>
	 * <p>
	 * @param isThereSingleTableCorrClause the isThereSingleTableCorrClause to set
	 */
	public void setCorrelationClause(boolean isThereCorrelationClause) {
		this.isThereCorrelationClause = isThereCorrelationClause;
	}

	/**
	 * Restituisce la full-select per table-reference <tt>nested-table-expression</tt><br>
	 * <p>
	 * @return the nestedTableFullSelect
	 */
	public SqlFullSelect getNestedTableFullSelect() {
		return nestedTableFullSelect;
	}

	/**
	 * Imposta la full-select per table-reference <tt>nested-table-expression</tt><br>
	 * <p>
	 * @param nestedTableFullSelect the nestedTableFullSelect to set
	 */
	public void setNestedTableFullSelect(SqlFullSelect nestedTableFullSelect) {
		this.nestedTableFullSelect = nestedTableFullSelect;
	}

	/**
	 * Restituisce il nome della function per table-reference <tt>table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name( ) )</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @return the tableFunctionName
	 */
	public String getTableFunctionName() {
		return tableFunctionName;
	}

	/**
	 * Imposta il nome della function per table-reference <tt>table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name ( ) )</tt> </li>
	 * <li> <tt>TABLE (function-name (expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name (expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @param tableFunctionName the tableFunctionName to set
	 */
	public void setTableFunctionName(String tableFunctionName) {
		this.tableFunctionName = tableFunctionName;
	}

	
	
	/**
	 * Restituisce le espressioni o i nomi delle transition-table per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name( ) )</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @return the al_tableFunction
	 */
	public ArrayList<SqlFromTableReferenceTableFunction> getTableFunctionElements() {
		return al_tableFunction;
	}

	/**
	 * Imposta le espressioni o i nomi delle transition-table per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name( ) )</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @param al_tableFunction the al_tableFunction to set
	 */
	public void setTableFunctionElements(ArrayList<SqlFromTableReferenceTableFunction> al_tableFunction) {
		this.al_tableFunction = al_tableFunction;
	}

	/**
	 * Restituisce il numero di cardinalita per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @return the tableFunctionCardinalityNumber
	 */
	public int getTableFunctionCardinalityNumber() {
		return tableFunctionCardinalityNumber;
	}

	/**
	 * Imposta il numero di cardinalita per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @param tableFunctionCardinalityNumber the tableFunctionCardinalityNumber to set
	 */
	public void setTableFunctionCardinalityNumber(int tableFunctionCardinalityNumber) {
		this.tableFunctionCardinalityNumber = tableFunctionCardinalityNumber;
	}

	/**
	 * Restituisce il numero di cardinalita per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @return the tableFunctionCardinalityMultiplierNumber
	 */
	public int getTableFunctionCardinalityMultiplierNumber() {
		return tableFunctionCardinalityMultiplierNumber;
	}

	/**
	 * Imposta il numero di cardinalita per table-reference <tt>nested-table-expression<</tt><br>
	 * <p>
	 * Si tratta di degli oggetti codificati fra parentesi dopo <tt>TABLE</tt>:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * @param tableFunctionCardinalityMultiplierNumber the tableFunctionCardinalityMultiplierNumber to set
	 */
	public void setTableFunctionCardinalityMultiplierNumber(int tableFunctionCardinalityMultiplierNumber) {
		this.tableFunctionCardinalityMultiplierNumber = tableFunctionCardinalityMultiplierNumber;
	}


	/**
	 * Restituisce se presente la clausola di cardinalita per table-reference <tt>table-function-reference<</tt><br>
	 * <p>
	 * @return the isThereTableFunctionCardinality
	 */
	public boolean isThereTableFunctionCardinality() {
		return isThereTableFunctionCardinality;
	}

	/**
	 * Imposta se presente la clausola di cardinalita per table-reference <tt>table-function-reference<</tt><br>
	 * <p>
	 * @param isThereTableFunctionCardinality the isThereTableFunctionCardinality to set
	 */
	public void setThereTableFunctionCardinality(boolean isThereTableFunctionCardinality) {
		this.isThereTableFunctionCardinality = isThereTableFunctionCardinality;
	}

	/**
	 * Restituisce se codificato <tt>FINAL</tt> per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>FINAL TABLE (INSERT statement)</tt> |correlation-clause</li>
	 * <li> <tt>FINAL TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>FINAL TABLE ((MERGE statement) |correlation-clause</tt> </li>
	 * <p>
	 * @return the isDataChangeFinal
	 */
	public boolean isDataChangeFinal() {
		return isDataChangeFinal;
	}

	/**
	 * Imposta se codificato <tt>FINAL</tt> per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>FINAL TABLE (INSERT statement)</tt> |correlation-clause</li>
	 * <li> <tt>FINAL TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>FINAL TABLE ((MERGE statement) |correlation-clause</tt> </li>
	 * <p>
	 * @param isDataChangeFinal the isDataChangeFinal to set
	 */
	public void setDataChangeFinal(boolean isDataChangeFinal) {
		this.isDataChangeFinal = isDataChangeFinal;
	}

	/**
	 * Restituisce se codificato <tt>OLD</tt> per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>OLD TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched DELETE statement) |correlation-clause</tt> </li>
	 * <p>
	 * 
	 * @return the isDataChangeOld
	 */
	public boolean isDataChangeOld() {
		return isDataChangeOld;
	}

	/**
	 * Imposta se codificato <tt>OLD</tt> per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>OLD TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched DELETE statement) |correlation-clause</tt> </li>
	 * <p>
	 * 
	 * @param isDataChangeOld the isDataChangeOld to set
	 */
	public void setDataChangeOld(boolean isDataChangeOld) {
		this.isDataChangeOld = isDataChangeOld;
	}

	/**
	 * Restituisce l'istruzione <tt>INSERT</tt>, <tt>UPDATE</tt>, <tt>DELETE</tt> o <tt>MERGE</tt><br>
	 * codificata per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>FINAL TABLE (INSERT statement)</tt> |correlation-clause</li>
	 * <li> <tt>FINAL TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>FINAL TABLE ((MERGE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched DELETE statement) |correlation-clause</tt> </li>
	 * <p>
	 * 
	 * @return the dataChangeStmt
	 */
	public InstructionSql getDataChangeStmt() {
		return dataChangeStmt;
	}

	/**
	 * Imposta l'istruzione <tt>INSERT</tt>, <tt>UPDATE</tt>, <tt>DELETE</tt> o <tt>MERGE</tt><br>
	 * codificata per table-reference <tt>data-change-table-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>FINAL TABLE (INSERT statement)</tt> |correlation-clause</li>
	 * <li> <tt>FINAL TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>FINAL TABLE ((MERGE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched UPDATE statement) |correlation-clause</tt> </li>
	 * <li> <tt>OLD TABLE (searched DELETE statement) |correlation-clause</tt> </li>
	 * <p>
	 * 
	 * @param dataChangeStmt the dataChangeStmt to set
	 */
	public void setDataChangeStmt(InstructionSql dataChangeStmt) {
		this.dataChangeStmt = dataChangeStmt;
	}


	/**
	 * Restituisce la variabile di table-locator codificata per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE ( table-locator-variable LIKE table-name ) |correlation-clause </tt> </li>
	 * <p>
	 * @return the tableLocatorHostVar
	 */
	public String getTableLocatorHostVar() {
		return this.tableLocatorHostVar;
	}

	/**
	 * Imposta la variabile di table-locator codificata per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE ( table-locator-variable LIKE table-name ) |correlation-clause </tt> </li>
	 * <p>
	 * @param tableLocatorHostVar the tableLocatorHostVar to set
	 */
	public void setTableLocatorHostVar (String tableLocatorHostVar){ 
		this.tableLocatorHostVar = tableLocatorHostVar;
	}

	/**
	 * Restituisce il nome della tabella <tt>LIKE</tt> codificata per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE ( table-locator-variable LIKE table-name ) |correlation-clause </tt> </li>
	 * <p>
	 * @return the tableLocatorEntityNameLike
	 */
	public String getTableLocatorEntityeNameLike() {
		return tableLocatorEntityNameLike;
	}

	/**
	 * Restituisce l'owner di qualificazione della tabella o una stringa vuota se nopn presente.<br>
	 * <p>
	 * @return the TableLocator owner
	 */
	public String getTableLocatorTableOwner() {
		String owner = "";
		owner = getTableOwner(this.tableLocatorEntityNameLike);
		return owner;
	}


	/**
	 * Imposta il nome della tabella <tt>LIKE</tt> codificata per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE ( table-locator-variable LIKE table-name ) |correlation-clause </tt> </li>
	 * <p>
	 * @param tableLocatorEntityNameLike the tableLocatorEntityNameLike to set
	 */
	public void setTableLocatorEntityNameLike(String tableLocatorEntityNameLike) {
		this.tableLocatorEntityNameLike = tableLocatorEntityNameLike;
	}

	/**
	 * Restituisce la clausola di correlazione per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>|AS correlation-name </tt> </li>
	 * <li> <tt>|AS correlation-name ( new-col1, new-col2,..., new-coln) </tt> </li>
	 * <p>
	 * @return the tableLocatorCorrelationName
	 */
	public String getTableLocatorCorrelationName() {
		return tableLocatorCorrelationName;
	}

	/**
	 * Imposta la clausola di correlazione per table-reference <tt>table-locator-reference</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>|AS correlation-name </tt> </li>
	 * <li> <tt>|AS correlation-name ( new-col1, new-col2,..., new-coln) </tt> </li>
	 * <p>
	 * @param tableLocatorCorrelationName the tableLocatorCorrelationName to set
	 */
	public void setTableLocatorCorrelationName(String tableLocatorCorrelationName) {
		this.tableLocatorCorrelationName = tableLocatorCorrelationName;
	}

	/**
	 * Restituisce l'espressione di <tt>XMLTABLE</tt> per table-reference <tt>xmltable-expression</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>XMLTABLE ( expression-value ) |correlation-clause </tt> </li>
	 * <p>
	 * @return the xmltableExpressionValue
	 */
	public String getXmltableExpressionValue() {
		return xmltableExpressionValue;
	}

	/**
	 * Imposta l'espressione di <tt>XMLTABLE</tt> per table-reference <tt>xmltable-expression</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * @param xmltableExpressionValue the xmltableExpressionValue to set
	 */
	public void setXmltableExpressionValue(String xmltableExpressionValue) {
		this.xmltableExpressionValue = xmltableExpressionValue;
	}

	/**
	 * Restituisce il descrittore della tabella in <tt>JOIN</> per table-reference <tt>joined-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>table-reference INNER JOIN table-reference ON join-condition</tt> </li>
	 * <li> <tt>table-reference LEFT|RIGHT|FULL |OUTER JOIN table-reference ON join-condition</tt> </li>
	 * <li> ( joined-table )</tt> </li>
	 * <p>
	 * @return the joinedTable
	 */
	public SqlFromTableReferenceJoinedTable getJoinedTable() {
		return joinedTable;
	}

	/**
	 * Imposta il descrittore della tabella in <tt>JOIN</> per table-reference <tt>joined-table</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>table-reference INNER JOIN table-reference ON join-condition</tt> </li>
	 * <li> <tt>table-reference LEFT|RIGHT|FULL |OUTER JOIN table-reference ON join-condition</tt> </li>
	 * <li> ( joined-table )</tt> </li>
	 * <p>
	 * @param joinedTable the joinedTable to set
	 */
	public void setJoinedTable(SqlFromTableReferenceJoinedTable joinedTable) {
		this.joinedTable = joinedTable;
	}


	/**
	 * Restituisce informazioni di identificazione sulle entities codificate nel table-reference</tt><br>
	 * <p>
	 * Per entity si intende una tabella, view, alias o sinoinimo.<br>
	 * <p>
	 * Viene restituito un ArrayList di stringhe a due dimensioni.<br>
	 * Ogni elemento contiene, a sua volta, un ArrayList di n elementi che sono, nell'ordine:
	 * <p>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name<br></li>
	 * <li>I successivi <b>n</b> elementi, se presenti, sono i new-column-names della correlation clause<br></li>
	 * </ul>
	 * <p>
	 * Se owner o correlation name non sono presenti, l'elemento corrispondente è stringa vuota.
	 * .
	 * @return the correlationClause
	 */
	public ArrayList<ArrayList<String>> getEntities() {
		
		ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
		ArrayList<ArrayList<String>> al_al_entity = null;
		ArrayList<ArrayList<String>> al_al_entityNested = null;
		ArrayList<SqlFromTableReference> al_entityJoin = null;
		ArrayList<String> al_entity = null;
		String entityName = "";
		String owner = "";
		String corrName = "";
		
		al_al_entity = new ArrayList<ArrayList<String>> ();
		
		// SINGLE_TABLE 
		if (typeTableReference == EnumSqlTableReferenceType.SINGLE_TABLE) {
			entityName = getTableName(this.singleTableEntityName);
			owner = getTableOwner(this.singleTableEntityName);
			corrName = this.correlationClause.getCorrName();
			al_entity = new ArrayList<String> ();
			al_entity.add(entityName);
			al_entity.add(owner);
			al_entity.add(corrName);
			for (String newColName : this.correlationClause.getNewColNames()) {
				al_entity.add(newColName);
			}
			al_al_entity.add(al_entity);
			return al_al_entity;
		}
			
		// NESTED_TABLE_EXPRESSION
		if (typeTableReference == EnumSqlTableReferenceType.NESTED_TABLE_EXPRESSION) {
			al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
			// Scan subselect, comunque annidate
			getSubselectRecursive(nestedTableFullSelect, al_sqlSubselect);
            for (SqlSubselectSelectInto sqlSubselet : al_sqlSubselect) {
            	al_al_entityNested = sqlSubselet.getEntities();
            	// Scan entities subselect
            	for (ArrayList<String> al_entityNested : al_al_entityNested) {
        			entityName = al_entityNested.get(0);
        			owner = al_entityNested.get(1);
        			corrName = al_entityNested.get(2);
        			al_entity = new ArrayList<String> ();
        			al_entity.add(entityName);
        			al_entity.add(owner);
        			al_entity.add(corrName);
         			for (String newColName : correlationClause.getNewColNames()) {
        				al_entity.add(newColName);
        			}
           			al_al_entity.add(al_entity);
				}
			}
			return al_al_entity;
		} 
		
		// TABLE_FUNCTION_REFERENCE
		if (typeTableReference == EnumSqlTableReferenceType.TABLE_FUNCTION_REFERENCE) {
			entityName = "";
			owner = "";
			corrName = correlationClause.getCorrName();
			al_entity = new ArrayList<String> ();
			al_entity.add(entityName);
			al_entity.add(owner);
			al_entity.add(corrName);
			for (String newColName : correlationClause.getNewColNames()) {
				al_entity.add(newColName);
			}
			al_al_entity.add(al_entity);
			return al_al_entity;	
		} 
		
		// DATA_CHANGE_TABLE_REFERENCE
		if (typeTableReference == EnumSqlTableReferenceType.DATA_CHANGE_TABLE_REFERENCE) {
			// TODO estrazione nome tabella/owner/ da insert/update/delete/merge
			entityName = "";
			owner = "";
			corrName = correlationClause.getCorrName();
			al_entity = new ArrayList<String> ();
			al_entity.add(entityName);
			al_entity.add(owner);
			al_entity.add(corrName);
			for (String newColName : correlationClause.getNewColNames()) {
				al_entity.add(newColName);
			}
			al_al_entity.add(al_entity);
			return al_al_entity;
		} 
		
		// TABLE_LOCATOR_REFERENCE
		if (typeTableReference == EnumSqlTableReferenceType.TABLE_LOCATOR_REFERENCE) {
			entityName = getTableName(tableLocatorEntityNameLike);
			owner = getTableOwner(tableLocatorEntityNameLike);
			corrName = tableLocatorCorrelationName;
			al_entity = new ArrayList<String> ();
			al_entity.add(entityName);
			al_entity.add(owner);
			al_entity.add(corrName);
			for (String newColName : correlationClause.getNewColNames()) {
				al_entity.add(newColName);
			}
			al_al_entity.add(al_entity);
			return al_al_entity;
		}
		
		// XMLTABLE_EXPRESSION
		if (typeTableReference == EnumSqlTableReferenceType.XMLTABLE_EXPRESSION) {
			entityName = "";
			owner = "";
			corrName = correlationClause.getCorrName();
			al_entity = new ArrayList<String> ();
			al_entity.add(entityName);
			al_entity.add(owner);
			al_entity.add(corrName);
			for (String newColName : correlationClause.getNewColNames()) {
				al_entity.add(newColName);
			}
			al_al_entity.add(al_entity);
			return al_al_entity;
		}

		// JOINED_TABLE
		if (typeTableReference == EnumSqlTableReferenceType.JOINED_TABLE) {
			al_entityJoin = new ArrayList<SqlFromTableReference> ();
			// Scan e, comunque annidate
			getJoinedTableRecursive(joinedTable, al_entityJoin);
			for (SqlFromTableReference tableReferenceJoin : al_entityJoin) {
				String entityNameJoin = "";
				entityNameJoin = tableReferenceJoin.getSingleTableEntityNameQualified();
       			entityName = getTableName(entityNameJoin);
    			owner =  getTableOwner(entityNameJoin);
    			corrName = tableReferenceJoin.getCorrelationClause().getCorrName();
    			al_entity = new ArrayList<String> ();
    			al_entity.add(entityName);
    			al_entity.add(owner);
    			al_entity.add(corrName);
    			// Nessun correlation name
       			al_al_entity.add(al_entity);
			}
		}
 
		return al_al_entity;
	}

	/*
	 * Accoda ricorsivamente le subselect della fullselect
	 */
	private void getSubselectRecursive(SqlFullSelect sqlFullSelect, ArrayList<SqlSubselectSelectInto> al_sqlSubselect) {
		
		if (sqlFullSelect.isSubSelect()) {
			al_sqlSubselect.add(sqlFullSelect.getSubselectInto());
		} else {
			getSubselectRecursive(sqlFullSelect.getFullSelect(), al_sqlSubselect);
		}

		// Scan linked To in UNION EXCEPT INTERSECT
		for (SqlFullSelectLinkedTo sqlFullselectLinkedTo : sqlFullSelect.getLinkedSubFullSelects()) {
			if (sqlFullselectLinkedTo.isLinkedToSubselect()) {
				al_sqlSubselect.add(sqlFullselectLinkedTo.getSubselect());
			} else {
				getSubselectRecursive(sqlFullselectLinkedTo.getFullSelect(), al_sqlSubselect);
			}
		}
	}

	/*
	 * Accoda ricorsivamente le entity di join
	 */
	private void getJoinedTableRecursive(SqlFromTableReferenceJoinedTable joinedTable, ArrayList<SqlFromTableReference> al_entityJoin) {
		
		if (joinedTable.isJoinedTableInvoked()) {
			getJoinedTableRecursive(joinedTable.getJoinedTableInvoked(), al_entityJoin);
		} else {
			al_entityJoin.add(joinedTable.getTableReference());
			al_entityJoin.add(joinedTable.getTableReferenceJoin());
		}
	}

	
	
	/*
	 * Restituisce il nome della tabella a fronte di un nome qualificato o meno del tipo:
	 * 
	 * table
	 * owner.table
	 * serverId.owner.table
	 * 
	 */
	private String getTableName(String tableNameQualified) {
		
		String tableName = "";
		int i1 = 0;
		int i2 = 0;
		
		tableName = tableNameQualified;
		
		// table
		i1 = tableNameQualified.indexOf(".");
		if (i1 < 0) {
			return tableName;
		}
		
		// owner.table
		i2 = tableNameQualified.indexOf(".", i1 + 1);
		if (i2 < 0) {
			tableName = tableNameQualified.substring(i1 + 1);
			return tableName;
		}
		
		// serverId.owner.table
		tableName = tableNameQualified.substring(i2 + 1);
		
		return tableName;
	}

	/*
	 * Restituiscel'owner della tabella a fronte di un nome o meno qualificato del tipo:
	 * 
	 * table
	 * owner.table
	 * serverId.owner.table
	 * 
	 */
	private String getTableOwner(String tableNameQualified) {

		String owner = "";
		int i1 = 0;
		int i2 = 0;
		
		owner = "";
		
		// table
		i1 = tableNameQualified.indexOf(".");
		if (i1 < 0) {
			return owner;
		}
		
		// owner.table
		i2 = tableNameQualified.indexOf(".", i1 + 1);
		if (i2 < 0) {
			owner = tableNameQualified.substring(0, i1);
			return owner;
		}
		
		// serverId.owner.table
		owner = tableNameQualified.substring(i1 + 1, i2);
		
		return owner;
	}


}
