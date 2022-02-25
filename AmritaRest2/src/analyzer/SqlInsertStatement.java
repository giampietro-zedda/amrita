
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlInsertStatement
 * </h1>
 * <p>
 * Descrive una INSERT statement che inserisce una o più righe di una tabella.<br> 
 * <p>
 * Gli oggetti utilizzati per descrivere lo statement Select sono:<br>
 * <p>
 * {@link SqlExpression}<br>
 * {@link SqlSearchConditions}<br> 
 * <p>
 * <tt>SqlSearchConditions</tt> descrive le codizioni di search delle righe da aggiornare.
 * <p>
 * <b>metodi di servizio</b>
 * <ul>
 * 
 * <li><tt>getEntities()</tt></li><br>
 * Restituisce i nomi delle tabelle o view utilizzate.<br>
 * 
 * <li><tt>getEntitiesColumns()</tt></li><br>
 * Restituisce tabella e  coppie di nomi colonne e corrispondenti variabili host.<br>
 * 
 * <li><tt>getHostVars()</tt></li><br>
 * Restituisce tutte le variabili host presenti, a qualsiasi livello.<br>
 * 
 * <li><tt>getHostVarsWhere()</tt></li><br>
 * Restituisce le variabili host presenti nella clausola where principale.<br>
 * 
 * <li><tt>getHostVarsWhereByEntity()</tt></li><br>
 * Restituisce tutte le variabili host presenti in tutte le clausola where,.<br>
 * anche di scalar-full-select comunque anndate, per ogni entity.<br>
 * 
 * <li><tt>getConstantsAlphanumeric()</tt></li><br>
 * Restituisce tutte le literal alfanumeriche presenti<br>
 * 
 * <li><tt>getConstantsNumeric()</tt></li><br>
 * Restituisce tutte le literal numeriche presenti<br>
 * </ul>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlInsertStatement 
*/

public class SqlInsertStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Descrizione corpo istruzione
	private String entityNameQualified = "";                    				// INSERT INTO entityName ... può essere qualificato
	private ArrayList<String> al_columnName = null;				        		// INSERT INTO entityName |( col1, ..., coln ) ...
	private ArrayList<String> al_columnInclude = null;							// INSERT INTO entityName ... |INCLUDE (col1 data-type1, ..., coln data-typen) ...
	private ArrayList<SqlExpression> al_valueExpression = null;     			// INSERT INTO entityName ... VALUES expression|NULL|DEFAULT|( ... )
	private ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;// WITH commonTableExpression, ...
	private SqlFullSelect fullSelect = null;									// INSERT INTO entityName ... full-select
    private String forRowHostVar = "";                          				// INSERT INTO entityName ... VALUES ... FOR ROW host-variable    OF ROWSET
    private int forRowNumber = 0;                          	    				// INSERT INTO entityName ... VALUES ... FOR ROW integer-constant OF ROWSET
    private String includeColumnsValue = "";                    				// INSERT INTO entityName ... INCLUDE ( value ) ...
    private String isolation = "";                              				// WITH RR|RS|CS|UR
    private int queryno = 0;                                    				// QUERYNO n
     
    // Opzioni presenti
    private boolean isValuesByList = false;                   	    			// VALUES ( ... )
    private boolean isValuesByExpression = false;                   			// VALUES expression
    private boolean isValuesByFullselect = false;                   			// SELECT * FROM table 
    private boolean isValuesByNull = false;                           			// VALUES NULL
    private boolean isValuesByDefault = false;                        			// VALUES DEFAULT
    private boolean isValuesByHostVarArray = false;                   			// VALUES host-var-array
    private boolean isWithCommonTableExpression = false;                   		// WITH table-identifier1 (cols) AS (fullselect) , ..., table-identifiern (cols) AS (fullselect)
    private boolean isIncludeColumnsValue = false;                   		    // INCLUDE ( col data-type, ... )
    private boolean isOverridingUserValue = false;                  			// OVERRIDING USER VALUE
    private boolean isQueryno = false;                         	 				// QUERYNO n
    private boolean isSkipLockData = false;                     				// SKIP LOCK DATA
    private boolean isIsolation = false;                        				// WITH RR|RS|CS|UR
    private boolean isForRowsHostVar = false;                       			// FOR ROW host-variable    OF ROWSET
    private boolean isForRowsNumber = false;                        			// FOR ROW integer-constant OF ROWSET
    private boolean isAtomic = false;                          					// VALUES ... ATOMIC
    private boolean isNotAtomicContinue = false;                    			// VALUES ... NOT ATOMIC CONTINUE ON SQLEXCEPTION
    
    
	/**
	 * Costruttore 
	 */
	public SqlInsertStatement() {
		super();

		al_columnName = new ArrayList<String> ();
		al_columnInclude = new ArrayList<String> (); 
		al_valueExpression = new ArrayList<SqlExpression> ();
		al_commonTableExpression = new ArrayList<SqlCommonTableExpression> (); 
	}




	/**
	 * Restituisce il nome eventualmente qualificato della tabella o view da aggiornare come indicato nello statement<br>
	 * <p>
     * <tt>UPDATE entityName WHERE search-condition</tt>
	 * <p>
	 * @return the entityNameQualified
	 */
	public String getEntityNameQualified() {
		return entityNameQualified;
	}




	/**
	 * Imposta il nome eventualmente qualificato della tabella o view da aggiornare come indicato nello statement<br>
	 * <p>
     * <tt>UPDATE entityName WHERE search-condition</tt>
	 * <p>
	 * @param entityNameQualified the entityNameQualified to set
	 */
	public void setEntityNameQualified(String entityNameQualified) {
		this.entityNameQualified = entityNameQualified;
	}

	
	/**
	 * Restituisce i nomi delle colonne che verranno inserite dallo statement.<br>
	 * <p>
	 * @return the al_columnName
	 */
	public ArrayList<String> getColumnsName() {
		return al_columnName;
	}


	/**
	 * Imnposta i nomi delle colonne che verranno inserite dallo statement.<br>
	 * <p>
	 * @param alColumnName the al_columnName to set
	 */
	public void setColumnsName(ArrayList<String> al_columnName) {
		this.al_columnName = al_columnName;
	}




	/**
	 * Restituisce il contenuto fra parentesi della clausola <tt>INCLUDE</tt><br>
	 * <p>
	 * <tt>INSERT INTO ... INCLUDE ( col1 data-type1, col2 data-type2, .. )</tt>
	 * <p>
	 * @return the al_columnInclude
	 */
	public ArrayList<String> getColumnsInclude() {
		return al_columnInclude;
	}




	/**
	 * Imposta il contenuto fra parentesi della clausola <tt>INCLUDE</tt><br>
	 * <p>
	 * <tt>INSERT INTO ... INCLUDE ( col1 data-type1, col2 data-type2, .. )</tt>
	 * <p>
	 * @param al_columnInclude the al_columnInclude to set
	 */
	public void setColumnsInclude(ArrayList<String> al_columnInclude) {
		this.al_columnInclude = al_columnInclude;
	}

	
	
	/**
	 * Restituisce le espressioni che costituiscono i valori da inserire per ogni colonna.<br>
	 * <br>
	 * Ciò a fronte di costrutti del tipo:<br>
	 * <p>
	 * <tt>INSERT INTO ... VALUES (expr1, expr2, ... exprn) </tt>
	 * <tt>INSERT INTO ... VALUES  expr </tt>
     * <p>
     * <tt>DEFAULT</tt> e <tt>NULL</tt> sono gestiti come particolari elementi di espressione.<br>
     * <p>
     * In caso di VALUES di una espressione l'insieme di espressioni restituito è di un solo elemento.<br>
     * <p>
     * 
	 * @return the al_valueExpression
	 */
	public ArrayList<SqlExpression> getValueExpressions() {
		return al_valueExpression;
	}




	/**
	 * Imposta le espressioni che costituiscono i valori da inserire per ogni colonna.<br>
	 * <br>
	 * Ciò a fronte di costrutti del tipo:<br>
	 * <p>
	 * <tt>INSERT INTO ... VALUES (expr1, expr2, ... exprn) </tt>
	 * <tt>INSERT INTO ... VALUES  expr </tt>
     * <p>
     * <tt>DEFAULT</tt> e <tt>NULL</tt> sono gestiti come particolari elementi di espressione.<br>
     * <p>
     * In caso di VALUES di una espressione l'insieme di espressioni restituito è di un solo elemento.<br>
     * <p>
     * 
	 * @param al_valueExpression the al_valueExpression to set
	 */
	public void setValueExpressions(ArrayList<SqlExpression> al_valueExpression) {
		this.al_valueExpression = al_valueExpression;
	}




	/**
	 * Restituisce i <tt>common-table-expression</tt> codificati  dopo la parola chiave <tt>WITH</tt><br>
	 * <p>
	 * @return the al_commonTableExpression
	 */
	public ArrayList<SqlCommonTableExpression> getCommonTableExpressions() {
		return al_commonTableExpression;
	}


	/**
	 * Imposta i <tt>common-table-expression</tt> codificati dopo la parola chiave <tt>WITH</tt><br>
	 * <p>
	 * @param al_commonTableExpression the al_commonTableExpression to set
	 */
	public void setCommonTableExpressions(ArrayList<SqlCommonTableExpression> al_commonTableExpression) {
		this.al_commonTableExpression = al_commonTableExpression;
	}


	/**
	 * Restituisce la <tt>full-select</tt> che rappresenta l'insieme di righe da inserire.<br>
	 * <p>
	 * <br>
	 * Ciò a fronte di costrutti del tipo:<br>
	 * <p>
	 * <tt>INSERT INTO ...SELECT * FROM table WHERE ... </tt>
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta la <tt>full-select</tt> che rappresenta l'insieme di righe da inserire.<br>
	 * <p>
	 * <br>
	 * Ciò a fronte di costrutti del tipo:<br>
	 * <p>
	 * <tt>INSERT INTO ...SELECT * FROM table WHERE ... </tt>
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.fullSelect = fullSelect;
	}



	/**
	 * Restituisce il semplice nome della tabella o view da aggiornare senza qualificazione.<br>
	 * <p>
	 * 
	 * <tt>UPDATE owner.entityName WHERE search-condition</tt>
	 * <p>
	 * @return the entityName 
	 */
	public String getEntityName() {
		return getTableName(this.entityNameQualified);
	}

	/**
	 * Restituisce il semplice nome della tabella o view da aggiornare senza qualificazione.<br>
	 * <p>
	 * 
	 * <tt>UPDATE owner.entityName WHERE search-condition</tt>
	 * <p>
	 * @return the owner
	 */
	public String getEntityOwner() {
		return getTableOwner(this.entityNameQualified);
	}




	/**
	 * Restituisce il contenuto fra parentesi dell'opzione INCLUDE.<br>
	 * <p>
	 * <tt>UPDATE ... INCLUDE ( col data-type, col data-type, ... ) ...</tt>
	 * <p>
	 * @return the includeColumnsValue
	 */
	public String getIncludeColumnsValue() {
		return includeColumnsValue;
	}




	/**
	 * Imposta il contenuto fra parentesi dell'opzione INCLUDE.<br>
	 * <p>
	 * <tt>UPDATE ... INCLUDE ( col data-type, col data-type, ... ) ...</tt>
	 * <p>
	 * @param includeColumnsValue the includeColumnsValue to set
	 */
	public void setIncludeColumnsValue(String includeColumnsValue) {
		this.includeColumnsValue = includeColumnsValue;
	}





	/**
	 * Restituisce la variabile host in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the forRowHostVar
	 */
	public String getForRowHostVar() {
		return forRowHostVar;
	}




	/**
	 * Imposta la variabile host in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param forRowHostVar the forRowHostVar to set
	 */
	public void setForRowHostVar(String forRowHostVar) {
		this.forRowHostVar = forRowHostVar;
	}




	/**
	 * Restituisce il numero di riga in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the forRowNumber
	 */
	public int getForRowNumber() {
		return forRowNumber;
	}




	/**
	 * Imposta il numero di riga in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param forRowNumber the forRowNumber to set
	 */
	public void setForRowNumber(int forRowNumber) {
		this.forRowNumber = forRowNumber;
	}




	/**
	 * Restituisce se presente la clausola include-column.<br>
	 * <p>
	 * <tt>INSERT INTO table ... INCLUDE ( col1 data-type1, col2 dara-type2, ... )</tt>
	 * <p>
	 * @return the isIncludeColumnsValue
	 */
	public boolean isIncludeColumnsValue() {
		return isIncludeColumnsValue;
	}




	/**
	 * Imposta se presente la clausola include-column.<br>
	 * <p>
	 * <tt>INSERT INTO table ... INCLUDE ( col1 data-type1, col2 dara-type2, ... )</tt>
	 * <p>
	 * @param isIncludeColumnsValue the isIncludeColumnsValue to set
	 */
	public void setIncludeColumns(boolean isIncludeColumnsValue) {
		this.isIncludeColumnsValue = isIncludeColumnsValue;
	}





	/**
	 * Restituisce se i valori da inserire sono espressi da un elenco fra parentesi.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES (expr1, NULL, DEFAULT, expr2, ... )</tt>
     * <p>
	 * @return the isValuesByList
	 */
	public boolean isValuesByList() {
		return isValuesByList;
	}




	/**
	 * Imposta se i valori da inserire sono espressi da un elenco fra parentesi.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES (expr1, NULL, DEFAULT, expr2, ... )</tt>
     * <p>
	 * @param isValuesByList the isValuesByList to set
	 */
	public void setValuesByList(boolean isValuesByList) {
		this.isValuesByList = isValuesByList;
	}




	/**
	 * Restituisce se il valore da inserire nelle colonne è espresso da una espressione.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES expression ...</tt>
     * <p>
	 * @return the isValuesByExpression
	 */
	public boolean isValuesByExpression() {
		return isValuesByExpression;
	}




	/**
	 * Imposta se il valore da inserire nelle colonne è espresso da una espressione.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES expression ...</tt>
     * <p>
	 * @param isValuesByExpression the isValuesByExpression to set
	 */
	public void setValuesByExpression(boolean isValuesByExpression) {
		this.isValuesByExpression = isValuesByExpression;
	}




	/**
	 * Restituisce se i valori da inserire nelle colonne sono espressi da una full-select.<br>
	 * <p>
     * <tt>INSERT INTO table ... SELECT * FROM table2 WHERE ...</tt>
     * <p>
	 * @return the isValuesByFullselect
	 */
	public boolean isValuesByFullselect() {
		return isValuesByFullselect;
	}




	/**
	 * Imposta se i valori da inserire nelle colonne sono espressi da una full-select.<br>
	 * <p>
     * <tt>INSERT INTO table ... SELECT * FROM table2 WHERE ...</tt>
     * <p>
	 * @param isValuesByFullselect the isValuesByFullselect to set
	 */
	public void setValuesByFullselect(boolean isValuesByFullselect) {
		this.isValuesByFullselect = isValuesByFullselect;
	}




	/**
	 * Restituisce se il valore da inserire nelle colonne è espresso dalla costante figurativa NULL.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES NULL ...</tt>
     * <p>
	 * @return the isValuesByNull
	 */
	public boolean isValuesByNull() {
		return isValuesByNull;
	}




	/**
	 * Imposta se il valore da inserire nelle colonne è espresso dalla costante figurativa NULL.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES NULL ...</tt>
     * <p>
	 * @param isValuesByNull the isValuesByNull to set
	 */
	public void setValuesByNull(boolean isValuesByNull) {
		this.isValuesByNull = isValuesByNull;
	}




	/**
	 * Restituisce se il valore da inserire nelle colonne deve il DEFAULT stabilito nella CREATE TABLE.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES DEFAULT ...</tt>
     * <p>
	 * @return the isValuesByDefault
	 */
	public boolean isValuesByDefault() {
		return isValuesByDefault;
	}




	/**
	 * Imposta se il valore da inserire nelle colonne deve il DEFAULT stabilito nella CREATE TABLE.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES DEFAULT ...</tt>
     * <p>
	 * @param isValuesByDefault the isValuesByDefault to set
	 */
	public void setValuesByDefault(boolean isValuesByDefault) {
		this.isValuesByDefault = isValuesByDefault;
	}




	/**
	 * Restituisce se il valore da inserire nelle colonne è codificato in un host-variable-array.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES host-variable-array ...</tt>
     * <p>
	 * @return the isValuesByHostVarArray
	 */
	public boolean isValuesByHostVarArray() {
		return isValuesByHostVarArray;
	}




	/**
	 * Imposta se il valore da inserire nelle colonne è codificato in un host-variable-array.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES host-variable-array ...</tt>
     * <p>
	 * @param isValuesByHostVarArray the isValuesByHostVarArray to set
	 */
	public void setValuesByHostVarArray(boolean isValuesByHostVarArray) {
		this.isValuesByHostVarArray = isValuesByHostVarArray;
	}




	/**
	 * Restituisce se codificata clausola WITH common-table-expressions.<br>
	 * <p>
     * <tt>INSERT INTO table ... WITH table-identifier1 (cols) AS (fullselect) , ..., table-identifiern (cols) AS (fullselect)</tt>
     * <p>
	 * @return the isWithCommonTableExpression
	 */
	public boolean isWithCommonTableExpression() {
		return isWithCommonTableExpression;
	}




	/**
	 * Imposta se codificata clausola WITH common-table-expressions.<br>
	 * <p>
     * <tt>INSERT INTO table ... WITH table-identifier1 (cols) AS (fullselect) , ..., table-identifiern (cols) AS (fullselect)</tt>
     * <p>
	 * @param isWithCommonTableExpression the isWithCommonTableExpression to set
	 */
	public void setWithCommonTableExpression(boolean isWithCommonTableExpression) {
		this.isWithCommonTableExpression = isWithCommonTableExpression;
	}




	/**
	 * Restituisce se presente l'opzione di override del valore.<br>
	 * <p>
     * <tt>INSERT INTO table ... OVERRIDING USER VALUE ...</tt>
     * <p>
	 * @return the isOverridingUserValue
	 */
	public boolean isOverridingUserValue() {
		return isOverridingUserValue;
	}




	/**
	 * Imposta se presente l'opzione di override del valore.<br>
	 * <p>
     * <tt>INSERT INTO table ... OVERRIDING USER VALUE ...</tt>
     * <p>
	 * @param isOverridingUserValue the isOverridingUserValue to set
	 */
	public void setOverridingUserValue(boolean isOverridingUserValue) {
		this.isOverridingUserValue = isOverridingUserValue;
	}




	/**
	 * Restituisce se presente l'opzione di not atomic su errori sql.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES ... NOT ATOMIC CONTINUE ON SQLEXCEPTION ...</tt>
     * <p>
	 * @return the isNotAtomicContinue
	 */
	public boolean isNotAtomicContinue() {
		return isNotAtomicContinue;
	}




	/**
	 * Imposta se presente l'opzione di not atomic su errori sql.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES ... NOT ATOMIC CONTINUE ON SQLEXCEPTION ...</tt>
     * <p>
	 * @param isNotAtomicContinue the isNotAtomicContinue to set
	 */
	public void setNotAtomicContinue(boolean isNotAtomicContinue) {
		this.isNotAtomicContinue = isNotAtomicContinue;
	}




	/**
	 * Restituisce se presente l'opzione di atomic.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES ... ATOMIC ...</tt>
     * <p>
	 * @return the isAtomic
	 */
	public boolean isAtomic() {
		return isAtomic;
	}




	/**
	 * Imposta se presente l'opzione di atomic.<br>
	 * <p>
     * <tt>INSERT INTO table ... VALUES ... ATOMIC ...</tt>
     * <p>
	 * @param isAtomic the isAtomic to set
	 */
	public void setAtomic(boolean isAtomic) {
		this.isAtomic = isAtomic;
	}




	/**
	 * Restituisce se presente l'opzione INCLUDE.<br>
	 * <p>
     * <tt>INSERT INTO table ...INCLUDE ( col1 data-type1, col2 data-type2, ...) ...</tt>
     * <p>
	 * @param isIncludeColumnsValue the isIncludeColumnsValue to set
	 */
	public void setIncludeColumnsValue(boolean isIncludeColumnsValue) {
		this.isIncludeColumnsValue = isIncludeColumnsValue;
	}




	/**
	 * Imposta se presente l'opzione INCLUDE.<br>
	 * <p>
     * <tt>INSERT INTO table ...INCLUDE ( col1 data-type1, col2 data-type2, ...) ...</tt>
     * <p>
	 * @param isForRowsHostVar the isForRowsHostVar to set
	 */
	public void setForRowsHostVar(boolean isForRowsHostVar) {
		this.isForRowsHostVar = isForRowsHostVar;
	}




	/**
	 * Restituisce il livello di isolation che può essere <tt>RR RS CS UR</tt>.<br>
	 * <p>
	 * @return the isolation
	 */
	public String getIsolation() {
		return isolation;
	}


	/**
	 * Imposta il livello di isolation che può essere <tt>RR RS CS UR</tt>.<br>
	 * <p>
	 * @param isolation the isolation to set
	 */
	public void setIsolation(String isolation) {
		this.isolation = isolation;
	}


	/**
	 * Restituisce il query number imposta da <tt>QUERYNO n</tt></tt>.<br>
	 * <p>
	 * Utilizzato nelle DESCRIBE.<br>
	 * <p>
	 * @return the queryno
	 */
	public int getQueryno() {
		return queryno;
	}


	/**
	 * Imposta il query number imposta da <tt>QUERYNO n</tt></tt>.<br>
	 * <p>
	 * Utilizzato nelle DESCRIBE.<br>
	 * <p>
	 * @param queryno the queryno to set
	 */
	public void setQueryno(int queryno) {
		this.queryno = queryno;
	}



	/**
	 * Restituisce se presente <tt>QUERYNO n</tt>
	 * <p>
	 * @return the isQueryno
	 */
	public boolean isQueryno() {
		return isQueryno;
	}


	/**
	 * Imposta se presente <tt>QUERYNO n</tt>
	 * <p>
	 * @param isQueryno the isQueryno to set
	 */
	public void setQueryno(boolean isQueryno) {
		this.isQueryno = isQueryno;
	}


	/**
	 * Restituisce se presente <tt>SKIP LOCKED DATA</tt>
	 * <p>
	 * @return the isSkipLockData
	 */
	public boolean isSkipLockData() {
		return isSkipLockData;
	}


	/**
	 * Imposta se presente <tt>SKIP LOCKED DATA</tt>
	 * <p>
	 * @param isSkipLockData the isSkipLockData to set
	 */
	public void setSkipLockData(boolean isSkipLockData) {
		this.isSkipLockData = isSkipLockData;
	}


	/**
	 * Restituisce se presente definizione di isolation level WITH a <tt> RR, RS, CS o UR</tt>
	 * <p>
	 * @return the isIsolation
	 */
	public boolean isIsolation() {
		return isIsolation;
	}


	/**
	 * Restituisce se presente definizione di isolation level WITH a <tt> RR, RS, CS o UR</tt>
	 * <p>
	 * @param isIsolation the isIsolation to set
	 */
	public void setIsolation(boolean isIsolation) {
		this.isIsolation = isIsolation;
	}





	/**
	 * Restituisce se presente FOR ROW hostvar in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the isForRowsHostVar
	 */
	public boolean isForRowsHostVar() {
		return isForRowsHostVar;
	}




	/**
	 * Imposta se presente FOR ROW hostvar in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param isForRowsHostVar the isForRowsHostVar to set
	 */
	public void setForRowHostVar(boolean isForRowsHostVar) {
		this.isForRowsHostVar = isForRowsHostVar;
	}




	/**
	 * Restituisce se presente FOR ROW number in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the isForRowsNumber
	 */
	public boolean isForRowsNumber() {
		return isForRowsNumber;
	}




	/**
	 * Imposta se presente FOR ROW number in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>UPDATE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param isForRowNumber the isForRowNumber to set
	 */
	public void setForRowsNumber(boolean isForRowsNumber) {
		this.isForRowsNumber = isForRowsNumber;
	}




	/**
	 * Restituisce informazioni sulle tabelle o sulle view utilizzate dall'istruzione dichiarate nelle clausole <tt>FROM</tt><br>
	 * Si prendono in considerazione tutte le clausole FROM comunque annidate in fullselect e subselect.<br>
	 * <p>
	 * Le informazioni comprendono il nome, l'owner di qualificazione e il correlation-name<br>
	 * Se il correlation-name o l'owner l'elemento corrispondente è impostato a stringa vuota.<br>
	 * <p>
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, a sua volta una ArrayList, contiene 3 + <b>n</b> elementi ed è relativo a una tabella o view,<br>
	 * dove <b>n</b> è il numero di AS new-column-names della correlation clause.
	 * <p>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name<br></li>
	 * <li>I successivi <b>n</b> elementi, se presenti, sono i new-column-names della correlation clause<br></li>
	 * </ul>
	 * <p>
	 * @return the tables o view
	 */
	public ArrayList<ArrayList<String>> getEntities() {
		
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<ArrayList<String>> al_al_entity = null;
		ArrayList<String> al_entity = null;

		al_al_entity = new ArrayList<ArrayList<String>> ();
		
		// Entity principale in aggiornamento
		al_entity = new ArrayList<String> ();
		al_entity.add(getEntityName());
		al_entity.add(getEntityOwner());
		al_entity.add("");								// correlation-name
		al_al_entity.add(al_entity);
		
		// Estrazione entity dichiarate in scalar-full-select di expression di VALUE
		for (SqlExpression sqlExpression : al_valueExpression) {
			// Scan elementi
			for (SqlExpressionElement sqlExpressionElement : sqlExpression.getElements()) {
				// Interessa solo scalar-full-select
				if (sqlExpressionElement.getTypeElement() != EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					continue;
				}
				al_subselect = sqlExpressionElement.getFullSelect().getAllSubselect();
				// Scan subselect individuate
				for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
					al_al_entity.addAll(sqlSubselect.getEntities());
				}
			}
		}
		return al_al_entity;
	}

	/**
	 * Restituisce informazioni dettagliate sulle colonne delle tabelle o view dichiarate nella clausola <tt>SET</tt><br>
	 * e in tutte le SRLECT presenti.<br>
	 * Si prendono in considerazione tutte le clausole FROM comunque annidate in fullselect e subselect.<br>
     *
	 * Si tratta di statement della forma:<br>
	 * <p>
	 * <ul>
	 * 	<li>UPDATE table  SET col = value</li>
	 * </ul>
	 * 
	 * <p>
	 * Le informazioni comprendono il nome della tabella, il suo owner, il suo correlation name<br>
	 * e il nome della colonna, con l'eventuale new-column-name specificato con <tt>AS new-column-Name</tt> dopo la colonna.<br>
	 * Se le informazioni non sono disponibili l'elemento corrispondente è impostato a stringa vuota.<br>
	 * <p>
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, una ArrayList, contiene a sua volta <b>3n+3</b> elementi ed è relativo a una tabella o view,
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt><br>
	 * <p>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nello statement UPDATE e nelle clausole <tt>FROM</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name e, se presente, la colonna è qualificata con questo<br></li>
	 * <li>I successivi elementi sono <b>n</b> sequenze di doppiette con il seguente significato:
	 *    <ul>
	 *       <li>Nome della colonna</li>
	 *       <li>new-column-name impostato con <tt>AS new-column-Name</tt></li>
	 *       <li><b>"E"</b> se colonna dichiarata esplicitamente e <b>"I"</b> se implicitamente, con <b>*</b> o <b>.*</b></tt></li>
	 *    </ul>
	 * </li>
	 * </ul>
	 * <p>
     * 
	 * @return the info columns by entity
	 */
	public ArrayList<ArrayList<String>> getEntitiesColumns() {
		
		ArrayList<ArrayList<String>> al_al_columnsByEntity = null;
		ArrayList<String> al_columnsByEntity = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_al_columnsByEntity = new ArrayList<ArrayList<String>> ();
		
        // Colonne principali dichiarate, come espressioni di un solo elemento
		for (String columnName : al_columnName) {

			al_columnsByEntity = new ArrayList<String> ();
			al_columnsByEntity.add(this.getEntityName());
			al_columnsByEntity.add(this.getEntityOwner());
			al_columnsByEntity.add("");											// Correlation-name
			al_columnsByEntity.add(columnName);
			al_columnsByEntity.add("");											// new col name
			al_columnsByEntity.add("E");										// Explicit column
			al_al_columnsByEntity.add(al_columnsByEntity);
		}
		
		// Colonne in espressioni di valori o in scalar-full-select
		for (SqlExpression sqlExpression : al_valueExpression) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementValue : sqlExpression.getElements()) {
				// Colonna, sempre dei entity in update
				if (sqlExpressionElementValue.getTypeElement() == EnumSqlExpressionElementType.COLUMN_NAME) {
					al_columnsByEntity = new ArrayList<String> ();
					al_columnsByEntity.add(this.getEntityName());
					al_columnsByEntity.add(this.getEntityOwner());
					al_columnsByEntity.add("");											// Correlation-name
					al_columnsByEntity.add(sqlExpressionElementValue.getColumnName());
					al_columnsByEntity.add("");											// new col name
					al_columnsByEntity.add("E");										// Explicit column
					al_al_columnsByEntity.add(al_columnsByEntity);
					continue;
				}
				// Scalar-full-select
				if (sqlExpressionElementValue.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementValue.getFullSelect().getAllSubselect();
					// Scan subselect individuate
					for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
						al_al_columnsByEntity.addAll(sqlSubselect.getEntitiesColumns());
					}
					continue;
				}
			} // end-for elementi expression
			
		} // end.for colonne assegnate
		
		return al_al_columnsByEntity;
	}


	/**
	 * Restituisce le variabili host in <tt>input</tt>, che iniziano con <tt>:</tt> <br>
	 * dichiarate nella <tt>UPDATE ... WHERE col1 = :hostVar1 AND col2 = hostVar2 ...</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * Vengono restituite anche tutte le variabili host di tutte le where delle eventuali full-select<br>
	 * presenti nel costrutto SET di assegnazione.<br>
	 * <p>
	 * @return the al_hostVarWhere
	 */
	public ArrayList<String> getHostVarsWhere() {
		
		ArrayList<String> al_hostVarWhere = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_hostVarWhere = new ArrayList<String> ();
		
		// Variabili host nelle scalar-full-select assegnate
		for (SqlExpression sqlExpression : al_valueExpression) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Scalar-full-select
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementRight.getFullSelect().getAllSubselect();
					// Scan subselect individuate
					for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
						al_hostVarWhere.addAll(sqlSubselect.getHostVarsWhere());
					}
					continue;
				}
				
			} // end-for elementi expression
			
		} // end-for espressioni assegnate
		
		return al_hostVarWhere;
	}



	/**
	 * Fornisce i nomi delle variabili host in input utilizzate con l'informazione della tabella di appartenenza.<br>
	 * <p>
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, una ArrayList, contiene a sua volta <b>n+1</b> elementi ed è relativo a una tabella o view,<br>
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SET</tt> di tt>UPDATE</tt> per la tabella<br>
	 * Vengono inoltre restituite tutte le variabili host in select di scalar-full-select assegnate ale colonne <br>
	 * della rabella in UPDATE.<br>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella in <tt>UPDATE</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name<br></li>
	 * <li>I successivi <b>n</b> elementi sono i nomi delle variabili host utilizzate in input. <br>
	 *     Si tratta di variabili host dichiarate in clausole <tt>WHERE</tt>, funzioni e altri costrutti.<br>
     *     Le variabili host sono restituite senza i due punti iniziali.<br></li>
	 * </ul>
	 * 
	 * <p>
     * 
     * 
	 * @return the host vars by table
	 */
	public ArrayList<ArrayList<String>> getHostVarsWhereByEntity() {
		
		ArrayList<ArrayList<String>> al_al_hostVar = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// Variabili host nelle scalar-full-select assegnate
		for (SqlExpression sqlExpression : al_valueExpression) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Scalar-full-select
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementRight.getFullSelect().getAllSubselect();
					// Scan subselect individuate
					for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
						al_al_hostVar.addAll(sqlSubselect.getHostVarsIntoByEntity());
					}
					continue;
				}
					
			} // end-for elementi expression
			
		} // end-for espressioni assegnate
		
		return al_al_hostVar;
	}


	
	
	/**
	 * Restituisce tutte le variabili host presenti nella where di UPDATE, in tutte <br>
	 * le subselect che compongono la fullselect e negli elementi assegnati<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		ArrayList<String> al_hostVarAll = null;
		
		// Variabili host di where di update e di scalar-full-select
		al_hostVarAll = this.getHostVarsWhere();

		// Aggiungo le eventuali variabili host in espressioni di assegnazione
		
		// Scan espressioni assegnate
		for (SqlExpression sqlExpression : al_valueExpression) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Host var
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.HOST_VAR) {
					al_hostVarAll.add(sqlExpressionElementRight.getHostVar());
					continue;
				}
				
			} // end-for elementi expression
			
		} // end-for espressioni assegnate

		return al_hostVarAll;
	}


	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti nella condizione di where<br>
	 * e in tutte le le eventuali scalar-full-select assegnate<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
	    ArrayList<String> al_constantAlpha = null;
	    ArrayList<SqlSubselectSelectInto> al_subselect = null;
  		
  	    al_constantAlpha = new ArrayList<String> ();
  	    
 		// Scan espressioni assegnate
		for (SqlExpression sqlExpression : al_valueExpression) {
			
			// Costanti esplicite in elementi espressione
			al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());
			
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Scalar-full-select
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementRight.getFullSelect().getAllSubselect();
					// Scan subselect individuate
					for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
						al_constantAlpha.addAll(sqlSubselect.getConstantsAlphanumeric());
					}
					continue;
				}
				
			} // end-for elementi expression
			
		} // end-for espressioni assegnate

		// Costanti in fullselect
		if (isValuesByFullselect) {
			al_constantAlpha.addAll(this.fullSelect.getConstantsAlphanumeric());
		}
		
		
		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti nella condizione di where<br>
	 * e in tutte le le eventuali scalar-full-select assegnate<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		
		ArrayList<String> al_constantNumeric = null;
	    ArrayList<SqlSubselectSelectInto> al_subselect = null;
  		
  	    al_constantNumeric = new ArrayList<String> ();
  	    
		// Scan espressioni assegnate
		for (SqlExpression sqlExpression : al_valueExpression) {
			
			// Costanti esplicite in elementi espressione
			al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());
			
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Scalar-full-select
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementRight.getFullSelect().getAllSubselect();
					// Scan subselect individuate
					for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
						al_constantNumeric.addAll(sqlSubselect.getConstantsNumeric());
					}
					continue;
				}
				
			} // end-for elementi expression
			
		} // end-for espressioni assegnate
		
		// Costanti in fullselect
		if (isValuesByFullselect) {
			al_constantNumeric.addAll(this.fullSelect.getConstantsNumeric());
		}

		return al_constantNumeric;
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

	/**
	 * Restituisce informazioni sulle tabelle o sulle view utilizzate dall'istruzione dichiarate nelle clausole <tt>FROM</tt><br>
	 * Si prendono in considerazione tutte le clausole FROM comunque annidate in fullselect e subselect.<br>
	 * <p>
	 * Le informazioni comprendono il nome, l'owner di qualificazione e il correlation-name<br>
	 * Se il correlation-name o l'owner l'elemento corrispondente è impostato a stringa vuota.<br>
	 * <p>
	 * Restituisce un'array list di @SqlTableDeclared con:<br>
	 * <p>
	 * <ul>
	 * <li>Nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Nome dell'owner di qualificazione<br></li>
	 * <li>Correlation-name<br></li>
	 * </ul>
	 * <p>
	 * @return the tables o view
	 */
	public ArrayList<SqlTableDeclared> getEntitiesDeclared() {
		SqlTableDeclared tableDeclared = null;
		ArrayList<SqlTableDeclared> al_table = null;
		ArrayList<ArrayList<String>> al_al_entity = null;

		al_table = new ArrayList<SqlTableDeclared>();
		al_al_entity = getEntities();
		
		// Scan Tables
		for (ArrayList<String> al : al_al_entity) {
			tableDeclared = new SqlTableDeclared();
			tableDeclared.setTableName(al.get(0));
			tableDeclared.setOwner(al.get(1));
			tableDeclared.setCorrNameAS(al.get(2));
			al_table.add(tableDeclared);
		}		
		return al_table;
	}	
	
	/**
	 * Restituisce informazioni dettagliate sulle colonne delle tabelle o view dichiarate nella clausola <tt>SELECT</tt><br>
	 * Si prendono in considerazione tutte le clausole FROM comunque annidate in fullselect e subselect.<br>
     *
	 * Si tratta di statement della forma:<br>
	 * <p>
	 * <ul>
	 * 	<li>SELECT col1, col2, corr3.col3, table4.col4, ... FROM  table1, owner2.table2, table3 AS corr3, table4</li>
	 * 	<li>SELECT * FROM table1, table2 ... WHERE ...</li>
	 * 	<li>SELECT table-name.*</li>	 
	 * 	<li>SELECT view-name.*</li>	 
	 * 	<li>SELECT corr-name.*</li>	 
	 * </ul>
	 * 
	 * <p>
	 * Le informazioni comprendono il nome della tabella, il suo owner, il suo correlation name<br>
	 * e il nome della colonna, con l'eventuale new-column-name specificato con <tt>AS new-column-Name</tt> dopo la colonna.<br>
	 * Se le informazioni non sono disponibili l'elemento corrispondente è impostato a stringa vuota.<br>
	 * <p>
	 * Restituisce un'array list di oggetti {@link}SqlTableColumnDeclaredComplete con le informazioni
	 * per ogni tabella dichiarata nello statement.<br>
	 * <p>
	 * <ul>
	 * <li>Nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Nome dell'owner di qualificazione<br></li>
	 * <li>Correlation-name e, se presente, la colonna è qualificata con questo<br></li>
	 * <li>ArrayList con informazioni colonne
	 *    <ul>
	 *       <li>Nome della colonna</li>
	 *       <li>new-column-name impostato con <tt>AS new-column-Name</tt></li>
	 *       <li><b>true</b> se colonna dichiarata esplicitamente e <b>false</b> se implicitamente, con <b>*</b> o <b>.*</b></tt></li>
	 *    </ul>
	 * </li>
	 * </ul>
	 * <p>
     * 
	 * @return the info columns by entity
	 */
	public ArrayList<SqlTableColumnDeclaredComplete> getEntitiesColumnsDeclared() {
		ArrayList<SqlTableColumnDeclaredComplete> al_entity = null;
		SqlTableColumnDeclaredComplete entityColumnDescriptor = null;
		SqlTableColumnDeclared column = null;
		ArrayList<ArrayList<String>> al_al_columnsByEntity = null;

		al_al_columnsByEntity = getEntitiesColumns();

		al_entity = new ArrayList<SqlTableColumnDeclaredComplete>();
		
		// Scan table array
		for (ArrayList<String> al : al_al_columnsByEntity) {
			// Info table
			entityColumnDescriptor = new SqlTableColumnDeclaredComplete();
			entityColumnDescriptor.getTableDeclared().setTableName(al.get(0));
			entityColumnDescriptor.getTableDeclared().setOwner(al.get(1));
			entityColumnDescriptor.getTableDeclared().setCorrNameAS(al.get(2));
			
			// Info columns
			for (int i = 3; i < al.size(); i=i+3) {
				column = new SqlTableColumnDeclared();
				column.setColumnName(al.get(i));
				column.setCorrNameAS(al.get(i + 1));
				if (al.get(i + 2).contentEquals("E")) {
					column.setColumnExplicit(true);
				} else {
					column.setColumnExplicit(false);
				}
				entityColumnDescriptor.getColumnsDeclared().add(column);
			}	
			al_entity.add(entityColumnDescriptor);
		}		
		return al_entity;
	}	
	
}
