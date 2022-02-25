
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDeleteStatement
 * </h1>
 * <p>
 * Descrive una DELETE FROM statement che elimina una o più righe di una tabella.<br> 
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
 * @since 02/Ago/2011 
 * @see SqlDeleteStatement 
*/
public class SqlDeleteStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Descrizione corpo istruzione
	private String entityNameQualified = "";                    	// DELETE entityName ... può essere qualificato
	private ArrayList<String> al_setColumnName = null; 	            // DELETE ... SET column = exprRight o (col1, ..) = (expr1|DEFAULT|NULL, ..)
	private ArrayList<SqlExpression> al_setExpressionRight = null;	// DELETE ... SET exprLeft = exprRight
	private SqlFullSelect setFullselectRight = null;				// DELETE ... SET column = SELECT * FROM  ..
    private SqlSearchConditions where = null;      					// DELETE ... SET ... WHERE search-conditions ...
    private String cursorName = "";                             	// DELETE ... SET ... WHERE CURRENT OF cursor-name
    private String forRowHostVar = "";                          	// DELETE ... SET ... WHERE CURRENT OF cursor-name FOR ROW host-variable OF ROWSET
    private int forRowNumber = 0;                          	    	// DELETE ... SET ... WHERE CURRENT OF cursor-name FOR ROW integer-constant OF ROWSET
    
    // Valori associati a opzioni
    private String correlationName = "";                        	// DELETE entityName ... |corr-name SET
    private String periodForValueLower = "";                    	// FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
    private String periodForValueHigher = "";                   	// FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
    private String includeColumnsValue = "";                    	// DELETE ... INCLUDE ( value ) ...
    private String isolation = "";                              	// RR|RS|CS|UR
    private int queryno = 0;                                    	// QUERYNO n
     
    // Opzioni presenti
    private boolean isSearchedDelete = false;                   	// DELETE ... SET ... WHERE search-conditions .
    private boolean isSetByFullselect = false;                   	// DELETE ... SET col = SELECT * FROM ...
    private boolean isPositionedDelete = false;                 	// DELETE ... SET ... WHERE CURRENT OF cursor-name FOR ROW host-variable|integer-constant OF ROWSET
    private boolean isPeriod = false;                           	// FOR PORTION OF BUSINESS_TIME FROM value1 TO value2
    private boolean isIncludeColumns = false;                   	// INCLUDE ( col data-type, ... )
    private boolean isQueryno = false;                         	 	// QUERYNO n
    private boolean isSkipLockData = false;                     	// SKIP LOCK DATA
    private boolean isIsolation = false;                        	// WITH RR|RS|CS|UR
    private boolean isForRowHostVar = false;                        // FOR ROW host-variable OF ROWSET
    private boolean isForRowNumber = false;                         // FOR ROW integer-constant OF ROWSET
    private boolean isWhere = false;                          		// FOR ROW integer-constant OF ROWSET
    
    
	/**
	 * Costruttore
	 */
	public SqlDeleteStatement() {
		super();
		al_setColumnName = new ArrayList<String> ();
		al_setExpressionRight = new ArrayList<SqlExpression> ();
	}




	/**
	 * Restituisce il nome eventualmente qualificato della tabella o view da aggiornare come indicato nello statement<br>
	 * <p>
     * <tt>DELETE entityName WHERE search-condition</tt>
	 * <p>
	 * @return the entityNameQualified
	 */
	public String getEntityNameQualified() {
		return entityNameQualified;
	}




	/**
	 * Imposta il nome eventualmente qualificato della tabella o view da aggiornare come indicato nello statement<br>
	 * <p>
     * <tt>DELETE entityName WHERE search-condition</tt>
	 * <p>
	 * @param entityNameQualified the entityNameQualified to set
	 */
	public void setEntityNameQualified(String entityNameQualified) {
		this.entityNameQualified = entityNameQualified;
	}

	/**
	 * Restituisce il semplice nome della tabella o view da aggiornare senza qualificazione.<br>
	 * <p>
	 * 
	 * <tt>DELETE owner.entityName WHERE search-condition</tt>
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
	 * <tt>DELETE owner.entityName WHERE search-condition</tt>
	 * <p>
	 * @return the owner
	 */
	public String getEntityOwner() {
		return getTableOwner(this.entityNameQualified);
	}


	/**
	 * Restituisce un ArrayList  con i nomi delle colonne da aggiornare.
	 * <p>
	 * @return the al_setColumnName
	 */
	public ArrayList<String> getSetColumnsName() {
		return al_setColumnName;
	}




	/**
	 * Imposta un ArrayList  con i nomi delle colonne da aggiornare.
	 * <p>
	 * @param al_setColumnName the al_setColumnName to set
	 */
	public void setSetColumnsName(ArrayList<String> al_setColumnName) {
		this.al_setColumnName = al_setColumnName;
	}




	/**
	 * Restituisce un ArrayList di espressioni delle dimensioni delle colonne da aggiornare.
	 * <p>
	 * Ogni espressione può contenere qualsiasi elemento inclusa una scalar-fulle-select.<br>
	 * <p>
	 * @return the al_setExpressionRight
	 */
	public ArrayList<SqlExpression> getSetExpressionsRight() {
		return al_setExpressionRight;
	}




	/**
	 * Restituisce un ArrayList di espressioni delle dimensioni delle colonne da aggiornare.
	 * <p>
	 * Ogni espressione può contenere qualsiasi elemento inclusa una scalar-fulle-select.<br>
	 * <p>
	 * @param al_setExpressionRight the al_setExpressionRight to set
	 */
	public void setSetExpressionsRight(ArrayList<SqlExpression> al_setExpressionRight) {
		this.al_setExpressionRight = al_setExpressionRight;
	}




	/**
	 * Restituisce il contenuto fra parentesi dell'opzione INCLUDE.<br>
	 * <p>
	 * <tt>DELETE ... INCLUDE ( col data-type, col data-type, ... ) ...</tt>
	 * <p>
	 * @return the includeColumnsValue
	 */
	public String getIncludeColumnsValue() {
		return includeColumnsValue;
	}




	/**
	 * Imposta il contenuto fra parentesi dell'opzione INCLUDE.<br>
	 * <p>
	 * <tt>DELETE ... INCLUDE ( col data-type, col data-type, ... ) ...</tt>
	 * <p>
	 * @param includeColumnsValue the includeColumnsValue to set
	 */
	public void setIncludeColumnsValue(String includeColumnsValue) {
		this.includeColumnsValue = includeColumnsValue;
	}




	/**
	 * Restituisce il cursore in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name ...</tt>
	 * <p>
	 * @return the cursorName
	 */
	public String getCursorName() {
		return cursorName;
	}




	/**
	 * Imposta il cursore in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name ...</tt>
	 * <p>
	 * @param cursorName the cursorName to set
	 */
	public void setCursorName(String cursorName) {
		this.cursorName = cursorName;
	}


	/**
	 * Restituisce la fullselect che determina il contenuto delle righe da deletare<br>
	 * <p>
     * <tt>DELETE entityName SET ciìol = SELECT é FROM tb</tt>
	 * <p>
	 * @return the setFullselectRight
	 */
	public SqlFullSelect getSetFullselectRight() {
		return setFullselectRight;
	}




	/**
	 * Imposta la fullselect che determina il contenuto delle righe da deletare<br>
	 * <p>
     * <tt>DELETE entityName SET ciìol = SELECT é FROM tb</tt>
	 * <p>
	 * @param setFullselectRight the setFullselectRight to set
	 */
	public void setSetFullselectRight(SqlFullSelect setFullselectRight) {
		this.setFullselectRight = setFullselectRight;
	}





	/**
	 * Restituisce la variabile host in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the forRowHostVar
	 */
	public String getForRowHostVar() {
		return forRowHostVar;
	}




	/**
	 * Imposta la variabile host in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param forRowHostVar the forRowHostVar to set
	 */
	public void setForRowHostVar(String forRowHostVar) {
		this.forRowHostVar = forRowHostVar;
	}




	/**
	 * Restituisce il numero di riga in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the forRowNumber
	 */
	public int getForRowNumber() {
		return forRowNumber;
	}




	/**
	 * Imposta il numero di riga in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param forRowNumber the forRowNumber to set
	 */
	public void setForRowNumber(int forRowNumber) {
		this.forRowNumber = forRowNumber;
	}




	/**
	 * Restituisce il valore inferiore della clausola di periodo.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @return the periodForValueLower
	 */
	public String getPeriodForValueLower() {
		return periodForValueLower;
	}




	/**
	 * Imposta il valore inferiore della clausola di periodo.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @param periodForValueLower the periodForValueLower to set
	 */
	public void setPeriodForValueLower(String periodForValueLower) {
		this.periodForValueLower = periodForValueLower;
	}




	/**
	 * Restituisce il valore superiore della clausola di periodo.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @return the periodForValueHigher
	 */
	public String getPeriodForValueHigher() {
		return periodForValueHigher;
	}




	/**
	 * Imposta il valore superiore della clausola di periodo.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @param periodForValueHigher the periodForValueHigher to set
	 */
	public void setPeriodForValueHigher(String periodForValueHigher) {
		this.periodForValueHigher = periodForValueHigher;
	}




	/**
	 * Restituisce il correlation name associato alla tabella in DELETE.<br>
	 * <p>
	 * <tt>DELETE table ... corr-name ...</tt>
	 * <p>
	 * @return the correlationName
	 */
	public String getCorrelationName() {
		return correlationName;
	}




	/**
	 * Imposta il correlation name associato alla tabella in DELETE.<br>
	 * <p>
	 * <tt>DELETE table ... corr-name ...</tt>
	 * <p>
	 * @param correlationName the correlationName to set
	 */
	public void setCorrelationName(String correlationName) {
		this.correlationName = correlationName;
	}


	


	/**
	 * Restituisce se lo statement DELETE è di tipo searched.<br>
	 * <p>
	 * <tt>DELETE table ... |WHERE search-conditions ...</tt>
	 * <p>
	 * @return the isSearchedDelete
	 */
	public boolean isSearchedDelete() {
		return isSearchedDelete;
	}




	/**
	 * Imposta se lo statement DELETE è di tipo searched.<br>
	 * <p>
	 * <tt>DELETE table ... |WHERE search-conditions ...</tt>
	 * <p>
	 * @param isSearchedDelete the isSearchedDelete to set
	 */
	public void setSearchedDelete(boolean isSearchedDelete) {
		this.isSearchedDelete = isSearchedDelete;
	}

	/**
	 * Restituisce se i valori da aggiornare sono recuoerati con una fullselect.<br>
	 * <p>
	 * @return the isSetByFullselect
	 */
	public boolean isSetByFullselect() {
		return isSetByFullselect;
	}




	/**
	 * Imposta se i valori da aggiornare sono recuoerati con una fullselect.<br>
	 * <p>
	 * @param isSetByFullselect the isSetByFullselect to set
	 */
	public void setSetByFullselect(boolean isSetByFullselect) {
		this.isSetByFullselect = isSetByFullselect;
	}





	/**
	 * Restituisce se lo statement DELETE è di tipo positioned update.<br>
	 * <p>
	 * <tt>DELETE table ... WHERE CURRENT OF cursor-name ...</tt>
	 * <p>
	 * @return the isPositionedDelete
	 */
	public boolean isPositionedDelete() {
		return isPositionedDelete;
	}




	/**
	 * Imposta se lo statement DELETE è di tipo positioned update.<br>
	 * <p>
	 * <tt>DELETE table ... WHERE CURRENT OF cursor-name ...</tt>
	 * <p>
	 * @param isPositionedDelete the isPositionedDelete to set
	 */
	public void setPositionedDelete(boolean isPositionedDelete) {
		this.isPositionedDelete = isPositionedDelete;
	}




	/**
	 * Restituisce se presente la clausola period.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @return the isPeriod
	 */
	public boolean isPeriod() {
		return isPeriod;
	}




	/**
	 * Imposta se presente la clausola period.<br>
	 * <p>
	 * <tt>DELETE table ... FOR PORTION OF BUSINESS_TIME FROM valueLower TO valueHigher</tt>
	 * <p>
	 * @param isPeriod the isPeriod to set
	 */
	public void setPeriod(boolean isPeriod) {
		this.isPeriod = isPeriod;
	}




	/**
	 * Restituisce se presente la clausola include-column.<br>
	 * <p>
	 * <tt>DELETE table ... INCLUDE ( col1 data-type1, col2 dara-type2, ... )</tt>
	 * <p>
	 * @return the isIncludeColumns
	 */
	public boolean isIncludeColumns() {
		return isIncludeColumns;
	}




	/**
	 * Imposta se presente la clausola include-column.<br>
	 * <p>
	 * <tt>DELETE table ... INCLUDE ( col1 data-type1, col2 dara-type2, ... )</tt>
	 * <p>
	 * @param isIncludeColumns the isIncludeColumns to set
	 */
	public void setIncludeColumns(boolean isIncludeColumns) {
		this.isIncludeColumns = isIncludeColumns;
	}




	/**
	 * Restituisce la search-condition codificata nell'istruzione <br>
	 * <p>
	 * <tt>DELETE ... WHERE search-condition</tt>
	 * <p>
	 * @return the where
	 */
	public SqlSearchConditions getWhere() {
		return where;
	}





	/**
	 * Imposta la search-condition codificata nell'istruzione <br>
	 * <p>
	 * <tt>DELETE ... WHERE search-condition</tt>
	 * <p>
	 * @param where the where to set
	 */
	public void setWhere(SqlSearchConditions where) {
		this.isWhere = true;
		this.where = where;
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
	 * Restituisce se presente FOR ROW hostvar in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the isForRowHostVar
	 */
	public boolean isForRowHostVar() {
		return isForRowHostVar;
	}




	/**
	 * Imposta se presente FOR ROW hostvar in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param isForRowHostVar the isForRowHostVar to set
	 */
	public void setForRowHostVar(boolean isForRowHostVar) {
		this.isForRowHostVar = isForRowHostVar;
	}




	/**
	 * Restituisce se presente FOR ROW number in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @return the isForRowNumber
	 */
	public boolean isForRowNumber() {
		return isForRowNumber;
	}




	/**
	 * Imposta se presente FOR ROW number in caso di DELETE positioned-updated.<br>
	 * <p>
	 * <tt>DELETE ... WHERE CURRENT OF cursor-name |FOR ROW host-var OF ROWSET</tt>
	 * <p>
	 * @param isForRowNumber the isForRowNumber to set
	 */
	public void setForRowNumber(boolean isForRowNumber) {
		this.isForRowNumber = isForRowNumber;
	}




	/**
	 * Restituisce se presente la clausola WHERE.<br>
	 * <p>
	 * @return the isWhere
	 */
	public boolean isWhere() {
		return isWhere;
	}




	/**
	 * Imposta se presente la clausola WHERE.<br>
	 * <p>
	 * @param isWhere the isWhere to set
	 */
	public void setWhere(boolean isWhere) {
		this.isWhere = isWhere;
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
		al_entity.add(getCorrelationName());
		al_al_entity.add(al_entity);
		
		// Estrazione entity dichiarate in scalar-full-select di expression di SET
		for (SqlExpression sqlExpression : al_setExpressionRight) {
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
	 * 	<li>DELETE table  SET col = value</li>
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
	 * <li>Il primo elemento è il nome della tabella dichiarata nello statement DELETE e nelle clausole <tt>FROM</tt><br></li>
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
		
        // Colonne principali dichiarate 
		for (String columnName : al_setColumnName) {

			al_columnsByEntity = new ArrayList<String> ();
			al_columnsByEntity.add(this.getEntityName());
			al_columnsByEntity.add(this.getEntityOwner());
			al_columnsByEntity.add(this.getCorrelationName());
			al_columnsByEntity.add(columnName);
			al_columnsByEntity.add("");											// new col name
			al_columnsByEntity.add("E");										// Explicit column
			al_al_columnsByEntity.add(al_columnsByEntity);
		}
		
		// Colonne assegnate o in scalar-full-select
		for (SqlExpression sqlExpression : al_setExpressionRight) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Colonna, sempre dei entity in update
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.COLUMN_NAME) {
					al_columnsByEntity = new ArrayList<String> ();
					al_columnsByEntity.add(this.getEntityName());
					al_columnsByEntity.add(this.getEntityOwner());
					al_columnsByEntity.add(this.getCorrelationName());
					al_columnsByEntity.add(sqlExpressionElementRight.getColumnName());
					al_columnsByEntity.add("");											// new col name
					al_columnsByEntity.add("E");										// Explicit column
					al_al_columnsByEntity.add(al_columnsByEntity);
					continue;
				}
				// Scalar-full-select
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
					al_subselect = sqlExpressionElementRight.getFullSelect().getAllSubselect();
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
	 * dichiarate nella <tt>DELETE ... WHERE col1 = :hostVar1 AND col2 = hostVar2 ...</tt><br>
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
		
		// Variabili host in condizione di search (Where)
		if (isSearchedDelete) {
			al_hostVarWhere.addAll(where.getHostVars());
		}
		
		// Variabili host nelle scalar-full-select assegnate
		for (SqlExpression sqlExpression : al_setExpressionRight) {
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
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SET</tt> di tt>DELETE</tt> per la tabella<br>
	 * Vengono inoltre restituite tutte le variabili host in select di scalar-full-select assegnate ale colonne <br>
	 * della rabella in DELETE.<br>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella in <tt>DELETE</tt><br></li>
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
		ArrayList<String> al_hostVar = null;
		ArrayList<String> al_hostVarWhere = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// Variabili host nella where di DELETE
		if (isSearchedDelete) {
			al_hostVarWhere = this.where.getHostVars();
			al_hostVar = new ArrayList<String> ();
			al_hostVar.add(this.getEntityName());
			al_hostVar.add(this.getEntityOwner());
			al_hostVar.add(this.getCorrelationName());
			// Scan host var in where
			for (String hostVarWhere : al_hostVarWhere) {
				al_hostVar.add(hostVarWhere);
			}
			// Variabili host in espressione assegnata
			for (SqlExpression sqlExpression : al_setExpressionRight) {
				// Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
					// Host var in in espressione assegnata
					if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.HOST_VAR) {
						al_hostVar.add(sqlExpressionElementRight.getHostVar());
					}
				} // end-for elementi expression
			} // end-for espressioni assegnate

			al_al_hostVar.add(al_hostVar);
		}
		
		// Variabili host nelle scalar-full-select assegnate
		for (SqlExpression sqlExpression : al_setExpressionRight) {
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
	 * Restituisce tutte le variabili host presenti nella where di DELETE, in tutte <br>
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
		for (SqlExpression sqlExpression : al_setExpressionRight) {
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
  		
  	    al_constantAlpha = new ArrayList<String> ();
  	    
  	    // Costanti in condizione di where
  	    if (isSearchedDelete) {
  	 	    al_constantAlpha.addAll(this.where.getConstantsAlphanumeric());
		}
   	    
		// Scan espressioni assegnate
		for (SqlExpression sqlExpression : al_setExpressionRight) {
			
			// Costanti esplicite in elementi espressione
			al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());
			
			
		} // end-for espressioni assegnate

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
  		
  	    al_constantNumeric = new ArrayList<String> ();
  	    
  	    // Costtanti in condizione di where
  	    if (isSearchedDelete) {
  	 	    al_constantNumeric.addAll(this.where.getConstantsNumeric());
		}
   	    
		// Scan espressioni assegnate
		for (SqlExpression sqlExpression : al_setExpressionRight) {
			
			// Costanti esplicite in elementi espressione
			al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());
						
		} // end-for espressioni assegnate
		
		return al_constantNumeric;
	}

	/**
	 * Restituisce il numero massimo di subselect nested<br>
	 * <br>
	 * Si tratta delle subselect presenti nella eventuale clausola where oppure<br>
	 * della subselect di individiazione valori, e dalle subselect codificate nelle <br>
	 * full-select in UNION, EXCEPT o INTERSECT.<br>
	 * <p>
	 * Questo rappresenta il primo livello di select di recupero dati, senza nesting, anndimento<br>
	 * <p>
	 * Per ogni subselect si analizzano ricorsivamente i predicati nella where, che possono fare
	 * esplicito riferimento a full-select o rimandare ad espressioni sql che possono contenere<br>
	 * full-select.<br>
	 * <p>
	 * Il processo continua ricorsivamente a partire da ogni subselect di primo livello individuata.<br>
	 * Viene resituito il livello di annidamento massimo raggiunto.<br
	 * <p>
	 * @return the subselect
	 */
	public  int getMaxSubselectNestedNumber() {
		int maxSubselectNestedNumber = 0;
		
		// Tratto solo le searched delete
		if (!this.isSearchedDelete) {
			return 0;
		}
		maxSubselectNestedNumber = this.where.getMaxSubselectNestedNumber();
		return maxSubselectNestedNumber;
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
