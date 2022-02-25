
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlMergeStatement
 * </h1>
 * <p>
 * Descrive una MERGE statement.<br> 
 * <p>
 * Gli oggetti utilizzati per descrivere lo statement Select sono:<br>
 * <p>
 * {@link SqlExpression}<br>
 * {@link SqlSearchConditions}<br> 
 * <p>
 * <tt>SqlSearchConditions</tt> descrive le codizioni di search presenti nelle wher di scalar-fullselect presenti
 * nelle espressioni.<br>
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
 * @see SqlMergeStatement 
*/

public class SqlMergeStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Campi per MERGE table AS correlation-name INCLUDE ...
	private String entityNameQualified = "";                    				// MERGE INTO entityName ... può essere qualificato
	private String correlationName = "";                    				    // MERGE INTO entityName AS correlationName
	private ArrayList<String> al_columnInclude = null;							// MERGE INTO entityName ... |INCLUDE (col1 data-type1, ..., coln data-typen) ...
   
	// Campi per USING source-table ON search-condition
	private ArrayList<SqlExpression> al_usingSourceValueExpression = null;      // MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)
    private String usingSourceValueAsCorrName = "";                             // MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)
    private ArrayList<String> al_usingSourceValueAsCorrNameColumn = null;       // MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)
    private String usingForRowHostVar = "";                          			// MERGE ... USING ... FOR ROW host-variable    OF ROWSET
    private int usingForRowNumber = 0;                          	    		// MERGE ... USING ... FOR ROW integer-constant OF ROWSET
    private SqlSearchConditions usingSearchConditions = null;                   // MERGE ... USING ... ON search-conditions

    // Campi per occorrenze di WHEN match-cond THEN mod-operation
    private ArrayList<SqlMergeWhenThen> al_whenThen = null;                     // MERGE ...WHEN NOT MATCHED ... THEN WHEN MATCHED ... THEN ...

    // Campi per opzioni
    private int queryno = 0;                                    				// MERGE ... QUERYNO n
     
    // Opzioni presenti
    private boolean isIncludeColumnsValue = false;                   		    // INCLUDE ( col data-type, ... )
    private boolean isNotAtomicContinue = false;                    			// VALUES ... NOT ATOMIC CONTINUE ON SQLEXCEPTION
    private boolean isQueryno = false;                         	 				// QUERYNO n
    private boolean isUsingForRowHostVar = false;                          	    // MERGE ... USING ... FOR ROW host-variable    OF ROWSET
    private boolean isUsingForRowNumber = false;                          	    // MERGE ... USING ... FOR ROW integer-constant OF ROWSET
  
    
	/**
	 * Costruttore 
	 */
	public SqlMergeStatement() {
		super();

		al_columnInclude = new ArrayList<String> (); 
		al_usingSourceValueExpression  = new ArrayList<SqlExpression> (); 
	    al_usingSourceValueAsCorrNameColumn = new ArrayList<String> ();    
	    al_whenThen = new  ArrayList<SqlMergeWhenThen> ();                       
	}




	/**
	 * Restituisce il nome eventualmente qualificato della tabella o view come indicato nello statement<br>
	 * <p>
     * <tt>MERGE entityName ...</tt>
	 * <p>
	 * @return the entityNameQualified
	 */
	public String getEntityNameQualified() {
		return entityNameQualified;
	}




	/**
	 * Imposta il nome eventualmente qualificato della tabella o view come indicato nello statement<br>
	 * <p>
     * <tt>MERGE entityName ...</tt>
	 * <p>
	 * @param entityNameQualified the entityNameQualified to set
	 */
	public void setEntityNameQualified(String entityNameQualified) {
		this.entityNameQualified = entityNameQualified;
	}

	

	/**
	 * Restituisce le colonne fra parentesi della clausola <tt>INCLUDE</tt><br>
	 * <p>
	 * <tt>MERGE ... INCLUDE ( col1 data-type1, col2 data-type2, .. )</tt>
	 * <p>
	 * Ogni elemento è costituito da nomeCol dataType.<br>
	 * <p>
	 * @return the al_columnInclude
	 */
	public ArrayList<String> getColumnsInclude() {
		return al_columnInclude;
	}




	/**
	 * Imposta il contenuto fra parentesi della clausola <tt>INCLUDE</tt><br>
	 * <p>
	 * <tt>MERGE ... INCLUDE ( col1 data-type1, col2 data-type2, .. )</tt>
	 * <p>
	 * Ogni elemento è costituito da nomeCol dataType.<br>
	 * <p>
	 * @param al_columnInclude the al_columnInclude to set
	 */
	public void setColumnsInclude(ArrayList<String> al_columnInclude) {
		this.al_columnInclude = al_columnInclude;
	}

	

	/**
	 * Restituisce il semplice nome della tabella o view da aggiornare senza qualificazione.<br>
	 * <p>
	 * 
	 * <tt>MERGE owner.entityName ...</tt>
	 * <p>
	 * @return the entityName 
	 */
	public String getEntityName() {
		return getTableName(this.entityNameQualified);
	}

	/**
	 * Imposta la variabile host in caso dicostrutto del tipo<br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW host-variable    OF ROWSET</tt>
	 * <p>
	 * @return the owner
	 */
	public String getEntityOwner() {
		return getTableOwner(this.entityNameQualified);
	}






	/**
	 * Restituisce il correlation name  esprresso da:
	 * <p>
	 * MERGE INTO table-name|view-name |AS correlation-name<br>
	 * <p>
	 * 
	 * @return the correlationName
	 */
	public String getCorrelationName() {
		return correlationName;
	}




	/**
	 * Imposta il correlation name  esprresso da:
	 * <p>
	 * MERGE INTO table-name|view-name |AS correlation-name<br>
	 * <p>
	 * 
	 * @param correlationName the correlationName to set
	 */
	public void setCorrelationName(String correlationName) {
		this.correlationName = correlationName;
	}




	/**
	 * Restituisce le espressioni nella clausola VALUE</tt><br>
	 * <p>
	 * <tt>MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)</tt>
	 * <p>
	 * @return the al_usingSourceValueExpression
	 */
	public ArrayList<SqlExpression> getUsingSourceValueExpressions() {
		return al_usingSourceValueExpression;
	}




	/**
	 * Imposta le espressioni nella clausola VALUE</tt><br>
	 * <p>
	 * <tt>MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)</tt>
	 * <p>
	 * @param alUsingSourceValueExpression the al_usingSourceValueExpression to set
	 */
	public void setUsingSourceValueExpressions(ArrayList<SqlExpression> al_usingSourceValueExpression) {
		this.al_usingSourceValueExpression = al_usingSourceValueExpression;
	}




	/**
	 * Restituisce il correlation name  esprresso da:
	 * <p>
	 * MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)<br>
	 * <p>
	 * 
	 * @return the usingSourceValueAsCorrName
	 */
	public String getUsingSourceValueAsCorrName() {
		return usingSourceValueAsCorrName;
	}




	/**
	 * Imposta il correlation name  esprresso da:
	 * <p>
	 *  MERGE ... USING expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)<br>
	 * <p>
	 * 
	 * @param usingSourceValueAsCorrName the usingSourceValueAsCorrName to set
	 */
	public void setUsingSourceValueAsCorrName(String usingSourceValueAsCorrName) {
		this.usingSourceValueAsCorrName = usingSourceValueAsCorrName;
	}




	/**
	 * Restituisce le colonne associate al correlation-name nella clausola VALUES</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... VALUES expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)</tt>
	 * <p>
	 * @return the al_usingSourceValueAsCorrNameColumn
	 */
	public ArrayList<String> getUsingSourceValueAsCorrNameColumns() {
		return al_usingSourceValueAsCorrNameColumn;
	}




	/**
	 * Imposta le colonne associate al correlation-name nella clausola VALUES</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... VALUES expr|( expr1, exprn)|FOR ... AS correlation-name (col1, coln)</tt>
	 * <p>
	 * @param al_usingSourceValueAsCorrNameColumn the al_usingSourceValueAsCorrNameColumn to set
	 */
	public void seUsingSourceValueAsCorrNameColumns(ArrayList<String> al_usingSourceValueAsCorrNameColumn) {
		this.al_usingSourceValueAsCorrNameColumn = al_usingSourceValueAsCorrNameColumn;
	}




	/**
	 * Restituisce la variabile host espressa da</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW host-variable    OF ROWSET</tt>
	 * <p>
	 * @return the usingForRowHostVar
	 */
	public String getUsingForRowHostVar() {
		return usingForRowHostVar;
	}




	/**
	 * Imposta la variabile host espressa da</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW host-variable    OF ROWSET</tt>
	 * <p>
	 * @param usingForRowHostVar the usingForRowHostVar to set
	 */
	public void setUsingForRowHostVar(String usingForRowHostVar) {
		this.usingForRowHostVar = usingForRowHostVar;
	}




	/**
	 * Restituisce il numero di righe espresso da</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW number  OF ROWSET</tt>
	 * <p>
	 * @return the usingForRowNumber
	 */
	public int getUsingForRowNumber() {
		return usingForRowNumber;
	}




	/**
	 * Imposta il numero di righe espresso da</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW number  OF ROWSET</tt>
	 * <p>
	 * @param usingForRowNumber the usingForRowNumber to set
	 */
	public void setUsingForRowNumber(int usingForRowNumber) {
		this.usingForRowNumber = usingForRowNumber;
	}




	/**
	 * Restituisce le condizioni di ricerca</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... ON search-conditions</tt>
	 * <p>
	 * @return the usingSearchConditions
	 */
	public SqlSearchConditions getUsingSearchConditions() {
		return usingSearchConditions;
	}




	/**
	 * Imposta le condizioni di ricerca</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... ON search-conditions</tt>
	 * <p>
	 * @param usingSearchConditions the usingSearchConditions to set
	 */
	public void setUsingSearchConditions(SqlSearchConditions usingSearchConditions) {
		this.usingSearchConditions = usingSearchConditions;
	}




	/**
	 * Restituisce le occorrenze di descrittori della clausola</tt><br>
	 * <p>
	 * <tt>MERGE ...WHEN NOT MATCHED ... THEN WHEN MATCHED ... THEN ...</tt><br>
	 * <p>
	 * @return the al_whenThen
	 */
	public ArrayList<SqlMergeWhenThen> getWhenThens() {
		return al_whenThen;
	}




	/**
	 * Imposta le occorrenze di descrittori della clausola</tt><br>
	 * <p>
	 * <tt>MERGE ...WHEN NOT MATCHED ... THEN WHEN MATCHED ... THEN ...</tt><br>
	 * <p>
	 * @param alWhenThen the al_whenThen to set
	 */
	public void setWhenThens(ArrayList<SqlMergeWhenThen> al_whenThen) {
		this.al_whenThen = al_whenThen;
	}




	/**
	 * Restituisce se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW host-variable  OF ROWSET</tt><br>
	 * <p>
	 * @return the isUsingForRowHostVar
	 */
	public boolean isUsingForRowHostVar() {
		return isUsingForRowHostVar;
	}




	/**
	 * Imposta se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW host-variable  OF ROWSET</tt><br>
	 * <p>
	 * @param isUsingForRowHostVar the isUsingForRowHostVar to set
	 */
	public void setUsingForRowHostVar(boolean isUsingForRowHostVar) {
		this.isUsingForRowHostVar = isUsingForRowHostVar;
	}




	/**
	 * Restituisce se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW number  OF ROWSET</tt><br>
	 * <p>
	 * @return the isUsingForRowNumber
	 */
	public boolean isUsingForRowNumber() {
		return isUsingForRowNumber;
	}




	/**
	 * Imposta se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR ROW number  OF ROWSET</tt><br>
	 * <p>
	 * @param isUsingForRowNumber the isUsingForRowNumber to set
	 */
	public void setUsingForRowNumber(boolean isUsingForRowNumber) {
		this.isUsingForRowNumber = isUsingForRowNumber;
	}





	/**
	 * Restituisce se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... QUERYNO number</tt><br>
	 * <p>
     * @return the queryno
	 */
	public int getQueryno() {
		return queryno;
	}




	/**
	 * Imposta se presente la clausol</tt><br>
	 * <p>
	 * <tt>MERGE ... INCLUDE ...</tt><br>
	 * <p>
	 * @param isIncludeColumnsValue the isIncludeColumnsValue to set
	 */
	public void setIncludeColumnsValue(boolean isIncludeColumnsValue) {
		this.isIncludeColumnsValue = isIncludeColumnsValue;
	}




	/**
	 * Restituisce la variabile host in caso dicostrutto del tipo<br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR host-variable  ROWS</tt>
	 * <p>
	 * @return the forRowHostVar
	 */
	public String getForRowHostVar() {
		return usingForRowHostVar;
	}




	/**
	 * Imposta la variabile host in caso di UPDATE positioned-updated.<br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR host-variable  ROWS</tt>
	 * <p>
	 * @param usingForRowHostVar the forRowHostVar to set
	 */
	public void setForRowHostVar(String usingForRowHostVar) {
		this.usingForRowHostVar = usingForRowHostVar;
	}




	/**
	 * Restituisce il numero di righe in caso dicostrutto del tipo<br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR number  ROWS</tt>
	 * <p>
	 * @return the forRowNumber
	 */
	public int getForRowNumber() {
		return usingForRowNumber;
	}




	/**Imposta
	 * Restituisce il numero di righe in caso dicostrutto del tipo<br>
	 * <p>
	 * <tt>MERGE ... USING ... FOR number  ROWS</tt>
	 * <p>
	 * @param forRowNumber the forRowNumber to set
	 */
	public void setForRowNumber(int usingForRowNumber) {
		this.usingForRowNumber = usingForRowNumber;
	}




	/**
	 * Restituisce se presente la clausola include-column.<br>
	 * <p>
	 * <tt>MERGE ... INCLUDE ( col data-type, ... )</tt>
	 * <p>
	 * @return the isIncludeColumnsValue
	 */
	public boolean isIncludeColumnsValue() {
		return isIncludeColumnsValue;
	}




	/**
	 * Imposta se presente la clausola include-column.<br>
	 * <p>
	 * <tt>MERGE ... INCLUDE ( col data-type, ... )</tt>
	 * <p>
	 * @param isIncludeColumnsValue the isIncludeColumnsValue to set
	 */
	public void setIncludeColumns(boolean isIncludeColumnsValue) {
		this.isIncludeColumnsValue = isIncludeColumnsValue;
	}



	/**
	 * Restituisce se presente l'opzione di not atomic su errori sql.<br>
	 * <p>
     * <tt>MERGE ... NOT ATOMIC CONTINUE ON SQLEXCEPTION ...</tt>
     * <p>
	 * @return the isNotAtomicContinue
	 */
	public boolean isNotAtomicContinue() {
		return isNotAtomicContinue;
	}




	/**
	 * Imposta se presente l'opzione di not atomic su errori sql.<br>
	 * <p>
     * <tt>MERGE ... NOT ATOMIC CONTINUE ON SQLEXCEPTION ...</tt>
     * <p>
	 * @param isNotAtomicContinue the isNotAtomicContinue to set
	 */
	public void setNotAtomicContinue(boolean isNotAtomicContinue) {
		this.isNotAtomicContinue = isNotAtomicContinue;
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
	 * Restituisce informazioni sulle tabelle o sulle view utilizzate in tutte le expression dichiarate<br>
	 * Si prendono in considerazione tutte le clausole FROM comunque annidate in fullselect e subselect di espressioni.<br>
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
		
		// Estrazione entity dichiarate in scalar-full-select di expression di USING source-table
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
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
		
		// Estrazione entity dichiarate in scalar-full-select di expression di WHEN ... THEN
		for (SqlMergeWhenThen whenThen : al_whenThen) {
			
			// Insert Values expressions
			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
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
			
			// Update Values expressions
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
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

		}

		return al_al_entity;
	}


	/**
	 * Restituisce le variabili host in <tt>input</tt>, che iniziano con <tt>:</tt> <br>
	 * dichiarate nella <tt> WHERE col1 = :hostVar1 AND col2 = hostVar2 ... di ebventuali subselect</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return the al_hostVarWhere
	 */
	public ArrayList<String> getHostVarsWhere() {
		
		ArrayList<String> al_hostVarWhere = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_hostVarWhere = new ArrayList<String> ();
		
		// Variabili host nelle scalar-full-select assegnate
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
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
		
		// Occorrenze When Then
		for (SqlMergeWhenThen whenThen : al_whenThen) {
			
			// Variabili host nelle scalar-full-select assegnate
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
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

			// Variabili host nelle scalar-full-select in insert
			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
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

		}
		
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
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
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
		
		// Occorrenze When Then
		for (SqlMergeWhenThen whenThen : al_whenThen) {
	
			// Variabili host nelle scalar-full-select assegnate
			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
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

			// Variabili host nelle scalar-full-select assegnate
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
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

		}
			
		return al_al_hostVar;
	}


	
	
	/**
	 * Restituisce tutte le variabili host presenti nella MERGE, in tutte <br>
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
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
			// Scan elementi espressione
			for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
				// Host var
				if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.HOST_VAR) {
					al_hostVarAll.add(sqlExpressionElementRight.getHostVar());
					continue;
				}
				
			} // end-for elementi expression
			
		} // end-for espressioni assegnate

		// Occorrenze When Then
		for (SqlMergeWhenThen whenThen : al_whenThen) {
			
			// Scan espressioni assegnate
			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
				// Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
					// Host var
					if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.HOST_VAR) {
						al_hostVarAll.add(sqlExpressionElementRight.getHostVar());
						continue;
					}
					
				} // end-for elementi expression
				
			} // end-for espressioni assegnate

			// Scan espressioni assegnate
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
				// Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElementRight : sqlExpression.getElements()) {
					// Host var
					if (sqlExpressionElementRight.getTypeElement() == EnumSqlExpressionElementType.HOST_VAR) {
						al_hostVarAll.add(sqlExpressionElementRight.getHostVar());
						continue;
					}
					
				} // end-for elementi expression
				
			} // end-for espressioni assegnate

		}

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
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
			
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

		// Search conditions
		al_constantAlpha.addAll(usingSearchConditions.getConstantsAlphanumeric());
		
		// Occorrenze When Then
		for (SqlMergeWhenThen whenThen : al_whenThen) {

			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
			
				// Costanti esplicite in elementi espressione
				al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());
				
			    // Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElement : sqlExpression.getElements()) {
					
					// Scalar-full-select
					if (sqlExpressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_subselect = sqlExpressionElement.getFullSelect().getAllSubselect();
						// Scan subselect individuate
						for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
							al_constantAlpha.addAll(sqlSubselect.getConstantsAlphanumeric());
						}
						continue;
					}
				} // end-for elementi expression
				
			}
			
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
				
				// Costanti esplicite in elementi espressione
				al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());
				
			    // Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElement : sqlExpression.getElements()) {
					
					// Scalar-full-select
					if (sqlExpressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_subselect = sqlExpressionElement.getFullSelect().getAllSubselect();
						// Scan subselect individuate
						for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
							al_constantAlpha.addAll(sqlSubselect.getConstantsAlphanumeric());
						}
						continue;
					}
				} // end-for elementi expression
				
			}

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
		for (SqlExpression sqlExpression : al_usingSourceValueExpression) {
			
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

		// Search conditions
		al_constantNumeric.addAll(usingSearchConditions.getConstantsNumeric());
		
		// Occorrenze When Then
		for (SqlMergeWhenThen whenThen : al_whenThen) {

			for (SqlExpression sqlExpression : whenThen.getInsertValuesExpressions()) {
			
				// Costanti esplicite in elementi espressione
				al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());
				
			    // Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElement : sqlExpression.getElements()) {
					
					// Scalar-full-select
					if (sqlExpressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_subselect = sqlExpressionElement.getFullSelect().getAllSubselect();
						// Scan subselect individuate
						for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
							al_constantNumeric.addAll(sqlSubselect.getConstantsNumeric());
						}
						continue;
					}
				} // end-for elementi expression
				
			}
			
			for (SqlExpression sqlExpression : whenThen.getUpdateSetExpressions()) {
				
				// Costanti esplicite in elementi espressione
				al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());
				
			    // Scan elementi espressione
				for (SqlExpressionElement sqlExpressionElement : sqlExpression.getElements()) {
					
					// Scalar-full-select
					if (sqlExpressionElement.getTypeElement() == EnumSqlExpressionElementType.SCALAR_FULL_SELECT) {
						al_subselect = sqlExpressionElement.getFullSelect().getAllSubselect();
						// Scan subselect individuate
						for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
							al_constantNumeric.addAll(sqlSubselect.getConstantsNumeric());
						}
						continue;
					}
				} // end-for elementi expression
				
			}

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
		
		owner = tableNameQualified;
		
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
