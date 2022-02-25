
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import enums.EnumSqlExpressionElementType;


/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlSubselectSelectInto
 * </h1>
 * <p>
 * Descrive una subselect o una select into.<br> 
 * <p>
 * Viene descritta sia la <tt>SELECT</tt> sia la <tt>SELECT INTO</tt> che ha 
 * le stesse caratteristiche più il parametro INTO e altre opzioni.<br>
 * <p>
 * Gli oggetti utilizzati per descrivere la subselect o la select into sono:<br>
 * <p>
 * {@link SqlColumnInSelect}
 * {@link SqlFromTableReference}
 * {@link SqlSearchConditions}
 * {@link SqlOrderBy}
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see  SqlSelectStatement
 * @see  SqlExpression}
 * @see  SqlFromTableReference}
 * @see  SqlSearchConditions}
 * @see  SqlExpression}
 * @see  SqlOrderBy}
*/

public class SqlSubselectSelectInto implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
    private ArrayList<SqlColumnInSelect> al_column = null;			// SELECT expr1 AS newCol1Name, expr2, col3, ...
    private ArrayList<String> al_hostVarInto = null;                // SELECT ... INTO hostVar1, hostVar2, ..
    private Map<String, String> map_hostVarIndicatorInto = null;    // SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, hostVar2, ..
                                                                    // Key = hostVar, Data = hostVarIndicator
    private ArrayList<SqlFromTableReference> al_from = null;  		// SELECT ... FROM singleTable|nestedTableExpression|tableFunctionReference|joinedTable|tableLocatorReference|xmlTableExpression
    private SqlSearchConditions where = null;      					// SELECT ... WHERE search-conditions
    private ArrayList<SqlExpression> al_groupBy = null;       		// SELECT ... GROUP BY col1, coln
    private SqlSearchConditions having = null;          			// SELECT ... HAVING search-conditions
    private ArrayList<SqlOrderBy> al_orderBy = null;          		// SELECT ... ORDER BY col1 ASC, col2 DESC 
   
    // Valori associati a opzioni
    private int fetchFirstRowsNumber = 0;                           // SELECT ... INTO ... FETCH FIRST |1|n ROW|ROWS ONLY
    private String isolation = "";                                  // SELECT ... INTO ... RR|RS|CS|UR
    private int queryno = 0;                                        // SELECT ... INTO ... QUERYNO n
    
    // Opzioni presenti
    private boolean isSelectAll = false;                             // SELECT ALL ...
    private boolean isSelectDistinct = false;                        // SELECT DISTINCT ...
    
    private boolean isSelectColsAsStar = false;                      // SELECT  * FROM ...
    private boolean isSelectColsAsTableViewStar = false;             // SELECT  tableName|ViewName.* FROM ...
    private boolean isSelectColsAsCorrNameStar = false;              // SELECT  CorrelationName.* FROM ...
    private boolean isSelectColsAsExpressions = false;            	 // SELECT  expr1 AS newCol1Name, expr2, ... 
    
    private boolean isInto = false;                                  // SELECT ... INTO ...
    private boolean isWhere = false;                                 // SELECT ... WHERE ...
    private boolean isGroupBy = false;                               // SELECT ... GROUP BY ...
    private boolean isHaving = false;                                // SELECT ... HAVING ...
    private boolean isOrderBy = false;                               // SELECT ... ORDER BY ...
    private boolean isFetchFirstNRows = false;                       // SELECT ... FETCH FIRST |1|n ROW|ROWS ONLY
    private boolean isSelectInto = false;                            // SELECT ... INTO ...
    private boolean isQueryno = false;                               // SELECT ... INTO ... QUERYNO n
    private boolean isSkipLockData = false;                          // SELECT ... INTO ... SKIP LOCK DATA
    private boolean isIsolation = false;                             // SELECT ... INTO ... WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepUpdateLocks = false;        // SELECT ... INTO ... WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepShareLocks = false;         // SELECT ... INTO ... WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepExclusiveLocks = false;     // SELECT ... INTO ... WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    
    
	/**
	 * Costruttore vuoto
	 */
	public SqlSubselectSelectInto() {
		super();
		al_hostVarInto = new ArrayList<String> ();
		map_hostVarIndicatorInto = new HashMap<String, String> ();
		al_column = new ArrayList<SqlColumnInSelect> ();		 
		al_from = new ArrayList<SqlFromTableReference> ();
	    al_groupBy = new ArrayList<SqlExpression> ();
	    al_orderBy = new ArrayList<SqlOrderBy> ();
	}



	



	/**
	 * Restituisce i descrittori delle colonne dichiarate nello statement <br>
	 * <p>
	 * <tt>SELECT expr1 AS newColName1, expr2, expr3 AS ...</tt><br>
	 * <p>
	 * Nella forma più semplice le colonne sono codificate con una espressione di un solo<br>
	 * elemento che rappresenta il nome della colonna come definita nel database.<br>
	 * Oppure sono codificate con la notazione con * per indicare tutte le colonne.<br>
	 * Nel caso più generale sono un'espressione comunque complessa.<br>
	 * Ogni elemento restituito, che sia o meno seguito dalla clausola<br>
	 * <tt>AS</tt> ha il suo corrispondente nell'insieme fornito dal metodo <br>
	 * <tt>getColumnExpressionNewColNames()</tt>
	 * <p>
	 * @return the al_column
	 */
	public ArrayList<SqlColumnInSelect> getColumns() {
		return al_column;
	}





	/**
	 * Imposta i descrittori delle colonne dichiarato nella <br>
	 * <p>
	 * <tt>SELECT expr1 AS newColName1, expr2, expr3 AS ...</tt><br>
	 * <p>
	 * Nella forma più semplice le colonne sono codificate con una espressione di un solo<br>
	 * elemento che rappresenta il nome della colonna come definita nel database.<br>
	 * Oppure sono codificate con la notazione con * per indicare tutte le colonne.<br>
	 * Nel caso più generale sono un'espressione comunque complessa.<br>
	 * Ogni elemento restituito, che sia o meno seguito dalla clausola<br>
	 * <tt>AS</tt> ha il suo corrispondente nell'insieme fornito dal metodo <br>
	 * <tt>getColumnExpressionNewColNames()</tt>
	 * <p>
	 * @param al_column the al_column to set
	 */
	public void setColumns(ArrayList<SqlColumnInSelect> al_column) {
		this.al_column = al_column;
	}








	/**
	 * Restituisce le variabili host in  <tt>output</tt>, che iniziano con <tt>:</tt> dichiarate nella <br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, grp2.hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * Non si restituiscono le variabili host di INDICATOR, anche se presenti.
	 * <p>
	 * Le variabili vengono restituite senza i : iniziali.<br>
	 * <p>
	 * @return the al_hostVarInto
	 */
	public ArrayList<String> getHostVarsIntoNoIndicator() {
		
		ArrayList<String> al_hostVar = null;
		String hostVar = "";
		int i = 0;

		al_hostVar = new ArrayList<String> ();
		
		// Scan variabli host
		for (String hostVarInto : this.al_hostVarInto) {
			
			// Inserimento host var
			hostVar = hostVarInto.substring(1);
			i = hostVar.indexOf(".");
			if (i > 0) {
				hostVar = hostVar.substring(i + 1);
			}
			al_hostVar.add(hostVar);
		}
		
		return al_hostVar;
	}

	/**
	 * Restituisce le variabili host in  <tt>output</tt>, che iniziano con <tt>:</tt> dichiarate nella <br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, grp2.hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * Non si restituiscono le variabili host di INDICATOR, anche se presenti.
	 * <p>
	 * Le variabili vengono restituite come sono trovate, con i : iniziali.<br>
	 * <p>
	 * @return the al_hostVarInto
	 */
	public ArrayList<String> getHostVarsIntoPure() {
		return this.al_hostVarInto;
	}

	/**
	 * Restituisce le variabili host in  <tt>output</tt>, che iniziano con <tt>:</tt> dichiarate nella <br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, grp2.hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * Si restituiscono anche le variabili host di INDICATOR, quamdo presenti.
	 * <p>
	 * Le variabili vengono restituite senza i : iniziali.<br>
	 * <p>
	 * @return the al_hostVarInto
	 */
	public ArrayList<String> getHostVarsInto() {
		ArrayList<String> al_hostVar = null;
		String hostVar = "";
		int i = 0;
		
		al_hostVar = new ArrayList<String> ();
		
		// Scan variabli host
		for (String hostVarInto : this.al_hostVarInto) {
			if (hostVarInto.trim().contentEquals("")) {
				continue;
			}
			
			// Inserimento host var
			hostVar = hostVarInto.substring(1);
			i = hostVar.indexOf(".");
			if (i > 0) {
				hostVar = hostVar.substring(i + 1);
			}
			al_hostVar.add(hostVar);
			
		}
		
		// Scan variabili host indicator
		for (String hostVarIndicator : this.map_hostVarIndicatorInto.values()) {
			
			// Indicator non presente
			if (hostVarIndicator.equals("")) {
				continue;
			}
			
			// Inserimento host var indicator 
			hostVar = hostVarIndicator.substring(1);
			i = hostVar.indexOf(".");
			if (i > 0) {
				hostVar = hostVar.substring(i + 1);
			}
			al_hostVar.add(hostVar);
		}
		
		return al_hostVar;
	}





	/**
	 * Imposta le variabili host in  <tt>output</tt>, che iniziano con <tt>:</tt> dichiarate nella <br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1, hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono impostate senza i due punti iniziali.<br>
	 * <p>
	 * @param al_hostVarInto the al_hostVarInto to set
	 */
	public void setHostVarsInto(ArrayList<String> al_hostVarInto) {
		this.al_hostVarInto = al_hostVarInto;
	}


	
	
	/**
	 * Restituisce la map hostVar hostVarIndicator.<br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Gli indicator non sono obbligatori.<br>
	 * <p>
	 * 
	 * @return the map_hostVarIndicatorInto
	 */
	public Map<String, String> getHostVarIndicatorIntoMap() {
		return map_hostVarIndicatorInto;
	}


	/**
	 * Imposta la map hostVar hostVarIndicator.<br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1 INDICATOR hostVar1Ind, hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Gli indicator non sono obbligatori.<br>
	 * <p>
	 * 
	 * @param mapHostVarIndicatorInto the map_hostVarIndicatorInto to set
	 */
	public void setHostVarIndicatorIntoMap(Map<String, String> map_hostVarIndicatorInto) {
		this.map_hostVarIndicatorInto = map_hostVarIndicatorInto;
	}







	/**
	 * Restituisce le variabili host in <tt>input</tt>, che iniziano con <tt>:</tt> <br>
	 * dichiarate nella <tt>SELECT ... WHERE col1 = :hostVar1 AND col2 = hostVar2 ...</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return the host var in where search conditions
	 */
	public ArrayList<String> getHostVarsWhere() {
		// Where presente
		if (isWhere()) {
			return this.where.getHostVars();
		}
		return new ArrayList<String> ();
	}





	/**
	 * Imposta le variabili host in input, che iniziano con <tt>:</tt> <br>
	 * dichiarate nella <tt>SELECT ... WHERE col1 = :hostVar1 AND col2 = hostVar2 ...</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono impostate senza i due punti iniziali.<br>
	 * <p>
	 * @param al_hostVarWhere the al_hostVarWhere to set
	 */
	public void setHostVarsWhere(ArrayList<String> al_hostVarInto) {
		this.al_hostVarInto = al_hostVarInto;
	}





	/**
	 * Restituisce i descrittori dei table-reference presenti nell'istruzione 
	 * <p>
	 * <tt>SELECT ... FROM table-reference1, table-reference2, ..</tt>
	 * <p>
	 * @return the al_from
	 */
	public ArrayList<SqlFromTableReference> getFromTableReferences() {
		return al_from;
	}





	/**
	 * Imposta i descrittori dei table-reference presenti <br>
	 * nell'istruzione <tt>SELECT ... FROM table-reference1, table-reference2, ..</tt>
	 * <p>
	 * @param alFrom the al_from to set
	 */
	public void setFromTableReferences(ArrayList<SqlFromTableReference> al_from) {
		this.al_from = al_from;
	}





	/**
	 * Restituisce la search-condition codificata nell'istruzione <br>
	 * <tt>SELECT ... WHERE search-condition</tt>
	 * <p>
	 * @return the where
	 */
	public SqlSearchConditions getWhere() {
		return where;
	}





	/**
	 * Imposta la search-condition codificata nell'istruzione <br>
	 * <tt>SELECT ... WHERE search-condition</tt>
	 * <p>
	 * @param where the where to set
	 */
	public void setWhere(SqlSearchConditions where) {
		this.where = where;
	}





	/**
	 * Restituisce le espressioni di grouping codificate da<br>
	 * <tt>SELECT ... GROUP BY expr1, expr2, ...</tt>
	 * <p>
	 * Nel caso più sempice un espressione rappresenta il nome di una colonna.<br>
	 * <p>
	 * @return the al_groupBy
	 */
	public ArrayList<SqlExpression> getGroupByExpressions() {
		return al_groupBy;
	}





	/**
	 * Restituisce le espressioni di grouping codificate da<br>
	 * <tt>SELECT ... GROUP BY expr1, expr2, ...</tt>
	 * <p>
	 * Nel caso più sempice un espressione rappresenta il nome di una colonna.<br>
	 * <p>
	 * @param alGroupBy the al_groupBy to set
	 */
	public void setGroupByExpressions(ArrayList<SqlExpression> al_groupBy) {
		this.al_groupBy = al_groupBy;
	}





	/**
	 * Restituisce la search-condition codificata nell'istruzione <br>
	 * <tt>SELECT ... HAVING search-condition</tt>
	 * <p>
	 * @return the having
	 */
	public SqlSearchConditions getHaving() {
		return having;
	}





	/**
	 * Imposta la search-condition codificata nell'istruzione <br>
	 * <tt>SELECT ... HAVING search-condition</tt>
	 * <p>
	 * @param having the having to set
	 */
	public void setHaving(SqlSearchConditions having) {
		this.having = having;
	}





	/**
	 * Restituisce gli elementi di ordinamento codificati nell'istruzione <br>
	 * <tt>SELECT ... ORDER BY col1 ASC, col2, ...</tt>
	 * <p>
	 * @return the al_orderBy
	 */
	public ArrayList<SqlOrderBy> getOrderByElements() {
		return this.al_orderBy;
	}





	/**
	 * Imposta gli elementi di ordinamento codificati nell'istruzione <br>
	 * <tt>SELECT ... ORDER BY col1 ASC, col2, ...</tt>
	 * <p>
	 * @param al_orderBy the al_orderBy to set
	 */
	public void setOrderByElements(ArrayList<SqlOrderBy> al_orderBy) {
		this.al_orderBy = al_orderBy;
	}





	/**
	 * Restituisce il numero massimo di righe da restituire
	 * come specificato dal parametro <tt>FETCH FIRST n ROWS ONLY</tt>
	 * <p>
	 * @return the fetchFirstRowsNumber
	 */
	public int getFetchFirstRowsNumber() {
		return fetchFirstRowsNumber;
	}





	/**
	 * Imposta il numero massimo di righe da restituire
	 * come specificato dal parametro <tt>FETCH FIRST n ROWS ONLY</tt>
	 * <p>
	 * @param fetchFirstRowsNumber the fetchFirstRowsNumber to set
	 */
	public void setFetchFirstRowsNumber(int fetchFirstRowsNumber) {
		this.fetchFirstRowsNumber = fetchFirstRowsNumber;
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
	 * Restituisce se codificata una <tt>SELECT ALL</tt>
	 * <p>
	 * @return the isSelectAll
	 */
	public boolean isSelectAll() {
		return isSelectAll;
	}





	/**
	 * Imposta se codificata una <tt>SELECT ALL</tt>
	 * <p>
	 * @param isSelectAll the isSelectAll to set
	 */
	public void setSelectAll(boolean isSelectAll) {
		this.isSelectAll = isSelectAll;
	}





	/**
	 * Restituisce se codificata una <tt>SELECT DISTINCT</tt>
	 * <p>
	 * @return the isSelectDistinct
	 */
	public boolean isSelectDistinct() {
		return isSelectDistinct;
	}





	/**
	 * Imposta se codificata una <tt>SELECT DISTINCT</tt>
	 * <p>
	 * @param isSelectDistinct the isSelectDistinct to set
	 */
	public void setSelectDistinct(boolean isSelectDistinct) {
		this.isSelectDistinct = isSelectDistinct;
	}





	/**
	 * Restituisce se sono richieste tutte le colonne a fronte di <tt>SELECT * FROM ..</tt>
	 * <p>
	 * @return the isSelectColsAsStar
	 */
	public boolean isSelectColsAsStar() {
		return isSelectColsAsStar;
	}





	/**
	 * Imposta se sono richieste tutte le colonne a fronte di <tt>SELECT * FROM ..</tt>
	 * <p>
	 * @param isSelectColsAsStar the isSelectColsAsStar to set
	 */
	public void setSelectColsAsStar(boolean isSelectColsAsStar) {
		this.isSelectColsAsStar = isSelectColsAsStar;
	}





	/**
	 * Restituisce se sono richieste tutte le colonne di una tabella o view<br>
	 * a fronte di <tt>SELECT table|view.* FROM ..</tt>
	 * <p>
	 * @return the isSelectColsAsTableViewStar
	 */
	public boolean isSelectColsAsTableViewStar() {
		return isSelectColsAsTableViewStar;
	}





	/**
	 * Imposta se sono richieste tutte le colonne di una tabella o view<br>
	 * a fronte di <tt>SELECT table|view.* FROM ..</tt>
	 * <p>
	 * @param isSelectColsAsTableViewStar the isSelectColsAsTableViewStar to set
	 */
	public void setSelectColsAsTableViewStar(boolean isSelectColsAsTableViewStar) {
		this.isSelectColsAsTableViewStar = isSelectColsAsTableViewStar;
	}

	/**
	 * Restituisce se sono richieste tutte le colonne di una tabella espressa <br>
	 * con il suo correlation name a fronte di <br>
	 * <p>
	 * <tt>SELECT corrName.* FROM table AS corrName</tt><br>
	 * <p>
	 * @return the isSelectColsAsTableViewStar
	 */
	public boolean isSelectColsAsCorrNameStar() {
		return isSelectColsAsCorrNameStar;
	}





	/**
	 * Imposta se sono richieste tutte le colonne di una tabella espressa <br>
	 * con il suo correlation name a fronte di <br>
	 * <p>
	 * <tt>SELECT corrName.* FROM table AS corrName</tt><br>
	 * <p>
	 * @param isSelectColsAsTableViewStar the isSelectColsAsTableViewStar to set
	 */
	public void setSelectColsCorrNameStar(boolean isSelectColsAsTableViewStar) {
		this.isSelectColsAsTableViewStar = isSelectColsAsTableViewStar;
	}





	/**
	 * Restituisce se sono richieste le colonne come lista di espressioni<br>
	 * a fronte di <tt>SELECT expr1 AS newCol1, expr2, ... FROM ..</tt>
	 * <p>
	 * Una espressione è nel caso limite e più normale, il nome di una colonna.<br>
	 * <p>
	 * @return the isSelectColsAsExpressions
	 */
	public boolean isSelectColsAsExpressions() {
		return isSelectColsAsExpressions;
	}





	/**
	 * Imposta se sono richieste le colonne come lista di espressioni<br>
	 * a fronte di <tt>SELECT expr1 AS newCol1, expr2, ... FROM ..</tt>
	 * <p>
	 * @param isSelectColsAsExpressions the isSelectColsAsExpressions to set
	 */
	public void setSelectColsAsExpressions(boolean isSelectColsAsExpressions) {
		this.isSelectColsAsExpressions = isSelectColsAsExpressions;
	}



	/**
	 * Restituisce se presente la clausola where<br>
	 * a fronte di <tt>SELECT ... WHERE ..</tt>
	 * <p>
	 * @return the isWhere
	 */
	public boolean isWhere() {
		return isWhere;
	}





	/**
	 * Imposta se presente la clausola where<br>
	 * a fronte di <tt>SELECT ... WHERE ..</tt>
	 * <p>
	 * @param isWhere the isWhere to set
	 */
	public void setWhere(boolean isWhere) {
		this.isWhere = isWhere;
	}

	/**
	 * Restituisce se presente la clausola into<br>
	 * a fronte di <tt>SELECT ... INTO ..</tt>
	 * <p>
	 * @return the isInto
	 */
	public boolean isInto() {
		return isInto;
	}





	/**
	 * Imposta se presente la clausola into<br>
	 * a fronte di <tt>SELECT ... INTO ..</tt>
	 * <p>
	 * @param isInto the isInto to set
	 */
	public void setInto(boolean isInto) {
		this.isInto = isInto;
	}





	/**
	 * Restituisce se presente la clausola group by<br>
	 * a fronte di <tt>SELECT ... GROUP BY ..</tt>
	 * <p>
	 * @return the isGroupBy
	 */
	public boolean isGroupBy() {
		return isGroupBy;
	}





	/**
	 * Imposta se presente la clausola group by<br>
	 * a fronte di <tt>SELECT ... GROUP BY ..</tt>
	 * <p>
	 * @param isGroupBy the isGroupBy to set
	 */
	public void setGroupBy(boolean isGroupBy) {
		this.isGroupBy = isGroupBy;
	}





	/**
	 * Restituisce se presente la clausola having<br>
	 * a fronte di <tt>SELECT ... HAVING ..</tt>
	 * <p>
	 * @return the isHaving
	 */
	public boolean isHaving() {
		return isHaving;
	}





	/**
	 * Imposta se presente la clausola having<br>
	 * a fronte di <tt>SELECT ... HAVING ..</tt>
	 * <p>
	 * @param isHaving the isHaving to set
	 */
	public void setHaving(boolean isHaving) {
		this.isHaving = isHaving;
	}





	/**
	 * Restituisce se presente la clausola order by<br>
	 * a fronte di <tt>SELECT ... ORDER BY ..</tt>
	 * <p>
	 * @return the isOrderBy
	 */
	public boolean isOrderBy() {
		return isOrderBy;
	}





	/**
	 * Imposta se presente la clausola order by<br>
	 * a fronte di <tt>SELECT ... ORDER BY ..</tt>
	 * <p>
	 * @param isOrderBy the isOrderBy to set
	 */
	public void setOrderBy(boolean isOrderBy) {
		this.isOrderBy = isOrderBy;
	}





	/**
	 * Restituisce se presente la clausola di limitazione righe<br>
	 * a fronte di <tt>FETCH FIRST n ROWS ONLY ..</tt>
	 * <p>
	 * @return the isFetchFirstNRows
	 */
	public boolean isFetchFirstNRows() {
		return isFetchFirstNRows;
	}





	/**
	 * Imposta se presente la clausola di limitazione righe<br>
	 * a fronte di <tt>FETCH FIRST n ROWS ONLY ..</tt>
	 * <p>
	 * @param isFetchFirstNRows the isFetchFirstNRows to set
	 */
	public void setFetchFirstNRows(boolean isFetchFirstNRows) {
		this.isFetchFirstNRows = isFetchFirstNRows;
	}





	/**
	 * Restituisce se la selec aggiorna variabili host<br>
	 * a fronte di <tt>SELECT ... INTO ..</tt>
	 * <p>
	 * @return the isSelectInto
	 */
	public boolean isSelectInto() {
		return isSelectInto;
	}





	/**
	 * Imposta se la selec aggiorna variabili host<br>
	 * a fronte di <tt>SELECT ... INTO ..</tt>
	 * <p>
	 * @param isSelectInto the isSelectInto to set
	 */
	public void setSelectInto(boolean isSelectInto) {
		this.isSelectInto = isSelectInto;
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
	 * Restituisce se presente <tt>SKIP LOCK DATA</tt>
	 * <p>
	 * @return the isSkipLockData
	 */
	public boolean isSkipLockData() {
		return isSkipLockData;
	}


	/**
	 * Imposta se presente <tt>SKIP LOCK DATA</tt>
	 * <p>
	 * @param isSkipLockData the isSkipLockData to set
	 */
	public void setSkipLockData(boolean isSkipLockData) {
		this.isSkipLockData = isSkipLockData;
	}


	/**
	 * Restituisce se presente definizione di isolation level a <tt>RR, RS, CS o UR</tt>
	 * <p>
	 * @return the isIsolation
	 */
	public boolean isIsolation() {
		return isIsolation;
	}


	/**
	 * Imposta se presente definizione di isolation level a <tt>RR, RS, CS o UR</tt>
	 * <p>
	 * @param isIsolation the isIsolation to set
	 */
	public void setIsolation(boolean isIsolation) {
		this.isIsolation = isIsolation;
	}


	/**
	 * Restituisce se presente l'opzione di isolation level <tt>USE AND KEEP UPDATE</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @return the isIsolationUseAndKeepUpdateLocks
	 */
	public boolean isIsolationUseAndKeepUpdateLocks() {
		return isIsolationUseAndKeepUpdateLocks;
	}


	/**
	 * Imposta se presente l'opzione di isolation level <tt>USE AND KEEP UPDATE</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @param isIsolationUseAndKeepUpdateLocks the isIsolationUseAndKeepUpdateLocks to set
	 */
	public void setIsolationUseAndKeepUpdateLocks(boolean isIsolationUseAndKeepUpdateLocks) {
		this.isIsolationUseAndKeepUpdateLocks = isIsolationUseAndKeepUpdateLocks;
	}


	/**
	 * Restituisce se presente l'opzione di isolation level <tt>USE AND KEEP SHARE</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @return the isIsolationUseAndKeepShareLocks
	 */
	public boolean isIsolationUseAndKeepShareLocks() {
		return isIsolationUseAndKeepShareLocks;
	}


	/**
	 * Imposta se presente l'opzione di isolation level <tt>USE AND KEEP SHARE</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @param isIsolationUseAndKeepShareLocks the isIsolationUseAndKeepShareLocks to set
	 */
	public void setIsolationUseAndKeepShareLocks(boolean isIsolationUseAndKeepShareLocks) {
		this.isIsolationUseAndKeepShareLocks = isIsolationUseAndKeepShareLocks;
	}


	/**
	 * Restituisce se presente l'opzione di isolation level <tt>USE AND KEEP EXCLUSIVE LOCK</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @return the isIsolationUseAndKeepExclusiveLocks
	 */
	public boolean isIsolationUseAndKeepExclusiveLocks() {
		return isIsolationUseAndKeepExclusiveLocks;
	}


	/**
	 * Imposta se presente l'opzione di isolation level <tt>USE AND KEEP EXCLUSIVE LOCK</tt><br>
	 * valida per isolation <tt>RR, RS</tt>
	 * <p>
	 * @param isIsolationUseAndKeepExclusiveLocks the isIsolationUseAndKeepExclusiveLocks to set
	 */
	public void setIsolationUseAndKeepExclusiveLocks(
			boolean isIsolationUseAndKeepExclusiveLocks) {
		this.isIsolationUseAndKeepExclusiveLocks = isIsolationUseAndKeepExclusiveLocks;
	}

	/**
	 * Restituisce informazioni sulle tabelle o sulle view utilizzate dall'istruzione dichiarate nella clausola <tt>FROM</tt><br>
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
		
		ArrayList<ArrayList<String>> al_al_entity = null;
		ArrayList<ArrayList<String>> al_al_entitySingleReference = null;
		al_al_entity = new ArrayList<ArrayList<String>>();
		
		// Scan FROM clause entries
		for (SqlFromTableReference fromReference : al_from) {
			
			// Si utilizza il metodo getEntities() di fromReference
			al_al_entitySingleReference = fromReference.getEntities();
			al_al_entity.addAll(al_al_entitySingleReference);
			
		}
		
		return al_al_entity;
	}

	/**
	 * Restituisce informazioni dettagliate sulle colonne delle tabelle o view dichiarate nella clausola <tt>SELECT</tt><br>
	 * ordinate per entity. Si tratta <br>
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
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, una ArrayList, contiene a sua volta <b>3n+3</b> elementi ed è relativo a una tabella o view,
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt><br>
	 * <p>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
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
		
		Map<String, ArrayList<String>> map_entity = null;
		ArrayList<ArrayList<String>> al_al_columnsByEntity = null;
		ArrayList<String> al_columnEntity = null;
		ArrayList<ArrayList<String>> al_al_columnImplicit = null;
		SqlExpression sqlExpression = null;
		ArrayList<SqlExpressionElement> al_sqlExpressionElement = null;
		SqlExpressionElement sqlExpressionElement = null;
		String keyMap = "";
		
		map_entity = new HashMap<String,ArrayList<String>> ();
		al_al_columnsByEntity = new ArrayList<ArrayList<String>>();
		
		// Le colonne possono essere in qualunque ordine e non aggregate per tabelle|view|..
		// a questo scopo si utilizza la map map_entity
		
		// Scan columns dichiarate in SELECT
		for (SqlColumnInSelect sqlColumn : this.al_column) {
			
			// Recupero/generazione entry map con key informazioni di identificazione 
			keyMap = sqlColumn.getTableViewName() + "-" + sqlColumn.getTableViewOwner() + "-" + sqlColumn.getColumnQualifier();
			al_columnEntity = map_entity.get(keyMap);
			if (map_entity.get(keyMap) == null) {
		    	al_columnEntity = new ArrayList<String> ();
		    	al_columnEntity.add(sqlColumn.getTableViewName());			
				al_columnEntity.add(sqlColumn.getTableViewOwner());
				al_columnEntity.add(sqlColumn.getColumnQualifier());
				map_entity.put(keyMap, al_columnEntity);
			}
			
			// La colonna è codificata da una sql expression, al limite una colonna specifica
			if (sqlColumn.isByExpression()) {
				sqlExpression =  sqlColumn.getExpression();
				al_sqlExpressionElement = sqlExpression.getElements();
				// L'sql expression codifica una singola colonna
				if (al_sqlExpressionElement.size() == 1) {
					sqlExpression = sqlColumn.getExpression();
					sqlExpressionElement = al_sqlExpressionElement.get(0);
 				    // Se si tratta di COLUMN_NAME sono presenti anche le informazioni sulla tabella
					// Accodamento nome definizione colonna e new column name utilizzato
					al_columnEntity.add(sqlExpressionElement.getColumnName());	
				}
				al_columnEntity.add(sqlColumn.getAsNewColumnName());		// Opzionale, può essere stringa vuota
				al_columnEntity.add("E");                                   // Colonna definita esplicitamente        
				continue;
			} 
			
			// La colonna è codificata da *, table-name.*, view-name.* o corr-name.*
			// Estraggo tutte le colonne definite da CREATE TABLE e memorizzate al momento dell'analisi
			al_al_columnImplicit = sqlColumn.getColumnsImplicit();
			
			// Non sono state identificate le colonne implicite, completo comunque l'indicazione
			if (al_al_columnImplicit.size() == 0) {
		    	al_columnEntity.add("");									// tableName
				al_columnEntity.add("");									// owner (può essere "")
				al_columnEntity.add("");									// correlationName (può essere "")
				al_columnEntity.add("");									// columnName
				al_columnEntity.add("");									// new columnName
				al_columnEntity.add("I");									// Colonna definita implicitamente 
				continue;
			}
			
			// Colonne implicite individuate, accodo 
			for (ArrayList<String> al_columnImplicit : al_al_columnImplicit) {
		    	al_columnEntity.add(al_columnImplicit.get(1));				// tableName
				al_columnEntity.add(al_columnImplicit.get(2));				// owner (può essere "")
				al_columnEntity.add(al_columnImplicit.get(3));				// correlationName (può essere "")
				al_columnEntity.add(al_columnImplicit.get(0));				// columnName
				al_columnEntity.add("");									// new columnName
				al_columnEntity.add("I");									// Colonna definita implicitamente                                   
			}
			
		} // end-while columns
		
		
		// Si portano in output le colonne aggregate per informazioni di identificazione 
		for (ArrayList<String> al_columnEntityFromMap : map_entity.values()) {
			al_al_columnsByEntity.add(al_columnEntityFromMap);
		}
		
		return al_al_columnsByEntity;
	}


	/**
	 * Fornisce i nomi delle variabili host in input utilizzate con l'informazione della tabella di appartenenza.<br>
	 * <p>
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, una ArrayList, contiene a sua volta <b>n+1</b> elementi ed è relativo a una tabella o view,<br>
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt> per la tabella<br>
	 * 
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name<br></li>
	 * <li>I successivi <b>n</b> elementi sono i nomi delle variabili host utilizzate in input, nell'ordine, <br>
	 *     dalla tabella. Si tratta di variabili host dichiarate in clausole <tt>WHERE</tt>, funzioni e altri<br>
     *     costrutti ma NON nella clausola <tt>INTO</tt>.<br>
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
		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// TODO
		
		return al_al_hostVar;
	}

	/**
	 * Fornisce i nomi delle variabili host in output, con il nome di colonna associato, per tabella di appartenenza,<br>
	 * per ogni tabella le cui colonne sono dichiarate nella clausola <tt>SELECT</tt> 
	 * <p>
	 * 
	 * 
	 * Si tratta delle variabili host e delle colonne in  <tt>output</tt>, raggruppate per  nome tabella, dichiarate nelle<br> 
	 * clausole <tt>SELECT</tt> e <tt>INTO</tt> nello statement: <br>
	 * <p>
	 * <tt>SELECT col1, ..., coln INTO hostVar1, ..., hostVarn FROM table1, tablen ...WHERE ...</tt><br>
	 * <p>
	 * Per ottenere informazioni ulteriori sulle tabelle, quali owner e correlation-name,<br>
	 * si utilizza il metodo <tt>getEntities()</tt> e si cerca l'elemento della tabella che interessa.<br>
	 * <p>
	 * 
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, a sua volta un ArrayList, contiene <b>2n+1</b> elementi ed è relativo a una tabella o view,<br>
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt> per la tabella<br>
	 * 
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Le successive <b>n</b> coppie di elementi sono, rispettivamente, il nome della variabile host <br>
	 *     e il nome della colonna corrispondente.<br>
	 *     La variabile host viene restituita senza i due punti iniziali.
	 *     </li>
	 * </ul>
	 * <p>
     * 
     * 
     * 
	 * @return the host vars by table
	 */
	public ArrayList<ArrayList<String>> getHostVarsIntoByEntity() {
		
		ArrayList<ArrayList<String>> al_al_hostVar = null;
		SqlColumnInSelect sqlColumn = null;
		Map<String, ArrayList<String>> map_entity = null;
		ArrayList<String> al_hostVarEntity = null;
		SqlExpression sqlExpression = null;
		ArrayList<SqlExpressionElement> al_sqlExpressionElement = null;
		SqlExpressionElement sqlExpressionElement = null;
		String keyMap = "";
		String hostVar = "";
		int iHostVar = 0;
		
		map_entity = new HashMap<String,ArrayList<String>> ();
		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// Le colonne possono essere in qualunque ordine e non aggregate per tabelle|view|..
		// a questo scopo si utilizza la map  
		
		// Scan columns dichiarate in SELECT
		for (int i = 0; i < this.al_column.size(); i++) {
			
			sqlColumn = this.al_column.get(i);
			hostVar = this.al_hostVarInto.get(iHostVar++);
			
			// Recupero/generazione entry map con key informazioni di identificazione tabella
			keyMap = sqlColumn.getTableViewName();
			al_hostVarEntity = map_entity.get(keyMap);
			if (map_entity.get(keyMap) == null) {
		    	al_hostVarEntity = new ArrayList<String> ();
		    	al_hostVarEntity.add(sqlColumn.getTableViewName());			
				map_entity.put(keyMap, al_hostVarEntity);
			}
			
			// La colonna è codificata da una sql expression 
			if (sqlColumn.isByExpression()) {
				sqlExpression =  sqlColumn.getExpression();
				al_sqlExpressionElement = sqlExpression.getElements();
				// L'sql expression è di un solo elemento
				if (al_sqlExpressionElement.size() == 1) {
					sqlExpression = sqlColumn.getExpression();
					sqlExpressionElement = al_sqlExpressionElement.get(0);
					// L'elemento è una colonna sql
					if (sqlExpressionElement.getTypeElement() == EnumSqlExpressionElementType.COLUMN_NAME) {
						al_hostVarEntity.add(hostVar);								// host-var
						al_hostVarEntity.add(sqlExpressionElement.getColumnName());	// columnName
						continue;
					}
 				}
				// L'elemento è singolo e NON è una colonna o espressione complessa (nessuna colonna di riferimento)
				al_hostVarEntity.add(hostVar);
				al_hostVarEntity.add("");
				continue;
			} 
			
			// La colonna è codificata da *, table-name.*, view-name.* o corr-name.*
			// Estraggo tutte le colonne definite da CREATE TABLE e memorizzate al momento dell'analisi
			for (ArrayList<String> al_columnImplicit : sqlColumn.getColumnsImplicit(sqlColumn.getTableViewName())) {
				hostVar = this.al_hostVarInto.get(iHostVar++);
				al_hostVarEntity.add(hostVar);								// host-var
				al_hostVarEntity.add(al_columnImplicit.get(0));				// columnName
			}
			
		} // end-while columns
		
		
		// Si portano in output le host-var e le colonne per ogni tabella
		for (ArrayList<String> al_columnEntityFromMap : map_entity.values()) {
			al_al_hostVar.add(al_columnEntityFromMap);
		}

		return al_al_hostVar;
	}

	/**
	 * Fornisce i nomi delle variabili host in output, con il nome di colonna associato, per la tabella richiesta,<br>
	 * dichiarata nella clausola <tt>SELECT</tt> 
	 * <p>
	 * 
	 * 
	 * Si tratta delle variabili host e delle colonne in  <tt>output</tt>, dichiarate nelle<br> 
	 * clausole <tt>SELECT</tt> e <tt>INTO</tt> nello statement: <br>
	 * <p>
	 * <tt>SELECT col1, ..., coln INTO hostVar1, ..., hostVarn FROM table1, tablen ...WHERE ...</tt><br>
	 * <p>
	 * Per ottenere informazioni ulteriori sulla tabelle, quali owner e correlation-name,<br>
	 * si utilizza il metodo <tt>getEntities()</tt> e si cerca l'elemento della tabella che interessa.<br>
	 * <p>
	 * 
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, a sua volta un ArrayList, contiene <b>2n+1</b> elementi ed è relativo a una tabella o view,<br>
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt> per la tabella<br>
	 * 
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Le successive <b>n</b> coppie di elementi sono, rispettivamente, il nome della variabile host <br>
	 *     e il nome della colonna corrispondente.<br>
	 *     La variabile host viene restituita senza i due punti iniziali.
	 *     </li>
	 * </ul>
	 * <p>
     * 
     * 
     * @param String tableName
	 * @return the host vars by table
	 */
	public ArrayList<String> getHostVarsIntoByEntity(String tableName) {
		
		ArrayList<ArrayList<String>> al_al_hostVarAll = null;

		al_al_hostVarAll = getHostVarsIntoByEntity();
		
		// Scan generale tabelle
		for (ArrayList<String> al_hostVarSingle : al_al_hostVarAll) {
			
			// Non è la tabella che interessa
			if (!al_hostVarSingle.get(0).equals(tableName)) {
				continue;
			}
			
			return al_hostVarSingle;
		}
		
		// Tabella fornita non presente
		
		return new ArrayList<String> ();
	}




	/**
	 * Restituisce tutte le variabili host presenti nelle clausole WHERE, HAVING e GROUP BY
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		
		ArrayList<String> al_hostVarAll = null;
		ArrayList<String> al_hostVarSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;
		
		al_hostVarAll = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Costanti estratte da where
		if (this.isWhere) {
			al_hostVarAll.addAll(this.where.getHostVars());
		}
		
		// Costanti estratte da Group By
		if (this.isGroupBy) {
			for (SqlExpression sqlExpression : this.getGroupByExpressions()) {
				al_hostVarAll.addAll(sqlExpression.getHostVars());
			}
		}

		// Costanti estratte ricorsivamente da tutti i predicati presenti in having
		if (this.isHaving) {
			getHostVarsPredicatesRecursive(this.having, al_sqlPredicate);
			
			// Scan predicati individuati
			for (SqlPredicate sqlPredicate : al_sqlPredicate) {
				al_hostVarSinglePredicate = sqlPredicate.getHostVars();
				al_hostVarAll.addAll(al_hostVarSinglePredicate);
			}
		}
 		
		return al_hostVarAll;
	}


	/*
	 * Accoda ricorsivamente i predicati della search condition
	 */
	private void getHostVarsPredicatesRecursive(SqlSearchConditions sqlSearchConditions, ArrayList<SqlPredicate> al_sqlPredicate) {
		
		if (sqlSearchConditions.isPredicate()) {
			al_sqlPredicate.add(sqlSearchConditions.getPredicate());
		} else {
			getHostVarsPredicatesRecursive(sqlSearchConditions.getSearchCondition(), al_sqlPredicate);
		}
		
		// Scan search conditions in AND OR
		for (SqlSearchConditions searchConditions : sqlSearchConditions.getSearchConditions()) {
			getHostVarsPredicatesRecursive(searchConditions, al_sqlPredicate);
		}
	}




	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti nelle clausole<br>
	 * <p>
	 * WHERE, HAVING e GROUP BY.<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
		ArrayList<String> al_constantAlpha = null;
		ArrayList<String> al_constantAlphaSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;

        al_constantAlpha = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Recupero ricorsivamente le costanti di tutti i predicati presenti in having
		if (isHaving) {
			getHostVarsPredicatesRecursive(this.having, al_sqlPredicate);
			// Scan predicati individuati
			for (SqlPredicate sqlPredicate : al_sqlPredicate) {
				al_constantAlphaSinglePredicate = sqlPredicate.getConstantsAlphanumeric();
				al_constantAlpha.addAll(al_constantAlphaSinglePredicate);
			}
		}
		

		// Recupero ricorsivamente le costanti di tutti i predicati presenti in having in Group By
		if (isGroupBy) {
			for (SqlExpression sqlExpression : this.getGroupByExpressions()) {
				al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());
			}
		}

		// Recupero ricorsivamente le costanti di tutti i predicati presenti in where
		if (isWhere) {
			al_constantAlpha.addAll(this.where.getConstantsAlphanumeric());
		}

		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti nelle clausole <br>
	 * <p>
	 * WHERE, HAVING e GROUP BY.<br>
	 * <p>
	 * Vengono restituite stringhe contenenti numeri validi.<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		
		ArrayList<String> al_constantNumeric = null;
		ArrayList<String> al_constantNumericSinglePredicate = null;
        ArrayList<SqlPredicate> al_sqlPredicate = null;

        al_constantNumeric = new ArrayList<String> ();
		al_sqlPredicate = new ArrayList<SqlPredicate> ();
		
		// Recupero ricorsivamente tutti i predicati presenti
		 
		// Scan predicati individuati
		if (isHaving) {
			getHostVarsPredicatesRecursive(this.having, al_sqlPredicate);
			for (SqlPredicate sqlPredicate : al_sqlPredicate) {
				al_constantNumericSinglePredicate = sqlPredicate.getConstantsNumeric();
				al_constantNumeric.addAll(al_constantNumericSinglePredicate);
			}
		}

		// Group By
		if (isGroupBy) {
			for (SqlExpression sqlExpression : this.getGroupByExpressions()) {
				al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());
			}
		}
		
		// Recupero ricorsivamente le costanti di tutti i predicati presenti in where
		if (isWhere) {
			al_constantNumeric.addAll(this.where.getConstantsNumeric());
		}
		
		return al_constantNumeric;
	}



	
}
