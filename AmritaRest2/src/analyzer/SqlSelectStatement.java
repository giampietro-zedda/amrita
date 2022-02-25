
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlSelectStatement
 * </h1>
 * <p>
 * Descrive una select-statement, che produce un elenco di righe, nel modo più generale possibile.<br> 
 * <p>
 * Gli oggetti utilizzati per descrivere lo statement Select sono:<br>
 * <p>
 * {@link SqlCommonTableExpression}<br>
 * {@link SqlFullSelect}<br> 
 * {@link SqlSubselectSelectInto}<br>
 * <p>
 * <tt>SqlCommonTableExpression</tt> identifica un insieme di full-select
 * utilizzate dallo stataement e identificate da un table-identifier locale.<br>
 * <p>
 * <tt> SqlFullSelect</tt> è il corpo vero e proprio dello statement e 
 * può avere definite ricorsivamente al suo interno sia full-select che subselect.<br>
 * <p>
 * La subselect è modellata per gestire sia la <tt>SELECT</tt> sia la <tt>SELECT INTO</tt> che ha 
 * le stesse caratteristiche più il parametro INTO e altre opzioni.<br>
 * <p>
 * Se in una full-select è presente una full-select fra parentesi <tt>(full-select)</tt>
 * questa viene definita una <tt>subquery</tt><br>
 * Nella forma più elementare una ful-select contiene una sola subselect della form <tt>SELECT * FROM table</tt>
 * <p>
 * <tt>SqlSubselectSelectInto</tt> viene utilizzato indirettamente, dalla full-select
 * espressa da {@link SqlFullSelect}.<br>
 * <p>
 * <b>metodi di servizio</b>
 * <ul>
 * 
 * <li><tt>getEntities()</tt></li><br>
 * Restituisce i nomi delle tabelle o view utilizzate.<br>
 * <b>.</b>
 * 
 * <li><tt>getEntitiesColumns()</tt></li><br>
 * E' un metodo sintetico per ottenere coppie di nomi colonne e corrispondento variabili
 * host per ogni tabella utilizzata, con la clausola INTO.<br>
 * Restituisce un'array list di stringhe a due dimensioni.<br>
 * Ogni elemento, una ArrayList, contiene un insieme di stringhe.<br>
 * La prima stringa è il nome della tabella dichiarate nella select<br>
 * Gli elementi successivi sono coppie di nomi di colonna e corrispondente host-var.<br>
 * I nomi delle colonne restituite sono quelli effettivi e non il nome sintetico correlato.<br>
 * Le variabili host vengono restituite senza i due punti iniziali.<br>
 * <b>.</b>
 * 
 * <li><tt>getHostVarsByTableInput()</tt></li><br>
 * Restituisce un'array list di stringhe a due dimensioni.<br>
 * Ogni elemento, una ArrayList, contiene un insieme di stringhe.<br>
 * La prima stringa è il nome della tabella dichiarate nella select e gli elementi successivi<br>
 * sono i nomi delle variabili host utilizzate in input, nelle clausole <tt>WHERE</tt> e in altri<br>
 * costrutti, nell'ordine, dalla tabella.<br>
 * Le variabili host sono restituite senza i due punti iniziali.<br>
 * <b>.</b>
 * 
 * <li><tt>getHostVarsByTableOutput()</tt></li><br>
 * Restituisce un'array list di stringhe a due dimensioni.<br>
 * Ogni elemento, una ArrayList, contiene un insieme di stringhe.<br>
 * La prima stringa è il nome della tabella dichiarate nella select e gli elementi successivi<br>
 * sono i nomi delle variabili host utilizzate in output, nella clausola<tt>INTO</tt>,<br>
 * nell'ordine, dalla tabella.<br>
 * Le variabili host sono restituite senza i due punti iniziali.<br>
 * <b>.</b>
 * 
 * <li><tt>getConstantssAlphanumeric()</tt></li><br>
 * Restituisce un'array list di stringhe con tutte le literal alfanumeriche<br>
 * <b>.</b>
 * 
 * <li><tt>getConstantsNumeric()</tt></li><br>
 * Restituisce un'array list di Integer con tutte le literal numeriche<br>
 * <b>.</b>
 * </ul>
 *  <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement 
*/

public class SqlSelectStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Descrizione corpo istruzione
    private ArrayList<SqlCommonTableExpression> al_commonTableExpression = null;	// WITH commonTableExpression, ...
    private SqlFullSelect fullSelect = null;										// full-select
    
    // Valori associati a opzioni
    private ArrayList<String> al_forUpdateColumn = null;							// FOR UPDATE OF col1, ... ,coln
    private String isolation = "";                                                  // RR|RS|CS|UR
    private int queryno = 0;                                                        // QUERYNO n
    private int optimizeForRowsNumber = 0;                                          // OPTIMIZE FOR n ROW|ROWS
    
    // Opzioni presenti
    private boolean isCommonTableExpression = false;                                // WITH commonTableExpression, ...
    private boolean isForUpdate = false;                                        	// FOR UPDATE OF col1, ... ,coln
    private boolean isForReadOnly = false;                                        	// FOR READ ONLY
    private boolean isOptimizeForRows = false;                                      // OPTIMIZE FOR n ROW|ROWS
    private boolean isQueryno = false;                                        		// QUERYNO n
    private boolean isSkipLockData = false;                                        	// SKIP LOCK DATA
    private boolean isIsolation = false;                                        	// WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepUpdateLocks = false;                       // WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepShareLocks = false;                        // WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    private boolean isIsolationUseAndKeepExclusiveLocks = false;                    // WITH RR|RS CS|UR o WITH RR|RS USE AND KEEP UPDATE|SHARE|EXCLUSIVE LOCKS 
    
    
	/**
	 * Costruttore vuoto
	 */
	public SqlSelectStatement() {
		super();
		al_commonTableExpression = new ArrayList<SqlCommonTableExpression> ();
		al_forUpdateColumn = new ArrayList<String> ();
	}



	/**
	 * Restituisce le colonne codificate nella clausola <tt>FOR UPDATE</tt><br>
	 * <p>
	 * @return the al_forUpdateColumn
	 */
	public ArrayList<String> getForUpdateColumns() {
		return al_forUpdateColumn;
	}


	/**
	 * Imposta le colonne codificate nella clausola <tt>FOR UPDATE</tt><br>
	 * <p>
	 * @param alForUpdateColumn the al_forUpdateColumn to set
	 */
	public void setForUpdateColumns(ArrayList<String> al_forUpdateColumn) {
		this.al_forUpdateColumn = al_forUpdateColumn;
	}


	/**
	 * Restituisce i <tt>common-table-expression</tt> codificati a inizio istruzione
	 * dopo la parola chiave <tt>WITH</tt><br>
	 * <p>
	 * @return the al_commonTableExpression
	 */
	public ArrayList<SqlCommonTableExpression> getCommonTableExpressions() {
		return al_commonTableExpression;
	}


	/**
	 * Imposta i <tt>common-table-expression</tt> codificati a inizio istruzione
	 * dopo la parola chiave <tt>WITH</tt><br>
	 * <p>
	 * @param al_commonTableExpression the al_commonTableExpression to set
	 */
	public void setCommonTableExpressions(ArrayList<SqlCommonTableExpression> al_commonTableExpression) {
		this.al_commonTableExpression = al_commonTableExpression;
	}


	/**
	 * Restituisce la <tt>full-select</tt> che rappresenta il corpo dello statement.<br>
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta la <tt>full-select</tt> che rappresenta il corpo dello statement.<br>
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.fullSelect = fullSelect;
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
	 * Restituisce il numero di righe impostate da <tt>OPTIMIZE FOR n ROW|ROWS</tt></tt>.<br>
	 * <p>
	 * @return the optimizeForRowsNumber
	 */
	public int getOptimizeForRowsNumber() {
		return optimizeForRowsNumber;
	}


	/**
	 * Imposta il numero di righe impostate da <tt>OPTIMIZE FOR n ROW|ROWS</tt></tt>.<br>
	 * <p>
	 * @param optimizeForRowsNumber the optimizeForRowsNumber to set
	 */
	public void setOptimizeForRowsNumber(int optimizeForRowsNumber) {
		this.optimizeForRowsNumber = optimizeForRowsNumber;
	}


	/**
	 * Restituisce se presente a inizio istruzione la definizione di common-table-expression,
	 * che iniziano con la parola chiave WITH.</tt></tt>.<br>
	 * <p>
	 * @return the isCommonTableExpression
	 */
	public boolean isCommonTableExpression() {
		return isCommonTableExpression;
	}


	/**
	 * Imposta se presente a inizio istruzione la definizione di common-table-expression,
	 * che iniziano con la parola chiave WITH.</tt></tt>.<br>
	 * <p>
	 * @param isCommonTableExpression the isCommonTableExpression to set
	 */
	public void setCommonTableExpression(boolean isCommonTableExpression) {
		this.isCommonTableExpression = isCommonTableExpression;
	}


	/**
	 * Restituisce se presente <tt>FOR UPDATE OF col1, .. coln</tt>
	 * <p>
	 * @return the isForUpdate
	 */
	public boolean isForUpdate() {
		return isForUpdate;
	}


	/**
	 * Imposta se presente <tt>FOR UPDATE OF col1, .. coln</tt>
	 * <p>
	 * @param isForUpdate the isForUpdate to set
	 */
	public void setForUpdate(boolean isForUpdate) {
		this.isForUpdate = isForUpdate;
	}


	/**
	 * Restituisce se presente <tt>FOR READ ONLY</tt>
	 * <p>
	 * @return the isForReadOnly
	 */
	public boolean isForReadOnly() {
		return isForReadOnly;
	}


	/**
	 * Imposta se presente <tt>FOR READ ONLY</tt>
	 * <p>
	 * @param isForReadOnly the isForReadOnly to set
	 */
	public void setForReadOnly(boolean isForReadOnly) {
		this.isForReadOnly = isForReadOnly;
	}


	/**
	 * Restituisce se presente <tt>OPTIMIZE FOR n ROW|ROWS</tt>
	 * <p>
	 * @return the isOptimizeForRows
	 */
	public boolean isOptimizeForRows() {
		return isOptimizeForRows;
	}


	/**
	 * Imposta se presente <tt>OPTIMIZE FOR n ROW|ROWS</tt>
	 * <p>
	 * @param isOptimizeForRows the isOptimizeForRows to set
	 */
	public void setOptimizeForRows(boolean isOptimizeForRows) {
		this.isOptimizeForRows = isOptimizeForRows;
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
	 * Restituisce se presente definizione di isolation level a <tt>RR, RS, CS o UR</tt>
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
	 * @param isIsolationUseAndKeepShare the isIsolationUseAndKeepShare to set
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
	public void setIsolationUseAndKeepExclusiveLocks(boolean isIsolationUseAndKeepExclusiveLocks) {
		this.isIsolationUseAndKeepExclusiveLocks = isIsolationUseAndKeepExclusiveLocks;
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
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;
		ArrayList<ArrayList<String>> al_al_entity = null;

		al_al_entity = new ArrayList<ArrayList<String>> ();
		
		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		

		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_al_entity.addAll(sqlSubselect.getEntities());
		}
		
		return al_al_entity;
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
	 * Restituisce un'array list di stringhe a due dimensioni.<br>
	 * <p>
	 * Ogni elemento, una ArrayList, contiene a sua volta <b>3n+3</b> elementi ed è relativo a una tabella o view,
	 * dove <b>n</b> è il numero di colonne dichiarate nella clausola <tt>SELECT</tt><br>
	 * <p>
	 * <ul>
	 * <li>Il primo elemento è il nome della tabella dichiarata nella clausola <tt>FROM</tt><br></li>
	 * <li>Il secondo elemento è il nome dell'owner di qualificazione<br></li>
	 * <li>Il terzo elemento è il correlation-name e, se presente, la colonna è qualificata con questo<br></li>
	 * <li>I successivi elementi sono <b>n</b> sequenze di triplette con il seguente significato:
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
		ArrayList<SqlSubselectSelectInto> al_subselect = null;

		al_al_columnsByEntity = new ArrayList<ArrayList<String>> ();
		
		// Tutte le subselect presenti
		// Nella fullselect principale e in With Common table expressions
		al_subselect = this.getAllSubselect();
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_al_columnsByEntity.addAll(sqlSubselect.getEntitiesColumns());
		}
		
		return al_al_columnsByEntity;
	}


	
	
	
	
	/**
	 * Restituisce tutte le subselect presenti nello statement<br>
	 * attraverso la full-select codificata e le eventuali common-table-expression<br>
	 * @return ArrayList<SqlSubselectSelectInto> 
	 */
	public ArrayList<SqlSubselectSelectInto> getAllSubselect() {
		
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		// subselect da fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();

		// subselect da With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		return al_subselect;
	}

	
	
	
	/**
	 * Restituisce se lo statement è del tipo:<br>
	 * <p>
	 *  <tt>SELECT * FROM table1, table2 ... WHERE ...</tt><br>
	 *  <tt>SELECT table1.*, table2.*, ... FROM table1, table2 ... WHERE ...</tt><br>
	 *  <tt>SELECT view.* FROM table1, table2 ... WHERE ...</tt><br>
	 *  <tt>SELECT corr-name.* FROM table1, table2 ... WHERE ...</tt><br>
 	 * <p>
     * L'utilizzo implicoito di tutte le colonne è sconsigliato.<br>
     * <p>
     * 
     * 
	 * @return boolean isSelectStar
	 */
	public boolean isSelectStar() {
		
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		
	    // Scan subselect presenti, a tutti i livelli
	    al_subselect = this.getAllSubselect();
	    for (SqlSubselectSelectInto subselect : al_subselect) {
	    	// Select * FROM
			if (subselect.isSelectColsAsStar()) {
				return true;
			}
	    	// Select table|view.* FROM
			if (subselect.isSelectColsAsTableViewStar()) {
				return true;
			}
	    	// Select corrName.* FROM table AS corrName
			if (subselect.isSelectColsAsCorrNameStar()) {
				return true;
			}
		}
		return false;
	}



	/**
	 * Restituisce le variabili host in  <tt>output</tt>, che iniziano con <tt>:</tt> dichiarate nella <br>
	 * <p>
	 * <tt>SELECT ... INTO hostVar1, hostVar2, ..., hostVarn</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return the al_hostVarInto
	 */
	public ArrayList<String> getHostVarsInto() {
		
		ArrayList<String> al_hostVarInto = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		al_hostVarInto = new ArrayList<String> ();

		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		
		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_hostVarInto.addAll(sqlSubselect.getHostVarsInto());
		}
		
		return al_hostVarInto;
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
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		
		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_al_hostVar.addAll(sqlSubselect.getHostVarsIntoByEntity());
		}
		
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
	public ArrayList<SqlTableHostVarDeclared> getHostVarsIntoByEntityFormatted() {
		
		SqlTableHostVarDeclared sqlTableHostVarDeclared = null;
		ArrayList<ArrayList<String>> al_al_hostVar = null;
		ArrayList<SqlTableHostVarDeclared> al_hostVarDeclared = null;

		String tableName = "";
		String columnName = "";
		String hostVarName = "";
				
		al_al_hostVar = getHostVarsIntoByEntity();
		
		// Formattazone per output
		al_hostVarDeclared = new ArrayList<SqlTableHostVarDeclared>();
		for (ArrayList<String> al : al_al_hostVar) {
			
			tableName = al.get(0);
			hostVarName = al.get(1).substring(1);
			columnName = al.get(2);
			
			sqlTableHostVarDeclared = new SqlTableHostVarDeclared();
			sqlTableHostVarDeclared.setTableName(tableName);
			sqlTableHostVarDeclared.setColumnName(columnName);
			sqlTableHostVarDeclared.setHostVarName(hostVarName);
			
			al_hostVarDeclared.add(sqlTableHostVarDeclared);
		}
		
		return al_hostVarDeclared;
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
	public ArrayList<SqlTableHostVarDeclared> getHostVarsIntoByEntityFormatted(String tableName) {
		
		ArrayList<SqlTableHostVarDeclared> al_hostVarDeclaredTable = null;
		ArrayList<SqlTableHostVarDeclared> al_hostVarDeclaredTableAll = null;

		// Tutte le tabelle dichiarate nello statement 
		al_hostVarDeclaredTableAll = getHostVarsIntoByEntityFormatted();
		al_hostVarDeclaredTable = new ArrayList<SqlTableHostVarDeclared>();
		
		// Scan tabelle
		for (SqlTableHostVarDeclared sqlTablesHostDeclared : al_hostVarDeclaredTableAll) {		
			if (!sqlTablesHostDeclared.getTableName().equals(tableName)) {continue;}
			al_hostVarDeclaredTable.add(sqlTablesHostDeclared);
		}
		
		return al_hostVarDeclaredTable;
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
		
		ArrayList<String> al_hostVarAll = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		al_hostVarAll = new ArrayList<String> ();
		
		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		
		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_hostVarAll.addAll(sqlSubselect.getHostVarsIntoByEntity(tableName));
		}
		
		return al_hostVarAll;
	}


	/**
	 * Restituisce le variabili host in <tt>input</tt>, che iniziano con <tt>:</tt> <br>
	 * dichiarate nella <tt>SELECT ... WHERE col1 = :hostVar1 AND col2 = hostVar2 ...</tt><br>
	 * <p>
	 * Si tratta di tutte le variabili host in modo indifferenziato rispetto alle tabelle<br>
	 * alle quali afferiscono, che vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return the al_hostVarWhere
	 */
	public ArrayList<String> getHostVarsWhere() {
		
		ArrayList<String> al_hostVarWhere = null;
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		al_hostVarWhere = new ArrayList<String> ();
		
		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		
		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_hostVarWhere.addAll(sqlSubselect.getHostVarsWhere());
		}
		
		return al_hostVarWhere;
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
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		ArrayList<SqlSubselectSelectInto> al_subselectWithCommon = null;

		al_al_hostVar = new ArrayList<ArrayList<String>>();
		
		// fullselect principale
		al_subselect = this.getFullSelect().getAllSubselect();
		
		// With Common table expressions
		for (SqlCommonTableExpression commonTableExpression : this.getCommonTableExpressions()) {
			al_subselectWithCommon = commonTableExpression.getFullSelect().getAllSubselect();
			al_subselect.addAll(al_subselectWithCommon);
		}
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			al_al_hostVar.addAll(sqlSubselect.getHostVarsWhereByEntity());
		}
		
		return al_al_hostVar;
	}


	/**
	 * Restituisce tutte le variabili host presenti in tutte le subselect che compongono la fullselect<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
	 * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		
		ArrayList<String> al_hostVarAll = null;
		String hostVarNormalized = "";

		al_hostVarAll = this.getFullSelect().getHostVars();
		
        // Scan variabili host
		for (int i = 0; i < al_hostVarAll.size(); i++) {
			if (al_hostVarAll.get(i).startsWith(":")) {
				hostVarNormalized = al_hostVarAll.get(i).substring(1);
				al_hostVarAll.set(i, hostVarNormalized);
			}
		}
		
		return al_hostVarAll;
	}


	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti in tutte le subselect che compongono la fullselect<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		ArrayList<String> al_constantAlpha = null;
		al_constantAlpha = this.getFullSelect().getConstantsAlphanumeric();
		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti in tutte le subselect che compongono la fullselect<br>
	 * <p>
	 * Vengono restituite stringhe contenenti numeri validi.<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		ArrayList<String> al_constantNumeric = null;
		al_constantNumeric = this.getFullSelect().getConstantsNumeric();
		return al_constantNumeric;
	}

	/**
	 * Restituisce il numero massimo di tabelle contemporaneamente in join individuate nelle clausole <tt>FROM</tt><br>
	 * <p>
	 * Si prende in cosiderazione la select e tutte le subselect, comunque nidificate, che la compongono.<br>
	 * Di ogni subselect si considera la clausola <tt>FROM</tt><br>
	 * Di ogni clausola <tt>FROM</tt><br> si considerano i table-reference joined<br>
	 * Di ogni  table-reference joined si contano le tabelle in join.<br>
	 * <p>
	 * Si considera il valore più alto<br>
	 * <p>
	 * @return the tables o view
	 */
	public int getMaxJoinedTablesNumber() {
		int numMaxJoinedTablesNumber;
		numMaxJoinedTablesNumber = this.fullSelect.getMaxJoinedTablesNumber();
		return numMaxJoinedTablesNumber;
	}
	
	/**
	 * Restituisce il numero massimo di subselect nested<br>
	 * <br>
	 * Si parte dal primo livello di subselect presente nella full-select, ovvero<br>
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
		maxSubselectNestedNumber = this.getFullSelect().getMaxSubselectNestedNumber();
		return maxSubselectNestedNumber;
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
