
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import enums.EnumSqlTableReferenceType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFullSelect
 * </h1>
 * <p>
 * Descrive una full-select, che rappresenta il corpo di ogni select-statement, descritta da {@link SqlSelectStatement}.<br> 
 * <p>
 * Una full-select può contenere, ricorsivamente, riferimento a ulteriori full-select oppure<br>
 * subselect descritta da {@link SqlSubselectSelectInto}, che modella uno<br>
 * statement sql foglia del tipo <tt>SELECT * FROM .. WHERE .. GROUP BY .. HAVING .. ORDER BY ..</tt><br>
 * Può inoltre essere presente la clausola di ordinamento <tt>ORDER BY ..</tt> e la clausola di controllo <br>
 * delle righe restituite <tt>FETCH FIRST ..</tt>
 * <p>
 * Gli oggetti utilizzati per descrivere la full-select sono:<br>
 * <p>
 * {@link SqlSubselectSelectInto}<br>
 * {@link SqlFullSelect}<br>  
 * {@link SqlFullSelectLinkedTo}<br>  
 * {@link SqlOrderBy}<br> 
 * <p>
 * Nella forma più elementare una full-select contiene una sola subselect della form <tt>SELECT * FROM table<\tt>
 * <p>
 * Nella forma più complessa una full-select contiene insiemi di coppie di subselect/full-select connesse
 * da informazioni di <tt>UNION EXCEP INTERSECT</tt>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlFullSelectLinkedTo 
 * @see SqlSubselectSelectInto
 * @see SqlOrderBy
*/

public class SqlFullSelect implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private SqlSubselectSelectInto subSelectInto = null;			// ... SELECT * ....INTO ..
	private SqlFullSelect fullSelect = null;						// (full-select) ...
	private ArrayList<SqlFullSelectLinkedTo> al_linkedTo = null;	// UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect|(fullSelect)
    private ArrayList<SqlOrderBy> al_orderByElement = null;         // ORDER BY element1, element2, ..
    private int fetchFirstRowsNumber = 0;                           // FETCH FIRST |1|n ROW|ROWS ONLY
    
    // Opzioni attive
    private boolean isSubselect = false;                      		// ... SELECT * ....
    private boolean isFullSelect = false;                      		// (full-select) ...
    private boolean isFullSelectWithLinkedTo = false;               // (full-select)|subselect  UNION|EXCEPT|INTERSECT |DISTINCT|ALL subselect|(fullSelect)
    private boolean isOrderBy = false;                      	    // ... ORDER BY ...
    private boolean isFetchFirstNRows = false;                      // FETCH FIRST |1|n ROW|ROWS ONLY
    
	/**
	 * Costruttore
	 */
	public SqlFullSelect() {
		super();
		al_linkedTo= new ArrayList<SqlFullSelectLinkedTo> ();
		al_orderByElement = new  ArrayList<SqlOrderBy> ();
	}

	/**
	 * Restituisce tutte le subselect codificate, anche ricorsivamente, nella full-select.<br>
	 * <br>
	 * Sono incluse anche tutte le subselect  connesse in UNION, INTERSECT e EXCEPT.
	 * <p>
	 * @return the subselect
	 */
	public ArrayList<SqlSubselectSelectInto> getAllSubselect() {
		ArrayList<SqlSubselectSelectInto> al_subselect = null;
		al_subselect = new ArrayList<SqlSubselectSelectInto> ();
		getAllSubselectRecursive(this, al_subselect);
		return al_subselect;
	}

	/*
	 * Estrazione ricorsiva dui tutte le subselect
	 * 
	 */
	private void getAllSubselectRecursive(SqlFullSelect sqlFullSelect, ArrayList<SqlSubselectSelectInto> al_subselect) {
		
		// Subselect da portare in output
		if (sqlFullSelect.isSubSelect()) {
			al_subselect.add(sqlFullSelect.getSubselectInto());
		} else {
			getAllSubselectRecursive(sqlFullSelect.getFullSelect(), al_subselect);
		}
		
		// Subselect connesse con UNION EXCEPT INTERSECT
		for (SqlFullSelectLinkedTo sqlFullSubselectLinked : sqlFullSelect.getLinkedSubFullSelects()) {
			// Subselect: la porto in output
			if (sqlFullSubselectLinked.isLinkedToSubselect()) {
				al_subselect.add(sqlFullSubselectLinked.getSubselect());
				continue;
			}
			// Fullselect: attivazione ricorsiva
			getAllSubselectRecursive(sqlFullSubselectLinked.getFullSelect(), al_subselect);
		}
	}

	/**
	 * Restituisce il descrittore della subselect codificata nella full-select.<br>
	 * La subselect è alternativa alla presenza (ricorsiva) di una full-select.<br>
	 * Per verificare se è codificata una subselect si utilizza il metodo <tt>isSubselect()</tt><br>
	 * <p>
	 * @return the subselect
	 */
	public SqlSubselectSelectInto getSubselectInto() {
		return subSelectInto;
	}

	/**
	 * Restituisce il descrittore di tutte le subselect codificata nella full-select.<br>
	 * <br>
	 * Si tratta della subselect alternativa alla presenza (ricorsiva) di una full-select<br>
	 * e di tutte le subselect codificate nelle full-select in UNION, EXCEPT o INTERSECT.<br>
	 * <br>
	 * Per verificare se è codificata una subselect si utilizza il metodo <tt>isSubselect()</tt><br>
	 * <p>
	 * @return the subselect
	 */
	public  ArrayList<SqlSubselectSelectInto> getSubselectIntoAll() {
		
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
		
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		getSubselectRecursive(this, al_sqlSubselect);  // Recupero ricorsivamente tutte le subselect presenti nelle fullselect
		
		return al_sqlSubselect;
	}

	/**
	 * Restituisce il numero massimo di subselect nested<br>
	 * <br>
	 * Si parte dal primo livello di subselect presente nella full-select, ovvero<br>
	 * della subselect di individiazione valori, e dalle subselect codificate nelle <br>
	 * full-select in UNION, EXCEPT o INTERSECT.<br>
	 * <p>
	 * Questo rappresenta il primo livello di select di recupero dati, senza nesting, anndidamento<br>
	 * <p>
	 * Per ogni subselect si analizzano ricorsivamente i predicati nella where, che possono fare
	 * esplicito riferimento a full-select o rimandare ad espressioni sql che possono contenere<br>
	 * full-select.<br>
	 * <p>
	 * Il processo continua ricorsivamente a partire da ogni subselect di primo livello individuata.<br>
	 * Viene resituito il livello massimo raggiunto.<br
	 * <p>
	 * @return the subselect
	 */
	public  int getMaxSubselectNestedNumber() {
		SqlSearchConditions searchConditions = null;
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
 		int maxNested = 0;
		int maxNestedWork = 0;
 		
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		
		// Scan subselect di primo livello
		getSubselectRecursive(this, al_sqlSubselect); 		 
		for (SqlSubselectSelectInto sqlSubselect : al_sqlSubselect) {
		   
			if (!sqlSubselect.isWhere()) {continue;}			// Interessano solo le subselect con where
			
			searchConditions = sqlSubselect.getWhere(); 
			maxNestedWork = searchConditions.getMaxSubselectNestedNumber();
			if (maxNestedWork > maxNested) {
				maxNested = maxNestedWork;
			}
		}
		return maxNested;
	}

	/**
	 * Imposta il descrittore della subselect codificata nella full-select.<br>
	 * La subselect è alternativa alla presenza (ricorsiva) di una full-select.<br>
	 * Per verificare se è codificata una subselect si utilizza il metodo <tt>isSubselect()</tt><br>
	 * <p>
	 * @param subselect the subselect to set
	 */
	public void setSubselectInto(SqlSubselectSelectInto subSelectInto) {
		this.subSelectInto = subSelectInto;
	}

	/**
	 * Restituisce i descrittori delle strutture di subselect o full-select collegate<br>
	 * <p>
	 * Ogni struttura associata rappresenta una connessione a una subselect o full-select.<br>
	 * Il tipo di associazione può essere di <tt>UNION</tt> di <tt>EXCEPT</tt><br> o di <tt>INTERSECT</tt><br>
	 * L'associazione può essere qualificata ulteriormente da <tt>ALL</tt> o <tt>DISTINCT</tt>.<br>
	 * <p>
	 * Nella terminologia DB2 una full-select fra parentessi viene chiamata subquery.<br>
	 * <p>
	 * @return the al_linkedTo subselect o full-select
	 */
	public ArrayList<SqlFullSelectLinkedTo> getLinkedSubFullSelects() {
		return al_linkedTo;
	}

	/**
	 * Imposta i descrittori delle strutture di subselect o full-select collegate<br>
	 * <p> 
	 * Ogni struttura associata rappresenta una connessione a una subselect o full-select.<br>
	 * Il tipo di associazione può essere di <tt>UNION</tt> di <tt>EXCEPT</tt><br> o di <tt>INTERSECT</tt><br>
	 * L'associazione può essere qualificata ulteriormente da <tt>ALL</tt> o <tt>DISTINCT</tt>.<br>
	 * <p>
	 * Nella terminologia DB2 una full-select fra parentessi viene chiamata subquery.<br>
	 * <p>
	 * @param al_linkedTo the al_linkedTo to set
	 */
	public void setLinkedSubFullSelects(ArrayList<SqlFullSelectLinkedTo> al_linkedTo) {
		this.al_linkedTo = al_linkedTo;
	}

	/**
	 * Restituisce il descrittore, codificato ricorsivamente, della full-select.<br>
	 * <p>
	 * Per sapere se la full-select corrente è descritta da una full-select ricorsiva,
	 * si usa il metodo isFullSelect().<br>
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return fullSelect;
	}

	/**
	 * Imposta il descrittore, codificato ricorsivamente, della full-select.<br>
	 * <p>
	 * Per sapere se la full-select corrente è descritta da una full-select ricorsiva,
	 * si usa il metodo isFullSelect().<br>
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.fullSelect = fullSelect;
	}

	
	
	/**
	 * Restituisce se la fullselect o la subselect è in UNION|EXCEPT|INTERCEPT<br>
	 * con altre fullselect o subselect.<br>
	 * <p>
	 * @return the isFullSelectWithLinkedTo
	 */
	public boolean isFullSelectWithLinkedTo() {
		return isFullSelectWithLinkedTo;
	}

	/**
	 * Imposta se la fullselect o la subselect è in UNION|EXCEPT|INTERCEPT<br>
	 * con altre fullselect o subselect.<br>
	 * <p>
	 * @param isFullSelectWithLinkedTo the isFullSelectWithLinkedTo to set
	 */
	public void setFullSelectWithLinkedTo(boolean isFullSelectWithLinkedTo) {
		this.isFullSelectWithLinkedTo = isFullSelectWithLinkedTo;
	}

	/**
	 * Restituisce gli elementi order by.<br>
	 * <p>
	 * Per sapere se la full-select corrente contiene la clausola order by si
	 * utilizza il metodo isOrderBy().<br>
	 * <p>
	 * @return the orderBy
	 */
	public ArrayList<SqlOrderBy> getOrderByElements() {
		return al_orderByElement;
	}

	/**
	 * Imposta gli elementi order by.<br>
	 * <p>
	 * Per sapere se la full-select corrente contiene la clausola order by si
	 * utilizza il metodo isOrderBy().<br>
	 * <p>
	 * @param orderByElements the orderByElements to set
	 */
	public void setOrderBy(ArrayList<SqlOrderBy> al_orderByElement) {
		this.al_orderByElement = al_orderByElement;
	}

	/**
	 * Restituisce il numero di righe impostato da <tt>FETCH FIRST n|1 ROWS|ROW<br>
	 * <p>
	 * @return the fetchFirstRowsNumber
	 */
	public int getFetchFirstRowsNumber() {
		return fetchFirstRowsNumber;
	}

	/**
	 * Imposta il numero di righe impostato da <tt>FETCH FIRST n|1 ROWS|ROW<br>
	 * <p>
	 * @param fetchFirstRowsNumber the fetchFirstRowsNumber to set
	 */
	public void setFetchFirstRowsNumber(int fetchFirstRowsNumber) {
		this.fetchFirstRowsNumber = fetchFirstRowsNumber;
	}

	/**
	 * Restituisce se nella full-select corrente è codificata e una subselect.<br>
	 * <p>
	 * @return the isSubselect
	 */
	public boolean isSubSelect() {
		return isSubselect;
	}

	/**
	 * Imposta se nella full-select corrente è codificata e una subselect.<br>
	 * <p>
	 * @param isSubselect the isSubselect to set
	 */
	public void setSubSelect(boolean isSubselect) {
		this.isSubselect = isSubselect;
	}

	/**
	 * Restituisce se nella full-select corrente è codificata una full-select.<br>
	 * <p>
	 * Si tratta della codifica ricorsiva diquesta stessa classe.<br>
	 * <p>
	 * @return the isFullSelect
	 */
	public boolean isFullSelect() {
		return isFullSelect;
	}

	/**
     * Imposta se nella full-select corrente è codificata una full-select.<br>
	 * <p>
	 * Si tratta della codifica ricorsiva diquesta stessa classe.<br>
	 * <p>
	 * @param isFullSelect the isFullSelect to set
	 */
	public void setFullSelect(boolean isFullSelect) {
		this.isFullSelect = isFullSelect;
	}

	/**
     * Restituisce se nella full-select corrente è presente <tt>FETCH FIRST n ROWS ONLY</tt><br>
	 * <p>
	 * @return the isFetchFirstNRows
	 */
	public boolean isFetchFirstNRows() {
		return isFetchFirstNRows;
	}

	/**
     * Imposta se nella full-select corrente è presente <tt>FETCH FIRST n ROWS ONLY</tt><br>
	 * <p>
	 * @param isFetchFirstNRows the isFetchFirstNRows to set
	 */
	public void setFetchFirstNRows(boolean isFetchFirstNRows) {
		this.isFetchFirstNRows = isFetchFirstNRows;
	}

	/**
     * Restituisce se nella full-select corrente è presente la clausola <tt>ORDER BY</tt><br>
	 * <p>
	 * @return the isOrderBy
	 */
	public boolean isOrderBy() {
		return isOrderBy;
	}

	/**
     * Restituisce se nella subselect codificata nella full-select corrente è presente la clausola <tt>ORDER BY</tt><br>
	 * <p>
	 * @return the isOrderBy
	 */
	public boolean isOrderBySubselect() {
		
		if (this.subSelectInto == null) {
			return isOrderBy;
		}
		
		return subSelectInto.isOrderBy();
	}

	/**
     * Imposta se nella full-select corrente è presentela clausola <tt>ORDER BY</tt><br>
	 * <p>
	 * @param isOrderBy the isOrderBy to set
	 */
	public void setOrderBy(boolean isOrderBy) {
		this.isOrderBy = isOrderBy;
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
		ArrayList<String> al_hostVarSingleSubselect = null;
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
		
		al_hostVarAll = new ArrayList<String> ();
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		
		// Recupero ricorsivamente tutte le subselect presenti nella fullselect
		getSubselectRecursive(this, al_sqlSubselect);
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselecte : al_sqlSubselect) {
			al_hostVarSingleSubselect = sqlSubselecte.getHostVars();
			al_hostVarAll.addAll(al_hostVarSingleSubselect);
		}
		
		return al_hostVarAll;
	}

	/*
	 * Accoda ricorsivamente le subselect della fullselect.
	 * 
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

	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti in tutte le subselect che compongono la fullselect<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
		ArrayList<String> al_constantAlpha = null;
		ArrayList<String> al_constantAlphaSingleSubselect = null;
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
		
		al_constantAlpha = new ArrayList<String> ();
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		
		// Recupero ricorsivamente tutte le subselect presenti nella fullselect
		getSubselectRecursive(this, al_sqlSubselect);
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_sqlSubselect) {
			al_constantAlphaSingleSubselect = sqlSubselect.getConstantsAlphanumeric();
			al_constantAlpha.addAll(al_constantAlphaSingleSubselect);
		}

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
		ArrayList<String> al_constantNumericSingleSubselect = null;
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;
		
		al_constantNumeric = new ArrayList<String> ();
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		
		// Recupero ricorsivamente tutte le subselect presenti nella fullselect
		getSubselectRecursive(this, al_sqlSubselect);
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselecte : al_sqlSubselect) {
			al_constantNumericSingleSubselect = sqlSubselecte.getConstantsNumeric();
			al_constantNumeric.addAll(al_constantNumericSingleSubselect);
		}
		
		return al_constantNumeric;
	}


	/**
	 * Restituisce informazioni sulle tabelle o sulle view utilizzate dall'istruzione dichiarate nelle clausole <tt>FROM</tt><br>
	 * Si prendono i  considerazione tutte le subselect, comunque nidificate, che compongono la fullselect.<br>
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
		
		ArrayList<ArrayList<String>> al_al_entitySingleSubselect = null;
		ArrayList<ArrayList<String>> al_al_entity = null;
        ArrayList<SqlSubselectSelectInto> al_sqlSubselect = null;

        al_al_entity = new ArrayList<ArrayList<String>>();
		al_sqlSubselect = new ArrayList<SqlSubselectSelectInto> ();
		
		// Recupero ricorsivamente tutte le subselect presenti nella fullselect
		getSubselectRecursive(this, al_sqlSubselect);
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_sqlSubselect) {
			al_al_entitySingleSubselect = sqlSubselect.getEntities();
			al_al_entity.addAll(al_al_entitySingleSubselect);
		}
		
		return al_al_entity;
	}

	/**
	 * Restituisce il numero massimo di tabelle contemporaneamente in join individuate nelle clausole <tt>FROM</tt><br>
	 * <p>
	 * Si prendono i  considerazione tutte le subselect, comunque nidificate, che compongono la fullselect.<br>
	 * Di ogni subselect si considera la clausola <tt>FROM</tt><br>
	 * Di ogni clausola <tt>FROM</tt><br> si considerano i table-reference joined<br>
	 * Di ogni  table-reference joined si contano le tabelle in join.<br>
	 * <p>
	 * Si considera il valore più alto<br>
	 * <p>
	 * @return the tables o view
	 */
	public int getMaxJoinedTablesNumber() {
		
		SqlFromTableReferenceJoinedTable joinedTable = null;
        ArrayList<SqlSubselectSelectInto> al_subselect = null;
        ArrayList<SqlFromTableReference> al_fromTableReference =  null;
        int joinedTablesNumber = 0;
        int joinedTablesNumberMax = 0;
        
		// Recupero ricorsivamente tutte le subselect presenti nella fullselect
		al_subselect = this.getAllSubselect();
		
		// Scan subselect individuate
		for (SqlSubselectSelectInto sqlSubselect : al_subselect) {
			
			al_fromTableReference = sqlSubselect.getFromTableReferences();

			// Scan table-references
			for (SqlFromTableReference sqlFromTableReference : al_fromTableReference) {
				
				// Interessano solo i table-reference joined
				if (sqlFromTableReference.getTypeTableReference() != EnumSqlTableReferenceType.JOINED_TABLE) {continue;}
				
				// Struttura con tutte le tabelle in join, ricorsiva
				joinedTable = sqlFromTableReference.getJoinedTable();
				joinedTablesNumber = joinedTable.getJoinedTables().size();
				if (joinedTablesNumber > joinedTablesNumberMax) {
					joinedTablesNumberMax = joinedTablesNumber;
				}
			}
		}
		
		return joinedTablesNumberMax;
	}


	
}
