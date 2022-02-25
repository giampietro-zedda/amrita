package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFetchStatement
 * </h1>
 * <p>
 * Descrive una FETCH statement che recupera una o più righe di una tabella, attraverso un cursore.<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 04/Ago/2011 
*/

public class SqlFetchStatement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Valori
    private SqlDeclareCursorStatement sqlDeclareCursor = null;     // Descrittore esterno cursore con tabelle interessate
	private String cursorName = "";                                // FETCH ... |FROM cursor-name
	private String intoDescriptorName = null; 	                   // FETCH ... INTO DESCRIPTOR descriptor-name
	private ArrayList<String> al_intoHostVar = null; 	           // FETCH ... INTO host-var1, host.var2, ..., host-varn
	private String afterAbsoluteHostVar = "";                      // FETCH SENSITIVE WITH CONTINUE AFTER ABSOLUTE hostVar ..
    private int afterAbsoluteNumber = 0;                           // FETCH SENSITIVE WITH CONTINUE AFTER ABSOLUTE integer-constant ..
    private String afterRelativeHostVar = "";                      // FETCH SENSITIVE WITH CONTINUE AFTER RELATIVE hostVar ..
    private int afterRelativeNumber = 0;                           // FETCH SENSITIVE WITH CONTINUE AFTER RELATIVE integer-constant ..
    private String afterRowSetHostVar = "";                        // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT ABSOLUTE host-var    
    private int afterRowSetNumber = 0;                             // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT RELATIVE integer-constant ..
	private String forRowsHostVar = "";                            // FETCH ... FOR hostVar ROWS
    private int forRowsNumber = 0;                                 // FETCH ... FOR integer-constant ROWS
 	

    // Opzioni presenti
    private boolean isInsensitive = false;                   	   // FETCH INSENSITIVE ...
    private boolean isSensitive = false;                   	       // FETCH SENSITIVE ...
    private boolean isWithContinue = false;                   	   // FETCH SENSITIVE WITH CONTINUE
    private boolean isBefore = false;                   	       // FETCH SENSITIVE WITH CONTINUE BEFORE ...
    private boolean isAfter = false;                   	           // FETCH SENSITIVE WITH CONTINUE AFTER ...
    private boolean isAfterNext = false;                   	       // FETCH SENSITIVE WITH CONTINUE AFTER NEXT ...
    private boolean isAfterPrior = false;                   	   // FETCH SENSITIVE WITH CONTINUE AFTER PRIOR ...
    private boolean isAfterFirst = false;                   	   // FETCH SENSITIVE WITH CONTINUE AFTER FIRST ...
    private boolean isAfterLast = false;                   	       // FETCH SENSITIVE WITH CONTINUE AFTER LAST ...
    private boolean isAfterCurrent = false;                   	   // FETCH SENSITIVE WITH CONTINUE AFTER CURRENT ...
    private boolean isAfterCurrentContinue = false;                // FETCH SENSITIVE WITH CONTINUE AFTER CURRENT CONTINUE ..
    private boolean isAfterAbsolute = false;                       // FETCH SENSITIVE WITH CONTINUE AFTER ABSOLUTE ...
    private boolean isAfterRelative = false;                       // FETCH SENSITIVE WITH CONTINUE AFTER RELATIVE ...
    private boolean isAfterHostVar = false;                        // FETCH SENSITIVE WITH CONTINUE AFTER ABSOLUTE|RELATIVE hostVar ..
    private boolean isAfterNumber = false;                         // FETCH SENSITIVE WITH CONTINUE AFTER ABSOLUTE|RELATIVE integer-constant ..
    private boolean isAfterNextRowSet = false;                     // FETCH SENSITIVE WITH CONTINUE AFTER NEXT ROWSET ..
    private boolean isAfterPriorRowSet = false;                    // FETCH SENSITIVE WITH CONTINUE AFTER PRIOR ROWSET ..
    private boolean isAfterFirstRowSet = false;                    // FETCH SENSITIVE WITH CONTINUE AFTER FIRST ROWSET ..
    private boolean isAfterLastRowSet = false;                     // FETCH SENSITIVE WITH CONTINUE AFTER LAST ROWSET ..
    private boolean isAfterCurrentRowSet = false;                  // FETCH SENSITIVE WITH CONTINUE AFTER CURRENT ROWSET ..
    private boolean isAfterRowSetAtAbsolute = false;               // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT ABSOLUTE host-var    
    private boolean isAfterRowSetAtRelative = false;               // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT RELATIVE integer-constant ..
    private boolean isAfterRowSetAtHostVar = false;                // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT ABSOLUTE host-var    
    private boolean isAfterRowSetAtNumber = false;                 // FETCH SENSITIVE WITH CONTINUE AFTER ROWSET SATARTING AT RELATIVE integer-constant ..
    private boolean isSingleRowFetch = false;         			   // FETCH ... INTO host-vars | INTO DESCRIPTOR descriptor.name
    private boolean isMultipleRowFetch = false;         		   // FETCH ... FOR ... ROWS INTO host-vars | INTO DESCRIPTOR descriptor.name
    private boolean isForRows = false;         		               // FETCH ... FOR ... ROWS  
    private boolean isForRowsHostVar = false;         		       // FETCH ... FOR host-var ROWS  
    private boolean isForRowsNumber = false;         		       // FETCH ... FOR integer-constant ROWS  
    private boolean isIntoHostVarsArray = false;         		   // FETCH ... INTO host-vars-array 
    private boolean isIntoDescriptor = false;         		       // FETCH ... INTO DESCRIPTOR descriptorName
    
	/**
	 * Costruttore
	 */
	public SqlFetchStatement() {
		super();
		al_intoHostVar = new ArrayList<String> (); 	          
	}

	
	
	
	/**
	 * Restituisce l'istruzione {@link SqlDeclareCursorStatement} associata al cursore
	 * utilizzato.
	 * 
	 * @return the sqlDeclareCursor
	 */
	public SqlDeclareCursorStatement getSqlDeclareCursor() {
		return sqlDeclareCursor;
	}




	/**
	 * Imposta l'istruzione {@link SqlDeclareCursorStatement} associata al cursore
	 * utilizzato.
	 * 
	 * @param sqlDeclareCursor the sqlDeclareCursor to set
	 */
	public void setSqlDeclareCursor(SqlDeclareCursorStatement sqlDeclareCursor) {
		this.sqlDeclareCursor = sqlDeclareCursor;
	}




	/**
	 * Restituisce il nome del cursore di fetch.<br>
	 * <p>
	 * @return the cursorName
	 */
	public String getCursorName() {
		return cursorName;
	}

	/**
	 * Imposta il nome del cursore di fetch.<br>
	 * <p>
	 * @param cursorName the cursorName to set
	 */
	public void setCursorName(String cursorName) {
		this.cursorName = cursorName;
	}

	/**
	 * Restituisce il nome del descriptor.<br>
	 * <p>
	 * @return the intoDescriptorName
	 */
	public String getIntoDescriptorName() {
		return intoDescriptorName;
	}

	/**
	 * Imposta il nome del descriptor.<br>
	 * <p>
	 * @param intoDescriptorName the intoDescriptorName to set
	 */
	public void setIntoDescriptorName(String intoDescriptorName) {
		this.intoDescriptorName = intoDescriptorName;
	}

	/**
	 * Restituisce le variabili host specificate nella clausola INTO.<br>
	 * <p>
	 * @return the al_intoHostVar
	 */
	public ArrayList<String> getIntoHostVars() {
		return al_intoHostVar;
	}

	/**
	 * Imposta le variabili host specificate nella clausola INTO.<br>
	 * <p>
	 * @param al_intoHostVar the al_intoHostVar to set
	 */
	public void setIntoHostVars(ArrayList<String> al_intoHostVar) {
		this.al_intoHostVar = al_intoHostVar;
	}

	/**
	 * Restituisce la variabile host specificata nella clausola ROWSET STARTING AT ABSOLUTE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterAbsoluteHostVar
	 */
	public String getAfterAbsoluteHostVar() {
		return afterAbsoluteHostVar;
	}

	/**
	 * Imposta la variabile host specificata nella clausola ROWSET STARTING AT ABSOLUTE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterAbsoluteHostVar the afterAbsoluteHostVar to set
	 */
	public void setAfterAbsoluteHostVar(String afterAbsoluteHostVar) {
		this.afterAbsoluteHostVar = afterAbsoluteHostVar;
	}

	/**
	 * Restituisce il numero specificato nella clausola ROWSET STARTING AT ABSOLUTE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterAbsoluteNumber
	 */
	public int getAfterAbsoluteNumber() {
		return afterAbsoluteNumber;
	}

	/**
	 * Imposta il numero specificato nella clausola ROWSET STARTING AT ABSOLUTE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterAbsoluteNumber the afterAbsoluteNumber to set
	 */
	public void setAfterAbsoluteNumber(int afterAbsoluteNumber) {
		this.afterAbsoluteNumber = afterAbsoluteNumber;
	}

	/**
	 * Restituisce la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterRelativeHostVar
	 */
	public String getAfterRelativeHostVar() {
		return afterRelativeHostVar;
	}

	/**
	 * Imposta la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterRelativeHostVar the afterRelativeHostVar to set
	 */
	public void setAfterRelativeHostVar(String afterRelativeHostVar) {
		this.afterRelativeHostVar = afterRelativeHostVar;
	}

	/**
	 * Restituisce la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterRelativeNumber
	 */
	public int getAfterRelativeNumber() {
		return afterRelativeNumber;
	}

	/**
	 * Imposta la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterRelativeNumber the afterRelativeNumber to set
	 */
	public void setAfterRelativeNumber(int afterRelativeNumber) {
		this.afterRelativeNumber = afterRelativeNumber;
	}

	/**
	 * Restituisce la variabile host specificata nella clausola ROWSET STARTING AT ABSOLUTE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterRowSetHostVar
	 */
	public String getAfterRowSetHostVar() {
		return afterRowSetHostVar;
	}

	/**
	 * Imposta la variabile host specificata nella clausola ROWSET STARTING AT ABSOLUTE host-variable
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterRowSetHostVar the afterRowSetHostVar to set
	 */
	public void setAfterRowSetHostVar(String afterRowSetHostVar) {
		this.afterRowSetHostVar = afterRowSetHostVar;
	}

	/**
	 * Restituisce la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @return the afterRowSetNumber
	 */
	public int getAfterRowSetRelativeNumber() {
		return afterRowSetNumber;
	}

	/**
	 * Imposta la variabile host specificata nella clausola ROWSET STARTING AT RELATIVE integer-constant
	 * per multiple-row-fetch.<br>
	 * <p>
	 * @param afterRowSetNumber the afterRowSetNumber to set
	 */
	public void setAfterRowSetNumber(int afterRowSetNumber) {
		this.afterRowSetNumber = afterRowSetNumber;
	}

	/**
	 * Restituisce se presente la clausola INSENSITIVE<br>
	 * <p>
	 * @return the isInsensitive
	 */
	public boolean isInsensitive() {
		return isInsensitive;
	}

	/**
	 * Imposta se presente la clausola INSENSITIVE<br>
	 * <p>
	 * @param isInsensitive the isInsensitive to set
	 */
	public void setInsensitive(boolean isInsensitive) {
		this.isInsensitive = isInsensitive;
	}

	/**
	 * Restituisce se presente la clausola SENSITIVE<br>
	 * <p>
	 * @return the isSensitive
	 */
	public boolean isSensitive() {
		return isSensitive;
	}

	/**
	 * Imposta se presente la clausola SENSITIVE<br>
	 * <p>
	 * @param isSensitive the isSensitive to set
	 */
	public void setSensitive(boolean isSensitive) {
		this.isSensitive = isSensitive;
	}

	/**
	 * Restituisce se presente la clausola WITH CONTINUE<br>
	 * <p>
	 * @return the isWithContinue
	 */
	public boolean isWithContinue() {
		return isWithContinue;
	}

	/**
	 * Imposta se presente la clausola WITH CONTINUE<br>
	 * <p>
	 * @param isWithContinue the isWithContinue to set
	 */
	public void setWithContinue(boolean isWithContinue) {
		this.isWithContinue = isWithContinue;
	}

	/**
	 * Restituisce se presente la clausola BEFORE di fetch-orientation<br>
	 * <p>
	 * @return the isBefore
	 */
	public boolean isBefore() {
		return isBefore;
	}

	/**
	 * Imposta se presente la clausola BEFORE di fetch-orientation<br>
	 * <p>
	 * @param isBefore the isBefore to set
	 */
	public void setBefore(boolean isBefore) {
		this.isBefore = isBefore;
	}

	/**
	 * Restituisce se presente la clausola AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfter
	 */
	public boolean isAfter() {
		return isAfter;
	}

	/**
	 * Imposta se presente la clausola AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfter the isAfter to set
	 */
	public void setAfter(boolean isAfter) {
		this.isAfter = isAfter;
	}

	/**
	 * Restituisce se presente la clausola NEXT di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterNext
	 */
	public boolean isAfterNext() {
		return isAfterNext;
	}

	/**
	 * Imposta se presente la clausola NEXT di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterNext the isAfterNext to set
	 */
	public void setAfterNext(boolean isAfterNext) {
		this.isAfterNext = isAfterNext;
	}

	/**
	 * Restituisce se presente la clausola PRIOR di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterPrior
	 */
	public boolean isAfterPrior() {
		return isAfterPrior;
	}

	/**
	 * Imposta se presente la clausola PRIOR di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterPrior the isAfterPrior to set
	 */
	public void setAfterPrior(boolean isAfterPrior) {
		this.isAfterPrior = isAfterPrior;
	}

	/**
	 * Restituisce se presente la clausola FIRST di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterFirst
	 */
	public boolean isAfterFirst() {
		return isAfterFirst;
	}

	/**
	 * Imposta se presente la clausola FIRST di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterFirst the isAfterFirst to set
	 */
	public void setAfterFirst(boolean isAfterFirst) {
		this.isAfterFirst = isAfterFirst;
	}

	/**
	 * Restituisce se presente la clausola LAST di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterLast
	 */
	public boolean isAfterLast() {
		return isAfterLast;
	}

	/**
	 * Imposta se presente la clausola LAST di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterLast the isAfterLast to set
	 */
	public void setAfterLast(boolean isAfterLast) {
		this.isAfterLast = isAfterLast;
	}

	/**
	 * Restituisce se presente la clausola CURRENT di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterCurrent
	 */
	public boolean isAfterCurrent() {
		return isAfterCurrent;
	}

	/**
	 * Imposta se presente la clausola CURRENT di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterCurrent the isAfterCurrent to set
	 */
	public void setAfterCurrent(boolean isAfterCurrent) {
		this.isAfterCurrent = isAfterCurrent;
	}

	/**
	 * Restituisce se presente la clausola CONTINUE di CURRENT di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterCurrentContinue
	 */
	public boolean isAfterCurrentContinue() {
		return isAfterCurrentContinue;
	}

	/**
	 * Imposta se presente la clausola CONTINUE di CURRENT di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterCurrentContinue the isAfterCurrentContinue to set
	 */
	public void setAfterCurrentContinue(boolean isAfterCurrentContinue) {
		this.isAfterCurrentContinue = isAfterCurrentContinue;
	}


	
	
	
	/**
	 * Restituisce se presente la clausola ABSOLUTE di AFTER<br>
	 * <p>
	 * @return the isAfterAbsolute
	 */
	public boolean isAfterAbsolute() {
		return isAfterAbsolute;
	}

	/**
	 * Imposta se presente la clausola ABSOLUTE di AFTER<br>
	 * <p>
	 * @param isAfterAbsolute the isAfterAbsolute to set
	 */
	public void setAfterAbsolute(boolean isAfterAbsolute) {
		this.isAfterAbsolute = isAfterAbsolute;
	}

	/**
	 * Restituisce se presente la clausola RELATIVE di AFTER<br>
	 * <p>
	 * @return the isAfterRelative
	 */
	public boolean isAfterRelative() {
		return isAfterRelative;
	}

	/**
	 * Imposta se presente la clausola RELATIVE di AFTER<br>
	 * <p>
	 * @param isAfterRelative the isAfterRelative to set
	 */
	public void setAfterRelative(boolean isAfterRelative) {
		this.isAfterRelative = isAfterRelative;
	}

	/**
	 * Restituisce se presente la clausola RELATIVE|ABSOKUTE host-var di AFTER<br>
	 * <p>
	 * @return the isAfterHostVar
	 */
	public boolean isAfterHostVar() {
		return isAfterHostVar;
	}

	/**
	 * Imposta se presente la clausola RELATIVE|ABSOKUTE host-var di AFTER<br>
	 * <p>
	 * @param isAfterHostVar the isAfterHostVar to set
	 */
	public void setAfterHostVar(boolean isAfterHostVar) {
		this.isAfterHostVar = isAfterHostVar;
	}

	/**
	 * Restituisce se presente la clausola RELATIVE|ABSOKUTE integer-constant di AFTER<br>
	 * <p>
	 * @return the isAfterNumber
	 */
	public boolean isAfterNumber() {
		return isAfterNumber;
	}

	/**
	 * Imposta se presente la clausola RELATIVE|ABSOKUTE integer-constant di AFTER<br>
	 * <p>
	 * @param isAfterNumber the isAfterNumber to set
	 */
	public void setAfterNumber(boolean isAfterNumber) {
		this.isAfterNumber = isAfterNumber;
	}

	/**
	 * Restituisce se presente la clausola NEXT ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterNextRowSet
	 */
	public boolean isAfterNextRowSet() {
		return isAfterNextRowSet;
	}

	/**
	 * Imposta se presente la clausola NEXT ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterNextRowSet the isAfterNextRowSet to set
	 */
	public void setAfterNextRowSet(boolean isAfterNextRowSet) {
		this.isAfterNextRowSet = isAfterNextRowSet;
	}

	/**
	 * Restituisce se presente la clausola PRIOR ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterPriorRowSet
	 */
	public boolean isAfterPriorRowSet() {
		return isAfterPriorRowSet;
	}

	/**
	 * Imposta se presente la clausola PRIOR ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterPriorRowSet the isAfterPriorRowSet to set
	 */
	public void setAfterPriorRowSet(boolean isAfterPriorRowSet) {
		this.isAfterPriorRowSet = isAfterPriorRowSet;
	}

	/**
	 * Restituisce se presente la clausola FIRST ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterFirstRowSet
	 */
	public boolean isAfterFirstRowSet() {
		return isAfterFirstRowSet;
	}

	/**
	 * Imposta se presente la clausola FIRST ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterFirstRowSet the isAfterFirstRowSet to set
	 */
	public void setAfterFirstRowSet(boolean isAfterFirstRowSet) {
		this.isAfterFirstRowSet = isAfterFirstRowSet;
	}

	/**
	 * Restituisce se presente la clausola LAST ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterLastRowSet
	 */
	public boolean isAfterLastRowSet() {
		return isAfterLastRowSet;
	}

	/**
	 * Imposta se presente la clausola LAST ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterLastRowSet the isAfterLastRowSet to set
	 */
	public void setAfterLastRowSet(boolean isAfterLastRowSet) {
		this.isAfterLastRowSet = isAfterLastRowSet;
	}


	
	/**
	 * Restituisce se presente la clausola CURRENT ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @return the isAfterCurrentRowSet
	 */
	public boolean isAfterCurrentRowSet() {
		return isAfterCurrentRowSet;
	}

	/**
	 * Imposta se presente la clausola CURRENT ROWSET di AFTER di fetch-orientation<br>
	 * <p>
	 * @param isAfterCurrentRowSet the isAfterCurrentRowSet to set
	 */
	public void setAfterCurrentRowSet(boolean isAfterCurrentRowSet) {
		this.isAfterCurrentRowSet = isAfterCurrentRowSet;
	}


	
	
	/**
	 * Restituisce se presente la clausola LAST ROWSET STARTING AT ABSOLUTE ... di AFTER<br>
	 * <p>
	 * @return the isAfterRowSetAtAbsolute
	 */
	public boolean isAfterRowSetAtAbsolute() {
		return isAfterRowSetAtAbsolute;
	}

	/**
	 * Imposta se presente la clausola LAST ROWSET STARTING AT ABSOLUTE ... di AFTER<br>
	 * <p>
	 * @param isAfterRowSetAtAbsolute the isAfterRowSetAtAbsolute to set
	 */
	public void setAfterRowSetAtAbsolute(boolean isAfterRowSetAtAbsolute) {
		this.isAfterRowSetAtAbsolute = isAfterRowSetAtAbsolute;
	}

	/**
	 * Restituisce se presente la clausola LAST ROWSET STARTING AT RELATIVE ... di AFTER<br>
	 * <p>
	 * @return the isAfterRowSetAtRelative
	 */
	public boolean isAfterRowSetAtRelative() {
		return isAfterRowSetAtRelative;
	}

	/**
	 * Imposta se presente la clausola LAST ROWSET STARTING AT RELATIVE ... di AFTER<br>
	 * <p>
	 * @param isAfterRowSetAtRelative the isAfterRowSetAtRelative to set
	 */
	public void setAfterRowSetAtRelative(boolean isAfterRowSetAtRelative) {
		this.isAfterRowSetAtRelative = isAfterRowSetAtRelative;
	}

	/**
	 * Restituisce se presente la clausola LAST ROWSET STARTING AT ABSOLUTE|RELATIVE host-var di AFTER<br>
	 * <p>
	 * @return the isAfterRowSetAtHostVar
	 */
	public boolean isAfterRowSetAtHostVar() {
		return isAfterRowSetAtHostVar;
	}

	/**
	 * Imposta se presente la clausola LAST ROWSET STARTING AT ABSOLUTE|RELATIVE host-var di AFTER<br>
	 * <p>
	 * @param isAfterRowSetAtHostVar the isAfterRowSetAtHostVar to set
	 */
	public void setAfterRowSetAtHostVar(boolean isAfterRowSetAtHostVar) {
		this.isAfterRowSetAtHostVar = isAfterRowSetAtHostVar;
	}

	/**
	 * Restituisce se presente la clausola LAST ROWSET STARTING AT ABSOLUTE|RELATIVE integer-constant di AFTER<br>
	 * <p>
	 * @return the isAfterRowSetAtNumber
	 */
	public boolean isAfterRowSetAtNumber() {
		return isAfterRowSetAtNumber;
	}

	/**
	 * Imposta se presente la clausola LAST ROWSET STARTING AT ABSOLUTE|RELATIVE integer-constant di AFTER<br>
	 * <p>
	 * @param isAfterRowSetAtNumber the isAfterRowSetAtNumber to set
	 */
	public void setAfterRowSetAtNumber(boolean isAfterRowSetAtNumber) {
		this.isAfterRowSetAtNumber = isAfterRowSetAtNumber;
	}


	/**
	 * Restituisce se single-row-fetch<br>
	 * <p>
	 * @return the isAfterRowSetAtNumber
	 */
	public boolean isSingleRowFetch() {
		return isSingleRowFetch;
	}
	
	/**
	 * Imposta se single-row-fetch<br>
	 * <p>
	 * @param isSingleRowFetch the isSingleRowFetch to set
	 */
	public void setSingleRowFetch(boolean isSingleRowFetch) {
		this.isSingleRowFetch = isSingleRowFetch;
	}

	/**
	 * Restituisce se single-row-fetch<br>
	 * <p>
	 * @return the isMultipleRowFetch
	 */
	public boolean isMultipleRowFetch() {
		return isMultipleRowFetch;
	}

	/**
	 * Restituisce se multiple-row-fetch<br>
	 * <p>
	 * @param isMultipleRowFetch the isMultipleRowFetch to set
	 */
	public void setMultipleRowFetch(boolean isMultipleRowFetch) {
		this.isMultipleRowFetch = isMultipleRowFetch;
	}

	/**
	 * Imposta se multiple-row-fetch<br>
	 * <p>
	 * @return the isForRows
	 */
	public boolean isForRows() {
		return isForRows;
	}

	/**
	 * Restituisce se presente la clausola FOR ... ROWS in multiple-row-fetch<br>
	 * <p>
	 * @param isForRows the isForRows to set
	 */
	public void setForRows(boolean isForRows) {
		this.isForRows = isForRows;
	}

	/**
	 * Restituisce se presente la clausola FOR host-var ROWS in multiple-row-fetch<br>
	 * <p>
	 * @return the isForRowsHostVar
	 */
	public boolean isForRowsHostVar() {
		return isForRowsHostVar;
	}

	/**
	 * Imposta se presente la clausola FOR host-var ROWS in multiple-row-fetch<br>
	 * <p>
	 * @param isForRowsHostVar the isForRowsHostVar to set
	 */
	public void setForRowsHostVar(boolean isForRowsHostVar) {
		this.isForRowsHostVar = isForRowsHostVar;
	}

	/**
	 * Restituisce se presente la clausola FOR integer-constant ROWS in multiple-row-fetch<br>
	 * <p>
	 * @return the isForRowsNumber
	 */
	public boolean isForRowsNumber() {
		return isForRowsNumber;
	}

	/**
	 * Imposta se presente la clausola FOR integer-constant ROWS in multiple-row-fetch<br>
	 * <p>
	 * @param isForRowsNumber the isForRowsNumber to set
	 */
	public void setForRowsNumber(boolean isForRowsNumber) {
		this.isForRowsNumber = isForRowsNumber;
	}

	/**
	 * Restituisce la variabile host del clausola FOR host-var ROWS in multiple-row-fetch<br>
	 * <p>
	 * @return the forRowsHostVar
	 */
	public String getForRowsHostVar() {
		return forRowsHostVar;
	}

	/**
	 * Imposta la variabile host del clausola FOR host-var ROWS in multiple-row-fetch<br>
	 * <p>
	 * @param forRowsHostVar the forRowsHostVar to set
	 */
	public void setForRowsHostVar(String forRowsHostVar) {
		this.forRowsHostVar = forRowsHostVar;
	}

	/**
	 * Restituisce il numero di roìighe della clausola FOR integer-constant ROWS in multiple-row-fetch<br>
	 * <p>
	 * @return the forRowsNumber
	 */
	public int getForRowsNumber() {
		return forRowsNumber;
	}

	/**
	 * Imposta il numero di roìighe della clausola FOR integer-constant ROWS in multiple-row-fetch<br>
	 * <p>
	 * @param forRowsNumber the forRowsNumber to set
	 */
	public void setForRowsNumber(int forRowsNumber) {
		this.forRowsNumber = forRowsNumber;
	}

	/**
	 * Imposta se presente la clausola FOR ... ROWS in multiple-row-fetch<br>
	 * <p>
	 * @return the isIntoHostVarsArray
	 */
	public boolean isIntoHostVarsArray() {
		return isIntoHostVarsArray;
	}

	/**
	 * Restituisce se presente la clausola INTO host-vars-array in multiple-row-fetch<br>
	 * <p>
	 * @param isIntoHostVarsArray the isIntoHostVarsArray to set
	 */
	public void setIntoHostVarsArray(boolean isIntoHostVarsArray) {
		this.isIntoHostVarsArray = isIntoHostVarsArray;
	}

	/**
	 * Restituisce se presente la clausola INTO DESCRIPTOR descriptor-name<br>
	 * <p>
	 * @return the isIntoDescriptor
	 */
	public boolean isIntoDescriptor() {
		return isIntoDescriptor;
	}

	/**
	 * Imposta se presente la clausola INTO DESCRIPTOR descriptor-name<br>
	 * <p>
	 * @param isIntoDescriptor the isIntoDescriptor to set
	 */
	public void setIntoDescriptor(boolean isIntoDescriptor) {
		this.isIntoDescriptor = isIntoDescriptor;
	}





}
