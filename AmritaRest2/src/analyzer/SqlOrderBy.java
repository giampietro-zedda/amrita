
package analyzer;

import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorOrderBy
 * </h1>
 * <p>
 * Descrive un singolo elemento della clausol ORDER BY codificata in subselect, select into e full-select<br> 
 * <p>
 * Si tratta di informazioni quali nome colonna di ordinamento e tipo ordinamento. la colonna di ordinamento
 * può essere espressa come un nome, un numero oppure una espressione.
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlFullSelect
 * @see SqlSubselectSelectInto
*/

public class SqlOrderBy implements Serializable{

	private static final long serialVersionUID = 1L;
	
	private String columnName = ""; 							// Nome colonna di ordinamento				
	private int columnNumber = 0; 								// Numero colonna di ordinamento			
	private String tableDesignator = ""; 						// Correlation name indicato in una precedente clausola from				
	private SqlExpression sortKeyExpression = null; 			// Espressione che rappresenta la chiave di ordinamento			
    private boolean isOrderAscending = true;                    // True indica ordinamento ascendente
    private boolean isOrderDescending = false;                  // True indica ordinamento discendente
	private boolean isOrderBySortKeyExpression = false;         // True indica ordinamento attraverso un'espressione
	private boolean isOrderByColumnName = false;                // True indica ordinamento attraverso il nome della colonna
	private boolean isOrderByColumnNumber = false;              // True indica ordinamento attraverso il numero della colonna
    private boolean isOrderByInputSequence = false;             // True indica ordinamento che riflette quello di caricamento tabella
    private boolean isOrderByOfTableDesignator = false;         // True indica ordinamento attraverso il table designator
     
    
	/**
	 * Costruttore vuoto
	 */
	public SqlOrderBy() {
		super();
	}


	/**
	 * Restituisce il nome della colonna di ordinamento.<br>
	 * <p>
	 * @return the columnName
	 */
	public String getColumnName() {
		return columnName;
	}


	/**
	 * Imposta il nome della colonna di ordinamento.<br>
	 * <p>
	 * @param columnName the columnName to set
	 */
	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}


	/**
	 * Restituisce il numero della colonna di ordinamento.<br>
	 * <p>
	 * @return the columnNumber
	 */
	public int getColumnNumber() {
		return columnNumber;
	}


	/**
	 * Imposta il numero della colonna di ordinamento.<br>
	 * <p>
	 * @param columnNumber the columnNumber to set
	 */
	public void setColumnNumber(int columnNumber) {
		this.columnNumber = columnNumber;
	}


	/**
	 * Restituisce il table designator, che deve essere stato
	 * codificato come correlation name nella precedente clausola FROM<br>
	 * <p>
	 * @return the tableDesignator
	 */
	public String getTableDesignator() {
		return tableDesignator;
	}


	/**
	 * Restituisce il table designator, che deve essere stato
	 * codificato come correlation name nella precedente clausola FROM<br>
	 * <p>
	 * @param tableDesignator the tableDesignator to set
	 */
	public void setTableDesignator(String tableDesignator) {
		this.tableDesignator = tableDesignator;
	}


	/**
	 * Restituisce l'espressione della chiave di ordinamento, se l'ordinamento
	 * della colonna è espresso in questa modalità.<br>
	 * <p>
	 * @return the sortKeyExpression
	 */
	public SqlExpression getSortKeyExpression() {
		return sortKeyExpression;
	}


	/**
	 * Imposta l'espressione della chiave di ordinamento, se l'ordinamento
	 * della colonna è espresso in questa modalità.<br>
	 * <p>
	 * @param sortKeyExpression the sortKeyExpression to set
	 */
	public void setSortKeyExpression(SqlExpression sortKeyExpression) {
		this.sortKeyExpression = sortKeyExpression;
	}


	/**
	 * Restituisce se l'ordinamento della colonna è espresso attraverso il suo nome.<br>
	 * <p>
	 * @return the isOrderByColumnName
	 */
	public boolean isOrderByColumnName() {
		return isOrderByColumnName;
	}


	/**
	 * Imposta de l'ordinamento della colonna è espresso attraverso il suo nome.<br>
	 * <p>
	 * @param isOrderByColumnName the isOrderByColumnName to set
	 */
	public void setOrderByColumnName(boolean isOrderByColumnName) {
		this.isOrderByColumnName = isOrderByColumnName;
	}


	/**
	 * Restituisce l'ordinamento della colonna è espresso attraverso il suo numero.<br>
	 * <p>
	 * @return the isOrderByColumnNumber
	 */
	public boolean isOrderByColumnNumber() {
		return isOrderByColumnNumber;
	}


	/**
	 * Imposta l'ordinamento della colonna è espresso attraverso il suo numero.<br>
	 * <p>
	 * @param isOrderByColumnNumber the isOrderByColumnNumber to set
	 */
	public void setOrderByColumnNumber(boolean isOrderByColumnNumber) {
		this.isOrderByColumnNumber = isOrderByColumnNumber;
	}


	/**
	 * Restituisce se l'ordinamento della colonna è espresso attraverso una espresione.<br>
	 * <p>
	 * @return the isOrderBySortKeyExpression
	 */
	public boolean isOrderBySortKeyExpression() {
		return isOrderBySortKeyExpression;
	}


	/**
	 * Imposta se l'ordinamento della colonna è espresso attraverso una espresione.<br>
	 * <p>
	 * @param isOrderBySortKeyExpression the isOrderBySortKeyExpression to set
	 */
	public void setOrderBySortKeyExpression(boolean isOrderBySortKeyExpression) {
		this.isOrderBySortKeyExpression = isOrderBySortKeyExpression;
	}


	/**
	 * @return the isOrderAscending
	 */
	public boolean isOrderAscending() {
		return isOrderAscending;
	}


	/**
	 * Restituisce se l'ordinamento è ascendente<br>
	 * <p>
	 * @param isOrderAscending the isOrderAscending to set
	 */
	public void setOrderAscending(boolean isOrderAscending) {
		this.isOrderAscending = isOrderAscending;
	}


	/**
	 * Imposta se l'ordinamento è ascendente<br>
	 * <p>
	 * @return the isOrderDescending
	 */
	public boolean isOrderDescending() {
		return isOrderDescending;
	}


	/**
	 * Restituisce se l'ordinamento è discendente<br>
	 * <p>
	 * @param isOrderDescending the isOrderDescending to set
	 */
	public void setOrderDescending(boolean isOrderDescending) {
		this.isOrderDescending = isOrderDescending;
		this.isOrderAscending = false;
	}


	/**
	 * Restituisce se l'ordinamento riflette la sequenza di caricamento delle righe<br>
	 * <p>
	 * @return the isOrderByInputSequence
	 */
	public boolean isOrderByInputSequence() {
		return isOrderByInputSequence;
	}


	/**
	 * Imposta se l'ordinamento riflette la sequenza di caricamento delle righe<br>
	 * <p>
	 * @param isOrderByInputSequence the isOrderByInputSequence to set
	 */
	public void setOrderByInputSequence(boolean isOrderByInputSequence) {
		this.isOrderByInputSequence = isOrderByInputSequence;
	}


	/**
	 * Restituisce se l'ordinamento rifletto quello associato al table designator
	 * impostato nella precedente clausola FROM<br>
	 * <p>
	 * @return the isOrderByOfTableDesignator
	 */
	public boolean isOrderByOfTableDesignator() {
		return isOrderByOfTableDesignator;
	}


	/**
	 * Imposta se l'ordinamento rifletto quello associato al table designator
	 * impostato nella precedente clausola FROM<br>
	 * <p>
	 * @param isOrderByOfTableDesignator the isOrderByOfTableDesignator to set
	 */
	public void setOrderByOfTableDesignator(boolean isOrderByOfTableDesignator) {
		this.isOrderByOfTableDesignator = isOrderByOfTableDesignator;
	}


	
}
