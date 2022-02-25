
package analyzer;
import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFromTableReferenceFullJoinExpression
 * </h1>
 * <p>
 * Descrive la condizione di un join usato in una join-condition di una clausola FROM<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlFromTableReference
 * @see SqlSelectStatement
 * @see SqlFullSelect
*/

public class SqlFromTableReferenceJoinedTableCondition implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
    private boolean isSearchCondition = false;              		// true indica search-condition
    private SqlSearchConditions searchCondition = null;             // search condition

    private boolean isFullOuterJoinAnd = false;                     // true indica AND 
    private boolean isFullOuterJoinLeftEqRight = false;             // true indica full-join-expression-left = full-join-expression-right  

    // Campi per Full Outer Join left 
    private String columnNameLeft = "";								// column-name
    private String castFunctionLeftValue = "";						// CAST( value )
    private boolean isColumnNameLeft = false;						//
    private boolean isCastFunctionLeft = false;						// CAST(expression|NULL|parameter-marker
    private boolean isCoalesceLeft = false;							// COALESCE( value )
    private String coalesceLeftValue = "";							// COALESCE( value )

    // Campi per Full Outer Join right
    private String columnNameRight = "";							// column-name
    private String castFunctionRightValue = "";						// CAST( value )
    private boolean isColumnNameRight = false;						//
    private boolean isCastFunctionRight = false;					//
    private boolean isCoalesceRight = false;						// COALESCE( value )
    private String coalesceRightValue = "";							// COALESCE( value )
    
	/**
	 * Costruttore vuoto
	 */
	public SqlFromTableReferenceJoinedTableCondition() {
		super();
	}

	/**
	 * Restituisce se la codizione di join è una search-condition<br>
	 * <p>
	 * <tt>JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @return the isSearchCondition
	 */
	public boolean isSearchCondition() {
		return isSearchCondition;
	}

	/**
	 * Imposta se la codizione di join è una search-condition<br>
	 * <p>
	 * <tt>JOIN table-reference ON join-condition</tt>
	 * <p>
	 * @param isSearchCondition the isSearchCondition to set
	 */
	public void setSearchCondition(boolean isSearchCondition) {
		this.isSearchCondition = isSearchCondition;
	}

	/**
	 * Restituisce se è attiva la condizione di AND del costrutto di outer join:<br>
	 * <p>
	 * <ul>
	 * <li>FULL OUTER JOIN full-join-expression-left = full-join-expression-right AND </li>
	 * </ul>
	 * @return the isFullOuterJoin
	 */
	public boolean isFullOuterJoinAnd() {
		return isFullOuterJoinAnd;
	}

	/**
	 * Imposta se è attiva la condizione di AND del costrutto di outer join:<br>
	 * <p>
	 * <ul>
	 * <li>FULL OUTER JOIN full-join-expression-left = full-join-expression-right AND </li>
	 * </ul>
	 * @param isFullOuterJoinAnd the isFullOuterJoinAnd to set
	 */
	public void setFullOuterJoinAnd(boolean isFullOuterJoinAnd) {
		this.isFullOuterJoinAnd = isFullOuterJoinAnd;
	}

	
	
	/**
	 * Restituisce se è attiva la condizione di eguaglianza di espressioni di join<br>
	 * <p>
	 * <ul>
	 * <li>FULL OUTER JOIN full-join-expression-left = full-join-expression-right</li>
	 * </ul>
	 * @return the isFullOuterJoinLeftEqRight
	 */
	public boolean isFullOuterJoinLeftEqRight() {
		return isFullOuterJoinLeftEqRight;
	}

	/**
	 * Imposta se è attiva la condizione di eguaglianza di espressioni di join<br>
	 * <p>
	 * <ul>
	 * <li>FULL OUTER JOIN full-join-expression-left = full-join-expression-right</li>
	 * </ul>
	 * @param isFullOuterJoinLeftEqRight the isFullOuterJoinLeftEqRight to set
	 */
	public void setFullOuterJoinLeftEqRight(boolean isFullOuterJoinLeftEqRight) {
		this.isFullOuterJoinLeftEqRight = isFullOuterJoinLeftEqRight;
	}

	/**
	 * Restituisce le condizione di search se il tipo di join è:<br>
	 * <p>
	 * <ul>
	 * <li>INNER</li>
	 * <li>LEFT OUTER</li>
	 * <li>RIGHT OUTER</li>
	 * </ul>
	 * @return the searchCondition
	 */
	public SqlSearchConditions getSearchCondition() {
		return searchCondition;
	}

	/**
	 * Imposta le condizione di search se il tipo di join è:<br>
	 * <p>
	 * <ul>
	 * <li>INNER</li>
	 * <li>LEFT OUTER</li>
	 * <li>RIGHT OUTER</li>
	 * </ul>
	 * @param searchCondition the searchCondition to set
	 */
	public void setSearchCondition(SqlSearchConditions searchCondition) {
		this.searchCondition = searchCondition;
	}

	/**
	 * Restituisce il nome della colonna di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the columnNameLeft
	 */
	public String getColumnNameLeft() {
		return columnNameLeft;
	}

	/**
	 * Imposta il nome della colonna di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param columnNameLeft the columnNameLeft to set
	 */
	public void setColumnNameLeft(String columnNameLeft) {
		this.columnNameLeft = columnNameLeft;
	}

	/**
	 * Restituisce il contenuto fra parentesi della funzione di cast di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>cast-function-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the castFunctionLeftValue
	 */
	public String getCastFunctionLeftValue() {
		return castFunctionLeftValue;
	}

	/**
	 * Imposta il contenuto fra parentesi della funzione di cast di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>cast-function-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param castFunctionLeftValue the castFunctionLeftValue to set
	 */
	public void setCastFunctionLeftValue(String castFunctionLeftValue) {
		this.castFunctionLeftValue = castFunctionLeftValue;
	}

	/**
	 * Restituisce se a sinistra della full-join-expression c'è il  nome di una colonna.<br>
	 * <p>
	 * <tt>column-name-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the isColumnNameLeft
	 */
	public boolean isColumnNameLeft() {
		return isColumnNameLeft;
	}

	/**
	 * Imposta se a sinistra della full-join-expression c'è il  nome di una colonna.<br>
	 * <p>
	 * <tt>column-name-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param isColumnNameLeft the isColumnNameLeft to set
	 */
	public void setColumnNameLeft(boolean isColumnNameLeft) {
		this.isColumnNameLeft = isColumnNameLeft;
	}

	/**
	 * Restituisce se a sinistra della full-join-expression c'è il  nome di una funzione di cast.<br>
	 * <p>
	 * <tt>cast-function-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the isCastFunctionLeft
	 */
	public boolean isCastFunctionLeft() {
		return isCastFunctionLeft;
	}

	/**
	 * Imposta se a sinistra della full-join-expression c'è il  nome di una funzione di cast.<br>
	 * <p>
	 * <tt>cast-function-left = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param isCastFunctionLeft the isCastFunctionLeft to set
	 */
	public void setCastFunctionLeft(boolean isCastFunctionLeft) {
		this.isCastFunctionLeft = isCastFunctionLeft;
	}

	/**
	 * Restituisce se a sinistra della full-join-expression c'è la funzione COALESCE.<br>
	 * <p>
	 * <tt>COALESCE(value) = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the isCoalesceLeft
	 */
	public boolean isCoalesceLeft() {
		return isCoalesceLeft;
	}

	/**
	 * Imposta se a sinistra della full-join-expression c'è la funzione COALESCE.<br>
	 * <p>
	 * <tt>COALESCE(value) = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param isCoalesceLeft the isCoalesceLeft to set
	 */
	public void setCoalesceLeft(boolean isCoalesceLeft) {
		this.isCoalesceLeft = isCoalesceLeft;
	}

	/**
	 * Restituisce il valore fra parentesi della funzione COALESCE di sinistra della full-join-expression.<br>
	 * <p>
	 * <tt>COALESCE(value) = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @return the coalesceLeftValue
	 */
	public String getCoalesceLeftValue() {
		return coalesceLeftValue;
	}

	/**
	 * Imposta il valore fra parentesi della funzione COALESCE di sinistra della full-join-expression.<br>
	 * <p>
	 * <tt>COALESCE(value) = column-name-right|cast-function-right|COALESCE(..)</tt>
	 * <p>
	 * @param coalesceLeftValue the coalesceLeftValue to set
	 */
	public void setCoalesceLeftValue(String coalesceLeftValue) {
		this.coalesceLeftValue = coalesceLeftValue;
	}

	/**
	 * Restituisce il nome della colonna di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = column-name-right</tt>
	 * <p>
	 * @return the columnNameRight
	 */
	public String getColumnNameRight() {
		return columnNameRight;
	}

	
	
	
	/**
	 * Imposta il nome della colonna di sinistra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = column-name-right</tt>
	 * <p>
	 * @param columnNameRight the columnNameRight to set
	 */
	public void setColumnNameRight(String columnNameRight) {
		this.columnNameRight = columnNameRight;
	}

	/**
	 * Restituisce il valore fra parentesi della funzione di cast di destra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = cast-function-right</tt>
	 * <p>
	 * @return the castFunctionRightValue
	 */
	public String getCastFunctionRightValue() {
		return castFunctionRightValue;
	}

	/**
	 * Imposta il valore fra parentesi della funzione di cast di destra della full-join-expression se il tipo di join è <tt>FULL OUTER JOIN</tt><br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = cast-function-right</tt>
	 * <p>
	 * @param castFunctionRightValue the castFunctionRightValue to set
	 */
	public void setCastFunctionRightValue(String castFunctionRightValue) {
		this.castFunctionRightValue = castFunctionRightValue;
	}

	/**
	 * Restituisce se a destra della full-join-expression c'è il  nome di una colonna.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = column-name-right</tt>
	 * <p>
	 * @return the isColumnNameRight
	 */
	public boolean isColumnNameRight() {
		return isColumnNameRight;
	}

	/**
	 * Imposta se a destra della full-join-expression c'è il  nome di una colonna.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = column-name-right</tt>
	 * <p>
	 * @param isColumnNameRight the isColumnNameRight to set
	 */
	public void setColumnNameRight(boolean isColumnNameRight) {
		this.isColumnNameRight = isColumnNameRight;
	}

	/**
	 * Restituisce se a destra della full-join-expression c'è il  nome di una funzione di cast.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = cast-function-right</tt>
	 * <p>
	 * @return the isCastFunctionRight
	 */
	public boolean isCastFunctionRight() {
		return isCastFunctionRight;
	}

	/**
	 * Imposta se a destra della full-join-expression c'è il  nome di una funzione di cast.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = cast-function-right</tt>
	 * <p>
	 * @param isCastFunctionRight the isCastFunctionRight to set
	 */
	public void setCastFunctionRight(boolean isCastFunctionRight) {
		this.isCastFunctionRight = isCastFunctionRight;
	}

	/**
	 * Restituisce se a destra della full-join-expression c'è la funzione COALESCE.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = COALESCE(value)</tt>
	 * <p>
	 * @return the isCoalesceRight
	 */
	public boolean isCoalesceRight() {
		return isCoalesceRight;
	}

	/**
	 * Imposta se a destra della full-join-expression c'è la funzione COALESCE.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = COALESCE(value)</tt>
	 * <p>
	 * @param isCoalesceRight the isCoalesceRight to set
	 */
	public void setCoalesceRight(boolean isCoalesceRight) {
		this.isCoalesceRight = isCoalesceRight;
	}

	/**
	 * Restituisce il valore fra parentesi della funzione COALESCE di destra della full-join-expression.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = COALESCE(value) = </tt>
	 * <p>
	 * @return the coalesceRightValue
	 */
	public String getCoalesceRightValue() {
		return coalesceRightValue;
	}

	/**
	 * Imposta il valore fra parentesi della funzione COALESCE di destra della full-join-expression.<br>
	 * <p>
	 * <tt>column-name-left|cast-function-left|COALESCE(..) = COALESCE(value) = </tt>
	 * <p>
	 * @param coalesceRightValue the coalesceRightValue to set
	 */
	public void setCoalesceRightValue(String coalesceRightValue) {
		this.coalesceRightValue = coalesceRightValue;
	}

}
