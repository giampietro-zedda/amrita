
package analyzer;
import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlFromTableReferenceTableFunction
 * </h1>
 * <p>
 * Descrive un singoli elemento della function-name dichiarata nella clausola FROM per <tt>table-function-reference</tt><br> 
 * contenuti fra parentesi<br>
 * <p>
 * <tt>TABLE (function-name ( expression1|TABLE transiction-table-name1 , expression2|TABLE transiction-table-name2, ..., )</tt><br>
 * <p> 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement
 * @see SqlFullSelect
*/

public class SqlFromTableReferenceTableFunction implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private boolean isExpression = false;       	// True indica che l'elemento è una espressione
	private boolean isTransitionTableName = false;  // True indica che l'elemento è il nome di una tabella 
 	private SqlExpression expression= null;         // Espressione 
 	private String transitionTableName = "";        // Nome tabella
	
    
	/**
	 * Costruttore vuoto
	 */
	public SqlFromTableReferenceTableFunction() {
		super();
	}


	/**
	 * Restituisce se l'elemento codifica una espressione.<br>
	 * <p>
	 * @return the isExpression
	 */
	public boolean isExpression() {
		return isExpression;
	}


	/**
	 * Imposta se l'elemento codifica una espressione.<br>
	 * <p>
	 * @param isExpression the isExpression to set
	 */
	public void setExpression(boolean isExpression) {
		this.isExpression = isExpression;
	}


	/**
	 * Restituisce se l'elemento definisce una transition-table-name per table-reference <table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @return the isTransitionTableName
	 */
	public boolean isTransitionTable() {
		return isTransitionTableName;
	}


	/**
	 * Imposta se l'elemento definisce una transition-table-name per table-reference <table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @param isTransitionTableName the isTransitionTableName to set
	 */
	public void setTransitionTable(boolean isTransitionTableName) {
		this.isTransitionTableName = isTransitionTableName;
	}


	/**
	 * Restituisce l'espressione codificata dall'elemento.<br>
	 * <p>
	 * @return the expression
	 */
	public SqlExpression getExpression() {
		return expression;
	}


	/**
	 * Imposta l'espressione codificata dall'elemento.<br>
	 * <p>
	 * @param expression the expression to set
	 */
	public void setExpression(SqlExpression expression) {
		this.expression = expression;
	}


	/**
	 * Restituisce il nome transition-table-name per table-reference <table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @return the transitionTableName
	 */
	public String getTransitionTableName() {
		return transitionTableName;
	}


	/**
	 * Imposta il nome transition-table-name per table-reference <table-function-reference<</tt><br>
	 * <p>
	 * Si tratta di:<br>
	 * <p>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY int</tt> </li>
	 * <li> <tt>TABLE (function-name(expr, TABLE transition-table-name, expr, ... ) ) CARDINALITY MULTIPLIER int</tt> </li>
	 * <p>
	 * <p>
	 * @param transitionTableName the transitionTableName to set
	 */
	public void setTransitionTableName(String transitionTableName) {
		this.transitionTableName = transitionTableName;
	}

	
}
