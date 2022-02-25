
package analyzer;
import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlCaseElement
 * </h1>
 * <p>
 * Descrive un singolo elemento <tt>WHEN expr-cond THEN expr-assign</tt> o <tt>ELSE expr-assign</tt><br>
 * di una struttura CASE<br>
 * <p>
 * Le espressioni di assegnazione a fronte di THEN o ELSE possono essere a loro volta <br>
 * strutture CASE ricorsive.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/10/2011 
 * @see SqlCaseElement
*/

public class SqlCaseElement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	// Sources
    private String exprWhenSource = "";   				// Source expressione condizionale dopo WHEN
    private String exprThenSource = "";   				// Source expressione di assegnazione dopo THEN
    private String exprElseSource = "";   				// Source expressione dopo ELSE
    
    // Codifica come espressioni
    private SqlExpression exprWhen = null;   			// Expressione condizionale dopo WHEN
    private SqlExpression exprThen = null;   			// Expressione di assegnazione dopo THEN
    private SqlExpression exprElse = null;   			// Expressione dopo ELSE
    
    // Attivazione ricorsiva
    boolean isExprThenRecursive = false;            	// Dopo THEN è codificata un'altra CASE ricorsivamente
    boolean isExprElseRecursive = false;            	// Dopo ELSE è codificata un'altra CASE ricorsivamente
    private SqlCaseStructure caseThenRecursive = null; 	// Struttura CASE ricorsiva dopo THEN
    private SqlCaseStructure caseElseRecursive = null; 	// Struttura CASE ricorsiva dopo ELSE
    
	/**
	 * Costruttore vuoto
	 */
	public SqlCaseElement() {
		super();
	}

	/**
	 * Restituisce il source dell'espressione condizionale dopo WHEN<br>
	 * <p>
	 * @return the exprWhenSource
	 */
	public String getExprWhenSource() {
		return exprWhenSource;
	}

	/**
	 * Imposta il source dell'espressione condizionale dopo WHEN<br>
	 * <p>
	 * @param exprWhenSource the exprWhenSource to set
	 */
	public void setExprWhenSource(String exprWhenSource) {
		this.exprWhenSource = exprWhenSource;
	}

	/**
	 * Restituisce il source dell'espressione di assegnazione dopo THEN<br>
	 * <p>
	 * @return the exprThenSource
	 */
	public String getExprThenSource() {
		return exprThenSource;
	}

	/**
	 * Imposta il source dell'espressione di assegnazione dopo THEN<br>
	 * <p>
	 * @param exprThenSource the exprThenSource to set
	 */
	public void setExprThenSource(String exprThenSource) {
		this.exprThenSource = exprThenSource;
	}

	/**
	 * Restituisce il source dell'espressione di assegnazione dopo ELSE<br>
	 * <p>
	 * @return the exprElseSource
	 */
	public String getExprElseSource() {
		return exprElseSource;
	}

	/**
	 * Imposta il source dell'espressione di assegnazione dopo ELSE<br>
	 * <p>
	 * @param exprElseSource the exprElseSource to set
	 */
	public void setExprElseSource(String exprElseSource) {
		this.exprElseSource = exprElseSource;
	}

	/**
	 * Restituisce l'espressione codificata condizionale dopo WHEN<br>
	 * <p>
	 * @return the exprWhen
	 */
	public SqlExpression getExprWhen() {
		return exprWhen;
	}

	/**
	 * Imposta l'espressione codificata condizionale dopo WHEN<br>
	 * <p>
	 * @param exprWhen the exprWhen to set
	 */
	public void setExprWhen(SqlExpression exprWhen) {
		this.exprWhen = exprWhen;
	}

	/**
	 * Restituisce l'espressione codificata di assegnazione dopo THEN<br>
	 * <p>
	 * @return the exprThen
	 */
	public SqlExpression getExprThen() {
		return exprThen;
	}

	/**
	 * Imposta l'espressione codificata di assegnazione dopo THEN<br>
	 * <p>
	 * @param exprThen the exprThen to set
	 */
	public void setExprThen(SqlExpression exprThen) {
		this.exprThen = exprThen;
	}

	/**
	 * Restituisce l'espressione codificata di assegnazione dopo ELSE<br>
	 * <p>
	 * @return the exprElse
	 */
	public SqlExpression getExprElse() {
		return exprElse;
	}

	/**
	 * Imposta l'espressione codificata di assegnazione dopo ELSE<br>
	 * <p>
	 * @param exprElse the exprElse to set
	 */
	public void setExprElse(SqlExpression exprElse) {
		this.exprElse = exprElse;
	}

	/**
	 * Restituisce se dopo THEN è presente una struttura CASE ricorsiva<br>
	 * <p>
	 * @return the isExprThenRecursive
	 */
	public boolean isExprThenRecursive() {
		return isExprThenRecursive;
	}

	/**
	 * Imposta se dopo THEN è presente una struttura CASE ricorsiva<br>
	 * <p>
	 * @param isExprThenRecursive the isExprThenRecursive to set
	 */
	public void setExprThenRecursive(boolean isExprThenRecursive) {
		this.isExprThenRecursive = isExprThenRecursive;
	}

	/**
	 * Restituisce se dopo ELSE è presente una struttura CASE ricorsiva<br>
	 * <p>
	 * @return the isExprElseRecursive
	 */
	public boolean isExprElseRecursive() {
		return isExprElseRecursive;
	}

	/**
	 * Imposta se dopo ELSE è presente una struttura CASE ricorsiva<br>
	 * <p>
	 * @param isExprElseRecursive the isExprElseRecursive to set
	 */
	public void setExprElseRecursive(boolean isExprElseRecursive) {
		this.isExprElseRecursive = isExprElseRecursive;
	}

	/**
	 * Restituisce la struttura CASE ricorsiva dopo THEN<br>
	 * <p>
	 * @return the caseThenRecursive
	 */
	public SqlCaseStructure getCaseThenRecursive() {
		return caseThenRecursive;
	}

	/**
	 * Imposta la struttura CASE ricorsiva dopo THEN<br>
	 * <p>
	 * @param caseThenRecursive the caseThenRecursive to set
	 */
	public void setCaseThenRecursive(SqlCaseStructure caseThenRecursive) {
		this.caseThenRecursive = caseThenRecursive;
	}

	/**
	 * Restituisce la struttura CASE ricorsiva dopo ELSE<br>
	 * <p>
	 * @return the caseElseRecursive
	 */
	public SqlCaseStructure getCaseElseRecursive() {
		return caseElseRecursive;
	}

	/**
	 * Imposta la struttura CASE ricorsiva dopo ELSE<br>
	 * <p>
	 * @param caseElseRecursive the caseElseRecursive to set
	 */
	public void setCaseElseRecursive(SqlCaseStructure caseElseRecursive) {
		this.caseElseRecursive = caseElseRecursive;
	}


	
}
