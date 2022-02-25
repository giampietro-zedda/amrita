
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import enums.EnumPrecompilerReservedWords;
import enums.EnumSqlOperator;
import enums.EnumSqlPredicate;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlPredicate
 * </h1>
 * <p>
 * Descrive un predicato Sql ovvero una condizione che può essere vera o falsa su una data riga o gruppo.<br> 
 * <p>
 * I predicati sono codificati come componenti della clausola <tt>WHERE</tt> e sono del tipo:<br>
 * <ul>
 *   <li>basic </li>
 *   <li>quantified </li>
 *   <li>BETWEEN </li>
 *   <li>DISTINCT </li>
 *   <li>EXISTS </li>
 *   <li>IN </li> 
 *   <li>LIKE </li> 
 *   <li>NULL </li> 
 *   <li>XMLEXISTS </li> 
 * 
 * </ul>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 19/lug/2011 
 * @see SqlExpression
 * @see SqlFullSelect
*/

public class SqlPredicate implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private EnumSqlPredicate typePredicate = null;             		// Tipologia predicato 
   
	// Campi per predicate BASIC
    private SqlExpression basicExpressionLeft = null;  				// expression ...
	private EnumSqlOperator basicOperator = null;					// ... = | <> | < | > | <= | >= ...	
    private SqlExpression basicExpressionRight = null;   			// ... expression	   	
	private boolean isBasicRowValueExpressions = false;				// (row-value-expression) =|<> (row-value-expression)
    private ArrayList<SqlExpression> al_basicRowValueExpressionLeft = null;
    private ArrayList<SqlExpression> al_basicRowValueExpressionRight = null;
	
	// Campi per predicate QUANTIFIED
    private SqlExpression quantifiedExpressionLeft = null;  	 	// expression 
	private EnumSqlOperator quantifiedOperator = null;				// ... =|<>|<|>|<=|>= ...	
	private EnumPrecompilerReservedWords quantifiedQualifier = null;// SOME|ANY|ALL	
	private SqlFullSelect quantifiedFullSelect = null;				// (full-select)
	private boolean isQuantifiedRowValueExpression = false;			// (row-value-expression) =|<> SOME|ANY|ALL (full-select)
    private ArrayList<SqlExpression> al_quantifiedRowValueExpressionLeft = null;

	// Campi per predicate BETWEEN
    private SqlExpression betweenExpression = null;  				// expression ...
    private boolean isBetweenNot = false;							// expression |NOT BETWEEN ...
    private SqlExpression betweenExpressionLower = null;  			// expression |NOT BETWEEN ... expressionLower
    private SqlExpression betweenExpressionHigher = null;  			// expression |NOT BETWEEN ... expressionLower AND expressionHigher
	
	// Campi per predicate DISTINCT
    private SqlExpression distinctExpressionLeft = null;  			// expression ...
    private SqlExpression distinctExpressionRight = null;   		// expression IS |NOT DISTINCT FROM expression   	
	private boolean isDistinctNot = false;							// expression IS  NOT DISTINCT FROM expression   	
	private boolean isDistinctRowValueExpression = false;			// (row-value-expression) IS |NOT DISTINCT FROM (row-value-expression)
    private ArrayList<SqlExpression> al_distinctRowValueExpressionLeft = null;
    private ArrayList<SqlExpression> al_distinctRowValueExpressionRight = null;
	
	// Campi per predicate EXISTS
    private SqlFullSelect existsFullSelect = null;                	// EXISTS(full-select)	
	
    // Campi per predicato IN
    private SqlExpression inExpressionLeft = null;  	    		// expression 
	private SqlFullSelect inFullSelect = null;						// expression |NOT IN (full-select)
    private boolean isInFullSelect = false;							// expression      IN (full-select)
    private boolean isInNot = false;								// expression  NOT IN (full-select)
    private ArrayList<SqlExpression> al_inExpressionRight = null;   // expression |NOT IN (expr1, expr2,.., exprn)
    private boolean isInRowValueExpressionLeft = false;				// (row-value-expression) |NOT IN (full-select)
    private ArrayList<SqlExpression> al_inRowValueExpressionLeft = null;

    
    // Campi per predicato LIKE
    private SqlExpression likeExpressionMatch = null;  	    		// match-expression 
    private SqlExpression likeExpressionPattern = null;  	    	// match-expression |NOT LIKE pattern-expression 
    private SqlExpression likeExpressionEscape = null;  	    	// match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression
    private boolean isLikeNot = false;								// match-expression  NOT LIKE pattern-expression |ESCAPE |escapeExpression

    // Campi per predicato NULL
    private SqlExpression nullExpression = null;  	    			// expression IS |NOT NULL
    private boolean isNullNot = false;								// expression IS  NOT NULL

    // Campi per predicato XMLEXISTS
    private String xmlexistsValue = ""; 	 						// XMLEXISTS(value)
    
	/**
	 * Costruttore
	 */
	public SqlPredicate() {
		super();
	    al_basicRowValueExpressionLeft = new ArrayList<SqlExpression> ();
	    al_basicRowValueExpressionRight = new ArrayList<SqlExpression> ();
	    al_quantifiedRowValueExpressionLeft =  new ArrayList<SqlExpression> ();
	    al_distinctRowValueExpressionLeft = new ArrayList<SqlExpression> ();
	    al_distinctRowValueExpressionRight = new ArrayList<SqlExpression> ();
	    al_inExpressionRight = new ArrayList<SqlExpression> ();
	    al_inRowValueExpressionLeft = new ArrayList<SqlExpression> ();

	}

	/**
	 * Restituisce il tipo di predicato.<br>
	 * <p>
	 * @return the typePredicate
	 */
	public EnumSqlPredicate getTypePredicate() {
		return typePredicate;
	}

	/**
	 * Imposta il tipo di predicato.<br>
	 * <p>
	 * @param typePredicate the typePredicate to set
	 */
	public void setTypePredicate(EnumSqlPredicate typePredicate) {
		this.typePredicate = typePredicate;
	}

	/**
	 * Restituisce l'espressione a sinistra del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @return the basicExpressionLeft
	 */
	public SqlExpression getBasicExpressionLeft() {
		return basicExpressionLeft;
	}

	/**
	 * Imposta l'espressione a sinistra del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @param basicExpressionLeft the basicExpressionLeft to set
	 */
	public void setBasicExpressionLeft(SqlExpression basicExpressionLeft) {
		this.basicExpressionLeft = basicExpressionLeft;
	}

	/**
	 * Restituisce il tipo di operatore del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @return the basicOperator
	 */
	public EnumSqlOperator getBasicOperator() {
		return basicOperator;
	}

	/**
	 * Imposta il tipo di operatore del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @param basicOperator the basicOperator to set
	 */
	public void setBasicOperator(EnumSqlOperator basicOperator) {
		this.basicOperator = basicOperator;
	}

	/**
	 * Restituisce l'espressione a destra del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @return the basicExpressionRight
	 */
	public SqlExpression getBasicExpressionRight() {
		return basicExpressionRight;
	}

	/**
	 * Imposta l'espressione a destra del predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @param basicExpressionRight the basicExpressionRight to set
	 */
	public void setBasicExpressionRight(SqlExpression basicExpressionRight) {
		this.basicExpressionRight = basicExpressionRight;
	}

	/**
	 * Restituisce se presenti espressioni di valutazione di riga nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * @return the isBasicRowValueExpressions
	 */
	public boolean isBasicRowValueExpressions() {
		return isBasicRowValueExpressions;
	}

	/**
	 * Imposta se presenti espressioni di valutazione di riga nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * @param isBasicRowValueExpressions the isBasicRowValueExpressions to set
	 */
	public void setBasicRowValueExpressions(boolean isBasicRowValueExpressions) {
		this.isBasicRowValueExpressions = isBasicRowValueExpressions;
	}

	
	
	/**
	 * Restituisce le espressioni di valutazione di riga di sinistra nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * <tt>(row-value-expression-left)</ttt> è composto da un insieme di espressioni sql separate da virgola.<br>
	 * <p>
	 * @return the al_basicRowValueExpressionLeft
	 */
	public ArrayList<SqlExpression> getBasicRowValueExpressionsLeft() {
		return al_basicRowValueExpressionLeft;
	}

	/**
	 * Imposta le espressioni di valutazione di riga di sinistra nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * <tt>(row-value-expression-left)</ttt> è composto da un insieme di espressioni sql separate da virgola.<br>
	 * <p>
	 * @param al_basicRowValueExpressionLeft the al_basicRowValueExpressionLeft to set
	 */
	public void setBasicRowValueExpressionsLeft(ArrayList<SqlExpression> al_basicRowValueExpressionLeft) {
		this.al_basicRowValueExpressionLeft = al_basicRowValueExpressionLeft;
	}

	/**
	 * Restituisce le espressioni di valutazione di riga di destra nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * <tt>(row-value-expression-right)</ttt> è composto da un insieme di espressioni sql separate da virgola.<br>
	 * <p>
	 * @return the al_basicRowValueExpressionRight
	 */
	public ArrayList<SqlExpression> getBasicRowValueExpressionsRight() {
		return al_basicRowValueExpressionRight;
	}

	/**
	 * Restituisce le espressioni di valutazione di riga di destra nel predicato <tt>BASIC</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) =|<> (row-value-expression-right)</tt><br>
	 * <p>
	 * <tt>(row-value-expression-right)</ttt> è composto da un insieme di espressioni sql separate da virgola.<br>
	 * <p>
	 * @param al_basicRowValueExpressionRight the al_basicRowValueExpressionRight to set
	 */
	public void setBasicRowValueExpressionRight(ArrayList<SqlExpression> al_basicRowValueExpressionRight) {
		this.al_basicRowValueExpressionRight = al_basicRowValueExpressionRight;
	}

	/**
	 * Restituisce l'espressione a sinistra del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @return the quantifiedExpressionLeft
	 */
	public SqlExpression getQuantifiedExpressionLeft() {
		return quantifiedExpressionLeft;
	}

	/**
	 * Imposta l'espressione a sinistra del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @param quantifiedExpressionLeft the quantifiedExpressionLeft to set
	 */
	public void setQuantifiedExpressionLeft(
			SqlExpression quantifiedExpressionLeft) {
		this.quantifiedExpressionLeft = quantifiedExpressionLeft;
	}

	/**
	 * Restituisce il tipo di operatore del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @return the quantifiedOperator
	 */
	public EnumSqlOperator getQuantifiedOperator() {
		return quantifiedOperator;
	}

	/**
	 * Imposta il tipo di operatore del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(expression-left) =|<> (expression-right)</tt><br>
	 * <p>
	 * @param quantifiedOperator the quantifiedOperator to set
	 */
	public void setQuantifiedOperator(EnumSqlOperator quantifiedOperator) {
		this.quantifiedOperator = quantifiedOperator;
	}

	/**
	 * Restituisce il qualificatore del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression) =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * Restituisce SOME, ANY o ALL<br>
	 * <p>
	 * @return the quantifiedQualifier
	 */
	public EnumPrecompilerReservedWords getQuantifiedQualifier() {
		return quantifiedQualifier;
	}

	/**
	 * Imposta il qualificatore del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>expression =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * Imposta SOME, ANY o ALL<br>
	 * <p>
	 * @param quantifiedQualifier the quantifiedQualifier to set
	 */
	public void setQuantifiedQualifier(EnumPrecompilerReservedWords quantifiedQualifier) {
		this.quantifiedQualifier = quantifiedQualifier;
	}

	/**
	 * Restituisce la full-select del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>expression =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @return the quantifiedFullSelect
	 */
	public SqlFullSelect getQuantifiedFullSelect() {
		return quantifiedFullSelect;
	}

	/**
	 * Imposta la full-select del predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>expression =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @param quantifiedFullSelect the quantifiedFullSelect to set
	 */
	public void setQuantifiedFullSelect(SqlFullSelect quantifiedFullSelect) {
		this.quantifiedFullSelect = quantifiedFullSelect;
	}

	/**
	 * Restituisce se presente un'espressione di valore di riga nel predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression) =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @return the isQuantifiedRowValueExpression
	 */
	public boolean isQuantifiedRowValueExpression() {
		return isQuantifiedRowValueExpression;
	}

	/**
	 * Imposta se presente un'espressione di valore di riga nel predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression) =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @param isQuantifiedRowValueExpression the isQuantifiedRowValueExpression to set
	 */
	public void setQuantifiedRowValueExpression(
			boolean isQuantifiedRowValueExpression) {
		this.isQuantifiedRowValueExpression = isQuantifiedRowValueExpression;
	}

	
	
	/**
	 * Restituisce le espressioni di valore di riga nel predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(row-value-expressions) =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @return the al_quantifiedRowValueExpressionLeft
	 */
	public ArrayList<SqlExpression> getQuantifiedRowValueExpressionsLeft() {
		return al_quantifiedRowValueExpressionLeft;
	}

	/**
	 * Imposta le espressioni di valore di riga nel predicato <tt>QUANTIFIED</tt>.<br>
	 * <p>
	 * <tt>(row-value-expressions) =|<> SOME|ANY|ALL (full-select)</tt><br>
	 * <p>
	 * @param alQuantifiedRowValueExpressionLeft the al_quantifiedRowValueExpressionLeft to set
	 */
	public void setQuantifiedRowValueExpressionLeft(ArrayList<SqlExpression> al_quantifiedRowValueExpressionLeft) {
		this.al_quantifiedRowValueExpressionLeft = al_quantifiedRowValueExpressionLeft;
	}

	/**
	 * Restituisce l'espressione da valutare nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @return the betweenExpression
	 */
	public SqlExpression getBetweenExpression() {
		return betweenExpression;
	}

	/**
	 * Imposta l'espressione da valutare nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @param betweenExpression the betweenExpression to set
	 */
	public void setBetweenExpression(SqlExpression betweenExpression) {
		this.betweenExpression = betweenExpression;
	}

	/**
	 * Restituisce se presente l'operatore <tt>NOT</tt> nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @return the isBetweenNot
	 */
	public boolean isBetweenNot() {
		return isBetweenNot;
	}

	/**
	 * Imposta se presente l'operatore <tt>NOT</tt> nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @param isBetweenNot the isBetweenNot to set
	 */
	public void setBetweenNot(boolean isBetweenNot) {
		this.isBetweenNot = isBetweenNot;
	}

	/**
	 * Restituisce l'espressione di limite inferiore nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @return the betweenExpressionLower
	 */
	public SqlExpression getBetweenExpressionLower() {
		return betweenExpressionLower;
	}

	/**
	 * Imposta l'espressione di limite inferiore nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @param betweenExpressionLower the betweenExpressionLower to set
	 */
	public void setBetweenExpressionLower(
			SqlExpression betweenExpressionLower) {
		this.betweenExpressionLower = betweenExpressionLower;
	}

	/**
	 * Restituisce l'espressione di limite superiore nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @return the betweenExpressionHigher
	 */
	public SqlExpression getBetweenExpressionHigher() {
		return betweenExpressionHigher;
	}

	/**
	 * Imposta l'espressione di limite superiore nel predicato <tt>BETWEEN</tt>.<br>
	 * <p>
	 * <tt>expression |NOT BETWEEN ... expressionLower AND expressionHigher</tt><br>
	 * <p>
	 * @param betweenExpressionHigher the betweenExpressionHigher to set
	 */
	public void setBetweenExpressionHigher(	SqlExpression betweenExpressionHigher) {
		this.betweenExpressionHigher = betweenExpressionHigher;
	}

	/**
	 * Restituisce l'espressione di sinistra del predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @return the distinctExpressionLeft
	 */
	public SqlExpression getDistinctExpressionLeft() {
		return distinctExpressionLeft;
	}

	/**
	 * Imposta l'espressione di sinistra del predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @param distinctExpressionLeft the distinctExpressionLeft to set
	 */
	public void setDistinctExpressionLeft(
			SqlExpression distinctExpressionLeft) {
		this.distinctExpressionLeft = distinctExpressionLeft;
	}

	/**
	 * Restituisce l'espressione di destra del predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @return the distinctExpressionRight
	 */
	public SqlExpression getDistinctExpressionRight() {
		return distinctExpressionRight;
	}

	/**
	 * Imposta l'espressione di destra del predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @param distinctExpressionRight the distinctExpressionRight to set
	 */
	public void setDistinctExpressionRight(SqlExpression distinctExpressionRight) {
		this.distinctExpressionRight = distinctExpressionRight;
	}

	/**
	 * Restituisce se presente l'operatore <tt>NOT</tt> nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @return the isDistinctNot
	 */
	public boolean getDistinctNot() {
		return isDistinctNot;
	}

	/**
	 * Imposta se presente l'operatore <tt>NOT</tt> nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>expression-left IS |NOT DISTINCT FROM expression-right  </tt><br>
	 * <p>
	 * @param isDistinctNot the isDistinctNot to set
	 */
	public void setDistinctNot(boolean isDistinctNot) {
		this.isDistinctNot = isDistinctNot;
	}

	/**
	 * Restituisce se presenti espressioni di valutazione di riga nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @return the isDistinctRowValueExpression
	 */
	public boolean isDistinctRowValueExpression() {
		return isDistinctRowValueExpression;
	}

	/**
	 * Imposta se presenti espressioni di valutazione di riga nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @param isDistinctRowValueExpression the isDistinctRowValueExpression to set
	 */
	public void setDistinctRowValueExpression(boolean isDistinctRowValueExpression) {
		this.isDistinctRowValueExpression = isDistinctRowValueExpression;
	}

		
	
	/**
	 * Restituisce le espressioni di valutazione di riga di sinistra nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @return the al_distinctRowValueExpressionLeft
	 */
	public ArrayList<SqlExpression> getDistinctRowValueExpressionsLeft() {
		return al_distinctRowValueExpressionLeft;
	}

	/**
	 * Imposta le espressioni di valutazione di riga di sinistra nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @param al_distinctRowValueExpressionLeft the al_distinctRowValueExpressionLeft to set
	 */
	public void setDistinctRowValueExpressionsLeft(ArrayList<SqlExpression> al_distinctRowValueExpressionLeft) {
		this.al_distinctRowValueExpressionLeft = al_distinctRowValueExpressionLeft;
	}

	/**
	 * Restituisce le espressioni di valutazione di riga di destra nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @return the al_distinctRowValueExpressionRight
	 */
	public ArrayList<SqlExpression> getDistinctRowValueExpressionsRight() {
		return al_distinctRowValueExpressionRight;
	}

	/**
	 * Imposta le espressioni di valutazione di riga di destra nel predicato <tt>DISTINCT</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left) IS |NOT DISTINCT FROM (row-value-expression-right)</tt><br>
	 * <p>
	 * @param al_distinctRowValueExpressionRight the al_distinctRowValueExpressionRight to set
	 */
	public void setDistinctRowValueExpressionsRight(ArrayList<SqlExpression> al_distinctRowValueExpressionRight) {
		this.al_distinctRowValueExpressionRight = al_distinctRowValueExpressionRight;
	}

	/**
	 * Restituisce la full-select del predicato <tt>EXISTS</tt>.<br>
	 * <p>
	 * <tt>EXISTS(full-select)	</tt><br>
	 * <p>
	 * @return the existsFullSelect
	 */
	public SqlFullSelect getExistsFullSelect() {
		return existsFullSelect;
	}

	/**
	 * Imposta la full-select del predicato <tt>EXISTS</tt>.<br>
	 * <p>
	 * <tt>EXISTS(full-select)	</tt><br>
	 * <p>
	 * @param existsFullSelect the existsFullSelect to set
	 */
	public void setExistsFullSelect(SqlFullSelect existsFullSelect) {
		this.existsFullSelect = existsFullSelect;
	}

	/**
	 * Restituisce l'espressione a sinistra del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expression-left |NOT IN (expr1, expr2,.., exprn)</tt><br>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the inExpressionLeft
	 */
	public SqlExpression getInExpressionLeft() {
		return inExpressionLeft;
	}

	/**
	 * Imposta l'espressione a sinistra del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expression-left |NOT IN (expr1, expr2,.., exprn)</tt><br>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param inExpressionLeft the inExpressionLeft to set
	 */
	public void setInExpressionLeft(SqlExpression inExpressionLeft) {
		this.inExpressionLeft = inExpressionLeft;
	}

	/**
	 * Restituisce la full-select del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the inFullSelect
	 */
	public SqlFullSelect getInFullSelect() {
		return inFullSelect;
	}

	/**
	 * Imposta la full-select del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param inFullSelect the inFullSelect to set
	 */
	public void setInFullSelect(SqlFullSelect inFullSelect) {
		this.inFullSelect = inFullSelect;
	}

	
	
	/**
	 * Restituisce se è codificata una full-select a destra del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the isInFullSelect
	 */
	public boolean isInFullSelect() {
		return isInFullSelect;
	}

	/**
	 * Imposta se è codificata una full-select a destra del predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param isInFullSelect the isInFullSelect to set
	 */
	public void setInFullSelect(boolean isInFullSelect) {
		this.isInFullSelect = isInFullSelect;
	}

	/**
	 * Restituisce se presente l'operatore <tt>NOT</tt> nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the isInNot
	 */
	public boolean isInNot() {
		return isInNot;
	}

	/**
	 * Imposta se presente l'operatore <tt>NOT</tt> nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expressionleft |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param isInNot the isInNot to set
	 */
	public void setInNot(boolean isInNot) {
		this.isInNot = isInNot;
	}

	/**
	 * Restituisce le espressioni a destra nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expression-left |NOT IN (expr1, expr2,.., exprn)</tt><br>
	 * <p>
	 * @return the al_inExpression
	 */
	public ArrayList<SqlExpression> getInExpressionsRight() {
		return al_inExpressionRight;
	}

	/**
	 * Imposta le espressioni a destra nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>expression-left |NOT IN (expr-right1, expr-right2,.., expr-rightn)</tt><br>
	 * <p>
	 * @param al_inExpressionRight the al_inExpressionRight to set
	 */
	public void setInExpressions(ArrayList<SqlExpression> al_inExpressionRight) {
		this.al_inExpressionRight = al_inExpressionRight;
	}

	/**
	 * Restituisce se a sinistra è presente una espressione di valutazione di valore, nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the isInRowValueExpressionLeft
	 */
	public boolean isInRowValueExpressionLeft() {
		return isInRowValueExpressionLeft;
	}

	/**
	 * Imposta se a sinistra è presente una espressione di valutazione di valore, nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param isInRowValueExpressionLeft the isInRowValueExpressionLeft to set
	 */
	public void setInRowValueExpressionLeft(boolean isInRowValueExpressionLeft) {
		this.isInRowValueExpressionLeft = isInRowValueExpressionLeft;
	}

	
	
	/**
	 * Restituisce l'espressione di valutazione di valore di sinistra, nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left |NOT IN (full-select)</tt><br>
	 * <p>
	 * @return the al_inRowValueExpressionLeft
	 */
	public ArrayList<SqlExpression> getInRowValueExpressionsLeft() {
		return al_inRowValueExpressionLeft;
	}

	/**
	 * Imposta l'espressione di valutazione di valore di sinistra, nel predicato <tt>IN</tt>.<br>
	 * <p>
	 * <tt>(row-value-expression-left |NOT IN (full-select)</tt><br>
	 * <p>
	 * @param al_inRowValueExpressionLeft the al_inRowValueExpressionLeft to set
	 */
	public void setInRowValueExpressionLeft(ArrayList<SqlExpression> al_inRowValueExpressionLeft) {
		this.al_inRowValueExpressionLeft = al_inRowValueExpressionLeft;
	}

	/**
	 * Restituisce l'espressione di match a sinistra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @return the likeExpressionMatch
	 */
	public SqlExpression getLikeExpressionMatch() {
		return likeExpressionMatch;
	}

	/**
	 * Imposta l'espressione di match a sinistra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @param likeExpressionMatch the likeExpressionMatch to set
	 */
	public void setLikeExpressionMatch(SqlExpression likeExpressionMatch) {
		this.likeExpressionMatch = likeExpressionMatch;
	}

	/**
	 * Restituisce l'espressione di pattern a destra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @return the likeExpressionPattern
	 */
	public SqlExpression getLikeExpressionPattern() {
		return likeExpressionPattern;
	}

	/**
	 * Imposta l'espressione di pattern a destra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @param likeExpressionPattern the likeExpressionPattern to set
	 */
	public void setLikeExpressionPattern(
			SqlExpression likeExpressionPattern) {
		this.likeExpressionPattern = likeExpressionPattern;
	}

	/**
	 * Restituisce l'espressione di escape a destra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @return the likeExpressionEscape
	 */
	public SqlExpression getLikeExpressionEscape() {
		return likeExpressionEscape;
	}

	/**
	 * Imposta l'espressione di escape a destra del predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @param likeExpressionEscape the likeExpressionEscape to set
	 */
	public void setLikeExpressionEscape(SqlExpression likeExpressionEscape) {
		this.likeExpressionEscape = likeExpressionEscape;
	}

	/**
	 * Restituisce se presente l'operatore <tt>NOT</> nel predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @return the isLikeNot
	 */
	public boolean isLikeNot() {
		return isLikeNot;
	}

	/**
	 * Imposta se presente l'operatore <tt>NOT</> nel predicato <tt>LIKE</tt>.<br>
	 * <p>
	 * <tt>match-expression |NOT LIKE pattern-expression |ESCAPE |escapeExpression</tt><br>
	 * <p>
	 * @param isLikeNot the isLikeNot to set
	 */
	public void setLikeNot(boolean isLikeNot) {
		this.isLikeNot = isLikeNot;
	}

	/**
	 * Restituisce l'espressione del predicato <tt>NULL</tt>.<br>
	 * <p>
	 * <tt>expression IS |NOT NULL</tt><br>
	 * <p>
	 * @return the nullExpression 
	 */
	public SqlExpression getNullExpression() {
		return nullExpression;
	}

	/**
	 * Imposta l'espressione del predicato <tt>NULL</tt>.<br>
	 * <p>
	 * <tt>expression IS |NOT NULL</tt><br>
	 * <p>
	 * @param nullExpression the nullExpression to set
	 */
	public void setNullExpression(SqlExpression nullExpression) {
		this.nullExpression = nullExpression;
	}

	/**
	 * Restituisce se presente l'operatore <tt>NOT</tt> nel predicato <tt>NULL</tt>.<br>
	 * <p>
	 * <tt>expression IS |NOT NULL</tt><br>
	 * <p>
	 * @return the isNullNot
	 */
	public boolean isNullNot() {
		return isNullNot;
	}

	/**
	 * Imposta se presente l'operatore <tt>NOT</tt> nel predicato <tt>NULL</tt>.<br>
	 * <p>
	 * @param isNullNot the isNullNot to set
	 */
	public void setNullNot(boolean isNullNot) {
		this.isNullNot = isNullNot;
	}

	/**
	 * Restituisce il valore del predicato <tt>XMLEXISTS</tt>.<br>
	 * <p>
	 * <tt>XMLEXISTS(value)</tt><br>
	 * <p>
	 * Viene restituita una stringa con il contenuto codificato fra parentesi, senza elaborazioni.<br>
	 * <p>
	 * @return the xmlexistsValue
	 */
	public String getXmlexistsValue() {
		return xmlexistsValue;
	}

	/**
	 * Imposta il valore del predicato <tt>XMLEXISTS</tt>.<br>
	 * <p>
	 * <tt>XMLEXISTS(value)</tt><br>
	 * <p>
	 * Viene restituita una stringa con il contenuto codificato fra parentesi, senza elaborazioni.<br>
	 * <p>
	 * @param xmlexistsValue the xmlexistsValue to set
	 */
	public void setXmlexistsValue(String xmlexistsValue) {
		this.xmlexistsValue = xmlexistsValue;
	}

	/**
	 * Restituisce tutte le variabili host presenti in tutte le espressioni presenti nel predicato.<br>
	 * <p>
	 * Le variabili host vengono restituite senza i due punti iniziali.<br>
	 * <p>
     * @return all host variables
	 */
	public ArrayList<String> getHostVars() {
		
		ArrayList<SqlExpression> al_sqlExpression = null;
		ArrayList<String> al_hostVar = null;
		al_hostVar = new ArrayList<String> ();
		
		al_sqlExpression = new ArrayList<SqlExpression>();
		
		// Caricamento di tutte le espressioni di cui eventualmente estrarre le variabili host
		if (basicExpressionLeft != null) {al_sqlExpression.add(basicExpressionLeft);}
		if (basicExpressionRight != null) {al_sqlExpression.add(basicExpressionRight);}
		if (al_basicRowValueExpressionLeft != null) {al_sqlExpression.addAll(al_basicRowValueExpressionLeft);}
		if (al_basicRowValueExpressionRight != null) {al_sqlExpression.addAll(al_basicRowValueExpressionRight);}
		if (quantifiedExpressionLeft != null) {al_sqlExpression.add(quantifiedExpressionLeft);}
		if (al_quantifiedRowValueExpressionLeft != null) {al_sqlExpression.addAll(al_quantifiedRowValueExpressionLeft);}
		if (al_distinctRowValueExpressionLeft != null) {al_sqlExpression.addAll(al_distinctRowValueExpressionLeft);}
		if (betweenExpression != null) {al_sqlExpression.add(betweenExpression);}
		if (betweenExpressionLower != null) {al_sqlExpression.add(betweenExpressionLower);}
		if (betweenExpressionHigher != null) {al_sqlExpression.add(betweenExpressionHigher);}
		if (distinctExpressionLeft != null) {al_sqlExpression.add(distinctExpressionLeft);}
		if (distinctExpressionRight != null) {al_sqlExpression.add(distinctExpressionRight);}
		if (al_distinctRowValueExpressionLeft != null) {al_sqlExpression.addAll(al_distinctRowValueExpressionLeft);}
		if (al_distinctRowValueExpressionRight != null) {al_sqlExpression.addAll(al_distinctRowValueExpressionRight);}
		if (inExpressionLeft != null) {al_sqlExpression.add(inExpressionLeft);}
		if (al_inExpressionRight != null) {al_sqlExpression.addAll(al_inExpressionRight);}
		if (al_inRowValueExpressionLeft != null) {al_sqlExpression.addAll(al_inRowValueExpressionLeft);}
		if (likeExpressionMatch != null) {al_sqlExpression.add(likeExpressionMatch);}
		if (likeExpressionPattern != null) {al_sqlExpression.add(likeExpressionPattern);}
		if (likeExpressionEscape != null) {al_sqlExpression.add(likeExpressionEscape);}
		
		// Scan espressioni e accodamento variabili host in espressione
		for (SqlExpression sqlExpression : al_sqlExpression) {
 			al_hostVar.addAll(sqlExpression.getHostVars());
		}
		
		return al_hostVar;
	}


	/**
	 * Restituisce tutte le costanti alfanumeriche, literal, presenti in tutte le espressioni del predicato.<br>
	 * <p>
     * @return all constants alpha
	 */
	public ArrayList<String> getConstantsAlphanumeric() {
		
		ArrayList<String> al_constantAlpha = null;
		al_constantAlpha = new ArrayList<String> ();
		
		// Basic
		if (basicExpressionLeft != null) {al_constantAlpha.addAll(basicExpressionLeft.getConstantsAlphanumeric());}
		if (basicExpressionRight != null) {al_constantAlpha.addAll(basicExpressionRight.getConstantsAlphanumeric());}
	    for (SqlExpression sqlExpression : al_basicRowValueExpressionLeft) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
	    for (SqlExpression sqlExpression : al_basicRowValueExpressionRight) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
	    
	    // Quantified
		if (quantifiedExpressionLeft != null) {al_constantAlpha.addAll(quantifiedExpressionLeft.getConstantsAlphanumeric());}
	    for (SqlExpression sqlExpression : al_quantifiedRowValueExpressionLeft) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
	
	    // Between
		if (betweenExpression != null) {al_constantAlpha.addAll(betweenExpression.getConstantsAlphanumeric());}
		if (betweenExpressionLower != null) {al_constantAlpha.addAll(betweenExpressionLower.getConstantsAlphanumeric());}
		if (betweenExpressionHigher != null) {al_constantAlpha.addAll(betweenExpressionHigher.getConstantsAlphanumeric());}
		if (distinctExpressionLeft != null) {al_constantAlpha.addAll(distinctExpressionLeft.getConstantsAlphanumeric());}
		if (distinctExpressionRight != null) {al_constantAlpha.addAll(distinctExpressionRight.getConstantsAlphanumeric());}
		   
		// Distinct row
		for (SqlExpression sqlExpression : al_distinctRowValueExpressionLeft) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
		for (SqlExpression sqlExpression : al_distinctRowValueExpressionRight) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
		   
		// Exists (full-select)
		if (existsFullSelect != null) {
			al_constantAlpha.addAll(existsFullSelect.getConstantsAlphanumeric());
		}
 		
		// In
		if (inExpressionLeft != null) {al_constantAlpha.addAll(inExpressionLeft.getConstantsAlphanumeric());}
		for (SqlExpression sqlExpression : al_inExpressionRight) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};
		for (SqlExpression sqlExpression : al_inRowValueExpressionLeft) {al_constantAlpha.addAll(sqlExpression.getConstantsAlphanumeric());};

		// Like
		if (likeExpressionMatch != null) {al_constantAlpha.addAll(likeExpressionMatch.getConstantsAlphanumeric());}
		if (likeExpressionPattern != null) {al_constantAlpha.addAll(likeExpressionPattern.getConstantsAlphanumeric());}
		if (likeExpressionEscape != null) {al_constantAlpha.addAll(likeExpressionEscape.getConstantsAlphanumeric());}
		if (nullExpression != null) {al_constantAlpha.addAll(nullExpression.getConstantsAlphanumeric());}
		   
		return al_constantAlpha;
	}

	/**
	 * Restituisce tutte le costanti numeriche, presenti nell'espressione.<br>
	 * <p>
	 * Vengono restituite stringhe contenenti numeri validi.<br>
	 * <p>
     * @return all constants numeric
	 */
	public ArrayList<String> getConstantsNumeric() {
		
		ArrayList<String> al_constantNumeric = null;
		al_constantNumeric = new ArrayList<String> ();
		
		// Basic
		if (basicExpressionLeft != null) {al_constantNumeric.addAll(basicExpressionLeft.getConstantsNumeric());}
		if (basicExpressionRight != null) {al_constantNumeric.addAll(basicExpressionRight.getConstantsNumeric());}
	    for (SqlExpression sqlExpression : al_basicRowValueExpressionLeft) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
	    for (SqlExpression sqlExpression : al_basicRowValueExpressionRight) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
	    
	    // Quantified
		if (quantifiedExpressionLeft != null) {al_constantNumeric.addAll(quantifiedExpressionLeft.getConstantsNumeric());}
	    for (SqlExpression sqlExpression : al_quantifiedRowValueExpressionLeft) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
	
	    // Between
		if (betweenExpression != null) {al_constantNumeric.addAll(betweenExpression.getConstantsNumeric());}
		if (betweenExpressionLower != null) {al_constantNumeric.addAll(betweenExpressionLower.getConstantsNumeric());}
		if (betweenExpressionHigher != null) {al_constantNumeric.addAll(betweenExpressionHigher.getConstantsNumeric());}
		if (distinctExpressionLeft != null) {al_constantNumeric.addAll(distinctExpressionLeft.getConstantsNumeric());}
		if (distinctExpressionRight != null) {al_constantNumeric.addAll(distinctExpressionRight.getConstantsNumeric());}
		   
		// Distinct row
		for (SqlExpression sqlExpression : al_distinctRowValueExpressionLeft) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
		for (SqlExpression sqlExpression : al_distinctRowValueExpressionRight) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
		   
		// Exists (full-select)
		if (existsFullSelect != null) {
			al_constantNumeric.addAll(existsFullSelect.getConstantsNumeric());
		}
		
		// In
		if (inExpressionLeft != null) {al_constantNumeric.addAll(inExpressionLeft.getConstantsNumeric());}
		for (SqlExpression sqlExpression : al_inExpressionRight) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};
		for (SqlExpression sqlExpression : al_inRowValueExpressionLeft) {al_constantNumeric.addAll(sqlExpression.getConstantsNumeric());};

		// Like
		if (likeExpressionMatch != null) {al_constantNumeric.addAll(likeExpressionMatch.getConstantsNumeric());}
		if (likeExpressionPattern != null) {al_constantNumeric.addAll(likeExpressionPattern.getConstantsNumeric());}
		if (likeExpressionEscape != null) {al_constantNumeric.addAll(likeExpressionEscape.getConstantsNumeric());}
		if (nullExpression != null) {al_constantNumeric.addAll(nullExpression.getConstantsNumeric());}
		   
		
		return al_constantNumeric;
	}


	
}
