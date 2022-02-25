package analyzer;
import java.io.Serializable;
/**
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * CobolPerformLoop
 * </h1>
 *  <p>
 *
 * Descrive una condizione completa di loop di uno statement perform.<br>
 * <p>
 * Vengono descritte tutte le condizioni di loop, in particolare:
 *    
 *   1) Varying identifier
 *   2) From expression
 *   3) By identifier
 *   4) Condition expression
 *  
 *   
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 31/05/2010
 * @see AnalyzerCobol
 * @see ExpressionCobol
 *   
*/

public class CobolPerformLoop implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private DataItemCobolIdentifier varyingIdentifier = null;   // Identifier di loop
	private ExpressionCobol fromExpression = null;   			// Espressione aritmetica di inizio identifier
	private DataItemCobolIdentifier byIdentifier = null;   		// Identifier di incremento/decremento varyingIdentifier
	private ExpressionCobol untilConditionExpression = null;   	// Espressione logico/aritmetica di condizione loop
	

	/*
	 * Costruttore vuoto
	 */
	public CobolPerformLoop() {
		super();
	}


	/**
	 * @return the varyingIdentifier
	 */
	public DataItemCobolIdentifier getVaryingIdentifier() {
		return varyingIdentifier;
	}


	/**
	 * @param varyingIdentifier the varyingIdentifier to set
	 */
	public void setVaryingIdentifier(DataItemCobolIdentifier varyingIdentifier) {
		this.varyingIdentifier = varyingIdentifier;
	}


	/**
	 * @return the fromExpression
	 */
	public ExpressionCobol getFromExpression() {
		return fromExpression;
	}


	/**
	 * @param fromExpression the fromExpression to set
	 */
	public void setFromExpression(ExpressionCobol fromExpression) {
		this.fromExpression = fromExpression;
	}


	/**
	 * @return the byIdentifier
	 */
	public DataItemCobolIdentifier getByIdentifier() {
		return byIdentifier;
	}


	/**
	 * @param byIdentifier the byIdentifier to set
	 */
	public void setByIdentifier(DataItemCobolIdentifier byIdentifier) {
		this.byIdentifier = byIdentifier;
	}


	/**
	 * @return the untilConditionExpression
	 */
	public ExpressionCobol getUntilConditionExpression() {
		return untilConditionExpression;
	}


	/**
	 * @param untilConditionExpression the untilConditionExpression to set
	 */
	public void setUntilConditionExpression(ExpressionCobol untilConditionExpression) {
		this.untilConditionExpression = untilConditionExpression;
	}

	
}
