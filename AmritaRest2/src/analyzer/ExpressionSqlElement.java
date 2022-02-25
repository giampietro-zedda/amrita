package analyzer;
import java.io.Serializable;
import enums.EnumSqlOperator;
import enums.EnumSymbolType;



/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda  Turin (ITALY)
  * 
 * <h1>
 * ExpressionSqlElement 
 * </h1>
 *  <p>
 * Questa classe modella un singolo elemento di una espressione logica e/o matematica e/o predicato Sql.<br>
 * <p>
 * L'elemento può essere una variabile host, una costante, un'operatore matematico, il nome di una colonna
 * un operatore di predicato, il riferimento a una full.select, a una function etc., 
 * oppure una ulteriore expressionSql<br>
 * Ogni oggetto di questa classe viene utilizzata da oggetti {@link ExpressionSql}
 * per comporre espressioni complete.
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/09/2010
 * @see ExpressionSql
 * @see EnumSymbolType
 *   
 */
public class ExpressionSqlElement implements Serializable {

	private static final long serialVersionUID = 1L;

	private EnumSymbolType typeElement = null;                 			// Parola riservata Cobol identificante il simbolo o il data item
	private EnumSqlOperator typeOperatorPredicate = null;      // Parola riservata Cobol identificante il simbolo o il data item
    private String valueElement = "";                                   // Nome costante, host-var, column, function etc.
    private ExpressionSql expression = null;                            // Oggetto expression
    private InstructionSql fullSelect = null;                           // Istruzione completa full-select se indicato da typeElement
	
	/**
	 * Costruttore 
	 */
	public ExpressionSqlElement() {
		this.typeElement = EnumSymbolType.NOT_ASSIGNED;
		this.typeOperatorPredicate = EnumSqlOperator.NOT_ASSIGNED;

	
	}


	/**
	 * 
	 * Restituisce true se l'elemento descrive un operando numerico, alfanumerico, un campo
	 * o una colonna di tabella.
	 * 
	 * @return the operand
	 */
	public boolean isOperand() {
		// TODO
		return false;
	}

	
	/**
	 * Restituisce la tipologia generale dell'elemento.<br>
	 * <p>
	 * 
	 * @return the typeElement
	 */
	public EnumSymbolType getTypeElement() {
		return typeElement;
	}


	/**
	 * Imposta la tipologia generale dell'elemento.<br>
	 * <p>
	 * 
	 * @param typeElement the typeElement to set
	 */
	public void setTypeElement(EnumSymbolType typeElement) {
		this.typeElement = typeElement;
	}


	/**
	 * Restituisce la tipologia specifica dell'operatore o del predicato
	 * descritto dall'elemento.<br>
	 * <p>
	 * 
	 * @return the typeOperatorPredicate
	 */
	public EnumSqlOperator getTypeOperatorPredicate() {
		return typeOperatorPredicate;
	}


	/**
	 * Imposta la tipologia specifica dell'operatore o del predicato
	 * descritto dall'elemento.<br>
	 * <p>
	 * 
	 * @param typeOperatorPredicate the typeOperatorPredicate to set
	 */
	public void setTypeOperatorPredicate(EnumSqlOperator typeOperatorPredicate) {
		this.typeOperatorPredicate = typeOperatorPredicate;
	}


	/**
	 * Restituisce il valore rappresentato dall'elemento.<br>
	 * <p>
	 * In base al tipo di elemento si tratta di costanti, variabili-host,
	 * function name etc.
	 * 
	 * @return the valueElement
	 */
	public String getValueElement() {
		return valueElement;
	}


	/**
	 * Imposta il valore rappresentato dall'elemento.<br>
	 * <p>
	 * In base al tipo di elemento si tratta di costanti, variabili-host,
	 * function name etc.
	 * 
	 * @return the valueElement
	 * @param valueElement the valueElement to set
	 */
	public void setValueElement(String valueElement) {
		this.valueElement = valueElement;
	}


	/**
	 * Restituisce l'espressione Sql completa codificata dall'elemento.<br>
	 * <p>
	 * Si tratta di un altro oggetto che può fare riferimento ad altre
	 * espressioni, ricorsivamente.<br>
	 * 
	 * @return the expression
	 */
	public ExpressionSql getExpression() {
		return expression;
	}


	/**
	 * Imposta l'espressione Sql completa codificata dall'elemento.<br>
	 * <p>
	 * Si tratta di un altro oggetto che può fare riferimento ad altre
	 * espressioni, ricorsivamente.<br>
	 * 
	 * @param expression the expression to set
	 */
	public void setExpression(ExpressionSql expression) {
		this.expression = expression;
	}


	/**
	 * Restituisce L'istruzione Sql completa codificata dall'elemento
	 * che rappresenta una full-select.<br>
	 * <p>
	 * 
	 * @return the fullSelect
	 */
	public InstructionSql getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta L'istruzione Sql completa codificata dall'elemento
	 * che rappresenta una full-select.<br>
	 * <p>
	 * 
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(InstructionSql fullSelect) {
		this.fullSelect = fullSelect;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		
		return this.typeElement.toString() + typeOperatorPredicate.toString() + " " + this.valueElement;
	}


	
	

}
