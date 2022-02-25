package analyzer;
import java.io.Serializable;

import utilities.StringService;

import enums.EnumCobolFigurativeConstants;
import enums.EnumCobolOperator;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumSymbolType;



/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda  Turin (ITALY)
  * 
 * <h1>
 * ExpressionCobolElement 
 * </h1>
 *  <p>
 * Questa classe modella un singolo elemento di una espressione logica e/o matematica Cobol.<br>
 * <p>
 * L'elemento può essere un operando (un campo definito nel programma, literal
 * o costante figurativa) o un operatore (logico o matematico)<br>
 * Ogni oggetto di questa classe viene utilizzata da oggetti {@link ExpressionCobol}
 * per comporre espressioni complete.
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/09/2010
 * @see ExpressionCobol
 * @see EnumCobolOperator
 * @see EnumSymbolType
 * @see EnumCobolFigurativeConstants
 *   
 */
public class ExpressionCobolElement implements Serializable {

	private static final long serialVersionUID = 1L;

	private EnumCobolReservedWords reservedWord = null;                 // Parola riservata Cobol identificante il simbolo o il data item
	private DataItemCobolIdentifier dataItemIdentifier = null;          // Identificatore completo di qualificatore se operando
    private boolean operand = false;                                    // True indica un simbolo Cobol operando di espressione
    
    
	
	/**
	 * Costruttore 
	 */
	public ExpressionCobolElement(EnumCobolReservedWords reservedWord, String symbolValue, DataItemCobolIdentifier dataItemIdentifier) {
		this.reservedWord = reservedWord;
		this.dataItemIdentifier = dataItemIdentifier;
        this.operand = false;  
        if (this.reservedWord.getCobolSymbolType() != EnumSymbolType.COBOL_SYMBOL_OPERATOR) {
       	    this.operand = true;  
		}
	}


	/**
	 * 
	 * Restituisce true se l'elemento descrive un operando di espressione Cobol.
	 * 
	 * @return the operand
	 */
	public boolean isOperand() {
		return operand;
	}

	/**
	 * 
	 * Restituisce la parola riservata cobol che rappresenta il simbolo
	 * nell'espressione.<br>
	 * 
	 * 
	 * @return the reserved cobol enumeration
	 */
	public EnumCobolReservedWords getReservedWordCobol() {
		return this.reservedWord;
	}
	/**
	 * 
	 * Restituisce il tipo di simbolo generale.
	 * 
	 * @return the symbolType
	 */
	public EnumSymbolType getSymbolType() {
		return this.reservedWord.getCobolSymbolType();
	}

	/**
	 * 
	 * Restituisce il tipo operatore se simbolo operatore.
	 * 
	 * @return the operatorType
	 */
	public EnumCobolOperator getOperatorType() {
		return this.reservedWord.getCobolOperator();
	}

	/**
	 * 
	 * Restituisce il tipo di costante figurativa se simbolo costante figurativa.
	 * 
	 * @return the figurativeType
	 */
	public EnumCobolFigurativeConstants getFigurativeType() {
		return this.reservedWord.getCobolFigurative();
	}

	/**
	 * @return the specialRegisterType
	 */
	public EnumCobolReservedWords getSpecialRegisterType() {
		
		if (this.reservedWord.getCobolInstrCategory() != EnumInstrDataCategory.COBOL_SPECIAL_REGISTER) {
			return EnumCobolReservedWords.NOT_ASSIGNED;
		}
		
		return this.reservedWord;
	}


	/**
	 * Restituisce il valore del simbolo.
	 * 
	 * @return the value
	 */
	public String getValue() {
		String value = "";
		
		switch (this.reservedWord.getCobolInstrCategory()) {
			case COBOL_FIGURATIVE:
				value = this.reservedWord.getCobolFigurative().toString();
				break;
			case COBOL_LITERAL_ALPHA:
				value = this.dataItemIdentifier.getNameIdentifier();
				break;

			case COBOL_LITERAL_NUM:
				value = this.dataItemIdentifier.getNameIdentifier();
				break;
				
			case COBOL_OPERATOR:
				value = this.reservedWord.getCobolOperator().toString();
				break;
				
			default:
				value = this.dataItemIdentifier.getNameIdentifier();
				break;
			}
		return value;
	}


	/**
	 * Restituisce il valore del simbolo numerico.
	 * 
	 * @return the symbolValue numeric
	 */
	public int getValueNumeric() {
		int value = 0;
		
		if (this.reservedWord.getCobolInstrCategory() != EnumInstrDataCategory.COBOL_LITERAL_NUM) {
			return 0;
		}
		
		value = StringService._getNumericInt(this.dataItemIdentifier.getNameIdentifier());
		
		return value;
	}

	/**
	 * @return the dataItemIdentifier
	 */
	public DataItemCobolIdentifier getDataItemIdentifier() {
		return dataItemIdentifier;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		
		String strType = "";
		if (this.reservedWord.getCobolSymbolType() == EnumSymbolType.NOT_ASSIGNED) {
			strType = this.reservedWord.toString();
		} else {
            strType = this.reservedWord.getCobolSymbolType().toString();
		}
		
		
		return strType + " " + this.getValue();
	}


	
	

}
