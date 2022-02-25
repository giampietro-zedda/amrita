package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumCobolFigurativeConstants;
import enums.EnumCobolOperator;
import enums.EnumCobolReservedWords;
import enums.EnumSymbolType;
import enums.EnumDataItemGeneric;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ExpressionSql  
 * </h1>
 *  <p>
 * Questa classe modella una espressione logica e/o matematica e/o con predicati Sql.<br>
 * <p>
 * Si tratta di operandi e operatori presenti in espressioni Sql e quindi contenenti
 * costanti, variabili host, function etc, ful-select etc.<br>
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/09/2010
 * @see ExpressionSqlElement
 * @see EnumCobolOperator
 * @see EnumSymbolType
 *   
 */
@SuppressWarnings("unused")
public class ExpressionSql implements Serializable {

	private static final long serialVersionUID = 1L;

	// Operandi e operatori in sequenza
	ArrayList<ExpressionSqlElement> al_expressionElement = null;
	
	
	/**
	 * Costruttore
	 */
	public ExpressionSql() {
		al_expressionElement = new ArrayList<ExpressionSqlElement> ();
	}
	
	/**
	 * Inserisce un elemento all'espressione Cobol.
	 * 
	 * @parm ExpressionSqlElement expressionElement
	 */
	public void addElement(ExpressionSqlElement expressionElement) {
		al_expressionElement.add(expressionElement);
	}

	/**
	 * Restituisce un Array con gli elementi dell'espressione.
	 * 
	 * @parm ExpressionSqlElement expressionElement
	 */
	public ExpressionSqlElement[] getElements() {
		ExpressionSqlElement ar_expressionElement[] = null;
		
		ar_expressionElement = new ExpressionSqlElement[al_expressionElement.size()];
		ar_expressionElement = al_expressionElement.toArray(ar_expressionElement);
		
		return ar_expressionElement;
	}


	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono variabili host o locali <br>
	 * <p>
	 * se nessun operando restituisce un array vuoto.
	 * 
	 * 
	 * @parm String[] nome-simbolo
	 */
	public String[] getOperandsFields() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionSqlElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getTypeElement() == EnumSymbolType.SQL_SYMBOL_HOST_VAR
			||  element.getTypeElement() == EnumSymbolType.SQL_SYMBOL_DEFINED_VAR) {
				al_elementField.add(element.getValueElement());
			}
		}
		
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);
		return ar_elementField;
	}

	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono literal numeriche Sql <br>
	 * <p>
	 * 
	 *  @parm String[] nome-simbolo
	 */
	public String[] getOperandsLiteralNumeric() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionSqlElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getTypeElement() == EnumSymbolType.SQL_SYMBOL_LITERAL_NUM) {
				al_elementField.add(element.getValueElement());
			}
		}
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);

		return ar_elementField;
	}

	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono literal alfanumeriche Sql <br>
	 * <p>
	 * 
	 *  @parm String[] nome-simbolo
	 */
	public String[] getOperandsLiteralAlpha() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionSqlElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getTypeElement() == EnumSymbolType.SQL_SYMBOL_LITERAL_ALPHA) {
					al_elementField.add(element.getValueElement());
			}
		}
		
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);

		return ar_elementField;
	}




	

	/**
	 * Ottimizza le strutture interne di memorizzazione.
	 * 
	 * @return int count of operators
	 */
	public void optimize() {
		al_expressionElement.trimToSize();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return al_expressionElement.toString();
	}
	
	

}
