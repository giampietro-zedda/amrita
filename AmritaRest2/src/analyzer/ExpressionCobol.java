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
 * ExpressionCobol  
 * </h1>
 *  <p>
 * Questa classe modella una espressione logica e/o matematica Cobol.<br>
 * <p>
 * Vengono memorizzati gli operandi e gli operatori nell'ordine in cui sono
 * stati trovati nell'espressione.<br>
 * Sono disponibili metodi per conteggiare operandi, operatori e per produrre
 * la rappresentazione in forma polacca inversa dell'espressione <b>
 * Gli operandi vengono memorizzati come identificatori cobol completi, con il
 * reference all'oggetto {@link InstructionCobolDataItem} che li descrive, gli
 * indici eventuali e il reference modification.
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/09/2010
 * @see ExpressionCobolElement
 * @see EnumCobolOperator
 * @see EnumSymbolType
 *   
 */
@SuppressWarnings("unused")
public class ExpressionCobol implements Serializable {

	private static final long serialVersionUID = 1L;

	// Operandi e operatori in sequenza
	ArrayList<ExpressionCobolElement> al_expressionElement = null;
	
	
	/**
	 * Costruttore
	 */
	public ExpressionCobol() {
		al_expressionElement = new ArrayList<ExpressionCobolElement> ();
	}
	
	/**
	 * Inserisce un elemento all'espressione Cobol.
	 * 
	 * @parm ExpressionCobolElement expressionElement
	 */
	public void addElement(ExpressionCobolElement expressionElement) {
		al_expressionElement.add(expressionElement);
	}

	/**
	 * Restituisce un Array con gli elementi dell'espressione.
	 * 
	 * @parm ExpressionCobolElement expressionElement
	 */
	public ExpressionCobolElement[] getElements() {
		ExpressionCobolElement ar_expressionElement[] = null;
		
		ar_expressionElement = new ExpressionCobolElement[al_expressionElement.size()];
		ar_expressionElement = al_expressionElement.toArray(ar_expressionElement);
		
		return ar_expressionElement;
	}

	/**
	 * Restituisce un Array con gli elementi della polacca inversa dell'espressione.<br>
	 * <p>
	 * Es pressioni come A = B + C vengono interpretate come BC+A=
	 * 
	 * @parm ExpressionCobolElement expressionElement
	 */
	public ExpressionCobolElement[] getReversedPolac() {
		
		// TODO
		
		return null;
	}

	/**
	 * Restituisce un Array con tutti gli operandi dell'espressione. <br>
	 * <p>
	 * 
	 * @parm String[] nome-simbolo
	 */
	public String[] getOperands() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() != EnumSymbolType.COBOL_SYMBOL_OPERATOR) {
				al_elementField.add(element.getValue());
			}
		}
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);
		return ar_elementField;
	}

	
	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono dei campi Cobol <br>
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
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() == EnumSymbolType.COBOL_SYMBOL_DATA_ITEM) {
				al_elementField.add(element.getValue());
			}
		}
		
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);
		return ar_elementField;
	}

	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono literal numeriche Cobol <br>
	 * <p>
	 * 
	 *  @parm String[] nome-simbolo
	 */
	public String[] getOperandsLiteralNumeric() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() == EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM) {
				al_elementField.add(element.getValue());
			}
		}
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);

		return ar_elementField;
	}

	/**
	 * Restituisce un Array con gli operandi dell'espressione che sono literal alfanumeriche Cobol <br>
	 * <p>
	 * 
	 *  @parm String[] nome-simbolo
	 */
	public String[] getOperandsLiteralAlpha() {
		ArrayList<String> al_elementField = null;
		String ar_elementField[];
		
		al_elementField = new ArrayList<String>();
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() == EnumSymbolType.COBOL_SYMBOL_LITERAL_ALPHA) {
				al_elementField.add(element.getValue());
			}
		}
		
		ar_elementField = new String[al_elementField.size()];
		ar_elementField = (String[]) al_elementField.toArray(ar_elementField);

		return ar_elementField;
	}

	/**
	 * Restituisce il nnumero totale di operatori OR e AND presenti nell'espressione,
	 * per il calcolo dell'indice esteso di complessità di McCabe <br>
	 * <p>
	 * 
	 *  @return the number of OR AND
	 */
	public int getMcCabeOperatorsExtended() {
		int cntOrAnd = 0;
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
		 
			// Interessano solo gli operatori OR AND
			if (element.getSymbolType() != EnumSymbolType.COBOL_SYMBOL_OPERATOR) {continue;}
			if (element.getOperatorType() != EnumCobolOperator.LOGIC_OR
			&&  element.getOperatorType() != EnumCobolOperator.LOGIC_AND) {
				continue;
			}
			
			// Conteggio
			cntOrAnd++;
		}
		return cntOrAnd;
	}

	/**
	 * Restituisce un array con le parole chiave nell'espressione,
	 * considerate operatori secondo Halstead.<br>
	 * Si tratta di simboli che implicano una <tt>azione</tt> come
	 * parentesi aperta, chiusa, virgole di separazione, tutti gli
	 * operatori matematici e logici in generale.<br>
	 * Queste informazioni sono essenziali per il calcolo degli
	 * indici di Halstead di lunghezza, volume, sforzo e difficoltà
	 * del programma.<br>
	 * Per approfondimenti si veda la classe {@link Metrics}.<br>
	 * <p>
	 * 
	 *  @return the number of Halstead operators
	 */
	public ArrayList<EnumCobolReservedWords> getHalsteadOperators() {
		ArrayList<EnumCobolReservedWords> al_operator = null;
		int cntOperators = 0;
		
		al_operator = new ArrayList<EnumCobolReservedWords>();
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
		 
			// Interessano solo gli operatori  
			if (element.getSymbolType() != EnumSymbolType.COBOL_SYMBOL_OPERATOR) {continue;}
			
			// Accodamento operatore
			al_operator.add(element.getReservedWordCobol());
		}
		
		return al_operator;
	}


	
	/**
	 * Restituisce il numero di operandi presenti nell'espressione.
	 * 
	 * @return int count of operands  
	 */
	public int getCountOperands() {
		
		int cnt = 0;
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() != EnumSymbolType.COBOL_SYMBOL_OPERATOR) {
				cnt++;
			}
		}
		return cnt;
	}

	/**
	 * Restituisce il numero di operatori presenti nell'espressione.<br>
	 * <p>
	 * Si tratta di operatori <, >, =, <> ma anche AND, OR.<br>
	 * In definitiva si tratta di simboli che non sono data item, literal o costanti figurative.<br>
	 * <p>
	 * 
	 * @return int count of operands
	 */
	public int getCountOperators() {
		
		int cnt = 0;
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() == EnumSymbolType.COBOL_SYMBOL_OPERATOR) {
				cnt++;
			}
		}
		return cnt;
	}

	/**
	 * Restituisce il numero di condizioni presenti nell'espressione.<br>
	 * <p>
	 * Una condizione è separata da un'altra da un operatore AND oppure OR.<br>
	 * Questo metodo restituisce il numero di operatori +1 di questo tipo presenti.<br>
	 * <p>
	 * @return int count of conditions
	 */
	public int getCountConditions() {
		
		int cnt = 1;
		
		// Scan Elementi espressione
		for (ExpressionCobolElement element : al_expressionElement) {
			// Campo: lo porto in output
			if (element.getSymbolType() == EnumSymbolType.COBOL_SYMBOL_OPERATOR) {
				if (element.getOperatorType() == EnumCobolOperator.LOGIC_AND
				||  element.getOperatorType() == EnumCobolOperator.LOGIC_OR) {
					cnt++;
				}
			}
		}
		return cnt;
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
