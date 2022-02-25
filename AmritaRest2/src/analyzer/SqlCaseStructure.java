
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlCaseStructure
 * </h1>
 * <p>
 * Descrive una struttura <tt>CASE WHEN expr THEN expr ELSE expr END AS type </tt> Sql.<br> 
 * <p>
 * Le espressioni possono essere una singola colonna, una literal o oespressioni Sql complesse.<br>
 * Le espressioni di THEN e di ELSE possono essere esprressioni ricorsive di ulteriori strutture CASE.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/10/2011 
 * @see SqlCaseElement
*/

public class SqlCaseStructure implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
    private ArrayList<SqlCaseElement> al_caseElement = null;   			// Elementi case When o Else
    private String asName = "";   										// Nome codificato in clausola AS name
       
    
	/**
	 * Costruttore vuoto
	 */
	public SqlCaseStructure() {
		super();
		al_caseElement = new ArrayList<SqlCaseElement> ();
	}


	/**
	 * Restituisce gli elementi When/Else presenti nella case,<br>
	 * <p>
	 * @return the al_caseElement
	 */
	public ArrayList<SqlCaseElement> getCaseElements() {
		return al_caseElement;
	}


	/**
	 * Imposta gli elementi When/Else presenti nella case,<br>
	 * <p>
	 * @param alCaseElement the al_caseElement to set
	 */
	public void setCaseElements(ArrayList<SqlCaseElement> alCaseElement) {
		al_caseElement = alCaseElement;
	}


	/**
	 * Restituisce il nome della clausa AS name dello statement CASE.<br>
	 * <p>
	 * @return the asName
	 */
	public String getAsName() {
		return asName;
	}


	/**
	 * Imposta il nome della clausa AS name dello statement CASE.<br>
	 * <p>
	 * @param asName the asName to set
	 */
	public void setAsName(String asName) {
		this.asName = asName;
	}


	
}
