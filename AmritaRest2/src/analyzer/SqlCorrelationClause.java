
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlCorrelationClause
 * </h1>
 * <p>
 * Descrive la clausola di correlazione, presente nella clausola FROM nelle istruzioni sql dove prevista,<br> 
 * che permette di associare un nome, detto correlation-name ed eventuali nomi di colonne, a una tabella.<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement
 * @see SqlFullSelect
 * @see SqlSubselectSelectInto
*/

public class SqlCorrelationClause implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private String corrName  = "";                      		// |AS corr-name |(new-col1, new-col2,...,new-coln)
	private ArrayList<String> al_newColName = null;             // |AS corr-name |(new-col1, new-col2,...,new-coln)
    
	/**
	 * Costruttore vuoto
	 */
	public SqlCorrelationClause() {
		super();
		al_newColName = new ArrayList<String> ();
	}

	/**
	 * Restituisce il correlation name.<br>
	 * <p>
	 * <tt>|AS corr-name |(new-col1, new-col2,...,new-coln)</tt><br>
	 * <p>
	 * @return the corrName
	 */
	public String getCorrName() {
		return corrName;
	}

	/**
	 * Imposta il correlation name.<br>
	 * <p>
	 * <tt>|AS corr-name |(new-col1, new-col2,...,new-coln)</tt><br>
	 * <p>
	 * @param corrName the corrName to set
	 */
	public void setCorrName(String corrName) {
		this.corrName = corrName;
	}

	/**
	 * Restituisce i nuovi nomi per le colonne associate al correlation name.<br>
	 * <p>
	 * <tt>|AS corr-name |(new-col1, new-col2,...,new-coln)</tt><br>
	 * <p>
	 * @return the al_newColName
	 */
	public ArrayList<String> getNewColNames() {
		return al_newColName;
	}

	/**
	 * Imposta i nuovi nomi per le colonne associate al correlation name.<br>
	 * <p>
	 * <tt>|AS corr-name |(new-col1, new-col2,...,new-coln)</tt><br>
	 * <p>
	 * @param alNewColName the al_newColName to set
	 */
	public void setNewColNames(ArrayList<String> alNewColName) {
		al_newColName = alNewColName;
	}


	
}
