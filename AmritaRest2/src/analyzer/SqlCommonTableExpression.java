
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorCommonTableExpression
 * </h1>
 * <p>
 * Descrive un common table expression utilizzato in CREATE VIEW nella clausola
 * AS WITH common-expressions e in altri costrutti.<br>
 * <p>
 * Si tratta di una modalità per definire delle full-select riutilizzabili nello 
 * stesso comanodo
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlCommonTableExpression 
*/

public class SqlCommonTableExpression implements Serializable{

	private static final long serialVersionUID = 1L;
	
    private String tableIdentifier = null;            		// table-identifier  
    private ArrayList<String> al_column = null;       	    // table-identifier (cols)
    private SqlFullSelect fullSelect = null;      			// table-identifier (cols) AS (full-select)
    
    
	/**
	 * Costruttore vuoto
	 */
	public SqlCommonTableExpression() {
		super();
		al_column = new  ArrayList<String> ();

	}





	/**
	 * Restituisce il table-identifier del <tt> common-table-expressions</tt> <br>
	 * <p>
	 * @return the tableIdentifier
	 */
	public String getTableIdentifier() {
		return this.tableIdentifier;
	}


	/**
	 * Imposta il table-identifier del <tt> common-table-expressions</tt> <br>
	 * <p>
	 * @param al_withTableIdentifier the al_withTableIdentifier to set
	 */
	public void setTableIdentifier(String tableIdentifier) {
		this.tableIdentifier = tableIdentifier;
	}


	/**
	 * Restituisce le colonne associate al table identifier <br>
	 * e identificate da <tt>AS full-select</tt> <br>
	 * <p>
	 * @return the al_column
	 */
	public ArrayList<String> getColumns() {
		return this.al_column;
	}


	/**
	 * Imposta le colonne associate al table identifier <br>
	 * e identificate da <tt>AS full-select</tt> <br>
	 * <p>
	 * @param al_column the al_column to set
	 */
	public void setColumns(ArrayList<String> al_column) {
		this.al_column = al_column;
	}


	/**
	 * Restituisce l'istruzione full-select associata al table identifier <br>
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return fullSelect;
	}


	/**
	 * Imposta l'istruzione full-select associata al table identifier <br>
	 * <p>
	 * @param SqlFullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.fullSelect = fullSelect;
	}

	
}
