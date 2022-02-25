
package analyzer;

import java.io.Serializable;

/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlTableColumnDeclared
 * </h1>
 * <p>
 * Descrive le informazioni della colonna dichiarata in statements SELECT, INSERT, DELETE, UPDATE.br>
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 05/Mar/2011 
 * @see SqlTable
 * @see SqlTableColumn
 * @see SqlTableDeclared
*/

/**
 * @author Amrita
 *
 */
public class SqlTableColumnDeclared implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Caratteristiche generali tabella
    //////////////////////////////////////////////////////////////////////////
    
	private String columnName = "";       		// select columnName from table
	private String corrNameAS = "";        		// select corrName.columnName from table
	private boolean isColumnExplicit = false;   // true  se select corrName.columnName from table
	                                            // false se select table.* from table
	
	/**
	 * Costruttore vuoto
	 */
	public SqlTableColumnDeclared() {
		super();
	}



	/**
	 * Restituisce il nome della colonna
	 * 
	 * select columnName from owner.tableName AS corrName
	 * 
	 * @return the columnName
	 */
	public String getColumnName() {
		return columnName;
	}



	/**
	 * Restituisce il nome della colonna
	 * 
	 * select columnName from owner.tableName AS corrName
	 * 
	 * @param columnName the columnName to set
	 */
	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}



	/**
	 * Restituisce true se colonna dichiarata esplicitamente false altrimenti
	 * 
	 * @return the isColumnExplicit
	 */
	public boolean isColumnExplicit() {
		return isColumnExplicit;
	}



	/**
	 * Imposta true se colonna dichiarata esplicitamente false altrimenti
	 * 
	 * @param isColumnExplicit the isColumnExplicit to set
	 */
	public void setColumnExplicit(boolean isColumnExplicit) {
		this.isColumnExplicit = isColumnExplicit;
	}



	/**
	 * Restituisce il correlation name della colonna 
	 * 
	 * select corrName.columnName from owner.tableName AS corrName
	 * 
	 * @return the corrNameAS
	 */
	public String getCorrNameAS() {
		return corrNameAS;
	}


	/**
	 * Imposta il correlation name della tabella 
	 * 
	 * select corrName.columnName from owner.tableName AS corrName
	 * 
	 * @param corrNameAS the corrNameAS to set
	 */
	public void setCorrNameAS(String corrNameAS) {
		this.corrNameAS = corrNameAS;
	}


	
	
}
