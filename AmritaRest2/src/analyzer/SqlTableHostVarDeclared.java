
package analyzer;

import java.io.Serializable;

/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlTableColumnDeclared
 * </h1>
 * <p>
 * Descrive le informazioni della variabile host dichiarata in statements SELECT, INSERT, DELETE, UPDATE.br>
 * <p>
 * Indica semplicemente il nome della colonna a cui la variabile host si riferisce.
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 30/Apr/2021 
 * @see SqlTable
 * @see SqlTableColumn
 * @see SqlTableDeclared
*/

/**
 * @author Amrita
 *
 */
public class SqlTableHostVarDeclared implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Caratteristiche generali tabella
    //////////////////////////////////////////////////////////////////////////
    
	private String hostVarName = "";            // 
	private String tableName = "";              // 
	private String columnName = "";       		// select columnName from table
	
	/**
	 * Costruttore vuoto
	 */
	public SqlTableHostVarDeclared() {
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
	 * Restituisce il nome della variabile host
	 * 
	 * @return the hostVarName
	 */
	public String getHostVarName() {
		return hostVarName;
	}



	/**
	 * Imposta il nome della variabile host
	 * 
	 * @param hostVarName the hostVarName to set
	 */
	public void setHostVarName(String hostVarName) {
		this.hostVarName = hostVarName;
	}



	/**
	 * Restituisce il nome della tabella
	 * 
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}



	/**
	 * Imposta il nome della tabella
	 * 
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}


	
}
