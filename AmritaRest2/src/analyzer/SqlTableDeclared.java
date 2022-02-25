
package analyzer;

import java.io.Serializable;

/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlTableDeclared
 * </h1>
 * <p>
 * Descrive le informazioni della tabella dichiarata in statements SELECT, INSERT, DELETE, UPDATE.br>
 * <p>
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 05/Mar/2011 
 * @see SqlTable
 * @see SqlTableColumn
*/

/**
 * @author Amrita
 *
 */
public class SqlTableDeclared implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////////
    // Caratteristiche generali tabella
    //////////////////////////////////////////////////////////////////////////
    
	private String tableName = "";         // select ...  from table
	private String owner = "";             // select ...  from owner.table
	private String corrNameAS = "";        // select ...  from owner.table AS corrName
	
	/**
	 * Costruttore vuoto
	 */
	public SqlTableDeclared() {
		super();
	}


	/**
	 * Restituisce il nome della tabella 
	 * 
	 * select ... from tableName
	 * 
	 * @return the tableName
	 */
	public String getTableName() {
		return tableName;
	}


	/**
	 * Imposta il nome della tabella 
	 * 
	 * select ... from tableName
	 * 
	 * @param tableName the tableName to set
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}


	/**
	 * Restituisce l'owner della tabella 
	 * 
	 * select ... from owner.tableName
	 * 
	 * @return the owner
	 */
	public String getOwner() {
		return owner;
	}


	/**
	 * Imposta l'owner della tabella 
	 * 
	 * select ... from owner.tableName
	 * 
	 * @param owner the owner to set
	 */
	public void setOwner(String owner) {
		this.owner = owner;
	}


	/**
	 * Restituisce il correlation name della tabella 
	 * 
	 * select ... from owner.tableName AS corrName
	 * 
	 * @return the corrNameAS
	 */
	public String getCorrNameAS() {
		return corrNameAS;
	}


	/**
	 * Imposta il correlation name della tabella 
	 * 
	 * select ... from owner.tableName AS corrName
	 * 
	 * @param corrNameAS the corrNameAS to set
	 */
	public void setCorrNameAS(String corrNameAS) {
		this.corrNameAS = corrNameAS;
	}


	
	
}
