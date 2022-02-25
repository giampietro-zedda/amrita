
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlTableColumnDefined
 * </h1>
 * <p>
 * Contenitore per le informazioni della tabella e colonne dichiarata in statements SELECT, INSERT, DELETE, UPDATE.br>
 * <p>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 05/Mar/2011 
 * @see SqlTable
 * @see SqlTableColumn
 * @see SqlTableColumnDefined
 * @see SqlTableDeclared
*/

/**
 * @author Amrita
 *
 */
public class SqlTableColumnDeclaredComplete implements Serializable{

	private static final long serialVersionUID = 1L;
	    
	private SqlTableDeclared tableDeclared = null;       		 
	private ArrayList<SqlTableColumnDeclared> columnsDeclared  = null;                                         
	
	/**
	 * Costruttore vuoto
	 */
	public SqlTableColumnDeclaredComplete() {
		super();
		tableDeclared = new SqlTableDeclared ();
		columnsDeclared = new ArrayList<SqlTableColumnDeclared>();
	}

	/**
	 * Restituisce descrittore tabella dichiarata con nome, owner e correlation name
	 * <p>
	 * @return the tableDeclared
	 */
	public SqlTableDeclared getTableDeclared() {
		return tableDeclared;
	}

	/**
	 * Imposta descrittore tabella dichiarata con nome, owner e correlation name
	 * <p>
	 * @param tableDeclared the tableDeclared to set
	 */
	public void setTableDeclared(SqlTableDeclared tableDeclared) {
		this.tableDeclared = tableDeclared;
	}

	/**
	 * Restituisce arrayList descrittore colonne dichiarata con nome colonna, correlation name
	 * e flag dichiarazione esplicita/implicita
	 * <p>
	 * @return the columnsDeclared
	 */
	public ArrayList<SqlTableColumnDeclared> getColumnsDeclared() {
		return columnsDeclared;
	}

	/**
	 * Imposta arrayList descrittore colonne dichiarata con nome colonna, correlation name
	 * e flag dichiarazione esplicita/implicita
	 * <p>
	 * @param columnsDeclared the columnsDeclared to set
	 */
	public void setColumnsDeclared(ArrayList<SqlTableColumnDeclared> columnsDeclared) {
		this.columnsDeclared = columnsDeclared;
	}

	

	
}
