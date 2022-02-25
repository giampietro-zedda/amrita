
package analyzer;
import java.io.Serializable;

import enums.EnumIndexOrder;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorPartition
 * </h1>
 * <p>
 * Descrive tutte le informazioni di una singola partizione di un indice o di una tabella Sql, a fronte di uno statement <br>
 * CREATE TABLE o CREATE INDEX.br>
 * <p>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/lug/2011 
 * @see SqlDescriptorTable
 * @see SqlDescriptorTableColumn
 * @see SqlDescriptorIndex
 * @see SqlDescriptorIndexColumn
 *
*/

/**
 */
public class SqlPartitionColumn implements Serializable {

 	private static final long serialVersionUID = 1L;
 	
	private String column = ""; 							   // Nome colonna indice/tabella
    private EnumIndexOrder order = null;                       // Tipo ordinamento |ASC|DESC|RANDOM
    private String keyExpression = "";                         // Stringa key expression in alternativa a order type per indici
    private boolean isNullLast = false;                        // column-name NULL LAST ASC|DESC|RANDOM

	
	/**
	 * Costruttore 
	 */
	public SqlPartitionColumn() {
		super();
		order = EnumIndexOrder.ORDER_ASCENDING;
	}


	/**
	 * Restituisce il nome della colonna della tabella o dell'indice, a cui la partizione si riferisce.<br>
	 * <p>
	 * @return the column
	 */
	public String getColumn() {
		return column;
	}


	/**
	 * Imposta il nome della colonna della tabella o dell'indice, a cui la partizione si riferisce.<br>
	 * <p>
	 * @param column the column to set
	 */
	public void setColumn(String column) {
		this.column = column;
	}


	/**
	 * Restituisce l'odinamento ASC|DESC|RANDOM della colonna della tabella o dell'indice, a cui la partizione si riferisce.<br>
	 * <p>
	 * @return the order
	 */
	public EnumIndexOrder getOrder() {
		return order;
	}


	/**
	 * Imposta l'odinamento ASC|DESC|RANDOM della colonna della tabella o dell'indice, a cui la partizione si riferisce.<br>
	 * <p>
	 * @param order the order to set
	 */
	public void setOrder(EnumIndexOrder order) {
		this.order = order;
	}


	/**
	 * Restituisce l'espressione chiave in alternativa a order type per indici.<br>
	 * <p>
	 * @return the keyExpression
	 */
	public String getKeyExpression() {
		return keyExpression;
	}


	/**
	 * Imposta l'espressione chiave in alternativa a order type per indici.<br>
	 * <p>
	 * @param keyExpression the keyExpression to set
	 */
	public void setKeyExpression(String keyExpression) {
		this.keyExpression = keyExpression;
	}


	/**
	 * Restituisce se presente il parametro NULL LAST a livello della colonna della partizione..<br>
	 * <p>
	 * @return the isNullLast
	 */
	public boolean isNullLast() {
		return isNullLast;
	}


	/**
	 * Imposta se presente il parametro NULL LAST a livello della colonna della partizione..<br>
	 * <p>
	 * @param isNullLast the isNullLast to set
	 */
	public void setNullLast(boolean isNullLast) {
		this.isNullLast = isNullLast;
	}


}
