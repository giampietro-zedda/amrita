
package analyzer;
import java.io.Serializable;

import enums.EnumIndexOrder;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDescriptorIndexColumn
 * </h1>
 * <p>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/lug/2011 
 * @see SqlDescriptorTable
 *
*/

/**
 * @author Amrita
 *
 */
public class SqlIndexColumn implements Serializable {

 	private static final long serialVersionUID = 1L;

 	// Variabili di istanza
	private String column= ""; 								   // Nome colonna indice
    private EnumIndexOrder order = null;                       // Tipo ordinamento |ASC|DESC|RANDOM
    private String keyExpression = "";                         // Stringa key expression in alternativa a order type
    
	
    
	/**
	 * Costruttore 
	 */
	public SqlIndexColumn() {
		super();
		order = EnumIndexOrder.NOT_ASSIGNED;
	}

	/**
	 * Restituisce il nome della colonna dell'indice.<br>
	 * <p>
	 * 
	 * @return the column
	 */
	public String getColumnName() {
		return column;
	}

	/**
	 * Imposta il nome della colonna dell'indice.<br>
	 * <p>
	 * @param column the column to set
	 */
	public void setColumnName(String column) {
		this.column = column;
	}

	/**
	 * Restituisce il tipo di ordinamento della colonna:<br>
	 * <p>
	 * @return the orderType
	 */
	public EnumIndexOrder getOrder() {
		return order;
	}

	/**
	 * Imposta il tipo di ordinamento della colonna:<br>
	 * <p>
 	 * @param EnumIndexOrder order the order to set
	 */
	public void setOrderType(EnumIndexOrder order) {
		this.order = order;
	}

	/**
	 * Restituisce una stringa con l'espressione chiave
	 * in alternativa al tipo ordinamento<br>
	 * <p>
 	 * @return the keyExpression
	 */
	public String getKeyExpression() {
		return keyExpression;
	}

	/**
	 * Imposta una stringa con l'espressione chiave
	 * in alternativa al tipo ordinamento<br>
	 * <p>
	 * @param keyExpression the keyExpression to set
	 */
	public void setKeyExpression(String keyExpression) {
		this.keyExpression = keyExpression;
	}


	
}
