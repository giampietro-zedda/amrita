
package analyzer;
import java.io.Serializable;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlCastSpecification
 * </h1>
 * <p>
 * Viene modellata la funzione di <tt>CAST</tt> che assume la forma:<br>
 * <p>
 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 01/lug/2011 
 * @see SqlDescriptorTable
 * @see SqlDataType
 *
*/

public class SqlCastSpecification implements Serializable {

 	private static final long serialVersionUID = 1L;

 	// Variabili di istanza
	private SqlExpression expression = null; 			// Espressione codificata
    private String parameterMarker = "";                // Stringa con delimitatore di parametro
	private boolean isExpression = false;               // true indica CAST ( expression ) AS data-type
	private boolean isNull = false;               		// true indica CAST ( NULL ) AS data-type
    private SqlDataType asDataType = null;              // Descrittore data type
	
	
	/**
	 * Costruttore 
	 */
	public SqlCastSpecification() {
		super();
	}


	/**
	 * Restituisce l'espressione di cast.<br>
	 * <p>
	 * <tt>CAST ( expression ) AS data-type<br>
	 * <p>
	 * @return the expression
	 */
	public SqlExpression getExpression() {
		return expression;
	}


	/**
	 * Imposta l'espressione di cast.<br>
	 * <p>
	 * <tt>CAST ( expression ) AS data-type<br>
	 * <p>
	 * @param expression the expression to set
	 */
	public void setExpression(SqlExpression expression) {
		this.expression = expression;
	}


	/**
	 * Restituisce il parameter marker.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @return the parameterMarker
	 */
	public String getParameterMarker() {
		return parameterMarker;
	}


	/**
	 * Imposta il parameter marker.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @param parameterMarker the parameterMarker to set
	 */
	public void setParameterMarker(String parameterMarker) {
		this.parameterMarker = parameterMarker;
	}


	/**
	 * Restituisce se presente un'espressione su cui effettuare il cast.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @return the isExpression
	 */
	public boolean isExpression() {
		return isExpression;
	}


	/**
	 * Imposta se presente un'espressione su cui effettuare il cast.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @param isExpression the isExpression to set
	 */
	public void setExpression(boolean isExpression) {
		this.isExpression = isExpression;
	}


	/**
	 * Restituisce se presente NULL su cui effettuare il cast.<br>
	 * <p>
	 * <tt>CAST ( NULL ) AS data-type<br>
	 * <p>
	 * @return the isNull
	 */
	public boolean isNull() {
		return isNull;
	}


	/**
	 * Imposta se presente NULL su cui effettuare il cast.<br>
	 * <p>
	 * <tt>CAST ( NULL ) AS data-type<br>
	 * <p>
	 * @param isNull the isNull to set
	 */
	public void setNull(boolean isNull) {
		this.isNull = isNull;
	}


	/**
	 * Restituisce il descrittore del data type codificato dopo <tt>AS</tt>.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @return the asDataType
	 */
	public SqlDataType getAsDataType() {
		return asDataType;
	}


	/**
	 * Imposta il descrittore del data type codificato dopo <tt>AS</tt>.<br>
	 * <p>
	 * <tt>CAST ( expression|NULL|parm-marker ) AS data-type<br>
	 * <p>
	 * @param asDataType the asDataType to set
	 */
	public void setAsDataType(SqlDataType asDataType) {
		this.asDataType = asDataType;
	}


	
}
