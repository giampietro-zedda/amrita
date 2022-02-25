/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda   Turin (ITALY)
*/
package exception;

/**
 * 
 * 
 * 
 * 
* @author amrita
*
*/
public class ExceptionGraphObjectDynamicInvokeError extends Exception {;
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public ExceptionGraphObjectDynamicInvokeError() {
		super("Errore di attivazione metodo su oggetto associato a arco");
	}

}
