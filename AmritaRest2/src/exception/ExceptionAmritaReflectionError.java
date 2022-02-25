package exception;

import utilities.ReflectionManager;
import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaReflectionError
 * </h1>
 * <p>
 * Questa classe identifica una condizione di errore avvenuta durante l'esecuzione di una
 * operazione di java.reflection, effettuata atttraverso un'istanza della classe {@link ReflectionManager}.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseManager
 * @see DataBaseEntityInterface
 * @see ReflectionManager
 * @see ExceptionAmrita
 * @see ExceptionAmritaAnnotationMissing
 * @see ExceptionAmritaSqlAccessDuplicate
 * @see ExceptionAmritaSqlAccessNotfound
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns
 * 
*/

public class ExceptionAmritaReflectionError extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaReflectionError() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaReflectionError(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}


}
