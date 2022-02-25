package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaSqlError
 * </h1>
 * <p>
 * Questa classe identifica una condizione fisica di errore in una operazione su una tabella sql,
 * a fronte di un'operazione su un'Entity.<br>
 * Si tratta delle operazioni previste da {@link DataBaseEntityInterface}
 * <b>create</b> <b>read</b> o <b>update</b> o <b>delete</b> o altro. <br>
 * Pur essendo state già loggate da {@link DataBaseManager}, tutte le informazioni sull'esito
 * dell'accesso Sql, sono disponibili attraverso l'oggetto {@link DataBaseStatusDetailed}, recuperabil
 * con specifico metodo <b>getInfoSql()</b> su questo oggetto.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseManager
 * @see DataBaseEntityInterface
 * @see ExceptionAmrita
 * @see ExceptionAmritaAnnotationMissing
 * @see ExceptionAmritaReflectionError
 * @see ExceptionAmritaSqlAccessDuplicate
 * @see ExceptionAmritaSqlAccessNotfound
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlMappingTableColumns

 * 
*/

public class ExceptionAmritaSqlError extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaSqlError() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaSqlError(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}


}
