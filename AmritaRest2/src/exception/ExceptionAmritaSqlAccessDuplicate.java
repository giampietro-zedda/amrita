package exception;

import utilities.ReflectionManager;
import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaSqlAccessDuplicate
 * </h1>
 * <p>
 * Questa classe identifica una condizione logica di riga già presente nella tabella sql trovata a fronte di
 * un'operazione <b>create</b> su un'Entity attraverso {@link DataBaseEntityInterface}.<br>
 * <p>
 * Pur essendo state già loggate da {@link DataBaseManager}, tutte le informazioni sull'esito
 * dell'accesso Sql, sono disponibili attraverso l'oggetto {@link DataBaseStatusDetailed}, recuperabile
 * con specifico metodo <b>getInfoSql()</b> su questo oggetto.

 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseManager
 * @see DataBaseEntityInterface
 * @see ReflectionManager
 * @see ExceptionAmrita
 * @see ExceptionAmritaAnnotationMissing
 * @see ExceptionAmritaReflectionError
 * @see ExceptionAmritaSqlAccessNotfound
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns

 * 
*/

public class ExceptionAmritaSqlAccessDuplicate extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaSqlAccessDuplicate() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaSqlAccessDuplicate(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}

}
