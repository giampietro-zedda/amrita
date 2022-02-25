package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaSqlAccessNotfound
 * </h1>
 * <p>
 * Questa classe identifica una condizione logica di riga non trovata a fronte di
 * un'operazione <b>read</b> oppure <b>update</b> oppure <b>delete</b> 
 * su un'Entity attraverso {@link DataBaseEntityInterface}.<br>
 * <p>
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
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns
 * 
*/

public class ExceptionAmritaSqlAccessNotfound extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaSqlAccessNotfound() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaSqlAccessNotfound(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}

}
