package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaSqlMappingTableColumns
 * </h1>
 * <p>
 * Questa classe identifica una condizione di errore nel processo di mapping fra l'oggetto POJO Entity
 * e la tabella Sql oppure fra i campi java definiti e le colonne della tabella Sql, 
 * attraverso {@link DataBaseEntityInterface}.<br>
 * <p>
 * Tipicamente potrebbero non essere specificate le Annotation di mapping nella classe di gestione dell'Entity
 * oppure la tabella non esiste sul database oppure un campo mappato non ha corrispondenza in nessuna
 * colonna della stabella specificatas.
 * <p>
 * Tutte le informazioni sull'esito dell'ultimo dell'accesso Sql, sono disponibili attraverso l'oggetto {@link DataBaseStatusDetailed}, recuperabil
 * con specifico metodo <b>getInfoSql() su questo oggetto</b>.

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
 * @see ExceptionAmritaSqlError

*/

public class ExceptionAmritaSqlMappingTableColumns extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaSqlMappingTableColumns() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaSqlMappingTableColumns(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}



}
