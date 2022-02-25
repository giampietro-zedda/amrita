package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaSqlConversionColumnValue
 * </h1>
 * <p>
 * Questa classe identifica una condizione di errore nel processo di conversione dei valori delle
 * colonne della tabella Sql da/a campi java dell'oggetto POJO istanza della classe di gestione Entity.
 * Tipicamente potrebbe essere stato dichiarato un campo nella tabella Sql incompatibile con il tipo
 * campo java corrispondente.
 * <p>
 * Tutte le informazioni sull'esito dell'ultimo dell'accesso Sql, sono disponibili attraverso l'oggetto {@link DataBaseStatusDetailed}, recuperabil
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
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns

 * 
*/

public class ExceptionAmritaSqlConversionColumnValue extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaSqlConversionColumnValue() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaSqlConversionColumnValue(DataBaseStatusDetailed dbsd,	EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}

}
