package exception;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import analyzer.DataBaseStatusDetailed;
import enums.EnumAmritaExceptionError;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda Turin (ITALY)
 * <h1>
 * ExceptionAmritaAnnotationMissing
 * </h1>
 * <p>
 * Questa eccezione viene lanciata quando l'oggetto POJO Entity utilizzato attraverso {@link DataBaseEntityInterface}
 * non ha indicate a livello di classe le annotazioni di mapping di tabella e colonne Sql
 * {@link DataBaseMappedTable} e {@link DataBaseMappedColumns}.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/03/2010
 * @see DataBaseManager
 * @see DataBaseEntityInterface
 * @see ExceptionAmrita
 * @see ExceptionAmritaReflectionError
 * @see ExceptionAmritaSqlAccessDuplicate
 * @see ExceptionAmritaSqlAccessNotfound
 * @see ExceptionAmritaSqlConversionColumnsValue
 * @see ExceptionAmritaSqlError
 * @see ExceptionAmritaSqlMappingTableColumns
 * 
*/

public class ExceptionAmritaAnnotationMissing extends ExceptionAmrita {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Costruttore vuoto
	 */
	public ExceptionAmritaAnnotationMissing() {
		super();
	}

	/**
	 * Costruttore con tutte le informazioni gestite
	 */
	public ExceptionAmritaAnnotationMissing(DataBaseStatusDetailed dbsd, EnumAmritaExceptionError qualifiedError, Exception excpOrigin) {
		super(dbsd, qualifiedError, excpOrigin);
	}
}
