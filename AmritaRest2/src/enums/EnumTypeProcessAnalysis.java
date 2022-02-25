/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  *  Questa enumerazione elenca i possibili formati di una data.
  *  Utilizzata nella gestione generalizzata tabelle.
  * 
 */
package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumTypeErrorTracedOnDb (25)
 * </h1>
 *  <p>
 * Questa enum elenca il tipo di operazione generale attivo del processo di analisi.<br>
 * Ciò avviene a fronte dell'analisi dei programmi, nella fase di parsing, oppure nella<br>
 * individuazione delle violazioni etc.<br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 08/10/2011
 * @see Analyzer
 */
@DataBaseMappedEnumeration
public enum EnumTypeProcessAnalysis {
	
	NOT_ASSIGNED,           		// 00 Di servizio 
	
	NORMALYZING_SOURCE,				// 01 Normalizzazione sorgente prima dell'analisi
	PARSING_SOURCE,					// 02 Analisi sorgente
	POST_PARSING_OPERATION,			// 03 Operazioni successive all'analisi
	DYNAMIC_CODE_SOLVING,			// 04 Soluzione codice dinamico
	PROGRAM_SERIALIZATION,			// 05 Serializzazione programma su disco
	DB_DELETE,						// 06 Data Base Delete
	DB_UPDATE,						// 07 Data Base Update
	METRIC_COMPUTING,				// 08 Calcolo metriche (misure)
	METRIC_VIOLATION_DETECTING,		// 09 Individuazione violazioni
	GENERIC 						// 10 Processo generico di cui non è necessario il dettaglio

}
