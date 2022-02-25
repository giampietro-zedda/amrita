package enums;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsQualitySections (T0042)	
  * </h1>
  *  <p>
  * This enumerations lists all quality group information for about the metrics.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/03/2011
  * @see Metrics
  *  
*/
public enum EnumMetricsQualitySections {
	
	NOT_ASSIGNED, 						// 00 Di servizio             
	COUNTERS_GENERAL,         			// 01 Contatori generali oggetti analizzati
	DIMENSION_SOURCES,        			// 02 Misure dimensionali sorgenti
	DATA_DEFINITION,        			// 03 Misure definizione dati
	DEVELOPMENT_TIME_VALUED,    		// 04 Tempi stimati di sviluppo 
	DOCUMENTATION,        				// 05 Misure di documentazione
	DYNAMIC_CODE,        				// 06 Misure dimensionali sorgenti
	VIOLATIONS,        					// 07 Misure violazioni
	DEAD_CODE,        					// 08 Misure di codice morto
	JCL,        						// 09 Misure jcl
	COMPLEXITY_STRUCTURAL,      		// 10 Misure di complessità strutturale
	COMPLEXITY_FUNCTIONAL,      		// 11 Misure di complessità funzionale generiche
	COMPLEXITY_FUNCTION_POINT,  		// 12 Misure di complessità funzionale function point
	COMPLEXITY_FUNCTION_REHOSTING,		// 13 Misure di complessità funzionale per rehosting
	COMPLEXITY_CICLOMATIC,        		// 14 Misure di complessità ciclomatica
	COMPLEXITY_HALSTEAD,        		// 15 Misure di complessità secondo Halstead
	COMPLEXITY_INDEXES_AVG,        		// 16 Indici di complessità/manutenibilità medi
	COMPLEXITY_INDEXES_MIN,        		// 17 Indici di complessità/manutenibilità minimi
	COMPLEXITY_INDEXES_MAX,        		// 18 Indici di complessità/manutenibilità massimi
	COMPLEXITY_INDEXES_TOT,        		// 19 Indici di complessità/manutenibilità totali
	SQALE_VIOLATION_BY_CATEGORY,    	// 20 Sqale, contatori violazioni per categorie
	SQALE_GENERAL,    					// 21 Sqale, valori generali
	SQALE_DETAIL_INDEXES_ABSOLUTE,   	// 22 Sqale, valori di dettaglio indice qualità assoluto SQI, SQxI
	SQALE_DETAIL_INDEXES_CONSOLIDATED,	// 23 Sqale, valori di dettaglio indici consolidati SCTx
	SQALE_DETAIL_INDEXES_DENSITY,    	// 24 Sqale, valori di dettaglio indici di densita SDxI
	SQALE_DETAIL_INDEXES_RATING_SRXI,   // 25 Sqale, valori di dettaglio indici squale rating SRxI 
	SQALE_DETAIL_INDEXES_RATING_SRXL,   // 26 Sqale, valori di dettaglio livelli squale rating SRxL 
}