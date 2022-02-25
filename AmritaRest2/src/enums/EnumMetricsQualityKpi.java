package enums;

/**
  * Copyright (c) 2010-2012 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsQualityQpi (T0042)	
  * </h1>
  *  <p>
  * This enumerations lists all Quality Kpi Indicators<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/03/2011
  * @see Metrics
  *  
*/
public enum EnumMetricsQualityKpi {
	
	NOT_ASSIGNED, 							// 00 Di servizio 
	KPI_PORTABILITY, 						// 01 SQALE
	KPI_MAINTENABILITY,						// 02 SQALE
	KPI_SECURITY,      						// 03 SQALE
	KPI_EFFICIENCY,    						// 04 SQALE
	KPI_CHANGEABILITY, 						// 05 SQALE
	KPI_RELIABILITY,   						// 06 SQALE
	KPI_TESTABILITY,   						// 07 SQALE
	KPI_SOURCE_SIZE,        				// 08 Misure dimensionali sorgenti
	KPI_DEVELOPMEN_TIME,                	// 09 Misure definizione dati
	KPI_DOCUMENTATION,        				// 10 Misure di documentazione
	KPI_DYNAMIC_CODE,        				// 11 Misure dimensionali sorgenti
	KPI_DEAD_CODE,        					// 12 Misure di codice morto
	KPI_COMPLEXITY_STRUCTURAL,      		// 13 Misure di complessità strutturale
	KPI_COMPLEXITY_FUNCTIONAL,      		// 14 Misure di complessità funzionale generiche
	KPI_COMPLEXITY_FUNCTION_POINT,  		// 15 Misure di complessità funzionale function point
	KPI_COMPLEXITY_REHOSTING,				// 16 Misure di complessità funzionale per rehosting
	KPI_COMPLEXITY_MCABE,          	    	// 17 Misure di complessità ciclomatica
	KPI_COMPLEXITY_HALSTEAD,        		// 18 Misure di complessità secondo Halstead
	KPI_SQALE_VIOLATION_COUNTERS,       	// 19 Sqale, contatori violazioni per categorie
	KPI_SQALE_GENERAL,    					// 20 Sqale, valori generali
	KPI_SQALE_DETAIL_INDEXES_ABSOLUTE,   	// 21 Sqale, valori di dettaglio indice qualità assoluto SQI, SQxI
	KPI_SQALE_DETAIL_INDEXES_CONSOLIDATED,	// 22 Sqale, valori di dettaglio indici consolidati SCTx
	KPI_SQALE_DETAIL_INDEXES_DENSITY,    	// 23 Sqale, valori di dettaglio indici di densita SDxI
	KPI_SQALE_DETAIL_INDEXES_RATING_SRXI,   // 24 Sqale, valori di dettaglio indici squale rating SRxI 
	KPI_SQALE_DETAIL_INDEXES_RATING_SRXL,   // 25 Sqale, valori di dettaglio livelli squale rating SRxL 
}