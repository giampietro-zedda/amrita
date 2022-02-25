package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumMetricsScope	
  * </h1>
  *  <p>
  * Questa enum elenca i possibili confini, o campi di validità delle metriche.<br>
  * <p>
  * Il livello di dettaglio massimo è dato dalle sezioni di programma (Section Cobol).<br>
  * Il livello di dettaglio minimo include tutti gli oggetti, di tutti i sottosistemi di, <br>
  * tutti i sistemi.<br>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 02/03/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsScope {
	
	NOT_ASSIGNED, 				// (00) Di servizio             
	SCOPE_LEVEL_OBJECT,         // (01) Metriche a livello di oggetto di analisi (PROGRAM, SCRIPT_SQL etc.) 
	SCOPE_LEVEL_SECTION,        // (02) Metriche a livello di singola sezione oggetto di analisi, come section di programma
	SCOPE_LEVEL_SUBSYSTEM,      // (03) Metriche a livello di sottosistema applicativo (in un sistema)  
	SCOPE_LEVEL_SYSTEM,         // (04) Metriche a livello di sistema applicativo   
	SCOPE_LEVEL_GLOBAL;         // (05) Metriche a livello globale per tutti i sistemi e sottosistemi  
}