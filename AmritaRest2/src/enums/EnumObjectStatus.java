package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumObjectStatus (3)
  * </h1>
  *  <p>
  * Questa enum elenca i possibili stati di un generico oggetto, descritoo da {@link EnumObject}
  * tipologie di oggettigestiti.<br>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumObject
  * @see EnumObjectOption
  * 
*/
@DataBaseMappedEnumeration
public enum EnumObjectStatus {
	
	NOT_ASSIGNED,								// 00 
	
	SOURCE_MEMBER_ACQUIRED,           			// 01 Sorgente acquisito nella libreria di pertinenza
	SOURCE_MEMBER_TYPE_DETECTED,            	// 02 Sorgente identificato come tipologia
	SOURCE_MEMBER_TYPE_NOT_DETECTED,        	// 03 Sorgente NON identificato come tipologia
	OBJECT_ANALYZED_WITH_NO_ERRORS,  	    	// 04 Oggetto a fronte di analisi sorgente senza errori
	OBJECT_ANALYZED_WITH_ERRORS,     			// 05 Oggetto a fronte di analisi sorgente con   errori
	OBJECT_ANALYZED_WITH_EXCEPTION,         	// 06 Oggetto a fronte di analisi sorgente in exception
	OBJECT_PROCESSED_WITH_NO_ERRORS,        	// 07 Oggetto a fronte di processo senza errori
	OBJECT_PROCESSED_WITH_ERRORS,          		// 08 Oggetto a fronte di processo con errori
	OBJECT_PROCESSED_WITH_EXCEPTION,       		// 09 Oggetto a fronte di processo in exception
	OBJECT_TO_BE_ANALYZED,           			// 10 Oggetto ancora da analizzare, a fronte di analisi di altri oggetti
	OBJECT_NOT_TO_BE_ANALYZED,              	// 11 Oggetto al quale non corrisponde un sorgente da analizzare
	OBJECT_TO_BE_ACQUIRED,              		// 12 Oggetto da acquisire con library scan (tipicamente un programma)
	OBJECT_ANALYZED_DYNAMIC_SOLVED,         	// 13 Oggetto con codice dinamico locale e/o spreaded risolto
	OBJECT_ANALYZED_DYNAMIC_SPREAD_TO_SOLVE, 	// 14 Oggetto con codice dinamico spreaded da risolvere
	OBJECT_ANALYZED_DYNAMIC_WAITING_FOR_DATA,  	// 15 Oggetto con codice dinamicoin attesa di dati esterni 
	OBJECT_ANALYZED_DYNAMIC,  					// 16 Oggetto con codice dinamico da risolvere/risolto/spreaded/local
	OBJECT_NOT_ACTIVE          			    	// 17 Oggetto non + attivo, da considerare come non esistente in analisi e processi
} 
