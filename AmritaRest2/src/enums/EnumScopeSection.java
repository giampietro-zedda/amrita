package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumScopeSection 
 * </h1>
 *  <p>
 * Questa enum elenca le possibili sezioni di uno scope, che identifica in generale un insieme di programmi,<br>
 * e quindi di oggetti.  Le sezioni di scope da attivare sono codificate nella tabella  ScopeSection.<br>
 * Possono esserci, per uno scope, più sezioni definite dello stesso tipo. Per esempio più sezioni<br>
 * che indicano match di caratteri sul nome sorgente.  <br>
 * 
 * <br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/02/2010
 * @see Analyzer
 * @see EntityScopeHeader
 * @see EntityScopeChild
 * @see EntityScopeSection
 * @see EntityScopeProgram
*/
@DataBaseMappedEnumeration
public enum EnumScopeSection {
	
	NOT_ASSIGNED,					// 00 
	
	SECTION_PROGRAM_SELECTION,      // 01 Selezione su nomi programmi sorgente e data di analisi
	SECTION_PROGRAM_OPTION,        	// 02 Selezione sulle opzioni (caratteristiche) del programma
	SECTION_COPY,        	        // 03 Selezione su nomi copy presenti nel programma sorgente
	SECTION_COPY_ITEM,        	    // 04 Selezione su nomi copy presenti nel programma sorgente che utilizzano l'item in Input o Output
	SECTION_IO_GENERAL,     	    // 05 Selezione su accesso a Db2/Sql/Adabas/Vsam/Screen
	SECTION_ENTITY_ACCESS,  		// 06 Selezione su accesso a Entity (db2, Adabas, dl1, vsam)
	SECTION_ENTITY_ACCESS_ITEM,  	// 07 Selezione su accesso a Entity item in input o output
	SECTION_PHISICAL_FILE_ACCESS,  	// 08 Selezione su accesso a file seq 
	SECTION_INTERNAL_FILE,       	// 09 Selezione su relazione a file interno (definito da FD)
	SECTION_EXTERNAL_FILE,       	// 10 Selezione su relazione a file esterno (DD name, Select .. assign))
	SECTION_OBJECT_RELATED,        	// 11 Selezione su nomi oggetti relazionati ai sorgenti
	SECTION_COBOL_FIELD,          	// 12 Selezione su nomi campi definiti, opzioni e valore opzioni
	SECTION_COBOL_LABEL,   	        // 13 Selezione su label Cobol 
	SECTION_COBOL_SECTION,   	    // 14 Selezione su section Cobol 
	SECTION_COBOL_INSTR,   	        // 15 Selezione su istruzioni Cobol 
	SECTION_CICS_INSTR,   		    // 16 Selezione su istruzioni Cics
	SECTION_CICS_INSTR_OPTION,      // 17 Selezione su operandi e opzioni Cics
	SECTION_PAR_OPEN,             	// 18 Di servizio, parentesi aperta
	SECTION_PAR_CLOSE,             	// 19 Di servizio, parentesi chiusa
	SECTION_AND,           	        // 20 Di servizio, mette in AND la sezione corrente con la successiva
	SECTION_OR            	        // 21 Di servizio, mette in OR la sezione corrente con la successiva

}
