package enums;

import analyzer.UserExit;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumUserExit  
 * </h1>
 *  <p>
 * Questa enum elenca le possibili operazioni previste per la user exit applicativa {@link UserExit}.
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 07/04/2010
 * @see UserExit
  *  
*/
public enum EnumUserExit {

	NOT_ASSIGNED,			  				// 00 Di servizio
	
	GET_SYSTEM_OWNER,     					// 01 Restituzione sistema	proprietario  
	GET_SUB_SYSTEM_OWNER,     			    // 02 Restituzione sotosistema proprietario	  
	GET_SYSTEM_SUB_SYSTEM_OWNER,     	    // 03 Restituzione sistema e sottosistema in analisi di pgm sorgenti/jcl da nome source
	FILTER_ON_OBJECT_NAME_EVALUATE,     	// 04 Valutazione filtro su nome sorgente fornito
	CTRL_NAME_FIELD,     		    		// 09 Controllo validità nome campo 	     (prefisso, suffisso etc.)
	CTRL_NAME_LABEL,     		    		// 10 Controllo validità nome label 	     (prefisso, suffisso etc.)
	CTRL_NAME_PARAGRAPH,     		    	// 11 Controllo validità nome paragrafo      (prefisso, suffisso etc.)
	CTRL_NAME_PARAGRAPH_THRU,     		    // 12 Controllo validità nome paragrafo thru (prefisso, suffisso etc.)
	CTRL_NAME_SECTION,     		    		// 13 Controllo validità nome section        (prefisso, suffisso etc.)
	CTRL_NAME_SECTION_THRU,     		    // 14 Controllo validità nome section thru   (prefisso, suffisso etc.)
	CTRL_NAME_PGM,     		    		    // 15 Controllo validità nome programma      (prefisso, suffisso etc.)
}
