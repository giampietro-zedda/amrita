package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumLogicSetMode
  * </h1>
  *  <p>
  * Questa enum elenca le modalità di assegnazione nelle logiche di trasformazione
  * dei campi e sottocampi elementari. L'enumerazione è generale e contiene gli entry
  * per tutti i linguaggi.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/03/2010
  * @see Analyzer
  * @see EnumLogicSetType
*/
@DataBaseMappedEnumeration
public enum EnumLogicSetMode {
 
	NOT_ASSIGNED,        								// 00 Di servizio 
	
	////////////////////////////////////////////////////////
	//  Specifici Cobol                                   //
	////////////////////////////////////////////////////////
	
	// Assegnazioni intermedie valori sottocampo (possono essere anche EIBTRMID, EIBTRNID, etc)
	ASSIGN_BY_COBOL_MOVE_FIELD,         				// 01 MOVE Field/Grp       To Field/Group/Redefines servizio
	ASSIGN_BY_COBOL_MOVE_GROUP,          				// 02 Move GrpInput        To GField/Group/Redefines
	ASSIGN_BY_COBOL_MOVE_BEYOND_DISCARDED,              // 03 Move field           To field(pos:lng)   oltre area corrente receiver
	ASSIGN_BY_COBOL_MOVE_ACROSS_DISCARDED,              // 04 Move field           To field(pos:lng)   non dalla prima posizione del receiver
	ASSIGN_BY_COBOL_MOVE_REF_MOD_DISCARDED,             // 05 Move field           To field(pos:lng)   con pos o lng di reference modification NON literal

	// Campo risultato definito in Using, Linkage o ioarea valorizzata da Read, con o senza trasformazioni (MOVE)
	LAST_SET_BY_COBOL_USING_PARM,     					// 06 Move FieldInUsing    To Field/Group/Redefines test
	LAST_SET_BY_COBOL_LINKAGE,        					// 07 Move FieldInLinkage  con campo di linkage e SET pointer TO ADDRESS OF lnk-area 
	LAST_SET_BY_COBOL_READ,           	 		    	// 08 Move FieldInIoareaFile  To Field/Group/Redefines  

	// Campo risultato valorizzato esplicitamente da Move come ultima assegnazione
	LAST_SET_BY_COBOL_MOVE_SPACES,            			// 09 Move SPACE           To field		 
	LAST_SET_BY_COBOL_MOVE_SPACES_IMPLICIT,            	// 10 Move field           To Field/Group/Redefines	        
	LAST_SET_BY_COBOL_MOVE_ZEROES,            			// 11 Move ZERO|ZEROES     To Field/Group/RedefiField/Group/Redefines 
	LAST_SET_BY_COBOL_MOVE_LITERAL_ALPHA,          		// 12 Move 'xx'            To Field/Group/Redefines test 	 
	LAST_SET_BY_COBOL_MOVE_LITERAL_NUM,          		// 13 Move num             To Field/Group/Redefines test 	 
	LAST_SET_BY_COBOL_MOVE_TBITEM_OF_OCCURS,      		// 14 Move FieldInOccurs   To Field/Group/Redefines test 

	// Campo risultato valorizzato esplicitamente da default value senza trasformazioni (Move)
	LAST_SET_BY_COBOL_VALUE_SPACES,            			// 15 Move SPACE/ZERO      To Field/Group/Redefines 		o Campo con VALUE Clause SPACE 	  
	LAST_SET_BY_COBOL_VALUE_ZEROES,            			// 16 Move SPACE/ZERO      To Field/Group/Redefines 		o Campo con VALUE Clause SPACE 	  
	LAST_SET_BY_COBOL_VALUE_LITERAL_ALPHA,          	// 17                                                         Campo con VALUE Clause 'xx' 	  
	LAST_SET_BY_COBOL_VALUE_LITERAL_NUM,          		// 18 Move num             To Field/Group/Redefines      	o Campo con VALUE Clause Numeric  
	LAST_SET_BY_COBOL_VALUE_HIGH_VALUE,             	// 19 Move HIGH VALUE      To Field/Group/Redefines 		o Campo con VALUE Clause HIGH-VALUE
	LAST_SET_BY_COBOL_VALUE_LOW_VALUE,             		// 20 Move LOW VALUE       To Field/Group/Redefines 		o Campo con VALUE Clause LOW-VALUE
	
	////////////////////////////////////////////////////////
	//  Validi per precompilatori Cics/Sql/Dl1       
	////////////////////////////////////////////////////////
	
	// Assegnati da campi Cics o campi di strutture dati Cics direttamente o senza trasformazioni (MOVE)
	LAST_SET_BY_CICS_RECEIVE_MAP,          	 	        // 21 Campo di mappa video Cics
	LAST_SET_BY_CICS_READ_VSAM,          	 	        // 22 Campo di Vsam acceduto da Cics
	LAST_SET_BY_CICS_READ_TS_QUEUE,          	 		// 23 Campo di Cics TS Queue
	LAST_SET_BY_CICS_READ_TD_QUEUE,             		// 24 Campo di Cics TD Queue
	LAST_SET_BY_CICS_DFHCOMMAREA,        				// 25 Campo in COMMAREA
	LAST_SET_BY_CICS_TWA,                			    // 26 TWA
	LAST_SET_BY_CICS_CSA,                			    // 27 CSA
	LAST_SET_BY_CICS_EIBTRMID,             				// 28 EIBTRMID
	LAST_SET_BY_CICS_EIBTRNID,             				// 29 EIBTRNID
	LAST_SET_BY_CICS_EIBRPOSN,             				// 30 EIBRPOSN
	LAST_SET_BY_CICS_EIBCPOSN,             				// 31 EIBCPOSN
	LAST_SET_BY_CICS_EIBCALEN,             				// 32 EIBCALEN

	// Assegnati da strutture dati esterne o database
	LAST_SET_BY_SQL_SELECT,                 			// 33 Campo di Sql Table
	LAST_SET_BY_FILE_GET,                 			    // 34 Campo di archivio Vsam

}
