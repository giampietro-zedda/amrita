package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumInstrDataCategory
  * </h1>
  *  <p>
  * Questa enum elenca le possibili categorie di oggetti, ovvero di definizioni,
  * codicabili nei programmi, copy, jcl etc., in termini di istruzioni, definizioni dati erc.<br>
  * Nel caso di JCL sono codificate le istruzioni specifiche.<br>
  * Questa Enum è qualificata dal prefisso per linguaggio.<br>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/04/2010
  * @see Analyzer
*/
@DataBaseMappedEnumeration
public enum EnumInstrDataCategory {
	    
	NOT_ASSIGNED,        				                                            // 000 Di servizio 
	
    // Instruction types
	COBOL_INSTRUCTION,               	                                            // 001 Istruzione generica Cobol, di qualsiasi divisione
	COBOL_PROC_INSTRUCTION,                                                         // 002 Istruzione di procedure division
	COBOL_PROC_INSTRUCTION_EXCEPTION,                                               // 003 Istruzione di eccezione dal flusso normale (es.Cobol ON SIZE ERROR)
	COBOL_PROC_PROC_INTERNAL,        	                                            // 004 Procedura del linguaggio come Section Cobol
	COBOL_PROC_LABEL,                                                               // 005 Label del linguaggio  
	COBOL_NOT_PROC,                                                                 // 006 Istruzioni non di procedure division ma di Identification, Environment e Data Division.
	COBOL_COPY_INSTRUCTION,                                                         // 007 Statement di inclusione di un altro sorgente 
	COBOL_COMPILER_DIRECTIVE,                                                       // 008 Direttiva Cobol per il compilatore
	
	// Cobol program areas
	COBOL_SOURCE_IDENTIFICATION,                                                    // 009 Istruzione di identificazione del sorgente
	COBOL_STRUCTURE_DEFINITION,                                                     // 010 Istruzione per definizione di un file, come FD record-name del Cobol
	COBOL_SECTION_PROGRAM_DELIMITER,                                                // 011 Istruzione di delimitatore sezione di programma come Environment Division, File Section, ..
	COBOL_ENVIRONMENT_DEFINITION,                                                   // 012 Istruzione di interazione con l'esterno, come SELECT file-name del Cobol
	 
	// Cobol symbol type
	COBOL_DATA_ITEM,                                                                // 013 Definizione specifica di un campo dati, nel formato del linguaggio
	COBOL_LITERAL_NUM,                                                              // 014 Definizione di una literal numerica
	COBOL_LITERAL_ALPHA,           		                                            // 015 Definizione di una literal alfanumerica
	COBOL_VALUE_CLAUSE,  				                                            // 016 Valore clausola non necessariamente come literal
	COBOL_FIGURATIVE,                                                               // 017 Definizione figurativa come ZERO/ZEROES etc.
	COBOL_OPERATOR,            			                                            // 018 Operatore logico AND, OR, NOT
	COBOL_INTERNAL_FILE,       			                                            // 019 Nome file interno al programma, come Fd cCobol
	COBOL_EXTERNAL_FILE,       			                                            // 020 Nome file esterno al programma, come in Select cCobol
	COBOL_SPECIAL_REGISTER,                                                         // 021 Registri speciali dell'ambiente (es. Tally)
	COBOL_OPTION_INSTRUCTION,                                                       // 022 Opzione di istruzione (es. BY REFERENCE di Call Cobol)
	COBOL_COPY_NAME_DATA,  				                                            // 023 Nome copy di data  
	COBOL_COPY_NAME_PROC,  				                                            // 024 Nome copy di procedure  
	COBOL_ENVIRONMENT_NAME,                                                         // 025 Nome ambiente come Cobol SYSIN, SYSIPT, CONSOLE, C01....
	COBOL_MNEMONIC_NAME,                                                            // 026 Nome mnemonico assegnato al nome di ambiente in Speciasl-Names
	COBOL_FUNCTION_INTRINSIC,                                                       // 027 Nome funzione intrinseca (es. UPPER-CASE)0

	// Compiler & precompiler
	SQL_PRECOMPILER,                	                                            // 028 Statement completa del precompilatore Sql/Db2 con EXEC SQL ..
	SQL_SCRIPT_INSTRUCTION,                                                         // 029 Statement completa in script Sql/Db2 NON embedded in programmi cobol
	SQL_COMMAND,                	  	                                            // 030 Statement completa in jcl per Sql/Db2 come BIND command etc
	DL1_PRECOMPILER,                	                                            // 031 Statement completa del precompilatore Dl1
	CICS_PRECOMPILER,                	                                            // 032 Statement completa del precompilatore Cics
	
	// Categorie generali istruzioni jcl Mvs
	JCL_MVS,						                                                // 033 Statement jcl generico
	JCL_MVS_NATIVE,						                                            // 034 Statement jcl nativo     come // JOB, // EXEC, //name DD etc, SET, INCLUDE, IF, ..
	JCL_MVS_JES2,						                                            // 035 Statement Jes2           come /*istrjes2
	JCL_MVS_JES3,						                                            // 036 Statement Jes3           come //*istrjs3
	JCL_MVS_JES3_COMMAND, 				                                            // 037 Statement Jes3           come //**command

	// Jcl nativo
	JCL_MVS_JOBEND("//", EnumInstrDataCategory.JCL_MVS_NATIVE),     				// 038 Statement Job 				come //AJUYY  JOB PRTY= 10,....   
	JCL_MVS_COMMENT ("//*", EnumInstrDataCategory.JCL_MVS_NATIVE),	   				// 039 Statement Commento           come //* QUESTO E' UN COMMENTO																							 
	JCL_MVS_DELIMITER ("/*", EnumInstrDataCategory.JCL_MVS_NATIVE),	   			    // 040 Statement Delimiter          come // SYSIN DD * ....... /* 																								 
    JCL_MVS_JOB("JOB", EnumInstrDataCategory.JCL_MVS_NATIVE),     					// 041 Statement Job 				come //AJUYY  JOB PRTY= 10,....   
	JCL_MVS_JCLLIB ("JCLLIB", EnumInstrDataCategory.JCL_MVS_NATIVE),	   			// 042 Statement JCLLIB             come //JCLPROC  JCLLIB ORDER=(LOBS0.PROCLIB,LOPS0.PROCLIB)   																								 
	JCL_MVS_OUTPUT ("OUTPUT", EnumInstrDataCategory.JCL_MVS_NATIVE),	   			// 043 Statement OUTPUT             come //X  OUTPUT=DOUBLE 																								 
	JCL_MVS_COMMAND ("COMMAND", EnumInstrDataCategory.JCL_MVS_NATIVE),	   			// 044 Statement COMMAND            come //JCLPROC  JCLLIB ORDER=(LOBS0.PROCLIB,LOPS0.PROCLIB)   																								 
	JCL_MVS_CNTL ("CNTL", EnumInstrDataCategory.JCL_MVS_NATIVE),	   				// 045 Statement CNTL               come //x  CNTL 																								 
	JCL_MVS_CNTL_END ("ENDCNTL", EnumInstrDataCategory.JCL_MVS_NATIVE),	   	    	// 046 Statement ENDCNTL            come //x ENDCNTL																								 
	JCL_MVS_STEP ("EXEC", EnumInstrDataCategory.JCL_MVS_NATIVE),		   			// 047 Statement exec pgm o proc 	come //STEP01 EXEC PGM=PGM001,....  
    //                         	      									                                              o  //STEP01 EXEC PROC=PROC001,PARM1=LL, ....
    // 													  				                                              o  //       EXEC PROC003,PARM1=XX, ....
    //  									   									                                      o  //P3     EXEC PGM=PGM001,PARM1=XX, ....
	JCL_MVS_DD ("DD", EnumInstrDataCategory.JCL_MVS_NATIVE),			   			// 048 Statement DD                 come //DD01    DD DSN='DSN01.AA',DISP=SHR
	//                                    							                                                  +  //        DD DSN='DSN02.CONCAT',DISP=SHR                                
	JCL_MVS_XMIT("XMIT", EnumInstrDataCategory.JCL_MVS_NATIVE),			   			// 049 Statement XMIT               come //  XMIT
	JCL_MVS_INCLUDE("INCLUDE", EnumInstrDataCategory.JCL_MVS_NATIVE),	        	// 050 Statement jcl Macro          come //  INCLUDE MEMBER=SQ3013   	   																								 
	JCL_MVS_SET("SET", EnumInstrDataCategory.JCL_MVS_NATIVE),			        	// 051 Statement jcl Macro          come //  SET PARM1=4,PARM2='JJ',...   																							 
	JCL_MVS_IF_THEN("IF", EnumInstrDataCategory.JCL_MVS_NATIVE),			    	// 052 Statement jcl Macro cond     come //  IF rc > 0 THEN 																						 
	JCL_MVS_END_IF("END-IF", EnumInstrDataCategory.JCL_MVS_NATIVE),		    		// 053 Statement jcl Macro end cond come //  ENDIF																						 
	JCL_MVS_PROC("PROC", EnumInstrDataCategory.JCL_MVS_NATIVE),                 	// 054 Statement def PROC 			come //Y6EXCIN  PROC WAITTIME=120,...																					 
	JCL_MVS_PROC_END("PEND", EnumInstrDataCategory.JCL_MVS_NATIVE),		    		// 055 Statement end PROC           come //  PEND   																							 
	JCL_MVS_SYSIN_STREAM("*", EnumInstrDataCategory.JCL_MVS_NATIVE),		    	// 056 Input card No jcl             dopo //DDNAME DD *   																							 
	
	// Jes2
	JCL_MVS_JES2_JOBPARM("/*JOBPARM", EnumInstrDataCategory.JCL_MVS_JES2),			// 057 Statement JES2                																					 
	JCL_MVS_JES2_MESSAGE("/*MESSAGE", EnumInstrDataCategory.JCL_MVS_JES2),			// 058 Statement JES2              																					 
	JCL_MVS_JES2_NETACCT("/*NETACCT", EnumInstrDataCategory.JCL_MVS_JES2),			// 059 Statement JES2                																					 
	JCL_MVS_JES2_NOTIFY("/*NOTIFY", EnumInstrDataCategory.JCL_MVS_JES2),			// 060 Statement JES2                																					 
	JCL_MVS_JES2_OUTPUT("/*OUTPUT", EnumInstrDataCategory.JCL_MVS_JES2),			// 061 Statement JES2                																						 
	JCL_MVS_JES2_PRIORITY ("/*PRIORITY", EnumInstrDataCategory.JCL_MVS_JES2),		// 062 Statement JES2              																					 
	JCL_MVS_JES2_ROUTE("/*ROUTE", EnumInstrDataCategory.JCL_MVS_JES2),				// 063 Statement JES2               																					 
	JCL_MVS_JES2_SETUP("/*SETUP", EnumInstrDataCategory.JCL_MVS_JES2),				// 064 Statement JES2                																			 
	JCL_MVS_JES2_SIGNOFF ("/*SIGNOFF", EnumInstrDataCategory.JCL_MVS_JES2),			// 065 Statement JES2               																					 
	JCL_MVS_JES2_SIGNON("/*SIGNON", EnumInstrDataCategory.JCL_MVS_JES2),			// 066 Statement JES2               																					 
	JCL_MVS_JES2_XEQ("/*XEQ", EnumInstrDataCategory.JCL_MVS_JES2),			    	// 067 Statement JES2                																					 
	JCL_MVS_JES2_XMIT("/*XMIT", EnumInstrDataCategory.JCL_MVS_JES2),				// 068 Statement JES2               																					 
	
	// Jes3
	JCL_MVS_JES3_DATASET("//*DATASET", EnumInstrDataCategory.JCL_MVS_JES3),			// 069 Statement JES3               																			 
	JCL_MVS_JES3_ENDDATASET("//*ENDDATASET", EnumInstrDataCategory.JCL_MVS_JES3),	// 070 Statement JES3               																			 
	JCL_MVS_JES3_ENDPROCESS("//*ENDPROCESS", EnumInstrDataCategory.JCL_MVS_JES3),	// 071 Statement JES3               																			 
	JCL_MVS_JES3_FORMAT("//*FORMAT", EnumInstrDataCategory.JCL_MVS_JES3),			// 072 Statement JES3                																			 
	JCL_MVS_JES3_MAIN("//*MAIN", EnumInstrDataCategory.JCL_MVS_JES3),				// 073 Statement JES3               																			 
	JCL_MVS_JES3_NET("//*NET", EnumInstrDataCategory.JCL_MVS_JES3),			    	// 074 Statement JES3               																		 
	JCL_MVS_JES3_NETACCT("//*NETACCT", EnumInstrDataCategory.JCL_MVS_JES3),			// 075 Statement JES3               																		 
	JCL_MVS_JES3_OPERATOR("//*OPERATOR", EnumInstrDataCategory.JCL_MVS_JES3),		// 076 Statement JES3             																			 
	JCL_MVS_JES3_PAUSE("//*PAUSE", EnumInstrDataCategory.JCL_MVS_JES3),				// 077 Statement JES3                																		 
	JCL_MVS_JES3_PROCESS("//*PROCESS", EnumInstrDataCategory.JCL_MVS_JES3),			// 078 Statement JES3               																		 
	JCL_MVS_JES3_ROUTE("//*ROUTE", EnumInstrDataCategory.JCL_MVS_JES3),				// 079 Statement JES3               																		 
	JCL_MVS_JES3_SIGNOFF("//*SIGNOFF", EnumInstrDataCategory.JCL_MVS_JES3),			// 080 Statement JES3                																	 
	JCL_MVS_JES3_SIGNON("//*SIGNON", EnumInstrDataCategory.JCL_MVS_JES3);			// 081 Statement JES3               																		 
	
	
	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	String valueText1 = "";							// Valore testuale per lo statement
	EnumInstrDataCategory instrCategory = null;     // Categoria istruzione jcl

	/*
     * Costruttore vuoto
     */
	EnumInstrDataCategory(){
	}

	
	/*
     * Costruttore con identificativo istruzione jcl
     */
	EnumInstrDataCategory(String  valueText1){
		this.valueText1 = valueText1;		
	}
	/*
     * Costruttore con identificativo istruzione jcl
     */
	EnumInstrDataCategory(String  valueText1, EnumInstrDataCategory instrCategory){
		this.valueText1 = valueText1;	
		this.instrCategory = instrCategory;
	}


	/**
	 * Restituisce la stringa identificativa dell'istruzione jcl.<br>
	 * 
	 * @return the valueKey
	 */
	public String getInstrKey() {
		return valueText1;
	}


	/**
	 * Restituisce la categoria dell'istruzione jcl.<br>
	 * 
	 * @return the instrCategory
	 */
	public EnumInstrDataCategory getInstrCategory() {
		return instrCategory;
	}
	

}
