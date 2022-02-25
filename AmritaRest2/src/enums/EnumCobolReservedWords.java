package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumCobolReservedWords
  * </h1>
  *  <p>
  * Identifica le parole riservate Cobol utili a riconoscere una definizione Cobol dichiarativa, di un campo
  * o di una istruzione. 
  * <p>
  * L'enumerazione è organizzata per offrire supporto generalizzato al riconoscimento delle
  * istruzioni attraverso una <b>limitata</b> descrizione della sintassi di ogni istruzione.<br>
  * Il nome dell'enumerazione serve solo da marker, mentre sono disponibili nel costruttore fino a quattro
  * stringhe per identificare l'istruzione.<br>
  * Se ogni stringa del costruttore non contiene il carattere speciale | allora deve essere presente nella posizione, 
  * per l'istruzione da identificare. Se il carattere speciale | è presente a inizio stringa allora questa è opzionale.
  * Se invece il carattere | è presente all'interno dell stringa allora ciò delimita sottostringhe opzionali.<br>
  * 
  * <b>Esempi:</b>
  * <p>
  * "IDENTIFICATION|ID", "DIVISION"
  * "ZERO|ZEROS|ZEROES|
  * <p>
  * Nei processi di analisi dei sorgenti viene utilizzata questa enumerazione per costruire delle Map con
  * tutte le sequenze valide di parole riservate di ogni istruzione, con il riferimento a questa enumerazione. 
  * Queste map governano il parsing del sorgente.
  * 
  * <p>
  * Nel caso di clausola Usage gestisce automaticamente il riferimento alla Enumerazione di pertinenza
  * {@link EnumCobolUsage} attraverso il metodo <b>getCobolUsage</b>.<br>
  * Nel caso di costante figurativa gestisce automaticamente il riferimento alla Enumerazione di pertinenza
  * {@link EnumCobolFigurativeConstants} attraverso il metod <b>getCobolFigurative</b>.<br>
  * Il metodo <b>toString</b> viene ridefinito per restituire il valore stringa corretto con un trattino
  * al poso dell'underscore, dove richiesto.
  * 
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/04/2010
  * @see Analyzer
  * @see EnumDataItemGeneric
  * @see EnumDataItemFigurative
  * @see EnumDataItemType 
  * @see EnumDataItemTypeSql
*/
@DataBaseMappedEnumeration
public enum EnumCobolReservedWords {
    
    NOT_ASSIGNED,																																	// 000

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate data item di inizio clausola o interne
	//////////////////////////////////////////////////////////////////////////////////////////////////

	DATA_ITEM_BLANK("BLANK", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 001     
	DATA_ITEM_EXTERNAL("EXTERNAL", "",  					EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 002              
	DATA_ITEM_GLOBAL("GLOBAL", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 003          
	DATA_ITEM_JUSTIFIED("JUSTIFIED", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 004          
	DATA_ITEM_OCCURS("OCCURS", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 005              
	DATA_ITEM_PICTURE("PICTURE|PIC", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 006           
	DATA_ITEM_SIGN("SIGN", "", 								EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 007  
	DATA_ITEM_SYNC("SYNCHRONIZED|SYNC", "", 				EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 008        
	DATA_ITEM_VALUE("VALUE", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM),      											// 009 
	DATA_ITEM_RENAMES("RENAMES", "", 						EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 010       
	DATA_ITEM_REDEFINES("REDEFINES", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM),     											// 011   
	DATA_ITEM_INDEXED("INDEXED", "", 						EnumInstrDataCategory.COBOL_DATA_ITEM),      											// 012    	
	DATA_ITEM_USAGE("USAGE", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM),      											// 013     
	DATA_ITEM_USAGE_BINARY("BINARY", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_BINARY),    				// 014 
	DATA_ITEM_USAGE_COMP("COMP|COMPUTATIONAL", "", 			EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP),     					// 015      
	DATA_ITEM_USAGE_COMP_1("COMP-1|COMPUTATIONAL-1", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_1),    				// 016 
	DATA_ITEM_USAGE_COMP_2("COMP-2|COMPUTATIONAL-2", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_2),    				// 017 
	DATA_ITEM_USAGE_COMP_3("COMP-3|COMPUTATIONAL-3", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_3),    				// 018 
	DATA_ITEM_USAGE_COMP_4("COMP-4|COMPUTATIONAL-4", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_4),    				// 019 
	DATA_ITEM_USAGE_COMP_5("COMP-5|COMPUTATIONAL-5", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_5),    				// 020 
	DATA_ITEM_USAGE_COMP_6("PACKED|PACKED-DECIMAL", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_PACKED_DECIMAL),    		// 021 
	DATA_ITEM_USAGE_COMP_X("COMP-X", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_COMP_X),    				// 022 
	DATA_ITEM_USAGE_DISPLAY("DISPLAY", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_DISPLAY),    				// 023 
	DATA_ITEM_USAGE_DISPLAY_1("DISPLAY-1", "", 				EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_DISPLAY_1),	    			// 024 
	DATA_ITEM_USAGE_POINTER("POINTER", "", 					EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_POINTER),     				// 025 
	DATA_ITEM_USAGE_PROC_POINTER("PROCEDURE-POINTER", "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_PROCEDURE_POINTER),    		// 026 
	DATA_ITEM_USAGE_FUNC_POINTER("FUNCTION-POINTER",  "", 	EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_FUNCTION_POINTER),     		// 027     
	DATA_ITEM_USAGE_INDEX("INDEX", "", 						EnumInstrDataCategory.COBOL_DATA_ITEM, EnumCobolUsage.USAGE_INDEX),     				// 028      	

	 
	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di inizio istruzioni di identification division
	//////////////////////////////////////////////////////////////////////////////////////////////////

	ID_DIV_PROGRAM_ID("PROGRAM-ID", "", 				    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 029
	ID_DIV_AUTHOR("AUTHOR", "", 						    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 030
	ID_DIV_INST("INSTALLATION", "", 					    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 031
	ID_DIV_DATE_WRITTEN("DATE-WRITTEN", "", 			    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 032
	ID_DIV_DATE_COMPILED("DATE-COMPILED", "", 			    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 033
	ID_DIV_SECURITY("SECURITY", "", 					    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 034
	ID_DIV_REMARKS("REMARKS", "", 						    EnumInstrDataCategory.COBOL_SOURCE_IDENTIFICATION),										// 035 

	  
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di inizio istruzioni di environment division
	//////////////////////////////////////////////////////////////////////////////////////////////////

	ENV_DIV_SOURCE_COMPUTER("SOURCE-COMPUTER", "",  	    EnumInstrDataCategory.COBOL_ENVIRONMENT_DEFINITION),									// 036
	ENV_DIV_OBJECT_COMPUTER("OBJECT-COMPUTER", "",  	    EnumInstrDataCategory.COBOL_ENVIRONMENT_DEFINITION),									// 037
	ENV_DIV_SPECIAL_NAMES("SPECIAL-NAMES", "",  		    EnumInstrDataCategory.COBOL_ENVIRONMENT_DEFINITION),									// 038
	ENV_DIV_REPOSITORY("REPOSITORY", "",  		            EnumInstrDataCategory.COBOL_ENVIRONMENT_DEFINITION),									// 039
	ENV_DIV_SELECT("SELECT", "", 						    EnumInstrDataCategory.COBOL_ENVIRONMENT_DEFINITION),									// 040


	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di ambiente (environment name)
	//////////////////////////////////////////////////////////////////////////////////////////////////

	ENV_NAME_SYSIN("SYSIN", "",  	    					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 041
	ENV_NAME_SYSIPT("SYSIPT", "",  	    					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 042
	ENV_NAME_SYSOUT("SYSOUT", "",  	    					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 043
	ENV_NAME_SYSLST("SYSLST", "",  	    					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 044
	ENV_NAME_SYSLIST("SYSLIST", "",      					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 045
	ENV_NAME_SYSPUNCH("SYSPUNCH", "",  	    				EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 046
	ENV_NAME_SYSPCH("SYSPCH", "",  	    					EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 047
	ENV_NAME_CONSOLE("CONSOLE", "",  	    				EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 048
	ENV_NAME_CSP("CSP", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 049
	ENV_NAME_S01("S01", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 050
	ENV_NAME_S02("S02", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 051
	ENV_NAME_S03("S03", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 052
	ENV_NAME_S04("S04", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 053
	ENV_NAME_S05("S05", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 054
	ENV_NAME_C01("C01", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 055
	ENV_NAME_C02("C02", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 056
	ENV_NAME_C03("C03", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 057
	ENV_NAME_C04("C04", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 058
	ENV_NAME_C05("C05", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 059
	ENV_NAME_C06("C06", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 060
	ENV_NAME_C07("C07", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 061
	ENV_NAME_C08("C08", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 062
	ENV_NAME_C09("C09", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 063
	ENV_NAME_C10("C10", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 064
	ENV_NAME_C11("C11", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 065
	ENV_NAME_C12("C12", "",  	    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 066
	ENV_NAME_AFP_5A("AFP-5A", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 067
	ENV_NAME_UPSI_0("UPSI-0", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 068
	ENV_NAME_UPSI_1("UPSI-1", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 069
	ENV_NAME_UPSI_2("UPSI-2", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 070
	ENV_NAME_UPSI_3("UPSI-3", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 071
	ENV_NAME_UPSI_4("UPSI-4", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 072
	ENV_NAME_UPSI_5("UPSI-5", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 073
	ENV_NAME_UPSI_6("UPSI-6", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 074
	ENV_NAME_UPSI_7("UPSI-7", "",    						EnumInstrDataCategory.COBOL_ENVIRONMENT_NAME),											// 075
	
	MNEMONIC_NAME("", "",  	    						    EnumInstrDataCategory.COBOL_MNEMONIC_NAME),												// 076

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di inizio istruzioni di data division
	//////////////////////////////////////////////////////////////////////////////////////////////////

	DATA_DIV_DATA_ITEM("9*", "", 							EnumInstrDataCategory.COBOL_DATA_ITEM, EnumSymbolType.COBOL_SYMBOL_DATA_ITEM),			// 077
	DATA_DIV_FD("FD", "", 								    EnumInstrDataCategory.COBOL_STRUCTURE_DEFINITION),										// 078
	DATA_DIV_SD("SD","", 								    EnumInstrDataCategory.COBOL_STRUCTURE_DEFINITION),										// 079
	DATA_DIV_RD("RD", "", 								    EnumInstrDataCategory.COBOL_STRUCTURE_DEFINITION),										// 080
 

	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di inizio istruzioni di procedure division
	//////////////////////////////////////////////////////////////////////////////////////////////////

	// Istruzioni principali ai fini dell'analisi dinamica
	PROC_DIVISION("PROCEDURE", "DIVISION",                  EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 081
	PROC_ENTRY("ENTRY", "",                                 EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 082
	PROC_LABEL("", "", 				                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 083
	PROC_SECTION("", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),										    // 084
	PROC_IF("IF", "", 				                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 085
	PROC_ELSE("ELSE", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 086
	PROC_NEXT_SENTENCE("NEXT", "SENTENCE", 	                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 087
	PROC_MOVE("MOVE", " |CORR|CORRESPONDING", 			    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 088
	PROC_GOTO("GO", " |TO", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 089
	PROC_CALL("CALL", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 090
	PROC_CANCEL("CANCEL", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 091
	PROC_PERFORM("PERFORM", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 092
	PROC_EVALUATE("EVALUATE", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 093
	PROC_SEARCH("SEARCH", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 094
	PROC_WHEN_EVALUATE_SEARCH("WHEN", "", 			        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 095
	PROC_INITIALIZE("INITIALIZE", "", 		                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 096
	PROC_OPEN("OPEN", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 097
	PROC_CLOSE("CLOSE", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 098
	PROC_READ("READ", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 099
	PROC_WRITE("WRITE", "",			                    	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 100
	PROC_REWRITE("REWRITE", "",			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 101
	PROC_DELETE("DELETE", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 102
	PROC_START("START", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 103
	PROC_EXIT("EXIT", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 104
	PROC_STOP("STOP", "RUN| ", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 105
	PROC_GOBACK("GOBACK", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 106
	PROC_CONTINUE("CONTINUE", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 107
	PROC_COMPUTE("COMPUTE", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 108
	PROC_SUBTRACT("SUBTRACT", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 109
	PROC_ADD("ADD", "", 				                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 110
	PROC_MULTIPLY("MULTIPLY", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 111
	PROC_DIVIDE("DIVIDE", "",			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 112
	PROC_SET("SET", "", 				                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 113
	PROC_RETURN("RETURN", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 114
	PROC_RELEASE("RELEASE", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 115
	PROC_ACCEPT("ACCEPT", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 116
	PROC_DISPLAY("DISPLAY", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 117
	PROC_DECLARATIVES("DECLARATIVES", "", 	                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 118
	PROC_INVOKE("INVOKE", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 119
	PROC_USE("USE", "", 				                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 120
	PROC_SORT("SORT", "", 			                        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 121
	PROC_MERGE("MERGE", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 122
    
	// Istruzioni secondarie ai fini dell'analisi dinamica
	PROC_ALTER("ALTER", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 123
	PROC_EXAMINE("EXAMINE", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 124
	PROC_EXHIBIT("EXHIBIT", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 125
	PROC_INSPECT("INSPECT", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 126
	PROC_STRING("STRING", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 127
	PROC_UNSTRING("UNSTRING", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 128
	PROC_TRANSFORM("TRANSFORM", "", 		                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 129
	PROC_INITIATE("INITIATE", "", 		                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 130
	PROC_TERMINATE("TERMINATE", "", 		                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 131
	PROC_ENTER("ENTER", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION),									 		// 132

	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di fine istruzioni di procedure division  
	//////////////////////////////////////////////////////////////////////////////////////////////////

	// Istruzioni principali ai fini dell'analisi dinamica
	PROC_END_IF("END-IF", "", 				                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_IF),			// 133
	PROC_END_CALL("END-CALL", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_CALL),		// 134
	PROC_END_PERFORM("END-PERFORM", "", 			        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_PERFORM),		// 135
	PROC_END_EVALUATE("END-EVALUATE", "", 		            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_EVALUATE),	// 136
	PROC_END_SEARCH("END-SEARCH", "", 			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_SEARCH),		// 137
	PROC_END_READ("END-READ", "", 			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_READ),		// 138
	PROC_END_WRITE("END-WRITE", "",			                EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_WRITE),		// 139
	PROC_END_REWRITE("END-REWRITE", "",			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_REWRITE),		// 140
	PROC_END_DELETE("END-DELETE", "", 			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_DELETE),		// 141
	PROC_END_START("END-START", "", 			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_START),		// 142
	PROC_END_COMPUTE("END-COMPUTE", "", 			        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_COMPUTE),		// 143
	PROC_END_SUBTRACT("END-SUBTRACT", "", 		            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_SUBTRACT),	// 144
	PROC_END_ADD("END-ADD", "", 				            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_ADD),			// 145
	PROC_END_MULTIPLY("END-MULTIPLY", "", 		            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_MULTIPLY),	// 146
	PROC_END_DIVIDE("END-DIVIDE", "",			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_DIVIDE),		// 147
	PROC_END_DECLARATIVES("END", "DECLARATIVES", 	        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_DECLARATIVES),// 148
	PROC_END_INVOKE("END-INVOKE", "", 	                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_DECLARATIVES),// 149

	// Istruzioni secondarie ai fini dell'analisi dinamica
	PROC_END_STRING("END-STRING", "", 			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_STRING),		// 150
	PROC_END_UNSTRING("END-UNSTRING", "", 		            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_UNSTRING),	// 151
	PROC_END_RETURN("END-RETURN", "", 			            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_RETURN),		// 152
	PROC_END_INITIATE("END-INITIATE", "", 		            EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_INITIATE),	// 153
	PROC_END_TERMINATE("END-TERMINATE", "", 		        EnumInstrDataCategory.COBOL_PROC_INSTRUCTION, EnumCobolReservedWords.PROC_TERMINATE),	// 154


	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate per sotto istruzioni di gestione exception
	//////////////////////////////////////////////////////////////////////////////////////////////////
    
	// Inizio per ON opzionale
	PROC_ON_SIZE_ERROR		(" |ON", "SIZE", 	"ERROR",            	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 155
	PROC_ON_OVERFLOW  		(" |ON", "OVERFLOW",                     	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 156
	PROC_ON_EXCEPTION 		(" |ON", "EXCEPTION",                   	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 157
	// Inizio per AT opziona 
	PROC_AT_END       		(" |AT", "END",           					EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 158
	PROC_AT_END_OF_PAGE		(" |AT", "END-OF-PAGE", 					EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 159
	PROC_AT_EOP				(" |AT", "EOP", 							EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 160
	// Inizio per NOT obbligatori
	PROC_NOT_ON_SIZE_ERROR	("NOT", " |ON", 	"SIZE", 	"ERROR",   	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 161
	PROC_NOT_ON_OVERFLOW	("NOT", " |ON",		"OVERFLOW",          	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 162
	PROC_NOT_ON_EXCEPTION	("NOT", " |ON",		"EXCEPTION",        	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 163
	PROC_NOT_AT_END			("NOT", " |AT", 	"END",           		EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 164
	PROC_NOT_AT_END_OF_PAGE	("NOT", " |AT", 	"END-OF-PAGE", 			EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 165
	PROC_NOT_AT_EOP			("NOT", " |AT", 	"EOP", 					EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 166
	PROC_NOT_INVALID_KEY	("NOT", "INVALID", 	" |KEY",          		EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 167
    // Inizio per INVALID
	PROC_INVALID_KEY		("INVALID", " |KEY",                     	EnumInstrDataCategory.COBOL_PROC_INSTRUCTION_EXCEPTION),			 		// 168
	

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate di delimitazione aree e sezioni del programma 
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	ID_DIVISION("IDENTIFICATION|ID", "DIVISION", 		    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 169
	ENV_DIVISION("ENVIRONMENT", "DIVISION", 				EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 170
	ENV_DIV_CONF_SECTION("CONFIGURATION", "SECTION", 	    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 171
	ENV_DIV_I_O_SECTION("INPUT-OUTPUT", "SECTION", 	    	EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 172
	ENV_DIV_FILE_CONTROL("FILE-CONTROL", "",  			    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 173
	ENV_DIV_I_O_CONTROL("I-O-CONTROL", "", 					EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 174
	DATA_DIVISION("DATA", "DIVISION", 					    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 175
	DATA_DIV_WS_SECTION("WORKING-STORAGE", "SECTION", 	    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 176
	DATA_DIV_LINKAGE_SECTION("LINKAGE", "SECTION", 			EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 177
	DATA_DIV_FILE_SECTION("FILE", "SECTION", 			    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 178
	DATA_DIV_REPORT_SECTION("REPORT", "SECTION", 		    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 179
	DATA_DIV_SCREEN_SECTION("SCREEN", "SECTION", 		    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 180
	DATA_DIV_COMM_SECTION("COMMUNICATION", "SECTION", 	    EnumInstrDataCategory.COBOL_SECTION_PROGRAM_DELIMITER),			 						// 181

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Special Register e campi di sistema
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	REGISTER_RETURN_CODE("RETURN-CODE", "",				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 188
	REGISTER_LENGTH_OF("LENGTH", "OF", 					    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 189
	REGISTER_ADDRESS_OF("ADDRESS", "OF", 				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 190
	REGISTER_LINAGE("LINAGE-COUNTER", "", 				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 191
	REGISTER_TALLY("TALLY", "", 						    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 192
	REGISTER_WHEN_COMPILED("WHEN-COMPILED", "", 		    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 193
	REGISTER_DEBUG_ITEM("DEBUG-ITEM", "", 				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 194
	REGISTER_DEBUG_NAME("DEBUG-NAME", "", 				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 195
	REGISTER_DEBUG_LINE("DEBUG-LINE", "", 				    EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 196
	REGISTER_DEBUG_CONTENTS("DEBUG-CONTENTS", "", 			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 197
	REGISTER_DEBUG_SUB_1("DEBUG-SUB-1", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 198
	REGISTER_DEBUG_SUB_2("DEBUG-SUB-2", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 199
	REGISTER_DATE("DATE", "", 				    			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 200
	REGISTER_TIME("TIME", "", 								EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 201
	REGISTER_DAY("DAY", "", 				    			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 202
	REGISTER_DAY_OF_WEEK("DAY-OF-WEEK", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 203
	REGISTER_SORT_CONTROL("SORT-CONTROL", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 204
	REGISTER_SORT_CORE_SIZE("SORT-CORE-SIZE", "", 			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 205
	REGISTER_SORT_FILE_SIZE("SORT-FILE-SIZE", "", 			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 206
	REGISTER_SORT_MESSAGE("SORT-MESSAGE", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 207
	REGISTER_SORT_MODE_SIZE("SORT-MODE-SIZE", "", 			EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 208
	REGISTER_SORT_RETURN("SORT-RETURN", "", 				EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 209
	REGISTER_XML_EVENT("XML-EVENT", "", 					EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 210
	REGISTER_XML_TEXT("XML-TEXT", "", 						EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 								// 211
	REGISTER_XML_NTEXT("XML-NTEXT", "", 					EnumInstrDataCategory.COBOL_SPECIAL_REGISTER),			 			                    // 212

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL function intrinsic
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	FUNCTION_ACOS("FUNCTION", "ACOS", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			                // 287
	FUNCTION_ANNUITY("FUNCTION", "ANNUITY", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 287
	FUNCTION_ASIN("FUNCTION", "ASIN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_ATAN("FUNCTION", "ATAN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 287
	FUNCTION_CHAR("FUNCTION", "CHAR", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 287
	FUNCTION_COS("FUNCTION", "COS", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 287
	FUNCTION_CURRENT_DATE("FUNCTION", "CURRENT-DATE", "", 				EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 275
	FUNCTION_DATE_OF_INTEGER("FUNCTION", "DATE-OF-INTEGER", "", 		EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 275
	FUNCTION_DATE_TO_YYYYDDD("FUNCTION", "DATE-TO-YYYYDDD", "", 		EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 175
	FUNCTION_DATEVAL("FUNCTION", "DATEVAL", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_DAY_OF_INTEGER("FUNCTION", "DAY-OF-INTEGER", "", 			EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_DAY_TO_YYYYDDD("FUNCTION", "DAY-TO-YYYYDDD", "", 			EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_DISPLAY_OF("FUNCTION", "DISPLAY-OF", "", 					EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_FACTORIAL("FUNCTION", "FACTORIAL", "", 					EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_INTEGER("FUNCTION", "INTEGER", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_INTEGER_OF_DATE("FUNCTION", "INTEGER-OF-DATE", "", 		EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_INTEGER_OF_DAY("FUNCTION", "INTEGER-OF-DAY", "", 			EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_INTEGER_PART("FUNCTION", "INTEGER-PART", "", 				EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_LENGTH("FUNCTION", "LENGTH", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_LOG("FUNCTION", "LOG", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_LOG10("FUNCTION", "LOG10", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_LOWER_CASE("FUNCTION", "LOWER-CASE", "", 					EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MAX("FUNCTION", "MAX", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MEAN("FUNCTION", "MEAN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MEDIAN("FUNCTION", "MEDIAN", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MIDRANGE("FUNCTION", "MIDRANGE", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MIN("FUNCTION", "MIN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_MOD("FUNCTION", "MOD", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_NATIONAL_OF("FUNCTION", "NATIONAL-OF", "", 				EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_NUMVAL("FUNCTION", "NUMVAL", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_NUMVAL_C("FUNCTION", "NUMVAL-C", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_ORD("FUNCTION", "ORD", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_ORD_MAX("FUNCTION", "ORD-MAX", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_ORD_MIN("FUNCTION", "ORD-MIN", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_PRESENT_VALUE("FUNCTION", "PRESENT-VALUE", "", 			EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_RANDOM("FUNCTION", "RANDOM", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_REM("FUNCTION", "REM", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_REVERSE("FUNCTION", "REVERSE", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_SIN("FUNCTION", "SIN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_SQRT("FUNCTION", "SQRT", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_STANDARD_DEVIATION("FUNCTION", "STANDARD-DEVIATION", "", 	EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_SUM("FUNCTION", "SUM", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_TAN("FUNCTION", "TAN", "", 								EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_UNDATE("FUNCTION", "UNDATE", "", 							EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_UPPER_CASE("FUNCTION", "UPPER-CASE", "", 					EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_VARIANCE("FUNCTION", "VARIANCE", "", 						EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_WHEN_COMPILED("FUNCTION", "WHEN-COMPILED", "", 			EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_YEAR_TO_YYY("FUNCTION", "YEAR-TO-YYYY", "", 				EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	FUNCTION_YEARWINDOW("FUNCTION", "YEARWINDOW", "", 					EnumInstrDataCategory.COBOL_FUNCTION_INTRINSIC),			 				// 187
	

	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Parole riservate e opzioni specifiche di istruzioni
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	OPT_BY_REFERENCE(" |BY", "REFERENCE", 					EnumInstrDataCategory.COBOL_OPTION_INSTRUCTION),										// 188
	OPT_BY_CONTENT(" |BY", "CONTENT", 					    EnumInstrDataCategory.COBOL_OPTION_INSTRUCTION),										// 189

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Costanti figurative
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
    // Costanti figurative              
	FIGURATIVE_ZERO("ZERO|ZEROS|ZEROES", 		"", 	    EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.ZERO, 				EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 190
	FIGURATIVE_SPACE("SPACE|SPACES", 			"", 	    EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.SPACE, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 191
	FIGURATIVE_HIGH_VALUE("HIGH-VALUE|HIGH-VALUES", "",     EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.HIGH_VALUE,		EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 192
	FIGURATIVE_LOW_VALUE("LOW-VALUE|LOW-VALUES", 	"",     EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.LOW_VALUE, 		EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 193
	FIGURATIVE_QUOTE("QUOTE|QUOTES", 			"", 	    EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.QUOTE, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 194
	FIGURATIVE_NULL("NULL|NULLS", 			"", 	    	EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.NULL, 				EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 195
	FIGURATIVE_ON("ON", 			"", 	    	        EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.ON, 				EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 196
	FIGURATIVE_OFF("OFF", 			"", 	    	        EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.OFF, 				EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 197
	FIGURATIVE_TRUE("TRUE", 			"", 	    		EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.TRUE, 				EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 198
	FIGURATIVE_FALSE("FALSE", 			"", 	    		EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.FALSE, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 199
	FIGURATIVE_NUMERIC("NUMERIC", 			"", 	    	EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.NUMERIC, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 200
	FIGURATIVE_ALPHABETIC("ALPHABETIC", 			"", 	EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.ALPHABETIC, 		EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 201
	FIGURATIVE_ALPHABETIC_LOWER("ALPHABETIC-LOWER", "", 	EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.ALPHABETIC_LOWER, 	EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 202
	FIGURATIVE_ALPHABETIC_UPPER("ALPHABETIC-UPPER", "", 	EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.ALPHABETIC_UPPER, 	EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// 203
	FIGURATIVE_POSITIVE("POSITIVE", "", 					EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.POSITIVE, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// XXX
	FIGURATIVE_NEGATIVE("NEGATIVE", "", 					EnumInstrDataCategory.COBOL_FIGURATIVE, EnumCobolFigurativeConstants.NEGATIVE, 			EnumSymbolType.COBOL_SYMBOL_FIGURATIVE),			// XXX

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Operatori e operandi
	//////////////////////////////////////////////////////////////////////////////////////////////////
	
	OPERATOR_OR("OR", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_OR, 				EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 204
	OPERATOR_AND("AND", 		"", 	      			    EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_AND, 				EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 205
	OPERATOR_NOT("NOT", 		"", 	      				EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_NOT, 				EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 206
	OPERATOR_EQUAL("=|EQUAL|EQUALS", "", 	      		    EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_EQUAL, 			EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 207
	OPERATOR_UNEQUAL("<>|UNEQUAL", "", 	      		        EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_UNEQUAL, 			EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 207
	OPERATOR_GREATER(">|GREATER|EXCEEDS", " |THEN", 	    EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_GREATER, 			EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 208
	OPERATOR_GREATER_EQUAL(">=", " |THEN", 	                EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_GREATER_EQUAL, 	EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 209
	OPERATOR_LESS("<|LESS", " |THEN", 	      		    	EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_LESS, 			EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 210
	OPERATOR_LESS_EQUAL("<=", " |THEN", 	                EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_LESS_EQUAL, 		EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 211
	OPERATOR_ANY("ANY", "", 	                            EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_ANY, 		        EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 212
	OPERATOR_THRU("THRU|THROUGH", 		"", 	      		EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.LOGIC_THRU, 			EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 213
	OPERATOR_THEN("THEN", 		"", 	      		        EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.THEN, 			        EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 213
	OPERATOR_PAR_OPEN("(", 		"", 	      				EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.PAR_OPEN, 				EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 214
	OPERATOR_PAR_CLOSE(")", 		"", 	      			EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.PAR_CLOSE, 				EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 215
	OPERATOR_ALL("ALL", "", 	      		        	    EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.ALL, 			        EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 207
	OPERATOR_OF("OF", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.OF, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 216
	OPERATOR_IN("IN", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.IN, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 217
	OPERATOR_IS("IS", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.IS, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 218
	OPERATOR_ADD("+", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.ADD, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 219
	OPERATOR_SUB("-", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.SUB, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 220
	OPERATOR_MULT("*", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.MULT, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 221
	OPERATOR_DIV("/", 		"", 	      					EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.DIV, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 222
	OPERATOR_POWER("**", 		"", 	      				EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.POWER, 					EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 222
	OPERATOR_REF_MOD_SEPARATOR(":", 		"", 	      	EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.REF_MOD_SEPARATOR, 		EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 222
	OPERATOR_COMMA_SEPARATOR(",", 		"", 	      	    EnumInstrDataCategory.COBOL_OPERATOR, EnumCobolOperator.COMMA_SEPARATOR, 		EnumSymbolType.COBOL_SYMBOL_OPERATOR),						// 222
	
	OPERAND_LITERAL_NUM("", 		"", 	      			EnumInstrDataCategory.COBOL_LITERAL_NUM, 										EnumSymbolType.COBOL_SYMBOL_LITERAL_NUM),					// 223
	OPERAND_LITERAL_ALPHA("", 		"", 	      		    EnumInstrDataCategory.COBOL_LITERAL_ALPHA, 										EnumSymbolType.COBOL_SYMBOL_LITERAL_ALPHA),					// 224

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Direttive per il compilatore
	//////////////////////////////////////////////////////////////////////////////////////////////////
                 				    
	DIR_COMPILER_COPY("COPY",  "", 							EnumInstrDataCategory.COBOL_COPY_INSTRUCTION),						    																	// 225
	DIR_COMPILER_EJECT("EJECT", "", 					    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 226
	DIR_COMPILER_TITLE("TITLE", "", 					    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 227
	DIR_COMPILER_SKIP1("SKIP1", "", 					    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 228
	DIR_COMPILER_SKIP2("SKIP2", "", 					    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 229		
	DIR_COMPILER_SKIP3("SKIP3", "", 					    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 230		
	DIR_COMPILER_SERVICE("SERVICE", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_REPLACE("REPLACE", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_INSERT("INSERT", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_DELETE("DELETE", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_BASIS("BASIS", "",   	                    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_PROCESS("PROCESS", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_CBL("CBL", "",   	                        EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_ENTER("ENTER", "",   	                    EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_START_CBL("*CBL", "",   	                EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_START_CONTROL("*CONTROL", "",   	        EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_READY_TRACE("READY", "TRACE",   	        EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231
	DIR_COMPILER_RESET_TRACE("RESET", "TRACE",   	        EnumInstrDataCategory.COBOL_COMPILER_DIRECTIVE),																							// 231

	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// COBOL Istruzioni di precompilatore gestite 
	//////////////////////////////////////////////////////////////////////////////////////////////////

	PRECOMPILER_SQL ("EXEC", "SQL",  					    EnumInstrDataCategory.SQL_PRECOMPILER),																										// 232   																			// 232
	PRECOMPILER_CICS("EXEC", "CICS", 					    EnumInstrDataCategory.CICS_PRECOMPILER),																									// 233
	PRECOMPILER_DL1 ("EXEC", "DLI",  					    EnumInstrDataCategory.DL1_PRECOMPILER),																										// 234
	
	//////////////////////////////////////////////////////////////////////////////////////////////////
	// Istruzioni specifiche Cobol/MF
	//////////////////////////////////////////////////////////////////////////////////////////////////

	PROC_COMMIT("COMMIT", "", 			                    EnumInstrDataCategory.COBOL_PROC_INSTRUCTION);									 															// 235
	
	
	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	String valueText1 = "";							// Valore singolo in alternativa a valueText2 cone ID o IDENTIFICATION
	String valueText2 = "";                         // Secondo valore come DIVISION dopo IDENTIFICATION o PROCEDURE
	String valueText3 = "";                         // Terzo valore come ERROR di ON SIZE ERROR
	String valueText4 = "";                         // Quarto valore come ERROR di NOT ON SIZE ERROR
	EnumInstrDataCategory en_InstrCategory = EnumInstrDataCategory.NOT_ASSIGNED;
	EnumCobolReservedWords en_InstrRelated = null;  // Istruzione collegata (es. se END-IF en_InstructionRelated = IF)
	EnumCobolUsage en_CobolUsage = EnumCobolUsage.NOT_ASSIGNED;
	EnumCobolFigurativeConstants en_CobolFigurativeConstant = EnumCobolFigurativeConstants.NOT_ASSIGNED;
	EnumSymbolType en_CobolSymbolType = EnumSymbolType.NOT_ASSIGNED;
	EnumCobolOperator en_CobolOperator = EnumCobolOperator.NOT_ASSIGNED;
	
	
	/*
     * 
     * Costruttore vuoto
     * 
     */
	EnumCobolReservedWords(){
	}

	
	/*
     * 
     * Costruttore con valore corretto parola Cobol
     * 
     */
	EnumCobolReservedWords(String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_InstrCategory = en_InstrCategory;
		
	}

	/*
     * 
     * Costruttore con valore corretto parola Cobol formata da 3 sottoparole chiave
     * 
     */
	EnumCobolReservedWords(String  valueText1, String  valueText2, String  valueText3,  EnumInstrDataCategory en_InstrCategory){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.valueText3 = valueText3;
		this.en_InstrCategory = en_InstrCategory;
		
	}

	/*
     * 
     * Costruttore con valore corretto parola Cobol formata da 4 sottoparole chiave
     * 
     */
	EnumCobolReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrDataCategory en_InstrCategory){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.valueText3 = valueText3;
		this.valueText4 = valueText4;
		this.en_InstrCategory = en_InstrCategory;
		
	}

	/*
     * 
     * Costruttore con valore corretto parola Cobol e rifierimento a istruzione collegata
     * 
     */
	EnumCobolReservedWords(String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory, EnumCobolReservedWords en_InstrRelated){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_InstrCategory = en_InstrCategory;
		this.en_InstrRelated = en_InstrRelated;
	}

	
 
   /*
    * 
    * Costruttore con valore corretto parola Cobol
    * 
   */
	EnumCobolReservedWords(String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory, EnumCobolUsage en_CobolUsage){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_CobolUsage = en_CobolUsage;
		this.en_InstrCategory = en_InstrCategory;
	}


	 
   /*
    * 
    * Costruttore con valore corretto parola Cobol
    * 
   */
	EnumCobolReservedWords(String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory, EnumCobolFigurativeConstants en_CobolFigurativeConstant, EnumSymbolType en_CobolSymbolType){
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_InstrCategory = en_InstrCategory;
		this.en_CobolFigurativeConstant = en_CobolFigurativeConstant;
		this.en_CobolSymbolType = en_CobolSymbolType;
	}


	
   /*
    * 
    * Costruttore per operatori Cobol 
    * 
   */
	EnumCobolReservedWords (String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory, EnumCobolOperator en_CobolOperator, EnumSymbolType en_CobolSymbolType) {
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_InstrCategory = en_InstrCategory;
		this.en_CobolOperator = en_CobolOperator;
		this.en_CobolSymbolType = en_CobolSymbolType;
	}

   /*
    * 
    * Costruttore per operandi Cobol 
    * 
   */
	EnumCobolReservedWords (String  valueText1, String  valueText2, EnumInstrDataCategory en_InstrCategory, EnumSymbolType en_CobolSymbolType) {
		this.valueText1 = valueText1;
		this.valueText2 = valueText2;
		this.en_InstrCategory = en_InstrCategory;
		this.en_CobolSymbolType = en_CobolSymbolType;
	}

	/**
	 * @return the valueText
	 */
	public String getValueText1() {
		return valueText1;
	}

	/**
	 * @return the valueText2
	 */
	public String getValueText2() {
		return valueText2;
	}

	/**
	 * @return the valueText3
	 */
	public String getValueText3() {
		return valueText3;
	}


	/**
	 * @return the valueText4
	 */
	public String getValueText4() {
		return valueText4;
	}


	

	/**
	 * @return the Category of instruction
	 */
	public EnumInstrDataCategory getCobolInstrCategory() {
		return this.en_InstrCategory;
	}

	/**
	 * @return the instruction related
	 */
	public EnumCobolReservedWords getCobolInstrRelated() {
		return this.en_InstrRelated;
	}


	/**
	 * @return the Cobol Usage
	 */
	public EnumCobolUsage getCobolUsage() {
		return this.en_CobolUsage;
	}

	/**
	 * @return the Cobol Usage
	 */
	public EnumCobolFigurativeConstants getCobolFigurative() {
		return this.en_CobolFigurativeConstant;
	}
    
	/**
	 * @return the en_CobolSymbolType
	 */
	public EnumSymbolType getCobolSymbolType() {
		return en_CobolSymbolType;
	}


	/**
	 * @param en_CobolSymbolType the en_CobolSymbolType to set
	 */
	public void setCobolSymbolType(EnumSymbolType en_CobolSymbolType) {
		this.en_CobolSymbolType = en_CobolSymbolType;
	}


	/**
	 * @return the en_CobolOperator
	 */
	public EnumCobolOperator getCobolOperator() {
		return en_CobolOperator;
	}


	/**
	 * @param en_CobolOperator the en_CobolOperator to set
	 */
	public void setCobolOperator(EnumCobolOperator en_CobolOperator) {
		this.en_CobolOperator = en_CobolOperator;
	}


	/*
	 * 
	 * Restituisce true se si tratta di un operatore Cobol.
	 * 
	 * 
	 */
	public boolean isCobolOperator() {
		if (en_CobolSymbolType != null) {
			return true;
		}
		
		return false;
	}

	/*
	 * 
	 * Restituisce true se si tratta di una istruzione Cobol
	 * 
	 * 
	 */
	public boolean isCobolInstruction() {
		if (en_InstrCategory != null) {
			return true;
		}
		
		return false;
	}
	

	
	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	@Override
	public String toString() {
		if (this == EnumCobolReservedWords.PROC_LABEL) {
			return "LABEL";
		}
		if (this == EnumCobolReservedWords.PROC_SECTION) {
			return "SECTION";
		}
		if (valueText1.equals("")) {
			return "NOT_ASSIGNED";
		}
				
		return valueText1 + " " + valueText2;
	}
}
