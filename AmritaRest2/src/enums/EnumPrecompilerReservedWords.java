package enums;

import analyzer.AmritaConstants;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumPrecompilerReservedWords (09)
  * </h1>
  *  <p>
  * Questa enum elenca tutte le istruzioni e opzioni dei vari linguaggi e amnbienti
  * intercettate nel processo di analisi.
  * Vengono codificate anche le istruzioni NON cobol quali istruzioni dirette ai vari
  * precompilatori come Cics e Sql.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumObject
   * 
*/
@DataBaseMappedEnumeration
public enum EnumPrecompilerReservedWords implements AmritaConstants{

			
	NOT_ASSIGNED,															// 000 
	
	/////////////////////////////////////////////////////////////
    // Tipologie Istruzioni Cics                                      
    /////////////////////////////////////////////////////////////
	
    // Bms
	CICS_INSTR_SEND 			("SEND", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 001
	CICS_INSTR_RECEIVE		    ("RECEIVE", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 002
	CICS_INSTR_CONVERSE			("CONVERSE", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 003

	// File control
	CICS_INSTR_READ				("READ", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 004
	CICS_INSTR_WRITE			("WRITE", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 005
	CICS_INSTR_REWRITE			("REWRITE", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 006
	CICS_INSTR_DELETE			("DELETE", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 007
	CICS_INSTR_UNLOCK			("UNLOCK", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 008
	CICS_INSTR_STARTBR			("STARTBR", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 009
	CICS_INSTR_READNEXT			("READNEXT", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 010
	CICS_INSTR_READPREV			("READPREV", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 011
	CICS_INSTR_RESETBR			("RESETBR", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 012
	CICS_INSTR_ENDBR			("ENDBR", "", 		   	 	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 	// 013
 
	// Temporary storage/Transient data (TS o TD)
	CICS_INSTR_WRITEQ			("WRITEQ", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 014
	CICS_INSTR_READQ			("READQ", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 015
	CICS_INSTR_DELETEQ			("DELETEQ", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 016
	
	// Program control
	CICS_INSTR_LINK				("LINK", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 017
	CICS_INSTR_XCTL				("XCTL", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 018
	CICS_INSTR_RETURN			("RETURN", "", 		   	 	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 019
	CICS_INSTR_LOAD				("LOAD", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 020
	CICS_INSTR_RELEASE			("RELEASE", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 021
	
	// Interval control
	CICS_INSTR_START			("START", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 022
	CICS_INSTR_RETREIVE			("RETREIVE", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 023
	CICS_INSTR_CANCEL			("CANCEL", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 024
	CICS_INSTR_ASKTIME			("ASKTIME", "", 	    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 025
	CICS_INSTR_DELAY			("DELAY", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 026
	CICS_INSTR_POST				("POST", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 027
	CICS_INSTR_WAIT				("WAIT", "", 		    	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 028

	// Access to system information
	CICS_INSTR_ADDRESS			("ADDRESS", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 029
	CICS_INSTR_ASSIGN			("ASSIGN", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 030
	
	// Built in
	CICS_INSTR_FORMATTIME		("FORMATTIME", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 031
	CICS_INSTR_BIF_DEEDIT		("BIF", "DEEDIT", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 032
	
	
	// Abnormal termination recovery/Handle condition
	CICS_INSTR_ABEND			("ABEND", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 033
	CICS_INSTR_HANDLE_ABEND  	("HANDLE", "ABEND", 		EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 034
	CICS_INSTR_HANDLE_AID		("HANDLE", "AID", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 035
	CICS_INSTR_HANDLE_CONDITION	("HANDLE", "CONDITION", 	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 036
	CICS_INSTR_IGNORE_CONDITION ("IGNORE", "CONDITION", 	EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 037
	
	// Trace control
	CICS_INSTR_ENTER			("ENTER", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 038
	CICS_INSTR_TRACE			("TRACE", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 039
	
	// Storage control
	CICS_INSTR_GETMAIN			("GETMAIN", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 040
	CICS_INSTR_FREEMAIN			("FREEMAIN", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 041
	
	// task control
	CICS_INSTR_SUSPEND			("SUSPEND", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 042
	CICS_INSTR_ENQ				("ENQ", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 043
	CICS_INSTR_DEQ				("DEQ", "", 				EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 044
	
	// Syncpoint control
	CICS_INSTR_SYNCPOINT		("SYNCPOINT", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 045

	// Varie non gestite
	CICS_INSTR_ALLOCATE		    ("ALLOCATE", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 045
	CICS_INSTR_FREE		        ("FREE", "", 			    EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 045
	CICS_INSTR_CONNECT		    ("CONNECT", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 045
	CICS_INSTR_INQUIRE		    ("INQUIRE", "", 			EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// 045
	CICS_INSTR_SET		        ("SET", "", 			    EnumInstrPrecompilerType.EXEC_CICS_INSTRUCTION), 			// XXX

	/////////////////////////////////////////////////////////////
    // Opzioni istruzioni Cics                           
    /////////////////////////////////////////////////////////////

	CICS_OPTION_TEXT("TEXT", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 046
	CICS_OPTION_GTEQ("GTEQ", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 047
	CICS_OPTION_EQUAL("EQUAL", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 048
	CICS_OPTION_ERASE("ERASE", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 049
	CICS_OPTION_NEXT("NEXT", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 050
	CICS_OPTION_REWRITE("REWRITE", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 051
	CICS_OPTION_AUXILIARY("AUXILIARY", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 052
	CICS_OPTION_DATAONLY("DATAONLY", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPTION),			// 053
	CICS_OPTION_MAPONLY("MAPONLY", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 054
	CICS_OPTION_ALARM("ALARM", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 055
	CICS_OPTION_WAIT("WAIT", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 056
	CICS_OPTION_ERASEAUP("ERASEAUP", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 057
	CICS_OPTION_FREEKB("FREEKB", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 058
	CICS_OPTION_FRSET("FRSET", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 059
	CICS_OPTION_ACCUM("ACCUM", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 060
	CICS_OPTION_ASIS("ASIS", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 061
	CICS_OPTION_GENERIC("GENERIC", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 062
	CICS_OPTION_CURSOR("CURSOR", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 063
	CICS_OPTION_UPDATE("UPDATE", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 064
	CICS_OPTION_HOLD("HOLD", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 065
	CICS_OPTION_MAIN("MAIN", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 066
	CICS_OPTION_RESET("RESET", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 067
	CICS_OPTION_CANCEL("CANCEL", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 068
	CICS_OPTION_NOHANDLE("NOHANDLE", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 069
	CICS_OPTION_ROLLBACK("ROLLBACK", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 070
	CICS_OPTION_ON("ON", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 071
	CICS_OPTION_OFF("OFF", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 072
	CICS_OPTION_ALL("ALL", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 073
	CICS_OPTION_SINGLE("SINGLE", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 074
	CICS_OPTION_SYSTEM("SYSTEM", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 075
	CICS_OPTION_F1("F1", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 076
	CICS_OPTION_BF("BF", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 077
	CICS_OPTION_BM("BM", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 078
	CICS_OPTION_DC("DC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 079
	CICS_OPTION_DI("DI", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 080
	CICS_OPTION_EC("EC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 081
	CICS_OPTION_IC("IC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 082
	CICS_OPTION_JC("JC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 083
	CICS_OPTION_KC("KC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 084
	CICS_OPTION_PC("PC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 085
	CICS_OPTION_SC("SC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 086
	CICS_OPTION_TC("TC", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 087
	CICS_OPTION_TD("TD", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 088
	CICS_OPTION_TS("TS", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 089
	CICS_OPTION_EI("EI", "", 								EnumInstrPrecompilerType.EXEC_CICS_OPTION), 		// 090


	
	/////////////////////////////////////////////////////////////
    // Operandi Istruzioni Cics                                  
    /////////////////////////////////////////////////////////////
    
	// Indicano campi rilevanti ai fini delle relazioni
	CICS_OPERAND_MAP("MAP", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 091
	CICS_OPERAND_MAPSET("MAPSET", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 092
	CICS_OPERAND_FILE("FILE", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 093
	CICS_OPERAND_PROGRAM("PROGRAM", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 094
	CICS_OPERAND_TRANSID("TRANSID", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 095
	CICS_OPERAND_INTO("INTO", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND, true), 	// 096
	CICS_OPERAND_FROM("FROM", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 097
	CICS_OPERAND_ITEM("ITEM", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 098
	CICS_OPERAND_COMMAREA("COMMAREA", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 099
	CICS_OPERAND_QUEUE("QUEUE", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 100
	CICS_OPERAND_QNAME("QNAME", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 101
	CICS_OPERAND_RIDFLD("RIDFLD", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 102
	CICS_OPERAND_LENGTH("LENGTH", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 103
	CICS_OPERAND_KEYLENGTH("KEYLENGTH", "", 				EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 104
	CICS_OPERAND_ABCODE("ABCODE", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 105
	CICS_OPERAND_CURSOR("CURSOR", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 106
	CICS_OPERAND_NUMREC("NUMREC", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 107
	CICS_OPERAND_TERMID("TERMID", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 108
	CICS_OPERAND_USING("USING", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND), 		// 109
	
	// Indicano campi non rilevanti ai fine delle relazioni
//	CICS_OPERAND_ECADDR("ECADDR", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 109
	CICS_OPERAND_INTERVAL("INTERVAL", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 110
	CICS_OPERAND_SET("SET", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 111
	CICS_OPERAND_HEADER("HEADER", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 112
	CICS_OPERAND_FROMLENGTH("FROMLENGTH", "", 				EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 113
	CICS_OPERAND_TOLENGTH("TOLENGTH", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 114
	CICS_OPERAND_REQID("REQID", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 115
	CICS_OPERAND_LABEL("LABEL", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 116
	CICS_OPERAND_TRACEID("TRACEID", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 117
	CICS_OPERAND_RESOURCE("RESOURCE", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 118
	CICS_OPERAND_DATA("DATA", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 119
	CICS_OPERAND_INITIMG("INITIMG", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 120
	CICS_OPERAND_RESP("RESP", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// XXX

	// Indicano campi di sistema Cics e in generale non rilevanti
	CICS_OPERAND_TRIGGER("TRIGGER", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 121
	CICS_OPERAND_CSA("CSA", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 122
	CICS_OPERAND_CWA("CWA", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 123
	CICS_OPERAND_TWA("TWA", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 124
	CICS_OPERAND_EIB("EIB", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 125
	CICS_OPERAND_TCTUA("TCTUA", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 126
	CICS_OPERAND_CWALENG("CWALENG", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 127
	CICS_OPERAND_TCTUALENG("TCTUALENG", "", 				EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 128
	CICS_OPERAND_TWALENG("TWALENG", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 129
	CICS_OPERAND_TERMCODE("TERMCODE", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 130
	CICS_OPERAND_FIELD("FIELD", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 131
	CICS_OPERAND_ABSTIME("ABSTIME", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 132
	CICS_OPERAND_YYDDD("YYDDD", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 133
	CICS_OPERAND_YYDDMM("YYDDMM", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 134
	CICS_OPERAND_MMDDYY("MMDDYY", "", 						EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 135
	CICS_OPERAND_DATE("DATE", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 136
	CICS_OPERAND_DATEFORM("DATEFORM", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 137
	CICS_OPERAND_DATESEP("DATESEP", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 138
	CICS_OPERAND_DAYCOUNT("DAYCOUNT", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 139
	CICS_OPERAND_DAYOFWEEK("DAYOFWEEK", "", 				EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 140
	CICS_OPERAND_DAYOFMONTH("DAYOFMONTH", "", 				EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 141
	CICS_OPERAND_MONTHOFYEAR("MONTHOFYEAR", "", 			EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 142
	CICS_OPERAND_YEAR("YEAR", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 143
	CICS_OPERAND_TIME("TIME", "", 							EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 144
	CICS_OPERAND_TIMESEP("TIMESEP", "", 					EnumInstrPrecompilerType.EXEC_CICS_OPERAND),      	// 145
	
	// Indicano le condizioni di exception alle quali possono essere associate label rilevanti per il grafo di programma
	CICS_EXCEPTION_DSIDERR("DSIDERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 146
	CICS_EXCEPTION_DSSTAT("DSSTAT", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 147
	CICS_EXCEPTION_DUPKEY("DUPKEY", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 148
	CICS_EXCEPTION_DUPREC("DUPREC", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 149
	CICS_EXCEPTION_ENDDATA("ENDDATA", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 150
	CICS_EXCEPTION_ENDFILE("ENDFILE", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 151
	CICS_EXCEPTION_ENDINPT("ENDINPT", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 152
	CICS_EXCEPTION_ENQBUSY("ENQBUSY", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 153
	CICS_EXCEPTION_ENVDEFERR("ENVDEFERR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 154
	CICS_EXCEPTION_EOC("EOC", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 155
	CICS_EXCEPTION_EODS("EODS", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 156
	CICS_EXCEPTION_EOF("EOF", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 157
	CICS_EXCEPTION_ERROR("ERROR", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 158
	CICS_EXCEPTION_EXPIRED("EXPIRED", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 159
	CICS_EXCEPTION_FUNCERR("FUNCERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 160
	CICS_EXCEPTION_IGREQCD("IGREQCD", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 161
	CICS_EXCEPTION_IGREQID("IGREQID", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 162
	CICS_EXCEPTION_ILLOGIC("ILLOGIC", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 163
	CICS_EXCEPTION_INBFMH("INBFMH", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 164
	CICS_EXCEPTION_INVERRTERM("INVERRTERM", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 165
	CICS_EXCEPTION_INVLDC("INVLDC", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 166
	CICS_EXCEPTION_INVMPSZ("INVMPSZ", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 167
	CICS_EXCEPTION_INVPARTN("INVPARTN", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 168
	CICS_EXCEPTION_INVPARTNSET("INVPARTNSET", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 169
	CICS_EXCEPTION_INVREQ("INVREQ", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 170
	CICS_EXCEPTION_INVTSREQ("INVTSREQ", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 171
	CICS_EXCEPTION_IOERR("IOERR", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 172
	CICS_EXCEPTION_ISCINVREQ("ISCINVREQ", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 173
	CICS_EXCEPTION_ITEMERR("ITEMERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 174
	CICS_EXCEPTION_JIDERR("JIDERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 175
	CICS_EXCEPTION_LENGERR("LENGERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),    	// 176
	CICS_EXCEPTION_MAPERROR("MAPERROR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 177
	CICS_EXCEPTION_MAPFAIL("MAPFAIL", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 178
	CICS_EXCEPTION_NAMEERROR("NAMEERROR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 179
	CICS_EXCEPTION_NOJBUFSP("NOJBUFSP", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 180
	CICS_EXCEPTION_NONVAL("NONVAL", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 181
	CICS_EXCEPTION_NOPASSBKED("NOPASSBKED", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 182
	CICS_EXCEPTION_NOSPACE("NOSPACE", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 183
	CICS_EXCEPTION_NOSPOOL("NOSPOOL", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 184
	CICS_EXCEPTION_NOSTART("NOSTART", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 185
	CICS_EXCEPTION_NOSTG("NOSTG", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 186
	CICS_EXCEPTION_NOTALLOC("NOTALLOC", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 187
	CICS_EXCEPTION_NOTAUTH("NOTAUTH", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 188
	CICS_EXCEPTION_NOTFND("NOTFND", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 188
	CICS_EXCEPTION_NOTOPEN("NOTOPEN", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 190
	CICS_EXCEPTION_OVERFLOW("OVERFLOW", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 191
	CICS_EXCEPTION_PARTNFAIL("PARTNFAIL", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 192
	CICS_EXCEPTION_PGMIDERR("PGMIDERR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 193
	CICS_EXCEPTION_QBUSY("QBUSY", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 194
	CICS_EXCEPTION_QIDERR("QIDERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 195
	CICS_EXCEPTION_QZERO("QZERO", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 196
	CICS_EXCEPTION_RDATT("RDATT", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 197
	CICS_EXCEPTION_RETPAGE("RETPAGE", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 198  
	CICS_EXCEPTION_ROLLEDBACK("ROLLEDBACK", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 199
	CICS_EXCEPTION_RTEFAIL("RTEFAIL", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 200
	CICS_EXCEPTION_RTESOME("RTESOME", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 201
	CICS_EXCEPTION_SELNERR("SELNERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 202
	CICS_EXCEPTION_SESSBUSY("SESSBUSY", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 203
	CICS_EXCEPTION_SESSIONERR("SESSIONERR", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 204
	CICS_EXCEPTION_SIGNAL("SIGNAL", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 205
	CICS_EXCEPTION_SYSBUSY("SYSBUSY", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 206
	CICS_EXCEPTION_SYSIDERR("SYSIDERR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 207
	CICS_EXCEPTION_TERMERR("TERMERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 208
	CICS_EXCEPTION_TERMIDERR("TERMIDERR", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 209
	CICS_EXCEPTION_TRANSIDERR("TRANSIDERR", "", 			EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 210
	CICS_EXCEPTION_TSIOERR("TSIOERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 211
	CICS_EXCEPTION_UNEXPIN("UNEXPIN", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 212
	CICS_EXCEPTION_WRONGSTAT("WRONGSTAT", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 213
	CICS_EXCEPTION_ANYKEY("ANYKEY", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 214
	CICS_EXCEPTION_CLEAR("CLEAR", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 215
	CICS_EXCEPTION_CLRPARTN("CLRPARTN", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 216
	CICS_EXCEPTION_ENTER("ENTER", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 217
	CICS_EXCEPTION_LIGHTPEN("LIGHTPEN", "", 				EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 218
	CICS_EXCEPTION_OPERID("OPERID", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 219
	CICS_OPERAND_CBIDERR("CBIDERR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 220
	CICS_OPERAND_CCERROR("CCERROR", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 221
	CICS_OPERAND_DISABLED("DISABLED", "", 					EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 222
	CICS_EXCEPTION_PA1("PA1", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 223
	CICS_EXCEPTION_PA2("PA2", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 224
	CICS_EXCEPTION_PA3("PA3", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 225
	CICS_EXCEPTION_PF1("PF1", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 226
	CICS_EXCEPTION_PF2("PF2", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 227
	CICS_EXCEPTION_PF3("PF3", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 228
	CICS_EXCEPTION_PF4("PF4", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 229
	CICS_EXCEPTION_PF5("PF5", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 230
	CICS_EXCEPTION_PF6("PF6", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 231
	CICS_EXCEPTION_PF7("PF7", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 232
	CICS_EXCEPTION_PF8("PF8", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 233
	CICS_EXCEPTION_PF9("PF9", "", 							EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 234
	CICS_EXCEPTION_PF10("PF10", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 235
	CICS_EXCEPTION_PF11("PF11", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 236
	CICS_EXCEPTION_PF12("PF12", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 237
	CICS_EXCEPTION_PF13("PF13", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 238
	CICS_EXCEPTION_PF14("PF14", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 239
	CICS_EXCEPTION_PF15("PF15", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 240
	CICS_EXCEPTION_PF16("PF16", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 241
	CICS_EXCEPTION_PF17("PF17", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 242
	CICS_EXCEPTION_PF18("PF18", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 243
	CICS_EXCEPTION_PF19("PF19", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 244
	CICS_EXCEPTION_PF20("PF20", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 245
	CICS_EXCEPTION_PF21("PF21", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 246
	CICS_EXCEPTION_PF22("PF22", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 247
	CICS_EXCEPTION_PF23("PF23", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 248
	CICS_EXCEPTION_PF24("PF24", "", 						EnumInstrPrecompilerType.EXEC_CICS_EXCEPTION),      // 249

	
	
	
	/////////////////////////////////////////////////////////////
    // Istruzioni Sql e relative opzioni                       //
    /////////////////////////////////////////////////////////////
    
	// Istruzioni Sql DML principali gestite ai fini di oggetti e relazioni
	SQL_SELECT("SELECT", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_INSERT("INSERT", "INTO", "", "", 										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_UPDATE("UPDATE", "", "", "", 											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DELETE("DELETE", "FROM", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_MERGE("MERGE", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DECLARE_CURSOR("DECLARE", "*"
			         , "|ASENSITIVE SCROLL|INSENSITIVE SCROLL|SENSITIVE SCROLL|SENSITIVE STATIC SCROLL|SENSITIVE DYNAMIC SCROLL|SCROLL|NO SCROLL"
			         , "CURSOR",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_OPEN("OPEN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_CLOSE("CLOSE", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_FETCH("FETCH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_COMMIT("COMMIT", "|WORK", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_ROLLBACK("ROLLBACK", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SAVEPOINT("SAVEPOINT", "", "", "", 										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_INCLUDE("INCLUDE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_BEGIN_DECLARE_SECTION("BEGIN", "DECLARE", "SECTION", "", 				EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_END_DECLARE_SECTION("END", "DECLARE","SECTION", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_PREPARE("PREPARE", "", "", "",				                			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_EXECUTE("EXECUTE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_EXECUTE_IMMEDIATE("EXECUTE", "IMMEDIATE", 							    EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_LOCK_TABLE("LOCK", "TABLE", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	
	// Istruzioni Sql non rilevanti ai fini di oggetti e relazioni
	SQL_DECLARE_STATEMENT("DECLARE", "*", "STATEMENT", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DECLARE_TABLE("DECLARE", "*", "TABLE", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DECLARE_VARIABLE("DECLARE", "*", "VARIABLE", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_CALL("CALL", "", "", "", 							        			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_ALLOCATE_CURSOR("ALLOCATE", "", 							            EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_ASSOCIATE_LOCATORS("ASSOCIATE", "|RESULT SET", "LOCATOR|LOCATORS", "", 	EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_FREE_LOCATOR("FREE", "LOCATOR", 										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_HOLD_LOCATOR("HOLD", "LOCATOR", "", "",						            EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_RELEASE_CONNECTION("RELEASE", "", "", "", 								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_RELEASE_SAVEPOINT("RELEASE", "|TO", "SAVEPOINT", "", 					EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DECLARE_GLOBAL_TEMPORARY_TABLE("DECLARE", "GLOBAL","TEMPORARY","TABLE", EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DESCRIBE("DESCRIBE", "", "", "", 										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DESCRIBE_CURSOR("DESCRIBE", "CURSOR","","", 							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DESCRIBE_INPUT("DESCRIBE", "INPUT","","", 								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_DESCRIBE_PROCEDURE("DESCRIBE", "PROCEDURE","","", 						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_CONNECT("CONNECT", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CONNECTION("SET", "CONNECTION", "", "", 							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_APPLICATION_ENCODING_SCHEMA("SET", "CURRENT","|APPLICATION","ENCODING SCHEME", EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_DEGREE("SET", "CURRENT", "DEGREE", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_LOCALE_LC_CTYPE("SET", "LC_CTYPE|CURRENT LC_TYPE|CURRENT LOCALE LC_TYPE",	EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_OPTIMIZATION_HINT("SET", "CURRENT", "OPTIMIZATION", "HINT",	EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_PACKAGESET("SET", "CURRENT", "PACKAGESET", "",				EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_PRECISION("SET", "CURRENT", "PRECISION", "",				EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_RULES("SET", "CURRENT", "RULES", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_SET_CURRENT_SQLID("SET", "CURRENT", "SQLID", "",						EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000
	SQL_WHENEVER("WHENEVER", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DML), 		// 000

	// Istruzioni Sql DDL embedded in programmi o codificate in script
	SQL_CREATE_DATABASE("CREATE", "DATABASE", "", "", 							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_STOGROUP("CREATE", "STOGROUP", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_TABLESPACE("CREATE", "|LARGE|LOB", "TABLESPACE", "",				EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_TABLE("CREATE", "TABLE", "", "", 								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_AUXILIARY_TABLE("CREATE", "AUXILIARY|AUX", "TABLE", "", 			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_GLOBAL_TEMPORARY_TABLE("CREATE", "GLOBAL", "TEMPORARY", "TABLE",	EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_VIEW("CREATE", "VIEW", "", "", 									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_ALIAS("CREATE", "ALIAS", "", "", 								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_SYNONIM("CREATE", "SYNONYM", "", "", 							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL),			// 000
	SQL_CREATE_INDEX("CREATE", "|UNIQUE", "|WHERE NOT NULL", "INDEX",			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_TRIGGER("CREATE", "TRIGGER", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_PROCEDURE("CREATE", "PROCEDURE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_FUNCTION("CREATE", "FUNCTION", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_DISTINCT_TYPE("CREATE", "DISTINCT", "TYPE", "",					EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_SEQUENCE("CREATE", "SEQUENCE", "", "",					        EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_COMMENT_ON("COMMENT", "ON", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_LABEL_ON("LABEL", "ON", "", "",						        			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_DATABASE("ALTER", "DATABASE", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_TABLESPACE("ALTER", "TABLESPACE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_TABLE("ALTER", "TABLE", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_FUNCTION("ALTER", "FUNCTION|SPECIFIC FUNCTION", "", "",			EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_INDEX("ALTER", "INDEX", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_PROCEDURE("ALTER", "PROCEDURE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_STOGROUP("ALTER", "STOGROUP", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_ALTER_SEQUENCE("ALTER", "SEQUENCE", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_RENAME("RENAME", "|TABLE", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_GRANT("GRANT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_REVOKE("REVOKE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_DROP("DROP", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_EXPLAIN("EXPLAIN", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_SET_HOSTVAR("SET", "*", "CURRENT SERVER|CURRENT PACKAGESET", "",		EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_SET_PATH("SET", "PATH|CURRENT_PATH|CURRENT PATH|CURRENT FUNCTION PATH",	EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_SIGNAL_SQLSTATE("SIGNAL", "SQLSTATE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_VALUES("VALUES", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_GET_DIAGNOSTIC("GET", "DIAGNOSTIC", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000

	// Istruzioni Sql DDL embedded in programmi o codificate in script per DB2/AIX64 Version 9.1.9   
	SQL_CREATE_WRAPPER("CREATE", "WRAPPER", "", "",					            EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_SERVER("CREATE", "SERVER", "", "",					            EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000
	SQL_CREATE_USER_MAPPING("CREATE", "USER", "MAPPING", "",					EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_DDL), 		// 000

	// Sql procedure statements (dentro Create Procedure/trigger/routines)
	SQL_PROCEDURE_DECLARE("DECLARE", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_SET("SET", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_LABEL("*:", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_GET_DIAGNOSTICS("GET", "DIAGNOSTICS", "", "",					EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_BEGIN("BEGIN", "|NOT ATOMIC|ATOMIC", "", "",					EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_CALL("CALL", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_SIGNAL("SIGNAL", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_RESIGNAL("RESIGNAL", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_LOOP("LOOP", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_REPEAT("REPEAT", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_ITERATE("ITERATE", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_WHILE("WHILE", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
 	SQL_PROCEDURE_FOR_EACH("FOR", "EACH", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
 	SQL_PROCEDURE_FOR_LOOP("FOR", "", "", "",								    EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
 	SQL_PROCEDURE_DO("DO", "", "", "",								    		EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_GOTO("GOTO", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_LEAVE("LEAVE", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_RETURN("RETURN", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_IF("IF", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_ELSE("ELSE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_ELSEIF("ELSEIF", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_CASE("CASE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_WHEN("WHEN", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END("END", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_IF("END", "IF", "", "",									EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_CASE("END", "CASE", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_FOR("END", "FOR", "", "",							        EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_LOOP("END", "LOOP", "", "",							    EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_WHILE("END", "WHILE", "", "",								EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000
	SQL_PROCEDURE_END_REPEAT("END", "REPEAT", "", "",							EnumInstrPrecompilerType.EXEC_SQL_INSTRUCTION_PROCEDURE), 		// 000

	// Special Sql register
	SQL_CURRENT_APPLICATION_ENCODING_SCHEME("CURRENT", "APPLICATION", "ENCODING", "SCHEME", EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_CLIENT_ACCTNG("CURRENT", "CLIENT_ACCTNG", "", "",				EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_CLIENT_APPLNAME("CURRENT", "CLIENT_APPLNAME", "", "",			EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_CLIENT_USERID("CURRENT", "CLIENT_USERID", "", "",				EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_CLIENT_WRKSTNNAME("CURRENT", "CLIENT_WRKSTNNAME", "", "",		EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_DEBUG_MODE("CURRENT", "DEBUG", "MODE", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_DECFLOAT_ROUNDING_MODE("CURRENT", "DECFLOAT", "ROUNDING","MODE",EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_EXPLAIN_MODE("CURRENT", "EXPLAIN", "MODE", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_MAINTAINED_TABLE("CURRENT", "MAINTAINED", "|TABLE", "",			EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_DATE("CURRENT", "DATE", "", "",									EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_DATE2("CURRENT_DATE", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER, SQL_CURRENT_DATE), 		// 000
	SQL_CURRENT_DEGREE("CURRENT", "DEGREE", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_LC_TYPE("CURRENT", "|LOCALE", "LC_TYPE", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_LC_TYPE2("CURRENT_LOCALE", "", "LC_TYPE", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER, SQL_CURRENT_LC_TYPE), 		// 000
	SQL_CURRENT_MEMBER("CURRENT", "MEMBER", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_OPTIMIZATION_HINT("CURRENT", "OPTIMIZATION", "HINT", "",		EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_PACKAGE_PATH("CURRENT", "PACKAGE", "PATH", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_PACKAGESET("CURRENT", "PACKAGESET", "", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_PATH("CURRENT|CURRENT_PATH", "|PATH", "", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_PRECISION("CURRENT", "PRECISION", "", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_REFRESH_AGE("CURRENT", "REFRESH", "AGE", "",					EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_ROUTINE_VERSION("CURRENT", "ROUTINE", "VERSION", "",			EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_RULES("CURRENT", "RULES", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_SCHEMA("CURRENT", "SCHEMA", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_SCHEMA2("CURRENT_SCHEMA", "SCHEMA", "", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER, SQL_CURRENT_SCHEMA), 		// 000
	SQL_CURRENT_SERVER("CURRENT", "SERVER", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_SQLID("CURRENT", "SQLID", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_TIME("CURRENT", "TIME", "", "",									EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_TIME2("CURRENT_TIME", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER, SQL_CURRENT_TIME), 		// 000
	SQL_CURRENT_TIMESTAMP("CURRENT", "TIMESTAMP", "", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_TIMESTAMP2("CURRENT_TIMESTAMP", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER, SQL_CURRENT_TIMESTAMP), 		// 000
	SQL_CURRENT_TIMEZONE("CURRENT", "TIMEZONE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_SESSION_TIMEZONE("SESSION", "TIMEZONE", "", "",							EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_CURRENT_USER("USER|SESSION_USER", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000
	SQL_TIMESTAMP("TIMESTAMP", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_SPECIAL_REGISTER), 		// 000

	// Datetime operands and durations (possono seguire function, (expression), constants, column-name, host variable) 
	SQL_YEAR("YEAR", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_YEARS("YEARS", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MONTH("MONTH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MONTHS("MONTHS", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_DAY("DAY", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_DAYS("DAYS", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_HOUR("HOUR", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_HOURS("HOURS", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MINUTE("MINUTE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MINUTES("MINUTES", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_SECOND("SECOND", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_SECONDS("SECONDS", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MICROSECOND("MICROSECOND", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	SQL_MICROSECONDS("MICROSECONDS", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_LABELED_DURATION), 		// 000
	
	// Operators
	SQL_OPRT_ADD("+", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.ADD), 		// 000
	SQL_OPRT_SUB("-", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.SUB), 		// 000
	SQL_OPRT_MULT("*", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.MULT), 	// 000
	SQL_OPRT_DIV("/", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.DIV), 		// 000
	SQL_OPRT_EQ("=", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.EQ), 		// 000
	SQL_OPRT_GE(">=", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.GE), 		// 000
	SQL_OPRT_GT(">", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.GT), 		// 000
	SQL_OPRT_LT("<", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.LT), 		// 000
	SQL_OPRT_LE("<=", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.LE), 		// 000
	SQL_OPRT_NE("<>", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.NE), 		// 000
	SQL_OPRT_PAR_OPEN("(", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.PAR_OPEN), // 000
	SQL_OPRT_PAR_CLOSE(")", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.PAR_CLOSE),// 000
	SQL_OPRT_AND("AND", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.AND), 	// 000
	SQL_OPRT_OR("OR", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.OR), 		// 000
	SQL_OPRT_NOT("NOT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_OPERATOR, EnumSqlOperator.NOT), 	// 000

	
	// Predicate
	SQL_PREDICATE_BETWEEN("BETWEEN", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.BETWEEN), // 000
	SQL_PREDICATE_DISTINCT("DISTINCT", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.DISTINCT), 	// 000
	SQL_PREDICATE_EXISTS("EXISTS", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.EXISTS), 	// 000
	SQL_PREDICATE_IN("IN", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.IN), 		// 000
	SQL_PREDICATE_LIKE("LIKE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.LIKE), 	// 000
	SQL_PREDICATE_NULL("NULL", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.NULL), 	// 000
	SQL_PREDICATE_XMLEXISTS("XMLEXISTS", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_PREDICATE, EnumSqlPredicate.XMLEXISTS), 	// 000

	// Keywords varie e opzioni varie
	SQL_WHERE("WHERE", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_FROM("FROM", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ORDER_BY("ORDER", "BY", "", "",											EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_GROUP_BY("GROUP", "BY", "", "",											EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_HAVING("HAVING", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_UNION("UNION", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_EXCEPT("EXCEPT", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_INTERSECT("INTERSECT", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_UNION_ALL("UNION", "ALL", "", "",										EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_DISTINCT("DISTINCT", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_AS("AS", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_INNER("INNER", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_OUTER("OUTER", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_FULL("FULL", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_JOIN("JOIN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_LEFT("LEFT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_RIGHT("RIGHT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_OF("OF", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_SOME("SOME", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ANY("ANY", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ALL("ALL", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ASC("ASC", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_DESC("DESC", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_READ("READ", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_UPDATE_KEY("UPDATE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ONLY("ONLY", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_SYSTEM_TIME("SYSTEM_TIME", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_BUSINESS_TIME("SYSTEM_TIME", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_OPTIMIZE_FOR("OPTIMIZE", "FOR", "", "",									EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_ROW("ROWS|ROW", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_WITH("WITH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_QUERYNO("QUERYNO", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 			// 000
	SQL_PROCEDURE_THEN("THEN", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_KEYWORD), 		// 000
	
	// Functions column aggregate
	SQL_MAX("MAX", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_MIN("MIN", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_AVG("AVG", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_SUM("SUM", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_COUNT("COUNT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_COUNT_BIG("COUNT_BIG", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_STDDEV("STDDEV|STDDEV_POP", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_STDDEV_SAMP("STDDEV_SAMP", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_VARIANCE("VARIANCE|VAR|VAR_POP", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_VARIANCE_SAMP("VARIANCE_SAMP|VAR_SAMP", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_COVARIANCE("COVARIANCE", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	SQL_CORRELATION("CORRELATION", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION, true), 		// 000
	
	// Functions scalar
	SQL_MIN_FUNC("MIN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MAX_FUNC("MAX", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ABS("ABS|ABSVAL", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ACOS("ACOS", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ADD_MONTHS("ADD_MONTHS", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ASIN("ASIN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ATAN("ATAN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ATANH("ATANH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ATAN2("ATAN2", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_BLOB("BLOB", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CCSID_ENCODING("CCSID_ENCODING", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CEIL("CEIL|CEILING", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CHAR("CHAR", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CLOB("CLOB", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_COALESCE("COALESCE|VALUE", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CONCAT("CONCAT|||", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_COS("COS", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_COSH("COSH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DATE("DATE", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAY_FUNC("DAY", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYOFMONTH("DAYOFMONTH", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYOFWEEK("DAYOFWEEK", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYOFWEEK_ISO("DAYOFWEEK_ISO", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYOFYEAR("DAYOFYEAR", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYSDAYS("DAYS", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DBCLOB("DBCLOB", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DECIMAL("DECIMAL|DEC", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DEGREES("DEGREES", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DIGITS("DIGITS", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DOUBLE("DOUBLE|DOUBLE_PRECISION|FLOAT", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_EXP("EXP", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_FLOAT("FLOAT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_FLOOR("FLOOR", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_GRAPHIC("GRAPHIC", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_HEX("HEX", "", "", "",													EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_HOUR_FUNC("HOUR", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_IDENTITY_VAL_LOCAL("IDENTITY_VAL_LOCAL", "", "", "",					EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_IFNULL("IFNULL", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_INSERT_FUNC("INSERT", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_INTEGER("INTEGER|INT", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_JULIAN_DAY("JULIAN_DAY", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LAST_DAY("LAST_DAY", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LCASE("LCASE|LOWER", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LEFT_FUNC("LEFT", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LENGTH("LENGTH", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LN("LN", "", "", "",												    EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LOCATE("LOCATE", "", "", "",										 	EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LOG10("LOG10", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_LTRIM("LTRIM", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MICROSECOND_FUNC("MICROSECOND", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
 	SQL_MIDNIGHT_SECONDS("MIDNIGHT_SECONDS", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MINUTE_FUNC("MINUTE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MOD("MOD", "", "", "",												    EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MONTH_FUNC("MONTH", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQREAD("MQREAD", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQREADCLOB("MQREADCLOB", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQRECEIVE("MQRECEIVE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQRECEIVECLOB("MQRECEIVECLOB", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQSEND("MQSEND", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MULTIPLY_ALT("MULTIPLY_ALT", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_NEXT_DAY("NEXT_DAY", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_NULLIF("NULLIF", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_POSSTR("POSSTR", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_POWER("POWER", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_QUARTER("QUARTER", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_RADIANS("RADIANS", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_RAISE_ERROR("RAISE_ERROR", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_RAND("RAND", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_REAL("REAL", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_REPLACE("REPLACE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_RIGHT_FUNC("RIGHT", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ROUND("ROUND", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ROUND_TIMESTAMP("ROUND_TIMESTAMP", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ROWID("ROWID", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_RTRIM("RTRIM", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SECOND_FUNC("SECOND", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SIGN("SIGN", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SIN("SIN", "", "", "",												    EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SINH("SINH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SMALLINT("SMALLINT", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SPACE("SPACE", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SQRT("SQRT", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_STRIP("STRIP", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_SUBSTR("SUBSTR", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TAN("TAN", "", "", "",												    EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TANH("TANH", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TIME("TIME", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TIMESTAMP_FORMAT("TIMESTAMP_FORMAT", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TRANSLATE("TRANSLATE", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TRUNCATE("TRUNCATE|TRUNC", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TRUNC_TIMESTAMP("TRUNC_TIMESTAMP", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_UCASE("UCASE|UPPER", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_VARCHAR("VARCHAR", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_VARGRAPHIC("VARGRAPHIC", "", "", "",									EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_WEEK("WEEK", "", "", "",												EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_WEEK_ISO("WEEK_ISO", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CAST("CAST", "", "", "",											    EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_XMLCAST("XMLCAST", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	
	// Functions table
	SQL_MQREADALL("MQREADALL", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQREADALLCLOB("MQREADALLCLOB", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQRECEIVEALL("MQRECEIVEALL", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MQRECEIVEALLCLOB("MQRECEIVEALLCLOB", "", "", "",						EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000

	// Functions user-defined (richiamate anche come dsname.functionName
	SQL_ALTDATE("ALTDATE", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_ALTTIME("ALTTIME", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_CURRENCY("CURRENCY", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_DAYNAME("DAYNAME", "", "", "",											EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_MONTHNAME("MONTHNAME", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TABLE_LOCATION("TABLE_LOCATION", "", "", "",							EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TABLE_NAME("ABLE_NAME", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_TABLE_SCHEMA("TABLE_SCHEMA", "", "", "",								EnumInstrPrecompilerType.EXEC_SQL_FUNCTION), 		// 000
	SQL_WHEATHER("WHEATHER", "", "", "",										EnumInstrPrecompilerType.EXEC_SQL_FUNCTION); 		// 000


	/////////////////////////////////////////////////////////////
    // Istruzioni Dl1 e relative opzioni                       //
    /////////////////////////////////////////////////////////////
    
	/////////////////////////////////////////////////////////////
    // Istruzioni Adabas                                       //
    /////////////////////////////////////////////////////////////
	
		
		
		//////////////////////////////////////////////////////////////////////////////////////
		// Variabili di istanza per ogni entry
		//////////////////////////////////////////////////////////////////////////////////////
		
		
		String valueText1 = "";																		// Identifica il tipo di istruzione
		String valueText2 = "";																		// Identifica il tipo di istruzione
		String valueText3 = "";																		// Identifica il tipo di istruzione
		String valueText4 = "";																		// Identifica il tipo di istruzione
		EnumPrecompilerReservedWords enumAlias = null;                      						// Tipo entry di riferimento con lo stesso significato
		EnumPrecompilerReservedWords ar_optionInstruction[] = new EnumPrecompilerReservedWords[0] ; // Entry che sono opzioni per l'istruzione ma identificano specifiche istruzioni
		EnumInstrPrecompilerType typeEntry = EnumInstrPrecompilerType.NOT_ASSIGNED;					// Vedi AmritaContants
		EnumSqlOperator typeOperator = EnumSqlOperator.NOT_ASSIGNED;
		EnumSqlPredicate typePredicate = EnumSqlPredicate.NOT_ASSIGNED;
		boolean outputOperand = false;              												// True indica operando aggiornato dall'istruzione
		boolean isAggregateFunction = false;                                                        // True indica funzione sql aggregata
		
		/*
	     * 
	     * Costruttore vuoto
	     * 
	     */
		EnumPrecompilerReservedWords(){
		}

		
		/*
	     * 
	     * Costruttore con valore corretto di inizio ExecCics
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, EnumInstrPrecompilerType typeEntry){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.typeEntry = typeEntry;
		}

		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, boolean isAggregateFunction){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
			this.isAggregateFunction = isAggregateFunction;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, EnumPrecompilerReservedWords ar_optionInstruction[]){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
			this.ar_optionInstruction = ar_optionInstruction;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, EnumSqlOperator typeOperator){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
			this.typeOperator = typeOperator;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, EnumSqlPredicate typePredicate){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
			this.typePredicate = typePredicate;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, EnumSqlOperator typeOperator, EnumPrecompilerReservedWords ar_optionInstruction[]){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.typeEntry = typeEntry;
			this.typeOperator = typeOperator;
			this.ar_optionInstruction = ar_optionInstruction;
		}
		
		/*
	     * 
	     * Costruttore per entries Sql con alias
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, String  valueText3, String  valueText4, EnumInstrPrecompilerType typeEntry, EnumPrecompilerReservedWords enumAlias){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.valueText2 = valueText2;
			this.valueText3 = valueText3;
			this.valueText4 = valueText4;
			this.enumAlias = enumAlias;
			this.typeEntry = typeEntry;
			
		}
		
		/*
	     * 
	     * Costruttore con valore corretto di inizio ExecCics
	     * 
	     */
		EnumPrecompilerReservedWords(String  valueText1, String  valueText2, EnumInstrPrecompilerType typeEntry, boolean outputOperand){
			this.valueText1 = valueText1;
			this.valueText2 = valueText2;
			this.typeEntry = typeEntry;
			this.outputOperand = outputOperand;
			
		}

		
		/**
		 * @return the valueText1
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
		 * Restituisce una enumerazione che indica il tipo di istruzione codificata.
		 * 
		 * @return the typeEntry
		 */
		public EnumInstrPrecompilerType getTypeEntry() {
			return typeEntry;
		}

		/**
		 * @return the outputOperand
		 */
		public boolean isOutputOperand() {
			return outputOperand;
		}


		/**
		 * @param outputOperand the outputOperand to set
		 */
		public void setOutputOperand(boolean outputOperand) {
			this.outputOperand = outputOperand;
		}

		

		/**
		 * Restituisce l'enumerazione a cui questa fa riferimento
		 * come CURRENT_LOCALE.
		 * 
		 * @return the enumAlias
		 */
		public EnumPrecompilerReservedWords getEnumAlias() {
			return enumAlias;
		}


		/**
		 * Restituisce l'enumerazione che indica se si tratta
		 * di un operatore Sql  .
		 * 
		 * @return the typeOperator
		 */
		public EnumSqlOperator getTypeOperatorPredicate() {
			return typeOperator;
		}



		/**
		 * Restituisce un array con le enumerazioni {@link EnumPrecompilerReservedWords} che
		 * sono delle opzioni per l'enumerazione corrente ma rappresentano a loro volta
		 * anche una istruzione valida.<br>
		 * Questa informazione si utilizza nel parsing di ScriptSql a istruzioni quali,
		 * per esempio, <tt>CREATE INDEX <\tt> e <tt>CREATE TSBLESPACE <\tt> che hanno
		 * come opzione <tt>CLOSE <\tt>, che identifica l'istruzione <tt>CLOSE CURSOR<\tt> 
		 * 
		 * @return the ar_optionInstruction
		 */
		public EnumPrecompilerReservedWords[] getOptionsAsInstruction() {
			return ar_optionInstruction;
		}



		/**
		 * Restituisce true se si tratta di una funzione aggregata quale:<br>
		 * <p>
		 * AVG <br>
	     * MAX <br>
	     * MIN <br>
	     * SUM <br>
	     * COUNT <br>
	     * COUNT_BIG <br>
	     * STDDEV <br>
	     * VARIANCE <br>
	     * COVARIANCE <br> 
         *
		 * @return the isAggregateFunction
		 */
		public boolean isAggregateFunction() {
			return isAggregateFunction;
		}

}
