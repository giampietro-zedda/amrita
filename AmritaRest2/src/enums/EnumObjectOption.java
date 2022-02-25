package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumObjectOption  
  * </h1>
  *  <p>
  * Questa enum elenca le opzioni di programma associabili alle varie
  * tipologie di oggetti gestiti.<br>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumObject
  * 
*/
@DataBaseMappedEnumeration
public enum EnumObjectOption {
	
	NOT_ASSIGNED,							//0
	
	// Opzioni programma generali
	PGM_BATCH,             					// 001 Programma batch
	PGM_ONLINE,            					// 002 Programma online
	PGM_BATCH_ONLINE,      					// 003 Programma condiviso fra batch e online
	PGM_INQUIRY,           					// 004 Programma di inquiry
	PGM_UPDATE,            					// 005 Programma di aggiornamento DB
	PGM_EXTRACTION,        					// 006 Programma di estrazione
	PGM_REPORT,            					// 007 Programma di stampa
	PGM_CICS,              					// 008 Programma Cics
	PGM_DB2,              					// 009 Programma DB2
	PGM_FREE3,              			    // 010 Programma Free3
	PGM_WITH_COPY,							// 011 Programma con copy richiamati
	PGM_WITH_COPY_UNUSED,					// 012 Programma con copy non utilizzati
	PGM_WITH_DEAD_CODE,						// 013 Programma con codice morto non utilizzato
	PGM_WITH_DYNAMIC_CODE,     			    // 014 Programma con codice dinamico
	PGM_WITH_DYNAMIC_CODE_SPREAD,     	    // 015 Programma con codice dinamico da risolvere in pgm chiamanti
	PGM_FREE1,                              // 016 Programma Free2
	PGM_FREE2,     			                // 017 Programma Free2

	// Opzioni programma su tipo I/O effettuato 
	PGM_WITH_I_O,              				// 018 Programma con operazioni I/O su DB, files o terminale
	PGM_WITH_I_O_SCREEN,             		// 019 Programma con operazioni a Terminale
	PGM_WITH_I_O_SEQ,             			// 020 Programma con operazioni su SEQ	
	PGM_WITH_I_O_VSAM,           			// 021 Programma con operazioni Vsam o index
	PGM_WITH_I_O_SQL,            			// 022 Programma con operazioni SQL/DBII
	PGM_WITH_I_O_ADABAS,         			// 023 Programma con operazioni ADABAS
	PGM_WITH_I_O_DL1,            			// 024 Programma con operazioni DL/1
	PGM_WITH_I_O_FREE1,            			// 025 Programma con operazioni Free1
	PGM_WITH_I_O_FREE2,            			// 026 Programma con operazioni Free2
	PGM_WITH_I_O_FREE3,            			// 027 Programma con operazioni Free3
	PGM_WITH_I_O_FREE4,            			// 028 Programma con operazioni Free4
	PGM_WITH_I_O_FREE5,            			// 029 Programma con operazioni Free5

	// Opzioni programma Cics
	PGM_CICS_WITH_SEND_TEXT,            	// 030 Programma Cics con Send Text
	PGM_CICS_WITH_SEND_MAP,             	// 031 Programma Cics con Send Map
	PGM_CICS_WITH_RECEIVE_MAP,          	// 032 Programma Cics con Receive Map
	PGM_CICS_WITH_COMMAREA,              	// 033 Programma Cics con Commarea definita
	PGM_CICS_WITH_COMMAREA_INPUT,       	// 034 Programma Cics con Commarea in input
	PGM_CICS_WITH_COMMAREA_OUTPUT,      	// 035 Programma Cics con Commarea in output
	PGM_CICS_WITH_COMMAREA_UPDATED,     	// 036 Programma Cics con Commarea aggiornata
	PGM_CICS_WITH_RETREIVE_INPUT_AREA,  	// 037 Programma Cics con Retrieve input area
	PGM_CICS_WITH_TWA,                  	// 038 Programma Cics con TWA
	PGM_CICS_WITH_CWA,                  	// 039 Programma Cics con CWA
	PGM_CICS_WITH_FREE1,                  	// 040 Programma Cics con Free1
	PGM_CICS_WITH_FREE2,                  	// 041 Programma Cics con Free2
	PGM_CICS_WITH_FREE3,                  	// 042 Programma Cics con Free3
	PGM_CICS_WITH_FREE4,                  	// 043 Programma Cics con Free4
	PGM_CICS_WITH_FREE5,                  	// 044 Programma Cics con Free5

	// Opzioni transazione Cics
	TRANSID_CICS_WITH_SEND_TEXT,            // 045 Transazione Cics con Send Text
	TRANSID_CICS_WITH_SEND_MAP,             // 046 Transazione Cics con Send Map
	TRANSID_CICS_WITH_RECEIVE_MAP,          // 047 Transazione Cics con Receive Map
	TRANSID_CICS_WITH_COMMAREA,             // 048 Transazione Cics con Commarea definita
	TRANSID_CICS_WITH_TWA,                  // 049 Transazione Cics con TWA
	TRANSID_CICS_WITH_CWA,                  // 050 Transazione Cics con CWA
	TRANSID_CICS_WITH_FREE1,                // 051 Transazione Cics con Free1
	TRANSID_CICS_WITH_FREE2,                // 052 Transazione Cics con Free2
	TRANSID_CICS_WITH_FREE3,                // 053 Transazione Cics con Free3
	TRANSID_CICS_WITH_FREE4,                // 054 Transazione Cics con Free4
	TRANSID_CICS_WITH_FREE5,                // 055 Transazione Cics con Free5
	TRANSID_NOT_UNUSED,      			    // 056 Transazione Cics non utilizzato in programmi, jcl, sources 
	
	// Opzioni copy
	COPY_SQL_INCLUDE,      					// 057 Copy sotto forma di INCLUDE Sql
	COPY_COBOL_OF_PROC_DIVISION,    		// 058 Copy Procedure 	Division Cobol
	COPY_COBOL_OF_DATA_DIVISION,    		// 059 Copy Data      	Division Cobol
	COPY_COBOL_OF_ENV_DIVISION,    		    // 060 Copy Environment Division Cobol
	COPY_COBOL_OF_BOTH_DATA_PROC,    		// 061 Copy presente sia in Data che procedure Division
	COPY_INCLUDES_COPY,    				    // 062 Copy con richiami di altri copy (nested copy)
	COPY_INCLUDED_IN_COPY,    		    	// 063 Copy incluso da qualche altro copy (nested copy)
	COPY_NOT_USED,      			        // 064 Copy definito nel sistema e non utilizzato

	// Opzioni ddl source
	DDL_SQL_WITH_CREATE_DATABASE,           // 065 Definizioni con create database
	DDL_SQL_WITH_CREATE_TABLE,              // 066 Definizioni con create table
	DDL_SQL_WITH_CREATE_INDEX,              // 067 Definizioni con create index
	DDL_SQL_WITH_CREATE_VIEW,               // 068 Definizioni con create view
	DDL_SQL_WITH_DROP_DATABASE,             // 069 Definizioni con drop database
	DDL_SQL_WITH_DROP_TABLE,               	// 070 Definizioni con drop table
	DDL_SQL_WITH_DROP_INDEX,               	// 071 Definizioni con drop index
	DDL_SQL_WITH_DROP_VIEW,               	// 072 Definizioni con drop view
	
	// Opzioni ebtity
	
	// Opzioni indice
	INDEX_VSAM_FILE,       					// 073 Index File Vsam
	INDEX_SQL_TABLE,       					// 074 Index Tabella SQL
	INDEX_ADABAS_TABLE,    					// 075 Index Tabella ADABAS
	INDEX_DL1_TABLE,       					// 076 Index Tabella DL/1
	INDEX_UNIQUE,          					// 077 Index Unique
	INDEX_PRIMARY,         					// 078 Index Primary Key
	INDEX_SECONDARY,       					// 079 Index Not Primary Key
	INDEX_FREE1,       					    // 080 Index Free1
	INDEX_FREE2,       					    // 081 Index Free2
	INDEX_FREE3,       					    // 082 Index Free3
	INDEX_FREE4,       					    // 083 Index Free4
	INDEX_FREE5,       					    // 084 Index Free5
	INDEX_NOT_USED,       					// 085 Index non utilizzato in nessuna tabella

	// Opzioni file interno (FD e Select Cobol)
	INTERNAL_FILE_SEQ,    					// 086 Syst File Sequenziale generico
	INTERNAL_FILE_SEQ_SYSIN,    			// 087 Syst File Sequenziale con schede input pilota varie 
	INTERNAL_FILE_VSAM,    					// 088 Vsam File  
	INTERNAL_FILE_VSAM_ESDS,    			// 089 Vsam File Sequenziale
	INTERNAL_FILE_VSAM_RRDS,    			// 090 Vsam File Direct
	INTERNAL_FILE_VSAM_KSDS,    			// 091 Vsam File Index
	INTERNAL_FILE_NOT_USED,    			    // 092 File interno non utilizzato in nessun jcl 

	// Opzioni file fisico
	PHISICAL_FILE_SEQ,    					// 093 Syst File Sequenziale generico
	PHISICAL_FILE_SEQ_SYSIN,    			// 094 Syst File Sequenziale con schede input pilota varie 
	PHISICAL_FILE_VSAM_ESDS,    			// 095 Vsam File Sequenziale
	PHISICAL_FILE_VSAM_RRDS,    			// 096 Vsam File Direct
	PHISICAL_FILE_VSAM_KSDS,    			// 097 Vsam File Index
	PHISICAL_FILE_NOT_USED,    			    // 098 File fisico non utilizzato in nessun jcl 

	// Opzioni jcl
	JCL_MVS,		                        // 099 Jcl Mvs 
	JCL_MVS_WITH_PROC_EMBEDDED,		        // 100 Jcl Mvs con definte Proc/Pend a inizio sorgente
	JCL_UNIX,    					        // 101 Jcl Unix .bat
	JCL_AS400_CLP,    					    // 102 AS/400 Clp

	// Opzioni Select AS/400
	SELECT_AS400_PRINTER,        			// 103 Device = PRINTER
	SELECT_AS400_FORMATFILE,     			// 104 Device = FORMATFILE
	SELECT_AS400_FORMATFILE_SI,  			// 105 Device = FORMATFILE con Attr. SI
	SELECT_AS400_TAPEFILE,       			// 106 Device = TAPEFILE
	SELECT_AS400_DISKETTE,   				// 107 Device = DISKETTE   
	// Device = DISKETTE
	SELECT_AS400_DISK,           			// 108 Device = DISK
	SELECT_AS400_DATABASE,       			// 109 Device = DATABASE
	SELECT_AS400_WORKSTATION,    			// 110 Device = WORKSTATION
	SELECT_AS400_WORKSTATION_SI, 			// 111 Device = WORKSTATION con Attr. SI
	SELECT_AS400_LITERAL,        			// 112 Device = Literal
    
	// Opzioni library
    LIBRARY_WITH_COBOL_COPY_PROC,			// 113 Libreria con Sorgenti Copy Cobol Procedure Division
    LIBRARY_WITH_COBOL_COPY_DATA,			// 114 Libreria con Sorgenti Copy Cobol Data Division
    LIBRARY_WITH_COBOL_PROGRAM,  			// 115 Libreria con Sorgenti programmi  Cobol
    LIBRARY_WITH_PL1_COPY,			        // 116 Libreria con Sorgenti Include PL1
    LIBRARY_WITH_PL1_PROGRAM,    			// 117 Libreria con Sorgenti programma PL1
    LIBRARY_WITH_ASM_COPY,			        // 118 Libreria con Sorgenti Copy Assembler
    LIBRARY_WITH_ASM_PROGRAM,    			// 119 Libreria con Sorgenti programma Assembler
    LIBRARY_WITH_JAVA_PROGRAM,   			// 120 Libreria con Sorgenti programma Java
    LIBRARY_WITH_SCRIPT_SQL,        	    // 121 Libreria con Sorgenti definizioni DDL SQL
    LIBRARY_WITH_DDL_DL1,        			// 122 Libreria con Sorgenti definizioni DDL DL1
    LIBRARY_WITH_DDL_ADABAS,     			// 123 Libreria con Sorgenti definizioni DDL ADABAS
    LIBRARY_WITH_JCL_MVS_JOB,        		// 124 Libreria con Sorgenti definizioni JCL JOB
    LIBRARY_WITH_JCL_MVS_PROC,  			// 125 Libreria con Sorgenti definizioni JCL PROC
    LIBRARY_WITH_CICS_BMS,       			// 126 Libreria con Sorgenti definizioni BMS mappe
	
	PGM_SMALL,     							// 12X Programma piccolo
	PGM_MEDIUM,     						// 128 Programma medio
	PGM_LARGE,     							// 129 Programma grande
	PGM_HUGE,     							// 130 Programma enorme
	PGM_WITH_BYNARY_FIELDS,     			// 131 Programma con campi binari definiti
	PGM_WITH_BINARY_FIELDS_FILES,           // 132 Programma con tabelle/files con dati binari
	PGM_WITH_DYNAMIC_CODE_TO_SOLVE,         // 133 Programma con codice dinamico da risolvere
	PGM_WITH_DYNAMIC_CODE_SOLVED,           // 134 Programma con codice dinamico risolto, anche se solo parzialmente
	PGM_WITH_DYNAMIC_CODE_SOLVED_FULL,      // 135 Programma con codice dinamico risolto completamente
	PGM_WITH_DYNAMIC_CODE_WAITING_FOR_DATA, // 136 Programma con codice dinamico in attesa di dati esterni per la soluzione
	SQL_SCRIPT_WITH_I_O_SQL,            	// 137 Script Sql con operazioni SQL/DBII
	SQL_SCRIPT_INQUIRY,            	        // 138 Script Sql con select lettura
	SQL_SCRIPT_UPDATE,            	        // 139 Script Sql con insert/delete/update
    JCL_MVS_WITH_INCLUDE,  			        // 140 Jcl con statement INCLUDE MEMBER=..
    LIBRARY_WITH_JCL_MVS_INCLUDE,  			// 141 Libreria con Sorgenti definizioni JCL INCLUDE
	ENTITY_WITH_BINARY_FIELDS,              // 142 Tab DB2/DL1/Vsam File con dati binari
	ENTITY_SQL_AS_VIEW,                     // 143 Tab DB2 definita come view
	ENTITY_SQL_AS_ALIAS,                    // 144 Tab DB2 definita come alias
	ENTITY_SQL_AS_SYNONYM,                  // 145 Tab DB2 definita come synonym
	PHISICAL_FILE_WITH_BINARY_FIELDS,       // 146 File fisico seq/vsam con dati binari
	COPY_COBOL_OF_ID_DIVISION;    		    // 147 Copy Identification Division Cobol


}
