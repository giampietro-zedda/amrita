package enums;

import analyzer.DataBaseMappedEnumeration;
/**
 * copyright c 2009 e-Amrita - Giampietro Zedda    Turin ITALY
 *
 * <h1>
 * EnumObject (1)
 * </h1>
 *  <p>
 * Questa enum elenca i possibili tipi di oggetto generati a fronte dei processi
 * di analisi dei sorgenti e delle successive elaborazioni. Non c'è una
 * corrispondenza diretta fra il tipo di sorgente e i tipi di oggetti generati
 * dai vari processi di analisi.<br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/02/2010
 * @see Analyzer
 * 
*/
@DataBaseMappedEnumeration
public enum EnumObject {

	 // Di servizio
	 NOT_ASSIGNED,							// 00

     // Oggetti programma 
	 OBJECT_PGM_COBOL,             			// 01 Programma COBOL
	 OBJECT_PGM_PL1,               			// 02 Programma PL1
	 OBJECT_PGM_JAVA,              			// 03 Programma Java
	 OBJECT_PGM_ASM,              			// 04 Programma Assembler
	 OBJECT_PGM_FREE1,              	    // 05 Programma Free1
	 OBJECT_PGM_FREE2,              	    // 06 Programma Free1
	 OBJECT_PGM_GENERIC,              	    // 07 Programma Generico in jcl // EXEC PGM=pgmname
	 
	 // Oggetto binario
	 OBJECT_LOAD_MODULE,              	    // 08 Modulo load assembler
	 
	 // Oggetti Copy book, Sql Include, ..
	 OBJECT_COPY_COBOL_PROC,	  	        // 09 Sorgenti Copy Cobol Procedure Division
	 OBJECT_COPY_COBOL_DATA,		        // 10 Sorgenti Copy Cobol Data Division
	 OBJECT_COPY_COBOL_ENV,		            // 11 Sorgenti Copy Cobol Environment Division
	 OBJECT_COPY_COBOL_ID,		            // 12 Sorgenti Copy Cobol Identification Division

	 OBJECT_FREE_13,                  	    // 13 Free
	 OBJECT_FREE_14,                  	    // 14 Free
	 
	 // Oggetti associati al Database e alle struttura dati Cics o di file system
	 OBJECT_ENTITY_SQL,      			    // 15 Tabella SQL
	 OBJECT_FREE_16,   			            // 16 Free
	 OBJECT_FREE_17,      		            // 17 Free
	 OBJECT_ENTITY_VSAM,      		        // 18 Tabella Vsam File System
	 OBJECT_FREE_19,      		            // 19 Tabella Free2
	 OBJECT_FREE_20,       		        	// 20 Tabella Free3
	 OBJECT_FREE_21,       		        	// 21 Tabella Free4
	 OBJECT_FREE_22,       		        	// 22 Tabella Free5

	 // Oggetti vari associati a files fisici e al file system
	 OBJECT_INTERNAL_FILE,                  // 23 Nome logico e/o interno di archivio di File System 
	 OBJECT_EXTERNAL_FILE,                  // 24 DDname in Jcl o Nome esterno di archivio mappato dal programma 
	 OBJECT_PHISICAL_FILE,                  // 25 Dsname Fisico Vsam/Seq extent indicato nel Jcl in modo generico
	 OBJECT_KSDS_FILE,           	        // 26 File Vsam KSDS
	 OBJECT_ESDS_FILE,       	            // 27 File Vsam ESDS
     OBJECT_RRDS_FILE,                      // 28 File Vsam RRDS
     OBJECT_SEQ_FILE,                       // 29 File Sequential
     OBJECT_FREE5,                          // 30 Free3
	 
	 // Oggetti specifici database SQL/DB2 
	 OBJECT_SQL_DBNAME,              	    // 31 Db2 Database Name
	 OBJECT_SQL_SUBSYSYSTEM,           		// 32 Db2 SubSystem
	 OBJECT_SQL_DB2_LOCATION,         	    // 33 DB2 Identificatore db2 nel server  
	 OBJECT_SQL_PLAN,             			// 34 Db2 Plan
	 OBJECT_SQL_PACKAGE,         			// 35 Db2 package
	 OBJECT_SQL_COLLECTION,         	    // 36 DB2 collection
	 OBJECT_SQL_TABLESPACE,         	    // 37 DB2 tablespace
	 OBJECT_SQL_OWNER,         	        	// 38 DB2 Proprietario oggetto
	 OBJECT_SQL_ALIAS,         	    		// 39 DB2 Alias tabella
	 OBJECT_SQL_SYNONYM,         	    	// 40 DB2 Sunonym tabella
	 OBJECT_SQL_INDEX,                      // 41 Sql index
	 OBJECT_SQL_SCRIPT,                     // 42 DB2 Script Sql con DDL, Commands etc., membro di libreria
	 OBJECT_SQL_STOGROUP,                   // 43 DB2 Storage group
     OBJECT_SQL_USER_FUNCTION,              // 44 DB2 function creata con CREATE FUNCTION
     OBJECT_FREE6,                          // 45 Free5
	 
	 // Oggetti specifici database DL1 
     OBJECT_DL1_FREE6,						// 46 Dl1 Database Name
	 OBJECT_DL1_FREE7,    					// 47 Dl1 DBD
	 OBJECT_DL1_FREE8,     					// 48 Dl1 PSB
	 OBJECT_DL1_FREE9,      				// 49 Dl1 Free1
	 OBJECT_DL1_FREE2,      				// 50 Dl1 Free2
	 OBJECT_DL1_FREE3,      				// 51 Dl1 Free3
	 OBJECT_DL1_FREE4,      				// 52 Dl1 Free4
	 OBJECT_DL1_FREE5,      				// 53 Dl1 Free5
	 
	 // Oggetti associati al Jcl
	 OBJECT_JCL_JOB,           	            // 54 JCL source name con scheda job non definizione proc e non include
	 OBJECT_JCL_PROC,           	        // 55 JCL source name con definizione proc  
	 OBJECT_JCL_INCLUDE,           	        // 56 JCL source incluso in jcl o altre include
	 OBJECT_JCL_JOBNAME,          	        // 57 Job Name su //Name   JOB
	 OBJECT_JCL_DDNAME,          	        // 58 JCL DDNAME
	 OBJECT_JCL_FREE2,          	        // 59 Free2
	 OBJECT_JCL_FREE3,          	        // 60 Free3
	 OBJECT_JCL_FREE4,          	        // 61 Free4
	 OBJECT_JCL_FREE5,          	        // 62 Free5
	 OBJECT_JCL_FREE6,          	        // 63 Free6
	 OBJECT_JCL_FREE7,          	        // 64 Free7
	 
	 // Oggetti associati al CICS
	 OBJECT_CICS_ABCODE,                    // 65 Abend Code
	 OBJECT_CICS_TRANSID,                   // 66 Transazione
	 OBJECT_CICS_FORM,                      // 67 Map-Mapset  identifica univocamente una mappa video
	 OBJECT_CICS_MAP,                       // 68 BMS Map     può comparire in mapset diversi
	 OBJECT_CICS_MAPSET,                    // 69 BMS Mapset  univoco nelCics
	 OBJECT_CICS_BMS_SOURCE,                // 70 BMS source name
	 OBJECT_CICS_TS_QUEUE,                  // 71 Coda TS
	 OBJECT_CICS_TD_QUEUE,                  // 72 Coda TD
	 OBJECT_CICS_MQ_QUEUE,                  // 73 Coda MQ
	 OBJECT_CICS_SYSTEM_FIELD,              // 74 System Field EnumdataItemSystemEnvironment like EIBTRNID, EIBTRMID, TWA, CSA, ...
	 OBJECT_CICS_PCT,                       // 75 PCT
	 OBJECT_CICS_PPT,                       // 76 PPT
	 OBJECT_CICS_FCT,                       // 77 FCT
	 OBJECT_CICS_TCT,                       // 78 TCT
	 OBJECT_CICS_FREE5,                     // 79 Free5
	 
	 // Oggetti AS400
	 OBJECT_AS400_FREE1,                    // 80 Equivalente a Cics TRANSID
	 OBJECT_AS400_FREE6,                    // 81 Equivalente a JCL
	 OBJECT_AS400_FREE7,                    // 82 Data Description Specification
	 OBJECT_AS400_FREE8,                    // 83 FREE1
	 OBJECT_AS400_FREE2,                    // 84 FREE2
	 OBJECT_AS400_FREE3,                    // 85 FREE3
	 OBJECT_AS400_FREE4,                    // 86 FREE4
	 OBJECT_AS400_FREE5,                    // 87 FREE5
	 
	 // Oggetti relativi ai sorgenti
	 OBJECT_LIBRARY,             			// 88 Libreria con sources da analizzare
	 OBJECT_SOURCE_MEMBER,             	    // 89 Membro di libreria sorgente
	 
	 // Oggetti utente e vari non classificabili
	 OBJECT_USER_TAG,            			// 90 User Tag
	 OBJECT_SYS,             				// 91 Aggregazione principale sorgenti e oggetti
	 OBJECT_SUBSYS,             			// 92 Aggregazione secondaria sorgenti e oggetti in ambito SYS
	 OBJECT_SYS_SUBSYS,             		// 93 Sistema + "-" + Sottosistema, identifica una unita di aggregazione di oggetti e relazioni
	 OBJECT_SYS_SUBSYS_GLOBAL;             	// 94 Aggregazione di tutti i sistemi e tutti i sottosistemi
	 
}
