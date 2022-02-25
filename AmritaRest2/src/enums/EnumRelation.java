package enums;
import analyzer.DataBaseMappedEnumeration;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumRelation (34)
 * </h1>
 *  <p>
 * Questa enum elenca le possibili relazioni fra oggetti gestite dai vari processi di analisi.
 * Le relazioni fanno riferimento agli oggetti descritti da {@link EnumObject}
 * <br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/02/2010
 * @see Analyzer
 * @see EnumObject
 * @see EnumObjectOption
*/
@DataBaseMappedEnumeration
public enum EnumRelation {
	
	NOT_ASSIGNED,						// 000 
	
	//////////////////////////////////////////////////////////////////
	// Programmi relazionati con
	//////////////////////////////////////////////////////////////////	

	// Relazioni programma generico con altri oggetti programma
	PGM_CALLED_PGM,                		// 001  CALL a pgm
	PGM_XCTL_PGM,                		// 002  CICS XCTL a pgm
	PGM_LINK_PGM,                		// 003  CICS LINK a pgm
	PGM_CANCEL_PGM,                		// 004  CANCEL 
	PGM_PGM_MISSING,                	// 005  CALL/CANCEL/LINK/XCTL a pgm  missing in tutti i sys/subsys
	PGM_COPY,                      		// 006  COBOL_COPY_INSTRUCTION
	PGM_COPY_UNUSED,                    // 007  COBOL_COPY_INSTRUCTION Unused in the program (Dead Code)
	PGM_COPY_MISSING,                   // 008  COBOL_COPY_INSTRUCTION missing copy in tutti i sys/subsys
	PGM_COPY_SQL_INCLUDE,               // 009  SQL_PRECOMPILERE_INCLUDE_INSTRUCTION
 
	// Relazioni programma con tabelle SQL, segmenti DL1, tabelle Adabas, files Vsam
	PGM_ENTITY,           		        // 010  ENTITY Sql/Dl1/Adabas/Vsam
	PGM_ENTITY_READ,      		        // 011  ENTITY READ
	PGM_ENTITY_READNEXT,  		   	    // 012  ENTITY READNEXT
	PGM_ENTITY_READPREV,  		   	    // 013  ENTITY READPREV
	PGM_ENTITY_UPDATE,    		   	    // 014  ENTITY UPDATE
	PGM_ENTITY_DELETE,    		   	    // 015  ENTITY DELETE
	PGM_ENTITY_INSERT,    		   	    // 016  ENTITY INSERT
	
	// Relazioni programma con nome file logico (FD interna al pgm o Select nome-file o file in Exec Cics)
	PGM_INTERNAL_FILE,              	// 017  FILE (Sequential, Vsam)
	PGM_INTERNAL_FILE_READ,         	// 018  FILE   READ
	PGM_INTERNAL_FILE_READNEXT,     	// 019  FILE   READNEXT
	PGM_INTERNAL_FILE_READPREV,     	// 020  FILE   READPREV
	PGM_INTERNAL_FILE_UPDATE,       	// 021  FILE   UPDATE
	PGM_INTERNAL_FILE_DELETE,       	// 022  FILE   DELETE
	PGM_INTERNAL_FILE_INSERT,       	// 023  FILE   INSERT
	PGM_INTERNAL_FILE_SORT,       	    // 024  FILE   SORT
	PGM_INTERNAL_FILE_MERGE,    	    // 025  FILE   MERGE 

	// Relazioni programma con nome file esterno dichiarato in ddname jcl o in Select nome-file Assigned To ...-external-file-name
	PGM_EXTERNAL_FILE,              	// 026  FILE (Sequential, Vsam)
	PGM_EXTERNAL_FILE_READ,         	// 027  FILE   READ
	PGM_EXTERNAL_FILE_READNEXT,     	// 028  FILE   READNEXT
	PGM_EXTERNAL_FILE_READPREV,     	// 029  FILE   READPREV
	PGM_EXTERNAL_FILE_UPDATE,       	// 030  FILE   UPDATE
	PGM_EXTERNAL_FILE_DELETE,       	// 031  FILE   DELETE
	PGM_EXTERNAL_FILE_INSERT,       	// 032  FILE   INSERT
	PGM_EXTERNAL_FILE_SORT,       	    // 033  FILE   SORT
	PGM_EXTERNAL_FILE_MERGE,    	    // 034  FILE   MERGE

	// Relazioni programma con nome file fisico (extent su disco)
	PGM_PHISICAL_FILE,             		// 035  FILE FISICO
	PGM_PHISICAL_FILE_READ,        		// 036  FILE FISICO READ
	PGM_PHISICAL_FILE_READNEXT,    		// 037  FILE FISICO READNEXT
	PGM_PHISICAL_FILE_READPREV,    		// 038  FILE FISICO READPREV
	PGM_PHISICAL_FILE_UPDATE,      		// 039  FILE FISICO UPDATE
	PGM_PHISICAL_FILE_DELETE,      		// 040  FILE FISICO DELETE
	PGM_PHISICAL_FILE_INSERT,      		// 041  FILE FISICO INSERT
	PGM_PHISICAL_FILE_SORT,      		// 042  FILE FISICO SORT
	PGM_PHISICAL_FILE_MERGE,   		    // 043  FILE FISICO MERGE
	
	// Relazioni programma con oggetti db (indici, tablespaces, databases, etc.)
	PGM_INDEX,                     		// 044  INDEX   Sql/Dl1/Adabas/File system
	PGM_SQL_DBNAME,                     // 045  DB2 DBNAME
	PGM_SQL_PACKAGE,              		// 046  DB2 PACKAGE
	PGM_SQL_PLAN,                		// 047  DB2 PLAN
	PGM_SQL_COLLECTION,                 // 048  DB2 COLLECTION
	FREE_049,                 		    // 049  FREE
	FREE_050,                 		    // 050  FREE

	// Relazioni programma con oggetti associati al Jcl
	PGM_JCL_JOB,                		// 051  JCL SOURCE
	PGM_JCL_JOBNAME,               		// 052  JCL JOBNAME
	PGM_JCL_PROC,               	    // 053  JCL PROC
    PGM_JCL_INCLUDE,                    // 054  JCL INCLUDE	
	
	// Relazioni programma con oggetti Cics
	PGM_CICS_ABCODE,               		// 055  CICS ABEND CODE
	PGM_CICS_LINK_PGM,             		// 056  EXEC CICS LINK
	PGM_CICS_XCTL_PGM,             		// 057  EXEC CICS XCTL
	PGM_CICS_LOAD_PGM,             		// 058  EXEC CICS LOAD
	PGM_CICS_RELEASE_PGM,             	// 059  EXEC CICS RELEASE
	PGM_CICS_TRANSID,              		// 060  TRANSID
	PGM_CICS_RETURN_TRANSID,       		// 061  RETURN TRANSID
	PGM_CICS_START_TRANSID,        		// 062  TRANSID CALLER
	PGM_CICS_FORM,                  	// 063  MAP+MAPSET
	PGM_CICS_MAP,               		// 064  MAP
	PGM_CICS_MAPSET,               		// 065  MAPSET
	PGM_CICS_SEND_MAP,                  // 066  MAP
	PGM_CICS_RECEIVE_MAP,               // 067  MAP
	
	// TODO
	PGM_CICS_TS_QUEUE,             		// 066  TS
	PGM_CICS_TS_QUEUE_READ,        		// 067  TS READ
	PGM_CICS_TS_QUEUE_WRITE,       		// 068  TS WRITE
	PGM_CICS_TS_QUEUE_REWRITE,     		// 069  TS REWRITE
	PGM_CICS_TS_QUEUE_DELETE,      		// 070  TS DELETE
	PGM_CICS_TD_QUEUE,             		// 071  TD
	PGM_CICS_TD_QUEUE_READ,        		// 072  TD READ
	PGM_CICS_TD_QUEUE_WRITE,       		// 073  TD WRITE
	PGM_CICS_TD_QUEUE_DELETE,      		// 074  TD DELETE
	PGM_CICS_MQ_QUEUE,             		// 075  MQ
	PGM_CICS_MQ_QUEUE_READ,        		// 076  MQ READ
	PGM_CICS_MQ_QUEUE_WRITE,       		// 077  MQ WRITE
	PGM_CICS_MQ_QUEUE_DELETE,      		// 078  MQ DELETE

	// Relazioni programma con oggetti non coperte dalle precedenti
	PGM_SD,               		        // 079  Cobol Screen Definition
	PGM_USER_TAG,               		// 080  User tag
	
	
	//////////////////////////////////////////////////////////////////
	// Moduli copy relazionati con
	//////////////////////////////////////////////////////////////////	

	COPY_COPY,              		   	// 081  COBOL_COPY_INSTRUCTION recursive
	COPY_ENTITY,              		   	// 082  ENTITY
	COPY_INTERNAL_FILE,                 // 083  INTERNAL FILE
	COPY_EXTERNAL_FILE,                 // 084  EXTERNAL FILE (DD Name)
	COPY_PHISICAL_FILE,     		   	// 085  PHISICAL FILE
	
	//////////////////////////////////////////////////////////////////
	// Transazioni CICS relazionati con
	//////////////////////////////////////////////////////////////////	
	
	// Relazioni transazione con altri oggetti programma
	TRANSID_CALLED_PGM,                	// 086  CALL
	TRANSID_XCTL_PGM,                	// 087  WHO CALL
	TRANSID_REFER_PGM,                	// 088  Refer to Program
	TRANSID_COPY,                      	// 089  COBOL_COPY_INSTRUCTION
	
	// Relazioni transazione con tabelle di database
	TRANSID_ENTITY,           		   	// 090  ENTITY (db2 table, Dl1 segment, Adabas table, altro)
	TRANSID_ENTITY_READ,      		   	// 091  ENTITY READ
	TRANSID_ENTITY_READNEXT,  		   	// 092  ENTITY READNEXT
	TRANSID_ENTITY_READPREV,  		   	// 093  ENTITY READPREV
	TRANSID_ENTITY_UPDATE,    		   	// 094  ENTITY UPDATE
	TRANSID_ENTITY_DELETE,    		   	// 095  ENTITY DELETE
	TRANSID_ENTITY_INSERT,    		   	// 096  ENTITY INSERT
	
	// Relazioni transazione con nome file fisico (extent su disco presente nei jcl)
	TRANSID_PHISICAL_FILE,             	// 097  FILE FISICO
	TRANSID_PHISICAL_FILE_READ,        	// 098  FILE FISICO READ
	TRANSID_PHISICAL_FILE_READNEXT,    	// 099  FILE FISICO READNEXT
	TRANSID_PHISICAL_FILE_READPREV,    	// 100  FILE FISICO READPREV
	TRANSID_PHISICAL_FILE_UPDATE,      	// 101  FILE FISICO UPDATE
	TRANSID_PHISICAL_FILE_DELETE,      	// 102  FILE FISICO DELETE
	TRANSID_PHISICAL_FILE_INSERT,      	// 103  FILE FISICO INSERT
	
	// Relazioni transazione con nome file logico (interno al pgm = DDNAME = FDName)
	TRANSID_EXTERNAL_FILE,              // 104  FILE (Sequential, Vsam)
	TRANSID_EXTERNAL_FILE_READ,         // 105  FILE   READ
	TRANSID_EXTERNAL_FILE_READNEXT,     // 106  FILE   READNEXT
	TRANSID_EXTERNAL_FILE_READPREV,     // 107  FILE   READPREV
	TRANSID_EXTERNAL_FILE_UPDATE,       // 108  FILE   UPDATE
	TRANSID_EXTERNAL_FILE_DELETE,       // 109  FILE   DELETE
	TRANSID_EXTERNAL_FILE_INSERT,       // 110  FILE   INSERT
 
	// Relazioni transazione con oggetti db (indici, tablespaces, databases, etc.)
	TRANSID_INDEX,                     	// 111  INDEX

	// Relazioni transazione con oggetti associati al Jcl
	TRANSID_JCL_JOB,                	// 112  JCL SOURCE
	TRANSID_JCL_JOBNAME,               	// 113  JCL JOBNAME
	TRANSID_JCL_DDNAME,                	// 114  EXTERNAL FILE NAME (DD Name)
	
	// Relazioni transazione con oggetti Cics
	TRANSID_CICS_ABCODE,               	// 115  CICS ABEND CODE
	TRANSID_CICS_LINK_PGM,             	// 116  EXEC CICS LINK
	TRANSID_CICS_XCTL_PGM,             	// 117  EXEC CICS XCTL
	TRANSID_CICS_TRANSID,              	// 118  TRANSID
	TRANSID_CICS_RETURN_TRANSID,       	// 119  RETURN TRANSID
	TRANSID_CICS_START_TRANSID,        	// 120  TRANSID CALLER
	TRANSID_CICS_FORM,                  // 121  MAP+MAPSET
	TRANSID_CICS_MAP,                  	// 122  MAP
	TRANSID_CICS_MAPSET,               	// 123  MAPSET
	TRANSID_CICS_TS_QUEUE,             	// 124  TS
	TRANSID_CICS_TS_QUEUE_READ,        	// 125  TS READ
	TRANSID_CICS_TS_QUEUE_WRITE,       	// 126  TS WRITE
	TRANSID_CICS_TS_QUEUE_REWRITE,     	// 127  TS REWRITE
	TRANSID_CICS_TS_QUEUE_DELETE,      	// 128  TS DELETE
	TRANSID_CICS_TD_QUEUE,             	// 129  TD
	TRANSID_CICS_TD_QUEUE_READ,        	// 130  TD READ
	TRANSID_CICS_TD_QUEUE_WRITE,       	// 131  TD WRITE
	TRANSID_CICS_TD_QUEUE_DELETE,      	// 132  TD DELETE
	TRANSID_CICS_MQ_QUEUE,             	// 133  MQ
	TRANSID_CICS_MQ_QUEUE_READ,        	// 134  MQ READ
	TRANSID_CICS_MQ_QUEUE_WRITE,       	// 135  MQ WRITE
	TRANSID_CICS_MQ_QUEUE_DELETE,      	// 136  MQ DELETE

	//////////////////////////////////////////////////////////////////
	// Mappe Cics relazionati con
	//////////////////////////////////////////////////////////////////	
	
	CICS_MAP_MAPSET,                    // 137  MAPSET
	CICS_FREE1,               			// 138  Free
	CICS_MAP_BMS_SOURCE,               	// 139  BMS_SOURCE
	CICS_MAPSET_BMS_SOURCE,             // 140  BMS_SOURCE
	CICS_FORM_BMS_SOURCE,               // 141  BMS_SOURCE
	
	
	//////////////////////////////////////////////////////////////////
	// Tabelle di database relazionati con
	//////////////////////////////////////////////////////////////////	

	ENTITY_INTERNAL_FILE,               // 142  INTERNAL_FILE
	ENTITY_PHISICAL_FILE,              	// 143  PHISICAL_FILE
	ENTITY_EXTERNAL_FILE,              	// 144  EXTERNAL_FILE
	ENTITY_ENTITY,            		   	// 145  ENTITY 
	ENTITY_INDEX,             		   	// 146  INDEX Sql/Dl1/Adabas/File system
	ENTITY_COPY,              		   	// 147  COBOL_COPY_INSTRUCTION
	ENTITY_ENTITY_ONE_TO_ONE,           // 148  Uno a Uno
	ENTITY_ENTITY_ONE_TO_MANY,          // 149  Uno a molti
	ENTITY_ENTITY_MANY_TO_ONE,          // 150  Molti a uno
	ENTITY_SQL_SCRIPT,        		   	// 151  DDL sorgente

	
	//////////////////////////////////////////////////////////////////
	// Indici relazionati con
	//////////////////////////////////////////////////////////////////	

	INDEX_INTERNAL_FILE,      		    // 152  INTERNAL FILE
	INDEX_PHISICAL_FILE,      		   	// 153  PHISICAL FILE
	INDEX_SQL_SCRIPT,      		       	// 154  SQL_SCRIPT

	
	//////////////////////////////////////////////////////////////////
	// File logici (FD, ..) relazionati con
	//////////////////////////////////////////////////////////////////	

	INTERNAL_FILE_EXTERNAL_FILE,        // 155  EXTERNAL_FILE
	INTERNAL_FILE_PHISICAL_FILE,        // 156  PHISICAL_FILE
	INTERNAL_FILE_JCL_JOBNAME,   		// 157  JCL JOBNAME
	INTERNAL_FILE_JCL_PROC,   		    // 158  JCL PROC
	INTERNAL_FILE_JCL_JOB,        	    // 159  JCL_JOB

	//////////////////////////////////////////////////////////////////
	// File Esterni (DD name, Select, ..) relazionati con
	//////////////////////////////////////////////////////////////////	

	EXTERNAL_FILE_PHISICAL_FILE,        // 160  PHISICAL_FILE
	EXTERNAL_FILE_JCL_JOBNAME,   		// 161  JCL JOBNAME
	EXTERNAL_FILE_JCL_PROC,   			// 162  JCL PROC
	EXTERNAL_FILE_JCL_JOB,              // 163  JCL SOURCE  

	
	//////////////////////////////////////////////////////////////////
	// File fisici relazionati con
	//////////////////////////////////////////////////////////////////	

	PHISICAL_FILE_JCL_JOBNAME,   		// 164  JCL JOBNAME
	PHISICAL_FILE_JCL_PROC,   			// 165  JCL PROCNAME
	PHISICAL_FILE_JCL_JOB,    		    // 166  JCL source
	PHISICAL_FILE_SQL_SCRIPT,    		// 167  DDL

	//////////////////////////////////////////////////////////////////
	// Database name relazionato con
	//////////////////////////////////////////////////////////////////	

	FREE_168,            		        // 168  FREE
	DBNAME_DB2_SUBSYS,                  // 169  DB2 SUB System
	DBNAME_DL1_PSB,                     // 170  DL1 PSB
	DBNAME_CICS_NAME,                   // 171  DL1 PSB
	DBNAME_ENTITY,                      // 172  ENTITY (Table, segment, ..)
	
	//////////////////////////////////////////////////////////////////
	// DB2 Environment Objects relazionati con
	//////////////////////////////////////////////////////////////////	
	
	SQL_PLAN_JCL_JOB,          		    // 173  JCL_JOB
	SQL_PLAN_SYSIN_SOURCE,              // 174  SYSIN_SOURCE
	SQL_PLAN_JCL_JOBNAME,         		// 175  JCL_JOBNAME
	SQL_PACKAGE_JCL_JOB,          	    // 176  JCL JOB
	SQL_PACKAGE_PHISICAL_FILE,          // 177  PHISICAL_FILE SYSIN 
	SQL_PACKAGE_JCL_JOBNAME,            // 178  JCL_JOBNAME
	SQL_COLLECTION_JCL,                 // 179  JCL source
	SQL_COLLECTION_PHISICAL_FILE,       // 180  PHISICAL_FILE SYSIN 
	SQL_COLLECTION_JCL_JOBNAME,         // 181  JCL_JOBNAME
	SQL_USER_FUNCTION_SQL_SCRIPT,       // 182  SQL_SCRIPT
	
	FREE_183,            		         // 183  FREE

	//////////////////////////////////////////////////////////////////
	// Nomi Cics relazionati con
	//////////////////////////////////////////////////////////////////	
	
	CICS_NAME_DB2_SUBSYS,           	// 184  DB2 SUB SYS
	CICS_NAME_DBNAME,               	// 185  DBNAME
	CICS_NAME_JCL_JOBNAME,          	// 186  JCL_JOBNAME
	CICS_NAME_JCL_JOB,           	    // 187  JCL_JOB

	
	//////////////////////////////////////////////////////////////////
	// Jcl sources relazionati con
	//////////////////////////////////////////////////////////////////	
	
	JCL_JOB_JCL_JOBNAME,         	    // 188  JCL_JOBNAME
	JCL_JOB_JCL_PROC,               	// 189  JCL_PROC
 	JCL_JOB_JCL_INCLUDE,                // 190  JCL_INCLUDE    

	//////////////////////////////////////////////////////////////////
	// Jcl Proc relazionati con
	//////////////////////////////////////////////////////////////////	

	JCL_PROC_SQL_SCRIPT,        	    // 191  SQL_SCRIPT   
	JCL_PROC_JCL_PROC,            	    // 192  PROC che fa exec di altra proc
	JCL_PROC_JCL_JOBNAME,               // 193  JCL_JOBNAME 

	
	//////////////////////////////////////////////////////////////////
	// Jcl include relazionati con
	//////////////////////////////////////////////////////////////////	

	JCL_INCLUDE_JCL_INCLUDE,        	// 194 JCL_INCLUDE        
	JCL_INCLUDE_JCL_PROC,        	    // 195 JCL_PROC
	JCL_INCLUDE_EXTERNAL_FILE,          // 196 EXTERNAL_FILE
	JCL_INCLUDE_PHISICAL_FILE,          // 197 HISICAL_FILE
	
	
	
	//////////////////////////////////////////////////////////////////
	// Libreria relaziona con
	//////////////////////////////////////////////////////////////////	
	
	LIBRARY_SOURCE_MEMBER,              // 198  SOURCE_MEMBER
	LIBRARY_PGM,               		    // 199  PGM
	LIBRARY_COPY,               		// 200  COBOL_COPY_INSTRUCTION
	LIBRARY_CICS_BMS_SOURCE,            // 201  BMS SOURCE
	LIBRARY_CICS_FORM,                  // 202  CICS MAP+MAPSET
	LIBRARY_CICS_MAP,                   // 203  CICS MAP
	LIBRARY_CICS_MAPSET,                // 204  CICS MAPSET
	LIBRARY_SQL_SCRIPT,              	// 205  SQL_SCRIPT
	LIBRARY_JCL_JOB,              		// 206  JCL_JOB
	LIBRARY_JCL_JOBNAME,            	// 207  JCL_JOBNAME
	LIBRARY_JCL_PROC,              		// 208  JCL_PROC 
	LIBRARY_JCL_INCLUDE,           		// 209  JCL_INCLUDE	

	//////////////////////////////////////////////////////////////////
	// Systema/Sotto sistema ralazionato con
	//////////////////////////////////////////////////////////////////	
	
	SYS_SUBSYS_DEFINED,	       		    // 210 	SUBSYS_DEFINED
	SYS_SUBSYS_COPY_MISSING,	        // 211 	COPY_MISSING
	SYS_SUBSYS_PGM_MISSING,	            // 212 	PGM_MISSING
  	SYS_SUBSYS_JCL_MVS_INCLUDE_MISSING, // 213  JCL_INCLUDE_MISSING
  	SYS_SUBSYS_JCL_MVS_PROC_MISSING,	// 214  JCL_PROC_MISSING
  	
	PGM_EXTERNAL_FILE_BYNARY_FIELDS,    // XXX  EXTERNAL_FILE
	PGM_PHISICAL_FILE_BYNARY_FIELDS,    // XXX  PHISICAL_FILE
	PGM_ENTITY_FILE_BYNARY_FIELDS,      // XXX  PGM_ENTITY Sql/Dl1/Adabas/Vsam
	PGM_ENTITY_SQL_DB2_LOCATIONS,       // XXX  DB2_LOCATION
	PGM_ENTITY_SQL_ALIAS,               // XXX  SQL_ALIAS
	PGM_ENTITY_SQL_SYNONYM,             // XXX  SQL_SYNONYM
	PGM_SQL_TABLESPACE,                 // XXX  SQL_TABLESPACE
	PGM_SQL_DB2_LOCATIONS,              // XXX  DB2_LOCATION
	PGM_SQL_OWNER,                      // XXX  SQL_OWNER
	PGM_SQL_ALIAS,                      // XXX  SQL_ALIAS
	PGM_SQL_SYNONYM,                    // XXX  SQL_SYNONYM
	ENTITY_SQL_OWNER,                   // XXX  SQL_OWNER
	ENTITY_SQL_ALIAS,                   // XXX  SQL_ALIAS
	ENTITY_SQL_SYNONYM,                 // XXX  SQL_SYNONYM
	ENTITY_SQL_TABLESPACE,              // XXX  SQL_TABLESPACE 
	INDEX_SQL_OWNER,                    // XXX  SQL_OWNER	
	TABLESPACE_SQL_SCRIPT,              // XXX  SQL_SCRIPT
	JCL_JOB_JCL_DDNAME,               	// XXX  JCL_JOB_JCL_DDNAME
	DBNAME_SYS,                         // XXX  SYSTEM
	DBNAME_SUBSYS,                      // XXX  SUBSYSTEM
	DBNAME_SQL_SCRIPT,                  // XXX  SQL_SCRIPT
	DBNAME_SQL_TABLESPACE,              // XXX  SQL_TABLESPACE 
	DBNAME_SQL_OWNER,                   // XXX  SQL_OWNER
	LIBRARY_CICS_PCT,                   // XXX  CICS PCT
	LIBRARY_CICS_PPT,                   // XXX  CICS PPT
	LIBRARY_CICS_TCT,                   // XXX  CICS TPT
	LIBRARY_CICS_FCT,                   // XXX  CICS FPT
    PGM_CICS_PPT,                       // XXX  CICS PPT
    PGM_CICS_PCT,                       // XXX  CICS PCT
    TRANSID_CICS_PCT,                   // XXX  CICS PCT
    EXTERNAL_FILE_CICS_FCT;             // XXX  CICS FCT
}
