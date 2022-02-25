package enums;

import analyzer.DataBaseMappedEnumeration;
/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDirectivesExecution 
 * </h1>
 *  <p>
 * Questa enum elenca le possibili direttive ammesse nel file pilota di processi/funzioni relativamente alla
 * modalità di recupero e filtro dei sorgenti, degli oggetti da elaborare e alle modalità di esecuzione.
 * 
 * Le direttive di processo, funzione e controllo devono essere codificate prima della direttiva
 * START, che avvia processi e funzioni in modalità multithread o normalmente in base alle opzioni
 * della direttiva stessa.<br>
 * Attraverso queste direttive è possibile controllare il modo nel quale vengono eseguiti
 * i processi.<br>
 *
 * La gestione o meno come thread separati viene specificata a livello di dichiarazione dei processi
 * e delle funzioni.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 28/02/2010
 * @see Analyzer
 * @see EnumDirectivesSource
*/
@DataBaseMappedEnumeration
public enum EnumDirectivesExecution {

	 // Di servizio
	 NOT_ASSIGNED,               		    		// Direttiva di esecuzione non assegnata
	 
	 /////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di impostazione directories alternative per sources/log/oggetti serializzati                          
	 /////////////////////////////////////////////////////////////////////////////////////////////////	 
	 
     DIR_PILOT_AND_FILTER,              			// Pilot  sources e processi e filtri     (.pilot)
     DIR_WORK,                                		// Working e temporaneo		    
	 DIR_LOG,                                 		// Log
	 DIR_OUTPUT,                              		// Output per funzioni 
	 DIR_RESOURCES,                        			// Resource
	 DIR_DATABASE,                          		// Database		              
	 DIR_COBOL_SRC_PGM_INPUT,          				// Pgm    Cobol sorgenti in analisi		  (.*) 
	 DIR_COBOL_SRC_COPY_INPUT,        				// Copy   Cobol sorgenti in analisi		  (.*)
	 DIR_COBOL_PGM,                  				// Pgm    Cobol codificati e serializzati (.program)			 
	 DIR_COBOL_COPY,                        		// Copy   Cobol codificati e serializzati (.copy)	
	 DIR_COBOL_GRAPH,                				// Grafi  Cobol codificati e serializzati (.graph)	
	 DIR_SQL_SCRIPT,                  				// Script Sql codificati e serializzati   (.scriptSql)			 
	 DIR_JCL_INPUT,                          		// Jcl    in input al processo di analisi (.*)        
	 DIR_JCL,                 			        	// Jcl    codificati e serializzati (.jclSource, .jclInclude, .jclProc)		 

	 /////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di accesso al database                         
	 /////////////////////////////////////////////////////////////////////////////////////////////////	 
	 
	 DATABASE_USER,  					            // Username (GZEDDA)
	 DATABASE_PWD, 					                // Password (giampietro4)
	 DATABASE_NAME,   					            // DbName   (Amrita)
	 DATABASE_TYPE,                                 // MYSQL, ORACLE, MSACCESS
	 DATABASE_DRIVER,                         	    // MYSQL=com.mysql.cj.jdbc.Driver, MSACCESS	= sun.jdbc.odbc.JdbcOdbcDriver
	 DATABASE_ACCESS_TYPE, 				            // LOCAL, REMOTE
	 DATABASE_URL, 				                	// MYSQL   =jdbc:mysql://localhost:3306/Amrita?autoReconnect=true&useSSL=false&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC
                                                    // MSACCESS=jdbc:odbc:DbAmrita
	 DATABASE_MAX_CONN,            				    // Max number of connections (3)
	 DATABASE_LOG_ANY_SQL_ENABLED,                  // Log oerazioni Sql abilitato
	 DATABASE_LOG_ANY_SQL_DISABLED,                 // Log oerazioni Sql disabilitato
	 DATABASE_COMMIT_BLOCK_UPDATE,                  //

	 
	 /////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di impostazione sistema/sottosistema di appartenenza oggetto/sorgente                           
	 /////////////////////////////////////////////////////////////////////////////////////////////////
    
     SYSTEM_INPUT,               					// Sistema       con gli oggetti da analizzare		 
	 SYSTEM_ON_NAME,           						// Sistema       come pos inizio in nome + valore
	 SYSTEM_BY_EXIT,         		    			// Sistema       come restituito da exit class
     SUB_SYSTEM_INPUT,               				// Sottosistema  con gli oggetti da analizzare    (*ALL* tutti i sottosistemi oppure s1 s2 s3 ... sn)	
	 SUB_SYSTEM_ON_NAME,       						// Sottosistema  come pos inizio in nome + lunghezza
	 SUB_SYSTEM_BY_EXIT,     		    			// Sottosistema  da exit class

	 
	 /////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di impostazione sistema/sottosistema di appartenenza oggetto/sorgente                           
	 /////////////////////////////////////////////////////////////////////////////////////////////////
    
//   SYSTEM_VALUE,               					// Sistema  	 come valore
//	 SUB_SYSTEM_VALUE,           					// Sottosistema  come valore
//   SYSTEM_TO_CLEAR,               			    // Sistema con le tabelle da deletare
//   SUB_SYSTEM_TO_CLEAR,               			// Sottosistema con le tabelle da deletare	

	 /////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di internazionalizzazione                     
	 /////////////////////////////////////////////////////////////////////////////////////////////////
    
     LOCALE_LANGUAGE,               				// Linguaggio secondo notazione locale java
     LOCALE_COUNTRY,               					// Paese secondo notazione locale java

	
	 ///////////////////////////////////////////////////////////////////////////////////////
     // Direttive per librerie, files da trattare, Cics da considerare , copy da includere                       
	 ///////////////////////////////////////////////////////////////////////////////////////
	 
     LIBRARY,     			                		// Identifica una libreria. par 1 = nome logico, par 2 = path libreria
	 LIBRARY_SOURCE_SEARCH,           				// Ricerca sorgenti copy/programmi nella libreria dichiarata
 	 LIBRARY_SOURCE_SEARCH_CLEAR,           		// Valori Library immessi precedentemente eliminati
	 LIBRARY_CLEAR,           						// Valori Library immessi precedentemente eliminati
	 COPY_PRECOMPILER,                              // Identifica un copy da includere come CICS|LINKAGE|WORKING/copy-name
	 COPY_PRECOMPILER_CLEAR,                        // Valori Library immessi precedentemente eliminati
	 FILE_PATH,     			            		// Identifica il path completo di un file sorgente da trattare
	 FILE_SUFFIX_COPY,     			                // Identifica un suffisso valido con cui cercare i copy nelle librerie
	 FILE_PATH_CLEAR,     			        		// Valori File immessi precedentemente eliminati
	 FILE_SUFFIX_COPY_CLEAR,     			        // Valori File immessi precedentemente eliminati
	 CICS,                                          // Nome Cics da considerare per mapping internal file/dsname
     CICS_CLEAR,                                    // Nome Cics da considerare per mapping internal file/dsname

	 ///////////////////////////////////////////////////////////////////////////
     // Direttive di limitazione sui sources/oggetti da trattare                    
	 ///////////////////////////////////////////////////////////////////////////

	 LIMIT_MAX_OBJECTS_ENABLED,        	    		// Limite massimo sorgenti/oggetti trattati abilitato  
	 LIMIT_MAX_OBJECTS_DISABLED,        			// Limite massimo sorgenti/oggetti trattati disabilitato  
	 LIMIT_MAX_OBJECTS_INPUT,        	    		// Numero massimo sorgenti/oggetti da trattare (che hanno superato i filtri)  
	 LIMIT_MAX_OBJECTS_TO_PROCESS,    				// Numero massimo sorgenti/oggetti da processare (per esempio da analizzare)      

	 
	 ///////////////////////////////////////////////////////////////////////////////////
     // Direttive di filtro sui nomi sorgenti/oggetti da trattare, fra quelli in  input                       
	 ///////////////////////////////////////////////////////////////////////////////////
	 
	 FILTER_BY_EXIT,                    			// Filtro sul nome sorgente come restituito da exit class
	 FILTER_ON_OBJECT_NAME_POS_VALUE,           	// Pos inizio caratteri di filtro nel nome source/oggetto + valore di match
	 FILTER_ON_OBJECT_NAME_RANGE,           	    // Obj type From o *, Obj name From o *, Obj type To o *, Obj Name To o *, Relationships o *
	 FILTER_DISABLED,                    			// Filtro sul nome sorgente/oggetto disabilitato
	 FILTER_CLEAR,                      			// Valori filtro immessi precedentemente eliminati

	 ///////////////////////////////////////////////////////////////////////////////////
     // Direttive di esclusione sui nomi sorgenti/oggetti in input e filtrati                       
	 ///////////////////////////////////////////////////////////////////////////////////
	 
	 EXCLUDE_BY_EXIT,                    		    // Esclusione del nome oggetto da quelli da processare
	 EXCLUDE_ON_OBJECT_NAME_POS_VALUE,           	// Pos inizio caratteri di filtro nel nome source/oggetto + valore di match
	 EXCLUDE_ON_OBJECT_NAME,                        // Esclusione del nome oggetto da quelli da processare
	 EXCLUDE_ON_OBJECT_NAME_RANGE,                  // Esclusione del range di nomi oggetto da quelli da processare
	 EXCLUDE_CLEAR,                    		        // Valori exclude immessi precedentemente eliminati

	 
	 //////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di filtro sullo stato degli oggetti da trattare già classificati in libreria                   
	 //////////////////////////////////////////////////////////////////////////////////////////

	 INPUT_OBJECTS_STATUS_ALL(EnumObjectStatus.NOT_ASSIGNED),                    							// Vengono trattati gli oggetti in qualsiasi stato
	 INPUT_OBJECTS_SOURCE_MEMBER_TYPE_DETECTED(EnumObjectStatus.SOURCE_MEMBER_TYPE_DETECTED),   			// Vengono trattati gli oggetti analizzati senza errori
	 INPUT_OBJECTS_TO_BE_ANALYZED(EnumObjectStatus.OBJECT_TO_BE_ANALYZED),                  				// Vengono trattati gli oggetti ancora da analizzare
	 INPUT_OBJECTS_ANALYZED_WITH_NO_ERRORS(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS),    			// Vengono trattati gli oggetti analizzati senza errori
	 INPUT_OBJECTS_ANALYZED_WITH_NO_ERRORS_WAITING(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS),    	// Vengono trattati gli oggetti analizzati senza errori	 
	 INPUT_OBJECTS_ANALYZED_DYNAMIC(EnumObjectStatus.OBJECT_ANALYZED_DYNAMIC), 								// Vengono trattati gli oggetti analizzati con codice dinamico da risolvere/risolto/spreaded/local	 
	 INPUT_OBJECTS_ANALYZED_DYNAMIC_SPREADED_TO_SOLVE(EnumObjectStatus.OBJECT_ANALYZED_DYNAMIC_SPREAD_TO_SOLVE), // Vengono trattati gli oggetti analizzati con codice dinamico spreaded da risolvere	 	 
	 INPUT_OBJECTS_ANALYZED_DYNAMIC_SOLVED(EnumObjectStatus.OBJECT_ANALYZED_DYNAMIC_SOLVED),    			// Vengono trattati gli oggetti analizzati con codice dinamico risolto
	 INPUT_OBJECTS_ANALYZED_DYNAMIC_WAITING_FOR_DATA(EnumObjectStatus.OBJECT_ANALYZED_DYNAMIC_WAITING_FOR_DATA),// Vengono trattati gli oggetti analizzati in attesa di dati esterni	 
	 INPUT_OBJECTS_ANALYZED_WITH_ERRORS(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS),          			// Vengono trattati gli oggetti analizzati con errori
	 INPUT_OBJECTS_ANALYZED_WITH_EXCEPTION(EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION),   				// Vengono trattati gli oggetti analizzati con exception
	 INPUT_OBJECTS_ANALYZED_WAITING_FOR_DATA(EnumObjectStatus.OBJECT_ANALYZED_DYNAMIC_WAITING_FOR_DATA),    // Vengono trattati gli oggetti analizzati con istruzioni dinamiche risolte
	 INPUT_OBJECTS_PROCESSED_WITH_NO_ERRORS(EnumObjectStatus.OBJECT_PROCESSED_WITH_NO_ERRORS),  			// Vengono trattati gli oggetti processati senza errori
	 INPUT_OBJECTS_PROCESSED_WITH_ERRORS(EnumObjectStatus.OBJECT_PROCESSED_WITH_ERRORS),        			// Vengono trattati gli oggetti processati con con errori
	 INPUT_OBJECTS_PROCESSED_WITH_EXCEPTION(EnumObjectStatus.OBJECT_PROCESSED_WITH_EXCEPTION),  			// Vengono trattati gli oggetti processati con exception
     INPUT_OBJECTS_STATUS_CLEAR(EnumObjectStatus.NOT_ASSIGNED),                    							// Eliminazione direttive del tipo precedente
     
     ///////////////////////////////////////////////////////////////////////////////
     // Direttive specifiche per metriche                    							   
	 ///////////////////////////////////////////////////////////////////////////////

     METRICS_SQUALE_VIOLATIONS_SCENARIO,       	   // Nome scenario di personalizzazione violazioni da utilizzare

     ///////////////////////////////////////////////////////////////////////////////
     // Direttive specifiche e di opzione di esecuzione processi e funzioni                     							   
	 ///////////////////////////////////////////////////////////////////////////////

	 // Generic
	 OPT_UPDATE_DB_ENABLED,	                    	// Aggiornamento database abilitato
	 OPT_UPDATE_DB_DISABLED,						// Aggiornamento database disabilitato
	 OPT_VERBOSE_MESSAGES_ENABLED,	                // Messaggi estesi abilitati
	 OPT_VERBOSE_MESSAGES_DISABLED,					// Messaggi estesi disabilitati
	 OPT_TRACE_ANY_ANALYSIS_ERROR_ON_DB_ENABLED,	// Scrittura errori su table OBJE per ogni errore di analisi abilitata
	 OPT_TRACE_ANY_ANALYSIS_ERROR_ON_DB_DISABLED,	// Scrittura errori su table OBJE per ogni errore di analisi disabilitata
	 OPT_STACK_TRACE_ON_PARSING_ERROR_ENABLED, 		// Attiva logging stack trace per ogni istruzione errata
	 OPT_STACK_TRACE_ON_PARSING_ERROR_DISABLED, 	// logging stack trace per ogni istruzione errata disabilitato
	 
	 // Compliance
	 OPT_IBM_DB2_MAINFRAME_V10_COMPLIANCE_ENABLED,  // Versione DB2 IBM MAINFRAME <= accettata abilitatata
	 OPT_IBM_DB2_MAINFRAME_V10_COMPLIANCE_DISABLED, // Versione DB2 IBM MAINFRAME <= accettata disabilitata
	 OPT_IBM_DB2_AIX_V9_COMPLIANCE_ENABLED,  		// Versione DB2 AIX/LINUX/WINDOW <= accettata abilitatata
	 OPT_IBM_DB2_AIX_V9_COMPLIANCE_DISABLED, 		// Versione DB2 IBM AIX/LINUX/WINDOW <= accettata disabilitata

	 // FUNCTION_LIBRARY_SCAN
	 OPT_DETECT_SOURCE_TYPE_ENABLED,        		// Attiva riconoscimento tipologia sorgente
	 OPT_DETECT_SOURCE_TYPE_DISABLED,       		// Riconoscimento tipologia sorgente disabilitato
	 OPT_SEARCH_SOURCES_FILE_RECURSIVE_ENABLED, 	// Ricerca sorgenti ricorsiva anche nelle sottodirectory
	 OPT_SEARCH_SOURCES_FILE_RECURSIVE_DISABLED,	// Ricerca sorgenti limitata alla directory dichiarata
	 OPT_FULL_INFO_FILE_SYSTEM_ENABLED,         	// Informazioni file system complete su files estratti abilitate
	 OPT_FULL_INFO_FILE_SYSTEM_DISABLED,        	// Informazioni file system complete su files estratti disabilitate
	 
	 // PROCESS_ANALYZE_SOURCES 
 	 OPT_DB_CREATE_COPY_DEF_ENABLED,		    		// Creazione definizione copy su database
	 OPT_DB_CREATE_COPY_DEF_DISABLED,		    		// Creazione definizione copy su database disabilitata
	 OPT_DB_CREATE_WHERE_USED_COPY_ITEMS_ENABLED,   	// Inserimento where used di ogni campo dei copy abilitato
	 OPT_DB_CREATE_WHERE_USED_COPY_ITEMS_DISABLED,  	// Inserimento where used di ogni campo dei copy disabilitato
	 OPT_DB_CREATE_WHERE_USED_ENTITY_COLUMNS_ENABLED, 	// Inserimento where used di ogni colonna di entity abilitato
	 OPT_DB_CREATE_WHERE_USED_ENTITY_COLUMNS_DISABLED,	// Inserimento where used di ogni colonna di entity disabilitato
	 OPT_FORCE_RE_ANALYSIS_COPY_ENABLED,              	// Rianalisi copy abilitata valida per tutti i copy 
	 OPT_FORCE_RE_ANALYSIS_COPY_DISABLED,             	// Rianalisi copy disabilitata
	 OPT_FORCE_RE_BUILD_COPY_DEF_ON_DB_ENABLED,       	    // Ricostruzione definizione tracciato copy su db 
	 OPT_FORCE_RE_BUILD_COPY_DEF_ON_DB_DISABLED,      	    // Ricostruzione definizione tracciato copy su db disabilitata
	 OPT_DETECT_DEAD_CODE_ENABLED,             	 		// Individuazione codice morto (fine parsing o funzione)
	 OPT_DETECT_DEAD_CODE_DISABLED,      				// Individuazione codice morto (fine parsing o funzione) disabilitata	 
	 OPT_PGM_GRAPH_CREATION_ENABLED,   		    	    // Creazione grafo di programma (fine parsing o funzione)
	 OPT_PGM_GRAPH_CREATION_DISABLED,   		        // Creazione grafo di programma (fine parsing o funzione) disabilitata
     
	 // Copy specifici se 
	 // OPT_DB_RE_BUILD_COPY_DEF_DISABLED 
	 // OPT_RE_ANALYSIS_COPY_DISABLED 
 	 COPY_TO_FORCE_RE_ANALYSIS,                 	// Nome copy di cui forzare il processo di analisi ricorsivo
 	 COPY_TO_FORCE_RE_BUILD_DEF_ON_DB,          	// Nome copy di cui forzare la ricostruzione della definizione su db (tracciato)

	 // PROCESS_PGM_LEVEL
	 OPT_SOLVE_DYNAMIC_LOCAL_ENABLED,           	// Soluzione istruzioni dinamiche a fine programma
	 OPT_SOLVE_DYNAMIC_LOCAL_DISABLED,    			// Soluzione istruzioni dinamiche a fine programma disabilitata
	 OPT_SOLVE_DYNAMIC_LOCAL_LIGHT_ENABLED,         // Soluzione istruzioni dinamiche con campi con value a fine programma
	 OPT_SOLVE_DYNAMIC_LOCAL_LIGHT_DISABLED,    	// Soluzione istruzioni dinamiche con campi con value a fine programma disabilitata
	 OPT_SOLVE_CODE_CICS_ENABLED,   	        	// Soluzione istruzioni Cics
	 OPT_SOLVE_CODE_CICS_DISABLED,               	// Soluzione istruzioni Cics disabilitata
	 OPT_SOLVE_DML_SQL_ENABLED,     	    		// Soluzione istruzioni Sql
	 OPT_SOLVE_DML_SQL_DISABLED,              	    // Soluzione istruzioni Sql disabilitata
	 	 
	 // PROCESS_SYSTEM_LEVEL 
	 OPT_SOLVE_DYNAMIC_SPREADED_ENABLED,	        // Soluzione istruzioni dinamiche spreaded
	 OPT_SOLVE_DYNAMIC_SPREADED_DISABLED,	        // Soluzione istruzioni dinamiche spreaded disabilitata
	 OPT_DETECT_DATA_MODEL_ENABLED,               	// Ricostruzione modello dati da oggetti e relazioni
	 OPT_DETECT_DATA_MODEL_DISABLED,                // Ricostruzione modello dati da oggetti e relazioni disabilitata
	 OPT_DETECT_INDIRECT_RELATIONSHIPS_ENABLED,     // Individuazione relazioni indirette 
	 OPT_DETECT_INDIRECT_RELATIONSHIPS_DISABLED,    // Individuazione relazioni indirette disabilitata 
	 OPT_ASSIGN_PHISICAL_TO_LOGICAL_FILE_ENABLED,   // Assegnazione file fisico in relazioni con logic file
	 OPT_ASSIGN_PHISICAL_TO_LOGICAL_FILE_DISABLED,  // Assegnazione file fisico in relazioni con logic file disabilitata
	 OPT_EVALUATE_METRICS_SYSTEM_ENABLED,           // Aggregazione metriche di sistema/sottosistem  (a partire da quelle di programma)
	 OPT_EVALUATE_METRICS_SYSTEM_DISABLED,          // Aggregazione metriche di sistema/sottosistema  disabilitata	 

	 // Metrics
	 OPT_METRICS_ALL_ENABLED,           	        // Valutazione metriche complete abilitata
	 OPT_METRICS_ALL_DISABLED,           	        // Valutazione metriche complete disabilitata
	 OPT_METRICS_BASIC_ENABLED,           			// Valutazione metriche di base dimensionali abilitata
	 OPT_METRICS_BASIC_DISABLED,          			// Valutazione metriche di base dimensionale disabilitata
	 OPT_METRICS_DETAIL_ENABLED,           			// Valutazione metriche di dettaglio mainline/section/programma abilitata
	 OPT_METRICS_DETAIL_DISABLED,          			// Valutazione metriche di dettaglio mainline/section/programma disabilitata
	 OPT_METRICS_MCCABE_ENABLED,           			// Valutazione metriche di McCabe abilitata
	 OPT_METRICS_MCCABE_DISABLED,          			// Valutazione metriche di McCabe disabilitata
	 OPT_METRICS_HALSTEAD_ENABLED,           		// Valutazione metriche di Halstead abilitata
	 OPT_METRICS_HALSTEAD_DISABLED,          		// Valutazione metriche di Halstead disabilitata
	 OPT_METRICS_COMPLEXITY_STRUCTURE_ENABLED,      // Valutazione metriche di complessità strutturale fan-in e an-out abilitata
	 OPT_METRICS_COMPLEXITY_STRUCTURE_DISABLED,     // Valutazione metriche di complessità strutturale fan-in e fan-out disabilitata
	 OPT_METRICS_COMPLEXITY_FUNCTION_ENABLED,       // Valutazione metriche di complessità funzionale abilitata
	 OPT_METRICS_COMPLEXITY_FUNCTION_DISABLED,      // Valutazione metriche di complessità funzionale disabilitata
	 OPT_METRICS_FUNCTION_POINT_ENABLED, 			// Valutazione metriche di function point abilitata
	 OPT_METRICS_FUNCTION_POINT_DISABLED,			// Valutazione metriche di function point disabilitata
	 OPT_METRICS_REHOSTING_ENABLED,                 // Valutazione metriche di rehosting abilitata
	 OPT_METRICS_REHOSTING_DISABLED,                // Valutazione metriche di rehosting abilitata
	 OPT_METRICS_PGM_DETAIL_ON_DB_ENABLED,          // Metriche di dettaglio per ogni section/paragrafo su database abilitate
	 OPT_METRICS_PGM_DETAIL_ON_DB_DISABLED,         // Metriche di dettaglio per ogni section/paragrafo su database disabilitate
	 OPT_METRICS_SQUALE_DETECT_VIOLATIONS_ENABLED,  // Valutazione metriche di violazione abilitate
	 OPT_METRICS_SQUALE_DETECT_VIOLATIONS_DISABLED, // Valutazione metriche di violazione disabilitate
	 OPT_METRICS_SQUALE_ENABLED,					// Sistema SQUALE abilitato
	 OPT_METRICS_SQUALE_DISABLED,					// Sistema SQUALE disabilitato
	 OPT_METRICS_SQUALE_VIOLATIONS_SCENARIO_ENABLED,// Personalizzazione violazioni metriche abilitate
	 OPT_METRICS_SQUALE_VIOLATIONS_SCENARIO_DISABLED,// Personalizzazione violazioni metriche disabilitate

	 	 // FUNCTION_PGM_SUMMARY 
	 OPT_PGM_SUMMARY_ALL_ENABLED,	                // Sommario programma con tutte le opzioni previste
	 OPT_PGM_SUMMARY_ALL_DISABLED,	                // Sommario programma con tutte le opzioni previste disabilitato
	 OPT_LIST_SOURCE_CODED_ENABLED,	                // Lista programma codificato
	 OPT_LIST_SOURCE_CODED_DISABLED,	            // Lista programma codificato disabilitata
	 OPT_LIST_XREF_LABEL_ENABLED,                   // Lista Xref label
	 OPT_LIST_XREF_LABEL_DISABLED,                  // Lista Xref label disabilitata
	 OPT_LIST_XREF_SECTION_ENABLED,	                // Lista Xref Sections
	 OPT_LIST_XREF_SECTION_DISABLED,	            // Lista Xref Sections disabilitata
	 OPT_LIST_XREF_SYMBOLS_ENABLED,	                // Lista Xref simboli (literal, data items)
	 OPT_LIST_XREF_SYMBOLS_DISABLED,	            // Lista Xref simboli (literal, data items) disabilitata
	 OPT_LIST_IO_FILE_SYSTEM_ENABLED,	            // Lista I-O file system effettuato dal pgm
	 OPT_LIST_IO_FILE_SYSTEM_DISABLED,	            // Lista I-O file system effettuato dal pgm disabilitata
	 OPT_LIST_IO_SQL_ENABLED,                       // Lista I-O tabelle sql effettuato dal pgm
	 OPT_LIST_IO_SQL_DISABLED,                      // Lista I-O tabelle sql effettuato dal pgm disabilitata
	 OPT_LIST_IO_DL1_ENABLED,                       // Lista I-O segmenti Dl1 effettuato dal pgm
	 OPT_LIST_IO_DL1_DISABLED,                      // Lista I-O segmenti Dl1 effettuato dal pgm disabilitata
	 OPT_LIST_IO_ADABAS_ENABLED,	                // Lista I-O files Adabas effettuato dal pgm
	 OPT_LIST_IO_ADABAS_DISABLED,	                // Lista I-O files Adabas effettuato dal pgm disabilitata
	 OPT_LIST_RELATIONSHIPS_ENABLED,	            // Lista Relazioni selettive fra oggetti
	 OPT_LIST_RELATIONSHIPS_DISABLED,	            // Lista Relazioni selettive fra oggetti disabilitata
	 OPT_LIST_RELATIONSHIPS_ORIGIN_ENABLED,	        // Lista origine Relazioni  
	 OPT_LIST_RELATIONSHIPS_ORIGIN_DISABLED,	    // Lista origine Relazioni disabilitata 
	 OPT_LIST_OPTIONS_ENABLED,	                    // Lista opzioni di programma
	 OPT_LIST_OPTIONS_DISABLED,	                    // Lista opzioni di programma disabilitata
     OPT_LIST_DYNAMIC_CODE_INFO_ENABLED,            // Lista Info di dettaglio istruzioni dinamiche (pasths campi, sottocampi,..)
     OPT_LIST_DYNAMIC_CODE_INFO_DISABLED,           // Lista Info di dettaglio istruzioni dinamiche (pasths campi, sottocampi,..) disabilitata
	 OPT_LIST_DEAD_CODE_ENABLED,                    // Lista dead code programma
	 OPT_LIST_DEAD_CODE_DISABLED,                   // Lista dead code programma disabilitata
	 OPT_LIST_METRICS_ENABLED,                		// Lista metriche di programma
	 OPT_LIST_METRICS_DISABLED,                		// Lista metriche di programma disabilitata
	 OPT_LIST_CRUD_MATRIX_ENABLED,               	// Lista matrice CRUD a livello di programma
	 OPT_LIST_CRUD_MATRIX_DISABLED,               	// Lista matrice CRUD a livello di programma disabilitata
	 OPT_LIST_PROGRAM_GRAPH_ENABLED,                // Lista grafo di programma
	 OPT_LIST_PROGRAM_GRAPH_DISABLED,               // Lista grafo di programma disabilitata
	 
	 // FUNCTION_SYSTEM_SUMMARY    						
	 OPT_LIST_SOURCES_MISSING_ENABLED,	            // Lista sorgenti mancanti per completare analisi
	 OPT_LIST_SOURCES_MISSING_DISABLED,	            // Lista sorgenti mancanti per completare analisi disabilitata
	 OPT_LIST_SOURCES_NOT_IDENTIFIED_ENABLED,	    // Lista sorgenti non identificati da LibraryScan
	 OPT_LIST_SOURCES_NOT_IDENTIFIED_DISABLED,	    // Lista sorgenti non identificati da LibraryScan disabilitata
	 OPT_LIST_OBJECTS_SYSTEM_ENABLED,	        	// Lista oggetti (pgm/files/..) con nomi, stato e opzioni specificate
	 OPT_LIST_OBJECTS_SYSTEM_DISABLED,	        	// Lista oggetti (pgm/files/..) con nomi, stato e opzioni specificate disabilitata
	 OPT_LIST_RELATIONSHIPS_SYSTEM_ENABLED,	        // Lista relazioni selettive per tipo, tipi oggetto, nomi ..
	 OPT_LIST_RELATIONSHIPS_SYSTEM_DISABLED,	    // Lista relazioni selettive per tipo, tipi oggetto, nomi .. disabilitata
	 OPT_LIST_RELATIONSHIPS_ORIGIN_SYSTEM_ENABLED,	// Lista origine Relazioni  
	 OPT_LIST_RELATIONSHIPS_ORIGIN_SYSTEM_DISABLED,	// Lista origine Relazioni disabilitata  
	 OPT_LIST_CRUD_MATRIX_SYSTEM_ENABLED,	        // Lista matrice CRUD a livello di sistema
	 OPT_LIST_CRUD_MATRIX_SYSTEM_DISABLED,	        // Lista matrice CRUD a livello di sistema disabilitata
	 OPT_LIST_METRICS_SYSTEM_ENABLED,	            // Lista metriche di sistema
	 OPT_LIST_METRICS_SYSTEM_DISABLED,	            // Lista metriche di sistema disabilitata
	 
	 
	 ///////////////////////////////////////////////////////////////////////////
	 // Direttive di controllo su esecuzione di processi e funzioni                         
	 ///////////////////////////////////////////////////////////////////////////

	 FILE_NAME_OUTPUT,	    				        // File di output per le funzioni che lo prevedono (non è il path completo)
	 USER_EXIT_CLASS,                       		// Nome classe di exit utente
     CUSTOMER_CODE,               					// Codice cliente  X user exit
     CUSTOMER_INFO,               					// Info specifiche X user exit
     EXECUTION_UNDER_THREAD_ENABLED,        		// Esecuzione sotto thread separato abilitata
     EXECUTION_UNDER_THREAD_DISABLED,       		// Esecuzione sotto thread separato disabilitata
     THREAD_NAME,                           		// Nome thread di esecuzione
     THREAD_GROUP,                          		// Gruppo thread di appartenenza
     THREAD_PRTY,                           		// Priorità thread  
     THREAD_MAX_SUB_THREADS,                		// Numero massimi subthreads da attivare (es. analisi parallele sorgenti)
     
     
	 ///////////////////////////////////////////////////////////////////////////
	 // Direttive specifiche per forward                     
	 ///////////////////////////////////////////////////////////////////////////

	 FWD_FUNCTION_START,	 		            	// Nome classe dichiarazione funzione da eseguire
     
	
	 //////////////////////////////////////////////////////////////////////////////////////////////////////
     // Direttive di selezione tipi oggetti/sorgenti da trattare  in modo massivo (con i filtri eventuali)
     //    parms *SINGLE* name        Viene trattato il sorgente o l'oggetto specificato con name
     //    parms *ALL*                Vengono trattati tutti i sorgenti/oggetti indicati dal tipo di direttiva
	 //////////////////////////////////////////////////////////////////////////////////////////////////////
	
     OBJECT_TYPE_INCLUDE_ALL,                                	  // Tratta  tutti i tipi di sorgenti/oggetti
     OBJECT_TYPE_EXCLUDE_ALL,                                     // Valori immessi precedentemente eliminati
	 OBJECT_TYPE_PGM_COBOL(EnumSourceType.COBOL_PROGRAM),         // Tratta programma Cobol specificato o tutti
	 OBJECT_TYPE_COPY_COBOL_ID(EnumSourceType.COBOL_COPY_ID),     // Tratta modulo Copy Cobol di identification division o tutti
	 OBJECT_TYPE_COPY_COBOL_ENV(EnumSourceType.COBOL_COPY_ENV),   // Tratta modulo Copy Cobol di environment division o tutti
	 OBJECT_TYPE_COPY_COBOL_DATA(EnumSourceType.COBOL_COPY_DATA), // Tratta modulo Copy Cobol di data division o tutti
	 OBJECT_TYPE_COPY_COBOL_PROC(EnumSourceType.COBOL_COPY_PROC), // Tratta modulo Copy Cobol di procedure division o tutti
	 OBJECT_TYPE_SQL_SCRIPT(EnumSourceType.SQL_SCRIPT),           // Tratta Ddl/dml Sql specificato in scripto tutti
	 OBJECT_TYPE_JCL_MVS_JOB(EnumSourceType.JCL_MVS_JOB),         // Tratta sorgente con Jcl job
	 OBJECT_TYPE_JCL_MVS_INCLUDE(EnumSourceType.JCL_MVS_INCLUDE), // Tratta sorgente con Jcl include
	 OBJECT_TYPE_JCL_MVS_PROC(EnumSourceType.JCL_MVS_PROC),       // Tratta sorgente con Jcl proc o tutti
	 OBJECT_TYPE_CICS_BMS(EnumSourceType.CICS_BMS),        		  // Tratta sorgente Cics Bms specificato o tutti


	 
	 /////////////////////////////////////////////////////////////////////////////////////
	 //
	 // Memorizzazioni comandi di esecuzione precedenti come direttiva singola.
	 // Utilizzato da:
	 //   FUNCTION_LIBRARY_SCAN
	 //   PROCESS_ANALYZE_SOURCES
	 //
	 // Alle funzioni e processi di esecuzione viene passato come parametro un arrayList
	 // di oggetti DirectivesInfo con tutte le informazioni di esecuzione correnti come
	 // librerie, files, filtri etc.
	 // Ogni oggetto DirectivesInfo identifica un insieme di oggetti da trattare, con i
	 // propri filtri, librerie, opzioni etc.
	 // L'attivazione dei processi/funzioni dichiarati si effettua con il comando START.
	 //
	 /////////////////////////////////////////////////////////////////////////////////////

	 OBJECTS_IDENTIFICATION_UNIT,                      // Tratta l'insieme di direttive precedenti come un unico insieme da passare alla funzione/processo successivi

	
	 /////////////////////////////////////////////////////////////////////////////////////
	 // I processi recuperano e catalogano informazioni da sorgenti e altre elaborazioni
	 // Le funzioni espongono le informazioni (tranne Library-Scan)
	 // Le elaborazioni singole sono pilotate dalle opzioni specifiche
	 /////////////////////////////////////////////////////////////////////////////////////

	 PROCESS_ANALYZE_SOURCES("ProcessAnalyzeSource"), 			// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_COPY_DATA("ProcessAnalyzeSource"), 		// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_COPY_PROC("ProcessAnalyzeSource"), 		// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_PGM("ProcessAnalyzeSource"), 				// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_CICS_BMS_MAP("ProcessAnalyzeSource"), 		// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_IBM_JCL("ProcessAnalyzeSource"), 			// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_ANALYZE_DDL_SQL("ProcessAnalyzeSource"), 			// Analisi preliminare sorgente, codifica, serializzazione e codice statico
	 PROCESS_PGM_LEVEL("ProcessPgmLevel"), 						// Elaborazione informazioni a livello di programma
	 PROCESS_SYSTEM_LEVEL("ProcessSystemLevel"), 				// Elaborazione informazioni a livello di sistema
	 FUNCTION_CLEAR_DB("FunctionClearDb"), 	                	// Delete righe tabelle livell di sistema/sottosistema
	 FUNCTION_LIBRARY_SCAN("FunctionLibraryScan"), 				// Scan librerie e classificazione sorgenti identificati da piloti e filtri
	 FUNCTION_PGM_SUMMARY("FunctionPgmSummary"), 	    		// Esposizione informazioni a livello di programma
	 FUNCTION_SYSTEM_SUMMARY("FunctionSystemSummary"), 	    	// Esposizione informazioni a livello di sistema/sottosistema

	
	 ///////////////////////////////////////////////////////////////////////////
	 // Avviamento processi e funzione dichiarati precedentemente e aggregati
	 // da OBJECTS_IDENTIFICATION_UNIT
	 ///////////////////////////////////////////////////////////////////////////
	 
	 START;

	

	 /////////////////////////////////////////////////////////////////////////////////////
	 //  Campi istanza enumerazione                                                     //
	 /////////////////////////////////////////////////////////////////////////////////////
	 
	 boolean isProcessOrFunction = false;			// True indica che l'entry descrive una direttiva di processo o funzione
	 
	 // Valido solo per diretttive su tipo sorgente
	 EnumSourceType en_sourceType = EnumSourceType.NOT_ASSIGNED;
	 
	 // Per associazione a stato oggetto da elaborare
	 EnumObjectStatus en_objectStatus = EnumObjectStatus.NOT_ASSIGNED;
	 
	 // Valido solo per diretttive PROCESS_.. o FUNCTION_...
	 String className = "";
	 

	 /*
	 * Costruttore per enumerazioni senza informazioni
	 */
	 EnumDirectivesExecution() {
		 this.en_sourceType = EnumSourceType.NOT_ASSIGNED;
	 }               			

	 /*
	  * Costruttore per enumerazioni tipologie sorgente
	 */
	 EnumDirectivesExecution(EnumSourceType en_sourceType) {
		this.en_sourceType = en_sourceType;
	}
	 
	 /*
	  * Costruttore per enumerazioni stato oggetti da elaborare
	 */
	 EnumDirectivesExecution(EnumObjectStatus en_objectStatus) {
		this.en_objectStatus = en_objectStatus;
	}               			
	 /*
	  * Costruttore per enumerazioni tipologie sorgente
	 */
	 EnumDirectivesExecution(String className) {
		this.className = className;
		this.isProcessOrFunction = true;
	}               			
    
	 /**
	  * Restituisce il tipo sorgente associato
	  */
	public EnumSourceType getSourceType(){
		return this.en_sourceType;
	}
	 /**
	  * Restituisce il nome della classe di esecuizione di processo/funzione
	  */
	public String getClassName(){
		return this.className;
	}

	/**
	 * 
	 * Restituisce true se è una direttiva di processo o funzione
	 * 
	 * @return the isProcessOrFunction
	 */
	public boolean isProcessOrFunction() {
		return isProcessOrFunction;
	}

	/**
	 * @return the en_objectStatus
	 */
	public EnumObjectStatus getObjectStatus() {
		return this.en_objectStatus;
	}

	/**
	 * @param enObjectStatus the en_objectStatus to set
	 */
	public void setObjectStatus(EnumObjectStatus enObjectStatus) {
		this.en_objectStatus = enObjectStatus;
	}

}
