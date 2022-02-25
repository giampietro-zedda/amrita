package enums;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * EnumAmritaExceptionError
 * </h1>
 * Vengono elencate le possibili categorie di errori, che vengono prodotte in output
 * in caso di errore intercettato e di exception standard ExceptionAmrita lanciata.
 * Exception Amrita porta in output la descrizione dell'eccezione intercettata a fronte di errore
 * con in più un elemento di questa enumerazione in coda alla stringa.<br>
 * Vengono elencate le  possibili cause di eccezione nei vari processi e funzioni di amrita.
 * I programmi chiamanti, in caso di errore, intercettano sempre l'exception standard ExceptionAmrita.
 * Tale exception viene generata e lanciata con trow sempre con un costruttore che indica anche un elemento 
 * di questa enumerazione, quando è più significativa della descrizione dell'eccezione origine.
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 26/mar/2010 
 * @see ExceptionAmrita
 *
*/

public enum EnumAmritaExceptionError {
	
	ERROR_SQL,						        // Errore in esecuzione di generica statement Sql
	ERROR_SQL_COMMIT,						// Errore in esecuzione di commit sql
	ERROR_SQL_ROLLBACK,						// Errore in esecuzione di rollback sql
	ERROR_DATA_BASE_DRIVER,			        // Errore in reperimento e caricamento driver
	ERROR_DATA_BASE_SESSION,                // Errore in stabilimento sessione con il database
	ERROR_DATA_BASE_GET_CONNECTION,	        // Errore in acquisizione connessioni pool 
	ERROR_DATA_BASE_ACQUIRE_CONNECTION,	    // Errore in acquisizione connessione da pool
	ERROR_DATA_BASE_DROP_CONNECTION,	    // Errore in rilascio connessione con database
	ERROR_DATA_BASE_INSPECTION,			    // Errore in funzioni di ispezione db/tabelle/indici/..
	ERROR_DB_COLUMN_MAPPING_NULL,			// Errore in mappatura ORM campo classe java / colonna db
	ERROR_DB_COLUMN_TYPE_DETECTING,			// Errore in mappatura ORM campo classe java / colonna db
	ERROR_PROGRAM_INTERNAL,					// Errore interno di programma o feature non implementata
	ERROR_LOGGER_NOT_SET,					// Interfaccia log LoggerFacade non impostato in SystemDefaults
	ERROR_DIRECTIVE_NOT_MANAGED,		    // Errore direttiva in file pilot non gestita
	ERROR_TOO_FEW_PARAMETERS,		        // Errore Numero parametri insufficiente (<3)
	ERROR_NULL_PARAMETER,		            // Errore Paramtro a null
	ERROR_CONFIG_FILE_NOT_FOUND,		    // Errore File di configurazione non trovato
	ERROR_PILOT_PROCESS_FILE_NOT_FOUND,		// Errore File pilota processi fornito come parametro inesistente
	ERROR_PILOT_SOURCES_FILE_NOT_FOUND,		// Errore File pilota sources fornito come parametro inesistente
	ERROR_I_O_FILE_SYSTEM,					// Errore di I/O in lettura/scrittura file su disco
	ERROR_REFLECTION_GET_INFO,		        // Errore durante utilizzando java reflection per ottenere nomi di campi, classi etc
	ERROR_REFLECTION_INVOKE,		        // Errore durante utilizzando java reflection di Invoke di metodi
	ERROR_REFLECTION_NEW_INSTANCE,		    // Errore durante utilizzando java reflection di new instance di costruttori
	ERROR_ANNOTATION_ENTITY,		        // Errore Annotazioni non definite in classe di gestione Entity
	ERROR_GRAPH_CREATION,					// Errore durante creazione grafo di programma
	ERROR_GRAPH_NAVIGATION,					// Errore durante navigazione grafo di programma
	ERROR_PATH_CREATION,					// Errore durante creazione path esecuzione grafo
	ERROR_SOURCE_GETTING,					// Errore durante recupero di un sorgente
	ERROR_SERIALIZE_PGM,					// Errore durante serializzazione programma
	ERROR_SERIALIZE_SQL_SCRIPT,				// Errore durante serializzazione scripy sql
	ERROR_SERIALIZE_COPY,					// Errore durante serializzazione copy
	ERROR_SERIALIZE_JCL,					// Errore durante serializzazione jcl
	ERROR_SERIALIZE_JCL_INCLUDE,			// Errore durante serializzazione jcl include
	ERROR_SERIALIZE_JCL_PROC,			    // Errore durante serializzazione jcl proc
	ERROR_UNSERIALIZE_PGM,					// Errore durante de-serializzazione programma
	ERROR_UNSERIALIZE_COPY,					// Errore durante de-serializzazione copy
	ERROR_UNSERIALIZE_JCL,					// Errore durante de-serializzazione jcl
	ERROR_UNSERIALIZE_JCL_INCLUDE,			// Errore durante de-serializzazione jcl include
	ERROR_UNSERIALIZE_JCL_PROC,			    // Errore durante de-serializzazione jcl proc
	ERROR_UPDATING_XREF,			        // Errore durante update Xref di programmas Cobol 
	ERROR_IDENTIFYING_COBOL_WORD,			// Errore durante identificazione parola riservata Cobol 
	ERROR_SOURCE_ANALYSIS,					// Errore durante analisi istruzioni sorgenti
	ERROR_COPY_ANALYSIS,					// Errore durante analisi copy e/o copy nested
	ERROR_JCL_VAR_SUBSTITUTION,				// Errore durante sostituzione variabili con valore corrente
	ERROR_JCL_DD_OVERRIDE,				    // Errore durante gestione override di dd
	ERROR_JCL_DSNAME_BACKWARD_REFERENCE,	// Errore durante verifica riferimento a dsname backward
	ERROR_JCL_DSNAME_FORWARD_REFERENCE,	    // Errore durante verifica riferimento a dsname forward
	ERROR_JCL_INCLUDE_ANALYSIS,				// Errore durante analisi jcl include
	ERROR_JCL_PROC_ANALYSIS,			    // Errore durante analisi jcl proc
	ERROR_INDEXING_INSTRUCTION,				// Errore durante indicizzazione identificatori istruzione
	ERROR_PARSING_COBOL_COPY,			    // Errore durante parsing di uno statement copy Cobol 
	ERROR_PARSING_COBOL_DATA_DIVISION,		// Errore durante parsing di Data Division Cobol
	ERROR_PARSING_COBOL_PROC_DIVISION,		// Errore durante parsing di Procedure Division Cobol
	ERROR_PARSING_COBOL_ENV_DIVISION,		// Errore durante parsing di Environment Division Cobol
	ERROR_PARSING_COBOL_DATA_ITEM,			// Errore durante parsing di un data item Cobol
	ERROR_PARSING_COBOL_IDENTIFIER,			// Errore durante parsing di un identificatorer Cobol in Procedure Division
	ERROR_PARSING_COBOL_COPY_DATA,			// Errore durante parsing di un copy Cobol di Data Division
	ERROR_PARSING_COBOL_COPY_PROC,			// Errore durante parsing di un copy Cobol di Procedure Division
	ERROR_PARSING_CICS_INSTRUCTION,			// Errore durante parsing Exec Cics .... End-Exec
	ERROR_RELATING_COBOL_PROC_DIVISION,		// Errore durante il relazionamento di istruzioni a fine parsing (IF/END-IF,...)
	ERROR_COMPLETING_COBOL_PERFORM,		    // Errore durante il caricamento dei numeri di istruzione Frm/Thru nelle perform
	ERROR_SOLVING_INSTRUCTION,			    // Errore durante soluzione istruzioni dinamiche
	ERROR_PROGRAM_DESCRIPTOR_CORRUPTED,		// Errore durante utilizzo informazioni di programma codificate, errore di programma
	ERROR_FORWARD_STARTING,					// Errore durante startup di forward
	
	ERROR_UNQUALIFIED;						// 
	
}
