
package analyzer;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.Stack;
import java.util.Map.Entry;

import utilities.DateTimeService;
import utilities.StringService;
import entities.EntityObject;
import entities.EntityObjectOption;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import enums.EnumAmritaExceptionError;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumLanguageItemInstr;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumRelation;
import enums.EnumSourceType;
import exception.ExceptionAmrita;

    
/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * AnalyzerJcl
 * </h1>
 * <p>
 * Questa classe gestisce l'analisi di un sorgente jcl Mvs Completo. <br>
 * <p>
 * Il jcl può contenere le seguenti tipologie di sorgente:
 * <ul>
 * 
 *  <li><b>jcl schedulabile</b></li><br>
 *  Si tratta di un jcl completo contenente una scheda job iniziale
 *  con successive istruzioni jcl di qualsiasi tipo.<br>
 *  Il jcl è pronto per esseere sottomesso.<br>
 *  
 *  <li><b>jcl include</b></li><br>
 *  Si tratta di un sorgente jcl che non contiene una scheda job e non è una proc.<br>
 *  Questo tipo di sorgente è configurabile come una copy, che nel jcl viene chiamata <tt>include</tt><br>
 *  Infatti questo tipo di soorgente jcl viene incluso con uno statement <tt>Include</tt><br>
 *  
 *  <li><b>Proc definition</b></li><br>
 *  Si tratta della definizione di una Proc.<br>
 *  Inizia con 0 o n commenti seguiti dallo sttatement <tt>Proc</tt><br>
 *  Può terminare o meno con lo statement <tt>End Proc</tt><br>
 *  
 *  <li><b>Proc embedded</b></li><br>
 *  Si tratta di un sorgente jcl che inizia con una o più definizioni
 *  di Proc e continua con un jcl schedulabile che inizia con una scheda Job.<br>
 *  Si suppone in questo caso che le proc definite in testa siano utilizzate dal jcl stesso.<br>
 *  Queste ultime vengono analizzate e memorizzate in modo trasparente insieme all'oggetto serializzato
 *  {@link JclMvs} output del processo di analisi del jcl.<br>
 *  Il jcl è pronto per esseere sottomesso.<br>
 *   
 * </ul>
 * 
 * A fine analisi, l'oggetto {@link JclMvs} viene serializzato e prodotto in un file nella directory di default, 
 * con nome <b>sourceName.jclJob</b>, <b>sourceName.jclInclude</b> o <b>sourceName.jclProc</b>, a seconda che il
 * tipo di jcl sia un jcl schedulabile completo di //Job, un modulo include o la definizione di una procedura
 * catalogata.<br>
 * Nel caso l'oggetto jcl serializzato fosse già presente, viene sovrascritto.<br>
 * <p>
 * Il sorgente jcl da analizzare, formalizzato in un oggetto {@link SourceInput}, viene fornito direttamente al
 * metodo pubblico di analisi AnalyzeJcl().<br>
 * <p>
 * L'organizzazione dell'oggetto che descrive il jcl, {@link JclMvs}, è la stessa di quella che descrive
 * un generico programma cobol, {@link ProgramCobol}.<br>
 * Infatti per il jcl viene gestita una struttura di oggetti contenitore (entries) che a loro volta contengono
 * l'istruzione analizzata e codificata come un oggetto {@link InstructionJclMvs}.<br>
 * A livello di entry sono memorizzate informazioni come nome proc o include dentro cui si trova l'iztruzione,
 * livello di nesting e così via.<br>
 * A fine analisi viene inserito su database l'oggetto Jcl e tutte le relazioni fra source name, jobname, 
 * program name, ddname, dsname e coì via.<br>
 * <p>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/01/2001
 * @see InstructionJclMvs
 * @see EnumInstrDataCategory
*/
public class AnalyzerJclMvs extends Analyzer implements AmritaConstants {
	

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi generalizzati  e centralizzati                                           
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

     
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali per analisi sorgente  jcl                                                 
    ////////////////////////////////////////////////////////////////////////////////////////////////////////	

    // Informazioni contesto ricorsivo di esecuzione di partenza.
	// Questo oggetto contiene la map con le variabili impostate da SET.
	// Queste sono sempre valide, anche se impostate da una proc o una include
	// richiamate ricorsivamente.
	private InnerContextAnalysis ictx = null;

    // Map con le variabili impostate da SET.
	// Queste sono sempre valide, anche se impostate da una proc o una include
	// richiamate ricorsivamente.
    private Map<String, String> map_varSet = null;             // Key=nome variabile, Data=valore corrente

	// Descrittore completo jcl corrente.
	// Contiene le istruzioni jcl analizzate per il sorgente jcl.
	// Si tratta di un jcl job, una proc o un'include group.
	private JclMvs jclMvs = null;                  					   
	
	// Tipologia oggetto jcl
	private EnumObject jclType = null;
	
	// Descrittori completi proc embedded.
	// Ogni proc definita prima della scheda job
	// viene analizzata e collocata in questa map.
	// Key  = nome proc
	// Data = InnerProcEmbedded object
	private Map<String, InnerProcEmbedded> map_procEmbedded = null;
	
    // Gestore centralizzato aggiornamenti db cumulativi.
    private AnalyzerDbInfo analyzerDbInfo = null;
   
    // Parole riservate per identificazione istruzione.
    // Key  = parola valida di inizio istruzione
    // Dati = classe interna con enumerazione descrivente l'istruzione jcl
    // La Map viene caricata dal costruttore a partire da EnumInstrDataCategory
    public Map<String, InnerKeyJclInstr> map_keyJclInstr;	

	// Valori correnti generali
    private String sourceNameUnderParsing = "" ;             					  // Nome jcl attualmente sotto analisi           	
	private String procNameEmbeddedUnderParsing = "" ;             				  // Nome proc embedded attualmente sotto analisi           	
	private String jclName = "";                 							      // Coincide con sourceNameUnderParsing
	private boolean isActiveAnalysisProcEmbedded = false;                         // Solo durante analisi di proc embedded
	private int curNumDefJcl = 0;                    							  // Numero di definizione corrente istruzione jcl 0-based
   
	// Informazioni su ultima istruzione analizzata significativa
    private String curProcStepName = "";                    					  // Nome step in ultima exec PROC  incontrata
    private String curPgmStepName = "";                    			 			  // Nome step in ultima exec PGM   incontrata

	// Informazioni processo di override DD statements
    private boolean isDDToAppend = false;                                         // True indica DD da accodare allo step della procedura 

    // Sistema/sottosistema del programma sotto analisi
    private UserExitInfo userExitInfoPgm = null;                                  

    
    
    /**
	 * Costruttore  per analisi sorgente jcl
	 */
	public AnalyzerJclMvs(UserConfiguration sd, ExecutionDirectives di, EnumObject jclType) {
		super(sd, di);
		 
	    // Inizializzazioni varie
		this.jclType = jclType;
		this.analyzerDbInfo = new AnalyzerDbInfo(sd, di, jclName);
		this.jclMvs = new JclMvs(sd, jclName);
		this.jclMvs.setJclType(jclType);
		
		// Allocazione Map parole riservate/Enum per parsing sorgente jcl
		// Allocazione Map con le proce embedded definite nel sorgente
		this.map_keyJclInstr = new HashMap<String,InnerKeyJclInstr>();
		this.map_procEmbedded = new HashMap<String, InnerProcEmbedded>();
		
		// Map variabili impostate dalle istruzioni SET
		this.map_varSet = new HashMap<String, String> ();
		
		// Scan enumerazione con parole riservate Jcl gestite
		for (EnumInstrDataCategory en_reservedWord : EnumInstrDataCategory.values()) {
			if (en_reservedWord == EnumInstrDataCategory.NOT_ASSIGNED) {
				continue;
			}
			
			InnerKeyJclInstr instructionWordsEntry = new InnerKeyJclInstr();
			instructionWordsEntry.en_WordReservedOwner = en_reservedWord;
			instructionWordsEntry.en_InstrCategory = en_reservedWord.getInstrCategory();
			
			this.map_keyJclInstr.put(en_reservedWord.getInstrKey(), instructionWordsEntry);
		}
	} 

 
	/**
	 * <h1>
	 * Analizza il sorgente del jcl memorizzato nell'istanza di SourceInput.<br>
	 * </h1>
	 * Nome jcl e istanza di SourceInput sono stati valorizzati dal costruttore di questa classe.<br>
	 * <p>
	 * @throws Exception 
	 */
	public void analyzeJclJob(SourceInput si, String jclName) throws Exception {
		
		InstructionJclMvs instructionJclMvs = null;
		JclMvsEntry jclEntry = null;
		
		
		initialOperations(si, jclName);   			// Operazioni iniziali per oggetto JclMvs da analizzare
		
		
		// Lettura ahead istruzione nell'oggetto Instruction... corretto
		instructionJclMvs = getNextJclInstruction(this.ictx);
        		
		// Scan istruzioni del jcl da analizzare come oggetto instructionJclMvs completo di info sorgente
		while (instructionJclMvs != null) {

			// Creazione entry di programma con cast corretto e impostazioni varie correnti
			jclEntry = new JclMvsEntry();
		    setJclEntryInfo(this.ictx, instructionJclMvs, jclEntry);
		    
			dispatchInstructionAnalyzer(this.ictx, instructionJclMvs, jclEntry);  // Analisi istruzione e aggiornamento strutture e variabili di controllo
			
			// Se parsing error impostazione flag generale di errore
			if (instructionJclMvs.isParsingError() 
			||  instructionJclMvs.isSemanticError()) {
				this.ictx.isAnyInstructionErrorDetected = true;
			}
			
			// Lettura in ciclo oggetto istruzione successiva
			instructionJclMvs = getNextJclInstruction(this.ictx);
	        
		} // end-while

		
		finalOperations();   
	}

	/*
	 * Impostazione informazioni su entry jcl, dopo individuazione istruzione
	 * 
	 */
	private void setJclEntryInfo(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) {

		int iVar = 0;
		
		// Individuazione se presenti variabili da sostituire
		while (iVar >= 0) {
			iVar = instruction.getSourceInstr().indexOf('&');
			
			// Nessuna variabile
			if (iVar < 0) {
				break;
			}
			iVar++;
			// Variabile temporanea
			if (instruction.getSourceInstr().charAt(iVar) == '&') {
				iVar++;
				continue;
			}
			
			// Variabile
			jclEntry.setAnyVarToReplace(true);
			break;
		}
		
		// Info generali
		jclEntry.setInstruction(instruction);
		jclEntry.setTypeInstr(instruction.getTypeInstrJcl());
		jclEntry.setEntryType(instruction.getTypeInstrJcl().getInstrCategory());
		jclEntry.setUnderProcName(ictx.procName);
		jclEntry.setUnderIncludeName(ictx.includeName);
		jclEntry.setUnderPgmStepName(this.curPgmStepName);
		jclEntry.setUnderProcStepName(this.curProcStepName);
		jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);
		jclEntry.setUnderProc(false);
		jclEntry.setUnderInclude(false);
		if (!ictx.procName.equals("")) {
        	jclEntry.setUnderProc(true);
		}
        if (!ictx.includeName.equals("")) {
        	jclEntry.setUnderInclude(true);
		}
	}


	/**
	 * 
	 * Analizza il modulo include memorizzato nell'istanza di SourceInput.<br>
	 *  
	 * <p>
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 */
	
 
	public void analyzeJclInclude(SourceInput si, String includeName) throws ExceptionAmrita, SQLException {
		
		EntityObject entityObject = null;
		InstructionJclMvs instructionIncludeStmt = null;
		
		//////////////////////////////////////////////////////////
		// (1) Inizializzazioni
		//////////////////////////////////////////////////////////		
		
		// Simulazione condizioni come per analisi dall'interno di un jcl
		this.si = si;
		this.sourceNameUnderParsing = includeName;
		this.jclType = EnumObject.OBJECT_JCL_INCLUDE;;
		this.jclName = includeName;
		this.jclMvs = new JclMvs(ucfg, includeName);
		this.jclMvs.setJclType(this.jclType);
		this.di.curObjectId = includeName;
		this.di.curObjectType = EnumObject.OBJECT_JCL_INCLUDE;
		setInitialContextAnalysis();                     // metodo pubblico di inizializzazione this.ictx	
		this.ictx.si = si;
		this.ictx.ar_RowsSource = si.getArrayRowSource();
		this.ictx.activeTypeSource = EnumSourceType.JCL_MVS_INCLUDE;
		this.ictx.activeSourceName = includeName;
		this.ictx.isAnalysisOfJcl = false;;  
		this.ictx.jclMvs = this.jclMvs;
		this.ictx.isAnalysisOfJcl = true;

		
		// Impostazioni per oggetti di aggregazione da inserire a fine elaborazione
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem("");    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS);
		entityObject.setIdObject("");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem("");   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
        // Impostazioni per oggetto include da inserire/aggiornare a fine elaborazione
	
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(this.jclType);
		entityObject.setIdObject(this.sourceNameUnderParsing);
        // Impostazioni come se il programma fosse alla sua prima analisi
		entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
		entityObject.setFileSource(di.filePathCurObj);   	   			          // Nome file
		entityObject.setLibrarySourceObject(di.libraryCodeCurObj);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
		entityObject.setLibrarySource(di.libraryPathCurObj);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmLastAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");

		// E' il primo oggetto che viene inserito/aggiornato
		this.analyzerDbInfo.addObjEntity(entityObject);
 		
		// Messaggio di inizio analisi
		logMessage(EnumMessageType.INFORMATION, "MI0129", this.sourceNameUnderParsing);

		
		//////////////////////////////////////////////////////////
		// (2) Analisi effettiva include
		//////////////////////////////////////////////////////////		
		
		// Impostazioni per metodo di analisi include
		instructionIncludeStmt = new InstructionJclMvs();
		instructionIncludeStmt.jclStmtPutParm("MEMBER", includeName);
		
        // Analisi include e inserimento in strutture per update su db		
		analyzeIncludeEntries(this.ictx, instructionIncludeStmt);
		
		// Errori di parsing/semantici: fine
		if (this.ictx.isAnyInstructionErrorDetected) {
			this.di.isExecutionWithErrors = true;
		}

		
		//////////////////////////////////////////////////////////
		// (3) Persistenza
		//////////////////////////////////////////////////////////		
		

		// Persistenza su file system già effettuata da analyzeIncludeEntries()
		
		// Persistenza su db: analyzerDbInfo è stato valorizzato con gli aggiornamenti da effettuare
  
	    // Aggiornamenti complessivi effettivi base dati
        if (this.di.optUpdateDb) {
 		   updateProcStatusOnDbStructure(this.ictx.jclMvs);			// Stato analisi  
  	 	   analyzerDbInfo.sqlDeleteGenericLevel();
 		   analyzerDbInfo.sqlDeleteJclProc(this.jclName, false, false);
           analyzerDbInfo.update(true, false, includeName, null);
		}

        // Errore di source non trovato
        if (instructionIncludeStmt.isSemanticError()) {
        	JclMvsEntry jclEntry = new JclMvsEntry();
        	jclEntry.setInstruction(instructionIncludeStmt);
        	instructionIncludeStmt.setTokenInError(includeName);
        	loggingParsingError(jclEntry);
		}
        
	    // Errori di parsing o sources non trovati: logging istruzioni in errore
  		if (this.ictx.isAnyInstructionErrorDetected) {
			loggingParsingErrors();						 
			this.di.isExecutionWithErrors = true;
		}

	}
	
	/**
	 * 
	 * Analizza la proc memorizzata nell'istanza di SourceInput.<br>
	 *  
	 * <p>
	 * @throws ExceptionAmrita 
	 * @throws SQLException 
	 */
	
 
	public void analyzeJclProc(SourceInput si, String procName) throws ExceptionAmrita, SQLException {
	
		EntityObject entityObject = null;
		InstructionJclMvs instructionProcStmt = null;
		
		//////////////////////////////////////////////////////////
		// (1) Inizializzazioni
		//////////////////////////////////////////////////////////		
		
		// Simulazione condizioni come per analisi dall'interno di un jcl
		this.si = si;
		this.sourceNameUnderParsing = procName;
		this.jclType = EnumObject.OBJECT_JCL_PROC;;
		this.jclName = procName;
		this.jclMvs = new JclMvs(ucfg, procName);
		this.jclMvs.setJclType(this.jclType);
		this.di.curObjectId = procName;
		this.di.curObjectType = EnumObject.OBJECT_JCL_PROC;
		setInitialContextAnalysis();                     // metodo pubblico di inizializzazione this.ictx	
		this.ictx.si = si;
		this.ictx.ar_RowsSource = si.getArrayRowSource();
		this.ictx.activeTypeSource = EnumSourceType.JCL_MVS_PROC;
		this.ictx.activeSourceName = procName;
		this.ictx.isAnalysisOfJcl = false;;  
		this.ictx.jclMvs = this.jclMvs;
		this.ictx.isAnalysisOfJcl = true;

		
		// Impostazioni per oggetti di aggregazione da inserire a fine elaborazione
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem("");    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS);
		entityObject.setIdObject("");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem("");   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
        // Impostazioni per oggetto include da inserire/aggiornare a fine elaborazione
	
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(this.jclType);
		entityObject.setIdObject(this.sourceNameUnderParsing);
        // Impostazioni come se il programma fosse alla sua prima analisi
		entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
		entityObject.setFileSource(di.filePathCurObj);   	   			          // Nome file
		entityObject.setLibrarySourceObject(di.libraryCodeCurObj);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
		entityObject.setLibrarySource(di.libraryPathCurObj);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmFirstAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");
   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmLastAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");

		// E' il primo oggetto che viene inserito/aggiornato
		this.analyzerDbInfo.addObjEntity(entityObject);
 		
		// Messaggio di inizio analisi
		logMessage(EnumMessageType.INFORMATION, "MI0129", this.sourceNameUnderParsing);

		
		//////////////////////////////////////////////////////////
		// (2) Analisi effettiva proc
		//////////////////////////////////////////////////////////		
		
		// Impostazioni per metodo di analisi include
		instructionProcStmt = new InstructionJclMvs();
		instructionProcStmt.jclStepPutExecName(procName);
		
        // Analisi include e inserimento in strutture per update su db		
		analyzeExpandProcEntries(this.ictx, instructionProcStmt);
		
		// Errori di parsing/semantici: fine
		if (this.ictx.isAnyInstructionErrorDetected) {
			this.di.isExecutionWithErrors = true;
		}

		
		//////////////////////////////////////////////////////////
		// (3) Persistenza
		//////////////////////////////////////////////////////////		
		

		// Persistenza su file system già effettuata da analyzeIncludeEntries()
		
		// Persistenza su db: analyzerDbInfo è stato valorizzato con gli aggiornamenti da effettuare
  
        // Aggiornamenti complessivi effettivi base dati
        if (this.di.optUpdateDb) {
 		   updateProcStatusOnDbStructure(this.ictx.jclMvs);			// Stato analisi  
 	 	   analyzerDbInfo.sqlDeleteGenericLevel();
 		   analyzerDbInfo.sqlDeleteJclProc(this.jclName, false, false);
           analyzerDbInfo.update(true, false, this.jclName, null);
		}

        // Errore di source non trovato
        if (instructionProcStmt.isSemanticError()) {
        	JclMvsEntry jclEntry = new JclMvsEntry();
        	jclEntry.setInstruction(instructionProcStmt);
        	instructionProcStmt.setTokenInError(procName);
        	loggingParsingError(jclEntry);
		}
        
	    // Errori di parsing o sources non trovati: logging istruzioni in errore
  		if (this.ictx.isAnyInstructionErrorDetected) {
			loggingParsingErrors();						 
			this.di.isExecutionWithErrors = true;
		}

	}
	
	/**
	 * Restituisce il nome del programma correntemente sotto analisi.
	 * 
	 * @return the program name
	 */
	public String getJclName() {
		return this.jclName;
	}



	/**
	 * Impostazione iniziale contesto di analisi.<br>
	 * <p>
	 * Questo metodo viene richiamato da {@link ProcessPgmLevel} per
	 * le elaborazioni a livello di programma, per simulare la stessa
	 * situazione operativa che si ha a fine analisi preliminare del
	 * programma, quando si effettuano contestualmente le stesse operazioni.<br>
	 * <p>
	 * 
	 */
	public void setInitialContextAnalysis() {
		this.ictx = new InnerContextAnalysis ();
		this.ictx.activeTypeSource = EnumSourceType.JCL_MVS_JOB;
		this.ictx.activeSourceName = this.sourceNameUnderParsing;
		this.jclName = this.sourceNameUnderParsing;
		
		this.ictx.si = si;
		this.ictx.ar_RowsSource = this.si.getArrayRowSource();
	}

	/**
	 * 
	 * Restituisce l'oggetto che aggrega tutti gli aggiornamenti da fare
	 * sul db per l'analisi del programma in corso.
	 * 
	 * @return AnalyzerDbInfo updates db container
	 */
	public AnalyzerDbInfo getDbInfo() {
		return this.analyzerDbInfo;
	}

	/**
	 * 
	 * Imposta l'oggetto che aggrega tutti gli aggiornamenti da fare
	 * sul db per l'analisi del programma in corso. <br>
	 * Metodo richiamato a fronte di esecuzione processi a livello pgm
	 * dopo l'analisi preliminare del programma.
	 * @return AnalyzerDbInfo updates db container
	 */
	public void setDbInfo(AnalyzerDbInfo analyzerDbInfo) {
		this.analyzerDbInfo = analyzerDbInfo;
		return;
	}

	
	/**
	 * Restituisce le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @return the userExitInfoPgm
	 */
	public UserExitInfo getUserExitInfoPgm() {
		return userExitInfoPgm;
	}


	/**
	 * Imposta le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @param userExitInfoPgm the userExitInfoPgm to set
	 */
	public void setUserExitInfoPgm(UserExitInfo userExitInfoPgm) {
		this.userExitInfoPgm = userExitInfoPgm;
	}




	 /**
	 * 
	 * Gestione serializzazione di oggetti, tipicamente include e sources jcl.
	 * L'oggetto  viene memorizzato su disco in formato serializzato.
	 * 
	 * @param String dirFileSerialized 		Directory file di output 
	 * @param String suffixFile 			Suffisso file di output (SUFFIX_SERIALIZED_COPY, SUFFIX_SERIALIZED_PGM, ..)
	 * @param String idObject 				Nome file di output 
	 * @param String objectToSerialize 		Istanza oggetto da serializzare
	 * @throws ExceptionAmrita 
	 */
	public void  putSerialized(String dirFileSerialized         // Recuperata dal chiamante da SystemDefaults
			                 , String suffixFile                // Passata dal chiamante da AmritaConstants
			                 , String idObject                  // Nome Jcl, include, ..
			                 , Object objectToSerialize         // Oggetto da serializzare
			                 ) throws ExceptionAmrita {
		
		ExceptionAmrita excp = null;
		ObjectOutputStream oos = null;
		String fileName = null;
		
		fileName = dirFileSerialized + idObject + "." + suffixFile;
		
		try {
			// Open del file
			oos = new ObjectOutputStream(new FileOutputStream(fileName));
			// Scrittura su file oggetto graph corrente
			oos.writeObject (objectToSerialize);
			// Chiususa file
			oos.close ();
 		} catch (Exception e) {
        	logMessage(EnumMessageType.ERROR_INTERNAL, "EI0008", e, idObject, objectToSerialize.getClass().getName(), fileName);
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_JOB)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SERIALIZE_JCL, e);
			}
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_INCLUDE)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SERIALIZE_JCL_INCLUDE, e);
			}
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_PROC)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SERIALIZE_JCL_PROC, e);
			}
 			// Il main gestisce l'eccezione, fornisce info supplementari e decide se continuare l'elaborazione
            throw excp;
		}	
 		
 		// Serializzazione completata: log messaggio informativo
 		 
		if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_JOB)) {
			logMessage(EnumMessageType.INFORMATION, "MI0117", this.sourceNameUnderParsing, fileName, JclMvs.class.getName());
		}
		if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_INCLUDE)) {
			logMessage(EnumMessageType.INFORMATION, "MI0118", this.sourceNameUnderParsing, idObject, fileName, JclMvs.class.getName());
		}
		if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_PROC)) {
			logMessage(EnumMessageType.INFORMATION, "MI0119", this.sourceNameUnderParsing, idObject, fileName, JclMvs.class.getName());
		}
		return;
	}
	
	
	


	/**
	 * 
	 * Restituisce loggetto serializzato con il path fornito.
	 * 
	 * @param String dirFileSerialized 		Directory file di output 
	 * @param String suffixFile 			Suffisso file di output (SUFFIX_SERIALIZED_COPY, SUFFIX_SERIALIZED_PGM, ..)
	 * @param String idObject 				Nome file di output 
	 * @return Object object 				Oggetto deserializzato di cui effettuare il casting
	 * @throws ExceptionAmrita 
	 */
	public Object getSerialized(String dirFileSerialized         // Recuperata dal chiamante da SystemDefaults
			                  , String idObject                  // Nome Copy, programma, ..
			                  , String suffixFile                // Passata dal chiamante da AmritaConstants
		                       ) throws ExceptionAmrita {
		
		Object objectUnserialized = null;
		ExceptionAmrita excp = null;
		ObjectInputStream ois = null;
		String fileName = null;
		
		fileName = dirFileSerialized + idObject + "." + suffixFile;

		try {
			// Open del file
			ois = new ObjectInputStream(new FileInputStream(fileName));
			objectUnserialized = ois.readObject();
			ois.close ();
			
 		} catch (Exception e) {
 			logMessage(EnumMessageType.ERROR_INTERNAL, "EI0009", e, idObject, CopyCobol.class.getName(), fileName);
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_JOB)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_JCL, e);
			}
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_INCLUDE)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_JCL_INCLUDE, e);
			}
			if (suffixFile.equalsIgnoreCase(SUFFIX_SERIALIZED_JCL_PROC)) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_UNSERIALIZE_JCL_PROC, e);
			}
 			// Il main gestisce l'eccezione, fornisce info supplementari e decide se continuare l'elaborazione
            throw excp;
		}

		// De-serializzazione completata: log messaggio informativo
  		if (this.di.optVerboseMessages) {
  			logMessage(EnumMessageType.ERROR_INTERNAL, "MI0120", this.sourceNameUnderParsing, idObject, CopyCobol.class.getName(), fileName);
		}
		return objectUnserialized;
	}



	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// 						Metodi privati  				 /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	/*
	 * Attivazione corretto analizzatore istruzione jcl in base alla divisione attiva.
     * Aggiornamneto strutture contenenti le istruzioni analizzate.
     * Aggiornamento strutture di cross reference del jcl, in
     * base a ogni istruzione analizzata.
	 * 
	 * Questo metodo viene attivato, anche in processo ricorsivo, a fronte di:
	 * 
	 *   1) Analisi jcl completo
	 *   2) Analisi definizione proc catalogata
	 *   3) Analisi definizione proc embedded
	 *   4) Analisi include richiamato nel programma
	 *   5) Analisi include richiamato da include (nested)
	 *   6) Analisi include specifico attivato da analyzeInclude()
	 * 
	 * 
	 */	
	private void dispatchInstructionAnalyzer(InnerContextAnalysis ictx					// Contesto sorgente corrente
								           , InstructionJclMvs instruction				// Istruzione sotto analisi
								           , JclMvsEntry jclEntry 						// Container per l'istruzione
								            ) throws ExceptionAmrita, SQLException {		
		
		
        // Analisi immediata solo se possibile
		// Le exec a proc e a pgm vengono sempre analizzate per impostazioni di servizio
        if ((jclEntry.isThereAnyVarToReplace() && jclEntry.isAllVarReplaced()) 
        ||  !jclEntry.isThereAnyVarToReplace()
        ||   jclEntry.getTypeInstr() == EnumInstrDataCategory.JCL_MVS_STEP) {
        	analyzeInstruction(ictx, instruction, jclEntry);
		}
		 
		// Inserimento istruzione analizzata nella struttura del jcl.
		// L'analisi potrebbe essere a fronte di statement include/proc nested.
		// In questo caso l'oggetto corrente è la include/proc nested
        if (ictx.isInstructionToAppend) {
            this.curNumDefJcl = ictx.jclMvs.addEntry(jclEntry);
		}
		
		// Gestione inclusione modulo include con eventuale analisi contestuale ricorsiva di include/proc 
		if (instruction.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_INCLUDE) {
			analyzeIncludeEntries(ictx, instruction);
		}

		// Gestione inclusione proc embedded o proc con eventuale analisi contestuale  
		if (instruction.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_STEP
		&& !instruction.jclStepIsExecPgm()) {
			analyzeExpandProcEntries(ictx, instruction);  
		}

	}

	
	
	/* --------------------------------
	 * Analisi istruzione specifica
	 * --------------------------------
	 * 
	 * E' disponibile l'istruzione e l'entry che la contiene.
	 * Viene attivato il corretto metodo di analisi.
	 * 
	 */
    private void analyzeInstruction(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {

    	// Analisi istruzioni specifiche
		switch (instruction.getTypeInstrJcl()) { 

			    // Istruzioni Jcl trattate direttamente
		
				case JCL_MVS_SET:  
					analyzeJclSet(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_JOB:  
					analyzeJclJob(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_STEP:  
					analyzeJclStep(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_DD:  
					analyzeJclDD(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_DELIMITER:  
					analyzeJclSysinStream(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_SYSIN_STREAM:  
					analyzeJclSysinStream(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_INCLUDE: 
					analyzeJclInclude(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_JCLLIB:  
					analyzeJclJcllib(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_IF_THEN:  
					analyzeJclIfThen(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_END_IF:  
					analyzeJclIfEnd(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_PROC:  
					analyzeJclProcStmt(ictx, instruction, jclEntry);
					break;
				case JCL_MVS_PROC_END:  
					analyzeJclProcEnd(ictx, instruction, jclEntry);
					break;
					
				// Istruzioni jcl, jes2, jes3 residue non influenti ai fini di oggetti e relazioni
				default:
					if (ictx.activeTypeInstr.getInstrCategory() == EnumInstrDataCategory.JCL_MVS_NATIVE) {
						analyzeJclGeneric(ictx, instruction, jclEntry);
						break;
					}
					if (ictx.activeTypeInstr.getInstrCategory() == EnumInstrDataCategory.JCL_MVS_JES2) {
						analyzeJclJes2(ictx, instruction, jclEntry);
						break;
					}
					if (ictx.activeTypeInstr.getInstrCategory() == EnumInstrDataCategory.JCL_MVS_JES3) {
						analyzeJclJes3(ictx, instruction, jclEntry);
						break;
					}
					
	    	
		} // end-switch
		
	}


	/* -----------------------------------------
     * Replace variabili in source istruzione
     * -----------------------------------------
     * 
     * Vengono risolti i valori correnti delle variabili presenti
     * nel sorgente dell'istruzione, procedendo alla sostituzione.
     * 
     * Vengono risolti prima i valori impostati nel livello corrente
     * di stack, come quello di una proc richiamata e successivamente
     * i valori globali impostati dalle SET incontrate precedentemente.
     *
     * Il livello corrente di stack contiene i valori eventualmente 
     * impostati da una SET precedente e rimpiazzati per esempio
     * solo per una specifica esecuzione di una proc.
     * 
     * Le sostituzioni vengono effettuate direttamente sul source 
     * dell'istruzione.
     */
	private void manageReplaceVarOnSourceInstruction(InnerContextAnalysis ictx
												   , InstructionJclMvs instruction,
													 JclMvsEntry jclEntry) {
		
		
		String sourceInstr = null;							// Source completo istruzione  
		String sourceInstrReplaced = null;					// Source completo istruzione dopo sostituzioni  
        String tokenError = "";
		int i = 0;              
		
		// Prima di sostituzione
		sourceInstr = instruction.getSourceInstr();
        
		// Sostituzioni correnti e da SET
		sourceInstrReplaced = replaceVarOnSourceInstruction(ictx.map_var, instruction);
		instruction.setSourceInstr(sourceInstrReplaced);
		sourceInstrReplaced = replaceVarOnSourceInstruction(this.map_varSet, instruction);
		instruction.setSourceInstr(sourceInstrReplaced);

		// Non è stata effettuata qualche sostituzione e non c'erano variabili da sostituire
		if (sourceInstr.equals(sourceInstrReplaced) && sourceInstrReplaced.indexOf("&") == -1) {
			return;
		}
		
		// Flag presenza variabili da sostituire
		jclEntry.setAnyVarToReplace(true);
		jclEntry.setAllVarReplaced(true);
		
		// Flag presenza di altre variabili da sostituire.
		// Necessario scan e analisi puntuale in quanto potrebbero esserci 
		// dataset temporanei &&datasetName
		i = sourceInstrReplaced.indexOf("&");
		while (i >  0) {
			
			// Dataset temporaneo: skip
			if (sourceInstrReplaced.substring(i + 1, i + 2).equals("&")) {
				i = sourceInstrReplaced.indexOf("&", i + 2);
				continue;
			}
			
			// Variabile ancora da sostituire
			if (sourceInstrReplaced.substring(i, i + 1).equals("&")) {
				jclEntry.setAllVarReplaced(false);
				tokenError = StringService._pad(sourceInstrReplaced.substring(i), ' ', 10, StringService.PAD_RIGHT);
				instruction.setInfoError(EnumMessageType.ERROR_INPUT, "ET0030", tokenError, null, this.jclName);
				instruction.setSemanticError(true);
				break;
			}
		}
		
		// Replace source istruzione con variabili sostituite
		instruction.setSourceInstr(sourceInstrReplaced);
		
	}

    /*
     * Sostituzione variabili presenti nell'istruzione con i valori
     * della map fornita
      * 
     */
    private String replaceVarOnSourceInstruction(Map<String, String> mapVar, InstructionJclMvs instruction) {
		
    	String sourceInstrReplaced = "";
    	String parmName = "";								// Nome parametro
		String parmValue = "";								// Valore parametro

        sourceInstrReplaced = instruction.getSourceInstr();
        
 		// Scan entries coppie correnti parametro/valore
		for (Entry<String, String> entryVar : mapVar.entrySet()) {
			parmName = entryVar.getKey();
			parmValue = entryVar.getValue();
			
			// Valore stringa fra apici: elimino apici
			if (parmValue.startsWith("'")) {
				parmValue = parmValue.substring(1);
				parmValue = parmValue.substring(0, parmValue.length() - 1);
			}
			
			// Sostituzioni a cascata
			sourceInstrReplaced = sourceInstrReplaced.replace("&"+parmName+".", parmValue);  	// Eventuale valore con   "." finale
			sourceInstrReplaced = sourceInstrReplaced.replace("&"+parmName, parmValue);  		// Eventuale valore senza "." finale
			
		}

    	return sourceInstrReplaced;
	}


	/* -----------------------------------------
     * Aggiornamento variabili e valori correnti
     * -----------------------------------------
     * 
     * Il valore delle variabili viene sostituito nell'istruzione
     * sorgente prima che questa sia analizzata.
     * 
     * Le variabili vengono definite oppure aggiornate da:
     * 
     * 1) Istruzione SET
     * 2) Definizione PROC
     * 3) Richiesta di espansione PROC con EXEC PROC=..
     * 
     * Il corretto valore corrente per le variabili è memorizzato nella
     * map della struttura di contesto attivo InnerContextAnalysis ictx.
     * 
     * A fronte di SET vengono aggiornata la map con le  variabili 
     * del contesto corrente.
     * 
     * A fronte di EXEC PROC=.., viene creato un nuovo livello di stack
     * per il contesto corrente, clonata la ma con le variabili correnti,
     * e quindi aggiornata con le variabili presenti nello statement EXEC.
     * 
     * Per tutta l'espansione della proc valgono le variabili aggiornate
     * con i valori dichiarati all'espansione.
     * Al termine dell'espansione viene ripristinato il contesto attivo
     * al momento della exec della proc, incluse le variabili correnti.
     * 
     * In questo metodo viene aggiornata la map delle variabili nella 
     * struttura di contesto corrente, a partire dalla istruzione fornita.
     * L'istruzione fornita può essere una SET o una EXEC PROC=..
     * 
     */
	private void updateMapCurrentParmAndValues(InnerContextAnalysis ictx
										     , InstructionJclMvs instruction) {
		
		ArrayList<String> al_parmName = null;				// Elenco nomi parametro
		String parmValue = "";								// Valore parametro

		// Nomi parametro dichiarati in SET o EXEC PROC
		al_parmName = instruction.jclStmtGetParmNames();
		
		// Scan nomi parametro 
		for (String parmName : al_parmName) {
			parmValue = instruction.jclStmtGetParm(parmName);
			ictx.map_var.put(parmName, parmValue);  			// Inserisce o aggiorna il valore per il parametro
		}
	}

	
    /* ---------------------------------------------------------
     * Aggiornamento variabili e valori correnti istruzione SET
     * ---------------------------------------------------------
     * 
     * Le variabili vengono definite oppure aggiornate istruzione SET 
     * e sono definite e/o aggiornate in modo globale durante l'analisi
     * del jcl.
     * Se sono aggiornate da SET dentro proc o include richiamate ricorsivamente
     * i valori sono validi e permanenti in generale. 
     * 
     */
	private void updateMapCurrentParmAndValuesSet(InstructionJclMvs instruction) {
		
		ArrayList<String> al_parmName = null;				// Elenco nomi parametro
		String parmValue = "";								// Valore parametro

		// Nomi parametro dichiarati in SET o EXEC PROC
		al_parmName = instruction.jclStmtGetParmNames();
		
		// Scan nomi parametro 
		for (String parmName : al_parmName) {
			parmValue = instruction.jclStmtGetParm(parmName);
			this.map_varSet.put(parmName, parmValue);  			// Inserisce o aggiorna il valore per il parametro
		}

	}

	
	/*
	 * --------------------------
	 * Analisi statement // JOB
	 * --------------------------
	 * 
	 * 
	 */
	private void analyzeJclJob(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String jobName = "";				    // Nome job
		String token = "";				        // Token
        int iJOB = 0;                           //
        
 		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//JobName
		jobName = token.substring(2);
		instruction.jclStmtPutName(jobName);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
	    iJOB = sourceInstr.indexOf(" JOB ");
	    str = sourceInstr.substring(iJOB + 5);
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}

 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded

	}
			 
	/* ------------------------------------------
	 * Analisi statement //Step EXEC 
	 * ------------------------------------------
	 * 
	 * 
	 * L'exec può essere relativa a:
	 * 
	 * 1) Exec di un programma
	 * 2) Exec di una proc catalogata
	 * 3) Exec di una proc embedded
	 * 
	 * In caso di esecuzione di una proc la sintassi può essere
	 * 
	 * 1) //stepName EXEC procName
	 * 2) //stepName EXEC PROC=procName
	 * 
	 * In caso di esecuzione di un programma la sintassi sarà
	 * 
	 * 1) //name EXEC PGM=pgmName
	 * 
	 * 
	 */
	private void analyzeJclStep(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {

		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String stepName = "";				    // Nome step
		String pgmExecuted = "";				// Programma eseguito
		String procExecuted = "";				// prorocedura catalogata richiamata
		String token = "";				        // Token
        int iEXEC = 0;                          //
        
 		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//StepName
		if (token.length() > 2) {
			stepName = token.substring(2);
			instruction.jclStmtPutName(stepName);
		}

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
	    iEXEC = sourceInstr.indexOf(" EXEC ");
	    str = sourceInstr.substring(iEXEC + 6).trim();
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}

		// Memorizzazione parametri come una hash map
		instruction.jclStmtPutParms(map_setVar);

		
		pgmExecuted = map_setVar.get("PGM");
		procExecuted = map_setVar.get("PROC");
		
		// EXEC PGM=pgmName
		if (pgmExecuted != null) {
			instruction.jclStepSetExecPgm(true);
			instruction.jclStepPutExecName(pgmExecuted);
			this.curPgmStepName = stepName;
			jclEntry.setUnderPgmStepName(this.curPgmStepName);
			
	    // EXEC PROC=procName
		} else if (procExecuted != null) {
			instruction.jclStepSetExecPgm(false);
			instruction.jclStepPutExecName(procExecuted);
			this.curProcStepName = stepName;
			ictx.procExecStmt = instruction;
			jclEntry.setUnderProcStepName(this.curProcStepName);
			updateMapCurrentParmAndValues(ictx, instruction);     // Update map parametro/valore in livello corrente di stack
			
		// EXEC procName
		} else {
			procExecuted = map_setVar.get("#1");
			instruction.jclStepSetExecPgm(false);
			instruction.jclStepPutExecName(procExecuted);
			this.curProcStepName = stepName;
			ictx.procExecStmt = instruction;
			jclEntry.setUnderProcStepName(this.curProcStepName);
			updateMapCurrentParmAndValues(ictx, instruction);     // Update map parametro/valore in livello corrente di stack
		}

 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
	/*
	 * ----------------------------
	 * Analisi statement // SET
	 * ----------------------------
	 * 
	 * 
	 * Vengono analizzate le coppe nome+valore.
	 * Le assegnazioni possono essere spalmate su + righe di continuazione.
	 * 
	 * 
	 */
	private void analyzeJclSet(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String setName = "";				    // Nome istruzione Set come //SetName   SET ..
		String token = "";				        // Token
        int iSET = 0;
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//SetName
		
		if (token.length() > 2) {
			setName = token.substring(2);
			instruction.jclStmtPutName(setName);
		}

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
	    iSET = sourceInstr.indexOf(" SET ");
	    str = sourceInstr.substring(iSET + 5);
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione parametri come una hash map
		instruction.jclStmtPutParms(map_setVar);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}
		
  	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded

	}
	
	
	
	/*
	 * ---------------------------
	 * Analisi statement DD
	 * ---------------------------
	 * 
	 * 
	 * Analisi statement //ddname DD ...
	 * Analisi statement //ProcStepName.ddname DD ...
	 * Analisi statement //   DD ...
	 * 
	 * La scheda dd può essere quella principale, con una ddname indicata,
	 * oppure concatenata, ovvero senza indicazione di ddname.
	 * In questo caso la dd risulta concatenata all'ultima dd completa inserita
	 * 
	 */
	private void analyzeJclDD(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
	
		Map<String, String> map_setVar = null;  //          
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String ddName = "";				        // Nome dd   come //ddname
		String procStepName = "";				// Nome step come //procStepName.ddname
		String str = "";						// Work
		String token = "";				        // Token
        int i = 0;
        int iDD = 0;
        
        sourceInstr = instruction.getSourceInstr();
                 
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					 
		
		// DD concatenata
		if (token.equals("//")) { 
			instruction.jclDDSetChained(true);
			
		// DD con nome
		} else if (token.indexOf(".") == -1) {
			instruction.jclDDSetOverride(false);
			ddName = token.substring(2).trim();
			instruction.jclStmtPutName(ddName);
			instruction.jclDDPutName(ddName);
			
		// DD Override di step
		} else {
		  jclEntry.setOverride(true);
		  i = token.indexOf(".");
		  procStepName = token.substring(2, i);
		  ddName = token.substring(i + 1);
		  instruction.jclDDSetOverride(true);
		  instruction.jclStmtPutName(token.substring(2));  			// -> ProcStepName.ddName
		  instruction.jclDDPutOverrideProcStepName(procStepName);   // -> ProcStepName
		  instruction.jclDDPutOverrideDDName(ddName);               // -> ddname
		  instruction.jclDDPutName(ddName);							// -> ddname
		}

        // Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
        iDD = sourceInstr.indexOf(" DD ");
        str = sourceInstr.substring(iDD + 4).trim();
        
		// Memorizzazione singole coppie nome/valore
		map_setVar = createHashMapFromVariabileAssign(str);
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
			// Riferimento backward
			if (entryMap.getKey().equals("DSN")
			&&  entryMap.getValue().startsWith("*")	) {
				jclEntry.setReferenceBackward(true);
			}
			// Riferimento forward
			if (entryMap.getKey().equals("DDNAME")) { 
				jclEntry.setReferenceForward(true);
			}
		}
		
		// Suppongo non sia sysin embedded
		ictx.ddStmtWithSysinEmbedded = null;
		ictx.ddSysinDelimiter = "";
		
		// Non è Sysin embedded: il primo parametro posizionale NON è "*"
		if (str.startsWith("*")) {
			instruction.jclDDSetInputEmbedded(true);
			ictx.ddStmtWithSysinEmbedded = instruction;
			ictx.ddSysinDelimiter = "/*";
			if (instruction.jclStmtGetParm("DLM") != null) {
			   ictx.ddSysinDelimiter = instruction.jclStmtGetParm("DLM") ;
			}
		}
		
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}

	
	/*
	 * ----------------------------
	 * Analisi statement // JCLLIB
	 * ----------------------------
	 * 
	 * 
	 */
	private void analyzeJclJcllib(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {

		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String jcllibName = "";				    // Nome job
		String token = "";				        // Token
        int iJCLLIB = 0;                        //
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//JclLibName
		jcllibName = token.substring(2);
		instruction.jclStmtPutName(jcllibName);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
		iJCLLIB = sourceInstr.indexOf(" JCLLIB ");
	    str = sourceInstr.substring(iJCLLIB + 8);
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}

 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded

	}
		 
	/*
	 * Analisi statement // IF cond THEN
	 */
	private void analyzeJclIfThen(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {

		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String jclCondition = "";				// Condizione
		String token = "";				        // Token
		String ifName = "";                     //
        int iIF = 0;                            //
        int iTHEN = 0;                          //
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//IfName
		ifName = token.substring(2);
		instruction.jclStmtPutName(ifName);

        // Inserimento condizione nell'istruzione
		iIF = sourceInstr.indexOf(" IF ");
		iTHEN = sourceInstr.indexOf(" THEN");
		jclCondition = sourceInstr.substring(iIF + 4, iTHEN);
		instruction.jclIfPutCondition(jclCondition);

 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
		 
	/*
	 * Analisi statement // END-IF
	 */
	private void analyzeJclIfEnd(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
		 
	/*
	 * Analisi statement // PROC
	 * 
	 * La proc può essere lo statement di definizione di una procedura catalogata,
	 * unica nel sorgente jcl, oppure una proc embedded che può trovarsi in qualsiasi
	 * punto del sorgente, all'inizio, prima o dopo la scheda job.
	 * 
	 * Le proc embedded vengono memorizzate in map_procEmbedded
	 * 
	 */
	private void analyzeJclProcStmt(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		InnerProcEmbedded innerProcEmbedded = null;	//
		Map<String,String> map_setVar = null;		//
		Scanner scn = null;							// Scanner gestione tokens
		String sourceInstr = null;					// Source completo istruzione senza punto
		String str = "";							// Work
		String procName = "";				    	// Nome statement PROC
		String token = "";				        	// Token
        int iPROC = 0;                           	//
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);						//procName  
		procName = token.substring(2);
		instruction.jclStmtPutName(procName);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
		iPROC = sourceInstr.indexOf(" PROC ");
	    str = sourceInstr.substring(iPROC + 6);
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}
		
		// Non può essere una proc embedded  
		if (ictx.jclMvs.getJclType() == EnumObject.OBJECT_JCL_PROC) {
			this.ictx.procExecStmt = instruction;							// Utilizzata in espansione proc
			return;
		}
		
		// Si tratta di una proc embedded 
		
		this.procNameEmbeddedUnderParsing = procName;
		this.isActiveAnalysisProcEmbedded = true;
		innerProcEmbedded = new InnerProcEmbedded();
		innerProcEmbedded.iJclStart = this.curNumDefJcl;
		innerProcEmbedded.jclInstrProc = instruction;
		innerProcEmbedded.procName = procName;
		this.map_procEmbedded.put(procName, innerProcEmbedded);
	 	jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	 
		jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);         
	}


	/*
	 * Analisi statement // PEND
	 * 
	 */
	private void analyzeJclProcEnd(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		InnerProcEmbedded innerProcEmbedded = null;	 
		
		// Fine definizione procedura catalogata: exit
		if (!this.isActiveAnalysisProcEmbedded) {
			return;
		}
		
		// Fine proc embedded
		innerProcEmbedded = this.map_procEmbedded.get(this.procNameEmbeddedUnderParsing);
		innerProcEmbedded.iJclEnd = this.curNumDefJcl + 1;
		jclEntry.setUnderProcEmbedded(true);
		jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);
		this.procNameEmbeddedUnderParsing = "";
		this.isActiveAnalysisProcEmbedded = false;
	}
		 

	/*
	 * Analisi schede dati sysin come, per esempio:
	 * 
	 * //SYSIN DD *  
	 * CARD1
	 * CARD2
	 * ..
	 * /*
	 * 
	 * Oppure:
	 * 
	 * //P1.DD1 DD *,DLM=FFFF
	 * CARD1
	 * CARD2
	 * ..
	 * FFFF
	 * 
	 * 
	 * Le schede dati vengono inserite nell'istruzione jcl DD di pertinenza sotto cui sono definite.
	 * In fase di gestione override le schede di sysin vengono inserite nell dd corretta.
	 * 
	 */
	private void analyzeJclSysinStream(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		Scanner scn = null;							// Scanner gestione tokens
		String sourceInstr = null;					// Source completo istruzione senza punto
        String token = "";
        
		sourceInstr = instruction.getSourceInstr();
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);						 
		
		// Fine sysin normale, senza custom delimiter
		if (token.equals("/*")) {
			ictx.isSysinEmbedded = false;
			ictx.ddStmtWithSysinEmbedded = null;
			return;
		}
		
		// Fine sysin con custom delimiter
		if (ictx.ddSysinDelimiter.equals("") 
		&& ictx.ddSysinDelimiter.equals(token)) {
			ictx.isSysinEmbedded = false;
			ictx.ddStmtWithSysinEmbedded = null;
			return;
		}
		
		// Accodamento scheda sysin in istruzione DD di appartenenza
		ictx.ddStmtWithSysinEmbedded.jclDDPutSysinCard(sourceInstr);
		
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded

 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
	
	
	/*
	 * ------------------------------
	 * Analisi include statement
	 * ------------------------------
	 * 
	 */
	private void analyzeJclInclude(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		 
		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String includeName = "";				// Nome statement INCLUDE
		String token = "";				        // Token
        int iINCLUDE = 0;                       //
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					//name  INCLUDE 
		includeName = token.substring(2);
		instruction.jclStmtPutName(includeName);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
		iINCLUDE = sourceInstr.indexOf(" INCLUDE ");
	    str = sourceInstr.substring(iINCLUDE + 9);
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
	    // C'è solo MEMBER=member
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}
		
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded

 		return;


	}

	/*
	 * ----------------------------------------------------------
	 * Analisi statement generico non contemplato nei precedenti
	 * ----------------------------------------------------------
	 * 
	 */
	private void analyzeJclGeneric(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
		 
	/* -----------------------------
	 * Analisi statement Jes2
	 * -----------------------------
	 * 
	 * 
	 * Si tratta di comandi del tipo /*jes2command ...
	 */
	private void analyzeJclJes2(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {

		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String jes2Command = "";				// Comanod Jes2 come /*ROUTE
		String token = "";				        // Token
        
		sourceInstr = instruction.getSourceInstr();
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					// /*Jes2Command
		jes2Command = token.substring(2);
		instruction.jclSetJes2Command(jes2Command, true);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
	    str = sourceInstr.substring(token.length()).trim();
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}
		
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
		 
	/* -----------------------------------
	 * Analisi command e statement Jes3
	 * -----------------------------------
	 * 
	 * 
	 * Si tratta di comandi del tipo //*jes3command ...
	 */
	private void analyzeJclJes3(InnerContextAnalysis ictx, InstructionJclMvs instruction, JclMvsEntry jclEntry) throws ExceptionAmrita {
		
		Map<String,String> map_setVar = null;
		Scanner scn = null;						// Scanner gestione tokens
		String sourceInstr = null;				// Source completo istruzione senza punto
		String str = "";						// Work
		String jes3Statement = "";				// Statement Jes3 come //*DATASET
		String token = "";				        // Token
        
		sourceInstr = instruction.getSourceInstr();
		
		// Comando Jes3 come //**VARY,4: exit
		if (instruction.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_JES3_COMMAND) {
			return;
		}
		
		scn = new Scanner(sourceInstr);
		token = nextToken(scn);					// //*Jes3Command
		jes3Statement = token.substring(3);
		instruction.jclSetJes3Command(jes3Statement, true);

		// Estrazione generalizzata coppie nome parametro/valore
		// Viene restituita una hash map con le coppie individuate.
		// Se si tratta di parametri posizionali la chiave sarà #n
		// con n 1-based
		
	    str = sourceInstr.substring(token.length()).trim();
	    map_setVar = createHashMapFromVariabileAssign(str);
		
		// Memorizzazione singole coppie nome/valore
		
		// Scan map entries
		for (Entry<String, String> entryMap : map_setVar.entrySet()) {
			instruction.jclStmtPutParm(entryMap.getKey(), entryMap.getValue());
		}
		
 	    jclEntry.setUnderProcEmbedded(this.isActiveAnalysisProcEmbedded);	// True solo se 
	    jclEntry.setUnderProcName(this.procNameEmbeddedUnderParsing);        //    istruzione di proc embedded
	}
		 

	
	/*
	 * --------------------------------------------------------------------------
	 * Estrazione coppie nome variabile/valore e restituzione hash map di output
	 * --------------------------------------------------------------------------
	 * 
	 * In caso di valori posizionali, ovvero stringhe, literal, valori fra parentesi
	 * e altro, senza operatore = di assegnazione, vengono inseriti nella hash map
	 * con chiave #n, dove n è il numero di parametro 1-based assegnato.
	 * I valori posizionali e i parametri, sono separati da una virgola;
	 * 
	 * Questo metodo viene utilizzato per tutte le istruzioni che contengono assegnazioni
	 * di parametri come // JOB, DD, SET etc.
	 * 
	 * 
	 */
	private Map<String, String> createHashMapFromVariabileAssign(String str) {

		HashMap<String, String> map_setVar = null;
		String parmKey = "";
		String parmValue = "";
		int i = 0;
		int iStartSet = 0;
		int iEndSet = 0;
		int cntParm = 0;
		
		map_setVar = new HashMap<String, String>();
		i = StringService._firstNoSpace(str, 0);
		iStartSet = i;
		   
		// Scan stringa da analizzare
		for (; i < str.length(); i++) {
			
			// Parametro posizionale Literal '
			if (str.substring(i, i + 1).equals("'")) {
	           for (i = i + 1; i < str.length(); i++) {
					if (!str.substring(i, i + 1).equals("'")) {continue;}   // Non è sicuramente un apice di chiusura
					// Doppio apice dentro stinga: non è di chiusura
					if (str.substring(i + 1, i + 2).equals("'")) {
						i = i + 1;
						continue;
					} // end-if
					
					// Apice di chiusura
					break;
	           } // end-for 
			    
	           iEndSet = i;
	           parmValue = str.substring(iStartSet, iEndSet + 1);
	           cntParm++;
	           map_setVar.put("#"+cntParm, parmValue);
	           iStartSet = i + 1;
	           continue;
	           
 			} // End-if 
			
			// Virgola: bypass
			if (str.substring(i, i + 1).equals(",")) {
				// Parametro posizionale
				if (i > iStartSet) {
					parmValue = str.substring(iStartSet, i).trim();
		            cntParm++;
		            map_setVar.put("#"+cntParm, parmValue);
				}
				iStartSet = i + 1;
				continue;
				
			} // end-if

			// Assegnazione
			if (str.substring(i, i + 1).equals("=")) {
				iEndSet = i;
				parmKey = str.substring(iStartSet, iEndSet).trim();
				iStartSet = i + 1;
				iEndSet = 0;
				
				// Ricerca pointer fine valore, bypass eventuale literal
				for (i++; i < str.length(); i++) {
					if (str.substring(i, i + 1).equals(" ")) {continue;}
					if (str.substring(i, i + 1).equals("'")) {
						// Bypass literal
						for (i++; i < str.length(); i++) {
							// Bypass doppi apici
							if (str.substring(i, i + 1).equals("'") 
							&& i < str.length() - 1 
							&& str.substring(i + 1, i + 2).equals("'") ) {
								i = i + 1;
								continue;
							}
							// Apice di chiusura
							if (str.substring(i, i + 1).equals("'")) {
								iEndSet = i;
								break;
							}
						} // end-for
					} // end-for
					
					// Valore literal
					if (iEndSet > 0) {
						break;
					}
					
					// Virgola di chiusura parametro
					if (str.substring(i, i + 1).equals(",")) {
						iEndSet = i - 1;
						break;
					} // end-if
					
				} // end-for
				
				// Ultimo parametro: valore fino a fine stringa
				if (i >= str.length()) {
					iEndSet = str.length() - 1;
				} // end-if
				
				// Estrazione valore
				parmValue = str.substring(iStartSet, iEndSet + 1).trim();
				map_setVar.put(parmKey, parmValue);
				iStartSet = i + 1;
				continue;
			}
		}
		
		return map_setVar;
	}




	

	/*
	 * ---------------------------------------
	 * Analisi ricorsiva include jcl.
	 * ---------------------------------------
	 * 
	 * Questo metodo viene richiamato durante l'analisi di un jcl o di una include richiamata
	 * ricorsivamente da un programma, oppure direttamente dalla funzione di analisi include,
	 * tramite il metodo analyzeInclude();
	 * 
	 */
 	private JclMvs analyzeIncludeRecursive(InnerContextAnalysis ictx				// Contesto sorgente chiamante
	    		                         , String includeNameToAnalyze  		    // Nome include da analizzare
	    								 , SourceInput si							// Imclude da analizzare
	    		                          ) throws ExceptionAmrita, SQLException {
    	
    	
 
		InstructionJclMvs instructionJclMvs = null;							    // Istruzione jcl generica		
		JclMvsEntry jclEntry = null;											// Entry generico di jcl
     	InnerContextAnalysis ictxIncludeCalled = null;						    // Nuovo oggetto contesto include da analizzare
       	JclMvs objectIncludeJcl = null;           							    // Oggetto include output del processo di analisi
        
        // Il nuovo contesto diventa il sorgente della include da analizzare, a partire dal contesto corrente
		objectIncludeJcl = new JclMvs(ucfg, includeNameToAnalyze);
		objectIncludeJcl.setJclType(EnumObject.OBJECT_JCL_INCLUDE);
     	ictxIncludeCalled = ictx.clone();
       	ictxIncludeCalled.jclMvs = objectIncludeJcl;							// Istanziato dopo
    	ictxIncludeCalled.ar_RowsSource = si.getArrayRowSource(); 
    	ictxIncludeCalled.activeTypeSource = EnumSourceType.JCL_MVS_INCLUDE;
    	ictxIncludeCalled.includeStmt = ictx.includeStmt;
       	ictxIncludeCalled.includeName = includeNameToAnalyze;
        ictxIncludeCalled.activeSourceName = includeNameToAnalyze;
       	ictxIncludeCalled.nestingLvel++;
       	ictxIncludeCalled.rowStart = 0;
       	ictxIncludeCalled.includeName = includeNameToAnalyze;

       	
 		// Lettura ahead istruzione include 
        instructionJclMvs = getNextJclInstruction(ictxIncludeCalled);
        		
		// Scan istruzioni jcl da analizzare come oggetto Instruction completo di info sorgente
		while (instructionJclMvs != null) {  
			
			// Creazione entry di programma con impostazioni varie correnti
			jclEntry = new JclMvsEntry();
			setJclEntryInfo(ictxIncludeCalled, instructionJclMvs, jclEntry);

			// Analisi statement jcl
			dispatchInstructionAnalyzer(ictxIncludeCalled, instructionJclMvs, jclEntry);

			// Se parsing error impostazione flag generale di errore
			if (instructionJclMvs.isParsingError() 
			||  instructionJclMvs.isSemanticError() ) {
				ictxIncludeCalled.isAnyInstructionErrorDetected = true;
			}
			
			// Lettura in ciclo oggetto istruzione successiva
			instructionJclMvs = getNextJclInstruction(ictxIncludeCalled);
	        
	    } //end-while
        
  
		// Eventuali include/proc sono state esplose e accodate ricorsivamente all'oggetto include corrente
		// L'oggetto include può quindi serializzato e restituito al chiamante 
		objectIncludeJcl.setInclude(true);
		objectIncludeJcl.setSourceName(includeNameToAnalyze);
		objectIncludeJcl.setLibOwnerCode(ictx.curLibraryCode);
		objectIncludeJcl.setLibOwnerPath(ictx.curLibraryPath);
		objectIncludeJcl.setJclType(EnumObject.OBJECT_JCL_INCLUDE);
		objectIncludeJcl.setJclTypeObject(EnumObjectOption.JCL_MVS);
		
		// Errori in include in analisi o in quelli nested inclusi
		if (ictxIncludeCalled.isAnyInstructionErrorDetected) {
			ictx.isAnyInstructionErrorDetected = true;
			objectIncludeJcl.setParsingWithErrors(true);			// Mark include in errore
			this.jclMvs.setParsingWithErrors(true);			        // Mark jcl sotto analisi in errore
		} else {
			// Serializzazione 
			putSerialized(ucfg.getDirJclObj(), SUFFIX_SERIALIZED_JCL_INCLUDE, includeNameToAnalyze, objectIncludeJcl);
		}
		
		// Update strutture di persistenza
		if (this.di.optUpdateDb) {
			updateIncludeStatusOnDbStructure(objectIncludeJcl);  // Stato analisi  
 		}

		return objectIncludeJcl;
	}

	
	/*
	 * ---------------------------------------
	 * Analisi ricorsiva Proc jcl
	 * ---------------------------------------
	 * 
	 * Questo metodo viene richiamato durante l'analisi di un jcl/proc/include richiamata
	 * ricorsivamente da un programma, oppure direttamente dalla funzione di analisi proc,
	 * tramite il metodo analyzeProc();
	 * Quando si incontra una istruzione di espansione di una proc, si attiva questo metodo.
	 * Se nella proc sono presenti statement include, questi vengono inclusi o analizzati
	 * ricorsivamente.
	 * Se nella proc sono presenti statement exec proc, queste vengono incluse o analizzati
	 * ricorsivamente.
	 * 
	 */
 	private JclMvs analyzeProcRecursive(InnerContextAnalysis ictx				// Contesto sorgente chiamante
    		                          , String procNameToAnalyze  		        // Nome proc da analizzare
    								  , SourceInput si							// Proc da analizzare
    		                          ) throws ExceptionAmrita, SQLException {
    	
    	
 
		InstructionJclMvs instructionJclMvs = null;							    // Istruzione jcl generica		
		JclMvsEntry jclEntry = null;											// Entry generico di jcl
     	InnerContextAnalysis ictxProcToAnalyze = null;						    // Nuovo oggetto contesto proc da analizzare
       	JclMvs objectProcJcl = null;           							    	// Oggetto JclProc output del processo di analisi
        	
        // Il nuovo contesto diventa il sorgente della proc da espandere, a partire dal contesto corrente
		// Clone contesto corrente, inclusa map variabili
       	// Le variabili sono già state aggiornate per il contesto corrente dall'analisi dell'istruzione Proc
		objectProcJcl = new JclMvs(ucfg, procNameToAnalyze);
		objectProcJcl.setJclType(EnumObject.OBJECT_JCL_PROC);
		objectProcJcl.setProcName(procNameToAnalyze);
       	ictxProcToAnalyze = ictx.clone();	
       	ictxProcToAnalyze.jclMvs = objectProcJcl;
       	ictxProcToAnalyze.ar_RowsSource = si.getArrayRowSource(); 
       	ictxProcToAnalyze.activeTypeSource = EnumSourceType.JCL_MVS_PROC;
       	ictxProcToAnalyze.procExecStmt = ictx.procExecStmt;
       	ictxProcToAnalyze.procName = procNameToAnalyze;
       	ictxProcToAnalyze.activeSourceName = procNameToAnalyze;
       	ictxProcToAnalyze.rowStart = 0;
		
      	// Lettura ahead istruzione proc
        instructionJclMvs = getNextJclInstruction(ictxProcToAnalyze);
        		
		// Scan istruzioni della proc da analizzare come oggetto Instruction completo di info sorgente
		while (instructionJclMvs != null) {
			
			// Creazione entry jcl e impostazioni varie correnti
			jclEntry = new JclMvsEntry();
			setJclEntryInfo(ictxProcToAnalyze, instructionJclMvs, jclEntry);
			
			// Analisi statement jcl
			dispatchInstructionAnalyzer(ictxProcToAnalyze, instructionJclMvs, jclEntry);
			
			// Se parsing error impostazione flag generale di errore
			if (instructionJclMvs.isParsingError() 
			||  instructionJclMvs.isSemanticError() ) {
				ictxProcToAnalyze.isAnyInstructionErrorDetected = true;
			}
			
			// Lettura in ciclo oggetto istruzione successiva
			instructionJclMvs = getNextJclInstruction(ictxProcToAnalyze);
	        
	    } //end-while
        
  
		// Sono state analizzate e accodate in ictx.al_ProcEntry tutte le istruzioni della proc,
		// Eventuali include richiamate sono state esplose e accodate ricorsivamente sempre a questa ArrayList
		// Eventuali proc richiamate sono state esplose e accodate ricorsivamente sempre a questa ArrayList
		// L'oggetto proc può quindi essere popolato, serializzato e restituito al chiamante 
		
		objectProcJcl.setProc(true);
		objectProcJcl.setSourceName(procNameToAnalyze);
		objectProcJcl.setLibOwnerCode(ictx.curLibraryCode);
		objectProcJcl.setLibOwnerPath(ictx.curLibraryPath);
		objectProcJcl.setJclType(EnumObject.OBJECT_JCL_PROC);
		objectProcJcl.setJclTypeObject(EnumObjectOption.JCL_MVS);

		// Errori in analisi o in eventuali include nested inclusi: propagazione errore di parsing al chiamante
		if (ictxProcToAnalyze.isAnyInstructionErrorDetected) {
			ictx.isAnyInstructionErrorDetected = true;
			objectProcJcl.setParsingWithErrors(true);			// Mark proc come in errore
			this.jclMvs.setParsingWithErrors(true);			    // Mark jcl sotto analisi in errore
		} else {
			putSerialized(ucfg.getDirJclObj(), SUFFIX_SERIALIZED_JCL_PROC, procNameToAnalyze, objectProcJcl);
		}

		// Update strutture di persistenza
		if (this.di.optUpdateDb) {
			updateIncludeStatusOnDbStructure(objectProcJcl);  // Stato analisi  
 		}
		return objectProcJcl;
	}


 	
 	
	/*
	 * ----------------------------------------------------------------------------------------------------------
	 * Restituisce il successivo oggetto InstructionJclMvs a partire dalle righe sorgente, in modo trasparente. 
	 * ----------------------------------------------------------------------------------------------------------
	 *   
	 * Se non ci sono più istruzioni jcl da restituire, restituisce null.
	 *  
	 *  
	 * @param InnerContextAnalysis 
	 * @return InstructionJclMvs istr jcl
	 * 
	 */
	private InstructionJclMvs getNextJclInstruction(InnerContextAnalysis ictx) {
		
		InstructionJclMvs instrJclMvs = null;		// Oggetto istruzione di output
		String ar_RowsSourceInstr[] = null;         // Righe del source origine componenti l'istruzione, esclusi i commenti precedenti
		ArrayList<String> al_RowsSourceInstr = null;// Righe del source origine componenti l'istruzione, esclusi i commenti precedenti
		String sourceInstr = "";  					// Stringa istruzuine completa istruzione senza // in righe successive
		                                			 
		
		// Estrazione istruzione successiva (memorizzata anche in this.sourceInstr)
		sourceInstr = getNextSourceInstruction(ictx);  // -> this.sourceInstr this.activeTypeInstr
		
		// Fine istruzioni o errore sorgente in input: restituisco oggetto istruzione null
		if (sourceInstr.equals("")) {      
			ictx.objectInstr = null;
			return null;
		}

		// Istruzione non riconosciuta: restituisco oggetto istruzione null
		if (ictx.activeTypeInstr == EnumInstrDataCategory.NOT_ASSIGNED) {
			return null;
		}
		
		// Composizione oggetto istruzione da restituire al chiamante
		ictx.nameInstr = ictx.activeTypeInstr.toString();
		instrJclMvs = new InstructionJclMvs();
		instrJclMvs.setTypeInstrJcl(ictx.activeTypeInstr);
		instrJclMvs.setName(ictx.activeTypeInstr.toString());
		instrJclMvs.setSourceInstr(sourceInstr);
		instrJclMvs.setRowStartSource(ictx.rowStartSource);
		instrJclMvs.setRowEndSource(ictx.rowEndSource);
		al_RowsSourceInstr = new ArrayList<String> ();
		for (int i = ictx.rowStartSource; i <= ictx.rowEndSource; i++) {
			al_RowsSourceInstr.add(ictx.ar_RowsSource[i]);
		}
		ar_RowsSourceInstr = new String[al_RowsSourceInstr.size()];
		ar_RowsSourceInstr = al_RowsSourceInstr.toArray(ar_RowsSourceInstr);
		instrJclMvs.setRowsSource(ar_RowsSourceInstr);
		
		ictx.objectInstr = instrJclMvs;
		
		this.di.excpInfo = ictx;
		
		return instrJclMvs;
	}

  
	
	/*
	 * -------------------------------------------------------------------------------------
	 *  Restituisce e riconosce una stringa con la successiva istruzione Jcl recuperando e 
	 *  concatenando le righe successive dalla // a pos 1.<br>
	 * ---------------------------------------------------------------------------------------
	 * 
	 *  <p>
	 *  Vengono contestualmente aggiornati gli array dei commenti prima di ogni riga. 
	 *  Se la definizione è spalmata su più righe con righe di commento fra una riga
	 *  e l'altra, questi commenti vengono accodati e messsi fra quelli prima della definizione.<br>
	 *  Una riga può contenere anche più di una istruzione e quindi viene anche gestita la posizione di
	 *  inizio e di fine di ogni definizione.<br>
	 *  L'istruzione può essere la definizione di una proc, il richiamo di una Include, una ddname
	 *  o altro.<br>
	 *  <p>
	 *  Vengono valorizzati i campi {@link EnumInstrDataCategory} activeTypeInstr con il codice dell'istruzione
	 *  intercettata e viene restituita la stringa copmpleta dell'istruzione, anche se era spalmata
	 *  su più righe.
	 *  
	 * @param InnerContextAnalysis ictx 
	 * @return String stringa con l'istruzione completa
	 * 
	 */
	private String getNextSourceInstruction(InnerContextAnalysis ictx) {
		
		EnumInstrDataCategory nextInstrType = null;
		String sourceInstrPackaged = "";        	// Stringa con istruzione completa packagizzata
		String rowWrk = "";
		int iStart = 0;
		int iStartInstr = 0;
		int iEndInstr = 0;
		int i = 0;
		boolean sv_isSysinEmbedded = false;
		
		// Fine sorgente
		if (ictx.rowStart >= ictx.ar_RowsSource.length) {
			return "";
		}
		
		// Inizializzazione campi e array list
		initializeForNewInstruction(ictx); 
		
		// (1) Ricerca prima istruzione utile 
		
        for (iStart = ictx.rowStart; iStart < ictx.ar_RowsSource.length; iStart++) {
        	ictx.rowAll = ictx.ar_RowsSource[iStart];
        	// Non è un commento: break e trattamento come istruzione
			if (ictx.rowAll.length() >= 2 && !ictx.rowAll.startsWith("//*")) {
				extractRowFields(ictx, ictx.rowAll);      
				break;
			}
		}
		
		// Fine sorgente
		if (iStart >= ictx.ar_RowsSource.length) {
			return "";
		}
		
		// Inserisco commenti presenti prima dell'istruzione 
		for (i = iStart; i < ictx.rowStartSource; i++) {
			rowWrk = ictx.ar_RowsSource[i];
			ictx.al_CommentsBeforeInstr.add(rowWrk);
		}
		
		iStartInstr = iStart;    // Riga utile inizio istruzione, dopo eventuali commenti

 		// (2) Individuazione ed estrazione completa istruzione
		
        // Individua il tipo di istruzione o NOT_ASSIGNED se non riconosciuta.
		ictx.rowAll = ictx.ar_RowsSource[iStartInstr];
		ictx.activeTypeInstr = getInstructionType(ictx);       
 
		// Tipo istruzione non individuata: errore interno o source errato
		if (ictx.activeTypeInstr == EnumInstrDataCategory.NOT_ASSIGNED) {
	       	logMessage(EnumMessageType.ERROR_INTERNAL, "EI0033", this.jclName, iStartInstr+"", ictx.rowAll);
	       	ictx.isAnyInstructionErrorDetected = true;
	       	ictx.sourceInstr = "";
			return "";
		}

		// (3) Caso particolare di sysin dd *, da restituire subito al chiamante
		if (ictx.activeTypeInstr == EnumInstrDataCategory.JCL_MVS_DD
		&&  ictx.isSysinEmbedded) {
			ictx.rowStartSource = ictx.rowStart;
			ictx.rowEndSource = ictx.rowStart;
			sourceInstrPackaged = packageInstr(ictx).toUpperCase();
			ictx.sourceInstr = sourceInstrPackaged;  
	        ictx.rowStart = iStartInstr + 1;
			return sourceInstrPackaged;
		}

		// (4) Caso particolare di sysin card embedded, da restituire subito al chiamante
		if (ictx.activeTypeInstr == EnumInstrDataCategory.JCL_MVS_SYSIN_STREAM) {
			ictx.rowStartSource = ictx.rowStart;
			ictx.rowEndSource = ictx.rowStart;
			sourceInstrPackaged = packageInstr(ictx).toUpperCase();
			ictx.sourceInstr = sourceInstrPackaged;  
	        ictx.rowStart = iStartInstr + 1;
			return sourceInstrPackaged;
		}
		
 		// (5) Individuazione istruzione successiva per packaging istruzione corrente
		
		sv_isSysinEmbedded = ictx.isSysinEmbedded;
        for (iEndInstr = iStartInstr + 1; iEndInstr < ictx.ar_RowsSource.length; iEndInstr++) {
        	ictx.rowAll = ictx.ar_RowsSource[iEndInstr];
			nextInstrType = getInstructionType(ictx);
        	// E' un commento: skip
			if (nextInstrType == EnumInstrDataCategory.JCL_MVS_COMMENT) {
				continue;       
			}
            // E' una scheda di continuazione, inizia per //
			if (nextInstrType == EnumInstrDataCategory.NOT_ASSIGNED) {
				continue;
			}
			// E' una nuova istruzione: cerco la riga precedente non commento
	        for (iEndInstr = iEndInstr - 1; iEndInstr >= iStartInstr; iEndInstr--) {
	        	ictx.rowAll = ictx.ar_RowsSource[iEndInstr];
	        	// E' un commento: skip
	        	if (ictx.rowAll.startsWith("//*")) {
					continue;
				}
	        	break;
	        }
			break;
		}
        ictx.isSysinEmbedded = sv_isSysinEmbedded;
        
		// Fine sorgente
		if (iEndInstr >= ictx.ar_RowsSource.length) {
			iEndInstr = ictx.ar_RowsSource.length - 1;
		} 
		
		
		// (6) Packaging istruzione e Adjust per successiva istruzione 

		// Package istruzione eventualmente spalmata su più righe in unica stringa
        // La stringa con l'istruzione viene restituita trimmata.
		ictx.rowStartSource = iStartInstr;
		ictx.rowEndSource = iEndInstr;
        sourceInstrPackaged = packageInstr(ictx).toUpperCase();
        ictx.sourceInstr = sourceInstrPackaged;  
 
        // Per successiva istruzione
        ictx.rowStart = ictx.rowEndSource + 1;
        
        // Imposto per reperimento successiva istruzione
		return sourceInstrPackaged;
	}
    
	


	/*
	 * ------------------------------------------------ 
	 * Operazioni iniziali sul jcl da analizzare
	 * ------------------------------------------------
	 * 
	 * 
	 */
	private void initialOperations(SourceInput si, String jclName) throws Exception {
		
		EntityObject entityObject = null;
        
		// Initial ora di inizio e reset errori
		this.di.curTimeMsStart = System.currentTimeMillis();
		this.di.curObjectWithErrors = false;
		
		// Descrittore sorgente e nome sorgente
		this.si = si;				
		this.jclName = jclName;
		this.sourceNameUnderParsing = jclName;
		this.di.curObjectId = jclName;
		this.di.curObjectType = this.jclType;
		
		// Si sta analizzando un jcl job, no include e no proc

		// Impostazione informazioni di contesto iniziali
		setInitialContextAnalysis();                     // metodo pubblico di inizializzazione this.ictx	
		this.ictx.jclMvs = this.jclMvs;
		this.ictx.isAnalysisOfJcl = true;
		
		// Impostazioni per oggetti di aggregazione da inserire a fine elaborazione
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem("");    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS);
		entityObject.setIdObject("");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem("");   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(EnumObject.OBJECT_SYS_SUBSYS);
		entityObject.setIdObject(" ");
		this.analyzerDbInfo.addObjEntity(entityObject);
		
		
		
        // Impostazioni per oggetto programma da inserire/aggiornare a fine elaborazione
	
		entityObject = new EntityObject();
		entityObject.setSystem(di.systemInput);   						 
		entityObject.setSubSystem(di.subSystemInput);    
		entityObject.setTypeObject(this.jclType);
		entityObject.setIdObject(this.sourceNameUnderParsing);
        // Impostazioni come se il programma fosse alla sua prima analisi
		entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
		entityObject.setFileSource(di.filePathCurObj);   	   			          // Nome file
		entityObject.setLibrarySourceObject(di.libraryCodeCurObj);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
		entityObject.setLibrarySource(di.libraryPathCurObj);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmLastAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");

		// E' il primo oggetto che viene inserito/aggiornato
		this.analyzerDbInfo.addObjEntity(entityObject);
 		
		// Messaggio di inizio analisi
		logMessage(EnumMessageType.INFORMATION, "MI0129", this.sourceNameUnderParsing);
		
 	}



	/*
	 * ------------------------------------------------
	 * Operazioni finali di analisi del jcl job.
	 * ------------------------------------------------
	 * 
	 * Analisi istruzioni e gestione sostituzione valore variabili
	 * Caricamento oggetti e relazioni in struttura di aggiornamento db
	 * Serializzazione jcl
	 * Aggiornamenti su db
	 */
	private void finalOperations() throws Exception {
        
		// Stato analisi
		this.jclMvs.setParsingWithErrors(this.ictx.isAnyInstructionErrorDetected);

		// Solo se non ci sono errori
  		if (!this.ictx.isAnyInstructionErrorDetected) {
  	 		solveVariablesAnalyze(this.ictx);			// Sostituzione variabili e analisi istruzioni attualizzate
  	 		solveStepOverrides();						// Gestione overrides a livello di proc e step
  	 		solveDsnameBackwardForward();				// Gestione riferimenti backward DSNAME=*. e forward DSNAME=ddname
  	 		setNumInstr();                              // Impostazione numero istruzione progressivo
   			generateDbUpdates(); 						// Generazione updates db da effettuare 
		}
 		   
	    // Errori di parsing o di sostituzione variabili o di override: logging istruzioni in errore
  		if (this.ictx.isAnyInstructionErrorDetected) {
			loggingParsingErrors();						 
			this.di.isExecutionWithErrors = true;
		}
		

        // Persistenza su file system, serializzazione oggetto
  		if (!this.ictx.isAnyInstructionErrorDetected) {
	  		putSerialized(ucfg.getDirCobolObjPgm(), SUFFIX_SERIALIZED_JCL_JOB, this.jclName, this.jclMvs);
  		}
  		
 		// Aggiornamento stato analisi jcl 
  		updateJclJobStatusOnDbStructure(this.jclMvs);
            
        // Aggiornamenti complessivi effettivi base dati
        if (this.di.optUpdateDb) {
        	analyzerDbInfo.sqlDeleteGenericLevel();
        	analyzerDbInfo.sqlDeleteJclJobLevel(this.jclName, false, false);
            analyzerDbInfo.update(true, false, this.jclName, null);
		}
        
		// Calcolo tempo di elapsed complessivo per il programma
		this.di.curTimeMsEnd = System.currentTimeMillis();
		this.di.curTimeMsElapsed = this.di.curTimeMsEnd - this.di.curTimeMsStart;
		Long timeElapsed = new Long(this.di.curTimeMsElapsed);
		
		// Messaggio di fine elaborazione
        if (this.ictx.isAnyInstructionErrorDetected) {
        	logMessage(EnumMessageType.INFORMATION, "MI0124", this.sourceNameUnderParsing, timeElapsed.toString());    
		} else {
			logMessage(EnumMessageType.INFORMATION, "MI0125", this.sourceNameUnderParsing, timeElapsed.toString()); 
		}
		 
 	}
  
	
	/*
	 * ---------------------------------------------------------
	 * Gestione sostituzione variabili
	 * ---------------------------------------------------------
	 * 
	 * Vengono verificate tutte le istruzioni jcl, sia quelle presenti
	 * nel source originario sia quelle inserite ricorsivamente a
	 * a fronte di include e/o proc nested.
	 * Viene gestita l'attualizzazione delle variabili dichiarate
	 * in SET e EXEC PROC per il corretto livello di nesting.
	 * 
	 * Viene aggiornato il valore corrente delle variabili nella map
	 * di variabili di ogni istruzione.
	 * 
	 * Viene aggiornato il sorgente dell'istruzione con i valori aggiornati.
	 * 
	 */
    private void solveVariablesAnalyze(InnerContextAnalysis ictx) throws ExceptionAmrita, SQLException {
       	
   		// Stack con le variabili aggiornate dalla exec di lancio della proc.
 		// In questo stack NON sono incluse le variabili impostate con SET.
 		// Ogni livello di stack include anche le variabili del livello di stack meno annidato
		Stack<Map<String, String>> stack_mapVar = null;
 
    	// Istruzioni di definizione e di esecuzione proc.
    	// Gli statement di definizione sono inclusi nel jcl 
    	// per impostare le eventuali variabili predefinite.
    	InstructionJclMvs instructionDefProc = null;
    	InstructionJclMvs instructionExecProc = null;
        
    	// Istruzioni, entries del jcl espanso
		ArrayList<JclMvsEntry> al_jclEntry = null;
    	JclMvsEntry jclEntry = null;
    	InstructionJclMvs instruction = null;
    	
    	// Varie
    	String underProcCur = "";
    	
    	al_jclEntry = this.jclMvs.getJclEntries();
 		stack_mapVar = new Stack<Map<String, String>> ();
  		stack_mapVar.push(ictx.map_var);                            // Push Map prima delle sostituzioni
    	ictx.isInstructionToAppend = false;                         // L'analisi di istruzione non ne provoca l'accodamento
  		
    	// Scan istruzioni
    	for (int i = 0; i < al_jclEntry.size(); i++) {
    		
    		jclEntry = al_jclEntry.get(i);
       		instruction = jclEntry.getInstruction();
    		
       		// Non interessano le Proc embedded
       		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
       		
    		// Istruzione SET
    		// Update map parametro/valore in map globale variabili impostate da SET
    		if (instruction.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_SET) {
        		updateMapCurrentParmAndValuesSet(instruction);
        		continue;
			}
    		
 
			// Statement di exec proc=nome: save istruzione di exec
			if (jclEntry.getTypeInstr() == EnumInstrDataCategory.JCL_MVS_STEP 
			&& !jclEntry.getInstruction().jclStepIsExecPgm()) {	
				instructionExecProc = instruction;  
				continue;
			}

			// Istruzione di definizione proc: stack map corrente e update valori variabili
			if (instruction.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_PROC) {
				underProcCur = instruction.jclProcGetName();                // Nuova procedura corrente
				instructionDefProc = instruction;							// Statement PROC VAR=Value,...
				stack_mapVar.push(ictx.map_var);                            // Push Map prima delle sostituzioni
				updateMapCurrentParmAndValues(ictx, instructionDefProc);    // Update map variabili da proc var=...
				updateMapCurrentParmAndValues(ictx, instructionExecProc);   // Ri-Update map variabili da exec proc=...
				continue;
			}

    		// Interessano solo istruzioni con variabili
    		if (!jclEntry.isThereAnyVarToReplace()) {
				continue;
			}
    		

			// Fine procedura attiva, potrebbe essere una procedura nested
			if (!jclEntry.getUnderProcName().equals(underProcCur)) {
				underProcCur = jclEntry.getUnderProcName();
				ictx.map_var = stack_mapVar.pop();
			}
			
			
			// Sostituzione variabili correnti con eventuale errore già nell'istruzione
			// Ordine di validità variabili:
			//  1) Variabili in statement di esecuzione proc
			//  2) Variabili in definizione proc aggiornate al livelo di nesting
			//  3) Variabili definite da SET (globali)
			manageReplaceVarOnSourceInstruction(ictx, instruction, jclEntry);

			// C'erano variabili e non tutte sono risolte: errore
			if (jclEntry.isThereAnyVarToReplace()
			&& !jclEntry.isAllVarReplaced()) {
				ictx.isAnyInstructionErrorDetected = true;
			}  
			   
			// C'erano variabili e sono tutte risolte: istruzione da analizzare
			if (jclEntry.isThereAnyVarToReplace()
			&&  jclEntry.isAllVarReplaced()) {
				dispatchInstructionAnalyzer(this.ictx, instruction, jclEntry);
			}
			
			// Errori a fronte dell'analisi con le variabili attualizzate
			if (instruction.isParsingError() 
			|| instruction.isSemanticError()) {
				ictx.isAnyInstructionErrorDetected = true;
			}
			
    		// Anche in caso di errore si analizzano le altre istruzioni
    		
		}
    }
    	
    
	
	/*
	 * ---------------------------------------------------------
	 * Gestione overrides a livello di step
	 * ---------------------------------------------------------
	 * 
	 * Questo metodo viene richiamato a fine analisi se non sono stati 
	 * riscontrati errori, sintattici, semantici e di sostituzione
	 * variabili.

	 * Vengono analizzate tutte le istruzioni jcl e viene aggiornato
	 * il jcl per  aggiornare le dd overridate nello step, se queste
	 * sono presenti, oppure accodare la nuova definizione in fondo
	 * allo step.
	 * Viene prodotto un nuovo insieme di statement jcl pronto per le
	 * successive elaborazioni.
	 * 
	 * Vengono recuperate tutte le istruzioni e gestiti:
	 * 
	 * 1) Override delle ddname in update di ddname di uno step
	 * 2) Override delle ddname in accodamento di uno step
	 * 3) I sysin embedded delimitati da /* o da custom delimiter
	 *    con valorizzazione istruzione DD con i valori trovati
	 * 
	 */
    private void solveStepOverrides() {
    	
    	ExceptionAmrita excp = null;
    	ArrayList<JclMvsEntry> al_jclMvs = null;
       	InstructionJclMvs jclInstr = null;
       	InstructionJclMvs jclInstrToOverride = null;
       	InstructionJclMvs jclInstrToAppend = null;
    	ArrayList<String> al_parmName = null;
       	JclMvsEntry jclEntry = null;
       	JclMvsEntry jclEntryToAppend = null;
       	JclMvsEntry jclEntryToOverride = null;
    	String procStepName = "";
     	String ddName = "";
     	String parmValue = "";
    	String tokenError = "";
    	String sourceInstr = "";
       	int i;
       	int iDDToOverride = 0;
     	
       	// Errori precedenti
       	if (this.ictx.isAnyInstructionErrorDetected) {
       		return;
       	}
       	
       	
    	al_jclMvs = this.jclMvs.getJclEntries();				// Jcl completo analizzato
    	
    	// Scan entries con istruzioni jcl originali e/o incluse e/o espanse
    	for (i = 0; i < al_jclMvs.size(); i++) {
    		
    		jclEntry = al_jclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();

    		// Non interessano le istruzione delle proc embedded  
       		if (jclEntry.isUnderProcEmbedded()) {
 				continue;
			}

    		// Interessano solo le schede DD 
    		if (jclInstr.getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
    		
    		// Interessano solo le DD di override
    		if (!jclInstr.jclDDIsOverride()) {
				continue;
			}
    		
       		// Override di DD nella proc dichiarata immediatamente prima
       		
            //procStepName.ddName DD
        	procStepName = jclInstr.jclDDGetOverrideProcStepName();	
            ddName =  jclInstr.jclDDGetOverrideDDName();
            
             
    		// Ricerca DD precedente da overridare o ultima DD a cui accodare
            iDDToOverride = getIndexDDToOverride(al_jclMvs, i - 1, procStepName, ddName);
            
            // -1 indica procStep non trovato prima dello statement di override
            if (iDDToOverride < 0) {
    			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_JCL_DD_OVERRIDE);
    			tokenError = procStepName +"." + ddName;
    			jclInstr.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0031", tokenError, excp, this.jclName);
    			jclInstr.setSemanticError(true);
    			this.ictx.isAnyInstructionErrorDetected = true;
    			continue;
			}
              
            // Si può procedere a override/append
            
      	    jclEntryToOverride = al_jclMvs.get(iDDToOverride);    // Da overridare o ultima a cui accodare
      	    jclInstrToOverride = jclEntryToOverride.getInstruction();
      	             
            // DD da overridare: rimpiazzo/inserisco i valori dei parametri
            if (!this.isDDToAppend) {
				al_parmName = jclInstr.jclStmtGetParmNames();
            	for (String parmName : al_parmName) {
            		parmValue = jclInstr.jclStmtGetParm(parmName);
            		jclInstrToOverride.jclStmtPutParm(parmName, parmValue);
				}
            	// Marcatura istruzione come overridata con o meno sysin embedded
            	jclEntryToOverride.setOverridedUpdate(true);
            	jclInstrToOverride.jclDDSetSysinCards(jclInstr.jclDDGetSysinCards());
            	jclInstrToOverride.jclDDSetInputEmbedded(jclInstr.jclDDIsInputEmbedded());
              	continue;
			}
            
            // DD da accodare alla proc/step indicati.
            // E' disponibile l'indice dell'ultima DD nella proc/step
            jclEntryToAppend = (JclMvsEntry) jclEntry.clone();
            jclEntryToAppend.setOverridedAppend(true);
            jclEntryToAppend.setInstruction((InstructionJclMvs) jclInstr.clone());
            jclInstrToAppend = jclEntryToAppend.getInstruction();
            jclInstrToAppend.jclStmtPutName(ddName);
            jclInstrToAppend.jclDDPutName(ddName);
            sourceInstr = jclInstrToAppend.getSourceInstr();
            sourceInstr = sourceInstr.replace(procStepName + "." + ddName, ddName);
            jclInstrToAppend.setSourceInstr(sourceInstr);
            jclInstrToAppend.jclDDSetSysinCards(jclInstr.jclDDGetSysinCards());
            jclInstrToAppend.jclDDSetInputEmbedded(jclInstr.jclDDIsInputEmbedded());
            al_jclMvs.add(iDDToOverride + 1, jclEntryToAppend);
            
            // Lo statement di override resta presente ma marcato
            i++;
     	}   	
	}

    /*
     * ----------------------------------------------------
     * Ricerca nel jcl  analizzato la DD fornita in input
     * ----------------------------------------------------
     * 
     * Viene restituito l'indice della DD trovata.
     * Qualificano la ricerca l'eventuale procStepName.
     * La ricerca inizia dallo statement il cui indice è fornito 
     * in input e continua all'indietro, in reverse order.
     * 
     * Se la ddname non viene trovata restituisce l'indice dell'ultima  DD
     * definita per lo step e imposta il flag variabile di istanza in append.
     * 
     * Se la ddname viene trovata restituisce il suo indice
     * e imposta il flag variabile di istanza in override.
     * 
     * Return -1 indica proceStep non trovato prima dello statement di override
     * Return 0  indica proceStep trovato e ddname non trovata prima dello statement di override
     * 
     */
	private int getIndexDDToOverride(ArrayList<JclMvsEntry> al_JclMvs
			                       , int iStart
								   , String procStepName
								   , String ddName) {

		JclMvsEntry jclEntry = null;
	   	InstructionJclMvs jclInstr = null;
	   	int iStepExec = 0; 								    // Index //ProcStep EXEC PGM=...
	   	int iDDReturn = 0; 								    // Index DD da overridare o index DD a cui accodare
	   	int i = 0;
	   	
		this.isDDToAppend = true;
		
    
		// Scan reverse fino a trovare la //stepName EXEC .. con il procStepName fornito
    	for (i = iStart; i > 0; i--) {
    		
    		jclEntry = al_JclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();
    		
    		// Non interessano gli statement dentro le proc embedded
    		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
    		
            if (jclInstr.getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_STEP 
			&&  jclInstr.jclStmtGetName().equals(procStepName)) {
            	iStepExec = i;
            	break;
			}
    	}
    	
    	// ProcStep non trovato:
    	if (i == 0) {
			return -1;
		}
    	
		
		// Scan statements in step
    	for (i = iStepExec + 1; i < al_JclMvs.size(); i++) {
    		
    		jclEntry = al_JclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();
    		
    		// Non interessano gli statement dentro le proc embedded
    		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
    		
			// Interessano solo le DD non chained e non statement di override
        	if (jclEntry.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
        		continue;
        	}
        	if (jclInstr.jclDDIsChained()) {
        		continue;
        	}
        	if (jclEntry.isOverride()) {
				continue;
			}
        	
        	
        	// Fine istruzioni step e dd non trovata: si va in append dopo iDDReturn
        	if (!procStepName.equals(jclEntry.getUnderPgmStepName())) {
				break;
			}
 
        	// Imposto all'ultima DD dello step, a cui fare append
        	iDDReturn = i;
         	

        	// La DD appartiene allo step della proc indicato: override
        	
         	// Interessa solo la ddname con il nome fornito in input
        	if (!jclInstr.jclDDGetName().equals(ddName)) {
        		continue;
			}
        	
        	// ddname individuata nello step di proc richiesto
        	this.isDDToAppend = false;
        	iDDReturn = i;
        	break;
     	}

    	// iDDReturn = 0 indica errore di override, ovvero ddname non trovata nello step richiesto
    	
    	return iDDReturn;
	}

	/*
	 * ---------------------------------------------------------
	 * Sostituzione riferimenti a dsname backward 
	 * ---------------------------------------------------------
	 * 
	 * Questo metodo viene richiamato a fine analisi se non sono stati 
	 * riscontrati errori, sintattici, semantici, di sostituzione
	 * variabili o di override.
	 * 
	 * Vengono analizzate le istruzioni DD
	 * 
	 * 1) Se DSNAME=*.ddName
	 *    E' un riferimento backward a una dd definita precedentemente
	 *    Vengono copiati nella DD corrente i parametri backward.
	 * 2) Se DSNAME=*.stepName.ddName
	 *    E' un riferimento backward a una dd definita precedentemente
	 *    nello step di esecuzione stepName.
	 *    Vengono copiati nella DD corrente i parametri backward.
	 * 3) Se DSNAME=*.procStepName.stepName.ddName
	 *    E' un riferimento backward a una dd definita precedentemente
	 *    nel procStepName e nello step di esecuzione stepName.
	 *    Vengono copiati nella DD corrente i parametri backward.
	 * 4) Se DDNAME=ddname
	 *    E' un riferimento forward a una ddname definita successivamente 
	 *    a quella corrente.
	 *    Vengono copiati nella DD corrente i parametri backward.
	 */
	private void solveDsnameBackwardForward() {

		ExceptionAmrita excp = null;
		ArrayList<String> al_parmNameForward = null;
		ArrayList<JclMvsEntry> al_jclMvs = null;
		JclMvsEntry jclEntry = null;
		JclMvsEntry jclEntryBackward = null;
		JclMvsEntry jclEntryForward = null;
	   	InstructionJclMvs jclInstr = null;
	   	InstructionJclMvs jclInstrBackward = null;
	   	InstructionJclMvs jclInstrForward = null;
	   	String dsname = "";
	   	String dsnameBackward = "";
	   	String ddnameForward = "";
	   	String parmValueForward = "";
	   	int iDDBackward = 0;
	   	int iDDForward = 0;
	   	
       	// Errori precedenti
       	if (this.ictx.isAnyInstructionErrorDetected) {
       		return;
       	}

  		al_jclMvs = this.jclMvs.getJclEntries();			// Jcl completo analizzato
		
		// Scan statements in jcl
    	for (int i = 0; i < al_jclMvs.size(); i++) {
    		
    		jclEntry = al_jclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();
    		
    		// Non interessano gli statement dentro le proc embedded
    		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
    		
			// Interessano solo le DD
        	if (jclEntry.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
        	
         	// Gestione dsname con riferimento backward
        	if (jclEntry.isReferenceBackward()) {
        		
            	dsname = jclInstr.jclStmtGetParm("DSN");
            	 
        		// Recupero dsname backward
        		iDDBackward = getBackwardDD(al_jclMvs, i - 1, dsname);
        		
        		// DD backward non individuata
        		if (iDDBackward == -1) {
        			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_JCL_DSNAME_BACKWARD_REFERENCE);
         			jclInstr.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0033", dsname, excp, this.jclName);
        			jclInstr.setSemanticError(true);
        			this.ictx.isAnyInstructionErrorDetected = true;
        			continue;
				}
        		
        		// Dsname individuato: recupero entry e istruzione con i parametri
           		jclEntryBackward = al_jclMvs.get(iDDBackward);
        		jclInstrBackward = jclEntryBackward.getInstruction();
        		dsnameBackward = jclInstrBackward.jclStmtGetParm("DSN");
        		
        		// Update il dsname con il dsname backward
        		jclInstr.jclStmtPutParm("DSN", dsnameBackward);
        		continue;
			}
  
         	// Gestione dsname con riferimento forward
        	if (jclEntry.isReferenceForward()) {
 
        		ddnameForward = jclInstr.jclStmtGetParm("DDNAME");
            	if (ddnameForward == null) {
    				continue;
    			}
            	
            	// Recupero ddName definita successivamente
            	iDDForward = getForwardDD(al_jclMvs, i + 1, ddnameForward);
            	
           		// DD forward non individuata
        		if (iDDForward >= al_jclMvs.size()) {
        			excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_JCL_DSNAME_FORWARD_REFERENCE);
         			jclInstr.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0034", ddnameForward, excp, this.jclName);
        			jclInstr.setSemanticError(true);
        			this.ictx.isAnyInstructionErrorDetected = true;
    			}

            	
           		jclEntryForward = al_jclMvs.get(iDDForward);
        		jclInstrForward = jclEntryForward.getInstruction();

            	// Scan parametri ddname forward
             	al_parmNameForward = jclInstrForward.jclStmtGetParmNames();
            	for (String parmNameForward : al_parmNameForward) {
            		parmValueForward = jclInstrForward.jclStmtGetParm(parmNameForward);
            		// Replace/put solo se non era dichiarato
            		if (jclInstr.jclStmtGetParm(parmNameForward) == null) {
            			jclInstr.jclStmtPutParm(parmNameForward, parmValueForward);
    				}
    			}
         	}
      	}
	}
	
	/* --------------------------------------------------------------------
	 * Impostazione numero istruzione in ogni istruzione del jcl
	 * -------------------------------------------------------------------
	 * 
	 * L'attribuzione è sequenziale senza distinzione se istruzioni
	 * appartenenti al proc embedded.
	 * 
	 */
	private void setNumInstr() {
		
		JclMvsEntry jclEntry = null;
	   	InstructionJclMvs jclInstr = null;
	   	int i = 0;
	   	
		// Scan statements in step
		for (i = 0; i < this.jclMvs.getJclEntries().size(); i++) {
			
			jclEntry = this.jclMvs.getJclEntries().get(i);
			jclInstr = jclEntry.getInstruction();
			jclInstr.setNumInstr(i);
			
	 	}
	}

	

	
	
	/*--------------------------------------------------------------------
	 * Restituisce il pointer della dd precedente referenziata dal dsname
	 * -------------------------------------------------------------------
	 * 
	 * Viene estratto dal dsname il procStepName, lo stepName e la ddName
	 * 
	 * A partire dall'istruzione di inizio specificata, si cerca la ddName
	 * richiesta.
	 * 
	 * Se non trovata si restituisce 0
	 */
	private int getBackwardDD(ArrayList<JclMvsEntry> al_jclMvs
							, int iStart
							, String dsname) {
		
		JclMvsEntry jclEntry = null;
	   	InstructionJclMvs jclInstr = null;
	   	String procStepName = "";
	   	String stepName = "";
	   	String ddName = "";
	   	int iDDBackward = -1;
	   	int iPoint1 = -1;						// DSN=*.ddName
	   	int iPoint2 = -1;						// DSN=*.stepName.ddName
	   	int iPoint3 = -1;						// DSN=*.procStepName.stepName.ddName
        int i = 0;
        
        
	   	// Individuazione procStepName, stepName e ddName da trovare
	   	
   		iPoint1 = 1; 
		iPoint3 = -1;
		iPoint2 = dsname.indexOf(".", 2);
		if (iPoint2 > 0) {
			iPoint3 = dsname.indexOf(".", iPoint2 + 1);
			}
		// DSN=*.ddName
		if (iPoint2 == -1 && iPoint3 == -1) {
			procStepName = "";
			stepName = "";
			ddName = dsname.substring(iPoint1 + 1);
		
		// DSN=*.stepName.ddName
		} else if (iPoint2 > 0 && iPoint3 == -1) {
   			procStepName = "";
			stepName = dsname.substring(iPoint1 + 1, iPoint2);
			ddName = dsname.substring(iPoint2 + 1);
		
		// DSN=*.procStepName.stepName.ddName
		} else {
   			procStepName = dsname.substring(iPoint1 + 1, iPoint2);
			stepName = dsname.substring(iPoint2 + 1, iPoint3);
			ddName = dsname.substring(iPoint3 + 1);
		}
		
		// Scan backward
		for (i = iStart; i > 0; i--) {

    		jclEntry = al_jclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();

    		// Non interessano gli statement dentro le proc embedded
    		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
    		
			// Interessano solo le DD
        	if (jclEntry.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}

			// Non interessano le DD di override
        	if (jclEntry.isOverride()) {
				continue;
			}
         	
        	// Non è il procStepName indicato in dsname
        	if (!procStepName.equals("") ) {
               	if (!jclEntry.getUnderProcStepName().equals(procStepName)) {
            		continue;
            	}
			}
         	
        	// Non è il stepName indicato in dsname
        	if (!stepName.equals("") ) {
            	if (!jclEntry.getUnderPgmStepName().equals(stepName)) {
            		continue;
            	}
			}
        	
        	// Non è la ddName indicato in dsname
        	if (!jclInstr.jclDDGetName().equals(ddName)) {
				continue;
			}
        	
        	// E' la ddname cercata
        	iDDBackward = i;
        	break;
		}
	   	return iDDBackward;
	}

	
	/*-----------------------------------------------------------------------
	 * Restituisce il pointer della dd successiva referenziata da DDNAME=..
	 * ----------------------------------------------------------------------
	 * 
	 */
	private int getForwardDD(ArrayList<JclMvsEntry> al_jclMvs
						   , int iStart
						   , String ddnameForward) {

		JclMvsEntry jclEntry = null;
	   	InstructionJclMvs jclInstr = null;
	   	int iDDForward = -1;
        int i = 0;
        
        
	   	// Individuazione procStepName, stepName e ddName da trovare
	   	
 		
		// Scan forward
		for (i = iStart; i < al_jclMvs.size(); i++) {

    		jclEntry = al_jclMvs.get(i);
    		jclInstr = jclEntry.getInstruction();
    		
    		// Non interessano gli statement dentro le proc embedded
    		if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
    		
			// Interessano solo le DD
        	if (jclEntry.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
        	
        	// Le dd non devono essere concatenate
		   	if (jclInstr.jclDDIsChained()) {
		   		continue;
	   		}

        	
        	// Non è la ddName cercata 
        	if (!jclInstr.jclDDGetName().equals(ddnameForward)) {
				continue;
			}
        	
        	// E' la ddname cercata
        	iDDForward = i;
        	break;
		}

		return iDDForward;
	}



	/*
	 * --------------------------------------------------------
	 * Generazione per updates data base
	 * --------------------------------------------------------
	 * 
     * Vengono recuperate le istruzioni jcl analizzate e quindi vengono generate 
     * tutte le informazioni di updates nell'oggetto AnalyzerDbInfo()
     * La sostituzione delle variabili è già stata effettuata.
     */
	@SuppressWarnings("unused")
	private void generateDbUpdates() {
		
		EntityObject eo = null;
		EntityObjectOption eoo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		
        InstructionJclMvs jclInstr = null;
		String jobName = "";
		String pgmExecuted = "";
		String procExecuted = "";
		String includeName = "";
		String includeNameOwner = "";
		String procName = "";
		String procNameOwner = "";
		String ddName = "";
		String dsname = "";
		String stepName = "";
		int numInstrJcl = 0;
		
		eoo = this.analyzerDbInfo.prepareForDbObjectOption(this.jclName, EnumObject.OBJECT_JCL_JOB, EnumObjectOption.JCL_MVS);
		if (this.jclMvs.isThereProcEmbedded()) {
			eoo = this.analyzerDbInfo.prepareForDbObjectOption(this.jclName, EnumObject.OBJECT_JCL_JOB, EnumObjectOption.JCL_MVS_WITH_PROC_EMBEDDED);
		}
		if (this.jclMvs.isThereAnyInclude()) {
			eoo = this.analyzerDbInfo.prepareForDbObjectOption(this.jclName, EnumObject.OBJECT_JCL_JOB, EnumObjectOption.JCL_MVS_WITH_INCLUDE);
		}
				
		
		// Scan istruzioni jcl definite nel sorgente
		for (JclMvsEntry jclEntry : this.jclMvs.getJclEntries()) {
			
			jclInstr = jclEntry.getInstruction();
			numInstrJcl = jclInstr.getNumInstr();
			
			// Non interessano gli statement di proc embedded
			if (jclEntry.isUnderProcEmbedded()) {
				continue;
			}
			
			
			switch (jclEntry.getTypeInstr()) {
			
			   	case JCL_MVS_JOB:
			   		
				   	jobName = jclInstr.jclStmtGetName();
				   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_JCL_JOBNAME, jobName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
				   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_JOB_JCL_JOBNAME, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_JOBNAME, jobName, this.userExitInfoPgm);
				   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_JOB_JCL_JOBNAME, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_JOBNAME, jobName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
				   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					ero.setInstrCategory(jclEntry.getEntryType());
				   	break;
				   	
			   	case JCL_MVS_INCLUDE:
			   		
			   		includeName = jclInstr.jclStmtGetParm("MEMBER");
				   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_JCL_INCLUDE, includeName, EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS, this.userExitInfoPgm);
				   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_JOB_JCL_INCLUDE, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_INCLUDE, includeName, this.userExitInfoPgm);
				   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_JOB_JCL_INCLUDE, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_JOBNAME, jobName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
				   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
				   	ero.setInstrCategory(jclEntry.getEntryType());
			   		
				   	// Include ricorsiva
				   	if (jclEntry.isUnderInclude()) {
			   			includeNameOwner = jclEntry.getUnderIncludeName();
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_INCLUDE_JCL_INCLUDE, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, EnumObject.OBJECT_JCL_INCLUDE, procName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_INCLUDE_JCL_INCLUDE, EnumObject.OBJECT_JCL_INCLUDE, includeName, EnumObject.OBJECT_JCL_INCLUDE, procName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	ero.setInstrCategory(jclEntry.getEntryType());
					}
			   	    break;
			   	    
			   	case JCL_MVS_PROC:
			   		
			   		procName = jclInstr.jclProcGetName();
				   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_JCL_PROC, includeName, EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS, this.userExitInfoPgm);
				   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_JOB_JCL_PROC, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_PROC, procName, this.userExitInfoPgm);
				   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_JOB_JCL_PROC, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_PROC, procName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
				   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
				   	ero.setInstrCategory(jclEntry.getEntryType());
			   		
				   	// Relazione con job name
				   	if (!jobName.equals("")) {
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_PROC_JCL_JOBNAME, EnumObject.OBJECT_JCL_PROC, procName, EnumObject.OBJECT_JCL_JOBNAME, jobName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_PROC_JCL_JOBNAME, EnumObject.OBJECT_JCL_PROC, procName, EnumObject.OBJECT_JCL_JOBNAME, jobName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	ero.setInstrCategory(jclEntry.getEntryType());
					}
				   	
				   	// Relazione con include
			   		if (jclEntry.isUnderInclude()) {
			   			includeNameOwner = jclEntry.getUnderIncludeName();
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_INCLUDE_JCL_PROC, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, EnumObject.OBJECT_JCL_PROC, procName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_INCLUDE_JCL_PROC, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, EnumObject.OBJECT_JCL_PROC, procName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	ero.setInstrCategory(jclEntry.getEntryType());
					}
			   		break;
			   		
			   	case JCL_MVS_STEP:
			   		
			   		stepName = jclInstr.jclStmtGetName();
			   		
			   		// Exec di un programma
			   		if (jclInstr.jclStepIsExecPgm()) {
			   		    
			   			// Relazione con jcl source
			   			pgmExecuted = jclInstr.jclStepGetExecName();
					   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObjectStatus.OBJECT_TO_BE_ANALYZED, this.userExitInfoPgm);
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PGM_JCL_JOB, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_JOB, this.jclName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_JCL_JOB, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_JOB, this.jclName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	ero.setInstrCategory(jclEntry.getEntryType());
					    ero.setLibrarySourceObject("");
					    ero.setLibrarySourcePath("");
					    ero.setFileSource(this.jclName);
						
					   	// Relazione con include sotto cui è definito lo statement
				   		if (jclEntry.isUnderInclude()) {
				   			includeNameOwner = jclEntry.getUnderIncludeName();
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PGM_JCL_INCLUDE, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_JCL_INCLUDE, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
						   	ero.setInstrCategory(jclEntry.getEntryType());
						    ero.setLibrarySourceObject("");
						    ero.setLibrarySourcePath("");
						    ero.setFileSource(this.jclName);
				   		}
					   	
				   		// Relazione con Proc sotto cui è definito lo statement
				   		if (jclEntry.isUnderProc()) {
				   			procNameOwner = jclEntry.getUnderProcName();
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PGM_JCL_PROC, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_PROC, procNameOwner, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PGM_JCL_PROC, EnumObject.OBJECT_PGM_GENERIC, pgmExecuted, EnumObject.OBJECT_JCL_PROC, procNameOwner, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
						   	ero.setInstrCategory(jclEntry.getEntryType());
						    ero.setLibrarySourceObject("");
						    ero.setLibrarySourcePath("");
						    ero.setFileSource(this.jclName);
				   		}
			   		}
			   		
			   		// Exec di una proc
			   		if (!jclInstr.jclStepIsExecPgm()) {
			   		    // Relazione jcl source / proc
			   			procExecuted = jclInstr.jclStepGetExecName();
					   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_JCL_PROC, procExecuted, EnumObjectStatus.OBJECT_TO_BE_ANALYZED, this.userExitInfoPgm);
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_JOB_JCL_PROC, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_PROC, procExecuted, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_JOB_JCL_PROC, EnumObject.OBJECT_JCL_JOB, this.jclName, EnumObject.OBJECT_JCL_PROC, procExecuted, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED,  this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	ero.setInstrCategory(jclEntry.getEntryType());
					    ero.setLibrarySourceObject("");
					    ero.setLibrarySourcePath("");
					    ero.setFileSource(this.jclName);
			   		}
                    break;
			   		
			   	case JCL_MVS_DD:
			   		
			   		ddName = jclInstr.jclStmtGetName();
			   		dsname = jclInstr.jclStmtGetParm("DSNAME");
			   		
			   		// DDName non concatenata: presente l'external name
			   		if (!jclInstr.jclDDIsChained()) {
			   			
			   		    // Relazione external file con jcl source  
					   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.EXTERNAL_FILE_JCL_JOB, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_JOB, this.jclName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.EXTERNAL_FILE_JCL_JOB, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_JOB, this.jclName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
					   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
			   		   
					   	// Relazione external file con phisical file 
					   	if (dsname != null) {
						   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_PHISICAL_FILE, pgmExecuted, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.EXTERNAL_FILE_PHISICAL_FILE, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_PHISICAL_FILE, dsname, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.EXTERNAL_FILE_PHISICAL_FILE, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_PHISICAL_FILE, dsname, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
						}
						
					   	// Relazione con Proc sotto cui è definito lo statement
					   	if (jclEntry.isUnderProc()) {
				   			procNameOwner = jclEntry.getUnderProcName();
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.EXTERNAL_FILE_JCL_PROC, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_PROC, procNameOwner, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.EXTERNAL_FILE_JCL_PROC, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_PROC, procNameOwner, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
						}
						
					   	// Relazione con Include sotto cui è definito lo statement
					   	if (jclEntry.isUnderInclude()) {
				   			includeNameOwner = jclEntry.getUnderIncludeName();
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_INCLUDE_EXTERNAL_FILE, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, EnumObject.OBJECT_EXTERNAL_FILE, ddName, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_INCLUDE_EXTERNAL_FILE, EnumObject.OBJECT_JCL_INCLUDE, includeNameOwner, EnumObject.OBJECT_EXTERNAL_FILE, ddName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
						}
					    
					   	// Relazione con Jobname
					   	if (!jobName.equals("")) {
						   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.EXTERNAL_FILE_JCL_JOBNAME, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_JOBNAME, jobName, this.userExitInfoPgm);
						   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.EXTERNAL_FILE_JCL_JOBNAME, EnumObject.OBJECT_EXTERNAL_FILE, ddName, EnumObject.OBJECT_JCL_JOBNAME, jobName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
						   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
						}
					}
			   		
			   		// Relazione jcl source con phisical file
			   		if (dsname != null) {
					   	eo = this.analyzerDbInfo.prepareForDbObject( EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED, this.userExitInfoPgm);
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PHISICAL_FILE_JCL_JOB, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_JOB, this.jclName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PHISICAL_FILE_JCL_JOB, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_JOB, this.jclName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
					}
					
				   	// Relazione con Proc sotto cui è definito lo statement
				   	if (jclEntry.isUnderProc() && dsname != null) {
			   			procNameOwner = jclEntry.getUnderProcName();
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PHISICAL_FILE_JCL_PROC, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_PROC, procNameOwner, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PHISICAL_FILE_JCL_PROC, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_PROC, procNameOwner, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
					}
				   	
					// Relazione con Include sotto cui è definito lo statement
				   	if (jclEntry.isUnderInclude() && dsname != null) {
			   			includeNameOwner = jclEntry.getUnderIncludeName();
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.JCL_INCLUDE_PHISICAL_FILE, EnumObject.OBJECT_JCL_INCLUDE, dsname, EnumObject.OBJECT_PHISICAL_FILE, includeNameOwner, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.JCL_INCLUDE_PHISICAL_FILE, EnumObject.OBJECT_JCL_INCLUDE, dsname, EnumObject.OBJECT_PHISICAL_FILE, includeNameOwner, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
					}
				   	
				    // Relazione phisical file con Jobname
				   	if (!jobName.equals("") && dsname != null) {
					   	er = this.analyzerDbInfo.prepareForDbObjectRelation(EnumRelation.PHISICAL_FILE_JCL_JOBNAME, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_JOBNAME, jobName, this.userExitInfoPgm);
					   	ero = this.analyzerDbInfo.prepareForDbObjectRelationOrigin(EnumRelation.PHISICAL_FILE_JCL_JOBNAME, EnumObject.OBJECT_PHISICAL_FILE, dsname, EnumObject.OBJECT_JCL_JOBNAME, jobName, jclInstr, jclMvs.getJclType(), this.jclName, EnumCobolReservedWords.NOT_ASSIGNED, this.userExitInfoPgm, jclInstr);
					   	completeDbUpatesRelationOrigin(jclEntry, ero, stepName, procName, pgmExecuted);
					}
			   		break;
				   	
			   	case JCL_MVS_PROC_END:
			   		break;
			   		
				default:
					break;
			}
			
			
		}
	

			
	}

    /*
     * ------------------------------------------------
     * Completamento informazioni su origine relazione
     * ------------------------------------------------
     * 
     */
	private void completeDbUpatesRelationOrigin(JclMvsEntry jclEntry
			                                  , EntityRelationOrigin ero
											  , String stepName
											  , String procName
											  , String pgmExecuted) {
		
	   	ero.setInstrLang(EnumLanguageItemInstr.STMT_JCL);
	   	ero.setInstrCategory(jclEntry.getEntryType());
		ero.setIdObjectOrigin(this.jclName);
		ero.setTypeObjectOrigin(jclMvs.getJclType());
		ero.setTypeObjectCross(EnumObject.OBJECT_PGM_GENERIC);
		ero.setInfo1Cross(stepName);
		ero.setInfo1Cross(procName);
		ero.setInfo1Cross(pgmExecuted);
	    ero.setLibrarySourceObject("");
	    ero.setLibrarySourcePath("");
	    ero.setFileSource(this.jclName);
		
	}


	/*
	 * --------------------------------------------------------------
	 * Analisi o espansione include
	 * --------------------------------------------------------------
	 * 
	 * Viene gestito il reperimento del modulo include già analizzato e serializzato
	 * o l'attivazione ricorsiva dell'analisi del modulo include.
	 * 
	 * Se non esiste viene attivato il processo ricorsivo di analisi, al termine del quale
	 * il modulo include serializzato viene letto e i suoi elementi inseriti nella struttra del 
	 * programma corrente.
	 */
 	private void analyzeIncludeEntries(
				                        InnerContextAnalysis ictx					// Contesto corrente
				                      , InstructionJclMvs instructionIncludeStmt	// Istruzione include da espandere
									  ) throws ExceptionAmrita, SQLException {
		    	
		JclMvs objectJclMvsInclude = null;
 		EntityObject entityObject = null;

 		SourceInput siInclude = null;
 		ExceptionAmrita excp = null;  
		String includeNameToInclude = "";
				
		// Opzione in jcl principale
		this.jclMvs.setAnyInclude(true);
		
		// Recupero include eventualmente già analizzato e codificato
		includeNameToInclude = instructionIncludeStmt.jclStmtGetParm("MEMBER");
		SourceManager sm = new SourceManager(ucfg);
 		// Include serializzato non presente nella libreria delle include serializzate
		if (sm.fileInfo(ucfg.getDirJclObj(), includeNameToInclude, SUFFIX_SERIALIZED_JCL_INCLUDE) == null) {
			objectJclMvsInclude = null;
		} else {
			objectJclMvsInclude = (JclMvs) getSerialized(ucfg.getDirJclObj(), includeNameToInclude, SUFFIX_SERIALIZED_JCL_INCLUDE);
		}
  		
 		// Include serializzato non trovato: necessario analizzare ricorsivamente il sorgente include jcl
 		if (objectJclMvsInclude == null) {
 			siInclude = getSourceProcInclude(ictx, includeNameToInclude);
 		    // Sorgente non trovato: marco istruzione include come semantic error
 			if (siInclude == null) {
 				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SOURCE_GETTING);
 				instructionIncludeStmt.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0026", "", excp, this.sourceNameUnderParsing, includeNameToInclude, di.al_librarySourceSearchCode.toString());
 				instructionIncludeStmt.setSemanticError(true);
 	 			// Include mancante a livello di sistema/sottosistema
 	 			sqlInsRelationIncludeJcllMissing(includeNameToInclude);
 	            return;  
			}
 		    // Sorgente disponibile: attivazione analisi ricorsiva
			logMessage(EnumMessageType.INFORMATION, "MI0121", this.sourceNameUnderParsing, includeNameToInclude);
			
		    // Impostazioni per oggetto Include da inserire/aggiornare a fine elaborazione
			entityObject = new EntityObject();
			entityObject.setSystem(di.systemInput);   						 
			entityObject.setSubSystem(di.subSystemInput);    
			entityObject.setTypeObject(EnumObject.OBJECT_JCL_INCLUDE);
			entityObject.setIdObject(includeNameToInclude);
	        // Impostazioni come se l'include fosse alla sua prima analisi
			entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
			entityObject.setFileSource(ictx.curFilePath);   	   			          // Nome file
			entityObject.setLibrarySourceObject(ictx.curLibraryCode);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
			entityObject.setLibrarySource(ictx.curLibraryPath);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
	   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
			entityObject.setTmFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
	   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
			entityObject.setTmLastAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
			this.analyzerDbInfo.addObjEntity(entityObject);							 // Inserimento in struttura per aggiornamenti finali
			
			// Inserimento sql Delete relazione Include-Missing per il sistema/sottosistema
			sqlInsDeleteRelationIncludeJclMissing(includeNameToInclude);
			
			// Analisi ricorsiva modulo include
	        objectJclMvsInclude = analyzeIncludeRecursive(ictx, includeNameToInclude, siInclude);
			
  		    // Errore di analisi della include o di una include nested
			if (ictx.isAnyInstructionErrorDetected) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_JCL_INCLUDE_ANALYSIS);
				instructionIncludeStmt.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0027", "", excp, includeNameToInclude);
				instructionIncludeStmt.setSemanticError(true);
				return;
			}
 		}
  
   		
        // L'include era già presente come oggetto serializzato o è stata effettuata l'analisi.
 		// Vengono estratti tutti gli entry della include e inseriti nella struttura del jcl
 		// Se l'include conteneva include nesting, questi sono già inclusi nella include serializzata.
 		// Eventuali statement include nella include nesting NON vengono più esplosi.
  		// Vengono rianalizzate le sole istruzioni con variabili, risolte.

        // Accodamento statements jcl serializzati o analizzati ricorsivamente 
        for (JclMvsEntry jclMvsEntryInclude : objectJclMvsInclude.getJclEntries()) {
         	this.curNumDefJcl = ictx.jclMvs.addEntry(jclMvsEntryInclude);
         	jclMvsEntryInclude.setUnderInclude(true);
         	jclMvsEntryInclude.getInstruction().setNumInstr(this.curNumDefJcl);

		}
        
		// Update strutture di persistenza
		if (this.di.optUpdateDb) {
			updateIncludeStatusOnDbStructure(objectJclMvsInclude);			// Stato analisi  
		}
    }

 	
	/*
	 * ------------------------------------
	 * Espansione o analisi proc
	 * ------------------------------------
	 * 
	 * 
	 * Viene gestita l'espansione della proc già analizzata e serializzata
	 * o l'attivazione dell'analisi ricorsiva della proc.
	 * 
	 * Se la proc da espandere è una proc embedded, è sicuramente stata definita
	 * precedentemente ed è quindi già stata analizzata e indicizzata in map_procEmbedded()
	 * 
	 * Se la proc non è embedded e non esiste come oggetto serializzato, viene attivato 
	 * il processo di analisi, al termine del quale la proc serializzata viene letta 
	 * e i suoi elementi inseriti nella struttra del programma corrente.
	 * 
	 * In ogni caso la proc, embedded o meno, viene espansa nel jcl.
	 * Non viene effettuata ancora nessuna operazione di override di DD statements,
	 * operazioni effettuata al termine del processo preliminare di analisi del sorgente.
	 * 
	 */
 	private void analyzeExpandProcEntries(InnerContextAnalysis ictx					// Contesto corrente
					                    , InstructionJclMvs instructionStepExecStmt	// Istruzione Exec Proc=.. da espandere
								         ) throws ExceptionAmrita, SQLException {
		    	
 		JclMvsEntry jclMvsEntry = null;
   		JclMvs objectJclMvsProc = null;
 		InnerProcEmbedded innerProcEmbedded = null;
 		EntityObject entityObject = null;
  		
 		SourceInput siProc = null;
 		ExceptionAmrita excp = null;  
		String procNameToExpand = "";
		
		procNameToExpand = instructionStepExecStmt.jclStepGetExecName();
		
		// Proc embedded: inclusione diretta nel jcl
		innerProcEmbedded = this.map_procEmbedded.get(procNameToExpand);
		if (innerProcEmbedded != null) {
			// La proc embedded è già stata espansa nel jcl originale
			// Le variabili non sono ancora state risolte
			for (int i = innerProcEmbedded.iJclStart; i <= innerProcEmbedded.iJclEnd; i++) {
				jclMvsEntry = this.jclMvs.getJclEntry(i);
				this.curNumDefJcl = ictx.jclMvs.addEntry(jclMvsEntry);
				jclMvsEntry.getInstruction().setNumInstr(this.curNumDefJcl);
			}
			return;
		}
		
		
		// Recupero proc già analizzato e codificato
		SourceManager sm = new SourceManager(ucfg);
 		
		// Proc serializzato non presente nella libreria delle proc serializzate
		if (sm.fileInfo(ucfg.getDirJclObj(), procNameToExpand, SUFFIX_SERIALIZED_JCL_PROC) == null) {
			objectJclMvsProc = null;
		} else {
			objectJclMvsProc = (JclMvs) getSerialized(ucfg.getDirJclObj(), procNameToExpand, SUFFIX_SERIALIZED_JCL_PROC);
		}
 		
 				
 		// Proc serializzata non trovato: necessario analizzare ricorsivamente il sorgente proc jcl
 		if (objectJclMvsProc == null) {
 			siProc = getSourceProcInclude(ictx, procNameToExpand);
 		    // Sorgente non trovato: marco istruzione proc come semantic error
 			if (siProc == null) {
 				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SOURCE_GETTING);
 				instructionStepExecStmt.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0028", "", excp, this.sourceNameUnderParsing, procNameToExpand, di.al_librarySourceSearchCode.toString());
 				instructionStepExecStmt.setSemanticError(true);
 				ictx.isAnyInstructionErrorDetected = true;
 	 			// Proc mancante a livello di sistema/sottosistema
 	 			sqlInsRelationProcJcllMissing(procNameToExpand);
 	            return;  
			}
 		    // Sorgente disponibile: attivazione analisi ricorsiva
			logMessage(EnumMessageType.INFORMATION, "MI0123", this.sourceNameUnderParsing, procNameToExpand);
			
		    // Impostazioni per oggetto proc da inserire/aggiornare a fine elaborazione
			entityObject = new EntityObject();
			entityObject.setSystem(di.systemInput);   						 
			entityObject.setSubSystem(di.subSystemInput);    
			entityObject.setTypeObject(EnumObject.OBJECT_JCL_PROC);
			entityObject.setIdObject(procNameToExpand);
	        // Impostazioni come se la proc fosse alla sua prima analisi
			entityObject.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);  // Status di default
			entityObject.setFileSource(ictx.curFilePath);   	   			          // Nome file
			entityObject.setLibrarySourceObject(ictx.curLibraryCode);   	   		  // Nome oggetto LIBRARY libreria sorgente di analisi
			entityObject.setLibrarySource(ictx.curLibraryPath);         			  // Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
	   		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
			entityObject.setTmFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
	   		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
			entityObject.setTmLastAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
			this.analyzerDbInfo.addObjEntity(entityObject);							 // Inserimento in struttura per aggiornamenti finali
			
			// Inserimento sql Delete relazione Include-Missing per il sistema/sottosistema
			sqlInsDeleteRelationProcJclMissing(procNameToExpand);
			
			// Analisi ricorsiva proc.
			// La ricorsività può essere dovuta alle proc e alle include richiamate dentro la proc da espandere.
			// Le proc non possono essere definite dentro altre proc, ma solo richiamate, da jcl/proc/include
	        objectJclMvsProc = analyzeProcRecursive(ictx, procNameToExpand, siProc);
			
  		    // Errore di analisi della Proc  
			if (ictx.isAnyInstructionErrorDetected) {
				excp = new ExceptionAmrita(EnumAmritaExceptionError.ERROR_JCL_PROC_ANALYSIS);
				instructionStepExecStmt.setInfoError(EnumMessageType.ERROR_INTERNAL, "ET0029", "", excp, procNameToExpand);
				instructionStepExecStmt.setSemanticError(true);
				return;
			}
 		}  
  
   		
        // La proc era presente come oggetto serializzato o è stata effettuata l'analisi.
 		// Vengono estratti tutti gli entry della proc e inseriti nella struttura del jcl.
 		// Se la proc conteneva exec proc o include o include nesting, questi sono già inclusi nella proc serializzata.
 		// Eventuali statement include nelle include nesting NON vengono più esplosi.
 		// Eventuali statement statement exec proc nelle proc nesting NON vengono più esplosi.
  		// Viene effettuata la sostituzione delle variabili correnti e ri-analizzata l'istruzione se necessario.
 		
        // Accodamento statements jcl serializzati o analizzati ricorsivamente 
        for (JclMvsEntry jclMvsEntryProc : objectJclMvsProc.getJclEntries()) {
        	jclMvsEntryProc.setUnderInclude(true);
        	this.curNumDefJcl = ictx.jclMvs.addEntry(jclMvsEntryProc);
        	jclMvsEntryProc.getInstruction().setNumInstr(this.curNumDefJcl);

		}
 		
		// Update strutture di persistenza
		if (this.di.optUpdateDb) {
			updateProcStatusOnDbStructure(objectJclMvsProc);			// Stato analisi  
		}
		
 	}


	
 	/*
	 * ------------------------------------------------------------------
 	 * Inserimento statement Sql di delete relazione di proc jcl missing
 	 * ------------------------------------------------------------------
 	 * 
 	 * Operazione a livello di sistema/sottosistema
 	 * 
 	 */
	private void sqlInsDeleteRelationProcJclMissing(String procNameToExpand) {
		String strSql = "";
		
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND relation =  " + EnumRelation.SYS_SUBSYS_JCL_MVS_PROC_MISSING.ordinal() +
		         " AND idObjectA = ''" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_SYS_SUBSYS.ordinal() +
		         " AND idObjectB = '" + procNameToExpand + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_JCL_PROC.ordinal();

		this.analyzerDbInfo.al_sqlDelete.add(strSql);
	}

	/*
 	 * Inserimento statement Sql di delete relazione di include jcl missing
 	 * a livello di sistema/sottosistema
 	 * 
 	 */
	private void sqlInsDeleteRelationIncludeJclMissing(String includeNameToInclude) {
		String strSql = "";
		
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND relation =  " + EnumRelation.SYS_SUBSYS_JCL_MVS_INCLUDE_MISSING.ordinal() +
		         " AND idObjectA = ''" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_SYS_SUBSYS.ordinal() +
		         " AND idObjectB = '" + includeNameToInclude + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_JCL_INCLUDE.ordinal();

		this.analyzerDbInfo.al_sqlDelete.add(strSql);
	}

	/*
 	 * Inserimento relazione di include jcl missing
 	 * a livello di sistema/sottosistema
 	 * 
 	 */
	private void sqlInsRelationIncludeJcllMissing(String includeNameToInclude) {
		
		EntityRelation er = null;
		// Inserimento relazione fra oggetto da relazionare e oggetto relazionato per inserimento finale
   		er = new EntityRelation();
		er.setSystem(di.systemInput);   						 
		er.setSubSystem(di.subSystemInput); 
  		er.setIdObjectA("");
   		er.setIdObjectB(includeNameToInclude);
   		er.setTypeObjectA(EnumObject.OBJECT_SYS_SUBSYS);
   		er.setTypeObjectB(EnumObject.OBJECT_JCL_INCLUDE);
   		er.setRelation(EnumRelation.SYS_SUBSYS_JCL_MVS_INCLUDE_MISSING);
   		this.analyzerDbInfo.addObjEntityRelation(er);  
		
	}

	/*
 	 * Inserimento relazione di proc jcl missing
 	 * a livello di sistema/sottosistema
 	 * 
 	 */
	private void sqlInsRelationProcJcllMissing(String ProcNameToExpand) {
		
		EntityRelation er = null;
		// Inserimento relazione fra oggetto da relazionare e oggetto relazionato per inserimento finale
   		er = new EntityRelation();
		er.setSystem(di.systemInput);   						 
		er.setSubSystem(di.subSystemInput); 
  		er.setIdObjectA("");  
   		er.setIdObjectB(ProcNameToExpand);
   		er.setTypeObjectA(EnumObject.OBJECT_SYS_SUBSYS);
   		er.setTypeObjectB(EnumObject.OBJECT_JCL_PROC);
   		er.setRelation(EnumRelation.SYS_SUBSYS_JCL_MVS_PROC_MISSING);
   		this.analyzerDbInfo.addObjEntityRelation(er);
		
	}





	/*
	 * 
	 * Aggiornamento stato del jcl job in oggetto include struttura db.
	 *  
	 */
    private void updateJclJobStatusOnDbStructure(JclMvs  objectJclJob)  {
							    	
		
		// Aggiornamento stato include in oggetto inserito a fronte di analisi statemet include
		for (EntityObject eo : this.analyzerDbInfo.al_DbObject) {
			if (eo.getTypeObject() == EnumObject.OBJECT_JCL_JOB
			&&  eo.getIdObject().equals(objectJclJob.getIncludeName())) {
				if (objectJclJob.isWithErrorsDetected()) {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);
				} else {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);
				}
			}
		}
	}


	/*
	 * 
	 * Aggiornamento stato del modulo include in oggetto include struttura db.
	 *  
	 */
    private void updateIncludeStatusOnDbStructure(JclMvs  objectInclude)  {
							    	
		
		// Aggiornamento stato include in oggetto inserito a fronte di analisi statemet include
		for (EntityObject eo : this.analyzerDbInfo.al_DbObject) {
			if (eo.getTypeObject() == EnumObject.OBJECT_JCL_INCLUDE
			&&  eo.getIdObject().equals(objectInclude.getIncludeName())) {
				if (objectInclude.isWithErrorsDetected()) {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);
				} else {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);
				}
			}
		}
	}

    
	/*
	 * 
	 * Aggiornamento stato della proc in oggetto proc struttura db.
	 *  
	 */
    private void updateProcStatusOnDbStructure(JclMvs objectProc)  {
							    	
		
		// Aggiornamento stato include in oggetto inserito a fronte di analisi statemet include
		for (EntityObject eo : this.analyzerDbInfo.al_DbObject) {
			if (eo.getTypeObject() == EnumObject.OBJECT_JCL_PROC
			&&  eo.getIdObject().equals(objectProc.getProcName())) {
				if (objectProc.isWithErrorsDetected()) {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS);
				} else {
					eo.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);
				}
			}
		}
	}


	/*
	 * Logging istruzioni errate con tutte le informazioni disponibili
	 * al momento del parsing oppure con variabili non risolte
	 * 
	 * 
	 */
	private void loggingParsingErrors() {
		
		Instruction instrGeneric = null;
		
		// Scan entries contenenti le istruzioni  
		for (JclMvsEntry entryInstrucion : jclMvs.getJclEntries()) {
			
			instrGeneric = entryInstrucion.getInstruction();
			
			// Nessun errore
			if (!instrGeneric.isParsingError() 
			&&  !instrGeneric.isSemanticError()) {
				continue;
			}

			loggingParsingError(entryInstrucion);
			
		} // end-for
	}
  

	/*
	 * Logging specifica istruzione errata con tutte le informazioni disponibili
	 * al momento del parsing oppure con variabili non risolte
	 * 
	 * 
	 */
	private void loggingParsingError(JclMvsEntry entryInstrucion) {
		
		Instruction instrGeneric = null;
		
		instrGeneric = entryInstrucion.getInstruction();
		
		// Errori di parsing o di analisi semantica o di sostituzione variabili
		// Messagio e informazioni generati al momento dell'intercettamento dell'errore
		if (di.optStackTraceOnParsingError) {
			logMessage(instrGeneric.getMsgType(), instrGeneric.getMsgCode(), instrGeneric.getExcpError(),  instrGeneric.getMsgParm());
		} else {
			logMessage(instrGeneric.getMsgType(), instrGeneric.getMsgCode(), instrGeneric.getMsgParm());
		}
		
		// Logging dettaglio istruzione in errore
		logMessage(EnumMessageType.INFORMATION
			 	 , "MI0126" 
				 , this.jclName
				 , instrGeneric.getSourceInstr()
				 , instrGeneric.getTokenInError()
				 , instrGeneric.getRowStartSource()+""
				 , instrGeneric.getRowEndSource()+""
		           );
	}
  

		

	/*
	 * ------------------------------------------------------------------------------
	 * Viene restituito il descrittore del sorgente della include o della proc
	 * ------------------------------------------------------------------------------
	 * 
	 * Viene gestito il reperimento della include sorgente da analizzare dall'elenco di librerie in search
	 * specificate nelle direttive di esecuzione.<br>
	 * Se non sono specificate librerie in source, oppure se il sorgente non viene trovato in queste
	 * librerie, si cerca nella stessa libreria del sorgente sotto analisi.<br>
	 * Se il sorgente non viene trovato, restituisce null.
	 * 
	 * 
	 * @return SourceInput includeProc 
	 */
    private SourceInput getSourceProcInclude(InnerContextAnalysis ictx   	 // Contesto chiamante (jcl o altra include)
    		                               , String memberName          	 // Membro include da includere
								            ) throws ExceptionAmrita  {
							    	
        String libraryPath = "";
		int i;
        
		// Allocazioni per recupero sorgente copy
		SourceInput siMember = null;
		SourceManager sm = new SourceManager(ucfg);
		  
			
		// Scan librerie in search nel pilot di esecuzione
		for (String librarySearchCode : this.di.al_librarySourceSearchCode) {
			
			// Scan librerie definite nel pilot di esecuzione
            for (i = 0; i < this.di.al_libraryCode.size(); i++) {
				if (this.di.al_libraryCode.get(i).equals(librarySearchCode)) {
					break;
				}
			}
			
			// Nome libreria in search non definita nel pilot: skip
            if (i >= this.di.al_libraryCode.size()) {
				continue;
			}
            
            ictx.curLibraryCode = this.di.al_libraryCode.get(i);
            ictx.curLibraryPath = this.di.al_libraryPath.get(i);
            
            libraryPath = this.di.al_libraryPath.get(i);
             
			// Recupero sorgente per attivazione processo di analisi ricorsivo
            siMember = sm.getSource(libraryPath				    // Ricerca nella libreria in path
			                	 ,memberName					// Estratto dallo statement Include 
			                     ,ictx.si.getSourceSuffix()     // Ricerca source con lo stesso suffisso del chiamante
			                     ,false                         // Nessun controllo sul tipo sorgente
			                     ,false                         // Nessuna informazione completa di file system
			                     );
			
			// Sorgente individuato: exit
			if (siMember != null) {
				break;
			}
			
			// Next libreria
			
		} // end.for
		
		
		// Sorgente non trovato nelle librerie in search: provo a cercarlo nella stessa directory del sorgente sotto analisi
		if (siMember == null) {
			siMember = sm.getSource(ictx.si.getDirInput()		// Ricerca nella stessa libreria corrente
			                	 ,memberName				    // Estratto dallo statement Include 
			                     ,ictx.si.getSourceSuffix()    // Ricerca source con lo stesso suffisso del chiamante
			                     ,false                         // Nessun controllo sul tipo sorgente
			                     ,false                         // Nessuna informazione completa di file system
			                     );
		}

		return siMember;
	}


    


	/*
     * 
     * 
     *  Restituisce il token successivo dopo aver verificato che esiste.
     *  Se non esiste restituisce ""
     *  Viene gestito un Token di Replacing By dello statement Copy 
     *  ==x==  oppure == x ==, come ==05== == 05 == ==(05)==
     *  
     * 
     */
	private String nextToken(Scanner scn) {
		
		String newToken = "";
		
		// Non ci sono altri token: return stringa vuota
		if (scn.hasNext()) {
			newToken = scn.next();
		} 

		return newToken;
	}

	

	/*
	 * 
	 * Individua il tipo di istruzione jcl codificato nella riga.
	 * 
	 * Nel caso di istruzione non individuata e SYSIN DD * attivo,
	 * marca l'istruzione come scheda di sysin.
	 * 
	 */
	private EnumInstrDataCategory getInstructionType(InnerContextAnalysis ictx) {
		 
        Scanner scn = null;
		InnerKeyJclInstr innerKeyJclInstr = null;
		String rowAllClean = "";
		
		if (ictx.rowAll.length() > 71) {
			rowAllClean = ictx.rowAll.substring(0, 71);
		} else {
			rowAllClean = ictx.rowAll;
		}
		
		
        scn = new Scanner(rowAllClean);
		
        ictx.token = nextToken(scn);
        
        innerKeyJclInstr = this.map_keyJclInstr.get(ictx.token);
        
        // Non inizia con un token conosciuto ma è ancora attiva la sysin dd * e non è il token di custom delimiter
        if (innerKeyJclInstr == null
        &&  ictx.isSysinEmbedded
        && !ictx.token.equals(ictx.ddSysinDelimiter)) {
        	return EnumInstrDataCategory.JCL_MVS_SYSIN_STREAM;
        }
        
        // Non inizia con un token conosciuto ma è ancora attiva la sysin dd * ed è il token di custom delimiter
        if (innerKeyJclInstr == null
        &&  ictx.isSysinEmbedded
        &&  ictx.token.equals(ictx.ddSysinDelimiter)) {
        	return EnumInstrDataCategory.JCL_MVS_DELIMITER;
        }
        
        // Non inizia con un token conosciuto e non è ancora attiva la sysin dd *
        if (innerKeyJclInstr == null
        &&  ictx.ddStmtWithSysinEmbedded == null
        && !ictx.token.startsWith("//")) {
        	// TODO istruzione  errata
        	return EnumInstrDataCategory.NOT_ASSIGNED;
        }
        
        // Primo token indica commento
        if (innerKeyJclInstr != null 
        && innerKeyJclInstr.en_WordReservedOwner == EnumInstrDataCategory.JCL_MVS_COMMENT) {
			return innerKeyJclInstr.en_WordReservedOwner;
		}
        
        // Primo token non //.
        // Si tratta di statement Jes2, jes3  
        if (innerKeyJclInstr != null 
        &&  innerKeyJclInstr.en_WordReservedOwner != EnumInstrDataCategory.JCL_MVS_JOBEND) {
			return innerKeyJclInstr.en_WordReservedOwner;
		}
        
        
        // Istruzione jcl, commento pieno o comando Jes3
        
        // Jes3 command, non codificato nella enumerazione
        if (ictx.token.startsWith("//**")) {
        	return EnumInstrDataCategory.JCL_MVS_JES3_COMMAND;
		}
        // Commento //*comment
		if (ictx.token.startsWith("//*")) {
			return EnumInstrDataCategory.JCL_MVS_COMMENT;
		}
		
        // Se è una istruzione jcl nativa, è identificata dal secondo token
		ictx.token = "";
		if (scn.hasNext()) {
			ictx.token = nextToken(scn);
		}
        innerKeyJclInstr = this.map_keyJclInstr.get(ictx.token);
        
        // Istruzione riconosciuta
        if (innerKeyJclInstr != null) {
        	// DD: verifica se in stream
        	if (innerKeyJclInstr.en_WordReservedOwner == EnumInstrDataCategory.JCL_MVS_DD) {
        		if (scn.hasNext()) {
        			ictx.token = nextToken(scn);
        		}
                if (ictx.token.startsWith("*")) {
					ictx.isSysinEmbedded = true;
				}
		     }
 		     return innerKeyJclInstr.en_WordReservedOwner;
		}

        // Istruzione di chiusura jcl
        if (rowAllClean.startsWith("//")
        &&	ictx.token.equals("")) {
        	return EnumInstrDataCategory.JCL_MVS_JOBEND;
		}
 
        
        // Sicuramente una istruzione di continuazione dell'istruzione precedente/i

        
		return EnumInstrDataCategory.NOT_ASSIGNED;
	}




	/*
	 * Compone l'istruzione a partire dalla riga di partenza.
	 * La prima riga viene portata in output con la //, quelle di continuazione senza.
	 * Si considera da posStartInstr, rowStartInstr fino alla riga rowEndInstr
	 * Vengono eliminate le righe vuote e i commenti, accodati in cima all'istruzione
	 * 
	 */
	private String packageInstr(InnerContextAnalysis ictx) {
		
		String instrPackaged = "";
		String row = "";
        
		// Prima riga istruzione
		if (ictx.ar_RowsSource[ictx.rowStartSource].length() <=  71) {
			row = ictx.ar_RowsSource[ictx.rowStartSource].trim();
		} 
		if (ictx.ar_RowsSource[ictx.rowStartSource].length() >  71) {
			row = ictx.ar_RowsSource[ictx.rowStartSource].substring(0, 71).trim();
		}  
		instrPackaged = rowWithoutComments(row);
		
		// Accodamento eventuali righe successive di continuazione
		for (int i = ictx.rowStartSource + 1; i <= ictx.rowEndSource; i++) {

			row = ictx.ar_RowsSource[i];
			
			// Commenti
			if (row.length() >= 3) {
				if (row.startsWith("//*")) {
					ictx.al_CommentsBeforeInstr.add(ictx.rowInstr);
					continue;
				}
			}

			// Riga nulla: skip
			if (row.equals("")) {
				continue;
			}

			// Riga continuazione istruzione
			if (ictx.ar_RowsSource[ictx.rowStartSource].length() <=  71) {
				row = ictx.ar_RowsSource[i].substring(3).trim();
				instrPackaged = instrPackaged + rowWithoutComments(row);
			} 
			if (ictx.ar_RowsSource[ictx.rowStartSource].length() >  71) {
				row = instrPackaged + ictx.ar_RowsSource[i].substring(2, 71).trim();
				instrPackaged = instrPackaged + rowWithoutComments(row);
			}  
		}
		
		return instrPackaged.trim();
	}

    /*
     * Restituisce la riga di jcl senza gli eventuali commenti finali.
     * Si scanna dall'ultima posizione della riga.
     * Se si trova una virgola è l'ultimo parametro di una continuazione.
     * Se si trova un = si tratta dell'ultimo parametro 
     * In entrambi i casi si mette a space il resto della riga
     * 
     */
	private String rowWithoutComments(String row) {
		String rowCleaned = "";
		int i = 0;
		rowCleaned = row;
		
		// Scan reverse riga
		for (i = row.length() - 1; i > 0; i--) {
			
			// Virgola di fine parametro, in istruzione di continuazione
			if (row.charAt(i) == ',') {
				rowCleaned = row.substring(0, i + 1) + " ";
				return rowCleaned;
			}
			
			// = di assegnazione parametro o di fine stringa o parametro
			if (row.charAt(i) == '='
			||	row.charAt(i) == ')'	
			||	row.charAt(i) == '"'	
			||	row.startsWith("'", i)) {
				break;
			}
		}

        // Nessuna assegnazione e nessun parametro: restituisco inalterata
		if (i == 0) {
			return row;
		}
		
		// Ultimo parametro stringa o fra parentesi
		if (row.charAt(i) == '"'
		||	row.startsWith("'", i)
		||	row.startsWith(")", i)) {
			rowCleaned = row.substring(0, i + 1) + " ";
			return rowCleaned;
		}
		
		// Ultimo parametro assegnato a valore terminato da space
		for (; i < row.length(); i++) {
			if (row.startsWith(" ", i)) {
				rowCleaned = row.substring(0, i) + " ";
				break;
			}
		}
		return rowCleaned;
	}


	/**
	 * 
	 * Estrazione informazioni specifiche dalla riga di 80 crt (max)
	 * @param ictx 
	 * 
	 */
	private void extractRowFields(InnerContextAnalysis ictx, String rowAll ) {
		
		ictx.rowRight = "";
		ictx.rowInstr = "";
		
		if (rowAll.length() > 71) {ictx.rowRight = rowAll.substring(71);}         
		if (rowAll.length() >= 71) {ictx.rowInstr = rowAll.substring(2,71);}         
		if (rowAll.length() > 2 && rowAll.length() < 71 ) {ictx.rowInstr = rowAll.substring(2);}
	}


	/**
	 * 
	 * Inizializzo strutture dinamiche di memorizzazione di una istruzione
	 * @param ictx 
	 * 
	 */
	private void initializeForNewInstruction(InnerContextAnalysis ictx) {
		
		ictx.rowRight = "";		
		ictx.rowInstr = "";          
		
		// Per gestione dinamica commenti incontrati prima dell'istruzione, a sinistra e a destra
		ictx.al_CommentsBeforeInstr.clear();       
	} 
	



	
    ////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	

	
	/*
	 * 
	 * Classe contenitore con le informazioni correnti sull'analisi del sorgente corrente.
	 * 
	 * Le informazioni sono relative al contesto di uno specifico livello ricorsivo.
	 * Una istanza viene istanziata relativamente al programma sotto analisi. 
	 * Per ogni modulo copy viene creata una nuova istanza e attivata l'analisi, ricorsivamente. 
	 * Così a qualsiasi livello di ricorsività dei moduli copy.
	 * 
	 */
	public class InnerContextAnalysis implements Cloneable {

	    // Oggetto jcl corrente, inizialmente il primo jcl sotto analisi
		JclMvs jclMvs = null;                           //
		
		// Condizioni generali di analisi
		boolean isAnalysisOfJcl = true;                 // False indica elaborazione a fronte di analisi specifica con metodo analyzeJclInclude() o analyzeJclProc()
	    boolean isAnyInstructionErrorDetected = false;  // True indica un errore di parsing durante l'analisi sorgenti o semantico o di ceazione grafo (semantico) 
	    boolean isInstructionToAppend = true;           // True indica che l'istruzione deve essere accodata dopo l'analisi, false che l'analisi è terminata
		
	    // Map con le variabili attive nel livello corrente e il loro valore, usate per sostituzione valori.
	    // Si tratta di valori dichiarati esplicitamente con SET o definiti nello statement EXERC PROC=, o nello statement PROC
	    Map<String, String> map_var = null;             // Key=nome variabile, Data=valore corrente
	    
	    // Informazioni su origine sorgente include/Proc analizzato on fly
	    String curLibraryCode = "";                     // Codice libreria sorgente corrente include/proc
	    String curLibraryPath = "";                     // Path   libreria sorgente corrente include/proc
	    String curFilePath = "";                        // Path   file sorgente corrente include/proc
	    
	    // Array cumulativo statements nested a fronte di include/proc/proc embedded ricorsivi
		ArrayList<JclMvsEntry> al_nestedEntry = null;  // Istruzioni cumulative a fronte di richiamo di include/proc nested
	    
		// Informazioni per gestione analisi include/proc on fly
		boolean isIncludeAncestorDeserialized = false;  // True indica che l'include con include nested presente nel programma è stato deserializzato e NON analizzato
		boolean isProcAncestorDeserialized = false;     // True indica che l'include con include nested presente nel programma è stato deserializzato e NON analizzato
		InstructionJclMvs includeStmt = null;           // Già analizzata e con le informazioni di replacing
		InstructionJclMvs procExecStmt = null;          // Già analizzata e con le informazioni sulle variabili utilizzate
        String includeName = "";                        // Nome include corrente se attiva analisi di include, impostato anche a fronte di statement include
        String procName = "";                           // Nome proc corrente se attiva analisi di ua include, impostato anche a fronte di statement proc
		int nestingLvel = 0;                            // Livelolo di ricorsivià include/proc

        // Informazioni per gestione sysin embedded 
		InstructionJclMvs ddStmtWithSysinEmbedded = null;// Ultima dd analizzata del tipo //ddname DD *
		String ddSysinDelimiter = "";                    // Chiude il sysin embedded se   //ddname DD *,DLM=FFF
		boolean isSysinEmbedded = false;                 // True indica che è attiva una sysyin embedded
        
        // Identificazione sorgente e istruzione
		SourceInput si = null;							// Descrittore completo sorgente sotto analisi
		EnumInstrDataCategory activeTypeInstr = null;        // Tipologia istruzione codificata, come parola riservata di inizio
		EnumSourceType activeTypeSource = null;         // Tipologia sorgente sotto analisi
		String[] ar_RowsSource = null;            		// Array righe sorgente  complessive
		String[] ar_RowsSourceInstruction = null;       // Array righe sorgente  istruzione corrente pacchettizzata. Include ebentuali altre istruzioni stessa riga
		String activeSourceName = "";                   // Nome sorgente in analisi, jcl o include al livello nesting corrente
		int activeInstrKeyWordsSize = 0;  	      		// Numero parole chiave che hanno identificato l'istruzione corrente
		int numInstr = 0;                     	 		// Numero sequenza entry (0-based)
		int rowStart = 0;	                 		    // Numero riga sorgente di analisi di inizio
		int rowStartSource = 0;	                 		// Numero riga sorgente di inizio istruzione
		int rowEndSource = 0;	                 		// Numero riga sorgente di fine istruzione
		String nameInstr = null;                  		// Nome istruzione (es. IF)
		boolean isLiteralInProgress = false;            // True indica token dentro literal in identificazione istruzione successiva
		boolean isLiteralDoubleDelimited = false;       // Nome istruzione (es. IF)
	    String sourceInstr = "";			 		 	// Istruzione source estratta completa, senza punto finale
	    Object objectInstr = "";			 		 	// Oggetto Instruction codificato da sourceInstr

		// Commenti istruzione corrente
	    ArrayList<String> al_CommentsBeforeInstr = null;// ArrayList righe commento precedenti l'istruzione  
		String[] ar_CommentsBeforeInstr = null;   		// Array righe commento precedenti l'istruzione

		// Campi e variabili di servizio per estrazione istruzione da righe sorgente
	    String token = "";						  		// Token di servizio
	    String curToken = "";							// Token corrente istruzione sotto analisi
	    String rowAll = "";								// Pos 1-80
	    String rowRight = "";							// Pos 72-80
	    String rowInstr = "";               			// Pos 8-71

	    
	    /*
	     * 
	     * Costruttore
	     * 
	     */
		public InnerContextAnalysis() {
			al_CommentsBeforeInstr = new ArrayList<String>();       
			activeTypeInstr = EnumInstrDataCategory.NOT_ASSIGNED;  
		    al_nestedEntry = new ArrayList<JclMvsEntry> ();
		    map_var = new HashMap<String, String> ();
		}


		/* (non-Javadoc)
		 * @see java.lang.Object#clone()
		 */
		@Override
		protected InnerContextAnalysis clone()  {
			InnerContextAnalysis ictxCloned = null;

			try {
				ictxCloned =   (InnerContextAnalysis) super.clone();
			} catch (CloneNotSupportedException e) {
				// Exception ne chiamanti
				ictxCloned = null;
			}
			
			// Inizializzazione variabili per nuovo contesto
			ictxCloned.activeInstrKeyWordsSize = 0;  	    
			ictxCloned.numInstr = 0;                     	 		 
			ictxCloned.rowStartSource = 0;	                 		 
			ictxCloned.rowEndSource = 0;	                 		 
			return ictxCloned;
		}

	
	}



	
	/*
	 *   Classe contenitore di servizio associato alla prima parola riservata di istruzioni Jcl.
	 *   
	 *   Nel jcl le istruzioni sono identificate da una sola parola.
	 *   
	 */
	private class InnerKeyJclInstr {
		
		public EnumInstrDataCategory en_WordReservedOwner = null;       // Enum parola riservata indicante l'istruzione
		@SuppressWarnings("unused")
		public EnumInstrDataCategory en_InstrCategory = null;      // Categoria word: JCL_MVS_NATIVE, INCLUDE, ....
		
        /*
         * Costruttore
         */
        private InnerKeyJclInstr() {
        	en_WordReservedOwner = EnumInstrDataCategory.NOT_ASSIGNED;
        	en_InstrCategory = EnumInstrDataCategory.NOT_ASSIGNED;
		}
	}
	/*
	 * Classe contenitore di servizio associato alle proc embedded definite nel Jcl.
	 *   
	 * In fase di analisi per ogni proc embedded incontrata si istanzia un oggetto di questa classe.
	 * 
	 */
	@SuppressWarnings("unused")
	private class InnerProcEmbedded {
		
		String procName = "";									   // Nome procedura embedded
		InstructionJclMvs jclInstrProc = null;                     // Statement PROC con i parametri
		int iJclStart = 0;                                         // Numero istruzione di inizio nel jcl
		int iJclEnd = 0;                                           // Numero istruzione di fine   nel jcl
		
		
    /*
     * Costruttore vuoto
     */
    private InnerProcEmbedded() {
 	}
  }
}
