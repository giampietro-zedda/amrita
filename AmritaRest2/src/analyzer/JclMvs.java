package analyzer;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import enums.EnumInstrDataCategory;
import enums.EnumObject;
import enums.EnumObjectOption;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda Turin (ITALY)
 * 
 * <h1>
 * JclMvs 
 * </h1>
 *  <p>
 * Questa classe  modella un generico jcl Mvs, a fronte dell'analisi di un sorgente jcl.<br>
 * <p>
 * Ogni istruzione presente nel jcl viene codificata come istanza della classe {@link InstructionJclMvs}, che contiene
 * tutti i metodi specifici di ogni tipologia di istruzione, codificata da {@link EnumInstrDataCategory}<br>
 * Sono codificate inoltre le informazioni del sorgente originale e quanto necessario per ricostruirlo.<br>
 * L'istruzione jcl {@link InstructionJclMvs} viene quindi collocata nell'oggetto contenitore {@link JclMvsEntry}, che
 * contiene ulteriori informazioni sul contesto in cui l'istruzione è collocata, quali il modulo include,
 * il livello di nesting e così via.<br>
 * In questa classe sono presenti i metodi per ottenere tutte le informazioni sul jcl, a livello generale e di ogni
 * singola istruzione.<br> 
 * Questa classe viene serializzata e fornisce uno strumento programmatico di interrogazione e manipolazione del jcl,
 * in modo simile a quanto ciene effettuato dalla classe {@link Program}, per i programmi.<br>
 *
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 23/01/2011
 * @see InstructionJcl
 * @see EnumInstrDataCategory
 *   
 */

public class JclMvs implements Serializable, AmritaConstants {

	private static final long serialVersionUID = 1L;

	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza di reference a servizi centralizzati e a oggetti condivisi                    //                                                        //
    ///////////////////////////////////
	/////////////////////////////////////////////////////////////////////
    
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  comuni a tutti i tipi di programma                                                                          
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    private EnumObject jclType = null;        								// OBJECT_JCL_SOURCE, OBJECT_JCL_PROC, OBJECT_JCL_INCLUDE
    private EnumObjectOption jclTypeObject = null;        					// JCL_MVS_WITH_PROC_EMBEDDED
    private String jclSourceName = "";              						// Nome source 
    private String libOwnerCode = "";              							// Codice libreria dove il source è collocato
    private String libOwnerPath = "";              							// Path   libreria dove il source è collocato
    private String jobName = "";              								// Nome Job
    private String procName = "";              								// Nome Proc codificata che il jcl descrive
    private boolean isProc = false;                 	        			// True indica che il jcl definisce una proc 
    private boolean isInclude = false;                 			        	// True indica Che il jcl decrive un include group (un copy jcl)
    private boolean isThereProcEmbedded = false;                 			// True indica Proc embedded presente prima del jcl job
    private boolean isThereJobStatement = false;                 			// True indica Che il jcl, a meno dei commenti, inizia con //XX JOB
	private boolean isWithErrorsDetected = false;							// True Indica errore di parsing sorgente 
	private boolean isThereAnyInclude = false;                              // True indica che il jcl richiama delle include

	
	// Entries contenenti istruzioni jcl in base al tipo di jcl attivo
	private ArrayList<ArrayList<JclMvsEntry>> al_procEmbedded = null;	    // Eventuale/i proc embedded
	private ArrayList<JclMvsEntry> al_entryJcl = null;	            	    // Entries con istruzioni jcl
	
	
	/**
	 * 
	 * Costruttore
	 * 
	 */
	public JclMvs(UserConfiguration sd, String jclSourceName) {
		
		// Nome modulo copy
		this.jclSourceName = jclSourceName;
		
		// Inizializzazione variabili di istanza
		this.jclType = EnumObject.NOT_ASSIGNED;
		this.al_entryJcl = new ArrayList<JclMvsEntry> ();
		this.al_procEmbedded = new ArrayList<ArrayList<JclMvsEntry>> ();
	}

	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                     Metodi pubblici                   /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
 
	

	/**
	 * Restituisce gli entry contenenti statement jcl schedulabili.<br>
	 * <p>
	 * Viene restituito un ArrayList di oggetti {@link JclMvsEntry} con tutte gli
	 * statemnt jcl, incluso la scheda job senza le eventuali precedenti proc embedded.<br>
	 * 
	 * 
	 * @return ArrayList<JclMvsEntry> al_entry
	 */
	public ArrayList<JclMvsEntry> getJclEntries() {
 		return this.al_entryJcl;
	}


	/**
	 * Restituisce l'entry contenente uno statement jcl, non di proc emedded.<br>
	 * <p>
	 * Viene richiesto il numero di definizine dello statement, 0 based.<br>
	 * Se l'entry non è presente viene restituito null.<br>
	 * <p>
	 * @return JclMvsEntry al_entry
	 */
	public JclMvsEntry getJclEntry(int numEntry) {
		
		// Out of range
		if (numEntry > this.al_entryJcl.size() - 1) {
			return null;
		}
 		return this.al_entryJcl.get(numEntry);
	}



	/**
	 * Imposta gli entries contenenti statement jcl.<br>
	 * <p>
	 * 
	 * @param ArrayList<JclMvsEntry> al_entryJcl
	 */
	public void setJclEntries(ArrayList<JclMvsEntry> al_entryJcl) {
		this.al_entryJcl = al_entryJcl;
	}



	

	/**
	 * Restituisce true se il jcl ha errori di parsing.<br>
	 * <p>
	 * 
	 * @return the isWithErrorsDetected
	 */
	public boolean isWithErrorsDetected() {
		return this.isWithErrorsDetected;
	}




	/**
	 * Restituisce true se il jcl contiene delle statement di inclusione siurce INCLUDE.<br>
	 * <p>
	 * 
	 * @return the isThereAnyInclude
	 */
	public boolean isThereAnyInclude() {
		return isThereAnyInclude;
	}



	/**
	 * Imposta se il jcl contiene delle statement di inclusione siurce INCLUDE.<br>
	 * <p>
	 * @param isThereAnyInclude the isThereAnyInclude to set
	 */
	public void setAnyInclude(boolean isThereAnyInclude) {
		this.isThereAnyInclude = isThereAnyInclude;
	}



	/**
	 * Imposta se il jcl ha errori di parsing.<br>
	 * 
	 * @param parsingWithErrors the parsingWithErrors to set
	 */
	public void setParsingWithErrors(boolean isWithErrorsDetected) {
		this.isWithErrorsDetected = isWithErrorsDetected;
	}


	
	
	

	/**
	 * Restituisce il codice della libreria dove il source jcl è collocato.<br>
	 * <p>
	 * Il codice della libreria è il nome dell'oggetto LIBRARY nel database.<br>
	 * 
	 * @return the libOwnerCode
	 */
	public String getLibOwnerCode() {
		return libOwnerCode;
	}


	/**
	 * Imposta il codice della libreria dove il source jcl è collocato.<br>
	 * <p>
	 * Il codice della libreria è il nome dell'oggetto LIBRARY nel database.<br>
	 * 
	 * @param libOwnerCode the libOwnerCode to set
	 */
	public void setLibOwnerCode(String libOwnerCode) {
		this.libOwnerCode = libOwnerCode;
	}



	/**
	 * Restituisce il path della libreria dove il source jcl è collocato.<br>
	 * <p>
	 * La libreria è un oggetto LIBRARY nel database.<br>
	 * 
	 * 
	 * @return the libOwnerPath
	 */
	public String getLibOwnerPath() {
		return libOwnerPath;
	}



	/**
	 * Imposta il path della libreria dove il source jcl è collocato.<br>
	 * <p>
	 * La libreria è un oggetto LIBRARY nel database.<br>
	 * 
	 * 
	 * @param libOwnerPath the libOwnerPath to set
	 */
	public void setLibOwnerPath(String libOwnerPath) {
		this.libOwnerPath = libOwnerPath;
	}



	/**
	 * Restituisce il nome del memro sorgente del jcl.<br>
	 * <p>
	 * 
	 * @return the jclSourceName
	 */
	public String getSourceName() {
		return jclSourceName;
	}



	/**
	 * Imposta il nome del memro sorgente del jcl.<br>
	 * <p>
	 * @param jclSourceName the sourceName to set
	 */
	public void setSourceName(String jclSourceName) {
		this.jclSourceName = jclSourceName;
	}



	/**
	 * Restituisce il nome della proc che il jcl codifica<br>
	 * <p>
	 * @return the procName
	 */
	public String getProcName() {
		return procName;
	}

	/**
	 * Restituisce il numero di definizione, 0-based della proc embedded definita nel jcl.<br>
	 * <p>
	 * Viene fornito il nome della proc, memorizzato nell'istruzione PROC, la prima di
	 * ogni procedure.<br>
	 * Se la proc non è esiste viene restituito -1.<br>
	 * <p>
	 * @return the procName
	 */
	public int getProcEmbeddedNumber(String procName) {
		ArrayList<JclMvsEntry> al_entryProcEmbedded = null;
        InstructionJclMvs instrProc = null;
		String procNameInstr = "";
		int procNumber = -1;
		
		// Scan Proc embedded presenti
		for (int i = 0; i < this.al_procEmbedded.size(); i++) {
			al_entryProcEmbedded = this.al_procEmbedded.get(i);
			instrProc = al_entryProcEmbedded.get(0).getInstruction();
			procNameInstr = instrProc.jclProcGetName();
			if (procName.equals(procNameInstr)) {
				procNumber = i;
				break;
			}
		}
		
		return procNumber;
	}
	/**
	 * Restituisce il numero di proc embedded definite nel jcl.<br>
	 * <p>
	 * @return int proc embedded size
	 */
	public int getProcEmbeddedSize(String procName) {
		return this.al_procEmbedded.size();
	}


	/**
	 * Restituisce i nomi di tutte le proc embedded codificate nel jcl<br>
	 * <p>
	 * @return the procName
	 */
	public String[] getProcEmbeddedNames() {
		ArrayList<JclMvsEntry> al_entryProcEmbedded = null;
		ArrayList<String> al_procEmbeddedName = null;
		String ar_procEmbeddedName[] = null;
        InstructionJclMvs instrProc = null;
		String procName = "";
		
		al_procEmbeddedName = new ArrayList<String> ();
		
		// Scan Proc embedded presenti
		for (int i = 0; i < this.al_procEmbedded.size(); i++) {
			al_entryProcEmbedded = this.al_procEmbedded.get(i);
			instrProc = al_entryProcEmbedded.get(0).getInstruction();
			
			procName = instrProc.jclProcGetName();
			al_procEmbeddedName.add(procName);
		}
		ar_procEmbeddedName = new String[al_procEmbeddedName.size()];
		ar_procEmbeddedName = al_procEmbeddedName.toArray(ar_procEmbeddedName);
		
		return ar_procEmbeddedName;
	}


	/**
	 * Restituisce tutte le istruzioni della proc embedded codificate dal jcl<br>
	 * <p>
	 * @return ArrayList<InstructionJclMvs> al_instrProcEmbedded
	 */
	public ArrayList<InstructionJclMvs> getProcEmbeddedInstr(int procNumber) {
		ArrayList<JclMvsEntry> al_entryProcEmbedded = null;
		ArrayList<InstructionJclMvs> al_instrJcl = null;
		
		al_instrJcl = new ArrayList<InstructionJclMvs>();
		al_entryProcEmbedded = this.al_procEmbedded.get(procNumber);
		
		// Scan contenitori istruzioni proc embedded
		for (JclMvsEntry entryProcEmbedded : al_entryProcEmbedded) {
			al_instrJcl.add(entryProcEmbedded.getInstruction());
		}
		return al_instrJcl;
	}



	/**
	 * Imposta il nome della proc codificata dal jcl<br>
	 * <p>
	 * @param procName the procName to set
	 */
	public void setProcName(String procName) {
		this.procName = procName;
	}



	/**
	 * Restituisce il nome del job del jcl<br>
	 * <p>
	 * La scheda job rappresenta la prima secheda utile del jcl,
	 * dopo eventuali righe di commento.<br>
	 * <p>
	 * @return the jobName
	 */
	public String getJobName() {
		return jobName;
	}


	/**
	 * Restituisce il nome della include che questo oggetto descrive<br>
	 * <p>
	 * Il nome della include coincide con il nome del source e ha senso richiamare
	 * questo metodo solo su un oggetto Include.
	 * <p>
	 * @return the include name
	 */
	public String getIncludeName() {
		return this.jclSourceName;
	}



	/**
	 * Imposta il nome del job del jcl<br>
	 * <p>
	 * La scheda job rappresenta la prima secheda utile del jcl,
	 * dopo eventuali righe di commento.<br>
	 * <p>
	 * @param jobName the jobName to set
	 */
	public void setJobName(String jobName) {
		this.jobName = jobName;
	}



	/**
	 * Restituisce il tipo di oggetto, OBJECT_JCL.<br>
	 * <p>
	 * @return the jclType
	 */
	public EnumObject getJclType() {
		return jclType;
	}



	/**
	 * Imposta il tipo di oggetto, OBJECT_JCL.<br>
	 * <p>
	 * @param jclType the jclType to set
	 */
	public void setJclType(EnumObject jclType) {
		this.jclType = jclType;
	}



	/**
	 * Restituisce il tipo di oggetto JCL, JCL_MVS, JCL_MVS_PROC.<br>
	 * <p>
	 * @return the jclTypeObject
	 */
	public EnumObjectOption getJclTypeObject() {
		return jclTypeObject;
	}



	/**
	 * Imposta il tipo di oggetto JCL, JCL_MVS, JCL_MVS_PROC.<br>
	 * <p>
	 * @param jclTypeObject the jclTypeObject to set
	 */
	public void setJclTypeObject(EnumObjectOption jclTypeObject) {
		this.jclTypeObject = jclTypeObject;
	}



	/**
	 * Restituisce true se presente una proc embedded.<br>
	 * <p>
	 * Si tratta di jcl che, prima di normali schede jcl di
	 * esecuzione, definiscono una proc interna con PROC/PROC END<br>
	 * richiamata successivamente nello stesso source.<br>
	 * <p>
	 * @return the isThereProcEmbedded
	 */
	public boolean isThereProcEmbedded() {
		return isThereProcEmbedded;
	}



	/**
	 * Imposta se presente una proc embedded.<br>
	 * <p>
	 * Si tratta di jcl che, prima di normali schede jcl di
	 * esecuzione, definiscono una proc interna con PROC/PROC END<br>
	 * richiamata successivamente nello stesso source.<br>
	 * <p>
	 * @param isThereProcEmbedded the isThereProcEmbedded to set
	 */
	public void setProcEmbedded(boolean isThereProcEmbedded) {
		this.isThereProcEmbedded = isThereProcEmbedded;
	}

	/**
	 * Restituisce true se presente la scheda // JOB.<br>
	 * <p>
	 * @return the isThereJobStatement
	 */
	public boolean isThereJobStatement() {
		return isThereJobStatement;
	}



	/**
	 * Imposta se presente la scheda // JOB.<br>
	 * <p>
	 * @param boolean isThereJobStatement
	 */
	public void setJobStatement(boolean isThereJobStatement) {
		this.isThereJobStatement = isThereJobStatement;
	}


	/**
	 * Restituisce true se il jcl descrive una proc.<br>
	 * <p>
	 * Si tratta di jcl che, prima di normali schede jcl di
	 * esecuzione, definiscono una proc interna con PROC/PROC END<br>
	 * richiamata successivamente nello stesso source.<br>
	 * <p>
	 * @return the isProc
	 */
	public boolean isProc() {
		return isProc;
	}



	/**
	 * Imposta true se il jcl descrive una proc.<br>
	 * <p>
	 * Si tratta di jcl che, prima di normali schede jcl di
	 * esecuzione, definiscono una proc interna con PROC/PROC END<br>
	 * richiamata successivamente nello stesso source.<br>
	 * <p>
	 * @param isProc the isProc to set
	 */
	public void setProc(boolean isProc) {
		this.isProc = isProc;
	}



	/**
	 * Restituisce true se si descrive un jcl include.<br>
	 * <p>
	 * I jcl include sono funzionalmente equivalenti ai Copy
	 * e sono inclusi nel jcl con lo statement <tt>INCLUDE memer</tt><br>
	 * <p>
	 * 
	 * @return the isInclude
	 */
	public boolean isInclude() {
		return isInclude;
	}



	/**
	 * Imposta se si descrive un jcl include.<br>
	 * <p>
	 * I jcl include sono funzionalmente equivalenti ai Copy
	 * e sono inclusi nel jcl con lo statement <tt>INCLUDE memer</tt><br>
	 * <p>
	 * 
	 * @param isInclude the isInclude to set
	 */
	public void setInclude(boolean isInclude) {
		this.isInclude = isInclude;
	}



	/**
	 * Restituisce un oggetto {@link InstructionJclMvs} con la scheda Job.<br>
	 * <p>
	 * Si tratta della prima istruzione definita in un jcl schedulabile, dopo le
	 * eventuali proc embedded.<br>
	 * lo statement contiene tutte le informazioni, fruibili da metodi specifici,
	 * che qualificano la scheda job.<br>
	 * Sono inoltre disponibili tutte le informazioni delle righe sorgente originali.<br>
	 * <p>
	 * @return the instrJob
	 */
	public InstructionJclMvs getJobInstr() {
		return this.al_entryJcl.get(0).getInstruction();
	}

	/**
	 * Restituisce i nomi degli step in sequenza nel jcl<br>
	 * <p>
	 * Gli step sono restituiti anche se definiti in proc annidate, a qualsiasi livello.<br>
	 * Uno step e' rappresentato da una exec a un programma o una procedura.<br>
	 * Se gli step non sono qualificati da un nome, allora viene restituito
	 * "#n# dove n è il numero di step 1-based.<br>
	 * <p>
	 * @return  ArrayList<String> al_stepName
	 */
	public ArrayList<String> getStepNames() {
		ArrayList<String> al_stepName = null;
		JclMvsEntry entryJcl = null;
		String stepName = "";
		
		al_stepName = new ArrayList<String>();
		
		// Scan entries jcl
		for (int i = 0; i < this.al_entryJcl.size(); i++) {
			entryJcl = this.al_entryJcl.get(i);
			
			// Interessano solo le exec
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			
			stepName = entryJcl.getInstruction().jclStmtGetName();
			
			// Nome stepdefinito oppure counter costruito
			if (stepName.equals("")) {
				al_stepName.add("#" + i + "#");
			} else {
				al_stepName.add(stepName);
			}
		}
		return al_stepName;
	}

	/**
	 * Restituisce l'entry in cui è codificato lo step presente nel jcl, identificato dal nome<br>
	 * <p>
	 * Viene restituita una istanza dell'oggetto {@link JclMvsEntry},
	 * che contiene tutte le informazioni sullo step quali nome, istruzione
	 * di exec, elenco istruzioni dd etc.<br>
	 * <p>
	 * Se lo step non è presente viene restituito null.<br>
	 * <p>
	 * @return  JclMvsEntry step
	 */
	public JclMvsEntry getStepEntry(String stepName) {
		JclMvsEntry entryJcl = null;
		String stepNameLoop = "";
		
		// Scan entries jcl
		for (int i = 0; i < this.al_entryJcl.size(); i++) {
			entryJcl = this.al_entryJcl.get(i);
			
			// Interessano solo le exec
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			
			stepNameLoop = entryJcl.getInstruction().jclStmtGetName();
			
			// Non è lo step richiesto
			if (!stepName.equals(stepNameLoop)) {
				continue;
			}
			return entryJcl;
		}
		return null;
	}

	/**
	 * Restituisce l'entry in cui è codificato lo step presente nel jcl, identificato dal numero 1-based.<br>
	 * <p>
	 * Il numero di step deve essere 1-based.<br>
	 * Viene restituita una istanza dell'oggetto {@link JclMvsEntry},
	 * che contiene tutte le informazioni sullo step quali nome, istruzione
	 * di exec, elenco istruzioni dd etc.<br>
	 * <p>
	 * Se lo step non è presente viene restituito null.<br>
	 * <p>
	 * @return  JclMvsEntry step
	 */
	public JclMvsEntry getStepEntry(int stepNumber) {
		
		JclMvsEntry entryJcl = null;
		int stepNameLoop = 0;
		
		// Scan entries jcl
		for (int i = 0; i < this.al_entryJcl.size(); i++) {
			entryJcl = this.al_entryJcl.get(i);
			
			// Interessano solo le exec
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			
			stepNameLoop++;
			
			// Non è lo step richiesto
			if (stepNameLoop < stepNumber) {
				continue;
			}
			return entryJcl;
		}
		return null;
	}


	
	/**
	 * Restituisce tutte le istruzioni presenti nel jcl<br>
	 * <p>
	 * Viene restituito un ArrayList con le istruzioni presenti a partire dalla scheda job.<br>
	 * Le tipologie di istruzioni sono codificate in {@link EnumInstrDataCategory}.<br>
	 * L'eventuale proc embedded, presente prima del corpo jcl vero e proprio,
	 * non viene restituita.<br>
	 * <p>
	 * @return   ArrayList<InstructionJclMvs> al_instrJcl
	 */
	public ArrayList<InstructionJclMvs> getJclInstr() {
		JclMvsEntry entryJcl = null;
		ArrayList<InstructionJclMvs> al_instruction = null;
		
		al_instruction = new ArrayList<InstructionJclMvs>();
		
		// Scan entries contenenti istruzioni jcl
		for (int i = 0; i < this.al_entryJcl.size(); i++) {
			entryJcl = this.al_entryJcl.get(i);
			al_instruction.add(entryJcl.getInstruction());
		}
		return al_instruction;
	}

	/**
	 * Restituisce tutte gli entry contenenti istruzioni presenti nel jcl, in un determinato step
	 * del quale viene fornito il nome<br>
	 * <p>
	 * Viene restituito un ArrayList con gli entry presenti nel solo step richiesto.<br>
	 * Le tipologie di istruzioni sono codificate in {@link EnumInstrDataCategory}.<br>
	 * Non viene restituito l'entry relativo allo step stesso, Exec<br>
	 * Se lo step è inesistente viene restituito null.<br>
	 * <p>
	 * @return   ArrayList<JclMvsEntry> al_entryJcl
	 */
	public ArrayList<JclMvsEntry> getStepEntries(String stepName) {
		ArrayList<JclMvsEntry> al_entry = null;
		JclMvsEntry entryJcl = null;
        int stepNumDef = 0;
		
        al_entry = new ArrayList<JclMvsEntry>();
        
		// Recupero il numero di definizione dello step
		stepNumDef = this.getStepNumDef(stepName);
		if (stepNumDef == -1) {
			return null;
		}
		
		entryJcl = this.getJclEntry(stepNumDef);
		al_entry.add(entryJcl);
		
		// Scan entries jcl a partire dallo step
		for (int i = stepNumDef + 1; i < this.al_entryJcl.size(); i++) {
			
			entryJcl = this.al_entryJcl.get(i);
			
			// Step successivo: break
			if (entryJcl.getInstruction().getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_STEP) {
				break;
			}
			// Append statement in step
			al_entry.add(entryJcl);
		}
		
		return al_entry;
	}
	
	/**
	 * Restituisce tutte gli entry contenenti istruzioni presenti nel jcl, di una proc embedded
	 * di cui è fornito il numero 0-based<br>
	 * <p>
	 * @return   ArrayList<JclMvsEntry> al_entryJcl
	 */
	public ArrayList<JclMvsEntry> getProcEmbeddedEntries(int procNumber) {
		ArrayList<JclMvsEntry> al_entry = null;
		ArrayList<JclMvsEntry> al_entryProcEmbedded = null;
		
		al_entry = new ArrayList<JclMvsEntry>();
		al_entryProcEmbedded = this.al_procEmbedded.get(procNumber);
		
		// Scan contenitori istruzioni proc embedded
		for (JclMvsEntry entryProcEmbedded : al_entryProcEmbedded) {
			al_entry.add(entryProcEmbedded);
		}
		return al_entry;
	}

	/**
	 * Restituisce tutte gli entry contenenti istruzioni presenti nel jcl, di una proc embedded
	 * di cui è fornito il nome<br>
	 * <p>
	 * Se la proc interna non è definita restituisce null.<br>
	 * <p>
	 * @return  ArrayList<JclMvsEntry> al_entryJcl
	 */
	public ArrayList<JclMvsEntry> getProcEmbeddedEntries(String procName) {
		ArrayList<JclMvsEntry> al_entry = null;
		ArrayList<JclMvsEntry> al_entryProcEmbedded = null;
		int procNumber = 0;
		
		procNumber = this.getProcEmbeddedNumber(procName);
		
		// Proc embedded inesistente
		if (procNumber == -1) {
			return null;
		}
		
		al_entry = new ArrayList<JclMvsEntry>();
		al_entryProcEmbedded = this.al_procEmbedded.get(procNumber);
		
		// Scan contenitori istruzioni proc embedded
		for (JclMvsEntry entryProcEmbedded : al_entryProcEmbedded) {
			al_entry.add(entryProcEmbedded);
		}
		return al_entry;
	}




	
	
	/**
	 * Restituisce tutte gli entry contenenti istruzioni presenti nel jcl, in un determinato step
	 * del quale viene fornito il numero, 1-based<br>
	 * <p>
	 * Viene restituito un ArrayList con gli entry presenti nel solo step richiesto.<br>
	 * Le tipologie di istruzioni sono codificate in {@link EnumInstrDataCategory}.<br>
	 * Non viene restituito l'entry relativo allo step stesso, Exec<br>
	 * Se lo step è inesistente viene restituito null.<br>
	 * <p>
	 * @return   ArrayList<JclMvsEntry> al_entryJcl
	 */
	public ArrayList<JclMvsEntry> getStepEntries(int stepNumber) {
		ArrayList<JclMvsEntry> al_entry = null;
		JclMvsEntry entryJcl = null;
	    int stepNumDef = -1;
	    int stepCnt = 0;
		
        al_entry = new ArrayList<JclMvsEntry>();
        
        // Ricerca lo step i-esimo richiesto
		// Scan entries jcl a partire dallo step
		for (int i = stepNumDef + 1; i < this.al_entryJcl.size(); i++) {
			
			entryJcl = this.al_entryJcl.get(i);
			
			// Step successivo: break
			if (entryJcl.getInstruction().getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_STEP) {
				stepCnt++;
			}
			
			// Non è lo step richiesto
			if (stepCnt < stepNumber) {
				continue;
			}
			
			// Step cercto: break
			stepNumDef = i;
			break;
		}
        
		// Step non trovato
		if (stepNumDef == -1) {
			return null;
		}
		
		// Statement exec in output
		al_entry.add(entryJcl);
		
		// Scan entries jcl a partire dallo statement successivo 
		for (int i = stepNumDef + 1; i < this.al_entryJcl.size(); i++) {
			
			entryJcl = this.al_entryJcl.get(i);
			
			// Step successivo: break
			if (entryJcl.getInstruction().getTypeInstrJcl() == EnumInstrDataCategory.JCL_MVS_STEP) {
				break;
			}
			// Append statement in step
			al_entry.add(entryJcl);
		}
		
		return al_entry;
	}

	/**
	 * Restituisce l'entry contenente l'istruzione descrivente lo step del quale viene fornito il numero 1-based<br>
	 * <p>
	 * Viene restituito un oggetto {@link JclMvsEntry} relativo all'istruzione Exec di programma o proc.<br>
	 * Le tipologie di istruzioni sono codificate in {@link EnumInstrDataCategory}.<br>
	 * Se lo step è inesistente viene restituito null.<br>
	 * <p>
	 * @return   JclMvsEntry step instruction  
	 */
	public JclMvsEntry getStepInstr(int stepNumber) {
		JclMvsEntry JclMvsEntry = null;
		ArrayList<JclMvsEntry> al_entryJcl = null;
		
		al_entryJcl = this.getStepEntries(stepNumber);
		
		// Step richiesto inesistente
		if (al_entryJcl == null) {
			return null;
		}
		
		// Istruzione //stepname EXEC PGM=xx, è la prima istruzione dello step
		JclMvsEntry = al_entryJcl.get(0);
		
		return JclMvsEntry;
	}

		
	/**
	 * Restituisce il nome del programma richiamato nello step nella scheda // EXEC<br>
	 * <p>
	 * Se lo step è inesistente viene restituito null.<br>
	 * <p>
	 * @return  String pgm or proc executed 
	 */
	public String getExecName(String stepName) {
		JclMvsEntry entryJcl = null;
		String execName = null;					// Pgm o proc
		
		entryJcl = this.getStepEntry(stepName);
		
		// Step richiesto inesistente
		if (entryJcl == null) {
			return null;
		}
		
		// Istruzione //stepname EXEC PGM=xx
		execName = entryJcl.getInstruction().jclStepGetExecName();
		
		return execName;
	}

	/**
	 * Restituisce il nome fisico dei file, ovvero il dsname, relativo
	 * alla ddname e step forniti.<br>
	 * <p>
	 * Viene restituito il dsname principale e tutti quelli concatenari.<br>
	 * Se lo step è inesistente viene restituito null.<br>
	 * Se la ddname è inesistente viene restituito un array list vuoto.<br>
	 * <p>
	 * @return  ArrayList<String> al_dsname
	 */
	public ArrayList<String> getDsname(String stepName, String ddName) {
		
		ArrayList<String> al_dsname = null;
		ArrayList<JclMvsEntry> al_entryStep = null;
		InstructionJclMvs instrDD = null;
		int i = 0;
		
		al_entryStep = this.getStepEntries(stepName);
		
		// Step inesistente
		if (al_entryStep == null) {
			return null;
		}
		
		al_dsname = new ArrayList<String>();
		
		// Scan statement nello step fino alla DD name richiesta
		for (i = 0; i < al_entryStep.size(); i++) {
			if (al_entryStep.get(i).getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {continue;}
			if (!al_entryStep.get(i).getInstruction().jclStmtGetName().equals(ddName)) {continue;}
			// DDName individuata
			break;
		}
		
		// DDName non presente nello step fornito: exit
		if (i >= al_entryStep.size()) {
			return al_dsname;
		}
		
		// Scan istruzioni componenti la DD
		for (; i < al_entryStep.size(); i++) {
			
			instrDD = al_entryStep.get(i).getInstruction();
			
			// Condizione di fine
			if (!instrDD.jclStmtGetName().equals("")
			&&  !instrDD.jclStmtGetName().equals(ddName)) {
				break;
			}
			
			// Si tratta della DD name principale o di una concatenata
			// Accodo statement da portare in output
			al_dsname.add(instrDD.jclStmtGetParm("DSNAME"));
		}
		
		return al_dsname;
	}

	/**
	 * Restituisce true se presente la ddname nello step fornito
	 * <p>
	 * Se lo step è inesistente viene restituito null.<br>
	 * Se la ddname è inesistente viene restituito false.<br>
	 * <p>
	 * @return  boolean isThereDDName
	 */
	public Boolean isThereDDName(String stepName, String ddName) {
		
		ArrayList<JclMvsEntry> al_entryStep = null;
		int i = 0;
		
		al_entryStep = this.getStepEntries(stepName);
		
		// Step inesistente
		if (al_entryStep == null) {
			return null;
		}
		
		// Scan statement nello step fino alla DD name richiesta
		for (i = 0; i < al_entryStep.size(); i++) {
			if (al_entryStep.get(i).getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {continue;}
			if (!al_entryStep.get(i).getInstruction().jclStmtGetName().equals(ddName)) {continue;}
			// DDName individuata
			break;
		}
		
		// DDName non presente nello step fornito: exit
		if (i >= al_entryStep.size()) {
			return false;
		}
				
		return true;
	}

	/**
	 * Restituisce true se presente la ddname in qualche step del jcl.<br>
	 * <p>
	 * Se la ddname è inesistente viene restituito false.<br>
	 * <p>
	 * @return  boolean isThereDDName
	 */
	public Boolean isThereDDName(String ddName) {
		
		boolean isThereDDName = false;
		
		// Scan step definiti nel jcl
		for (JclMvsEntry entryJcl : this.al_entryJcl) {
			if (entryJcl.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {continue;}
			if (!entryJcl.getInstruction().jclStmtGetName().equals(ddName)) {continue;}
			
			// DDName individuata
			isThereDDName = true;
			break;
		}
		return isThereDDName;
	}

	
	/**
	 * Restituisce il nome del programma richiamato nello step nella scheda // EXEC 
	 * del quale viene fornito il numero 1-based<br>
	 * <p>
	 * Se lo step è inesistente viene restituito null.<br>
	 * <p>
	 * @return  String pgm or proc executed 
	 */
	public String getExecName(int stepNumber) {
		JclMvsEntry entryJcl = null;
		String execName = null;					// Pgm o proc
		
		entryJcl = this.getStepEntry(stepNumber);
		
		// Step richiesto inesistente
		if (entryJcl == null) {
			return null;
		}
		
		// Istruzione //stepname EXEC PGM=xx
		execName = entryJcl.getInstruction().jclStepGetExecName();
		
		return execName;
	}


	/**
	 * Restituisce i nomi di tutti i programmi richiamati negli step definiti.<br>
	 * <p>
	 * Si tratta dei programmi di ogni scheda // EXEC PGM=xx<br>
	 * <p>
	 * @return  ArrayList<String> al_execPgmName
	 */
	public ArrayList<String> getExecPgmNames() {
		ArrayList<String> al_execPgmName = null;
		String execName = "";
		
		al_execPgmName = new ArrayList<String>();
		
		// Scan step presenti nel jcl
		for (JclMvsEntry entryJcl : this.al_entryJcl) {
	
			// Non è uno step di exec: skip
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			// Non è una exec di un programma: skip
			if (!entryJcl.getInstruction().jclStepIsExecPgm()) {
				continue;
			}
			
			execName = entryJcl.getInstruction().jclStepGetExecName();
			al_execPgmName.add(execName);
			break;
		}
		return al_execPgmName;
	}


	/**
	 * Restituisce i nomi di tutte le proc richiamate negli step definiti.<br>
	 * <p>
	 * Si tratta delle proc di ogni scheda // EXEC procname<br>
	 * <p>
	 * @return  ArrayList<String> al_execProcName
	 */
	public ArrayList<String> getExecProcNames() {
		ArrayList<String> al_execProcName = null;
		String execName = "";
		
		al_execProcName = new ArrayList<String>();
		
		// Scan step presenti nel jcl
		for (JclMvsEntry entryJcl : this.al_entryJcl) {
	
			// Non è uno step di exec: skip
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			// Non è una exec di una proc: skip
			if (entryJcl.getInstruction().jclStepIsExecPgm()) {
				continue;
			}
			
			execName = entryJcl.getInstruction().jclStepGetExecName();
			al_execProcName.add(execName);
			break;
		}
		return al_execProcName;
	}

	/**
	 * Restituisce il numero dello step 0-based, fornito il suo nome.<br>
	 * <p>
	 * Vengono esaminati gli step nella sequenza in cu sono presenti nel jcl<br>
	 * Se non viene trovato nessuno step con il nome fornito, si restituisce -1.
	 * <p>
	 * @param String stepName
	 * @return int stepNumer
	 */
	public int getStepNumDef(String stepName) {
		JclMvsEntry entryJcl = null;
		String stepNameLoop = "";
		
		// Scan entries jcl
		for (int i = 0; i < this.al_entryJcl.size(); i++) {
			entryJcl = this.al_entryJcl.get(i);
			
			// Interessano solo le exec
			if (entryJcl.getInstruction().getTypeInstrJcl() != EnumInstrDataCategory.JCL_MVS_STEP) {
				continue;
			}
			
			stepNameLoop = entryJcl.getInstruction().jclStmtGetName();
			
			// Non è lo step richiesto
			if (!stepName.equals(stepNameLoop)) {
				continue;
			}
			
			// E' lo step richiesto
			return i;
		}
		return -1;
	}


	/**
	 * Restituisce i nomi di tutte le ddname del jcl.<br>
	 * <p>
	 * Vengono esaminati tutti gli step ed eliminati i duplicati<br>
	 * <p>
	 * @return  ArrayList<String> al_ddnames
	 */
	public ArrayList<String> getDDNames() {
		ArrayList<String> al_ddName = null;
		ArrayList<String> al_ddNameNoDuplicate = null;
		Set<String> set_ddName = null;
		
		al_ddName = new ArrayList<String>();
		al_ddNameNoDuplicate = new ArrayList<String>();
		set_ddName = new HashSet<String>();
		
		// Scan entries contenenti statement jcl
		for (JclMvsEntry entryStep : this.al_entryJcl) {
			
			// Interessano solo le schede dd
			if (entryStep.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
			// Interessano solo le DD con il nome esterno, non concatenate
			if (entryStep.getInstruction().jclDDIsChained()) {
				continue;
			}
			
			// Sicuramente presente il nome esterno della dd
			al_ddName.add(entryStep.getInstruction().jclStmtGetName());
		}
		
		// Eliminazione eventuali duplicati
		set_ddName.addAll(al_ddName);
		al_ddNameNoDuplicate.addAll(set_ddName);
		
		return al_ddNameNoDuplicate;
	}

	/**
	 * Restituisce i nomi di tutte le ddname di uno step del jcl.<br>
	 * <p>
	 * Se lo step è inesistente viene restituito null<br>
	 * <p>
	 * @return  ArrayList<String> al_ddnames
	 */
	public ArrayList<String> getDDNames(String stepName) {

		ArrayList<JclMvsEntry> al_entryStep = null;
		ArrayList<String> al_ddName = null;

		al_ddName = new ArrayList<String>();
		al_entryStep = this.getStepEntries(stepName);
		
		// Step inesistente
		if (al_entryStep == null) {
			return null;
		}
		
		// Scan statement nello step
		for (JclMvsEntry entryStep : al_entryStep) {
			
			// Interessano solo le schede dd
			if (entryStep.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
			// Interessano solo le DD con il nome esterno, non concatenate
			if (entryStep.getInstruction().jclDDIsChained()) {
				continue;
			}
			
			// Sicuramente presente il nome esterno della dd
			al_ddName.add(entryStep.getInstruction().jclStmtGetName());
		}
		return al_ddName;
	}

	/**
	 * Restituisce tutte le istruzioni DD in uno step del jcl.<br>
	 * <p>
	 * Se lo step è inesistente viene restituito null<br>
	 * <p>
	 * @return  ArrayList<InstructionJclMvs> al_ddInstr
	 */
	public ArrayList<InstructionJclMvs> getDDInstructions(String stepName) {

		ArrayList<InstructionJclMvs> al_ddInstr = null;
		ArrayList<JclMvsEntry> al_entryStep = null;
		
		al_entryStep = this.getStepEntries(stepName);
		
		// Step inesistente
		if (al_entryStep == null) {
			return null;
		}

		al_ddInstr = new ArrayList<InstructionJclMvs>();


		// Scan statement nello step
		for (JclMvsEntry entryStep : al_entryStep) {
			
			// Interessano solo gli statement DD
			if (entryStep.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
			// Accodo statement da portare in output
			al_ddInstr.add(entryStep.getInstruction());
		}
		return al_ddInstr;
	}

	/**
	 * Restituisce l'istruzioni DD codificata in uno step del jcl.<br>
	 * <p>
	 * Se lo step è inesistente viene restituito null<br>
	 * Se la ddname è inesistente viene restituito null<br>
	 * <p>
	 * @return  InstructionJclMvs ddInstr
	 */
	public InstructionJclMvs getDDInstruction(String stepName, String ddname) {
		InstructionJclMvs ddInstr = null;
		ArrayList<JclMvsEntry> al_entryStep = null;
		
		al_entryStep = this.getStepEntries(stepName);
		
		// Step inesistente
		if (al_entryStep == null) {
			return null;
		}
		
		// Scan statement nello step
		for (JclMvsEntry entryStep : al_entryStep) {
			// Interessano solo gli statement DD
			if (entryStep.getTypeInstr() != EnumInstrDataCategory.JCL_MVS_DD) {
				continue;
			}
			
			// Interessano solo le DD con il nome esterno, non concatenate
			if (entryStep.getInstruction().jclDDIsChained()) {
				continue;
			}

			// Non è la ddname cercata: skip
			if (!entryStep.getInstruction().jclStmtGetName().equals(ddname)) {
				continue;
			}
			
			// Individuata ddname nello step richiesto
			ddInstr = entryStep.getInstruction();
			break;
		}
		return ddInstr;
	}

	/**
	 * Accoda l'entry con l'istruzione di una proc embedded<br>
	 * <p>
	 * @param JclMvsEntry entryProcEmbedded
	 */
	public void addEntryProcEmbedded(JclMvsEntry entryProcEmbedded, int numProcEmbedded) {
		ArrayList<JclMvsEntry> al_entryprocEmbedded = null;
		
		// Alla prima richiesta di accodamento creazione ArrayList
		if (numProcEmbedded + 1 == this.al_procEmbedded.size() - 1) {
			al_entryprocEmbedded = new ArrayList<JclMvsEntry>();
			this.al_procEmbedded.add(al_entryprocEmbedded);
		}
		
		// Controllo di sicurezza
		if (numProcEmbedded >= this.al_procEmbedded.size()) {
			return;
		}
		
		// Recupero entries proc embedded
		al_entryprocEmbedded = this.al_procEmbedded.get(numProcEmbedded);

		// Accodo entry 
		al_entryprocEmbedded.add(entryProcEmbedded);
		return;
	}

	/**
	 * Accoda l'entry con l'istruzionejcl<br>
	 * <p>
	 * @param JclMvsEntry entryJcl
	 * @return int index inserted
	 */
	public int addEntry(JclMvsEntry entryJcl) {
		this.al_entryJcl.add(entryJcl);
		return (al_entryJcl.size() - 1);
	}


	
////////////////////////////////////////////////////////////////////////////////////////////
//////////////////Classi interne di servizio usate come strutture dati  /////////////////// 
////////////////////////////////////////////////////////////////////////////////////////////



	
/*
 * 
 * Classe contenitore con le informazioni relative a uno step di esecuzione.
 * 
 * 
 */
public class InnerJclStep {
	
	String stepName = "";   						    // Nome step
	
	// Entry contenente Istruzione che descrive lo step di esecuzione, con l'exec a pgm/proc e i parametri passati
	JclMvsEntry entryStep = null;
	
	// Istruzioni che qualificano lo step.
	// Si tratta di DD ma possono essere anche Include o altro
	ArrayList<JclMvsEntry> al_entryStep = null;
 
	
    /*
     * Costruttore
     */
	public InnerJclStep() {
	}




	
	
}







}
