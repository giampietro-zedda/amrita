package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


/**
 * 
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionJclMvs
 * </h1>
 *  <p>
 * Questa classe modella una generica istruzione jcl Mvs come descritto in {@link EnumInstrJclType} <br>
 * Per ogni tipologia di istruzione sono previsti metodi per inserire e recuperare le informazioni
 * chiave, attraverso i metodi generici ereditati da {@link Instruction}.<br>
 * Le informazioni sulle righe sorgente relative all'istruzione e i commenti, sono gestite 
 * in modo generale, per istruzioni di qualsiasi tipo, dalla classe {@link Instruction}, che
 * definisce anche le strutture parametro/valore per l'istruzione.
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/01/2011
 * @see Instruction
 * 
 * 
*/
public class InstructionJclMvs extends Instruction implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    													

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionJclMvs()  {
		super();
	}
	
	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStart 		    	Posizione inizio istruzione in riga sorgente
	 *  @param PosEnd    		    	Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  
	 */
	public InstructionJclMvs(int numInstr
						    ,int rowStartSource
						    ,int rowEndSource
						    ,int posStartInstr
						    ,int posEndInstr
					 	    ,String ar_RowsSource[]
				            ,String ar_CommentsBeforeInstr[]
				            ,String ar_CommentsLeftInstr[]                        
				            ,String ar_CommentsRightInstr[]                        
				            ,String name
				            ,String sourceInstr 
			               ) {
		
		super(numInstr
		 	 ,rowStartSource
			 ,rowEndSource
			 ,posStartInstr
			 ,posEndInstr
			 ,ar_RowsSource
			 ,ar_CommentsBeforeInstr
			 ,ar_CommentsLeftInstr
			 ,ar_CommentsRightInstr
			 ,name
			 ,sourceInstr
			 );	
		
	}


	/**
	 * 
	 * Restituisce il nome della proc o null se non presente<br> 
	 * <p>
	 * @return the proc name
	 */
	public String jclProcGetName() {
		String procName = "";
		procName = (String) this.getMapDescriptorObject("$JCL-STMT$NAME$");
		return procName;
	}

	/**
	 * 
	 *  Imposta il nome della proc<br> 
	 * <p>
	 * @param String procName
	 */
	public void jclProcPutName(String procName) {
		addMapDescriptorObject("$JCL-STMT$NAME$", procName);
		return;
	}


	/**
	 * 
	 * Restituisce il valore di un parametro presente nella scheda jcl<br> 
	 * <p>
	 * Se il parametro non è presente restituisce null.
	 * <p>
	 * @param String parmName
	 * @return String parmValue
	 */
	public String jclStmtGetParm(String parmName) {
		String parmValue = "";
		parmValue = (String) this.getMapDescriptorObject("$JCL-STMT$PARM"+parmName);
		return parmValue;
	}

	/**
	 * 
	 * Restituisce l'elenco dei nomi dei parametri presenti nella scheda DD<br> 
	 * <p>
	 * @return ArrayList<String> al_parmNames
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> jclStmtGetParmNames() {
		ArrayList<String> al_parmNames = null;
		al_parmNames = (ArrayList<String>) this.getMapDescriptorObject("$JCL-STMT$PARM-LIST");
		if (al_parmNames == null) {
			al_parmNames = new ArrayList<String>();
			al_parmNames.trimToSize();
		}
		return al_parmNames;
	}
	

	/**
	 * 
	 * Imposta il valore di un parametro presente nella scheda jcl<br> 
	 * <p>
	 * @param String parmName
	 * @param String parmValue
	 */
	@SuppressWarnings("unchecked")
	public void jclStmtPutParm(String parmName, String parmValue) {
		ArrayList<String> al_jclStmtParmNames = null;
		this.addMapDescriptorObject("$JCL-STMT$PARM"+parmName, parmValue);
		al_jclStmtParmNames = (ArrayList<String>) this.getMapDescriptorObject("$JCL-STMT$PARM-LIST");
		if (al_jclStmtParmNames == null) {
			al_jclStmtParmNames = new ArrayList<String>();
			this.addMapDescriptorObject("$JCL-STMT$PARM-LIST", al_jclStmtParmNames);
		}
		al_jclStmtParmNames.add(parmName);
		return;
	}


	/**
	 * 
	 * Restituisce il nome dello statemnt jcl codificato subito dopo la //<br> 
	 * <p>
	 * Si tratta del nome codificato in:<br>
	 * <tt>//Name JOB</tt><br>
	 * <tt>//Name JOBLIB</tt><br>
	 * <tt>//Name DD</tt><br>
	 * <tt>//StepName.ddname DD</tt><br>
	 * <tt>//ProcStep.StepName.ddname DD</tt><br>
	 * <tt>//StepName EXEC PGM=AA</tt><br>
	 * <tt>....</tt><br>
	 * Se il nome non è presente restituisce una stringa vuota.
	 * <p>
	 * @return String jclStmtName
	 */
	public String jclStmtGetName() {
		String jclStmtName = "";
		jclStmtName = (String) this.getMapDescriptorObject("$JCL-STMT$NAME$");
		if (jclStmtName == null) {
			jclStmtName = "";
		}
		return jclStmtName;
	}
	
	/**
	 * Imposta il nome dello statement jcl codificato subito dopo la //<br> 
	 * <p>
	 * Si tratta del nome codificato in:<br>
	 * <tt>//Name JOB</tt><br>
	 * <tt>//Name JOBLIB</tt><br>
	 * <tt>//Name DD</tt><br>
	 * <tt>//StepName EXEC PGM=AA</tt><br>
	 * tt>....</tt><br>
	 * <p>
	 * @param String jclStmtName
	 */
	public void jclStmtPutName(String jclStmtName) {
		this.addMapDescriptorObject("$JCL-STMT$NAME$", jclStmtName);
		return;
	}


	
	/**
	 * 
	 * Restituisce il nome del programma o della proc eseguita nello step<br> 
	 * <p>
	 * Se il parametro non è presente restituisce una stringa vuota.
	 * <p>
	 * @return String stepExecName
	 */
	public String jclStepGetExecName() {
		String stepExecName = "";
		stepExecName = (String) this.getMapDescriptorObject("$STEP$EXEC-NAME");
		if (stepExecName == null) {
			stepExecName = "";
		}
		return stepExecName;
	}
	/**
	 * 
	 * Restituisce true se lo step è relativo alla esecuzione di un programma
	 * e non di una proc.<br> 
	 * <p>
	 * @returnboolean isExecPgm
	 */
	public boolean jclStepIsExecPgm() {
		if (this.getMapDescriptorObject("$STEP$IS-EXEC-PGM") == null) {
			return false;
		}
		Boolean isExecPgm = (Boolean) this.getMapDescriptorObject("$STEP$IS-EXEC-PGM");
		return isExecPgm;
	}

	/**
	 * 
	 * Imposta se lo step è relativo alla esecuzione di un programma
	 * e non di una proc.<br> 
	 * <p>
	 * @param boolean isExecPgm
	 */
	public void jclStepSetExecPgm(Boolean isExecPgm) {
		this.addMapDescriptorObject("$STEP$IS-EXEC-PGM", isExecPgm); 
		return;
	}

	/**
	 * 
	 * Imposta il nome del programma o della proc eseguita nello step<br> 
	 * <p>
	 * @param String stepExecName
	 */
	public void jclStepPutExecName(String stepExecName) {
		this.addMapDescriptorObject("$STEP$EXEC-NAME", stepExecName);
		return;
	}
	/**
	 * 
	 * Imposta il nome dello step della proc di cui la dd è in override<br>  
	 * <p>
	 * Come <tt>//ProcStepName.ddname DD DSN=...
	 * @param String procStepName
	 */
	public void jclDDPutOverrideProcStepName(String procStepName) {
		addMapDescriptorObject("$DD$OVERRIDE-STEPNAME", procStepName);
		return;
	}
	/**
	 * 
	 * Imposta il nome della dd è in override<br>  
	 * <p>
	 * Come <tt>//ProcStepName.ddname DD DSN=...
	 * @param String ddname
	 */
	public void jclDDPutOverrideDDName(String procStepName) {
		addMapDescriptorObject("$DD$OVERRIDE-DDNAME", procStepName);
		return;
	}
	
	/**
	 * 
	 * Restituisce il nome della dd è in override<br>  
	 * <p>
	 * Come <tt>//ProcStepName.ddname DD DSN=...<br>
	 * Se non c'è override di proc restituisce stringa vuota.
	 * @return String ddname
	 */
	public String jclDDGetOverrideDDName() {
		String val = "";
		val = (String) getMapDescriptorObject("$DD$OVERRIDE-DDNAME");
		if (val == null) {
			val = "";
		}
		return val;
	}
	
	/**
	 * 
	 * Restituisce il nome dello step della proc di cui la dd è in override<br>  
	 * <p>
	 * Come <tt>//ProcStepName.ddname DD DSN=...<br>
	 * Se non c'è override di proc restituisce stringa vuota.
	 * @return String procStepName
	 */
	public String jclDDGetOverrideProcStepName() {
		String val = "";
		val = (String) getMapDescriptorObject("$DD$OVERRIDE-STEPNAME");
		if (val == null) {
			val = "";
		}
		return val;
	}
	

	/**
	 * 
	 * Restituisce la libreria da cui la dd, un sysin, preleva i dati<br> 
	 * <p>
	 * @return the ddSysinLibrary
	 */
	public String jclDDGetSysinLibrary() {
		String ddSysinLibrary = "";
		ddSysinLibrary = (String) this.getMapDescriptorObject("$DD$SYSIN-LIB");
		return ddSysinLibrary;
	}

	/**
	 * 
	 *  Imposta la libreria da cui la dd, un sysin, preleva i dati<br> 
	 * <p>
	 * @param String ddSysinLibrary
	 */
	public void jclDDPutSysinLibrary(String ddSysinLibrary) {
		addMapDescriptorObject("$DD$SYSIN-LIB", ddSysinLibrary);
		return;
	}
	/**
	 * 
	 * Restituisce il membro di libreria da cui la dd, un sysin, preleva i dati<br> 
	 * <p>
	 * @return the ddSysinLibraryMember
	 */
	public String jclDDGetSysinLibraryMember() {
		String ddSysinLibraryMember = "";
		ddSysinLibraryMember = (String) this.getMapDescriptorObject("$DD$SYSIN-LIB-MEMBER");
		return ddSysinLibraryMember;
	}

	/**
	 * 
	 * Imposta il membro di libreria da cui la dd, un sysin, preleva i dati<br> 
	 * <p>
	 * @param String ddName
	 */
	public void jclDDPutSysinLibraryMember(String ddSysinLibraryMember) {
		addMapDescriptorObject("$DD$SYSIN-LIB-MEMBER", ddSysinLibraryMember);
		return;
	}
	
	/**
	 * 
	 * Restituisce le schede embedded di sysin esclusa l'ultima di
	 * chiusura con il delimiter /* o custom<br> 
	 * <p>
	 * @return ArrayList<String> al_sysinCard
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> jclDDGetSysinCards() {
		ArrayList<String> al_sysinCard = null;
		al_sysinCard =  (ArrayList<String>) this.getMapDescriptorObject("$DD$SYSIN-CARDS");
		return al_sysinCard;
	}

	/**
	 * 
	 * Imposta le schede embedded di sysin esclusa l'ultima di
	 * chiusura con il delimiter /* o custom<br> 
	 * <p>
	 * @param ArrayList<String> al_sysinCard
	 */
	@SuppressWarnings("unchecked")
	public void jclDDSetSysinCards(ArrayList<String> al_sysinCard ) {
		al_sysinCard =  (ArrayList<String>) this.addMapDescriptorObject("$DD$SYSIN-CARDS", al_sysinCard);
		return;
	}

	/**
	 * 
	 * Inserisce una scheda embedded di sysin<br> 
	 * <p>
	 * @param String sysinCard
	 */
	@SuppressWarnings("unchecked")
	public void jclDDPutSysinCard(String sysinCard) {
		ArrayList<String> al_sysinCard = null;
		al_sysinCard =  (ArrayList<String>) this.getMapDescriptorObject("$DD$SYSIN-CARDS");
		if (al_sysinCard == null) {
		    al_sysinCard = new ArrayList<String>();
		    this.addMapDescriptorObject("$DD$SYSIN-CARDS", al_sysinCard);
		}
		al_sysinCard.add(sysinCard);
		return;
	}


	
	
	/**
	 * 
	 * Restituisce il nome della dd specificato nello statement DD<br> 
	 * <p>
	 * il nome della dd può essere espresso da:
	 * <tt>//ddname DD ...</tt>
	 * <tt>//ProcStepName.ddname DD ...</tt>
	 *<p>
	 * @return the ddName
	 */
	public String jclDDGetName() {
		String ddName = "";
		ddName = (String) this.getMapDescriptorObject("$DD$NAME");
		return ddName;
	}

	/**
	 * Imposta il nome della dd specificato nello statement DD<br> 
	 * <p>
	 * il nome della dd può essere espresso da:
	 * <tt>//ddname DD ...</tt>
	 * <tt>//ProcStepName.ddname DD ...</tt>
	 *<p>
	 * @param String ddName
	 */
	public void jclDDPutName(String ddName) {
		addMapDescriptorObject("$DD$NAME", ddName);
		return;
	}

	/**
	 * 
	 * Restituisce true se la ddname identifica un GDG<br> 
	 * <p>
	 * @return booleam isGdg
	 */
	public boolean jclDDIsGdg() {
		Boolean isGdg = null;
		isGdg = (Boolean) this.getMapDescriptorObject("$DD$IS-GDG");
		if (isGdg == null) {
			return false;
		}
		return isGdg;
	}

	/**
	 * 
	 * Imposta se la ddname identifica un GDG<br> 
	 * <p>
	 * @param booleam isGdg
	 */
	public void jclDDSetIsGdg(boolean isGdg) {
		addMapDescriptorObject("$DD$IS-GDG", isGdg);
		return;
	}

	/**
	 * 
	 * Restituisce true se la ddname e' una ddname concatenata<br> 
	 * <p>
	 * @return booleam isChained
	 */
	public boolean jclDDIsChained() {
		Boolean isChained = null;
		isChained = (Boolean) this.getMapDescriptorObject("$DD$IS-CHAINED");
		if (isChained == null) {
			return false;
		}
		return isChained;
	}

	/**
	 * 
	 * Restituisce true se l'istruzione jcl descrive il comando jes2 fornito<br> 
	 * <p>
	 * @param String jes2Command
	 * @return booleam isJes2Command
	 */
	public boolean jclIsJes2Command(String jes2Command) {
		Boolean isJes2Command = null;
		isJes2Command = (Boolean) this.getMapDescriptorObject("$JES2$"+jes2Command);
		if (isJes2Command == null) {
			return false;
		}
		return isJes2Command;
	}

	/**
	 * 
	 * Imposta se l'istruzione jcl descrive il comando jes2 fornito<br> 
	 * <p>
	 * @param String jes2Command
	 * @param boolean isJes2Command
	 */
	public void jclSetJes2Command(String jes2Command, boolean isJes2Command) {
		addMapDescriptorObject("$JES2$"+jes2Command, isJes2Command);
		return;
	}

	/**
	 * 
	 * Restituisce true se l'istruzione jcl descrive il comando jes3 fornito<br> 
	 * <p>
	 * @param String jes2Command
	 * @return booleam isJes2Command
	 */
	public boolean jclIsJes3Command(String jes3Command) {
		Boolean isJes3Command = null;
		isJes3Command = (Boolean) this.getMapDescriptorObject("$JES3$"+jes3Command);
		if (isJes3Command == null) {
			return false;
		}
		return isJes3Command;
	}

	/**
	 * 
	 * Imposta se l'istruzione jcl descrive il comando jes3 fornito<br> 
	 * <p>
	 * @param String jes3Command
	 * @param boolean isJes3Command
	 */
	public void jclSetJes3Command(String jes3Command, boolean isJes3Command) {
		addMapDescriptorObject("$JES3$"+jes3Command, isJes3Command);
		return;
	}

	/**
	 * 
	 * Imposta se la ddname e' una ddname concatenata o meno<br> 
	 * <p>
	 * @param booleam isChained
	 */
	public void jclDDSetChained(boolean isChained) {
		addMapDescriptorObject("$DD$IS-CHAINED", isChained);
		return;
	}

	/**
	 * 
	 * Restituisce true se la ddname è ovverride di un'altra DD<br> 
	 * <p>
	 * Si tratta di <tt>//Stepname.DDName DD .... </tt><br>
	 * @return boolean isOverride
	 */
	public boolean jclDDIsOverride() {
		Boolean isOverride = null;
		isOverride = (Boolean) this.getMapDescriptorObject("$DD$IS-OVERRIDE");
		if (isOverride == null) {
			return false;
		}
		return isOverride;
	}

	/**
	 * Imposta true se la ddname è ovverride di un'altra DD<br> 
	 * <p>
	 * Si tratta di <tt>//Stepname.DDName DD .... </tt><br>
	 * <p>
	 * @param booleam isOverride
	 */
	public void jclDDSetOverride(boolean isOverride) {
		addMapDescriptorObject("$DD$IS-OVERRIDE", isOverride);
		return;
	}

	/**
	 * 
	 * Imposta ddname alla quale la ddname corrente è concatenatata<br> 
	 * <p>
	 * @param booleam isChained
	 */
	public void jclDDPurChainedDDNameTo(String ddnameChainedTo) {
		addMapDescriptorObject("$DD$CHAINED-TO", ddnameChainedTo);
		return;
	}

	/**
	 * 
	 * Restituisce ddname alla quale la ddname corrente è concatenatata<br> 
	 * <p>
	 * @param booleam isChained
	 */
	public String jclDDGetChainedDDNameTo() {
		String ddnameChainedTo = "";
		ddnameChainedTo = (String) getMapDescriptorObject("$DD$CHAINED-TO");
		return ddnameChainedTo;
	}

	/**
	 * 
	 * Restituisce true se la dd dichiara un input embedded<br> 
	 * <p>
	 * Caso tipico è <code>//SYSIN DD *</code><br>
	 * <p>
	 * @return boolean true se input embedded
	 */
	public boolean jclDDIsInputEmbedded() {
		Boolean isEmbedded = null;
		isEmbedded = (Boolean) this.getMapDescriptorObject("$DD$INP-EMBEDDED");
		if (isEmbedded == null) {
			return false;
		}
		return isEmbedded;
	}
	
	/**
	 * 
	 * Imposta che la dd dichiara un input embedded<br> 
	 * <p>
	 * Caso tipico è <code>//SYSIN DD *</code><br>
	 * <p>
	 * @param boolean isEmbedded
	 */
	public void jclDDSetInputEmbedded(boolean isEmbedded) {
		this.addMapDescriptorObject("$STEP$INP-EMBEDDED", isEmbedded);
	}
	



	/**
	 * 
	 * Restituisce una HashMap con tutti le coppie nome parm/valore
	 * presenti nell'istruzione.<br> 
	 * <p>
	 * @return HashMap<key, val> map_set
	 */
	@SuppressWarnings("unchecked")
	public Map<String, String> jclStmtGetParms() {
		this.getMapDescriptorObject("$MAP$HASH$");
		return (HashMap<String, String>) this.getMapDescriptorObject("$MAP$HASH$");
	}

	/**
	 * 
	 * Imposta una HashMap con tutti le coppie nome parm/valore
	 * presenti nell'istruzione.<br> 
	 * <p>
	 * @param HashMap<key, val> map_set
	 */
	public void jclStmtPutParms(Map<String, String> map_set) {
		this.addMapDescriptorObject("$MAP$HASH$", map_set);
		return;
	}

	
	
	/**
	 * 
	 * Restituisce la condizione completa espressa dallo statement if.<br> 
	 * <p>
	 * @return the jclCondition
	 */
	public String jclIfGetCondition() {
		String jclCondition = "";
		jclCondition = (String) this.getMapDescriptorObject("$IF$CONDITION");
		return jclCondition;
	}

	/**
	 * 
	 * Imposta la condizione completa espressa dallo statement if.<br> 
	 * <p>
	 * @param String jclCondition
	 */
	public void jclIfPutCondition(String jclCondition) {
		addMapDescriptorObject("$IF$CONDITION", jclCondition);
		return;
	}

	
}
