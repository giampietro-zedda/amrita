package analyzer;
import java.io.Serializable;
import enums.EnumInstrDataCategory;
/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * JclMvsEntry 
 * </h1>
 *  <p>
 * Questa classe  modella un generico entry codificato di un sorgente jcl<br>
 * Ogni entry rappresenta un contenitore dove è memorizzata una istruzione jcl,
 * che può essere una scheda job, una dd oppure una include etc., come codificato in {@link EnumInstrJclType}.<br>
 * Vengono censite le informazioni per sapere se uno statement è codificato dentro un modulo source
 * richiamato con <tt>Include</tt> e a quale livello di anndidamento.<br>
 * Istanze di questa classe vengono memorizzate in oggetti {@link JclMvs} come Array di entry.<br>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 24/01/2011
 * @see AnalyzerJclMvs
 * @see JclMvs
 * @see EnumInstrJclType
 */
public class JclMvsEntry implements Cloneable, Serializable {

	private static final long serialVersionUID = 1L;
	

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                                                               //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	// Tipo istruzione
	private EnumInstrDataCategory  typeInstr = null;	  // Ridondante in quanto già nell'istruzione. Evita estrazione e casting
	
	// Tipologia entry nel programma/copy
	private EnumInstrDataCategory entryType = null;	  	  // Jcl nativo, Include, macro statement IF, SET etc
	
	// Istruzione codificata
	private InstructionJclMvs instruction = null;         // Istruzione jcl
	
	// Valide se l'entry è definito dentro altre strutture 
	private String underProcName = "";              	  // Nome Proc sotto cui l'istruzione è definita o da cui è stata espansa
	private String underProcStepName = "";                // Nome step di exec Proc sotto cui l'istruzione è definita o da cui è stata espansa
	private String underPgmStepName = "";              	  // Nome Step di exec Pgm sotto cui l'istruzione è definita  
	private String underIncludeName = "";              	  // Nome Include sotto cui l'istruzione è definita  
	private boolean underInclude = false;              	  // True indica che l'istruzione è definito in una include richiamato in questo jcl
	private boolean underProc = false;              	  // True indica che l'istruzione è definito in una proc espansa in questa jcl, embedded o meno
	private boolean underProcEmbedded = false;            // True indica che l'istruzione è definito in una proc embedded  
	private boolean anyVarToReplace = false;              // True indica istruzione con variabili da sostituire, contenenti &varName. 
	                                                      // La sostituzione di variabili può provocare inserimento e/o eliminazione di nuovi parametri                                                  
	private boolean allVarReplaced = false;               // True indica che tutte le variabili &varName sono state sostituite con un valore. 
	private int levelNestingInclude = 0;               	  // Livello di annidamento del modulo include sotto il quale l'item si trova

	// Numero entry con istruzione Owner
	private int numEntryCondition = 0;                    // Numero entry con macro istruzione condizionale

	// Indicazione di entry sotto condizione
	private boolean underCondition = false;               // True indica entry sotto ramo true/else, evaluate when o search when
	
	// Indicazione di entry aggiunta allo step definito in una proc richiamata
	private boolean addedToStep = false;                  // Per esempio //step3.ddnam9 DD .......  dove ddname9 non era definita

	// Indicazione di entry overidato, ovvero rimpiazzato o accodato nello step definito in una proc richiamata precedente
	private boolean replacedInStep = false;               // Per esempio a fronte di override //step3.ddnam1 DD .......  dove ddname1 è definito 
	
	// Indicazione di statement di override in update o in append
	private boolean override = false;                     // Per esempio //step.ddname          DD .......  dove ddname è definito nello step
	                                                      // oppure      //procStep.step.ddname DD .......  dove ddname è definito nello step di procStep
	
	// Indicazione di statement overridata
	private  boolean overridedUpdate = false;             // Statement aggiornata da statement di override
	private  boolean overridedAppend = false;             // Statement inserita a fronte di statement di override
	
	
	// Indicazione di statement con dsname reference
	private boolean referenceBackward = false;			  // DD statement con DSNAME=*.---
	private boolean referenceForward = false;             // DD statement con DSNAME=ddname
	
	/**
     * 
     * Costruttore vuoto
     * 
     */
 	public JclMvsEntry() {
		entryType = EnumInstrDataCategory.NOT_ASSIGNED;	
		typeInstr = EnumInstrDataCategory.NOT_ASSIGNED;
		} 
	
	
	/**
	 * 
	 * Restituisce la categoria dell'oggetto ProgramCobolEntry
	 * 
	 * @return EnumInstrDataCategory the entryType
	 */
	public EnumInstrDataCategory getEntryType() {
		return entryType;
		
	}


	/**
	 * 
	 * Imposta la categoria dell'oggetto ProgramCobolEntry
	 * 
	 * @param EnumInstrDataCategory entryType
	 */
	public void setEntryType(EnumInstrDataCategory entryType) {
		this.entryType = entryType;
	}


	/**
	 * Restituisce il tipo istruzione jcl.<br>
	 * <p>
	 * 
	 * @return the typeInstr
	 */
	public EnumInstrDataCategory getTypeInstr() {
		return typeInstr;
	}







	/**
	 * Imposta il tipo istruzione jcl.<br>
	 * <p>
	 * 
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumInstrDataCategory typeInstr) {
		this.typeInstr = typeInstr;
	}



	/**
	 * 
	 * Restituisce l'istruzione codificata nell'entry.<br>
	 * <p>
	 * @return InstructionJclMvs instruction
	 */
	public InstructionJclMvs getInstruction() {
		return instruction;
	}


	/**
	 * 
	 * Imposta l'istruzione codificata nell'entry.<br>
	 * <p>
	 * {@link InstructionJclMvs}  						 
	 * 
	 * @param InstructionJclMvs  the instruction to set
	 */
	public void setInstruction(InstructionJclMvs instruction) {
		this.instruction =  instruction;
	}



	
	/**
	 * Imposta se l'entry l'struzione è eseguita in modo condizionale
	 * a fronte della macro istruzione <tt>IF</tt><br>
	 * 
	 * @param underCondition the underCondition to set
	 */
	public void setUnderCondition(boolean underCondition) {
		this.underCondition = underCondition;
	}


	/**
	 * Indica se l'istruzione, dentro un jcl include o proc,
	 * contiene qualche variabile <tt>&varName<\tt> di cui fare replace 
	 * con i valori correnti delle variabili.<br>
	 * Il valore delle variabili viene impostato dalla macro istruzione SET
	 * o nello statement di inclusione di una proc.<br>
	 * In queste condizioni l'istruzione deve essere analizzata al momento
	 * dell' inclusione, se codificata dentro una Include, o al momento
	 * della espansione, se codificata dentro una proc. <br>
	 * <p>
	 * 
	 * @return the anyVarToReplace
	 */
	public boolean isThereAnyVarToReplace() {
		return this.anyVarToReplace;
	}

	/**
	 * Imposta se l'istruzione, dentro un jcl include o proc,
	 * contiene qualche variabile <tt>&varName<\tt> di cui fare replace 
	 * con i valori correnti delle variabili.<br>
	 * Il valore delle variabili viene impostato dalla macro istruzione SET
	 * o nello statement di inclusione di una proc.<br>
	 * In queste condizioni l'istruzione deve essere analizzata al momento
	 * dell' inclusione, se codificata dentro una Include, o al momento
	 * della espansione, se codificata dentro una proc. <br>
	 * <p>
	 * @param anyVarToReplace the replacedBy to set
	 */
	public void setAnyVarToReplace(boolean anyVarToReplace) {
		this.anyVarToReplace = anyVarToReplace;
	}

	
	/**
	 * Restituisce true se l'istruzione, dentro un jcl include o proc o jcl job,
	 * ha tutte le variabili <tt>&varName<\tt> rimpiazzate dal valore 
	 * correnti delle variabili.<br>
	 * Il valore delle variabili viene impostato dalla macro istruzione SET
	 * o nello statement di inclusione di una proc.<br>
	 * In queste condizioni l'istruzione deve essere analizzata al momento
	 * dell' inclusione, se codificata dentro una Include, o al momento
	 * della espansione, se codificata dentro una proc. <br>
	 * <p>
	 * @return the allVarReplaced
	 */
	public boolean isAllVarReplaced() {
		return allVarReplaced;
	}


	/**
	 * Imposta se l'istruzione, dentro un jcl include o proc o jcl job,
	 * ha tutte le variabili <tt>&varName<\tt> rimpiazzate dal valore 
	 * correnti delle variabili.<br>
	 * Il valore delle variabili viene impostato dalla macro istruzione SET
	 * o nello statement di inclusione di una proc.<br>
	 * In queste condizioni l'istruzione deve essere analizzata al momento
	 * dell' inclusione, se codificata dentro una Include, o al momento
	 * della espansione, se codificata dentro una proc. <br>
	 * <p>
	 * @param allVarReplaced the allVarReplaced to set
	 */
	public void setAllVarReplaced(boolean allVarReplaced) {
		this.allVarReplaced = allVarReplaced;
	}


	/**
	 * 
	 * Restituisce true se lo statement espresso dall'entry è sotto un modulo richiamato con Include<br>
	 * <p>
	 * 
	 * @return boolean underIncludeName
	 */
	public boolean isUnderInclude() {
		return this.underInclude;
	}

	/**
	 * 
	 * Restituisce il nome del modulo include sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @return String underIncludeName
	 */
	public String getUnderIncludeName() {
		return this.underIncludeName;
	}

	/**
	 * Restituisce true se lo statement espresso dall'entry è dentro una Proc, embedded o meno.<br>
	 * <p>
	 * @return the underProc
	 */
	public boolean isUnderProc() {
		return underProc;
	}


	/**
     * Imposta se lo statement espresso dall'entry è dentro una Proc, embedded o meno.<br> 
     * <p>
	 * @param underProc the underProc to set
	 */
	public void setUnderProc(boolean underProc) {
		this.underProc = underProc;
	}


	/**
	 * Restituisce true se lo statement espresso dall'entry è dentro una Proc embedded.<br>
	 * <p>
	 * Viene restituito true sia se lo statement è nella definizione della proc,
	 * sia se si trova in una sua espansione.
	 * @return the underProcEmbedded
	 */
	public boolean isUnderProcEmbedded() {
		return underProcEmbedded;
	}


	/**
     * Imposta se lo statement espresso dall'entry è dentro una Proc embedded.<br> 
     * <p>
	 * @param underProc the underProcEmbedded to set
	 */
	public void setUnderProcEmbedded(boolean underProcEmbedded) {
		this.underProcEmbedded = underProcEmbedded;
	}


	/**
	 * Imposta  se l'entry descrive un'istruzione dentro un modulo include.<br>
	 * <p>
	 * 
	 * @param boolean underInclude
	 */
	public void setUnderInclude(boolean underInclude) {
		this.underInclude = underInclude;
	}



	/**
	 * 
	 * Imposta il nome del modulo include sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @paramString underIncludeName to set
	 */
	public void setUnderIncludeName(String underIncludeName) {
		this.underIncludeName = underIncludeName;
	}

	/**
	 * 
	 * Imposta il nome della proc sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @param String underProcName to set
	 */
	public void setUnderProcName(String underProcName) {
		this.underProcName = underProcName;
	}


	/**
	 * 
	 * Restituisce il nome della proc sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @return String underProcName
	 */
	public String getUnderProcName() {
		return this.underProcName;
	}

	/**
	 * Restituisce il nome dello step di procedura sotto la quale
	 * l'istruzione è definita.<br>
	 * <p>
	 * Si tratta dello step definito in <tt>//proceStepName EXEC PROC=procName<br>
	 * <p>
	 * 
	 * @return the underProcStepName
	 */
	public String getUnderProcStepName() {
		return this.underProcStepName;
	}


	/**
	 * Imposta il nome dello step di procedura sotto la quale
	 * l'istruzione è definita.<br>
	 * <p>
	 * Si tratta dello step definito in <tt>//proceStepName EXEC PROC=procName<br>
	 * <p>
	 * @param underProcStepName the underProcStepName to set
	 */
	public void setUnderProcStepName(String underProcStepName) {
		this.underProcStepName = underProcStepName;
	}


	/**
	 * 
	 * Imposta il nome dello step sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @param String underPgmStepName to set
	 */
	public void setUnderPgmStepName(String underPgmStepName) {
		this.underPgmStepName = underPgmStepName;
	}


	/**
	 * 
	 * Restituisce il nome dello step sotto il quale la definizione
	 * è inserita.<br>
	 * <p>
	 * 
	 * @return String underPgmStepName
	 */
	public String getUnderPgmStepName() {
		return this.underPgmStepName;
	}


	/**
	 * Restituisce true se l'entry l'struzione è eseguita in modo condizionale
	 * a fronte della macro istruzione <tt>IF</tt><br>
	 * 
	 * @param underCondition the underCondition to set
	 */
	public boolean getUnderCondition(boolean underCondition) {
		return this.underCondition;
	}


	/**
	 * 
	 * Restituisce il livello di annidamento della definizione
	 * nei moduli include nested, 0-based.
	 * 
	 * @return int levelNestingInclude
	 */
	public int getLevelNestingInclude() {
		return this.levelNestingInclude;
	}


	/**
	 * 
	 * Imposta il livello di annidamento della definizione
	 * nei modulo include nested, 0-based.<br>
	 * <p>
	 * 
	 * 
	 * @param levelNestingInclude the levelNestingInclude to set
	 */
	public void setLevelNestingInclude(int levelNestingInclude) {
		this.levelNestingInclude = levelNestingInclude;
	}


	/**
	 * 
	 * Imposta il numero di istruzione a partire dall'inizio del jcl.<br>
	 * I numeri di istruzione sono 0-based.<br>
	 * <p>
	 * 
	 * @param int numInstr to set
	 */
	public void setNumInstr(int numInstr) {
   	    this.instruction.setNumInstr(numInstr);
     	
	}


	/**
	 * Restituisce il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale è una macro istruzione condizionale IF<br>
	 * <p>
	 * 
	 * @return the numEntryCondition
	 */
	public int getNumEntryCondition() {
		return this.numEntryCondition;
	}



	/**
	 * Imposta il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale è una macro istruzione condizionale IF<br>
	 * 
	 * @param numEntryCondition the numEntryOwner to set
	 */
	public void setNumEntryCondition(int numEntryCondition) {
		this.numEntryCondition = numEntryCondition;
	}


	/**
	 * Restituisce true se l'entry di jcl è definita sotto
	 * una macro istruzione condizionale<br>
	 * <p>
	 * 
	 * @return boolean true se entry sotto condizione
	 */
	public boolean isUnderCondition() {
		return this.underCondition;
	}

	

	/**
	 * Restituisce true se l'entry è stato accodato allo
	 * step definito nella procedura richiamata a cui si riferisce.<br>
	 * <p>
	 * 
	 * @return the addedToStep
	 */
	public boolean isAddedToStep() {
		return addedToStep;
	}


	/**
	 * Imposta se l'entry è accodato allo
	 * step definito nella procedura richiamata a cui si riferisce.<br>
	 * <p>
	 * 
	 * @param addedToStep the addedToStep to set
	 */
	public void setAddedToStep(boolean addedToStep) {
		this.addedToStep = addedToStep;
	}


	/**
	 * Restituisce true se l'entry è stato rimpiazzato nello step
	 * definito nella procedura richiamata a cui si riferisce.<br>
	 * <p>
	 * @return the replacedInStep
	 */
	public boolean isReplacedInStep() {
		return replacedInStep;
	}


	/**
	 * Imposta se l'entry è stato rimpiazzato nello step
	 * definito nella procedura richiamata a cui si riferisce.<br>
	 * <p>
	 * 
	 * @param replacedInStep the replacedInStep to set
	 */
	public void setReplacedInStep(boolean replacedInStep) {
		this.replacedInStep = replacedInStep;
	}

	

	/**
	 * Restituisce true se lo statement è uno statement di override.<br>
	 * <p>
	 * Gli statement di override sono collocati dopo il richiamo a proc
	 * e sostituiscono uno statement di jcl o vengono accodati allo step
	 * indicato.<br>
	 * <p>
	 * Gli statement di override sono del tipo:<br>
	 * <tt>//step.ddname          DD .......  dove ddname è definito nello step</tt><br>
     * <tt>//procStep.step.ddname DD .......  dove ddname è definito nello step di procStep</tt><br>
     * <p>
	 * @return the override
	 */
	public boolean isOverride() {
		return this.override;
	}


	/**
	 * Restituisce true se lo statement è uno statement di override.<br>
	 * <p>
	 * Gli statement di override sono collocati dopo il richiamo a proc
	 * e sostituiscono uno statement di jcl o vengono accodati allo step
	 * indicato.<br>
	 * <p>
	 * Gli statement di override sono del tipo:<br>
	 * <tt>//step.ddname          DD .......  dove ddname è definito nello step</tt><br>
     * <tt>//procStep.step.ddname DD .......  dove ddname è definito nello step di procStep</tt><br>
     * <p>
	 * @param override the override to set
	 */
	public void setOverride(boolean override) {
		this.override = override;
	}

	

	/**
	 * Restituisce true se lo statemnt è stato aggiornato
	 * da uno statement di override in aggiornamento di
	 * una dd esistente.<br>
	 * <p>
	 * 
	 * @return the overridedUpdate
	 */
	public boolean isOverridedUpdate() {
		return overridedUpdate;
	}


	/**
	 * Imposta se lo statemnt è stato aggiornato
	 * da uno statement di override in aggiornamento di
	 * una dd esistente.<br>
	 * <p>
	 * 
	 * @param overridedUpdate the overridedUpdate to set
	 */
	public void setOverridedUpdate(boolean overridedUpdate) {
		this.overridedUpdate = overridedUpdate;
	}


	/**
	 * Restituisce true se lo statemnt è stato aggiornato
	 * da uno statement di override in accodamento a
	 * uno step.<br>
	 * <p>
	 * @return the overridedAppend
	 */
	public boolean isOverridedAppend() {
		return overridedAppend;
	}


	/**
	 * Imposta se lo statemnt è stato aggiornato
	 * da uno statement di override in accodamento a
	 * uno step.<br>
	 * <p>
	 * @param overridedAppend the overridedAppend to set
	 */
	public void setOverridedAppend(boolean overridedAppend) {
		this.overridedAppend = overridedAppend;
	}


	/**
	 * Restituisce true se lo statement DD ha nel DSNAME
	 * un riferimento backward<br>
	 * <tt>//ddname DD DSNAME=*.ddname</tt>
	 * <tt>//ddname DD DSNAME=*.stepName.ddname</tt>
	 * <tt>//ddname DD DSNAME=*.procStepName.stepName.ddname</tt>
	 * <p>
	 * @return the referenceBackward
	 */
	public boolean isReferenceBackward() {
		return referenceBackward;
	}


	/**
	 * Imposta se lo statement DD ha nel DSNAME
	 * un riferimento backward<br>
	 * <tt>//ddname DD DSNAME=*.ddname</tt>
	 * <tt>//ddname DD DSNAME=*.stepName.ddname</tt>
	 * <tt>//ddname DD DSNAME=*.procStepName.stepName.ddname</tt>
	 * <p>
	 * 
	 * @param referenceBackward the referenceBackward to set
	 */
	public void setReferenceBackward(boolean referenceBackward) {
		this.referenceBackward = referenceBackward;
	}


	/**
	 * Restituisce true se lo statement DD ha nel DSNAME
	 * un riferimento forward<br>
	 * <tt>//ddname DD DSNAME=ddname</tt>
	 * <p>
	 * @return the referenceForward
	 */
	public boolean isReferenceForward() {
		return referenceForward;
	}


	/**
	 * Imposta se lo statement DD ha nel DSNAME
	 * un riferimento forward<br>
	 * <tt>//ddname DD DSNAME=ddname</tt>
	 * <p>
	 * @param referenceForward the referenceForward to set
	 */
	public void setReferenceForward(boolean referenceForward) {
		this.referenceForward = referenceForward;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return instruction.getSourceInstr();
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone()  {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}

	
}
