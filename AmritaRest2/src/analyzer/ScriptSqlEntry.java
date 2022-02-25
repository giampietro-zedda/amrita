package analyzer;
import java.io.Serializable;
import enums.EnumInstrDataCategory;


/**
 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ScriptSqlEntry 
 * </h1>
 *  <p>
 * Questa classe  modella un generico entry codificato di uno script Sql<br>
 * Ogni entry è relativo a uno statement sql che si trova in un0 script, come una CREATE TABLE
 * uno statement IF, CASE o una FETCH etc.<br>
 * Istanze di questa classe vengono memorizzate in oggetti {@link ScriptSql} come Array di entry.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 29/06/2011
 * @see ProgramCobol
 * @see CopyCobol
 * @see Analyzer
 * @see AnalyzerCobol
 * @see AnalyzerCobolCopyDataDivision
 * @see AnalyzerCobolCopyProcedure
 * @see ScriptSql
 */

public class ScriptSqlEntry implements Cloneable, Serializable {

	private static final long serialVersionUID = 1L;
	

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                                                               //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	// Tipologia entry nel programma/copy
	private EnumInstrDataCategory entryType = null;	  	  // Statement embedded in pgm di precompilatore, statement di script, command
	
	private InstructionSql instruction = null;            // Istruzione Sql 
	
	// Collegamento fra istruzione e fine istruzione e istruzione con istruzione correlabile
	private int numInstrRelated = 0;					  // Istruzione di fine/inizio blocco, come END  per BEGIN e viceversa

	// Numero entry con istruzione Owner
	private int numEntryOwner = 0;                        // Numero entry owner. If se in ramo If/Else, Numero Evaluate/Search se in ramo When, Inner Perform e paragrafo

	// Indicazione di entry sotto condizione
	private boolean underCondition = false;               // True indica entry sotto ramo condizionale o when
	
	// Numero entry con istruzione Owner
	private int levelDeepEntry = 0;                       // Livello entry di programma. Se > 0 indica entry collocato sotto istruzione condizionale (If/Evaluate,..)


	
	/**
     * 
     * Costruttore vuoto
     * 
     */
 	public ScriptSqlEntry() {
		entryType = EnumInstrDataCategory.NOT_ASSIGNED;	
	} 
	
	
 	
 	
 


	/**
	 * 
	 * Restituisce la categoria dell'oggetto ScriptSqlEntry
	 * 
	 * @return EnumInstrDataCategory the entryType
	 */
	public EnumInstrDataCategory getEntryType() {
		return entryType;
		
	}


	/**
	 * 
	 * Imposta la categoria dell'oggetto ScriptSqlEntry
	 * 
	 * @param EnumInstrDataCategory entryType
	 */
	public void setEntryType(EnumInstrDataCategory entryType) {
		this.entryType = entryType;
	}










	/**
	 * 
	 * Restituisce l'istruzione codificata nell'entry.<br>
	 * <p>
	 * 
	 * @return the instruction
	 */
	public InstructionSql getInstruction() {
		return instruction;
	}


	/**
	 * 
	 * Imposta l'istruzione codificata nell'entry.<br>
	 * <p>
	 * 
	 * @param instructionSql the instruction to set
	 */
	public void setInstruction(InstructionSql instruction) {
		this.instruction =  instruction;
	}



	

	/**
	 * Imposta se l'entry è definito in un ramo
	 * true/Else di If, sotto una Evaluate When o sotto
	 * una Search When
	 * 
	 * @param underCondition the underCondition to set
	 */
	public void setUnderCondition(boolean underCondition) {
		this.underCondition = underCondition;
	}




	/**
	 * 
	 * Imposta il numero di istruzione a partire dall'inizio del programma.
	 * I numeri di istruzione sono 0-based.
	 * 
	 * @param int numInstr to set
	 */
	public void setNumInstr(int numInstr) {
		
   	    this.instruction.setNumInstr(numInstr);
     	
	}

	/**
	 * 
	 * Restituisce true se l'entry contiene un'istruzione di Cobol Procedure Division.
	 * 
	 * @return boolean 
	 */
	public boolean isInsideInstructionCobolProcedure() {
		
   	    if (entryType == EnumInstrDataCategory.COBOL_PROC_INSTRUCTION) {
			return true;
		}
     	return false;
	}

	/**
	 * 
	 * Restituisce true se l'entry contiene un'istruzione di un precompilatore.
	 * 
	 * @return boolean 
	 */
	public boolean isInsideInstructionPrecompiler() {
		
  	    if (entryType == EnumInstrDataCategory.SQL_PRECOMPILER
	    ||  entryType == EnumInstrDataCategory.CICS_PRECOMPILER	
	    ||  entryType == EnumInstrDataCategory.DL1_PRECOMPILER	
  	    ) {
			return true;
		}
     	return false;
	}

	/**
	 * 
	 * Restituisce true se l'entry contiene un'istruzione di un precompilatore Sql
	 * 
	 * @return boolean 
	 */
	public boolean isInsideInstructionPrecompilerSql() {
		
   	    if (entryType == EnumInstrDataCategory.SQL_PRECOMPILER) {
			return true;
		}
   	    
     return false;
	}

	/**
	 * 
	 * Restituisce true se l'entry contiene un'istruzione di un precompilatore Cics
	 * 
	 * @return boolean 
	 */
	public boolean isInsideInstructionPrecompilerCics() {
		
   	    if (entryType == EnumInstrDataCategory.CICS_PRECOMPILER) {
			return true;
		}
   	    
     return false;
	}

	/**
	 * 
	 * Restituisce true se l'entry contiene un'istruzione di un precompilatore DL1
	 * 
	 * @return boolean 
	 */
	public boolean isInsideInstructionPrecompilerDL1() {
		
   	    if (entryType == EnumInstrDataCategory.DL1_PRECOMPILER) {
			return true;
		}
   	    
     return false;
	}

	/**
	 * 
	 * Restituisce il numero di istruzione di fine o di inizio blocco istruzione.<br>
	 * <p>
	 * Se l'istruzione corrente è una IF restituisce, se presente, il numero
	 * della END-IF o dell'istruzione successiva. <br>
	 * Se l'istruzione corrente è una END-IF restituisce il numero della IF. <br>
	 * Se Perform Inner restituisce il numero di istruzione della END-PERFORM e così via. <br>
	 * In generale viene quindi restituito il numero di istruzione END-xxxx
	 * oppure il numero di istruzione successiva all'ultima del blocco, chiusa
	 * con un punto. <br>
	 * <p>
	 * Le istruzioni iteressate alla chiusura del blocco con END-xxxx sono:<br>
	 * <p>
	 * ACCEPT<br>
	 * IF<br>
	 * PERFORM<br>
	 * EVALUATE<br>
	 * SEARCH<br>
	 * ADD<br>
	 * SUBTRACT<br>
	 * DIVIDE<br>
	 * MULTIPLY<br>
	 * CALL<br>
	 * START<br>
	 * READ<br>
	 * DELETE<br>
	 * READ<br>
	 * WRITE<br>
	 * REWRITE<br>
	 * STRING<br>
	 * UNSTRING<br>
	 * 
	 * 
	 * Restituisce il numero di istruzione di inizio blocco entro il quale l'istruzione è definita.<br>
	 * Per esempio le istruzioni dentro un ramo IF true fanno riferimento alla IF di pertinenza, come anche
	 * l'eventuale ELSE.<br>
	 * Eventuali istruzioni sotto il ramo ELSE fanno riferimento alla ELSE di pertinenza.
	 * <p>
	 * <p>
	 * Le istruzioni iteressate a questa gestione sono:<br>
	 * <p>
	 * IF<br>
	 * PERFORM<br>
	 * EVALUATE<br>
	 * SEARCH<br>
	 * <P>
	 * Tipicamente le istruzioni di Else e di Exception fanno riferimento all'istruzione collegata:
	 * <p>
	 * ELSE<br>
	 * 
	 * ON_SIZE_ERROR<br>
	 * ON_OVERFLOW <br>  
	 * ON_EXCEPTION <br>
	 * AT_END <br>
	 * AT_END_OF_PAGE<br>
	 * AT_EOP	<br>
	 * NOT_ON_SIZE_ERROR <br> 
	 * NOT_ON_OVERFLOW <br> 
	 * NOT_ON_EXCEPTION	<br>
	 * NOT_AT_END <br>
	 * NOT_AT_END_OF_PAGE <br>
	 * NOT_AT_EOP <br>	
	 * NOT_INVALID_KEY <br>	 
	 * INVALID_KEY <br>
	 * 
	 * END-ACCEPT<br>
	 * END-IF<br>
	 * END-PERFORM<br>
	 * END-EVALUATE<br>
	 * END-SEARCH<br>
	 * END-ADD<br>
	 * END-SUBTRACT<br>
	 * END-DIVIDE<br>
	 * END-MULTIPLY<br>
	 * END-CALL<br>
	 * END-START<br>
	 * END-READ<br>
	 * END-DELETE<br>
	 * END-READ<br>
	 * END-WRITE<br>
	 * END-REWRITE<br>
	 * END-STRING<br>
	 * END-UNSTRING<br>
	 * 
	 * @return the numInstrEnd
	 */
	public int getNumInstrRelated() {
		return numInstrRelated;
	}

	/**
	 * 
	 * 
	 * Imposta il numero di istruzione di fine blocco istruzione.<br>
	 * <p>
	 * Se l'istruzione corrente è una IF imposta il numero della END-IF o dell'istruzione successiva. <br>
	 * Se Perform Inner imposta il numero di istruzione della END-PERFORM e così via. <br>
	 * Se l'istruzione non prevede una istruzione END-xxxx, oppure l'istruzione è chiusa con un punto,
	 * imposta il numero di istruzione a quella di chiusura del blocco, terminata con un punto. <br>
	 * <p>
	 * Le istruzioni iteressate alla chiusura del blocco con END-xxxx sono:<br>
	 * <p>
	 * ACCEPT<br>
	 * IF<br>
	 * PERFORM<br>
	 * EVALUATE<br>
	 * SEARCH<br>
	 * ADD<br>
	 * SUBTRACT<br>
	 * DIVIDE<br>
	 * MULTIPLY<br>
	 * CALL<br>
	 * START<br>
	 * READ<br>
	 * DELETE<br>
	 * READ<br>
	 * WRITE<br>
	 * REWRITE<br>
	 * STRING<br>
	 * UNSTRING<br>
     *
	 * 
	 * Imposta il numero di istruzione di inizio blocco entro il quale l'istruzione è definita.<br>
	 * Per esempio le istruzioni dentro un ramo IF true fanno riferimento alla IF di pertinenza, come anche
	 * l'eventuale ELSE.<br>
	 * Eventuali istruzioni sotto il ramo ELSE fanno riferimento alla ELSE di pertinenza.
	 * <p>
	 * <p>
	 * Le istruzioni iteressate a questa gestione sono:<br>
	 * <p>
	 * IF<br>
	 * PERFORM<br>
	 * EVALUATE<br>
	 * SEARCH<br>
	 * <P>
	 * Tipicamente le istruzioni di Else e di Exception fanno riferimento all'istruzione collegata:
	 * <p>
	 * ELSE<br>
	 * 
	 * ON_SIZE_ERROR<br>
	 * ON_OVERFLOW <br>  
	 * ON_EXCEPTION <br>
	 * AT_END <br>
	 * AT_END_OF_PAGE<br>
	 * AT_EOP	<br>
	 * NOT_ON_SIZE_ERROR <br> 
	 * NOT_ON_OVERFLOW <br> 
	 * NOT_ON_EXCEPTION	<br>
	 * NOT_AT_END <br>
	 * NOT_AT_END_OF_PAGE <br>
	 * NOT_AT_EOP <br>	
	 * NOT_INVALID_KEY <br>	 
	 * INVALID_KEY <br>
	 * 
	 * END-ACCEPT<br>
	 * END-IF<br>
	 * END-PERFORM<br>
	 * END-EVALUATE<br>
	 * END-SEARCH<br>
	 * END-ADD<br>
	 * END-SUBTRACT<br>
	 * END-DIVIDE<br>
	 * END-MULTIPLY<br>
	 * END-CALL<br>
	 * END-START<br>
	 * END-READ<br>
	 * END-DELETE<br>
	 * END-READ<br>
	 * END-WRITE<br>
	 * END-REWRITE<br>
	 * END-STRING<br>
	 * END-UNSTRING<br>
	 * @param numInstrEnd the numInstrEnd to set
	 */
	public void setNumInstrRelated(int numInstrRelated) {
		this.numInstrRelated = numInstrRelated;
	}


	/**
	 * Restituisce il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale può essere unas IF, una Evaluate, una
	 * Search o una Inner perform.
	 * 
	 * @return the numEntryOwner
	 */
	public int getNumEntryOwner() {
		return numEntryOwner;
	}



	/**
	 * Imposta il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale può essere unas IF, una Evaliuate, una
	 * Search o una Inner perform.
	 * 
	 * @param numEntryOwner the numEntryOwner to set
	 */
	public void setNumEntryOwner(int numEntryOwner) {
		this.numEntryOwner = numEntryOwner;
	}


	/**
	 * Restituisce il livello di anndidamento dell'entry di programma.<br>
	 * <p>
	 * Una istruzione sotto IF/Evaluate/.. restituisce un livello > 0
	 * 
	 * @return the levelDeepEntry
	 */
	public int getLevelDeepEntry() {
		return levelDeepEntry;
	}


	/**
	 * Imposta il livello di anndidamento dell'entry di programma.<br>
	 * <p>
	 * Una istruzione sotto IF/Evaluate/.. ha un livello > 0.
	 * 
	 * @param levelDeepEntry the levelDeepEntry to set
	 */
	public void setLevelDeepEntry(int levelDeepEntry) {
		this.levelDeepEntry = levelDeepEntry;
	}



	/**
	 * Restituisce true se l'entry di programma è definita sotto
	 * una istruzione condizionale o una Evaluate When oppure
	 * una Search When<br>
	 * <p>
	 * 
	 * @return boolean true se entry sotto condizione
	 */
	public boolean isUnderCondition() {
		return this.underCondition;
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
