package analyzer;
import java.io.Serializable;

import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;


/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda  Turin (ITALY)
 * 
 * <h1>
 * ProgramCobolEntry 
 * </h1>
 *  <p>
 * Questa classe  modella un generico entry codificato di un programma Cobol <br>
 * Ogni entry può essere relativo alla Data Division o alla Procedure Division e descrive
 * una definizione dati, un'istruzione del linguaggio cobol, di un precompilatore oppure
 * uno statement copy, etc.<br>
 * Vengono censite le informazioni per sapere se uno statement è codificato dentro un modulo
 * Copy e a quale livello di anndidamento.<br>
 * Istanze di questa classe vengono memorizzate in oggetti {@link ProgramCobol} e {@link CopyCobol}
 * come Array di entry.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/04/2010
 * @see ProgramCobol
 * @see CopyCobol
 * @see Analyzer
 * @see AnalyzerCobol
 * @see AnalyzerCobolCopyDataDivision
 * @see AnalyzerCobolCopyProcedure
 */

public class ProgramCobolEntry<I extends Instruction> implements Cloneable, Serializable {

	private static final long serialVersionUID = 1L;
	

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza                                                                               //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	// Tipo istruzione
	private EnumCobolReservedWords  typeInstr = null;	  	// Ridondante in quanto già nell'istruzione. Evita estrazione e casting
	
	// Tipologia entry nel programma/copy
	private EnumInstrDataCategory entryType = null;	  	  	// Data item, statement copy, statement precompilatore, struttura
	
	// Valide se definizione dati. Area del programma dove il dato è definito
	private EnumCobolReservedWords programDivision = null;	// Identification/Environment/Data/Procedure Division ...
	private EnumCobolReservedWords programSection = null;	// Configuration/File/Working-Storage/Linkage Section
    private int pos = 0;                          		  	// Posizione campo 0-based dall'inizio della struttura liv 01 (es copy)
	
	// Istruzione Cobol che può essere:
	//   InstructionCobolIdentification  		Se l'entry descrive uno statement di identification division 	  
	//   InstructionCobolEnvironment  			Se l'entry descrive uno statement di environment division 	  
	//   InstructionCobolDataItem  				Se l'entry descrive uno statement di data division, di definizione data item  	  
	//   InstructionCobolDataStruct				Se l'entry descrive uno statement di data division, di struttura, come FD nome-file ...
	//   InstructionCobolProcedure  			Se l'entry descrive uno statement di procedure division
	//   InstructionCics  				        Se l'entry descrive uno statement cics di precompilatore
	//   InstructionSql  				        Se l'entry descrive uno statement sql  di precompilatore
	//   InstructionCobol  						Se l'entry descrive uno statement non contemplato nei precedenti come copy, direttive etc.
 	private I instruction = null;
	
	// Valide se l'entry è definito dentro un modulo copy 
	private String underCopyName = "";              	  // Nome copy sotto cui l'item è definito (ridondante ma utile e chiaro)
	private boolean underCopy = false;              	  // True indica che il data item è definito in un copy richiamato in questo copy
	private boolean replacedBy = false;                   // True indica istruzione dentro copy con valori replaced
	private int levelNestingCopy = 0;               	  // Livello di annidamento del modulo copy sotto il quale l'item si trova

	// Valide se l'entry definisce un'istruzione è questa si trova dentro una procedure interna (come una Section Cobol)
	private boolean underProcInternal = false;            // True indica che l'istruzione si trova in una Procedure interna
	private int underProcInternalPointer = 0;             // Pointer a definizione section/paragrafo entro cui si trova l'istruzione
 	
	// Se istruzione di definizine dati, true indica che il dato è definito ma mai utilizzato direttamente
	// Se istruzione di procedure division, true indica l'istruzione non è mai eseguita, in quanto appartenente
	//   a una section o a un paragrafo non referenziato oppure è una label non referenziata
	// Se l'istruzione è un Copy statement, indica che nessun campo del copy è referenziato
	private boolean deadCode = false;                     // True indica dead code
    
	// Si tratta di istruzioni collocate dopo statements di flusso incondizionato quali
	// Stop Run, Goback, Exec Cics Xctl, Return, Abend
	// senza label referenziate che le precedono.
	private boolean deadCodeUnreachable = false;          // True indica istruzione fisicamente non raggiungibile
    
	// Indica se l'entry contiene una istruzione di dichiarazione non procedurale (DECLARATIVES)
	private boolean isDeclarative = false;
	
	// Indica se l'entry contiene una istruzione della mainline del programma
	private boolean isMainline = false;
	
	// Istruzione NON presente nel source e inserita per simpulazione precompilatore
	// Indica se l'entry è stato inserito per simulazione di copy incluso da precompilatore.
	// In questo caso le informazioni sulle righe sorgente non sono significative.
	private boolean isEntryPrecompiler = false;
	
	// Collegamento fra istruzione e fine istruzione e istruzione con istruzione correlabile
	private int numInstrRelated = 0;					  // Istruzione di fine/inizio blocco, come END-IF per IF, IF per END-IF END-EVALUATE per EVALUATE 
	                                                      // Istruzione correlabile, come IF per ELSE/NEXT SENTENCE, SEARCH/EVALUATE per WHEN
	private int numInstrRelatedElse = 0;				  // Istruzione di ELSE per IF
	
	// Numero entry con istruzione Owner
	private int numEntryOwnerConditional = 0;             // Numero entry owner. If se in ramo If/Else, Numero Evaluate/Search se in ramo When, Inner Perform e paragrafo

	// Indicazione di entry sotto condizione
	private boolean underCondition = false;               // True indica entry sotto ramo true/else, evaluate when o search when
	
	// Numero entry con istruzione Owner
	private int levelDeepEntry = 0;                       // Livello entry di programma. Se > 0 indica entry collocato sotto istruzione condizionale (If/Evaluate,..)

	// Istruzione con gestione exception
	private boolean withExceptionManaged = false;         // Indica presenza ON ERROR, AT END, INVALID KEY etc., immediatamente successivi all'istruzione
	
 	
	/**
     * 
     * Costruttore vuoto
     * 
     */
 	public ProgramCobolEntry() {
		entryType = EnumInstrDataCategory.NOT_ASSIGNED;	
		typeInstr = EnumCobolReservedWords.NOT_ASSIGNED;
		programDivision = EnumCobolReservedWords.NOT_ASSIGNED;
		programSection = EnumCobolReservedWords.NOT_ASSIGNED;
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
	 * @return the typeInstr
	 */
	public EnumCobolReservedWords getTypeInstr() {
		return typeInstr;
	}







	/**
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumCobolReservedWords typeInstr) {
		this.typeInstr = typeInstr;
	}







	/**
	 * 
	 * Restituisce la divisione del programma dove l'entry è situato.<br>
	 * L'area è codificata dalla parola riservata Cobol codificata da {@link EnumCobolReservedWords}
	 * 
	 * @return the EnumCobolReservedWords program area
	 */
	public EnumCobolReservedWords getProgramDivision() {
		return programDivision;
	}


	/**
	 * 
	 * Imposta la divisione del programma dove l'entry è situato.<br>
	 * L'area è codificata dalla parola riservata Cobol codificata da {@link EnumCobolReservedWords}
	 * 
	 * @paramEnumCobolReservedWords programArea
	 */
	public void setProgramDivision(EnumCobolReservedWords programDivision) {
		this.programDivision = programDivision;
	}


	
	/**
	 * Restituisce la sezione del programma dove l'entry è situato.<br>
	 * L'area è codificata dalla parola riservata Cobol codificata da {@link EnumCobolReservedWords}
	 * 
	 * @return the programSection
	 */
	public EnumCobolReservedWords getProgramSection() {
		return programSection;
	}

	/**
	 * Imposta la sezione del programma dove l'entry è situato.<br>
	 * L'area è codificata dalla parola riservata Cobol codificata da {@link EnumCobolReservedWords}
	 * 
	 * @param programSection the programSection to set
	 */
	public void setProgramSection(EnumCobolReservedWords programSection) {
		this.programSection = programSection;
	}







	/**
	 * 
	 * Restituisce l'istruzione codificata nell'entry.<br>
	 * <p>
	 * {@link InstructionCobolDataItem}  						 
	 * {@link InstructionCobolDataStruct}				 
	 * {@link InstructionCobolProcedure}  				 
	 * {@link InstructionCobolCopy} 					 
	 * {@link InstructionCics}  			 
	 * 
	 * @return the instruction
	 */
	public I getInstruction() {
		return instruction;
	}


	/**
	 * 
	 * Imposta l'istruzione codificata nell'entry.<br>
	 * <p>
	 * {@link InstructionCobolDataItem}  						 
	 * {@link InstructionCobolDataStruct}				 
	 * {@link InstructionCobolProcedure}  				 
	 * {@link InstructionCobolCopy} 					 
	 * {@link InstructionCics}  			 

	 * 
	 * @param instruction2 the instruction to set
	 */
	@SuppressWarnings("unchecked")
	public void setInstruction(Instruction instruction) {
		this.instruction = (I) instruction;
	}



	
	/**
	 * Restituisce true se la definizione dati non è utilizzata nel programma
	 * oppure se l'istruzione è codice morto.
	 * 
	 * @return the deadCode
	 */
	public boolean isDeadCode() {
		return deadCode;
	}


	/**
	 * 
	 * Si imposta a true se la definizione dati non è utilizzata nel programma
	 * oppure se l'istruzione è codice morto.
	 * 
	 * @param deadCode the deadCode to set
	 */
	public void setDeadCode(boolean deadCode) {
		this.deadCode = deadCode;
	}


	/**
	 * Restituisce se l'istruzione è fisicamente raggiungibile da altre parti del programma.<br>
	 * <p>
	 * Si tratta di istruzioni collocate dopo statements di flusso incondizionato quali<br>
	 * Stop Run, Goback, Exec Cics Xctl, Return, Abend senza label referenziate che le precedono.<br>
	 * <p>
	 * @return the deadCodeUnreachable
	 */
	public boolean isDeadCodeUnreachable() {
		return deadCodeUnreachable;
	}

	/**
	 * Imposta se l'istruzione è fisicamente raggiungibile da altre parti del programma.<br>
	 * <p>
	 * Si tratta di istruzioni collocate dopo statements di flusso incondizionato quali<br>
	 * Stop Run, goback, GoTo, Exec Cics Return, Start, Xctl o Abend senza label <br>
	 * referenziate che le precedono.<br>
	 * <p>
	 * @param deadCodeUnreachable the deadCodeUnreachable to set
	 */
	public void setDeadCodeUnreachable(boolean deadCodeUnreachable) {
		this.deadCodeUnreachable = deadCodeUnreachable;
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
	 * Indica se l'istruzione, dentro una copy, è stata
	 * oggetto di replace.
	 * 
	 * 
	 * @return the replacedBy
	 */
	public boolean isReplacedBy() {
		return replacedBy;
	}

	/**
	 * Imposta se l'istruzione, dentro una copy, è stata
	 * oggetto di replace.
	 * 
	 * 
	 * @param replacedBy the replacedBy to set
	 */
	public void setReplacedBy(boolean replacedBy) {
		this.replacedBy = replacedBy;
	}



	/**
	 * 
	 * Restituisce true se l'istruzione Cobol o di precompiler,
	 * è definita all'interno di una procedura interna Cobol come una Section.
	 * 
	 * @return the underProcInternal
	 */
	public boolean isUnderProcInternal() {
		return underProcInternal;
	}


	/**
	 * 
	 * Imposta se l'istruzione Cobol o di precompiler,
	 * è definita all'interno di una procedura interna Cobol come una Section.
	 * 
	 * 
	 * @param boolean underProcInternal
	 */
	public void setUnderProcInternal(boolean underProcInternal) {
		this.underProcInternal = underProcInternal;
	}


	/**
	 * 
	 * Restituisce il pointer dell'entry relativo alla procedura
	 * interna (Section Cobol), all'interno della quale l'istruzione
	 * corrente è definita.
	 * 
	 * @return the underProcInternalPointer
	 */
	public int getUnderProcInternalPointer() {
		return underProcInternalPointer;
	}


	/**
	 * 
     * Imposta il pointer dell'entry relativo alla procedura
	 * interna (Section Cobol), all'interno della quale l'istruzione
	 * corrente è definita.
	 * 
	 * @param underProcInternalPointer the underProcInternalPointer to set
	 */
	public void setUnderProcInternalPointer(int underProcInternalPointer) {
		this.underProcInternalPointer = underProcInternalPointer;
	}


	/**
	 * 
	 * Restituisce true se lo statement espresso dall'entry è sotto un modulo copy
	 * 
	 * @return boolean isUnderCopy
	 */
	public boolean isUnderCopy() {
		return this.underCopy;
	}

	/**
	 * 
	 * Restituisce il nome del modulo copy sotto il quale la definizione
	 * è inserita.
	 * 
	 * @return String underCopyName
	 */
	public String getUnderCopyName() {
		return this.underCopyName;
	}

	/**
	 * 
	 * Imposta  se l'entry descrive un'istruzione sotto modulo copy
	 * 
	 * @param boolean underCopy
	 */
	public void setUnderCopy(boolean underCopy) {
		this.underCopy = underCopy;
	}



	/**
	 * 
	 * Imposta il nome del modulo copy sotto il quale la definizione
	 * è inserita.
	 * 
	 * @paramString underCopyName to set
	 */
	public void setUnderCopyName(String underCopyName) {
		this.underCopyName = underCopyName;
	}

	/**
	 * 
	 * Restituisce la posizione 0-based del data item,
	 * se l'entry definisce un data item, nello 01
	 * di appartenenza.
	 * 
	 * @return the pos
	 */
	public int getPos() {
		return pos;
	}


	/**
	 * 
	 * Imposta la posizione 0-based del data item,
	 * se l'entry definisce un data item.
	 * 
	 * @param pos the pos to set
	 */
	public void setPos(int pos) {
		this.pos = pos;
	}


	/**
	 * 
	 * Restituisce il livello di annidamento della definizione
	 * nei copy nested, 0-based.
	 * 
	 * @return int copyNestinglevel
	 */
	public int getLevelNestingCopy() {
		return this.levelNestingCopy;
	}


	/**
	 * 
	 * Imposta il livello di annidamento della definizione
	 * nei copy nested, 0-based.
	 * 
	 * @param copyNestinglevel the copyNestinglevel to set
	 */
	public void setLevelNestingCopy(int levelNestingCopy) {
		this.levelNestingCopy = levelNestingCopy;
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
	 * della END-IF o dell'istruzione di chiusura. <br>
	 * Se l'istruzione corrente è una END-IF restituisce il numero della IF. <br>
	 * Se Perform Inner restituisce il numero di istruzione della END-PERFORM e così via. <br>
	 * In generale viene quindi restituito il numero di istruzione END-xxxx
	 * oppure l'ultimo numero di istruzione del blocco, chiusa con un punto. <br>
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
	 * 
	 * Imposta il numero di istruzione di ELSE dell'istruzione IF.<br>
	 * <p>
	 * Se l'istruzione corrente è una IF imposta il numero della eventuale ELSE presente. <br>
	 * <p>
	 * 
	 * @return the numInstrRelatedElse
	 */
	public int getNumInstrRelatedElse() {
		return numInstrRelatedElse;
	}

	/**
	 * 
	 * Restituisce il numero di istruzione di ELSE dell'istruzione IF.<br>
	 * <p>
	 * Se l'istruzione corrente è una IF restituisce il numero della eventuale ELSE presente. <br>
	 * <p>
	 * @param numInstrRelatedElse the numInstrRelatedElse to set
	 */
	public void setNumInstrRelatedElse(int numInstrRelatedElse) {
		this.numInstrRelatedElse = numInstrRelatedElse;
	}







	/**
	 * Restituisce true se l'istruzione gestisce le condizioni
	 * di exception.<br>
	 * <p>
	 * Per esempio può trattarsi di READ con INVALID KEY
	 * COMPUTE con ON SIZE ERROR etc.<br>
	 * Le eccezioni sono trattate come istruzioni separate
	 * comunque collegate all'istruzione di appartenenza. 
	 * 
	 * 
	 * @return the withExceptionManaged
	 */
	public boolean isWithExceptionManaged() {
		return withExceptionManaged;
	}




	/**
	 * Imposta se l'istruzione gestisce le condizioni
	 * di exception.<br>
	 * <p>
	 * Per esempio può trattarsi di READ con INVALID KEY
	 * COMPUTE con ON SIZE ERROR etc.<br>
	 * Le eccezioni sono trattate come istruzioni separate
	 * comunque collegate all'istruzione di appartenenza. 
	 * 
	 * 
	 * 
	 * @param withExceptionManaged the withExceptionManaged to set
	 */
	public void setWithExceptionManaged(boolean withExceptionManaged) {
		this.withExceptionManaged = withExceptionManaged;
	}



	/**
	 * Restituisce il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale può essere unas IF, una Evaluate, una
	 * Search o una Inner perform.
	 * 
	 * @return the numEntryOwnerConditional
	 */
	public int getNumEntryOwnerConditional() {
		return numEntryOwnerConditional;
	}



	/**
	 * Imposta il numero di istruzione condizionale sotto la quale
	 * l'entry è definito.<br>
	 * <p>
	 * L'istruzione condizionale può essere unas IF, una Evaliuate, una
	 * Search o una Inner perform.
	 * 
	 * @param numEntryOwnerConditional the numEntryOwnerConditional to set
	 */
	public void setNumEntryOwnerConditional(int numEntryOwnerConditional) {
		this.numEntryOwnerConditional = numEntryOwnerConditional;
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



	/**
	 * Restituisce se l'entry contiene uno statement all'interno di DECLARATIVES cobol.<br>
	 * <b>
	 * Sono inclusi gli statement con DECLARATIVES e END DECLARATIVES.<br>
	 * <p>
	 * @return the isDeclarative
	 */
	public boolean isDeclarative() {
		return isDeclarative;
	}


	/**
	 * Imposta se l'entry contiene uno statement all'interno di DECLARATIVES cobol.<br>
	 * <b>
	 * Sono inclusi gli statement con DECLARATIVES e END DECLARATIVES.<br>
	 * <p>
	 * @param isDeclarative the isDeclarative to set
	 */
	public void setDeclarative(boolean isDeclarative) {
		this.isDeclarative = isDeclarative;
	}

	/**
	 * Restituisce se l'entry è relativo a una istruzione nella mainline del programma.<br>
	 * <b>
	 * Si tratta di istruzioni immediatamente eseguibili allo start del programma.<br>
	 * <p>
	 * @return the isMainline
	 */
	public boolean isMainline() {
		return isMainline;
	}


	/**
	 * Restituisce se l'entry è stato inserito da Amrita per simulare<br>
	 * l'inclusione di copy da parte del precompilatore.<br>
	 * <p>
	 * I copy di cui biene simulata l'inclusione vengono indicati nelle<br>
	 * direttive di esecuzione.<br>
	 * <br>
	 * Gli entries inseriti in simulazione non hanno le informazioni sulle<br>
	 * righe sorgente significative.<br>
	 * <p>
	 * @return the isEntryPrecompiler
	 */
	public boolean isEntryPrecompiler() {
		return isEntryPrecompiler;
	}



	/**
	 * Imposta se l'entry è stato inserito da Amrita per simulare<br>
	 * l'inclusione di copy da parte del precompilatore.<br>
	 * <p>
	 * I copy di cui biene simulata l'inclusione vengono indicati nelle<br>
	 * direttive di esecuzione.<br>
	 * <br>
	 * Gli entries inseriti in simulazione non hanno le informazioni sulle<br>
	 * righe sorgente significative.<br>
	 * <p>
	 * @param isEntryPrecompiler the isEntryPrecompiler to set
	 */
	public void setEntryPrecompiler(boolean isEntryPrecompiler) {
		this.isEntryPrecompiler = isEntryPrecompiler;
	}







	/**
	 * Imposta se l'entry è relativo a una istruzione nella mainline del programma.<br>
	 * <b>
	 * Si tratta di istruzioni immediatamente eseguibili allo start del programma.<br>
	 * <p>
	 * @param isMainline the isMainline to set
	 */
	public void setMainline(boolean isMainline) {
		this.isMainline = isMainline;
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
