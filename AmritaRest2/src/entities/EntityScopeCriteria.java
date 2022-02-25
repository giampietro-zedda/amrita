package entities;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedForAny;
import analyzer.DataBaseMappedForAnys;
import analyzer.DataBaseMappedTable;
import enums.EnumCobolReservedWords;
import enums.EnumCobolUsage;
import enums.EnumPrecompilerOptionsInstruction;
import enums.EnumPrecompilerReservedWords;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumRelation;
import enums.EnumScopeSection;
import enums.EnumTable;
import enums.EnumWhereUsedType;

/**
     * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityScopeCriteria (SCPC)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella ScopeCriteria e descrive le condizioni che ogni specifica sezione di scope deve soddisfare. <br>
	 * La condizione, come un'opzione di programma, un accesso in output a una colonna di tabella, il matching del nome del programma <br>
	 * con una regular expression etc, deve essere verificata come vera o falsa, come indicato nella sezione.<br>
	 * Ogni sezione viene quindi valutata singolarmente come una combinazione strutturata, anche con parentesi, di condizioni vere o false.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObject 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityRelation
	 * @see EntityRelationOrigin
	 * @see EntityCopyEntityDefinition
	 * @see EntityWhereUsedItem
	 * @see EntityMapDescriptor
	 * @see EntityMapItem
	 * @see EntityTagValue
	 * @see EntityDynamicValueExt
	 * @see EntityScopeHeader
	 * @see EntityScopeSection
	 * @see EntityScopeItem
	 * @see EntityScopeChild
	 * @see EntityScopeProgram
	 * @see EntityScopeObject
	 * @see EntityScopeRelation
	 * @see EntityProcessLog
	 * @see EntityMetric
	 * @see EntityTableHeader    
	 * @see EntityTableStructure   
	 * @see EntityTableData 
	 * @see EntityDynamicField
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicFieldSubValue
	 * @see EntityDynamicFieldSubSetting
*/

@DataBaseMappedTable("ScopeCriteria")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
			                "system    		   sys  	         PK"
						   ,"subSystem 		   subSys    	     PK"    
						   ,"idScope 		   idScope 	         PK"    
						   ,"section 		   section 	         PK"    
						   ,"idObject          idObject 	     PK"    
						   ,"numeSeqItem 	   numeSeqItem       PK"   
						   // Colonne senza vincoli di chiave (No key)
						   ,"mustBeTrue        mustBeTrue        NK"   
						   ,"posFrom 		   posFrom           NK"   
						   ,"length			   length            NK"   
						   ,"regularExpr 	   regularExpr       NK"   
						   ,"valueToBeMatched  valueToBeMatched  NK"   
						   ,"dateAnalysis 	   dateAnalysis      NK"   
						   ,"fieldName 		   fieldName         NK"   
						   ,"underCopy 	       underCopy         NK"   
						   ,"underGroupName    underGroupName    NK"   
						   ,"underGroup 	   underGroup        NK"   
						   ,"underRedefines    underRedefines    NK"   
						   ,"occurs 		   occurs            NK"   
						   ,"occursFrom 	   occursFrom        NK"   
						   ,"occursTo 		   occursTo          NK"   
						   ,"pictureCobol 	   pictureCobol      NK"   
						   ,"usageCobol 	   usageCobol        NK"   
						   ,"numInt 		   numInt            NK"   
						   ,"numDec 		   numDec            NK"   
						   ,"sizeBytes 	       sizeBytes         NK"   
						   ,"instrCobol 	   instrCobol        NK"   
						   ,"instrCics 		   instrCics         NK"   
						   ,"instrCicsOption   instrCicsOption   NK"   
						   ,"programOption 	   programOption     NK"   
						   ,"typeObjectRelated typeObjectRelated NK"   
						   ,"idObjectRelated   idObjectRelated   NK"   
						   ,"relPgmOther 	   relPgmOtherx      NK"   

                             }
         )
/*
@DataBaseMappedForAnys(forAny = {@DataBaseMappedForAny(entity = "EntityScopeHeader",
                                                       colsBound = {"system      system"
                                                                   ,"subSystem   subSystem"    
                                                                   ,"idScope 	 idScope"    
                                                                   }
                                                      )
                                } 
                      )
*/

public class EntityScopeCriteria {

	///////////////////////////////////////////////////////////////////////
    // Data Items ScopeSection                                           //                                                        //
    ///////////////////////////////////////////////////////////////////////
		
	// Primary key
	private String system = "";            								// Sistema applicativo
	private String subSystem = "";         								// Sotto sistema applicativo
	private String idScope = "";          	    						// Identificativo scope  
	private EnumScopeSection section = null;    						// Sezione scope (o parentesi aperta/chiusa) (T0044)
	private String idObject = "";                 						// Nome copy/entity/oggetto relazionato (duplicato per univocità chiave)
	private int numeSeqItem = 0;                                        // Numero sequenza in EntityCopyEntityDefinition  per le sezioni che lo prevedono                                                          
	
	// Data
	private boolean mustBeTrue = false;                 				// True indica programma da portare in scope solo se verificata la condizione in section

	                                                                    // * Individuazione sorgente o campo/label/section definito dentro sorgente *
	private int posFrom = 0;		        		    				// Posizione nel nome sorgente 0-based
	private int length = 0;		            		    				// Lunghezza da posFrom
	private String regularExpr = "";		    						// Regular expression in alternativa da applicare ai nomi dei sorgenti/entiity/
	private String valueToBeMatched = "";		    					// Valore in nome programma/campo da trovare
													    				// Valore in Nome di campo che deve essere presente/assente nel programma
													    				// Valore in Label che deve essere presente/assente nel programma
													    				// Valore in Section che deve essere presente/assente nel programma
													    				// Istruzione Cobol che deve essere presente/assente nel programma
													    				// Istruzione Cics che deve essere presente/assente nel programma
													    				// Opzione istruzione Cics che deve essere presente/assente nel programma
	private String dateAnalysis = "";                                   // Data analisi sorgente, lo scope individua sorgenti con data >=
	
	                                                                    // * Caratteristiche campo che deve essere definito/non definito nel source *
	private String fieldName = "";                                      // Nome campo che deve esssere definito/non definito nel source
	private String underCopy = "";                                      // Nome copy sotto il quale deve essere definito il campo
	private String underGroupName = "";                                 // Nome campo di gruppo sotto il quale deve essere definito/non definito il campo 
	private boolean underGroup = false;                                 // True se il campo deve essere definito/non definito sotto un gruppo
    private boolean underRedefines = false;                             // True se il campo è definita/non definita sotto un campo con clausola redefines
    private boolean withRedefines = false;                              // True se il campo ha definita/non definita la clausola redefines
	private int occurs = 0;                                             // Numero occurs con cui il campo deve essere definito/non definito
	private int occursFrom = 0;                                         // Numero occurs from con cui il campo deve essere definito/non definito
	private int occursTo = 0;                                           // Numero occurs to con cui il campo deve essere definito/non definito
    private String pictureCobol = "";                                   // Picture Cobol come codificata nel source che deve essere presente/assente per il campo
    private EnumCobolUsage usageCobol = null;                           // Usage Cobol che deve essere presente/assente per il campo (T0016)
	private int numInt = 0;											    // Numero interi che devono essere definiti per il campo
	private int numDec = 0;											    // Numero decimali che devono essere definiti per il campo
    private int sizeBytes = 0;										    // Lunghezza in bytes definiti per il campo
	
	                                                                    // * Istruzioni Cobol/Cics del programma *                                                       
	private EnumCobolReservedWords instrCobol = null;   				// Istruzione cobol di cui verificare presenza/assenza (T0029)
	private EnumPrecompilerReservedWords instrCics = null;   			// Istruzione cobol di cui verificare presenza/assenza (T0009)
	private EnumPrecompilerOptionsInstruction instrCicsOption = null; 	// Opzione che l'istruzione Cics deve avere/non avere (T0015)
	                                                                    
																		// * Opzioni e relazioni con altri oggetti (per esempio Copy, entity, entity_read ..) *
	private EnumObjectOption programOption = null;      				// Opzione programma che deve avere il programma (T0004) PGM_WITH_I_O_SCREEN, PGM_WITH_I_O_SQL,....
	private EnumObject typeObjectRelated = null; 						// Tipo oggetto relazionato/acceduto (T0001)
	private String idObjectRelated = "";                				// Nome oggetto relazionato/acceduto (Nome tabella, segmento Dl1, etc.)
	private EnumRelation relPgmOther = null;							// Relazione programma con Entity/phisical file/external file/.. (T0034) 
																		//  PGM_ENTITY, PGM_ENTITY_READ, ... PGM_ENTITY_INSERT   
																		//  PGM_PHISICAL_FILE, PGM_PHISICAL_FILE_READ, ... PPGM_PHISICAL_FILE_INSERT 
																		//  PGM_INTERNAL_FILE, PGM_INTERNAL_FILE_READ, ... PGM_INTERNAL_FILE_INSERT   
																		//  PGM_EXTERNAL_FILE, PGM_EXTERNAL_FILE_READ, ... PGM_EXTERNAL_FILE_INSERT   	
                                                        				//  PGM_COPY_BOOK
	
	                                                                    // * Accesso a campo di copy o colonna di tabella *
	private String copyItemOrColumn = "";                         		// Nome campo copy o nome colonna
	private EnumWhereUsedType typeIOitemColumn = null;      			// Utilizzo campo/colonna in input o output (T0010)
	
	
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public EntityScopeCriteria() {
		super();
		section = EnumScopeSection.NOT_ASSIGNED;
		usageCobol = EnumCobolUsage.NOT_ASSIGNED;
		instrCobol = EnumCobolReservedWords.NOT_ASSIGNED;
		instrCics = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		instrCicsOption = EnumPrecompilerOptionsInstruction.NOT_ASSIGNED;
		programOption = EnumObjectOption.NOT_ASSIGNED;
		typeObjectRelated = EnumObject.NOT_ASSIGNED;
		relPgmOther = EnumRelation.NOT_ASSIGNED;
		typeIOitemColumn = EnumWhereUsedType.NOT_ASSIGNED;
		 

	}

	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}

	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}

	/**
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}

	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}

	/**
	 * @return the idScope
	 */
	public String getIdScope() {
		return idScope;
	}

	/**
	 * @param idScope the idScope to set
	 */
	public void setIdScope(String idScope) {
		this.idScope = idScope;
	}

	/**
	 * @return the section
	 */
	public EnumScopeSection getSection() {
		return section;
	}

	/**
	 * @param section the section to set
	 */
	public void setSection(EnumScopeSection section) {
		this.section = section;
	}

	/**
	 * @return the posFrom
	 */
	public int getPosFrom() {
		return posFrom;
	}

	/**
	 * @param posFrom the posFrom to set
	 */
	public void setPosFrom(int posFrom) {
		this.posFrom = posFrom;
	}

	/**
	 * @return the length
	 */
	public int getLength() {
		return length;
	}

	/**
	 * @param length the length to set
	 */
	public void setLength(int length) {
		this.length = length;
	}

	
	/**
	 * @return the mustBeTrue
	 */
	public boolean isMustBeTrue() {
		return mustBeTrue;
	}

	/**
	 * @param mustBeTrue the mustBeTrue to set
	 */
	public void setMustBeTrue(boolean mustBeTrue) {
		this.mustBeTrue = mustBeTrue;
	}

	/**
	 * @return the regularExpr
	 */
	public String getRegularExpr() {
		return regularExpr;
	}

	/**
	 * @param regularExpr the regularExpr to set
	 */
	public void setRegularExpr(String regularExpr) {
		this.regularExpr = regularExpr;
	}

	/**
	 * @return the instrCobol
	 */
	public EnumCobolReservedWords getInstrCobol() {
		return instrCobol;
	}

	/**
	 * @param instrCobol the instrCobol to set
	 */
	public void setInstrCobol(EnumCobolReservedWords instrCobol) {
		this.instrCobol = instrCobol;
	}

	/**
	 * @return the instrCics
	 */
	public EnumPrecompilerReservedWords getInstrCics() {
		return instrCics;
	}

	/**
	 * @param instrCics the instrCics to set
	 */
	public void setInstrCics(EnumPrecompilerReservedWords instrCics) {
		this.instrCics = instrCics;
	}

	/**
	 * @return the instrCicsOption
	 */
	public EnumPrecompilerOptionsInstruction getInstrCicsOption() {
		return instrCicsOption;
	}

	/**
	 * @param instrCicsOption the instrCicsOption to set
	 */
	public void setInstrCicsOption(EnumPrecompilerOptionsInstruction instrCicsOption) {
		this.instrCicsOption = instrCicsOption;
	}

	/**
	 * @return the programOption
	 */
	public EnumObjectOption getProgramOption() {
		return programOption;
	}

	/**
	 * @param programOption the programOption to set
	 */
	public void setProgramOption(EnumObjectOption programOption) {
		this.programOption = programOption;
	}

	/**
	 * @return the typeObjectRelated
	 */
	public EnumObject getTypeObjectRelated() {
		return typeObjectRelated;
	}

	/**
	 * @param typeObjectRelated the typeObjectRelated to set
	 */
	public void setTypeObjectRelated(EnumObject typeObjectRelated) {
		this.typeObjectRelated = typeObjectRelated;
	}

	/**
	 * @return the idObjectRelated
	 */
	public String getIdObjectRelated() {
		return idObjectRelated;
	}

	/**
	 * @param idObjectRelated the idObjectRelated to set
	 */
	public void setIdObjectRelated(String idObjectRelated) {
		this.idObjectRelated = idObjectRelated;
	}

	/**
	 * @return the relPgmOther
	 */
	public EnumRelation getRelPgmOther() {
		return relPgmOther;
	}

	/**
	 * @param relPgmOther the relPgmOther to set
	 */
	public void setRelPgmOther(EnumRelation relPgmOther) {
		this.relPgmOther = relPgmOther;
	}

	

	/**
	 * @return the idObject
	 */
	public String getIdObject() {
		return idObject;
	}

	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}

	/**
	 * @return the numeSeqItem
	 */
	public int getNumeSeqItem() {
		return numeSeqItem;
	}

	/**
	 * @param numeSeqItem the numeSeqItem to set
	 */
	public void setNumeSeqItem(int numeSeqItem) {
		this.numeSeqItem = numeSeqItem;
	}

	/**
	 * @return the valueToBeMatched
	 */
	public String getValueToBeMatched() {
		return valueToBeMatched;
	}

	/**
	 * @param valueToBeMatched the valueToBeMatched to set
	 */
	public void setValueToBeMatched(String valueToBeMatched) {
		valueToBeMatched = valueToBeMatched;
	}

	/**
	 * @return the dateAnalysis
	 */
	public String getDateAnalysis() {
		return dateAnalysis;
	}

	/**
	 * @param dateAnalysis the dateAnalysis to set
	 */
	public void setDateAnalysis(String dateAnalysis) {
		this.dateAnalysis = dateAnalysis;
	}

	/**
	 * @return the fieldName
	 */
	public String getFieldName() {
		return fieldName;
	}

	/**
	 * @param fieldName the fieldName to set
	 */
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	/**
	 * @return the underCopy
	 */
	public String getUnderCopy() {
		return underCopy;
	}

	/**
	 * @param underCopy the underCopy to set
	 */
	public void setUnderCopy(String underCopy) {
		this.underCopy = underCopy;
	}

	/**
	 * @return the underGroupName
	 */
	public String getUnderGroupName() {
		return underGroupName;
	}

	/**
	 * @param underGroupName the underGroupName to set
	 */
	public void setUnderGroupName(String underGroupName) {
		this.underGroupName = underGroupName;
	}

	/**
	 * @return the underGroup
	 */
	public boolean isUnderGroup() {
		return underGroup;
	}

	/**
	 * @param underGroup the underGroup to set
	 */
	public void setUnderGroup(boolean underGroup) {
		this.underGroup = underGroup;
	}

	/**
	 * @return the underRedefines
	 */
	public boolean isUnderRedefines() {
		return underRedefines;
	}

	/**
	 * @param underRedefines the underRedefines to set
	 */
	public void setUnderRedefines(boolean underRedefines) {
		this.underRedefines = underRedefines;
	}

	/**
	 * @return the withRedefines
	 */
	public boolean isWithRedefines() {
		return withRedefines;
	}

	/**
	 * @param withRedefines the withRedefines to set
	 */
	public void setWithRedefines(boolean withRedefines) {
		this.withRedefines = withRedefines;
	}

	/**
	 * @return the occurs
	 */
	public int getOccurs() {
		return occurs;
	}

	/**
	 * @param occurs the occurs to set
	 */
	public void setOccurs(int occurs) {
		this.occurs = occurs;
	}

	/**
	 * @return the occursFrom
	 */
	public int getOccursFrom() {
		return occursFrom;
	}

	/**
	 * @param occursFrom the occursFrom to set
	 */
	public void setOccursFrom(int occursFrom) {
		this.occursFrom = occursFrom;
	}

	/**
	 * @return the occursTo
	 */
	public int getOccursTo() {
		return occursTo;
	}

	/**
	 * @param occursTo the occursTo to set
	 */
	public void setOccursTo(int occursTo) {
		this.occursTo = occursTo;
	}

	/**
	 * @return the pictureCobol
	 */
	public String getPictureCobol() {
		return pictureCobol;
	}

	/**
	 * @param pictureCobol the pictureCobol to set
	 */
	public void setPictureCobol(String pictureCobol) {
		this.pictureCobol = pictureCobol;
	}

	/**
	 * @return the usageCobol
	 */
	public EnumCobolUsage getUsageCobol() {
		return usageCobol;
	}

	/**
	 * @param usageCobol the usageCobol to set
	 */
	public void setUsageCobol(EnumCobolUsage usageCobol) {
		this.usageCobol = usageCobol;
	}

	/**
	 * @return the numInt
	 */
	public int getNumInt() {
		return numInt;
	}

	/**
	 * @param numInt the numInt to set
	 */
	public void setNumInt(int numInt) {
		this.numInt = numInt;
	}

	/**
	 * @return the numDec
	 */
	public int getNumDec() {
		return numDec;
	}

	/**
	 * @param numDec the numDec to set
	 */
	public void setNumDec(int numDec) {
		this.numDec = numDec;
	}

	/**
	 * @return the sizeBytes
	 */
	public int getSizeBytes() {
		return sizeBytes;
	}

	/**
	 * @param sizeBytes the sizeBytes to set
	 */
	public void setSizeBytes(int sizeBytes) {
		this.sizeBytes = sizeBytes;
	}

	/**
	 * @return the copyItemOrColumn
	 */
	public String getCopyItemOrColumn() {
		return copyItemOrColumn;
	}

	/**
	 * @param copyItemOrColumn the copyItemOrColumn to set
	 */
	public void setCopyItemOrColumn(String copyItemOrColumn) {
		this.copyItemOrColumn = copyItemOrColumn;
	}

	/**
	 * @return the typeIOitemColumn
	 */
	public EnumWhereUsedType getTypeIOitemColumn() {
		return typeIOitemColumn;
	}

	/**
	 * @param typeIOitemColumn the typeIOitemColumn to set
	 */
	public void setTypeIOitemColumn(EnumWhereUsedType typeIOitemColumn) {
		this.typeIOitemColumn = typeIOitemColumn;
	}

}
