package entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumPrecompilerReservedWords;
import enums.EnumLanguageItemInstr;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumRelationSourceProcess;
import enums.EnumRelationType;
import enums.EnumSourceType;

/**
	* Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityRelationOrigin (RelationOrigin, RELO)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella RelationOrigin e descrive l'origine di una relazione fra due oggetti.
	* la stessa relazione può essere generata a fronte di situazioni molto diverse, in differenti 
	* istruzioni dello stesso programma.<br>
	* <p>
	* vengono memorizzate le seguenti informazioni di incrocio in base al tipo di relazione:
	* <p>
	* <b>EXTERNAL-FILE-JCL-SOURCE</b><br>
	* <p>
	*  RELOTYPC typeObjectCross   	OBJECT-PHISICAL-FILE <br>
	*  RELOIDOC idObjectCross 		Phisical file name<br>
	*  RELOIN1C info1Cross   		Step name<br>
	*  RELOIN2C info2Cross   		Proc name<br>
	*  RELOIN3C info3Cross   		Exec name pgm<br>
	* <p>
	* <b>PHISICAL-FILE-JCL-SOURCE</b><br>
	* <p>
	*  RELOTYPC typeObjectCross   	OBJECT-EXTERNAL-FILE <br>
	*  RELOIDOC idObjectCross 		External file name (ddname)<br>
	*  RELOIN1C info1Cross   		Step name<br>
	*  RELOIN2C info2Cross   		Proc name<br>
	*  RELOIN3C info3Cross   		Exec name pgm<br>
	* <p>
	* <b>PGM-JCL-SOURCE</b><br>
	* <p>
	*  RELOLIBO librarySourceObject Nome oggetto LIBRARY libreria  <br>  
	*  RELOLIBS librarySourcePath   Libreria sorgente (per esempio di analisi)   <br>
	*  RELOFILS fileSource          Nome file sorgente (per esempio di analisi)  <br>
	*  RELOTYPS typeSource; 		Tipologia sorgente  <br>
 	*  RELOIN1C info1Cross   		Step name<br>
	*  RELOIN2C info2Cross   		Proc name<br>
	*  RELOIN3C info3Cross   		Exec name pgm<br>
	* 
	* <p>
	* <b>PGM-PHISICAL-FILE</b><br>
	* <p>
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
	* @see EntityDynamicValue
	* @see EntityDynamicFieldSubSetting
*/

@Entity(name="RelationOrigin")
public class EntityRelationOrigin {

	///////////////////////////////////////////////////////////////////////
    // Data Items RelationOrigin                                         //                                                     
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id()
    @Column(name="sys")
	private String system = "";            					// RELOSYST(PK) Sistema applicativo
	@Id()
    @Column(name="subSys")
	private String subSystem = "";         					// RELOSUBS(PK) Sotto sistema applicativo
	@Id()
    @Column(name="relation")
	private EnumRelation relation = null;   				// RELOTYPR(PK) Relazione codificata fra oggetti A e B (T0034 )										
	@Id()
    @Column(name="typeObjectA")
	private EnumObject typeObjectA;             			// RELOTYPA(PK) Tipo oggetto A  (T0001)
	@Id()
    @Column(name="typeSourceA")
	private EnumSourceType typeSourceA; 				    // OBJTTYPS     Tipologia sorgente (T0002)	
	@Id()
    @Column(name="idObjectA")
	private String idObjectRelA = "";          				// RELOIDOA(PK) Nome oggetto A (es. programma, file fisico,..)
	@Id()
    @Column(name="typeObjectB")
	private EnumObject typeObjectB;             			// RELOTYPB(PK) Tipo oggetto B  (T0001)
	@Id()
    @Column(name="typeSourceB")
	private EnumSourceType typeSourceB; 				    // OBJTTYPS     Tipologia sorgente (T0002)	
	@Id()
    @Column(name="idObjectB")
	private String idObjectRelB = "";          				// RELOIDOB(PK) Nome oggetto B (es. programma, file fisico,..)
	@Id()
    @Column(name="numInstrOrigin")
	private int numInstrOrigin = 0;                         // RELONISO(PK) Numero istruzione origine in pgm A (proc/env/data)
	                                                        //              o numero progressivo origine se relazione indiretta
	
	// Data    
	@Column(name="relationType")
	private EnumRelationType relationType = null;       	// RELOTREL     Tipo relazione 	(T0005)
	@Column(name="instrProgramArea")
	private EnumCobolReservedWords instrProgramArea = null; // RELOINPA     Area di programma istruzione (T0029)
	@Column(name="instrCategory")
	private EnumInstrDataCategory instrCategory = null;     // RELOINCA     Categoria istruzione (procedure o non) (T0018)
	@Column(name="relationSource")
	private EnumRelationSourceProcess relationSource = null;// RELORELS		Sorgente relazione 	(T0006)
	@Column(name="instrLang")
	private EnumLanguageItemInstr instrLang = null;         // RELOLANG     Linguaggio istruzione origine (Cobol,.. Pl1) (T0007)
	@Column(name="instrTypePrecompiler")
	private EnumPrecompilerReservedWords instrTypePrecompiler = null;  // RELOITYP     Tipo istruzione precompilatore (T0009)
	@Column(name="idObjectOrigin")
	private String idObjectOrigin = "";          		    // RELOIDOO     Nome oggetto origine della relazione (può coincidere con idObjectRelA)
	@Column(name="typeObjectOrigin")
	private EnumObject typeObjectOrigin;             	    // RELOTYOO     Tipo oggetto origine (T0001)
	@Column(name="rowStart")
	private int rowStart = 0;          		            	// RELOXXXX     Start instruction source row nel programma (es. CALL o COPY stmt)
	@Column(name="rowEnd")
	private int rowEnd = 0;          		            	// RELOXXXX     End instruction source row nel programma
	@Column(name="rowStartInCopy")
	private int rowStartInCopy = 0;          		        // RELOXXXX     Start instruction source row nel Copy
	@Column(name="rowEndInCopy")
	private int rowEndInCopy = 0;          		            // RELOXXXX     End instructionsource row nel Copy
	@Column(name="copyOrigin")
	private String copyOrigin = "";          		        // RELOXXXX     Copy dove si origina la relazione
	
                                                            // ** Informazioni valide solo per alcune relazioni **
	@Column(name="librarySourceObject")
 	private String librarySourceObject = "";  		        // RELOLIBO     Nome oggetto LIBRARY libreria   
	@Column(name="librarySourcePath")
	private String librarySourcePath = "";  				// RELOLIBS     Libreria sorgente (per esempio di analisi)  
	@Column(name="fileSource")
	private String fileSource = "";     				    // RELOFILS     Nome file sorgente (per esempio di analisi) 
	@Column(name="typeObjectCross")
	private EnumObject typeObjectCross;             	    // RELOTYPC     Tipo oggetto di cross per la relazione  (T0001) 
	@Column(name="idObjectCross")
	private String idObjectCross = "";          		    // RELOIDOC     Nome oggetto di cross per la relazione (es. programma, file fisico,..) 
	@Column(name="info1Cross")
	private String info1Cross = "";          		        // RELOIN1C     Info1 di cross per la relazione (es. step in jcl)  
	@Column(name="info2Cross")
	private String info2Cross = "";          		        // RELOIN2C     Info2 di cross per la relazione (es. proc in jcl) 
	@Column(name="info3Cross")
	private String info3Cross = "";          		        // RELOIN3C     Info3 di cross per la relazione (es. program exec in jcl) 
		
	/*
	* Costruttore
	*/
	public EntityRelationOrigin() {
		super();
		relation = EnumRelation.NOT_ASSIGNED;
		typeObjectA = EnumObject.NOT_ASSIGNED;
		typeObjectB = EnumObject.NOT_ASSIGNED;
		relationType = EnumRelationType.NOT_ASSIGNED;
		typeObjectOrigin = EnumObject.NOT_ASSIGNED;
		relationSource = EnumRelationSourceProcess.NOT_ASSIGNED;		
		instrProgramArea = EnumCobolReservedWords.NOT_ASSIGNED;
		instrCategory = EnumInstrDataCategory.NOT_ASSIGNED;
		instrLang = EnumLanguageItemInstr.NOT_ASSIGNED;		
		instrTypePrecompiler = EnumPrecompilerReservedWords.NOT_ASSIGNED;	
		typeSourceA = EnumSourceType.NOT_ASSIGNED;
		typeSourceB = EnumSourceType.NOT_ASSIGNED;
		typeObjectCross = EnumObject.NOT_ASSIGNED;		
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
	* @return the relation
	*/
	public EnumRelation getRelation() {
		return relation;
	}

	/**
	* @param relation the relation to set
	*/
	public void setRelation(EnumRelation relation) {
		this.relation = relation;
	}

	/**
	* @return the typeObjectA
	*/
	public EnumObject getTypeObjectA() {
		return typeObjectA;
	}

	/**
	* @param typeObjectA the typeObjectA to set
	*/
	public void setTypeObjectA(EnumObject typeObjectA) {
		this.typeObjectA = typeObjectA;
		if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObjectA == EnumObject.OBJECT_COPY_COBOL_ID) {
			this.typeSourceA = EnumSourceType.COBOL_COPY_ID;
		} else if (typeObjectA == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSourceA = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObjectA == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSourceA = EnumSourceType.CICS_BMS;
		} else if (typeObjectA == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObjectA == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSourceA = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSourceA = EnumSourceType.NOT_ASSIGNED;
		}	
	}

	/**
	* @return the typeObjectB
	*/
	public EnumObject getTypeObjectB() {
		return typeObjectB;
	}

	/**
	* @param typeObjectB the typeObjectB to set
	*/
	public void setTypeObjectB(EnumObject typeObjectB) {
		this.typeObjectB = typeObjectB;
		if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObjectB == EnumObject.OBJECT_COPY_COBOL_ID) {
			this.typeSourceB = EnumSourceType.COBOL_COPY_ID;
		} else if (typeObjectB == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSourceB = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObjectB == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSourceB = EnumSourceType.CICS_BMS;
		} else if (typeObjectB == EnumObject.OBJECT_SQL_SCRIPT) {
			this.typeSourceB = EnumSourceType.SQL_SCRIPT;
		} else if (typeObjectB == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObjectB == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSourceB = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSourceB = EnumSourceType.NOT_ASSIGNED;
		}	
	}


	/**
	* @return the idObjectRelA
	*/
	public String getIdObjectRelA() {
		return idObjectRelA;
	}

	/**
	* @param idObjectRelA the idObjectRelA to set
	*/
	public void setIdObjectRelA(String idObjectRelA) {
		this.idObjectRelA = idObjectRelA;
	}

	/**
	* @return the idObjectRelB
	*/
	public String getIdObjectRelB() {
		return idObjectRelB;
	}

	/**
	* @param idObjectRelB the idObjectRelB to set
	*/
	public void setIdObjectRelB(String idObjectRelB) {
		this.idObjectRelB = idObjectRelB;
	}

	/**
	* @return the relationType
	*/
	public EnumRelationType getRelationType() {
		return relationType;
	}

	/**
	* @param relationType the relationType to set
	*/
	public void setRelationType(EnumRelationType relationType) {
		this.relationType = relationType;
	}

	/**
	 * @return the instrProgramArea
	 */
	public EnumCobolReservedWords getInstrProgramArea() {
		return instrProgramArea;
	}

	/**
	 * @param instrProgramArea the instrProgramArea to set
	 */
	public void setInstrProgramArea(EnumCobolReservedWords instrProgramArea) {
		this.instrProgramArea = instrProgramArea;
	}

	/**
	 * @return the instrCategory
	 */
	public EnumInstrDataCategory getInstrCategory() {
		return instrCategory;
	}

	/**
	 * @param instrCategory the instrCategory to set
	 */
	public void setInstrCategory(EnumInstrDataCategory instrCategory) {
		this.instrCategory = instrCategory;
	}

	/**
	* @return the numInstrOrigin
	*/
	public int getNumInstrOrigin() {
		return numInstrOrigin;
	}

	/**
	* @param numInstrOrigin the numInstrOrigin to set
	*/
	public void setNumInstrOrigin(int numInstrOrigin) {
		this.numInstrOrigin = numInstrOrigin;
	}

	/**
	* @return the idObjectPgmOrigin
	*/
	public String getIdObjectOrigin() {
		return idObjectOrigin;
	}

	/**
	* @param idObjectOrigin the idObjectOrigin to set
	*/
	public void setIdObjectOrigin(String idObjectOrigin) {
		this.idObjectOrigin = idObjectOrigin;
	}

	/**
	* @return the typeObjectOrigin
	*/
	public EnumObject getTypeObjectOrigin() {
		return typeObjectOrigin;
	}

	/**
	* @param typeObjectPgmOrigin the typeObjectPgmOrigin to set
	*/
	public void setTypeObjectOrigin(EnumObject typeObjectOrigin) {
		this.typeObjectOrigin = typeObjectOrigin;
	}


	/**
	* @return the relationSource
	*/
	public EnumRelationSourceProcess getRelationSource() {
		return relationSource;
	}

	/**
	* @param relationSource the relationSource to set
	*/
	public void setRelationSource(EnumRelationSourceProcess relationSource) {
		this.relationSource = relationSource;
	}

	/**
	* @return the instrLang
	*/
	public EnumLanguageItemInstr getInstrLang() {
		return instrLang;
	}

	/**
	* @param instrLang the instrLang to set
	*/
	public void setInstrLang(EnumLanguageItemInstr instrLang) {
		this.instrLang = instrLang;
	}

	/**
	* @return the instrTypePrecompiler
	*/
	public EnumPrecompilerReservedWords getInstrTypePrecompiler() {
		return instrTypePrecompiler;
	}

	/**
	* @param instrType the instrType to set
	*/
	public void setInstrTypePrecompiler(EnumPrecompilerReservedWords instrType) {
		this.instrTypePrecompiler = instrType;
	}

	/**
	 * Restituisce il nome dell'oggetto Library dal quale il programma,
	 * il source jcl, il source DDL etc, origine della relazione, è stato recuperato. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @return the librarySourceObject
	 */
	public String getLibrarySourceObject() {
		return librarySourceObject;
	}

	/**
	 * Imposta il nome dell'oggetto Library dal quale il programma,
	 * il source jcl, il source DDL etc, origine della relazione, è stato recuperato. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @param librarySourceObject the librarySourceObject to set
	 */
	public void setLibrarySourceObject(String librarySourceObject) {
		this.librarySourceObject = librarySourceObject;
	}

	/**
	 * Restituisce il path della Library dal quale il programma,
	 * il source jcl, il source DDL etc, origine della relazione, è stato recuperato. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @return the librarySourcePasth
	 */
	public String getLibrarySourcePath() {
		return librarySourcePath;
	}

	/**
	 * Imposta il path della Library dal quale il programma,
	 * il source jcl, il source DDL etc, origine della relazione, è stato recuperato. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @param librarySource the librarySource to set
	 */
	public void setLibrarySourcePath(String librarySourcePath) {
		this.librarySourcePath = librarySourcePath;
	}

	/**
	 * Restituisce il nome del file sorgente dove ha origine la relazione, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @return the fileSource
	 */
	public String getFileSource() {
		return fileSource;
	}

	/**
	 * Imposta il nome del file sorgente dove ha origine la relazione, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * @param fileSource the fileSource to set
	 */
	public void setFileSource(String fileSource) {
		this.fileSource = fileSource;
	}

	/**
	 * Restituisce il tipo del file sorgente dove ha origine la relazione, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @return the typeSource
	 */
	public EnumSourceType getTypeSourceA() {
		return typeSourceA;
	}

	/**
	 * Restituisce il tipo del file sorgente dell'oggetto relazionato, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @return the typeSource
	 */
	public EnumSourceType getTypeSourceB() {
		return typeSourceB;
	}

	/**
	 * Imposta il tipo del file sorgente dove ha origine la relazione, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @param typeSource the typeSource to set
	 */
	public void setTypeSourceA(EnumSourceType typeSourceA) {
		this.typeSourceA = typeSourceA;
	}

	/**
	 * Imposta il tipo del file sorgente relazionato, come
	 * il membro source jcl, il source DDL etc. <br>
	 * <p>
	 * Per esempio durante l'analisi del jcl vengono inserite le relazioni
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * nello step di exec, l'external file è la ddname e il phisical file è
	 * il dsname.
	 * 
	 * 
	 * @param typeSource the typeSource to set
	 */
	public void setTypeSourceB(EnumSourceType typeSourceB) {
		this.typeSourceB = typeSourceB;
	}


	/**
	 * Restituisce il tipo di oggetto che scaturissce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_INTERNAL_FILE, viene resituito
	 * il tipo oggetto EXTERNAL_FILE. <br>
	 * A fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * il tipo oggetto PHISICAL_FILE. <br>
	 * <p>
	 * Le ultime due informazioni sono inserite a fronte dell'analisi del jcl e
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * rappresentano informazioni di incrocio che valgono solo per quel tipo di
	 * relazioni.
	 * 
	 * @return the typeObjectCross
	 */
	public EnumObject getTypeObjectCross() {
		return typeObjectCross;
	}

	/**
	 * Imposta il tipo di oggetto che scaturissce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_INTERNAL_FILE, viene resituito
	 * il tipo oggetto EXTERNAL_FILE. <br>
	 * A fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * il tipo oggetto PHISICAL_FILE. <br>
	 * <p>
	 * Le ultime due informazioni sono inserite a fronte dell'analisi del jcl e
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * rappresentano informazioni di incrocio che valgono solo per quel tipo di
	 * relazioni.
	 * 
	 * @param typeObjectCross the typeObjectCross to set
	 */
	public void setTypeObjectCross(EnumObject typeObjectCross) {
		this.typeObjectCross = typeObjectCross;
	}

	/**
	 * Restituisce il nome dell'oggetto che scaturissce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_INTERNAL_FILE, viene resituito
	 * il nome dell'oggetto EXTERNAL_FILE che è la ddname. <br>
	 * A fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * il nome dell'oggetto PHISICAL_FILE che è il dsname della ddname. <br>
	 * <p>
	 * Le ultime due informazioni sono inserite a fronte dell'analisi del jcl e
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * rappresentano informazioni di incrocio che valgono solo per quel tipo di
	 * relazioni.
	 * 
	 * @return the idObjectCross
	 */
	public String getIdObjectCross() {
		return idObjectCross;
	}

	/**
	 * Imposta il nome dell'oggetto che scaturissce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_INTERNAL_FILE, viene resituito
	 * il nome dell'oggetto EXTERNAL_FILE che è la ddname. <br>
	 * A fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * il nome dell'oggetto PHISICAL_FILE che è il dsname della ddname. <br>
	 * <p>
	 * Le ultime due informazioni sono inserite a fronte dell'analisi del jcl e
	 * PGM_EXTERNAL_FILE o PGM_PHISICAL_FILE, dove il nome del programma è dichiarato
	 * rappresentano informazioni di incrocio che valgono solo per quel tipo di
	 * relazioni.
	 * 
	 * 
	 * @param idObjectCross the idObjectCross to set
	 */
	public void setIdObjectCross(String idObjectCross) {
		this.idObjectCross = idObjectCross;
	}

	/**
	 * Restituisce un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * lo step del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserite a fronte dell'analisi del jcl.
	 * 
	 * @return the info1Cross
	 */
	public String getInfo1Cross() {
		return info1Cross;
	}

	/**
	 * Imposta un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * lo step del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserite a fronte dell'analisi del jcl.
	 * 
	 * @param info1Cross the info1Cross to set
	 */
	public void setInfo1Cross(String info1Cross) {
		this.info1Cross = info1Cross;
	}

	/**
	 * Restituisce un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * la proc del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserite a fronte dell'analisi del jcl.
	 * 
	 * 
	 * @return the info2Cross
	 */
	public String getInfo2Cross() {
		return info2Cross;
	}

	/**
	 * Imposta un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_EXTERNAL_FILE, viene resituito
	 * la proc del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserite a fronte dell'analisi del jcl.
	 * 
	 * 
	 * 
	 * @param info2Cross the info2Cross to set
	 */
	public void setInfo2Cross(String info2Cross) {
		this.info2Cross = info2Cross;
	}

	/**
	 * Restituisce un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_PHISICAL_FILE, viene resituito
	 * il programma eseguito allo step del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserite a fronte dell'analisi del jcl.
	 * 
	 * 
	 * @return the info3Cross
	 */
	public String getInfo3Cross() {
		return info3Cross;
	}

	/**
	 * Imposta un'informazione specifica che scaturisce dall'origine
	 * della relazione fra due oggetti<br>
	 * <p>
	 * L'informazione è opzionale.<br>
	 * Per esempio a fronte della relazione PGM_PHISICAL_FILE, viene resituito
	 * il programma eseguito allo step del jcl dove il file esterno è dichiarato. <br>
	 * Il significato di questa informazione sdipende dasl tipo di relazione.
	 * <p>
	 * Questa informazione è inserita a fronte dell'analisi del jcl.
	 * 
	 * 
	 * 
	 * @param info3Cross the info3Cross to set
	 */
	public void setInfo3Cross(String info3Cross) {
		this.info3Cross = info3Cross;
	}

	
	/**
	 * @return the rowStart
	 */
	public int getRowStart() {
		return rowStart;
	}

	/**
	 * @param rowStart the rowStart to set
	 */
	public void setRowStart(int rowStart) {
		this.rowStart = rowStart;
	}

	/**
	 * @return the rowEnd
	 */
	public int getRowEnd() {
		return rowEnd;
	}

	/**
	 * @param rowEnd the rowEnd to set
	 */
	public void setRowEnd(int rowEnd) {
		this.rowEnd = rowEnd;
	}

	
	/**
	 * @return the rowStartInCopy
	 */
	public int getRowStartInCopy() {
		return rowStartInCopy;
	}

	/**
	 * @param rowStartInCopy the rowStartInCopy to set
	 */
	public void setRowStartInCopy(int rowStartInCopy) {
		this.rowStartInCopy = rowStartInCopy;
	}

	/**
	 * @return the rowEndInCopy
	 */
	public int getRowEndInCopy() {
		return rowEndInCopy;
	}

	/**
	 * @param rowEndInCopy the rowEndInCopy to set
	 */
	public void setRowEndInCopy(int rowEndInCopy) {
		this.rowEndInCopy = rowEndInCopy;
	}

	/**
	 * @return the copyOrigin
	 */
	public String getCopyOrigin() {
		return copyOrigin;
	}

	/**
	 * @param copyOrigin the copyOrigin to set
	 */
	public void setCopyOrigin(String copyOrigin) {
		this.copyOrigin = copyOrigin;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityRelationOrigin objEntitytRelationOrigin = null;
		
		objEntitytRelationOrigin = (EntityRelationOrigin) obj;
		 
		return this.system.equals(objEntitytRelationOrigin.system)
		 &&    this.subSystem.equals(objEntitytRelationOrigin.subSystem)
		 &&    this.relation == objEntitytRelationOrigin.relation
		 &&    this.idObjectRelA.equals(objEntitytRelationOrigin.idObjectRelA)
		 &&    this.idObjectRelB.equals(objEntitytRelationOrigin.idObjectRelB)
		 &&    this.typeObjectA == objEntitytRelationOrigin.typeObjectA
		 &&    this.typeObjectB == objEntitytRelationOrigin.typeObjectB
		 &&    this.numInstrOrigin == objEntitytRelationOrigin.numInstrOrigin;
		 
	}

	public int compareTo(Object obj) {
		EntityRelationOrigin objToCompare = null;
		objToCompare = (EntityRelationOrigin) obj;
		
		if (this.system.compareTo(objToCompare.getSystem()) != 0) {
			return this.system.compareTo(objToCompare.getSystem());
		}
		if (this.subSystem.compareTo(objToCompare.getSubSystem()) != 0) {
			return this.subSystem.compareTo(objToCompare.getSubSystem());
		}
		if (this.relation.compareTo(objToCompare.getRelation()) != 0) {
			return this.relation.compareTo(objToCompare.getRelation());
		}
		if (this.typeObjectA.compareTo(objToCompare.getTypeObjectA()) != 0) {
			return this.typeObjectA.compareTo(objToCompare.getTypeObjectA());
		}
		if (this.idObjectRelA.compareTo(objToCompare.getIdObjectRelA()) != 0) {
			return this.idObjectRelA.compareTo(objToCompare.getIdObjectRelA());
		}
		if (this.typeObjectB.compareTo(objToCompare.getTypeObjectB()) != 0) {
			return this.typeObjectB.compareTo(objToCompare.getTypeObjectB());
		}
		if (this.idObjectRelB.compareTo(objToCompare.getIdObjectRelB()) != 0) {
			return this.idObjectRelB.compareTo(objToCompare.getIdObjectRelB());
		}
		if ((this.numInstrOrigin - objToCompare.getNumInstrOrigin()) != 0) {
 			return (this.numInstrOrigin - objToCompare.getNumInstrOrigin());
 		}
		return 0;
	}

}
