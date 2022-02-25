package entities;

import enums.EnumLanguageItemInstr;
import enums.EnumPrecompilerReservedWords;
import enums.EnumObject;
import enums.EnumWhereUsedType;
import enums.EnumWhereUsedTypeAlias;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import analyzer.Analyzer;

/**
 	 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityWhereUsedItem (WhereUsedItem, WHRI)
	 * </h1>
	 *  <p>
	 * Questa classe mappa la tabella WhereUsedItem e descrive l'utilizzo (Where Used) di un <b>campo</b> di un modulo
	 * copy o di una tabella DB2 o di una mappa etc., descritto dall'entity {@link EntityCopyEntityDefinition}.
	 * Ogni riga di questa EntityWhereUsedItem indica il nome del programma, il tipo di programma e
	 * il numero di istruzione.
	 * Viene inoltre indicato il nome locale con il quale l'item è referenziato, ovvero l'Alias e la
	 * sua tipologia. Nel caso di alias di campi relativi ad archivi non database (es. files Vsam),
	 * l'alias individuato potrebbe coincidere solo parzialmente. Per esempio un programma
	 * potrebbe non utilizzare il modulo copy ufficiale descrivente il tracciato record con un match
	 * solo parziale del campo definito.
	 * Gli utilizzi degli oggetti complessi quali moduli copy, entities, files fisici etc, vengono
	 * invece soddisfatti attraverso le entities {@link EntityRelation} e {@link EntityRelationOrigin}.
	 * 
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

 
@Entity(name="WhereUsed")
public class EntityWhereUsedItem {

	///////////////////////////////////////////////////////////////////////
    // Data Items WhereUsedItem                                          
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            			// Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         			// Sotto sistema applicativo 
	@Id
	@Column(name="idObject")
	private String idObject = "";          			// Nome oggetto (Copy, Entity, Map)
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       	// Tipologia oggetto (T0001)
	@Id
	@Column(name="numSeq")
	private int numSeq = 0;                     	// Numero sequenza definizione in CPYE campo/colonna
	@Id
	@Column(name="idField")
	private String idField = "";                	// Nome campo/colonna come definito in descrittore entitity/copy/mapItem 
	@Id
	@Column(name="idObjectRefer")
	private String idObjectRefer = "";          	// Nome oggetto dove il campo è referenziato (Programma)
	@Id
	@Column(name="typeObjectRefer")
	private EnumObject typeObjectRefer = null;    	// Tipologia oggetto programma dove il campo è referenziato (T0001)
	@Id
	@Column(name="numInstrRefer")
	private int numInstrRefer = 0;              	// Numero istruzione in programma/copy dove il campo è referenziato
    
	// Data
	@Column(name="idAliasLocal")
	private String idAliasLocal = "";           	// Nome campo locale utilizzato nel programma
	@Column(name="idIoarea")
	private String idIoarea = "";               	// Nome Ioarea locale sotto cui il campo è definito
	@Column(name="idObjectCopy")
	private String idObjectCopy = "";           	// Nome oggetto copy  sotto cui l'alias è definito 
	@Column(name="typeWhereUsed")
    private EnumWhereUsedType typeWhereUsed = null; // Tipo where used (INPUT,OUTPUT) (T0010)
	@Column(name="typeAlias")
    private EnumWhereUsedTypeAlias typeAlias = null;// Tipo Alias (FULL, LOWER,..) (T0011)
	@Column(name="posInIoarea")
	private int posInIoarea = 0;                	// Posizione campo in ioarea (0-based)
	@Column(name="usedBytes")
	private int usedBytes = 0;                   	// Bytes utilizzati nel campo
	@Column(name="numInt")
	private int numInt = 0;                     	// umero interi campo
	@Column(name="numDec")
	private int numDec = 0;                        	// Numero decimali campo
	@Column(name="signed")
	private boolean signed = false;             	// Campo definito con segno
	@Column(name="instrLang")
	private EnumLanguageItemInstr instrLang = null; // Linguaggio istruzione (Cobol, Pl1, ..)  (T0007)
	@Column(name="instrType")
	private EnumPrecompilerReservedWords instrType = null; // Tipo istruzione codificata (T0009)
	@Column(name="rowStart")
	private int rowStart = 0;          		        // Start instructionsource row
	@Column(name="rowEnd")
	private int rowEnd = 0;          		        // End instructionsource row
	@Column(name="rowStartInCopy")
	private int rowStartInCopy = 0;          		// Start instruction source row nel Copy
	@Column(name="rowEndInCopy")
	private int rowEndInCopy = 0;          		    // End instructionsource row nel Copy

	
	/*
	 * Costruttore
	 */
	public EntityWhereUsedItem() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObjectRefer = EnumObject.NOT_ASSIGNED;
		typeWhereUsed = EnumWhereUsedType.NOT_ASSIGNED;	  
		typeAlias = EnumWhereUsedTypeAlias.NOT_ASSIGNED;	
		instrLang = EnumLanguageItemInstr.NOT_ASSIGNED;
		instrType = EnumPrecompilerReservedWords.NOT_ASSIGNED;		
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
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}

	/**
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
	}

	/**
	 * @return the numSeq
	 */
	public int getNumSeq() {
		return numSeq;
	}

	/**
	 * @param numSeq the numSeq to set
	 */
	public void setNumSeq(int numSeq) {
		this.numSeq = numSeq;
	}

	
	/**
	 * @return the idField
	 */
	public String getIdField() {
		return idField;
	}

	/**
	 * @param idField the idField to set
	 */
	public void setIdField(String idField) {
		this.idField = idField;
	}

	/**
	 * @return the idObjectRefer
	 */
	public String getIdObjectRefer() {
		return idObjectRefer;
	}

	/**
	 * @param idObjectRefer the idObjectRefer to set
	 */
	public void setIdObjectRefer(String idObjectRefer) {
		this.idObjectRefer = idObjectRefer;
	}

	/**
	 * @return the typeObjectRefer
	 */
	public EnumObject getTypeObjectRefer() {
		return typeObjectRefer;
	}

	/**
	 * @param typeObjectRefer the typeObjectRefer to set
	 */
	public void setTypeObjectRefer(EnumObject typeObjectRefer) {
		this.typeObjectRefer = typeObjectRefer;
	}

	/**
	 * @return the numInstrRefer
	 */
	public int getNumInstrRefer() {
		return numInstrRefer;
	}

	/**
	 * @param numInstrRefer the numInstrRefer to set
	 */
	public void setNumInstrRefer(int numInstrRefer) {
		this.numInstrRefer = numInstrRefer;
	}

	/**
	 * @return the idAliasLocal
	 */
	public String getIdAliasLocal() {
		return idAliasLocal;
	}

	/**
	 * @param idAliasLocal the idAliasLocal to set
	 */
	public void setIdAliasLocal(String idAliasLocal) {
		this.idAliasLocal = idAliasLocal;
	}

	/**
	 * @return the idIoarea
	 */
	public String getIdIoarea() {
		return idIoarea;
	}

	/**
	 * @param idIoarea the idIoarea to set
	 */
	public void setIdIoarea(String idIoarea) {
		this.idIoarea = idIoarea;
	}

	/**
	 * @return the idObjectCopy
	 */
	public String getIdObjectCopy() {
		return idObjectCopy;
	}

	/**
	 * @param idObjectCopy the idObjectCopy to set
	 */
	public void setIdObjectCopy(String idObjectCopy) {
		this.idObjectCopy = idObjectCopy;
	}

	/**
	 * @return the typeWhereUsed
	 */
	public EnumWhereUsedType getTypeWhereUsed() {
		return typeWhereUsed;
	}

	/**
	 * @param typeWhereUsed the typeWhereUsed to set
	 */
	public void setTypeWhereUsed(EnumWhereUsedType typeWhereUsed) {
		this.typeWhereUsed = typeWhereUsed;
	}

	/**
	 * @return the typeAlias
	 */
	public EnumWhereUsedTypeAlias getTypeAlias() {
		return typeAlias;
	}

	/**
	 * @param typeAlias the typeAlias to set
	 */
	public void setTypeAlias(EnumWhereUsedTypeAlias typeAlias) {
		this.typeAlias = typeAlias;
	}

	/**
	 * @return the posInIoarea
	 */
	public int getPosInIoarea() {
		return posInIoarea;
	}

	/**
	 * @param posInIoarea the posInIoarea to set
	 */
	public void setPosInIoarea(int posInIoarea) {
		this.posInIoarea = posInIoarea;
	}

	/**
	 * @return the usedBytes
	 */
	public int getUsedBytes() {
		return usedBytes;
	}

	/**
	 * @param usedBytes the usedBytes to set
	 */
	public void setUsedBytes(int usedBytes) {
		this.usedBytes = usedBytes;
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
	public void setNumDec(int dec) {
		this.numDec = dec;
	}

	/**
	 * @return the signed
	 */
	public boolean getSigned() {
		return signed;
	}

	/**
	 * @param signed the signed to set
	 */
	public void setSigned(boolean signed) {
		this.signed = signed;
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
	 * @return the instrType
	 */
	public EnumPrecompilerReservedWords getInstrType() {
		return instrType;
	}

	/**
	 * @param instrType the instrType to set
	 */
	public void setInstrType(EnumPrecompilerReservedWords instrType) {
		this.instrType = instrType;
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

	@Override
	public boolean equals(Object obj) {
		EntityWhereUsedItem objEntitytWhereUsedItem = null;
		
		objEntitytWhereUsedItem = (EntityWhereUsedItem) obj;
		 
		return this.system.equals(objEntitytWhereUsedItem.system)
		 &&    this.subSystem.equals(objEntitytWhereUsedItem.subSystem)
		 &&    this.idObject.equals(objEntitytWhereUsedItem.idObject)
		 &&    this.typeObject == objEntitytWhereUsedItem.typeObject
		 &&    this.numSeq == objEntitytWhereUsedItem.numSeq
		 &&    this.idField == objEntitytWhereUsedItem.idField
		 &&    this.idObjectRefer == objEntitytWhereUsedItem.idObjectRefer
		 &&    this.numInstrRefer == objEntitytWhereUsedItem.numInstrRefer;		 
	}

	public int compareTo(Object obj) {
		EntityWhereUsedItem objToCompare = null;
		objToCompare = (EntityWhereUsedItem) obj;
		int i = 0;
		
		if (this.system.compareTo(objToCompare.getSystem()) != 0) {
			return this.system.compareTo(objToCompare.getSystem());
		}
		if (this.subSystem.compareTo(objToCompare.getSubSystem()) != 0) {
			return this.subSystem.compareTo(objToCompare.getSubSystem());
		}
		if (this.idObject.compareTo(objToCompare.getIdObject()) != 0) {
			return this.idObject.compareTo(objToCompare.getIdObject());
		}
		if (this.typeObject.compareTo(objToCompare.getTypeObject()) != 0) {
			return this.typeObject.compareTo(objToCompare.getTypeObject());
		}
		i = this.numSeq - objToCompare.getNumSeq();
		if (i != 0) {
 			return i;
 		}
		if (this.idField.compareTo(objToCompare.getIdField()) != 0) {
			return this.idField.compareTo(objToCompare.getIdField());
		}
		if (this.idObjectRefer.compareTo(objToCompare.getIdObjectRefer()) != 0) {
			return this.idObjectRefer.compareTo(objToCompare.getIdObjectRefer());
		}
		i = this.numInstrRefer - objToCompare.getNumInstrRefer();
		if (i != 0) {
 			return i;
 		}
		return 0;
	}
}

