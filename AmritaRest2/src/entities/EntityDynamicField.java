package entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import enums.EnumCobolReservedWords;
import enums.EnumPrecompilerReservedWords;
import enums.EnumObject;

/**
	* Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
	*
	* <h1>
	* EntityDynamicField (DynamicField, DFLD)
	* </h1>
	*  <p>
	* Questa classe mappa la tabella DFLD e descrive un campo di un'istruzione dinamica di un programma.
	* Un'istruzione dinamica è una normale istruzione dove, parametri rilevanti per il popolamento di
	* oggetti e relazioni, sono espressi genericamente da campi e non da literal esplicite.<br>
	* <p>
	* Tale istruzione deve essere risolta dal processo di soluzione delle istruzioni dinamiche
	* e può essere risolta nello stesso programma o in programmi diversi, chiamati e/o chiamanti.<br>
	* Le istruzioni dinamiche possono avere più di un campo da risolvere e quindi per istruzioni di
	* questo tipo è possibile avere più righe di oggetti di questa classe.<br>
	* <p>
	* Nel caso un'istruzione avesse una situazione mista di operandi espressi come literal e operandi
	* espressi come campi, viene in ogni caso inserita, una riga per ogni campo, al fine di mantenere
	* l'integrità della funzione.<br>
	* <p>
	* In caso di istruzione non risolta a causa di waiting di dati esterni cercati, e non trovati,
	* nella tabella <t>DVAE<\t>, vengono memorizzati gli estremi del  media esterno di cui
	* inserire i valori in <t>DVAE<\t> per completare il processo di soluzione dinamica dell'istruzione.<br>
	* Per ogni valore esterno da recuperare viene inserita una riga in questa tabella.
	* <p>
	* Nella tabella <t>DWTE<\t> vengono invece memorizzati tutti gli estremi dei media esterni necessari, di cui
	* inserire i valori in <t>DVAE<\t>
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
@Entity(name="DynamicField")
public class EntityDynamicField implements Serializable {

	private static final long serialVersionUID = 1L;
	
	///////////////////////////////////////////////////////////////////////
    // Data Items DynamicField                                           //                                                       
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
	@Column(name="sys")
	private String system = "";            								// Sistema applicativo
	@Id
	@Column(name="subSys")
	private String subSystem = "";         								// Sotto sistema applicativo
	@Id
	@Column(name="idObject")
	private String idObject = "";          								// Nome programma di partenza
	@Id
	@Column(name="typeObject")
	private EnumObject typeObject = null;       						// Tipologia oggetto programma (T0001)
	@Id
	@Column(name="numInstr")
	private int numInstr = 0; 											// Numero istruzione
	@Id
	@Column(name="idField")
	private String idField = "";										// Nome operando/campo da risolvere/risolto
	
	// Data
	@Column(name="numField")
	private int numField = 0;           					        	// Numero campo codificato nella struttura di programma
	@Column(name="instrCobolType")
	private EnumCobolReservedWords instrCobolType = null; 				// Tipo istruzione Cobol codificata (T0029)
	@Column(name="instrPrecompType")
	private EnumPrecompilerReservedWords instrPrecompType = null; 		// Tipo istruzione precompilatore codificata (T0009)
	@Column(name="instrPrecompOprndType")
	private EnumPrecompilerReservedWords instrPrecompOprndType = null;	// Tipo Operando precompilatore codificato (T0009)
	@Column(name="light")
	private boolean light = false;										// Indicazione di campo dinamico light (campo valorizzato da value nel programma)
	@Column(name="solved")
	private boolean solved = false;										// ndicazione di campo risolto, con valori validi recuperati
	@Column(name="solvedFull")
	private boolean solvedFull = false;							    	// Indicazione di campo risolto, con tutti i possibili validi recuperati
	@Column(name="waitingForData")
	private boolean waitingForData = false;						    	// Indicazione di campo in attesa di dati esterni per essere risolto
	@Column(name="spreaded")
	private boolean spreaded = false;									// Indicazione di campo con assegnazioni spreaded, nei pgm chiamanti/chiamati
	
	
	/*
	 * 
	 * Costruttore 
	 * 
	 */
	public EntityDynamicField() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		instrCobolType = EnumCobolReservedWords.NOT_ASSIGNED;
		instrPrecompType = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		instrPrecompOprndType = EnumPrecompilerReservedWords.NOT_ASSIGNED; 
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
	 * @return the numInstr
	 */
	public int getNumInstr() {
		return numInstr;
	}


	/**
	 * @param numInstr the numInstr to set
	 */
	public void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}



	/**
	 * @return the numField
	 */
	public int getNumField() {
		return numField;
	}


	/**
	 * @param numField the numField to set
	 */
	public void setNumField(int numField) {
		this.numField = numField;
	}


	/**
	 * @return the instrCobolType
	 */
	public EnumCobolReservedWords getInstrCobolType() {
		return instrCobolType;
	}


	/**
	 * @param instrCobolType the instrCobolType to set
	 */
	public void setInstrCobolType(EnumCobolReservedWords instrCobolType) {
		this.instrCobolType = instrCobolType;
	}


	/**
	 * @return the instrPrecompType
	 */
	public EnumPrecompilerReservedWords getInstrPrecompType() {
		return instrPrecompType;
	}


	/**
	 * @param instrPrecompType the instrPrecompType to set
	 */
	public void setInstrPrecompType(EnumPrecompilerReservedWords instrPrecompType) {
		this.instrPrecompType = instrPrecompType;
	}


	
	
	/**
	 * @return the light
	 */
	public boolean getLight() {
		return light;
	}


	/**
	 * @param light the light to set
	 */
	public void setLight(boolean light) {
		this.light = light;
	}


	/**
	 * @return the instrPrecompOprndType
	 */
	public EnumPrecompilerReservedWords getInstrPrecompOprndType() {
		return instrPrecompOprndType;
	}


	/**
	 * @param instrPrecompOprndType the instrPrecompOprndType to set
	 */
	public void setInstrPrecompOprndType(EnumPrecompilerReservedWords instrPrecompOprndType) {
		this.instrPrecompOprndType = instrPrecompOprndType;
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
	 * Restituisce true se il campo è stato risolto e ha prodotto
	 * dei valori, anche se non tutti quelli possibili.<br>
	 * <p>
	 * 
	 * @return the solved
	 */
	public boolean getSolved() {
		return solved;
	}


	/**
	 * Imposta se il campo è stato risolto e ha prodotto
	 * dei valori, anche se non tutti quelli possibili.<br>
	 * <p>
	 * 
	 * @param solved the solved to set
	 */
	public void setSolved(boolean solved) {
		this.solved = solved;
	}


	
	/**
	 * Restituisce true se il campo è stato risolto e ha prodotto
	 * tutti i valori possibili.<br>
	 * <p>
	 * 
	 * @return the solved
	 */
	public boolean getSolvedFull() {
		return solvedFull;
	}


	/**
	 * Imposta se il campo è stato risolto e ha prodotto
	 * tutti i valori possibili.<br>
	 * <p>
	 * @param solvedFull the solvedFull to set
	 */
	public void setSolvedFull(boolean solvedFull) {
		this.solvedFull = solvedFull;
	}


	/**
	 * Restituisce true se il campo, un suo sottocampo una sua
	 * porzione parziale è in attesa di dati esterni, come una colonna
	 * di un file, di una tabella, il nome di un terminale etc.<br>
	 * <p>
	 * 
	 * @return the waitingForData
	 */
	public boolean getWaitingForData() {
		return waitingForData;
	}

    /**
	 * Impostae se il campo, un suo sottocampo una sua
	 * porzione parziale è in attesa di dati esterni, come una colonna
	 * di un file, di una tabella, il nome di un terminale etc.<br>
	 * <p>
	 * 
	 * @param waitingForData the waitingForData to set
	 */
	public void setWaitingForData(boolean waitingForData) {
		this.waitingForData = waitingForData;
	}


	/**
	 * Restituisce se il campo o un suo sottocampo
	 * o qualsiasi sua assegnazione parziale, sia da
	 * risolversi spreaded, nei programmi chiamanti.<br>
	 * <p>
	 * 
	 * @return the spreaded
	 */
	public boolean getSpreaded() {
		return spreaded;
	}


	/**
	 * Imposta se il campo o un suo sottocampo
	 * o qualsiasi sua assegnazione parziale, sia da
	 * risloversi spreaded, nei programmi chiamanti.<br>
	 * <p>
	 * 
	 * @param spreaded the spreaded to set
	 */
	public void setSpreaded(boolean spreaded) {
		this.spreaded = spreaded;
	}	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityDynamicField objEntityDynamicField = null;
		
		objEntityDynamicField = (EntityDynamicField) obj;
		 
		return this.system.equals(objEntityDynamicField.system)
		 &&    this.subSystem.equals(objEntityDynamicField.subSystem)
		 &&    this.idObject.equals(objEntityDynamicField.idObject)
		 &&    this.typeObject == objEntityDynamicField.typeObject
		 &&    this.numInstr == objEntityDynamicField.numInstr
		 &&    this.idField.equals(objEntityDynamicField.idField);		 
	}

	public int compareTo(Object obj) {
		EntityDynamicField objToCompare = null;
		objToCompare = (EntityDynamicField) obj;
		
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
		if ((this.numInstr - objToCompare.getNumInstr() != 0)) {
			return this.numInstr - objToCompare.getNumInstr();
		}
		if (this.idField.compareTo(objToCompare.getIdField()) != 0) {
			return this.idField.compareTo(objToCompare.getIdField());
		}

		return 0;
	}	


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "EntityDynamicField [idObject=" + idObject + ", numInstr=" + numInstr + ", idField=" + idField
				+ ", numField=" + numField + "]";
	}
	
}
