package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsScope;
import enums.EnumMetricsViolation;
import enums.EnumMetricsViolationFixUnit;
import enums.EnumMetricsViolationSeverity;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityMetricViolation (MetricViolation, METV) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityMetricViolation, ovvero la tabella oggetti MetricViolation che contiene il dettaglio delle
	 * violazioni a livello di sistema/sottosistema/programma/section/paragrafo.<br>
	 * <p>
	 * Per ogni violazione, al livello di aggregazione specifico, vengono memorizzati gli estremi per individuarla,<br>
	 * che possono essere liste di programmi, di numeri di istruzioni o numeri di definizione dati, così come<br>
	 * codificati in fase di analisi del programma.<br>
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0ù	
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

@Entity(name="MetricViolation")
public class EntityMetricViolation {

	///////////////////////////////////////////////////////////////////////
    // Data Items Metric violation                                                                                                     //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            									// METVSYST(PK) Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         									// METVSUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          									// METVIDOB(PK) Nome oggetto (es. programma, Copy, Jcl, ..)
	@Id
    @Column(name="scope")
	private EnumMetricsScope scope = null;          					    // METVSCOP(PK) Livello di aggregazione codificato  (T045)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       							// METVTYPO(PK) Tipologia oggetto (T0001)
	@Id
    @Column(name="section")
	private String section = "";                							// METVSECT(PK) Program section di aggregazione se oggetto programma
	@Id
    @Column(name="typeViolation")
	private EnumMetricsViolation typeViolation = null;  					// METVTYPV(PK) Tipologia violazione (T0046)

	// Data
	
	@Column(name="severityViolation")
	private EnumMetricsViolationSeverity  severityViolation = null;			// METVSVRT     Severità violazione (T0047)
	@Column(name="originViolation")
	private String originViolation = "";           							// METVORGN     Elenco di numeri istruzione/definizione
	@Column(name="originViolationRows")
	private String originViolationRows = "";           						// METVSROW     Elenco di numeri riga sorgente
	@Column(name="originViolationRowsCopy")
	private String originViolationRowsCopy = "";           					// METVSCPY     Elenco di numeri riga sorgente di copy (-1 non significativo)
	@Column(name="cntViolations")
	private int cntViolations = 0;           				        		// METVCNTV     Contatore violazioni
	@Column(name="value")
	private String value = "";           				    				// METVVALU     Valore violazione (Es. valore di soglia o altro)
	@Column(name="remediationCost")
    private int remediationCost = 0;                               			// METVRCST     Costo remediation in minuti, ore, giorni ..
	@Column(name="remediationUnit")
	private EnumMetricsViolationFixUnit  remediationUnit = null;			// METVRUNT     Unità di misura remediation (T0048)
	@Column(name="qualityCharacteristic")
	private EnumMetricsQualityCharacteristics qualityCharacteristic = null; // METVQCHR     Caratteristica di qualità (T0049)
    
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityMetricViolation() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		scope = EnumMetricsScope.NOT_ASSIGNED;
		typeViolation = EnumMetricsViolation.NOT_ASSIGNED;
		severityViolation = EnumMetricsViolationSeverity.NOT_ASSIGNED;
		remediationUnit = EnumMetricsViolationFixUnit  .NOT_ASSIGNED;
		qualityCharacteristic = EnumMetricsQualityCharacteristics .NOT_ASSIGNED; 
		
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
	 * @return the section
	 */
	public String getSection() {
		return section;
	}


	/**
	 * @param section the section to set
	 */
	public void setSection(String section) {
		this.section = section;
	}


	/**
	 * @return the typeViolation
	 */
	public EnumMetricsViolation getTypeViolation() {
		return typeViolation;
	}


	/**
	 * @param typeViolation the typeViolation to set
	 */
	public void setTypeViolation(EnumMetricsViolation typeViolation) {
		this.typeViolation = typeViolation;
	}


	/**
	 * @return the severityViolation
	 */
	public EnumMetricsViolationSeverity getSeverityViolation() {
		return severityViolation;
	}


	/**
	 * @param severityViolation the severityViolation to set
	 */
	public void setSeverityViolation(EnumMetricsViolationSeverity severityViolation) {
		this.severityViolation = severityViolation;
	}


	/**
	 * @return the originViolation
	 */
	public String getOriginViolation() {
		return originViolation;
	}


	/**
	 * @param originViolation the originViolation to set
	 */
	public void setOriginViolation(String originViolation) {
		this.originViolation = originViolation;
	}


	/**
	 * @return the originViolationRows
	 */
	public String getOriginViolationRows() {
		return originViolationRows;
	}


	/**
	 * @param originViolationRows the originViolationRows to set
	 */
	public void setOriginViolationRows(String originViolationRows) {
		this.originViolationRows = originViolationRows;
	}


	/**
	 * @return the originViolationRowsCopy
	 */
	public String getOriginViolationRowsCopy() {
		return originViolationRowsCopy;
	}


	/**
	 * @param originViolationRowsCopy the originViolationRowsCopy to set
	 */
	public void setOriginViolationRowsCopy(String originViolationRowsCopy) {
		this.originViolationRowsCopy = originViolationRowsCopy;
	}


	/**
	 * @return the cntViolations
	 */
	public int getCntViolations() {
		return cntViolations;
	}


	/**
	 * @param cntViolations the cntViolations to set
	 */
	public void setCntViolations(int cntViolations) {
		this.cntViolations = cntViolations;
	}

	

	/**
	 * @return the value
	 */
	public String getValue() {
		return value;
	}


	/**
	 * @param value the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}


	/**
	 * @return the remediationCost
	 */
	public int getRemediationCost() {
		return remediationCost;
	}


	/**
	 * @return the remediationUnit
	 */
	public EnumMetricsViolationFixUnit getRemediationUnit() {
		return remediationUnit;
	}


	/**
	 * @param remediationUnit the remediationUnit to set
	 */
	public void setRemediationUnit(EnumMetricsViolationFixUnit remediationUnit) {
		this.remediationUnit = remediationUnit;
	}


	/**
	 * @param remediationCost the remediationCost to set
	 */
	public void setRemediationCost(int remediationCost) {
		this.remediationCost = remediationCost;
	}


	/**
	 * @return the qualityCharacteristic
	 */
	public EnumMetricsQualityCharacteristics getQualityCharacteristic() {
		return qualityCharacteristic;
	}


	/**
	 * @param qualityCharacteristic the qualityCharacteristic to set
	 */
	public void setQualityCharacteristic(
			EnumMetricsQualityCharacteristics qualityCharacteristic) {
		this.qualityCharacteristic = qualityCharacteristic;
	}


	public EnumMetricsScope getScope() {
		return scope;
	}


	public void setScope(EnumMetricsScope scope) {
		this.scope = scope;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  "Metric for:" + typeObject.toString() + " IdObject:" + idObject + " section:" + section;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityMetricViolation objEntityMetric = null;
		
		objEntityMetric = (EntityMetricViolation) obj;
		 
		return this.system.equals(objEntityMetric.system)
		 &&    this.subSystem.equals(objEntityMetric.subSystem)
		 &&    this.idObject.equals(objEntityMetric.idObject)
		 &&    this.typeObject == objEntityMetric.typeObject
		 &&    this.section.equals(objEntityMetric.section);
		 
	}


}
