package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsQualityFactors;
import enums.EnumMetricsRuleType;
import enums.EnumMetricsViolation;
import enums.EnumMetricsViolationFixUnit;
import enums.EnumMetricsViolationSeverity;
import enums.EnumThresholds;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityMetricViolationConfig
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityMetricViolationConfig, di configurazione delle violazioni, base del
	 * sistema di qualità SQUALE.<br>
	 * Ogni configurazione è idebtificata da un codice<br>
	 * <p>
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

@Entity(name="MetricViolationConfig")
public class EntityMetricViolationConfig {

	///////////////////////////////////////////////////////////////////////
    // Data Items Metric violation                                                                                                     //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            									// Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         									// Sotto sistema applicativo
	@Id
    @Column(name="configName")
	private String configName = "";          								// Codice configurazione
	@Id
    @Column(name="typeViolation")
	private EnumMetricsViolation typeViolation = null;          			// Tipo violazione

	// Data
	
	@Column(name="ruleEnabled")
	private boolean ruleEnabled = false;           				    		// True = violatione rule enabled
	@Column(name="violationDesc")
	private String violationDesc = "";           				    		// Descrizione violazione
	@Column(name="violationSeverity")
	private EnumMetricsViolationSeverity violationSeverity = null;     		// Livello severità 
	@Column(name="ruleType")
	private EnumMetricsRuleType ruleType = null;           				    // FIXED/LINEAR
	@Column(name="qualityFactor")
	private EnumMetricsQualityFactors qualityFactor = null; 				// Fattore di qualità  
	@Column(name="qualityCharacteristic")
	private EnumMetricsQualityCharacteristics qualityCharacteristic = null; // Caratteristica di qualità  
	@Column(name="remediationCost")
    private int remediationCost = 0;                               			// Costo remediation in minuti, ore, giorni ..
	@Column(name="remediationUnit")
	private EnumMetricsViolationFixUnit  remediationUnit = null;			// Unità di misura remediation 
	@Column(name="threshold")
	private EnumThresholds threshold = null;						        // Soglie violazioni 
	@Column(name="thresholdGood")
	private String thresholdGood = "";										// LT, GT, BT (between)
	@Column(name="thresholdLow")
	private String thresholdLow = "";										// Related to thresholdGood
	@Column(name="thresholdHigh")
	private String thresholdHigh = "";										// Related to thresholdGood
    

	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityMetricViolationConfig() {
		super();
		typeViolation = EnumMetricsViolation.NOT_ASSIGNED;
		violationSeverity = EnumMetricsViolationSeverity.NOT_ASSIGNED;
		ruleType = EnumMetricsRuleType.NOT_ASSIGNED;
		remediationUnit = EnumMetricsViolationFixUnit  .NOT_ASSIGNED;
		qualityFactor = EnumMetricsQualityFactors.NOT_ASSIGNED;
		qualityCharacteristic = EnumMetricsQualityCharacteristics.NOT_ASSIGNED; 
		remediationUnit = EnumMetricsViolationFixUnit.NOT_ASSIGNED; 
		threshold = EnumThresholds.NOT_ASSIGNED;
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
	 * @return the configName
	 */
	public String getConfigName() {
		return configName;
	}


	/**
	 * @param configName the configName to set
	 */
	public void setConfigName(String configName) {
		this.configName = configName;
	}


	/**
	 * @return the violationType
	 */
	public EnumMetricsViolation getTypeViolation() {
		return typeViolation;
	}


	/**
	 * @param violationType the violationType to set
	 */
	public void setTypeViolation(EnumMetricsViolation typeViolation) {
		this.typeViolation = typeViolation;
	}


	/**
	 * @return the violationSeverity
	 */
	public EnumMetricsViolationSeverity getViolationSeverity() {
		return violationSeverity;
	}


	/**
	 * @param violationSeverity the violationSeverity to set
	 */
	public void setViolationSeverity(EnumMetricsViolationSeverity violationSeverity) {
		this.violationSeverity = violationSeverity;
	}


	/**
	 * @return the ruleEnabled
	 */
	public boolean isRuleEnabled() {
		return ruleEnabled;
	}

	/**
	 * @return the ruleEnabled
	 */
	public boolean getRuleEnabled() {
		return ruleEnabled;
	}


	/**
	 * @param ruleEnabled the ruleEnabled to set
	 */
	public void setRuleEnabled(boolean ruleEnabled) {
		this.ruleEnabled = ruleEnabled;
	}


	/**
	 * @return the violationDesc
	 */
	public String getViolationDesc() {
		return violationDesc;
	}


	/**
	 * @param violationDesc the violationDesc to set
	 */
	public void setViolationDesc(String violationDesc) {
		this.violationDesc = violationDesc;
	}


	/**
	 * @return the ruleType
	 */
	public EnumMetricsRuleType getRuleType() {
		return ruleType;
	}


	/**
	 * @param ruleType the ruleType to set
	 */
	public void setRuleType(EnumMetricsRuleType ruleType) {
		this.ruleType = ruleType;
	}


	/**
	 * @return the qualityFactor
	 */
	public EnumMetricsQualityFactors getQualityFactor() {
		return qualityFactor;
	}


	/**
	 * @param qualityFactor the qualityFactor to set
	 */
	public void setQualityFactor(EnumMetricsQualityFactors qualityFactor) {
		this.qualityFactor = qualityFactor;
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
	public void setQualityCharacteristic(EnumMetricsQualityCharacteristics qualityCharacteristic) {
		this.qualityCharacteristic = qualityCharacteristic;
	}


	/**
	 * @return the remediationCost
	 */
	public int getRemediationCost() {
		return remediationCost;
	}


	/**
	 * @param remediationCost the remediationCost to set
	 */
	public void setRemediationCost(int remediationCost) {
		this.remediationCost = remediationCost;
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
	 * @return the threshold
	 */
	public EnumThresholds getThreshold() {
		return threshold;
	}


	/**
	 * @param threshold the threshold to set
	 */
	public void setViolationThreshold(EnumThresholds threshold) {
		this.threshold = threshold;
	}


	/**
	 * @return the thresholdGood
	 */
	public String getThresholdGood() {
		return thresholdGood;
	}


	/**
	 * @param thresholdGood the thresholdGood to set
	 */
	public void setThresholdGood(String thresholdGood) {
		this.thresholdGood = thresholdGood;
	}


	/**
	 * @return the thresholdLow
	 */
	public String getThresholdLow() {
		return thresholdLow;
	}


	/**
	 * @param thresholdLow the thresholdLow to set
	 */
	public void setThresholdLow(String thresholdLow) {
		this.thresholdLow = thresholdLow;
	}


	/**
	 * @return the thresholdHigh
	 */
	public String getThresholdHigh() {
		return thresholdHigh;
	}


	/**
	 * @param thresholdHigh the thresholdHigh to set
	 */
	public void setThresholdHigh(String thresholdHigh) {
		this.thresholdHigh = thresholdHigh;
	}



}
