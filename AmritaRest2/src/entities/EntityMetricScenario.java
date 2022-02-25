package entities;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsViolation;
import enums.EnumMetricsViolationFixUnit;
import enums.EnumMetricsViolationSeverity;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityMetricScenario (MetricScenario, METS) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityMetricScenario, ovvero la tabella oggetti METS che contiene le personalizzazioni<br>
	 * delle violazioni a livello di scenario<br>
	 * <p>
	 * Lo scopo è quello di effettuare differenti valutazioni delle metriche a seconda delle esigenze,<br>
	 * individuando ogni nuova configurazione con uno scenario, quindi un nome, diverso.<br>
	 * Pertanto in uno scenario una violazione può essere abilitata e in un altro no, in uno<br>
	 * può essere associata a una caratteristica di qualità e in un altro a un'altra.<br>
	 * A fine analisi del programma e nelle elaborazioni post analisi delle metriche, <br>
	 * viene utilizzata questa tabella.<br>
	 * Questa impostazione offre la massima flessibilità senza dover effettuare costose rianalisi dei
	 * sorgenti.<br>
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

@DataBaseMappedTable("MetricScenario")
@DataBaseMappedColumns(java_db_col = {  
		                        // Colonne primary key
	                             "idScenario		  		 idScenario 	       PK"    
							    ,"typeViolation 		  	 typeViolation 	       PK"  
							    // Colonne senza vincoli di chiave (No key)
								,"violationEnabled			 violationEnabled      NK"
								,"severityViolation			 severityViolation     NK"
								,"remediationUnit			 remediationUnit       NK"
								,"remediationCost			 remediationCost       NK"
								,"qualityCharacteristic	     qualityCharacteristic NK"
								,"thresholdLow    	         thresholdLow          NK"
								,"thresholdHigh    	         thresholdHigh         NK"
								}
         )

public class EntityMetricScenario {

	///////////////////////////////////////////////////////////////////////
    // Data Items Metric violation                               
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	private String idScenario = "";            							    // METSIDSC(PK) Identificativo personalizzazione
	private EnumMetricsViolation typeViolation = null;  					// METSTYPV(PK) Tipologia violazione (T0046)

	// Data
	private boolean violationEnabled = false;                               // METSVIEN     True = violazione abilitata
	private EnumMetricsViolationSeverity  severityViolation = null;			// METSSVRT     Severità violazione (T0047)
	private EnumMetricsViolationFixUnit  remediationUnit = null;			// METSRUNT     Unità di misura remediation (T0048)
    private int remediationCost = 0;                               			// METSRCST     Costo remediation in minuti, ore, giorni ..
	private int thresholdLow = 0; 											// METSLSHL     Livello di soglia low
	private int thresholdHigh = 0; 											// METSLSHH     Livello di soglia high
	private EnumMetricsQualityCharacteristics qualityCharacteristic = null; // METSQCHR     Caratteristica di qualità (T0049)
    	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityMetricScenario() {
		super();
		typeViolation = EnumMetricsViolation.NOT_ASSIGNED;
		severityViolation = EnumMetricsViolationSeverity.NOT_ASSIGNED;
		remediationUnit = EnumMetricsViolationFixUnit  .NOT_ASSIGNED;
		qualityCharacteristic = EnumMetricsQualityCharacteristics .NOT_ASSIGNED; 
		
	}


	
	/**
	 * @return the idScenario
	 */
	public String getIdScenario() {
		return idScenario;
	}


	/**
	 * @param idScenario the idScenario to set
	 */
	public void setIdScenario(String idScenario) {
		this.idScenario = idScenario;
	}


	/**
	 * @return the violationEnabled
	 */
	public boolean getViolationEnabled() {
		return violationEnabled;
	}


	/**
	 * @param violationEnabled the violationEnabled to set
	 */
	public void setViolationEnabled(boolean violationEnabled) {
		this.violationEnabled = violationEnabled;
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
	 * @return the thresholdLow
	 */
	public int getThresholdLow() {
		return thresholdLow;
	}


	/**
	 * @param thresholdLow the thresholdLow to set
	 */
	public void setThresholdLow(int thresholdLow) {
		this.thresholdLow = thresholdLow;
	}



	/**
	 * @return the thresholdHigh
	 */
	public int getThresholdHigh() {
		return thresholdHigh;
	}



	/**
	 * @param thresholdHigh the thresholdHigh to set
	 */
	public void setThresholdHigh(int thresholdHigh) {
		this.thresholdHigh = thresholdHigh;
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


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  "Metric for:" + this.idScenario;
	}



}
