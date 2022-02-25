/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityMetricViolation;
import entities.EntityMetricViolationConfig;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsQualityFactors;
import enums.EnumMetricsViolation;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity MetricViolation.
 */
public interface IDAOMetricViolationConfig extends IDAO<EntityMetricViolationConfig> {

	/*
	 * 1) Get a specific violation rule
	 */
	public  List<EntityMetricViolationConfig> findViolation(String sys, String subSys, String configName, EnumMetricsViolation typeViolation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all configuration violation rules 
	 */
	public  List<EntityMetricViolationConfig> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all configuration violation rules with a specific code 
	 */
	public  List<EntityMetricViolationConfig> findAll(String sys, String subSys, String configName) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get all configuration violation rules with a specific code And quality factor
	 */
	public  List<EntityMetricViolationConfig> findAll(String sys, String subSys, String configName, EnumMetricsQualityFactors qualityFactor) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all configuration violation rules with a specific code And quality characteristic
	 */
	public  List<EntityMetricViolationConfig> findAll(String sys, String subSys, String configName, EnumMetricsQualityCharacteristics qualityCharacteristic) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 6) Get all configuration violation rules with a specific code And quality factor and characteristic
	 */
	public  List<EntityMetricViolationConfig> findAll(String sys, String subSys, String configName, EnumMetricsQualityFactors qualityFactor, EnumMetricsQualityCharacteristics qualityCharacteristic) throws SQLException, ExceptionAmritaSqlError; 



}