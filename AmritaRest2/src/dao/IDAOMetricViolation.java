/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityMetricViolation;
import enums.EnumMetricsScope;
import enums.EnumMetricsViolation;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity MetricViolation.
 */
public interface IDAOMetricViolation extends IDAO<EntityMetricViolation> {

	/*
	 * 1) Get specific violation
	 */
	public  List<EntityMetricViolation> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject, String section, EnumMetricsViolation typeViolation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all Object violations
	 */
	public  List<EntityMetricViolation> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 


	/*
	 * 3) Get metrics for all subsystem violations
	 */
	public  List<EntityMetricViolation> findAll(String sys, String subSys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get metrics for all subsystem violations
	 */
	public  List<EntityMetricViolation> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get metrics for all system violation
	 */
	public  List<EntityMetricViolation> findAll(String sys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError; 




}