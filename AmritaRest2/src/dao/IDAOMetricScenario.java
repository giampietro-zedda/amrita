/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityMetricScenario;
import enums.EnumMetricsViolation;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOMetricScenario extends IDAO<EntityMetricScenario> {

	/*
	 * 1) Get metrics for a specific program/section SCOPE_LEVEL_SECTION
	 */
	public  List<EntityMetricScenario> findAll(String idScenario, EnumMetricsViolation typeViolation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get metrics for all section SCOPE_LEVEL_OBJECT
	 */
	public  List<EntityMetricScenario> findAll(String idScenario) throws SQLException, ExceptionAmritaSqlError; 

}