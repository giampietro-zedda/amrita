/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityMetricValue;
import enums.EnumMetricsScope;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOMetricValue extends IDAO<EntityMetricValue> {

	/*
	 * 1) Get metrics for a specific program/section SCOPE_LEVEL_SECTION
	 */
	public  List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject, String section) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get metrics for all section SCOPE_LEVEL_OBJECT
	 */
	public  List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 


	/*
	 * 3) Get metrics for all subsystem SCOPE_LEVEL_SUBSYSTEM
	 */
	public  List<EntityMetricValue> findAll(String sys, String subSys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get metrics for all subsystem SCOPE_LEVEL_SYSTEM
	 */
	public  List<EntityMetricValue> findAll(String sys, EnumMetricsScope scope) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all metrics for a specific program
	 */
	public  List<EntityMetricValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 


}