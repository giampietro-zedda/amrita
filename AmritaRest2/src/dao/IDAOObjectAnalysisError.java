/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityObjectAnalysisError;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOObjectAnalysisError extends IDAO<EntityObjectAnalysisError> {

	/*
	 * 1 Get all Object analysis errors for an object of a specific sub System
	 */
	public  List<EntityObjectAnalysisError> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get Object analysis error for an object of a specific sub System at specific instruction
	 */
	public  List<EntityObjectAnalysisError> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int rowNum) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Object analysis errors in specific sub Systems, regardless id and type object
	 */
	public  List<EntityObjectAnalysisError> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4 Get all Object analysis errors in all sub Systems
	 */
	public  List<EntityObjectAnalysisError> findAll(String sys) throws SQLException, ExceptionAmritaSqlError; 


}