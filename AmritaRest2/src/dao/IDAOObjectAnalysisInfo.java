/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityObjectAnalysisInfo;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOObjectAnalysisInfo extends IDAO<EntityObjectAnalysisInfo> {

	/*
	 * 1 Get all Object analysis errors for an object of a specific sub System
	 */
	public  List<EntityObjectAnalysisInfo> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Object analysis errors for an object of a specific sub System
	 */
	public  List<EntityObjectAnalysisInfo> findAll(String sys, String subSys, String idObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Object analysis errors in specific sub Systems, regardless id and type object
	 */
	public  List<EntityObjectAnalysisInfo> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 


}