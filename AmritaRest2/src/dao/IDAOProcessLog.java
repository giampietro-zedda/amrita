/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityProcessLog;
import enums.EnumDirectivesExecution;

import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOProcessLog extends IDAO<EntityProcessLog> {

	/*
	 * 1) Get all processLog related to a user by date greater or equal 
	 */
	public  List<EntityProcessLog> findAllByUser(String user, String sys, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 2) Get all processLog related to a user by date greater or equal 
	 */
	public  List<EntityProcessLog> findAllByUserDateTime(String user, String sys, String dtStartProcess, String tmStartProcess) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 3) Get all processLog related by date greater or equal 
	 */
	public  List<EntityProcessLog> findAllByDate(String sys, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4)Get all processLog related by idObject and date greater or equal 
	 */
	public  List<EntityProcessLog> findAllByIdObject(String sys, String idObject, String dtStartProcess) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all object names in relation with an object 
	 */
	public  List<EntityProcessLog> findAllByTypeProcess(String sys, EnumDirectivesExecution typeProcess,  String dtStartProcess) throws SQLException, ExceptionAmritaSqlError; 
}


