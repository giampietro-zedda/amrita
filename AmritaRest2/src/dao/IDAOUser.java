/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityUser;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOUser extends IDAO<EntityUser> {

	/*
	 * 1) Get all users defined
	 */
	public  List<EntityUser> findAll() throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 1) Get sepecific user defined
	 */
	public  List<EntityUser> findAll(String user) throws SQLException, ExceptionAmritaSqlError; 


}