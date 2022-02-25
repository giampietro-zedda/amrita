/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityObjectOption;
import enums.EnumObject;
import enums.EnumObjectOption;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity ObjectOption.
 */
public interface IDAOObjectOption extends IDAO<EntityObjectOption> {

	/*
	 * 1 Get all ObjectOption of the object specified, all parameters required
	 */
	public  List<EntityObjectOption> findAll(String sys, String subSys, EnumObject typeObject, String idObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Objects with the specified option, on a specifiv subSys 
	 */
	public  List<EntityObjectOption> findAll(String sys, String subSys, EnumObjectOption option) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Objects with the specified option 
	 */
	public  List<EntityObjectOption> findAll(String sys, EnumObjectOption option) throws SQLException, ExceptionAmritaSqlError; 


}