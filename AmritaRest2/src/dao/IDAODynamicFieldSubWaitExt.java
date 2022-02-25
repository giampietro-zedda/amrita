/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityDynamicFieldSubWaitExt;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;



/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAODynamicFieldSubWaitExt extends IDAO<EntityDynamicFieldSubWaitExt> {


	/*
	 * 1 Get all waiting rec in the sys
	 */
	public  List<EntityDynamicFieldSubWaitExt> findAll(String sys) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 2 Get all waiting rec in the sys/subSys
	 */
	public  List<EntityDynamicFieldSubWaitExt> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 3 Get all waiting rec in the sys/subSys for a specific program
	 */
	public  List<EntityDynamicFieldSubWaitExt> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4 Get all waiting rec in the sys/subSys for a specific program at a specific instruction
	 */
	public  List<EntityDynamicFieldSubWaitExt> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5 Get all waiting rec in the sys/subSys for a specific program at a specific instruction and a specifi subField
	 */
	public  List<EntityDynamicFieldSubWaitExt> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField) throws SQLException, ExceptionAmritaSqlError; 

}

