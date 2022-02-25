/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityDynamicFieldSub;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAODynamicFieldSub extends IDAO<EntityDynamicFieldSub> {

	/*
	 * 1 Get all Dynamic sub fields defined for the dynamic field, at a specific instruction
	 */
	public  List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Dynamic sub fields defined for the specific dynamic field, by idField 
	 */
	public  List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic sub fields defined for the specific dynamic field, by numInstr, idField
	 */
	public  List<EntityDynamicFieldSub> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField) throws SQLException, ExceptionAmritaSqlError; 

}