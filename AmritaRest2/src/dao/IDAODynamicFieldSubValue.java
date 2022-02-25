/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityDynamicFieldSubValue;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAODynamicFieldSubValue extends IDAO<EntityDynamicFieldSubValue> {

	/*
	 * 1 Get all Dynamic values for the specific dynamic sub field at a specific instruction
	 */
	public  List<EntityDynamicFieldSubValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Dynamic values for the specific dynamic field at a specific instruction
	 */
	public  List<EntityDynamicFieldSubValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic values for the specific dynamic field at a specific instruction, values for the complete field and all subFields
	 */
	public  List<EntityDynamicFieldSubValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstrd) throws SQLException, ExceptionAmritaSqlError; 

}