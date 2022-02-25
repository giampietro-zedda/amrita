/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityDynamicFieldSubSetting;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAODynamicFieldSubSetting extends IDAO<EntityDynamicFieldSubSetting> {

	/*
	 * 1 Get all Dynamic sub fields settings of a dynamic instruction for field/subfield all pgms all chains
	 */
	public  List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Dynamic sub fields settings of a dynamic instruction for field/subfield all chains specific program
	 */
	public  List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField, String idPgmSet) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic sub fields settings of a dynamic instruction for field/subfield specific chain specific program
	 */
	public  List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField, String idSubField, String idPgmSet, int numChain) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4 Get all Dynamic sub fields settings only LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE 
	 *   for logic spread in callers/called
	 */
	public  List<EntityDynamicFieldSubSetting> findAll(String sys, String subSys, boolean onlyUnresolved) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5Get all Dynamic sub fields settings only LAST_SET_BY_COBOL_USING_PARM | LAST_SET_BY_COBOL_LINKAGE 
	 *   for logic spread in callers/called in all subSys
	 */
	public  List<EntityDynamicFieldSubSetting> findAll(String sys, boolean onlyUnresolved) throws SQLException, ExceptionAmritaSqlError; 

}