/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityDynamicValueExt;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAODynamicValueExt extends IDAO<EntityDynamicValueExt> {

	/*
	 * 1 Get all Dynamic Values EXTERNAL, idObjectExternal is the DDname of Vsam/Seq file
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, EnumObject typeObjectExternal) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 2 Get all Dynamic Values EXTERNAL, idObjectExternal is the Sql Table name 
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, String idFieldColumnExternal, EnumObject typeObjectExternal) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic Values EXTERNAL for VSAM with DDName of Vsam/Seq file and Dsname available
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, EnumObject typeObjectExternal, String dsnameExternal) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic Values EXTERNAL for VSAM with DDName and Dsname in a specific Cics 
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, EnumObject typeObjectExternal, String dsnameExternal, String CicsNameExternal) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4 Get all Dynamic Values EXTERNAL for Cics system fields EIBTRMID|EIBTRNID
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, EnumObject typeObjectExternal, EnumDataItemSystemEnvironment typeSystemFieldExternal, String idSystemFieldExternal) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5 Get all Dynamic Values EXTERNAL for Cics system fields EIBTRMID|EIBTRNID in a specific Cics
	 */
	public  List<EntityDynamicValueExt> findAll(String sys, String subSys, String idObjectExternal, EnumObject typeObjectExternal, EnumDataItemSystemEnvironment typeSystemFieldExternal, String idSystemFieldExternal, String CicsNameExternal) throws SQLException, ExceptionAmritaSqlError; 

}
