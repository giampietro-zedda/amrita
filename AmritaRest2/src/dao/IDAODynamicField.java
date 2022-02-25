/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityDynamicField;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAODynamicField extends IDAO<EntityDynamicField> {

	/*
	 * 1 Get all Dynamic fields defined for the object (program), full key (one row)
	 */
	public  List<EntityDynamicField> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all Dynamic fields defined for the object (program) by idObject
	 */
	public  List<EntityDynamicField> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3 Get all Dynamic fields defined for the object (program) By id field
	 */
	public  List<EntityDynamicField> findAll(String sys, String subSys, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4 Get all Dynamic fields defined for the object (program) By numInstr
	 */
	public  List<EntityDynamicField> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int numInstr) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5 Get all Dynamic fields defined for the object (program) By sys
	 */
	public  List<EntityDynamicField> findAll(String sys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 6 Get all Dynamic fields defined for the object (program) By sys, subSys
	 */
	public  List<EntityDynamicField> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 


}
