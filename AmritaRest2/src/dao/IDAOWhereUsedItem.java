/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityWhereUsedItem;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOWhereUsedItem extends IDAO<EntityWhereUsedItem> {

	/*
	 * 1) Get all where used for the copy/entity field generated in a specific subsys
	 */
	public  List<EntityWhereUsedItem> findAll(String sys, String subSys, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all where used for the copy/entity field regardless subsys origin and program
	 */
	public  List<EntityWhereUsedItem> findAll(String sys, String idObject, EnumObject typeObject, String idField) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all where used for the copy in a specific subSystem for all fields
	 */
	public  List<EntityWhereUsedItem> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get all where used for the copy/entity field regardless subsys origin in a specific program
	 */
	public  List<EntityWhereUsedItem> findAll(String sys, String idObject, EnumObject typeObject, String idField, String idObjectRefer) throws SQLException, ExceptionAmritaSqlError; 

}