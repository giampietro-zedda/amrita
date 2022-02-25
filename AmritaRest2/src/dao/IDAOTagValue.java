/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityTagValue;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity DynamicField.
 */
public interface IDAOTagValue extends IDAO<EntityTagValue> {

	/*
	 * 1 Get specific tag value
	 */
	public  List<EntityTagValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject, int progr) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2 Get all tag value
	 */
	public  List<EntityTagValue> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

}
