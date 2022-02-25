/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityMapItem;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity MapDescriptor.
 */
public interface IDAOMapItem extends IDAO<EntityMapItem> {


	/*
	 * 1) Get All map fields
	 */
	public  List<EntityMapItem> findAll(String sys, String SubSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 


}