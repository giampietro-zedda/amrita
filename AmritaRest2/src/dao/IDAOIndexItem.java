/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityIndexItem;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;


/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOIndexItem extends IDAO<EntityIndexItem> {

	/*
	 * 1) Get all indexes defined
	 */
	public  List<EntityIndexItem> findAll() throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 1) Get sepecific index defined
	 */
	public  List<EntityIndexItem> findAll(String system, String subSys, String idObject, EnumObject typeObject, String idObjectOwner, EnumObject typeObjectOwner, int numSeq) throws SQLException, ExceptionAmritaSqlError; 


}