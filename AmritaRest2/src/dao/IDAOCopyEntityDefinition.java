/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityCopyEntityDefinition;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOCopyEntityDefinition extends IDAO<EntityCopyEntityDefinition> {

	/*
	 * Get all item definitions for the copy/entity
	 */
	public  List<EntityCopyEntityDefinition> findAll(String sys, String subSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

}