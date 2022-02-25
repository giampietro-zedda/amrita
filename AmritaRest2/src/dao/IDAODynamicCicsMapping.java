/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityDynamicCicsMapping;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;



/**
 * Interfaccia DAO per specifiche operazioni per EntityDynamicCicsMapping (TODO).
 */
public interface IDAODynamicCicsMapping extends IDAO<EntityDynamicCicsMapping> {

	/*
	 * 1 Get all mappings available
	 */
	public  List<EntityDynamicCicsMapping> findAll(String sys) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 2 Get dsname mapping for specific subSys
	 */
	public  List<EntityDynamicCicsMapping> findAllBySubSys(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 3 Get dsname mapping for specific Cics 
	 */
	public  List<EntityDynamicCicsMapping> findAllByCics(String sys, String cicsName) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 4 Get dsname mapping for specific externalName
	 */
	public  List<EntityDynamicCicsMapping> findAll(String sys, String subSys, EnumObject typeObject, String externalName) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 5 Get dsname mapping for specific externalName and cicsName
	 */
	public  List<EntityDynamicCicsMapping> findAll(String sys, String subSys, String cicsName, EnumObject typeObject, String externalName) throws SQLException, ExceptionAmritaSqlError; 


}

