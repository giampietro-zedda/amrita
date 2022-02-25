/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityMapDescriptor;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity MapDescriptor.
 */
public interface IDAOMapDescriptor extends IDAO<EntityMapDescriptor> {


	/*
	 * 1) Get specific subSys MapDescriptor
	 */
	public  List<EntityMapDescriptor> findAll(String sys, String SubSys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all MapDescriptor defined in all subSystems
	 */
	public  List<EntityMapDescriptor> findAll(String sys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all subSys MapDescriptor
	 */
	public  List<EntityMapDescriptor> findAll(String sys, String SubSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

}