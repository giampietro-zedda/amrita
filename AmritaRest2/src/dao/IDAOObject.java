/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityObject;
import enums.EnumObject;
import enums.EnumObjectStatus;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAOObject extends IDAO<EntityObject> {

	/*
	 * 1) Get all Object of the type specified  (i.e. COBOL_PROGRAMS)
	 */
	public  List<EntityObject> findAll(String sys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all Object of the type specified (i.e. COBOL_PROGRAMS)
	 */
	public  List<EntityObject> findAll(String sys, String SubSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all Object of the type status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String sys, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get all Object of the type status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String sys, String subSys, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all Object of the type and status specified (i.e. COBOL_PROGRAM & ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String sys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 6) Get all Object of the type and status specified (i.e. COBOL_PROGRAM & ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String sys, String subSys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 7) Get all Object of the system & subsystem specified
	 */
	public  List<EntityObject> findAll(String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 8) Get all object with a give type Object in all systems
	 */
	public  List<EntityObject> findAll(EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 9) Get all object in a system  
	 */
	public  List<EntityObject> findAll(String sys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 10) Get a specific Object 
	 */
	public  List<EntityObject> findAll(String sys, String SubSys, EnumObject typeObject, String idObject) throws SQLException, ExceptionAmritaSqlError; 


	//
    // Begin entries with options and like
	//
	/*
	 * 11) Get all Object of the type specified  (i.e. COBOL_PROGRAMS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject, String sys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 12) Get all Object of the type specified (i.e. COBOL_PROGRAMS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject, String sys, String SubSys, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 13) Get all Object of the type status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject, String sys, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 14) Get all Object of the type status (i.e. ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject,  String sys, String subSys, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 15) Get all Object of the type and status specified (i.e. COBOL_PROGRAM & ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject,  String sys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 16) Get all Object of the type and status specified (i.e. COBOL_PROGRAM & ANALYZED_WITH_NO_ERRORS)
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject,  String sys, String subSys, EnumObject typeObject, EnumObjectStatus statusObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 17) Get all Object of the system & subsystem specified
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject,  String sys, String subSys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 18) Get all object with a give type Object
	 */
	public  List<EntityObject> findAll(String likeIdObject, String [] options,EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 19) Get all object in a system  
	 */
	public  List<EntityObject> findAll(String [] options, String likeIdObject,  String sys) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 20) Get all object in a system of a specific type regardless the subsystem 
	 */
	public  List<EntityObject> findProgramsByName(String sys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

}