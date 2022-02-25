/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityRelation;
import enums.EnumObject;
import enums.EnumRelation;
import enums.EnumSourceType;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAORelation extends IDAO<EntityRelation> {

	/*
	 * 1) Get all object names related to an object
	 */
	public  List<EntityRelation> findAllBySubSysIdARel(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all object names in relation with an object in a given subSys
	 */
	public  List<EntityRelation> findAllBySubSysIdBRel(String sys, String subSys, String idObjectB, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get the relation between two objects
	 */
	public  List<EntityRelation> findAllBySubSysIdAIdBRel(String sys, String subSys,  String idObjectA, EnumObject typeObjectA, EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get all relations of a specific object in relation with  
	 */
	public  List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumSourceType typeSourceA) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all relations of a specific object related to
	 */
	public  List<EntityRelation> findAllBySubSysIdB(String sys, String subSys, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 6) Get all relations between two objects
	 */
	public  List<EntityRelation> findAllBySubSysIdAIdB(String sys, String subSys,  String idObjectA, EnumObject typeObjectA, EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 7) Get all relations of a specific object in relation with in all subSys 
	 */
	public  List<EntityRelation> findAllByIdA(String sys, String idObjectA, EnumObject typeObjectA) throws SQLException, ExceptionAmritaSqlError;  

	/*
	 * 8) Get all relations of a specific object related to in all subSys 
	 */
	public  List<EntityRelation> findAllByIdB(String sys, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 9) Get all relations of a specific object related to a specific object with no type specified 
	 */
	public  List<EntityRelation> findAllByIdAIdB(String sys, String subSys,  String idObjectA, EnumObject typeObjectA, EnumRelation relation, String idObjectB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 10) Get all relations in a subsys regardless id objects and origin type
	 */
	public  List<EntityRelation> findAllByIdAIdB(String sys, String subSys,  EnumRelation relation, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 11) Get all relations of a specific object with a type object 
	 */
	public  List<EntityRelation> findAllByIdA(String sys, String subSys,  String idObjectA, EnumObject typeObjectA, EnumRelation relation, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 


	/*
	 * 12) Get all relations of a specific type regardless subSys, id and type
	 */
	public  List<EntityRelation> findAll(String sys, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 


	/*
	 * 13) Get all object names in relation with an object regardless subSys
	 */
	public  List<EntityRelation> findAllBySubSysIdBRel(String sys, String idObjectB, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 14) Get all relations between two objects regardless subsys
	 */
	public  List<EntityRelation> findAllBySubSysIdAIdB(String sys, String idObjectA, EnumObject typeObjectA, EnumSourceType typeSourceA, String idObjectB, EnumObject typeObjectB, EnumSourceType typeSourceB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 15) Get all relations with a object regardless the subsys
	 */
	public  List<EntityRelation> findAllByIdARel(String sys, String idObjectA, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 16) Get all relations with a object 
	 */
	public  List<EntityRelation> findAllByIdARel(String sys, String idObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 17) Get all relations of a specific object in relation with in all subSys 
	 */
	public  List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError;  


	/*
	 * 18) Get all relations of a specific type in a subSys
	 */
	public  List<EntityRelation> findAllBySubSysRel(String sys, String subSys, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError;  

	/*
	 * 19) Get all relations of a specific type in all subsys
	 */
	public  List<EntityRelation> findAllByRel(String sys,  EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError;  

	/*
	 * 20) Get all relations of a specific type in all subsys
	 */
	public  List<EntityRelation> findAllByRel(String sys, String subSys, EnumObject typeObjectA, EnumRelation relation) throws SQLException, ExceptionAmritaSqlError;  

	/*
	 * 21) Get all relations of a specific type object in relation with  in the subSys
	 */
	public  List<EntityRelation> findAllBySubSysTypeA(String sys, String subSys, EnumObject typeObjectA) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 22) Get all relations of a specific object in relation with  in the subSys
	 */
	public  List<EntityRelation> findAllBySubSysIdA(String sys, String subSys, String idObjectA) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 23) Get all relations of a specific object in relation with  in the subSys
	 */
	public  List<EntityRelation> findAllBySubSysIdARelIdB(String sys, String subSys, String idObjectA, EnumRelation relation, String idObjectB) throws SQLException, ExceptionAmritaSqlError; 
	/*
	 * 24) Get all relations between a object and a typeObject
	 */
	public  List<EntityRelation> findAllBySubSysIdATypeATypeB(String sys, String subSys,  String idObjectA, EnumObject typeObjectA, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 


}