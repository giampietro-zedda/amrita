/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;
import entities.EntityRelationOrigin;
import enums.EnumObject;
import enums.EnumRelation;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity Object.
 */
public interface IDAORelationOrigin extends IDAO<EntityRelationOrigin> {

	/*
	 * 1) Get all origin of relation between 2 specific objects
	 */
	public  List<EntityRelationOrigin> findAllOrigin(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumRelation relation, String idObjectB, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all origin of relation between an object and all object of a specific type
	 */
	public  List<EntityRelationOrigin> findAllOrigin(String sys, String subSys, String idObjectA, EnumObject typeObjectA, EnumRelation relation, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all origin of inverse relation between an object and all object of a specific type
	 */
	public  List<EntityRelationOrigin> findAllOrigin(String sys, String subSys, EnumObject typeObjectA, EnumRelation relation,  String idObjectB, EnumObject typeObjectB) throws SQLException, ExceptionAmritaSqlError; 

}