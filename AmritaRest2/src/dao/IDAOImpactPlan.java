/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityImpactPlan;
import enums.EnumObject;
import enums.EnumTypeImpactChange;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity ImpactPlan.
 */
public interface IDAOImpactPlan extends IDAO<EntityImpactPlan> {

	/*
	 * 1) Get all operations of a specific impact plan
	 */
	public  List<EntityImpactPlan> findAll(String sys, String idPlan) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get a specific  operation of a specific impact plan
	 */
	public  List<EntityImpactPlan> findAll(String sys, String idPlan, int numOp) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all operations related to an object (map, pgm, copy, ...)
	 */
	public  List<EntityImpactPlan> findAll(String sys, String idObject, EnumObject typeObject) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 4) Get all operations related to a change type in a specific subSys
	 */
	public  List<EntityImpactPlan> findAll(String sys, String subSys, EnumTypeImpactChange typeImpactChange) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 5) Get all operations related to a change type for a specific object
	 */
	public  List<EntityImpactPlan> findAll(String sys, String idObject, EnumObject typeObject, EnumTypeImpactChange typeImpactChange) throws SQLException, ExceptionAmritaSqlError; 

}