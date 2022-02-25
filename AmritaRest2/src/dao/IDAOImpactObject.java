/**
 * Specific method defined for DAO Object
 */
package dao;
import java.sql.SQLException;
import java.util.List;

import entities.EntityImpactObject;
import enums.EnumObject;
import exception.ExceptionAmritaSqlError;

/**
 * Interfaccia DAO per specifiche operazioni per entity ImpactPlan.
 */
public interface IDAOImpactObject extends IDAO<EntityImpactObject> {

	/*
	 * 1) Get all impact changes of a specific operation number o a specific object target
	 */
	public  List<EntityImpactObject> findAll(String sys, String idPlan, int numOp, EnumObject typeObjectOrigin, String idObjectOrigin, EnumObject typeObjectTarget, String idObjectTarget) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 2) Get all impact changes regardless operation number of a specific object target
	 */
	public  List<EntityImpactObject> findAll(String sys, String idPlan, EnumObject typeObjectOrigin, String idObjectOrigin, EnumObject typeObjectTarget, String idObjectTarget) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 * 3) Get all impact changes of a specific operation number regardless the object target
	 */
	public  List<EntityImpactObject> findAll(String sys, String idPlan, int numOp, EnumObject typeObjectOrigin, String idObjectOrigin) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 *4) Get all impact changes regardless operation number and the object target
	 */
	public  List<EntityImpactObject> findAll(String sys, String idPlan, EnumObject typeObjectOrigin, String idObjectOrigin) throws SQLException, ExceptionAmritaSqlError; 

	/*
	 *5) Get all impact changes regardless any filter
	 */
	public  List<EntityImpactObject> findAll(String sys, String idPlan) throws SQLException, ExceptionAmritaSqlError; 

}