package rest;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import analyzer.DataBaseConnections;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import dao.DAOFactory;
import dao.IDAOSqlGeneric;
import dao.MySQLDAOFactory;
import entities.EntitySqlGeneric;
import enums.EnumObject;
import enums.EnumRelation;
import exception.ExceptionAmrita;



@Path("/")   
public class RestDbAccessDAOSqlGeneric {

	@GET  
	@Produces("application/json")
	@Path("crudMatrix/{user}/{sys}/{subSys}/{idPgmFrom}/{idPgmTo}/{idEntityFrom}/{idEntityTo}")  
	public Response getCruMatrix(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idPgmFrom")    String idPgmFrom
			, @PathParam("idPgmTo")      String idPgmTo
			, @PathParam("idEntityFrom") String idEntityFrom
			, @PathParam("idEntityTo")   String idEntityTo

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonCrudMatrix(user, sys, subSys, idPgmFrom, idPgmTo, idEntityFrom, idEntityTo);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	private String getResultJsonCrudMatrix(String user, String sys, String subSys, String idPgmFrom, String idPgmTo, String idEntityFrom, String idEntityTo) throws  JSONException, SQLException, ExceptionAmrita {
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String result = "";
	       
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		//1 sys/subSys/*/*/*/*
		if (!subSys.equals("*") && idPgmFrom.equals("*") && idPgmTo.equals("*") && idEntityFrom.equals("*") && idEntityTo.equals("*")) {
			ls_object=dao.sqlCrudMatrix(sys, subSys);
		//2 sys/subSys/idPgmFrom/idPgmTo/idEntityFrom/idEntityTo
		} else if (!subSys.equals("*") && !idPgmFrom.equals("*") && !idPgmTo.equals("*") && !idEntityFrom.equals("*") && !idEntityTo.equals("*")) {
			ls_object=dao.sqlCrudMatrix(sys, subSys, idPgmFrom, idPgmTo, idEntityFrom, idEntityTo);	
		//3 sys/subSys/idPgmFrom/idPgmTo/*/*
		} else if (!subSys.equals("*") && !idPgmFrom.equals("*") && !idPgmTo.equals("*") && idEntityFrom.equals("*") && idEntityTo.equals("*")) {
			ls_object=dao.sqlCrudMatrixByPgm(sys, subSys, idPgmFrom, idPgmTo);	
		//3 sys/subSys/*/*/idEntityFrom/idEntityTo
		} else if (!subSys.equals("*") && !idPgmFrom.equals("*") && !idPgmTo.equals("*") && !idEntityFrom.equals("*") && !idEntityTo.equals("*")) {
			ls_object=dao.sqlCrudMatrixByEntity(sys, subSys, idEntityFrom, idEntityTo);	

		// Not managed
		} else {
			ls_object=new ArrayList<EntitySqlGeneric>();
		}
		
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric entityObjectOption : ls_object) {
			jsonObject = new JSONObject(entityObjectOption);
			populateJsonCrudMatrix(entityObjectOption, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		dao.setConn(null);
		
		return result;	
}


	private void populateJsonCrudMatrix(EntitySqlGeneric e, JSONObject jsonObject) {
 		jsonObject.put("sys", e.getSys()); 		
 		jsonObject.put("subSys", e.getSubSystem());
 		jsonObject.put("pgm", e.getPgm()); 		
 		jsonObject.put("entity", e.getEntity());
 		jsonObject.put("relation", e.getRelation());
 		jsonObject.put("relationOrdinal", e.getRelation().ordinal());
	}	

	@GET  
	@Produces("application/json")
	@Path("treeViewRelationsGrouped/{user}/{sys}/{subSys}/{typeObject}/{idObject}/{isDirectYN}")  
	public Response getTreeViewRelationsGrouped(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String typeObject
			, @PathParam("idObject")     String idObject
			, @PathParam("isDirectYN")   String isDirectYN

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
		int typeObjectN = 0;
		boolean isDirect = false;
		
		if (isDirectYN.equals("Y")) {
			isDirect = true;
		} else {
			isDirect = false;
		}
		typeObjectN = Integer.parseInt(typeObject);
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlTreeViewRelations(sys, subSys, typeObjectN, idObject, isDirect);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();
			// Gestione specifica per copy
			if (eo.getRelation() == EnumRelation.PGM_COPY ) {
				if (eo.getTypeObjectB() == EnumObject.OBJECT_COPY_COBOL_DATA) {
					jsonObject.put("relation", "PGM_COPY_DATA"); 	
				} else {
					jsonObject.put("relation", "PGM_COPY_PROC"); 
				}
			} else {
				jsonObject.put("relation", eo.getRelation()); 	
			}
	 			
	 		jsonObject.put("typeObjectA", eo.getTypeObjectA().ordinal()); 		
	 		jsonObject.put("typeObjectB", eo.getTypeObjectB().ordinal()); 		
	 		jsonObject.put("relationOrdinal", eo.getRelation().ordinal()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	
	@GET  
	@Produces("application/json")
	@Path("subSysOwnerUsed/{user}/{sys}/{subSys}")  
	public Response getSubSysShared(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys 

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlSubSysOwnerUsed(sys, subSys);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("subSys", eo.getSubSystem());
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	


	@GET  
	@Produces("application/json")
	@Path("typeObjectsShared/{user}/{sys}/{subSysOwner}/{subSys}")  
	public Response getTypeObjectsShared(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSysOwner")  String subSysOwner
			, @PathParam("subSys")       String subSys

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlTypeObjectsShared(sys, subSysOwner, subSys);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("typeObject", eo.getTypeObjectA()); 		
	 		jsonObject.put("typeObjectOrdinal", eo.getTypeObjectA().ordinal()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	

	@GET  
	@Produces("application/json")
	@Path("objectsOwned/{user}/{sys}/{subSys}/{typeObject}")  
	public Response getObjectsOwned(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String typeObject

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlObjectsOwned(sys, subSys, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("idObject", eo.getIdObject()); 		
	 		jsonObject.put("istatusObject", eo.getStatusObject());	
	 		jsonObject.put("istatusObjectOrdinal", eo.getStatusObject().ordinal());	
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);		
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	


	
	@GET  
	@Produces("application/json")
	@Path("objectsShared/{user}/{sys}/{subSysOwner}/{subSys}/{typeObject}")  
	public Response getObjectsShared(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSysOwner")  String subSysOwner
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String typeObject

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlObjectsShared(sys, subSysOwner, subSys, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("subSys", eo.getSubSystem()); 		
	 		jsonObject.put("idObject", eo.getIdObject()); 		
	 		jsonObject.put("statusObject", eo.getStatusObject()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	

	@GET  
	@Produces("application/json")
	@Path("qualityConfigurations/{user}/{sys}/{subSys}/")  
	public Response getQualityConfigurations(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys

			) throws JSONException, SQLException, ExceptionAmrita {
	
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 
		UserConfiguration ucfg = null;
		List<EntitySqlGeneric> ls_object = null;
		String resultJson = "";
				
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
		Connection conn = DataBaseConnections.getConnection();
		IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
	    
		ls_object=dao.sqlQualityConfig(sys, subSys);
		
	    jsonArray = new JSONArray();

		// Populate Json
		for (EntitySqlGeneric eo : ls_object) {
			jsonObject = new JSONObject();	 			
	 		jsonObject.put("subSys", subSys); 		
	 		jsonObject.put("configName", eo.getIdObject()); 		
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		dao.setConn(null);

		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}	

}  
