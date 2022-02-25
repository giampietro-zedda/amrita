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

import analyzer.AmritaStartup;
import analyzer.DataBaseConnections;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import dao.IDAOCopyEntityDefinition;
import dao.IDAODynamicCicsMapping;
import dao.IDAODynamicField;
import dao.IDAODynamicFieldSub;
import dao.IDAODynamicFieldSubSetting;
import dao.IDAODynamicFieldSubValue;
import dao.IDAODynamicFieldSubWaitExt;
import dao.IDAODynamicValueExt;
import dao.IDAOImpactCompile;
import dao.IDAOImpactObject;
import dao.IDAOImpactPlan;
import dao.IDAOMapDescriptor;
import dao.IDAOMapItem;
import dao.IDAOMetricValue;
import dao.IDAOMetricViolation;
import dao.IDAOMetricViolationConfig;
import dao.IDAOObject;
import dao.IDAOObjectAnalysisError;
import dao.IDAOObjectAnalysisInfo;
import dao.IDAOObjectOption;
import dao.IDAOProcessLog;
import dao.IDAORelation;
import dao.IDAORelationOrigin;
import dao.IDAOSqlGeneric;
import dao.IDAOUser;
import dao.IDAOWhereUsedItem;
import dao.DAOImplCopyEntityDefinition;
import dao.DAOImplDynamicCicsMapping;
import dao.DAOImplDynamicField;
import dao.DAOImplDynamicFieldSub;
import dao.DAOImplDynamicFieldSubSetting;
import dao.DAOImplDynamicFieldSubValue;
import dao.DAOImplDynamicFieldSubWaitExt;
import dao.DAOImplDynamicValueExt;
import dao.DAOImplImpactCompile;
import dao.DAOImplImpactObject;
import dao.DAOImplImpactPlan;
import dao.DAOImplMapDescriptor;
import dao.DAOImplMapItem;
import dao.DAOImplMetricValue;
import dao.DAOImplMetricViolation;
import dao.DAOImplMetricViolationConfig;
import dao.DAOImplObject;
import dao.DAOImplObjectAnalysisError;
import dao.DAOImplObjectAnalysisInfo;
import dao.DAOImplObjectOption;
import dao.DAOImplObjectShort;
import dao.DAOImplProcessLog;
import dao.DAOImplRelation;
import dao.DAOImplRelationOrigin;
import dao.DAOImplSqlGeneric;
import dao.DAOImplUser;
import dao.DAOImplWhereUsedItem;
import entities.EntityCopyEntityDefinition;
import entities.EntityDynamicCicsMapping;
import entities.EntityDynamicField;
import entities.EntityDynamicFieldSub;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import entities.EntityDynamicFieldSubWaitExt;
import entities.EntityDynamicValueExt;
import entities.EntityImpactCompile;
import entities.EntityImpactObject;
import entities.EntityImpactPlan;
import entities.EntityMapDescriptor;
import entities.EntityMapItem;
import entities.EntityMetricValue;
import entities.EntityMetricViolation;
import entities.EntityMetricViolationConfig;
import entities.EntityObject;
import entities.EntityObjectAnalysisError;
import entities.EntityObjectAnalysisInfo;
import entities.EntityObjectOption;
import entities.EntityObjectShort;
import entities.EntityProcessLog;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import entities.EntitySqlGeneric;
import entities.EntityUser;
import entities.EntityWhereUsedItem;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumDirectivesExecution;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsQualityFactors;
import enums.EnumMetricsQualityKpi;
import enums.EnumTypeImpactChange;
import enums.EnumMetricsScope;
import enums.EnumMetricsViolation;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumRelation;
import exception.ExceptionAmrita;

/*
 * Web service for Login, the user configuration will be fulfilled from database User table
 * If the user supplied is not active, it will be added to the list of active users
 * 
 */
@Path("/")   
public class RestDbAccessDAOEntity {


	
	@GET  
	@Produces("application/json")
	@Path("user/{user}")  
	public Response getUsers(@PathParam("user") String user) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonUser(user);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	@GET  
	@Produces("application/json")
	@Path("object/{user}/{sys}/{subSys}/{typeObject}/{statusObject}/{idObjectLike}/{options}")  
	public Response getObjects(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String type 
			, @PathParam("statusObject") String status 
			, @PathParam("idObjectLike") String like 
			, @PathParam("options")      String options 

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonObject(user, sys, subSys, type, status, like, options);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("objectShort/{user}/{sys}/{subSys}/{typeObject}/{statusObject}")  
	public Response getObjectsShort(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String type 
			, @PathParam("statusObject") String status 

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonObjectShort(user, sys, subSys, type, status);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	@GET  
	@Produces("application/json")
	@Path("objectOption/{user}/{sys}/{subSys}/{typeObject}/{idObject}/{option}")  
	public Response getObjectOptions(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("typeObject")   String type 			
			, @PathParam("idObject")     String idObject 
			, @PathParam("option")       String option 

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonObjectOption(user, sys, subSys, type, idObject, option);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	@GET  
	@Produces("application/json")
	@Path("relation/{user}/{sys}/{subSys}/{idObjectA}/{typeObjectA}/{typeSourceA}/{relation}/{idObjectB}/{typeObjectB}/{typeSourceB}")  
	public Response getRelations(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObjectA")    String idObjectA 
			, @PathParam("typeObjectA")  String typeA 			
			, @PathParam("typeSourceA")  String sourceA 
			, @PathParam("relation")     String relation 
			, @PathParam("idObjectB")    String idObjectB 
			, @PathParam("typeObjectB")  String typeB			
			, @PathParam("typeSourceB")  String sourceB

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonRelation(user, sys, subSys, idObjectA, typeA, sourceA, relation, idObjectB, typeB, sourceB);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("relationOrigin/{user}/{sys}/{subSys}/{idObjectA}/{typeObjectA}/{typeSourceA}/{relation}/{idObjectB}/{typeObjectB}/{typeSourceB}")  
	public Response getRelationsOrigin(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObjectA")    String idObjectA 
			, @PathParam("typeObjectA")  String typeA 			
			, @PathParam("typeSourceA")  String sourceA 
			, @PathParam("relation")     String relation 
			, @PathParam("idObjectB")    String idObjectB 
			, @PathParam("typeObjectB")  String typeB			
			, @PathParam("typeSourceB")  String sourceB

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonRelationOrigin(user, sys, subSys, idObjectA, typeA, relation, idObjectB, typeB);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("copyEntityDefinition/{user}/{sys}/{subSys}/{idObject}/{typeObject}")  
	public Response getCopyEntityDefinition(
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 			

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonCopyEntityDefinition(user, sys, subSys, idObject, type);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("whereUsed/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{idField}/{numSeq}")  
	public Response getwhereUsed(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 			
			, @PathParam("idField")      String idField 			
			, @PathParam("numSeq")       String numSeq 			

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonWhereUsed(user, sys, subSys, idObject, type, idField, numSeq);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("dynamicField/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{numInstr}/{idField}")  
	public Response getDynamicField(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 			
			, @PathParam("numInstr")     String numInstr 			
			, @PathParam("idField")      String idField 			

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicField(user, sys, subSys, idObject, type, numInstr, idField);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	@GET  
	@Produces("application/json")
	@Path("dynamicFieldSub/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{numInstr}/{idField}")  
	public Response getDynamicFieldSub(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 			
			, @PathParam("numInstr")     String numInstr 			
			, @PathParam("idField")      String idField 			

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicFieldSub(user, sys, subSys, idObject, type, numInstr, idField);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("DynamicSubFieldSetting/{user}/{sys}/{subSys}/{idObject}/{numInstr}/{idField}/{idSubField}")  
	public Response getDynamicFieldSubSetting(
											  @PathParam("user")         String user
											, @PathParam("sys")          String sys
											, @PathParam("subSys")       String subSys
											, @PathParam("idObject")     String idObject           // Pgm origin
											, @PathParam("typeObject")   String type 			
											, @PathParam("numInstr")     String numInstr  
											, @PathParam("idField")      String idField 
											, @PathParam("idSubField")   String idSubField 
											, @PathParam("idPgmSet")     String idPgmSet 											
											, @PathParam("numChain")     String numChain 
									
											) throws JSONException, SQLException, ExceptionAmrita {
				
		String resultJson=getResultJsonDynamicFieldSubSetting(user, sys, subSys, idObject, type, numInstr, idField, idSubField, idPgmSet, numChain);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("dynamicFieldSubValue/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{numInstr}/{idField}/{idSubField}")  
	public Response getDynamicFieldSubValue(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 			
			, @PathParam("numInstr")     String numInstr 			
			, @PathParam("idField")      String idField 	
			, @PathParam("idSubField")   String idSubField 	

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicFieldSubValue(user, sys, subSys, idObject, type, numInstr, idField, idSubField);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("objectAnalysisError/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{rowNum}")  
	public Response getObjectAnalysisError(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 						
			, @PathParam("rowNum")       String rowNum 			

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonObjectAnalysisError(user, sys, subSys, idObject, type, rowNum);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("objectAnalysisInfo/{user}/{sys}/{subSys}/{idObject}/{typeObject}")  
	public Response getObjectAnalysisInfo(  
			  @PathParam("user")         String user
			, @PathParam("sys")          String sys
			, @PathParam("subSys")       String subSys
			, @PathParam("idObject")     String idObject 
			, @PathParam("typeObject")   String type 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonObjectAnalysisInfo(user, sys, subSys, idObject, type);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("metricViolationConfiguration/{user}/{sys}/{subSys}/{configName}/{typeViolation}/{qualityFactor}/{qualityCharacteristic}")  
	public Response getMetricViolationConfig(   
			  @PathParam("user")          			String user
			, @PathParam("sys")           			String sys
			, @PathParam("subSys")        			String subSys
			, @PathParam("configName")    			String configName
			, @PathParam("typeViolation") 			String typeViolation 						
			, @PathParam("qualityFactor") 			String qualityFactor 						
			, @PathParam("qualityCharacteristic") 	String qualityCharacteristic 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonMetricViolationConfig(user, sys, subSys, configName, typeViolation, qualityFactor, qualityCharacteristic);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}


	@GET  
	@Produces("application/json")
	@Path("metricViolation/{user}/{sys}/{subSys}/{scope}/{idObject}/{typeObject}/{section}/{typeViolation}")  
	public Response getMetricViolation(   
			  @PathParam("user")          String user
			, @PathParam("sys")           String sys
			, @PathParam("subSys")        String subSys
			, @PathParam("scope")         String scope
			, @PathParam("idObject")      String idObject 
			, @PathParam("typeObject")    String typeObject 						
			, @PathParam("section")       String section 						
			, @PathParam("typeViolation") String typeViolation 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonMetricViolation(user, sys, subSys, scope, idObject, typeObject, section, typeViolation);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("metricValue/{user}/{sys}/{subSys}/{scope}/{idObject}/{typeObject}/{section}")  
	public Response getMetricValue(  
			  @PathParam("user")          String user
			, @PathParam("sys")           String sys
			, @PathParam("subSys")        String subSys
			, @PathParam("scope")         String scope
			, @PathParam("idObject")      String idObject 
			, @PathParam("typeObject")    String typeObject 						
			, @PathParam("section")       String section 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonMetricValue(user, sys, subSys, scope, idObject, typeObject, section);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("dynamicFieldSubWaitExt/{user}/{sys}/{subSys}/{idObject}/{typeObject}/{numInstr}/{idField}/{idSubField}")  
	public Response getDynamicFieldSubWaitExt(  
			  @PathParam("user")          String user
			, @PathParam("sys")           String sys
			, @PathParam("subSys")        String subSys
			, @PathParam("idObject")      String idObject 
			, @PathParam("typeObject")    String typeObject 						
			, @PathParam("numInstr")      String numInstr 						
			, @PathParam("idField")       String idField 						
			, @PathParam("idSubField")    String idSubField 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicFieldSubWaitExt(user, sys, subSys, idObject, typeObject, numInstr, idField, idSubField);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("dynamicValueExt/{user}/{sys}/{subSys}/{idObjectExternal}/{typeObjectExternal}/{idFieldColumnExternal}/{dsname}/{typeSystemFieldExternal}/{idSystemFieldExternal}/{CicsNameExternal}")  
	public Response getDynamicValueExt(  
			  @PathParam("user")          			String user
			, @PathParam("sys")           			String sys
			, @PathParam("subSys")        			String subSys
			, @PathParam("idObject")      			String idObjectExternal 
			, @PathParam("typeObjectExternal")    	String typeObjectExternal 						
			, @PathParam("idFieldColumnExternal")	String idFieldColumnExternal 
			, @PathParam("typeObject")   			String dsname 						
			, @PathParam("numInstr")      			String typeSystemFieldExternal 						
			, @PathParam("idField")       			String idSystemFieldExternal 						
			, @PathParam("idSubField")    			String CicsNameExternal 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicValueExt(user, sys, subSys, idObjectExternal, typeObjectExternal, idFieldColumnExternal, dsname, typeSystemFieldExternal, idSystemFieldExternal, CicsNameExternal);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	@GET  
	@Produces("application/json")
	@Path("dynamicCicsMapping/{user}/{sys}/{subSys}/{typeObject}/{externalName}/{cicsName}")  
	public Response getDynamicCicsMapping(  
			  @PathParam("user")          			String user
			, @PathParam("sys")           			String sys
			, @PathParam("subSys")        			String subSys
			, @PathParam("typeObject")    			String typeObject 						
			, @PathParam("typeObject")   			String externalName 						
			, @PathParam("numInstr")      			String cicsName 												

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonDynamicCicsMapping(user, sys, subSys, typeObject, externalName, cicsName);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	@GET  
	@Produces("application/json")
	@Path("mapDescriptor/{user}/{sys}/{subSys}/{idObject}/{typeObject}")  
	public Response getMapDescriptor(  
			  @PathParam("user")          String user
			, @PathParam("sys")           String sys
			, @PathParam("subSys")        String subSys
			, @PathParam("idObject")      String idObject    // Map or Mapset
			, @PathParam("typeObject")    String typeObject 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonMapDescriptor(user, sys, subSys, idObject, typeObject);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	

	@GET  
	@Produces("application/json")
	@Path("mapItem/{user}/{sys}/{subSys}/{idObject}/{typeObject}")  
	public Response getMapItem(  
			  @PathParam("user")          String user
			, @PathParam("sys")           String sys
			, @PathParam("subSys")        String subSys
			, @PathParam("idObject")      String idObject 
			, @PathParam("typeObject")    String typeObject 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonMapItem(user, sys, subSys, idObject, typeObject);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	
	@GET  
	@Produces("application/json")
	@Path("impactPlan/{user}/{sys}/{subSys}/{idPlan}/{numOp}/{typeObjectOrigin}/{idObjectOrigin}/{typeImpactChange}")  
	public Response getImpactPlan(  
			  @PathParam("user")          		String user
			, @PathParam("sys")           		String sys
			, @PathParam("subSys")        		String subSys
			, @PathParam("idPlan")      		String idPlan 
			, @PathParam("numOp")    			String numOp 						
			, @PathParam("typeObjectOrigin")    String typeObjectOrigin						
			, @PathParam("idObjectOrigin")    	String idObjectOrigin						
			, @PathParam("typeImpactChange")    String typeImpactChange 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonImpactPlan(user, sys, subSys, idPlan, numOp, typeObjectOrigin, idObjectOrigin, typeImpactChange);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	@GET  
	@Produces("application/json")
	@Path("impactObject/{user}/{sys}/{idPlan}/{numOp}/{typeObjectOrigin}/{idObjectOrigin}/{typeObjectTarget}/{idObjectTarget}")  
	public Response getImpactObject(  
			  @PathParam("user")          		String user
			, @PathParam("sys")           		String sys
			, @PathParam("idPlan")      		String idPlan 
			, @PathParam("numOp")    			String numOp 						
			, @PathParam("typeObjectOrigin")    String typeObjectOrigin						
			, @PathParam("idObjectOrigin")    	String idObjectOrigin						
			, @PathParam("typeObjectTarget")    String typeObjectTarget						
			, @PathParam("idObjectTarget")    	String idObjectTarget						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonImpactObject(user, sys, idPlan, numOp, typeObjectOrigin, idObjectOrigin, typeObjectTarget, idObjectTarget);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	@GET  
	@Produces("application/json")
	@Path("impactPgmToRecompile/{user}/{sys}/{idPlan}")  
	public Response getImpactPgmTpRecompile(  
			  @PathParam("user")          		String user
			, @PathParam("sys")           		String sys
			, @PathParam("idPlan")      		String idPlan 

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonImpactPgmTpRecompile(user, sys, idPlan);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
		
	@GET  
	@Produces("application/json")
	@Path("processLog/{user}/{sys}/{idObject}/{typeProcess}/{dtStartProcess}/{tmStartProcess}")  
	public Response getProcessLog(  
			  @PathParam("user")          		String user
			, @PathParam("sys")           		String sys
			, @PathParam("idObject")      		String idObject 
			, @PathParam("typeProcess")   		String typeProcess 						
			, @PathParam("dtStartProcess")   	String dtStartProcess 						
			, @PathParam("tmStartProcess")   	String tmStartProcess 						

			) throws JSONException, SQLException, ExceptionAmrita {

		String resultJson=getResultJsonProcessLog(user, sys, idObject, typeProcess, dtStartProcess, tmStartProcess);
		return Response
				.status(200)
				.entity(resultJson)
				.build();
	}
	
	
	@GET  
	@Produces("application/json")
	@Path("enum/{user}/{enumName}")  
	public Response getEnums(
							    @PathParam("user") String user
							   ,@PathParam("enumName") String enumName
							) throws JSONException, SQLException {

		JSONObject jsonObject = null; 
		JSONArray jsonArray = new JSONArray();; 		
		String result = "";

		switch (enumName) {
		case "EnumObject":
			for (EnumObject enumO : EnumObject.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;
		case "EnumObjectStatus":
			for (EnumObjectStatus enumO : EnumObjectStatus.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumObjectOption":
			for (EnumObjectOption enumO : EnumObjectOption.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumRelation":
			for (EnumRelation enumO : EnumRelation.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumDirectivesExecution":
			for (EnumDirectivesExecution enumO : EnumDirectivesExecution.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumImpactType":
			for (EnumTypeImpactChange enumO : EnumTypeImpactChange.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumMetricsQualityKpi":
			for (EnumMetricsQualityKpi enumO : EnumMetricsQualityKpi.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;

		case "EnumMetricsQualityFactors":
			for (EnumMetricsQualityFactors enumO : EnumMetricsQualityFactors.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;
			
		case "EnumMetricsQualityCharacteristics":
			for (EnumMetricsQualityCharacteristics enumO : EnumMetricsQualityCharacteristics.values()) {
				jsonObject = new JSONObject();
				jsonObject.put("ordinal", enumO.ordinal()+"");
				jsonObject.put("value", enumO.toString());
				jsonArray.put(jsonObject);
			}
			break;
			
		default:
			break;
		}

		result = jsonArray.toString();	

		return Response
				.status(200)
				.entity(result)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("sys/{user}")  
	public Response getAllSys(@PathParam("user") String user)   throws JSONException, SQLException, ExceptionAmrita {
		String result = "";
		result = getAllSysSubSys(user, EnumObject.OBJECT_SYS);		
		return Response
				.status(200)
				.entity(result)
				.build();
	}

	@GET  
	@Produces("application/json")
	@Path("subsys/{user}")  
	public Response getAllSubSys(@PathParam("user") String user)   throws JSONException, SQLException, ExceptionAmrita {
		String result = "";
		result = getAllSysSubSys(user, EnumObject.OBJECT_SUBSYS);		
		return Response
				.status(200)
				.entity(result)
				.build();
	}

	private String getAllSysSubSys(String user, EnumObject e)   throws JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObject> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);

		ls_object=eoDAO.findAll(e);

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObject entityObject : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObject(entityObject, jsonObject);
			jsonArray.put(jsonObject);
		}
        
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		result = jsonArray.toString();	
		return result;
	}	

	private String getResultJsonUser(String user) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null;
		List<EntityUser> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOUser eoDAO = (DAOImplUser) AmritaStartup.sqlFactory.getDAOUser(conn, false, false, ucfg);

		if (user.equals("*")) {
				ls_object=eoDAO.findAll();
		} else {
				ls_object=eoDAO.findAll(user);
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityUser entityUser : ls_object) {
			jsonObject = new JSONObject();
			populateJsonUser(entityUser, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;
	}  

	private String getResultJsonObject(String user, String sys, String subSys, String type, String status, String likeInp, String options) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObject> ls_object = null;
		String result = "";
		String like = likeInp.replace('*', '%');
		String[] ar_opt = null;;
		if (!options.equals("*")) {
			options = options.replace("|", " ");
			ar_opt = options.split(" ");
		} else {
			ar_opt = new String[0];
		}
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
		
		//1 sys/*/type/*
		if (subSys.equals("*") && !type.equals("*") && status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, EnumObject.values()[Integer.parseInt(type.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, EnumObject.values()[Integer.parseInt(type.trim())]);
			}

		//2           sys/subSys/type/*
		//12 opt/like/sys/subSys/type/*
		} else if (!subSys.equals("*") && !type.equals("*") && status.equals("*")) {		
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())]);	
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())]);	
			}

		//3           sys/*/*/status
		//13 opt/like/sys/*/*/status
		} else if (subSys.equals("*") && type.equals("*") && !status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			}

		//4           sys/subSys/*/status
		//14 opt/like/sys/subSys/*/status
		} else if (!subSys.equals("*") && type.equals("*")  && !status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys,subSys, EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys,subSys, EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			}

		//5           sys/*/type/status 
	    //15 opt/like/sys/*/type/status	
		} else if (subSys.equals("*") && !type.equals("*") && !status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			}

		//6           sys/subSys/type/status
		//16 opt/like/sys/subSys/type/status
		} else if (!subSys.equals("*") && !type.equals("*") && !status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
			}

		//7           sys/subSys/*/*
		//17 opt/like/sys/subSys/*/*
		} else if (!subSys.equals("*") && type.equals("*") && status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys, subSys);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys, subSys);
			}

		//8           */*/type/*
		//18 opt/like/*/*/type/*
		} else if (subSys.equals("*") && !type.equals("*") && status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(EnumObject.values()[Integer.parseInt(type.trim())]);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys,  EnumObject.values()[Integer.parseInt(type.trim())]);
			}

		//9           sys/*/*/*
		//19 opt/like/sys/*/*/*
		} else if (subSys.equals("*") && type.equals("*") && status.equals("*")) {
			if (likeInp.equals("*") && options.equals("*")) {
				ls_object=eoDAO.findAll(sys);
			} else {
				ls_object=eoDAO.findAll(ar_opt, like, sys);
			}

			// Not managed
		} else {
			ls_object=new ArrayList<EntityObject>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObject entityObject : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObject(entityObject, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;
	}  

	private String getResultJsonObjectShort(String user, String sys, String subSys, String type, String status) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObjectShort> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		DAOImplObjectShort eoDAO = (DAOImplObjectShort) AmritaStartup.sqlFactory.getDAOObjectShort(conn, false, false, ucfg);

		//1 sys/*/type/status
		if (subSys.equals("*") && !type.equals("*") && !status.equals("*")) {
			ls_object=eoDAO.findAll(sys, EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);

		//2 sys/subSys/type/status
		} else if (!subSys.equals("*") && !type.equals("*") && !status.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys,  EnumObject.values()[Integer.parseInt(type.trim())],  EnumObjectStatus.values()[Integer.parseInt(status.trim())]);		
		//3 sys/*/*/status
		} else if (subSys.equals("*") && !status.equals("*")) {
			ls_object=eoDAO.findAll(sys, EnumObjectStatus.values()[Integer.parseInt(status.trim())]);
		
		//4 sys/subSys/type/*
		} else if (!subSys.equals("*") && !type.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())]);
			
	    // Not managed
		} else {
			ls_object=new ArrayList<EntityObjectShort>();
		}
		

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObjectShort entityObject : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObjectShort(entityObject, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;
	}  
	private String getResultJsonObjectOption(String user, String sys, String subSys, String type, String idObject, String option) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObjectOption> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOObjectOption eoDAO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false, false, ucfg);
		
		//1 sys/subSys/type/idObject
		if (!subSys.equals("*") && !type.equals("*") && !idObject.equals("*") && !type.equals("*")  && option.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(type.trim())], idObject);
			//2 sys/subSys/option
		} else if (!subSys.equals("*") && !type.equals("*") && idObject.equals("*") && type.equals("*")  && !option.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, EnumObjectOption.values()[Integer.parseInt(option.trim())]);	
			//3 sys/option
		} else if (!subSys.equals("*") && type.equals("*") && idObject.equals("*") && type.equals("*")  && !option.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, EnumObjectOption.values()[Integer.parseInt(option.trim())]);		 

			// Not managed
		} else {
			ls_object=new ArrayList<EntityObjectOption>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObjectOption entityObjectOption : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObjectOption(entityObjectOption, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;
	}


	private String getResultJsonRelation(String user, String sys, String subSys, String idObjectA, String typeA, String sourceA,
	        String relation, String idObjectB, String typeB, String sourceB) throws  JSONException, SQLException, ExceptionAmrita {

 		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityRelation> ls_object = null;
		List<EntitySqlGeneric> ls_generic = null;
		EnumObjectStatus statusObject = null;
		String subSysOwner = "";
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAORelation erDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false, false, ucfg);
 		IDAOSqlGeneric<EntitySqlGeneric> egDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		//1 sys/subSys/idObjectA/typeA/relation/*/*
		if (!subSys.equals("*") && !idObjectA.equals("*")  && !typeA.equals("*")&& !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdARel(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())]);
	//	//2 sys/subSys/*/*/*/relation/idObjectB/*/*     
		} else if (!subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*") && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdBRel(sys, subSys, idObjectB, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//3 sys/subSys/idObjectA/typeA/*/relation/idObjectB/typeB/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdAIdBRel(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], null,  idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())], null, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//4 sys/subSys/idObjectA/typeA/*/*/*/*/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdA(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], null);
		//5 sys/subSys/*/*/*/*/idObjectB/typeB/*
		} else if (!subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && relation.equals("*") && !idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdB(sys, subSys, idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())], null);
		//6 sys/subSys/idObjectA/typeA/*/*/idObjectB/typeB/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && !idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdAIdB(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], null, idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())], null);
		//7 sys/*/idObjectA/typeA/*/*/*/*/*
		} else if (subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByIdA(sys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())]);
	//	//8 sys/*/*/*/*/idObjectB/typeB/*
		} else if (subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && relation.equals("*") && !idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllByIdB(sys, idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())], null);
		//9 sys/subSys/idObjectA/typeObjectA/relation/idObjectB/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByIdAIdB(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())], idObjectB);
		//10 sys/subSys/*/*/*/relation/*/typeB/*
		} else if (!subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllByIdAIdB(sys, subSys, EnumRelation.values()[Integer.parseInt(relation.trim())], EnumObject.values()[Integer.parseInt(typeB.trim())]);
		//11 sys/subSys/idObjectA/typeObjectA/relation/*/typeB
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllByIdA(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())], EnumObject.values()[Integer.parseInt(typeB.trim())]);
	//	//12 sys/*/*/*/*/relation/*/*/*
		} else if (subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAll(sys, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//13 sys/*/*/*/*/relation/idObjectB/*/*     
		} else if (subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*") ) {
			ls_object=erDAO.findAllBySubSysIdBRel(sys, idObjectB, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//14 sys/*/idObjectA/typeA/*/*/idObjectB/typeB/*
		} else if (subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && !idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdAIdB(sys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], null, idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())], null);
		//15 sys/*/idObjectA/typeA/*/relation/*/*/*
		} else if (subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByIdARel(sys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//16 sys/*/idObjectA/*/*/relation/*/*/*
		} else if (subSys.equals("*") && !idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByIdARel(sys, idObjectA, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//17 sys/subSys/idObjectA/*/*/relation/*/*/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdA(sys, subSys, idObjectA, EnumRelation.values()[Integer.parseInt(relation.trim())]);
	//	//18 sys/subSys/*/*/*/relation/*/*/*
		} else if (!subSys.equals("*") && idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysRel(sys, subSys, EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//19 sys/*/*/typeA/*/relation/*/*/*
		} else if (subSys.equals("*") && idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByRel(sys, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//20 sys/subSys/*/typeA/*/relation/*/*/*
		} else if (!subSys.equals("*") && idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllByRel(sys, subSys, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())]);
		//21 sys/subSys/*/typeA/*/*/*/*/*
		} else if (!subSys.equals("*") && idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysTypeA(sys, subSys, EnumObject.values()[Integer.parseInt(typeA.trim())]);
		//22 sys/subSys/idObjectA/*/*/*/*/*/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && typeA.equals("*") && relation.equals("*") && idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdA(sys, subSys, idObjectA);
		//23 sys/subSys/idObjectA/*/*/relation/idObjectB/*/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*")  && typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdARelIdB(sys, subSys, idObjectA, EnumRelation.values()[Integer.parseInt(relation.trim())], idObjectB);
		//24 sys/subSys/idObjectA/typeA/*/*/*/typeB/*
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && relation.equals("*") && idObjectB.equals("*")  && !typeB.equals("*")) {
			ls_object=erDAO.findAllBySubSysIdATypeATypeB(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumObject.values()[Integer.parseInt(typeB.trim())]);


		// Not managed
		} else {
			ls_object=new ArrayList<EntityRelation>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityRelation er : ls_object) {
			jsonObject = new JSONObject();
			ls_generic = egDAO.sqlSubSysOwner(er.getSystem(), er.getSubSystem(), er.getTypeObjectB().ordinal(), er.getIdObjectB());
			// Object not analyzed/acquired/...
			// Force space subsystem to be different from the current subsystem
			subSysOwner = ls_generic.get(0).getSubSysOwner();
			statusObject = ls_generic.get(0).getStatusObject();
			if (statusObject == EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED 
			||  statusObject == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED	
			||  statusObject == EnumObjectStatus.SOURCE_MEMBER_ACQUIRED		
			||  statusObject == EnumObjectStatus.SOURCE_MEMBER_TYPE_DETECTED
			||  statusObject == EnumObjectStatus.SOURCE_MEMBER_TYPE_NOT_DETECTED) {
				subSysOwner = "";
			}
			populateJsonRelation(er, jsonObject, subSysOwner);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		egDAO.setConn(null);
		erDAO.setConn(null);

		return result;	}


	private String getResultJsonRelationOrigin(String user, String sys, String subSys, String idObjectA, String typeA, String relation, String idObjectB, String typeB) 
			throws  JSONException, SQLException, ExceptionAmrita {
		
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityRelationOrigin> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAORelationOrigin eoDAO = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false, false, ucfg);
		
		//1 sys/subSys/idObjectA/typeA/relation/idObjectB/typeB
		if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && !idObjectB.equals("*") && !typeB.equals("*")) {
			ls_object=eoDAO.findAllOrigin(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())], idObjectB, EnumObject.values()[Integer.parseInt(typeB.trim())]);
		//2 sys/subSys/idObjectA/typeA/relation/*/typeB
		} else if (!subSys.equals("*") && !idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*") && !typeB.equals("*")) {
			ls_object=eoDAO.findAllOrigin(sys, subSys, idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())], EnumObject.values()[Integer.parseInt(typeB.trim())]);
		//3 sys/subSys/*/typeB/relation/idObjectA/typeA
		} else if (!subSys.equals("*") && idObjectA.equals("*") && !typeA.equals("*") && !relation.equals("*") && idObjectB.equals("*") && !typeB.equals("*")) {
			ls_object=eoDAO.findAllOrigin(sys, subSys, EnumObject.values()[Integer.parseInt(typeB.trim())], EnumRelation.values()[Integer.parseInt(relation.trim())], idObjectA, EnumObject.values()[Integer.parseInt(typeA.trim())]);
		// Not managed
		} else {
			ls_object=new ArrayList<EntityRelationOrigin>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityRelationOrigin entityRelationOrigin : ls_object) {
			jsonObject = new JSONObject();
			populateJsonRelationOrigin(entityRelationOrigin, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	}


	private String getResultJsonCopyEntityDefinition(String user, String sys, String subSys, String idObject, String type) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityCopyEntityDefinition> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOCopyEntityDefinition eoDAO = (DAOImplCopyEntityDefinition) AmritaStartup.sqlFactory.getDAOCopyEntityDefinition(conn, false, false, ucfg);

		ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityCopyEntityDefinition copyEntityDefinition : ls_object) {
			jsonObject = new JSONObject();
			populateJsonCopyEntityDefinition(copyEntityDefinition, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}
	
	private String getResultJsonWhereUsed(String user, String sys, String subSys, String idObject, String type, String idField, String idObjectRefer) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityWhereUsedItem> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOWhereUsedItem eoDAO = (DAOImplWhereUsedItem) AmritaStartup.sqlFactory.getDAOWhereUsedItem(conn, false, false, ucfg);

		//X sys/subSys/idObject/type/idField/idObjectRefer

		//1 sys/subSys/idObject/type/idField/*
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*") && !idField.equals("*") && idObjectRefer.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField);
		//2 sys/*/idObject/type/idField/*
		} else if (subSys.equals("*") && !idObject.equals("*")  && !type.equals("*") && !idField.equals("*") && idObjectRefer.equals("*")) {
			ls_object=eoDAO.findAll(sys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField);
		//3 sys/subSys/idObject/type/*/*
		} else if (!subSys.equals("*") && !idObject.equals("*")  && !type.equals("*") && idField.equals("*") && idObjectRefer.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);
		//4 sys/*/idObject/type/idField/*
		} else if (subSys.equals("*") && !idObject.equals("*")  && !type.equals("*") && !idField.equals("*") && !idObjectRefer.equals("*")) {
			ls_object=eoDAO.findAll(sys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField, idObjectRefer);
		// Not managed
		} else {
			ls_object=new ArrayList<EntityWhereUsedItem>();
		}
		
		
		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityWhereUsedItem whereUsed : ls_object) {
			jsonObject = new JSONObject();
			populateJsonWhereUsed(whereUsed, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}

	
	private String getResultJsonDynamicField(String user, String sys, String subSys, String idObject, String type, String numInstr, String idField) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicField> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicField eoDAO = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false, false, ucfg);

		//1 sys/subSys/idObject/type/numInstr/idField/
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && !idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField);
		//2 sys/subSys/idObject/type/*/*/
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && numInstr.equals("*")  && idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);
		//3  sys/subSys/idObject/type/*/idField/
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && numInstr.equals("*")  && !idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField);
		//4  sys/subSys/idObject/type/numInstr/*
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], numInstr);				
		//5  sys/*/*/*/*/*
		} else if (subSys.equals("*") && idObject.equals("*") && type.equals("*")  && numInstr.equals("*")  && idField.equals("*")) {
		 	ls_object=eoDAO.findAll(sys);
		//6  sys/subSys/*/*/*/*
		} else if (!subSys.equals("*") && idObject.equals("*") && type.equals("*")  && numInstr.equals("*")  && idField.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys);
					
				
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicField>();
		}
				
		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicField dynamicField : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicField(dynamicField, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}	
	
	private String getResultJsonDynamicFieldSub(String user, String sys, String subSys, String idObject, String type, String numInstr, String idField) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicFieldSub> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicFieldSub eoDAO = (DAOImplDynamicFieldSub) AmritaStartup.sqlFactory.getDAODynamicFieldSub(conn, false, false, ucfg);

		//1 sys/subSys/idObject/type/numInstr/idField/
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && !idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField);
		//2 sys/subSys/idObject/type/*/*/
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && numInstr.equals("*")  && idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField);
		//3  sys/subSys/idObject/type/*/idField/
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && numInstr.equals("*")  && !idField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], idField);
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicFieldSub>();
		}
				
		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicFieldSub dynamicFieldSub : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicFieldSub(dynamicFieldSub, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	

	private String getResultJsonDynamicFieldSubSetting(String user, String sys, String subSys, String idObject, String type, String numInstr, String idField, String idSubField, String idPgmSet, String numChain) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicFieldSubSetting> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicFieldSubSetting eoDAO = (DAOImplDynamicFieldSubSetting) AmritaStartup.sqlFactory.getDAODynamicFieldSubSetting(conn, false, false, ucfg);
		
		//1 sys/subSys/idProgram/type/numInstr/idField/idSubField/*/*
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !idField.equals("*") && !idSubField.equals("*") && idPgmSet.equals("*") && numChain.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField, idSubField);
		//2 sys/subSys/idProgram/type/numInstr/idField/idSubField/idPgmSet/*
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !idField.equals("*") && !idSubField.equals("*") && !idPgmSet.equals("*") && numChain.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField, idSubField, idPgmSet);
		//3  sys/subSys/idProgram/type/numInstr/idField/idSubField/idPgmSet/numChain
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !idField.equals("*") && !idSubField.equals("*") && !idPgmSet.equals("*") && !numChain.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField, idSubField, idPgmSet, Integer.parseInt(numChain.trim()));
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicFieldSubSetting>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicFieldSubSetting dynamicFieldSubSetting : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicFieldSubSetting(dynamicFieldSubSetting, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	
	
	private String getResultJsonDynamicFieldSubValue(String user, String sys, String subSys, String idObject, String type, String numInstr, String idField, String idSubField) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicFieldSubValue> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicFieldSubValue eoDAO = (DAOImplDynamicFieldSubValue) AmritaStartup.sqlFactory.getDAODynamicFieldSubValue(conn, false, false, ucfg);

		//1 sys/subSys/idObject/type/numInstr/idField/idSubField
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && !idField.equals("*") && !idSubField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr.trim()), idField, idSubField);
		//2 sys/subSys/idObject/type/numInstr/idField/*/
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && !idField.equals("*") && idSubField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr), idField);
		//3  sys/subSys/idObject/type/*/*/*
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !numInstr.equals("*")  && idField.equals("*") && idSubField.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(numInstr));
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicFieldSubValue>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicFieldSubValue dynamicFieldSubValue : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicFieldSubValue(dynamicFieldSubValue, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	

	private String getResultJsonObjectAnalysisError(String user, String sys, String subSys, String idObject, String type, String rowNum) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObjectAnalysisError> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOObjectAnalysisError eoDAO = (DAOImplObjectAnalysisError) AmritaStartup.sqlFactory.getDAOObjectAnalysisError(conn, false, false, ucfg);

		//1 sys/subSys/idObject/type/* 
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && rowNum.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);
		//2 sys/subSys/idObject/type/rowNum
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*")  && !rowNum.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())], Integer.parseInt(rowNum));
		//3  sys/subSys/*/*/*
		} else if (!subSys.equals("*") && idObject.equals("*") && type.equals("*")  && rowNum.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys);
		//4  sys/*/*/*/*
		} else if (subSys.equals("*") && idObject.equals("*") && type.equals("*")  && rowNum.equals("*") ) {
		    ls_object=eoDAO.findAll(sys);
			
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityObjectAnalysisError>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObjectAnalysisError objectAnalysisError : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObjectAnalysisError(objectAnalysisError, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	

	private String getResultJsonObjectAnalysisInfo(String user, String sys, String subSys, String idObject, String type) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityObjectAnalysisInfo> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOObjectAnalysisInfo eoDAO = (DAOImplObjectAnalysisInfo) AmritaStartup.sqlFactory.getDAOObjectAnalysisInfo(conn, false, false, ucfg);

		//1 sys/subSys/idObject/type/* 
		if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);
		//2 sys/subSys/idObject/type/rowNum
		} else if (!subSys.equals("*") && !idObject.equals("*") && !type.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(type.trim())]);
		//3  sys/subSys/*/*
		} else if (!subSys.equals("*") && idObject.equals("*") && type.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys);
			
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityObjectAnalysisInfo>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityObjectAnalysisInfo objectAnalysisInfo : ls_object) {
			jsonObject = new JSONObject();
			populateJsonObjectAnalysisInfo(objectAnalysisInfo, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	

	private String getResultJsonMetricViolationConfig(String user, String sys, String subSys, String configName,  String typeViolation, String qualityFactor, String qualityCharacteristic) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityMetricViolationConfig> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

	
		Connection conn = DataBaseConnections.getConnection();
		IDAOMetricViolationConfig eoDAO = (DAOImplMetricViolationConfig) AmritaStartup.sqlFactory.getDAOMetricViolationConfig(conn, false, false, ucfg);
		
		//1 sys/subSys/configName/typeViolation/*/*
		if (!subSys.equals("*") && !configName.equals("*") && !typeViolation.equals("*") && qualityFactor.equals("*") && qualityCharacteristic.equals("*")   ) {
			ls_object=eoDAO.findViolation(sys, subSys, configName, EnumMetricsViolation.values()[Integer.parseInt(typeViolation.trim())] );		
		//2 sys/subSys/*/*/*/*
		} else if (!subSys.equals("*") && configName.equals("*") && typeViolation.equals("*") && qualityFactor.equals("*") && qualityCharacteristic.equals("*")  ) {
			ls_object=eoDAO.findAll(sys, subSys);			
		//3  sys/subSys/configName/*/*/*
		} else if (!subSys.equals("*") && !configName.equals("*") && typeViolation.equals("*") && qualityFactor.equals("*") && qualityCharacteristic.equals("*")   ) {
			ls_object=eoDAO.findAll(sys, subSys,  configName);			
		//4 sys/subSys/configName/*/qualityFactor/*
		} else if (!subSys.equals("*") && !configName.equals("*") && typeViolation.equals("*") && !qualityFactor.equals("*") && qualityCharacteristic.equals("*")   ) {
			ls_object=eoDAO.findAll(sys, subSys, configName, EnumMetricsQualityFactors.values()[Integer.parseInt(qualityFactor.trim())]);				
		//5  sys/subSys/configName/*/*/qualityCharacteristic
		} else if (!subSys.equals("*") && !configName.equals("*") && typeViolation.equals("*") && qualityFactor.equals("*") && !qualityCharacteristic.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, configName, EnumMetricsQualityCharacteristics.values()[Integer.parseInt(qualityCharacteristic.trim())]);
		//6  sys/subSys/configName/*/qualityFactor/qualityCharacteristic
		} else if (!subSys.equals("*") && !configName.equals("*") && typeViolation.equals("*") && !qualityFactor.equals("*") && !qualityCharacteristic.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, configName, EnumMetricsQualityFactors.values()[Integer.parseInt(qualityFactor.trim())], EnumMetricsQualityCharacteristics.values()[Integer.parseInt(qualityCharacteristic.trim())]);			
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityMetricViolationConfig>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityMetricViolationConfig metricViolationConfig : ls_object) {
			jsonObject = new JSONObject();
			populateJsonMetricViolationConfig(metricViolationConfig, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}		
	private String getResultJsonMetricViolation(String user, String sys, String subSys, String scope,  String idObject, String typeObject, String section, String typeViolation) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityMetricViolation> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

	
		Connection conn = DataBaseConnections.getConnection();
		IDAOMetricViolation eoDAO = (DAOImplMetricViolation) AmritaStartup.sqlFactory.getDAOMetricViolation(conn, false, false, ucfg);

		//1 sys/subSys/scope/idObject/typeObject/section/typeViolation
		if (!subSys.equals("*") && !scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*") && !section.equals("*") && !typeViolation.equals("*")   ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())], idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], section, EnumMetricsViolation.values()[Integer.parseInt(typeViolation.trim())] );
		//2 sys/subSys/scope/idObject/typeObject/*/*
		} else if (!subSys.equals("*") && !scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  ) {
		ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())], idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		//3  sys/subSys/scope/*/*/*/*
		} else if (!subSys.equals("*") && !scope.equals("*")  && idObject.equals("*") && typeObject.equals("*") && section.equals("*") && typeViolation.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys,  EnumMetricsScope.values()[Integer.parseInt(scope.trim())]);
		//4 sys/subSys/*/idObject/typeObject/*/*
		} else if (!subSys.equals("*") && scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*") && section.equals("*") && typeViolation.equals("*")  ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);			
		//5  sys/*/*/*/*/*/*
		} else if (!subSys.equals("*") && scope.equals("*") && idObject.equals("*") && typeObject.equals("*") && section.equals("*") && typeViolation.equals("*")  ) {
		    ls_object=eoDAO.findAll(sys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())]);
			
			
		// Not managed
		} else {
			ls_object=new ArrayList<EntityMetricViolation>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityMetricViolation metricViolation : ls_object) {
			jsonObject = new JSONObject();
			populateJsonMetricViolation(metricViolation, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return result;	
	}	

	private String getResultJsonMetricValue(String user, String sys, String subSys, String scope,  String idObject, String typeObject, String section) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityMetricValue> ls_object = null;
		String result = "";

		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOMetricValue eoDAO = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false, false, ucfg);

		//1 sys/subSys/scope/idObject/typeObject/section   SCOPE_LEVEL_SECTION
		if (!subSys.equals("*") && !scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*") && !section.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())], idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], section);
		//2 sys/subSys/scope/idObject/typeObject/*         SCOPE_LEVEL_OBJECT (pgm)
		} else if (!subSys.equals("*") && !scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  ) {
		    ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())], idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		//3  sys/subSys/scope/*/*/*               SCOPE_LEVEL_SUBSYSTEM
		} else if (!subSys.equals("*") && !scope.equals("*") && idObject.equals("*") && typeObject.equals("*") && section.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())]);
		//4  sys/subSys/scope/*/*/*                        SCOPE_LEVEL_SYSTEM
		} else if (!subSys.equals("*") && !scope.equals("*") && idObject.equals("*") && typeObject.equals("*") && section.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, EnumMetricsScope.values()[Integer.parseInt(scope.trim())]);
	    //5 sys/subSys/*/idObject/typeObject/*            All  program metrics
	    } else if (!subSys.equals("*") && scope.equals("*") && !idObject.equals("*") && !typeObject.equals("*") && section.equals("*")  ) {
		    ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		// Not managed
		} else {
			ls_object=new ArrayList<EntityMetricValue>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityMetricValue metricValue : ls_object) {
			jsonObject = new JSONObject();
			populateJsonMetricValue(metricValue, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}	

	private String getResultJsonDynamicFieldSubWaitExt(String user, String sys, String subSys, String idObject, String typeObject, String numInstr, String idField, String idSubField ) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicFieldSubWaitExt> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicFieldSubWaitExt eoDAO = (DAOImplDynamicFieldSubWaitExt) AmritaStartup.sqlFactory.getDAODynamicFieldSubWaitExt(conn, false, false, ucfg);

		//1 sys/*/*/*/*/*/*    
		if (subSys.equals("*") && idObject.equals("*") && typeObject.equals("*")  && numInstr.equals("*")  && idField.equals("*") && idSubField.equals("*")) {
			ls_object=eoDAO.findAll(sys);		
		//2 sys/subSys/*/*/*/*/*
		} else if (!subSys.equals("*") && idObject.equals("*") && typeObject.equals("*")  && numInstr.equals("*")  && idField.equals("*") && idSubField.equals("*") ) {
		    ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);			
	    //3 sys/subSys/idObject/typeObject/*/*/*
		} else if (!subSys.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  && numInstr.equals("*")  && idField.equals("*") && idSubField.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);		
	    //4 sys/subSys/idObject/typeObject/numInstr/*/*
		} else if (!subSys.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  && !numInstr.equals("*")  && idField.equals("*") && idSubField.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], Integer.parseInt(numInstr.trim()));			
		//5 sys/subSys/idObject/typeObject/numInstr/idField/idSubField
		} else if (!subSys.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  && !numInstr.equals("*")  && !idField.equals("*") && !idSubField.equals("*") ) {
			    ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())], Integer.parseInt(numInstr.trim()), idField, idSubField);
					    
	    // Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicFieldSubWaitExt>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicFieldSubWaitExt entityDynamicFieldSubWaitExt : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicFieldSubWaitExt(entityDynamicFieldSubWaitExt, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}

	private String getResultJsonDynamicValueExt(String user, String sys, String subSys, String idObjectExternal, String typeObjectExternal, String idFieldColumnExternal, String dsname, String typeSystemFieldExternal, String idSystemFieldExternal, String CicsNameExternal) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicValueExt> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
        // sys/subSys/idObjectExternal/typeObjectExternal/idFieldColumnExternal/dsname/typeSystemFieldExternal/idSystemFieldExternal/CicsNameExternal
		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicValueExt eoDAO = (DAOImplDynamicValueExt) AmritaStartup.sqlFactory.getDAODynamicValueExt(conn, false, false, ucfg);
		
		//1 sys/subSys/idObjectExternal/typeObjectExternal/*/*/*/*   
		if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && idFieldColumnExternal.equals("*")  && dsname.equals("*")  && typeSystemFieldExternal.equals("*") && idSystemFieldExternal.equals("*") && CicsNameExternal.equals("*")) {
			ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())]);		
		
		//2 sys/subSys/idObjectExternal/typeObjectExternal/idFieldColumnExternal/*/*/*/*
		} else if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && !idFieldColumnExternal.equals("*")  && dsname.equals("*")  && typeSystemFieldExternal.equals("*") && idSystemFieldExternal.equals("*") && CicsNameExternal.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, idFieldColumnExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())]);			
					    
		//2 sys/subSys/idObjectExternal/typeObjectExternal/*/dsname/*/*/*
		} else if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && idFieldColumnExternal.equals("*")  && !dsname.equals("*")  && typeSystemFieldExternal.equals("*") && idSystemFieldExternal.equals("*") && CicsNameExternal.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())], dsname);			
						    
		//3 sys/subSys/idObjectExternal/typeObjectExternal/*/dsname/*/*/CicsNameExternal
		} else if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && idFieldColumnExternal.equals("*")  && !dsname.equals("*")  && typeSystemFieldExternal.equals("*") && idSystemFieldExternal.equals("*") && !CicsNameExternal.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())], dsname, CicsNameExternal);			
							    
		//4 sys/subSys/idObjectExternal/typeObjectExternal/*/*/typeSystemFieldExternal/idSystemFieldExternal/*
		} else if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && idFieldColumnExternal.equals("*")  && dsname.equals("*")  && !typeSystemFieldExternal.equals("*") && !idSystemFieldExternal.equals("*") && CicsNameExternal.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())], EnumDataItemSystemEnvironment.values()[Integer.parseInt(typeSystemFieldExternal.trim())], idSystemFieldExternal);			
								    
		//5 sys/subSys/idObjectExternal/typeObjectExternal/*/*/typeSystemFieldExternal/idSystemFieldExternal/CicsNameExternal
		} else if (!subSys.equals("*") && !idObjectExternal.equals("*") && !typeObjectExternal.equals("*") && idFieldColumnExternal.equals("*")  && dsname.equals("*")  && !typeSystemFieldExternal.equals("*") && !idSystemFieldExternal.equals("*") && CicsNameExternal.equals("*")) {
		    ls_object=eoDAO.findAll(sys, subSys, idObjectExternal, EnumObject.values()[Integer.parseInt(typeObjectExternal.trim())], EnumDataItemSystemEnvironment.values()[Integer.parseInt(typeSystemFieldExternal.trim())], idSystemFieldExternal, CicsNameExternal);			
		    
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicValueExt>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicValueExt entityDynamicValueExt : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicValueExt(entityDynamicValueExt, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}
	
	private String getResultJsonDynamicCicsMapping(String user, String sys, String subSys, String typeObject, String externalName, String cicsName) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityDynamicCicsMapping> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}
         
		Connection conn = DataBaseConnections.getConnection();
		IDAODynamicCicsMapping eoDAO = (DAOImplDynamicCicsMapping) AmritaStartup.sqlFactory.getDAODynamicCicsMapping(conn, false, false, ucfg);
		
		//1 sys/*/*/*/* 
		if (!subSys.equals("*")  && !typeObject.equals("*") && !externalName.equals("*")  && !cicsName.equals("*")  ) {
			ls_object=eoDAO.findAll(sys);		
		
		//2 sys/subSys/*/*/* 
		} else if (!subSys.equals("*")  && typeObject.equals("*") && externalName.equals("*")  && cicsName.equals("*") ) {
			ls_object=eoDAO.findAllBySubSys(sys, subSys);		
		}
		
		 //3 sys/*/*/*/cicsName 
		else if (!subSys.equals("*")  && !typeObject.equals("*") && !externalName.equals("*")  && !cicsName.equals("*") ) {
			ls_object=eoDAO.findAllByCics(sys, cicsName);		
		}
		
		//4 sys/subSys/typeObject/externalName/cicsName 
		else if (!subSys.equals("*")  && !typeObject.equals("*") && !externalName.equals("*")  && cicsName.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(typeObject.trim())], externalName);		
		}
		    
		//5 sys/subSys/typeObject/externalName/* 
		else if (!subSys.equals("*")  && !typeObject.equals("*") && !externalName.equals("*")  && !cicsName.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, cicsName, EnumObject.values()[Integer.parseInt(typeObject.trim())], externalName);		
	 		    
		// Not managed
		} else {
			ls_object=new ArrayList<EntityDynamicCicsMapping>();
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityDynamicCicsMapping entityDynamicCicsMapping : ls_object) {
			jsonObject = new JSONObject();
			populateJsonDynamicCicsMapping(entityDynamicCicsMapping, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}
	
// user, sys, subSys, idObjectExternal, typeObjectExternal, dsname, typeSystemFieldExternal, idSystemFieldExternal, CicsNameExternal	
	private String getResultJsonMapDescriptor(String user, String sys, String subSys, String mapname, String typeObject) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityMapDescriptor> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOMapDescriptor eoDAO = (DAOImplMapDescriptor) AmritaStartup.sqlFactory.getDAOMapDescriptor(conn, false, false, ucfg);

		//1 sys/subSys/idObject/typeObject    
		if (!subSys.equals("*") && !mapname.equals("*") && !typeObject.equals("*")  ) {
		ls_object=eoDAO.findAll(sys, subSys, mapname, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		//2 sys/*/*/typeObject
		} else if (subSys.equals("*") && mapname.equals("*") && !typeObject.equals("*")  ) {
		    ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		//3 sys/subSys/*/typeObject
		} else if (!subSys.equals("*") && mapname.equals("*") && !typeObject.equals("*")  ) {
		    ls_object=eoDAO.findAll(sys, subSys, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		}

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityMapDescriptor entityMapDescriptor : ls_object) {
			jsonObject = new JSONObject();
			populateJsonMapDescriptor(entityMapDescriptor, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}

	private String getResultJsonMapItem(String user, String sys, String subSys, String idObject, String typeObject) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityMapItem> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOMapItem eoDAO = (DAOImplMapItem) AmritaStartup.sqlFactory.getDAOMapItem(conn, false, false, ucfg);

		//1 sys/subSys/idObject/typeObject    
		if (!subSys.equals("*") && !idObject.equals("*") && !typeObject.equals("*")  ) {
			ls_object=eoDAO.findAll(sys, subSys, idObject, EnumObject.values()[Integer.parseInt(typeObject.trim())]);
		} 

		// No data
		jsonArray = new JSONArray();

		// Populate Json
		for (EntityMapItem entityMapItem : ls_object) {
			jsonObject = new JSONObject();
			populateJsonMapItem(entityMapItem, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}
	
	private String getResultJsonImpactPlan(String user, String sys, String subSys, String idPlan, String numOp, String typeObjectOrigin, String idObjectOrigin, String typeImpactChange) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityImpactPlan> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactPlan eoDAO = (DAOImplImpactPlan) AmritaStartup.sqlFactory.getDAOImpactPlan(conn, false, false, ucfg);

		//  sys/subSys/idPlan/numOp/idObjectOrigin/typeObjectOrigin/typeImpactChange   
		
		//1 sys/*/idPlan/*/*/*/*  
		if (subSys.equals("*") && !idPlan.equals("*") && numOp.equals("*") && idObjectOrigin.equals("*") && typeObjectOrigin.equals("*") && typeImpactChange.equals("*")   ) {
			ls_object=eoDAO.findAll(sys, idPlan);			
		
		//2 sys/*/idPlan/numOp/*/*/* 
		} else if (subSys.equals("*") && !idPlan.equals("*") && !numOp.equals("*") && idObjectOrigin.equals("*") && typeObjectOrigin.equals("*") && typeImpactChange.equals("*") ) {
			ls_object=eoDAO.findAll(sys, idObjectOrigin, Integer.parseInt(numOp));		
			
		//3 sys/*/*/*/idObjectOrigin/typeObjectOrigin/* 	
		} else if (subSys.equals("*") && idPlan.equals("*") && numOp.equals("*") && !idObjectOrigin.equals("*") && !typeObjectOrigin.equals("*") && typeImpactChange.equals("*") ) {		
			ls_object=eoDAO.findAll(sys, idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())]);		
			
		//4 sys/subSys/*/*/*/*/typeImpactChange 	
		} else if (!subSys.equals("*") && idPlan.equals("*") && numOp.equals("*") && idObjectOrigin.equals("*") && typeObjectOrigin.equals("*") && !typeImpactChange.equals("*") ) {
			ls_object=eoDAO.findAll(sys, subSys, EnumTypeImpactChange.values()[Integer.parseInt(typeImpactChange.trim())]);		
		
		//5 sys/*/*/*/idObjectOrigin/typeObjectOrigin/typeImpactChange
		} else if (subSys.equals("*") && idPlan.equals("*") && numOp.equals("*") && !idObjectOrigin.equals("*") && !typeObjectOrigin.equals("*") && !typeImpactChange.equals("*") ) {
			ls_object=eoDAO.findAll(sys, idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], EnumTypeImpactChange.values()[Integer.parseInt(typeImpactChange.trim())]);		
		
		// No data 	
		} else {
			ls_object = new ArrayList<EntityImpactPlan>	();
		} 

		jsonArray = new JSONArray();
		
		// Populate Json
		for (EntityImpactPlan entityImpactPlan : ls_object) {
			jsonObject = new JSONObject();
			populateJsonImpactPlan(entityImpactPlan, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}	
	
	private String getResultJsonImpactObject(String user, String sys, String idPlan, String numOp, String typeObjectOrigin, String idObjectOrigin, String typeObjectTarget,  String idObjectTarget) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityImpactObject> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactObject eoDAO = (DAOImplImpactObject) AmritaStartup.sqlFactory.getDAOImpactObject(conn, false, false, ucfg);

		//1 sys/idPlan/numOp/typeObjectOrigin/idObjectOrigin/idObjectTarget/typeObjectTarget   
		if (!idPlan.equals("*") && !numOp.equals("*") && !typeObjectOrigin.equals("*") && idObjectOrigin.equals("*") && !typeObjectTarget.equals("*") && !idObjectTarget.equals("*")    ) {
			ls_object=eoDAO.findAll(sys, idPlan, Integer.parseInt(numOp), EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectTarget.trim())], idObjectTarget );			
		
		//2 sys/idPlan/*/typeObjectOrigin/idObjectOrigin/idObjectTarget/typeObjectTarget  
		} else if (!idPlan.equals("*") && numOp.equals("*") && !typeObjectOrigin.equals("*") && idObjectOrigin.equals("*") && !typeObjectTarget.equals("*") && !idObjectTarget.equals("*")    ) {
			ls_object=eoDAO.findAll(sys, idPlan, EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], idObjectOrigin, EnumObject.values()[Integer.parseInt(typeObjectTarget.trim())], idObjectTarget );			
		
		//3 sys/idPlan/numOp/typeObjectOrigin/idObjectOrigin/*/*  
		} else if (!idPlan.equals("*") && !numOp.equals("*") && !typeObjectOrigin.equals("*") && idObjectOrigin.equals("*") && typeObjectTarget.equals("*") && idObjectTarget.equals("*")    ) {
			ls_object=eoDAO.findAll(sys, idPlan, Integer.parseInt(numOp), EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], idObjectOrigin);			
			
		//4 sys/idPlan/*/typeObjectOrigin/idObjectOrigin/*/*  
		} else if (!idPlan.equals("*") && numOp.equals("*") && !typeObjectOrigin.equals("*") && idObjectOrigin.equals("*") && typeObjectTarget.equals("*") && idObjectTarget.equals("*")    ) {
			ls_object=eoDAO.findAll(sys, idPlan, EnumObject.values()[Integer.parseInt(typeObjectOrigin.trim())], idObjectOrigin);			
				
		//5 sys/idPlan/*/*/*/*/*  
		} else if (!idPlan.equals("*") && numOp.equals("*") && typeObjectOrigin.equals("*") && idObjectOrigin.equals("*") && typeObjectTarget.equals("*") && idObjectTarget.equals("*")    ) {
			ls_object=eoDAO.findAll(sys, idPlan);			
								
		// No data 	
		} else {
			ls_object = new ArrayList<EntityImpactObject>();
		} 

		// No data
		jsonArray = new JSONArray();
		
		// Populate Json
		for (EntityImpactObject entityImpactObject : ls_object) {
			jsonObject = new JSONObject();
			populateJsonImpactObject(entityImpactObject, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}	

	private String getResultJsonImpactPgmTpRecompile(String user, String sys, String idPlan) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityImpactCompile> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOImpactCompile eoDAO = (DAOImplImpactCompile) AmritaStartup.sqlFactory.getDAOImpactCompile(conn, false, false, ucfg);

		//1 sys/idPlan
		if (!idPlan.equals("*")  ) {
			ls_object=eoDAO.findAll(sys, idPlan);			
						
		// No data 	
		} else {
			ls_object = new ArrayList<EntityImpactCompile>();
		} 

		jsonArray = new JSONArray();

		// Populate Json
		for (EntityImpactCompile entityImpactCompile : ls_object) {
			jsonObject = new JSONObject();
			populateJsonImpactCompile(entityImpactCompile, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return result;	
	}		
	private String getResultJsonProcessLog(String user, String sys, String idObject, String typeObject, String dtStartProcess, String tmStartProcess) throws  JSONException, SQLException, ExceptionAmrita {
		UserConfiguration ucfg = null;
		JSONObject jsonObject = null; 
		JSONArray jsonArray = null; 		
		List<EntityProcessLog> ls_object = null;
		String result = "";
		
		// Login should be done
		// If not take default properties
		ucfg = UserActive.getUser(user);
		if (ucfg == null) {
		    ucfg = new UserConfiguration(user);
		}

		Connection conn = DataBaseConnections.getConnection();
		IDAOProcessLog eoDAO = (DAOImplProcessLog) AmritaStartup.sqlFactory.getDAOProcessLog(conn, false, false, ucfg);

		//1 user/sys/dtStartProcess/*/*/*            
		if (!user.equals("*") && !dtStartProcess.equals("*")  && tmStartProcess.equals("*") && idObject.equals("*") && typeObject.equals("*")  ) {
			ls_object=eoDAO.findAllByUser(user, sys, dtStartProcess);
		//2 user/sys/dtStartProcess/tmStartProcess/*/*            
		} else if (!user.equals("*") && !dtStartProcess.equals("*") && !tmStartProcess.equals("*") && idObject.equals("*") && typeObject.equals("*")  ) {
			ls_object=eoDAO.findAllByUserDateTime(user, sys, dtStartProcess, tmStartProcess);
		//3 */sys/dtStartProcess/"/*/* 
		} else if (!user.equals("*") && !dtStartProcess.equals("*")  && !tmStartProcess.equals("*") && idObject.equals("*") && typeObject.equals("*") ) {
		    ls_object=eoDAO.findAllByDate(sys, dtStartProcess);
		//4 */sys/dtStartProcess/*/idObject/* 
		} else if (!user.equals("*") && !dtStartProcess.equals("*") && !tmStartProcess.equals("*") && idObject.equals("*") && typeObject.equals("*")  ) {
			 ls_object=eoDAO.findAllByIdObject(sys, idObject, dtStartProcess);
		//5 */sys/dtStartProcess/*/*/typeProcess 
		} else if (!user.equals("*") && !dtStartProcess.equals("*")  && !tmStartProcess.equals("*") && idObject.equals("*") && typeObject.equals("*")  ) {
			 ls_object=eoDAO.findAllByTypeProcess(sys, EnumDirectivesExecution.values()[Integer.parseInt(typeObject.trim())], dtStartProcess);

	    // Not managed
		} else {
			ls_object=new ArrayList<EntityProcessLog>();
		}

		jsonArray = new JSONArray();

		// Populate Json
		for (EntityProcessLog entityProcessLog : ls_object) {
			jsonObject = new JSONObject();
			populateJsonEntityprocessLog(entityProcessLog, jsonObject);
			jsonArray.put(jsonObject);
		}

		// Return json object
		result = jsonArray.toString();		
        
	    DataBaseConnections.releaseConnection(conn);
	    eoDAO.setConn(null);
	    
		return result;	
	}	
	
	private void populateJsonUser(EntityUser eo, JSONObject jsonObject) {
		// User Data
		jsonObject.put("baseUrl", eo.getBaseUrl());                           
		jsonObject.put("companyCode", eo.getCompanyCode()); 							 
		jsonObject.put("company", eo.getCompany()); 									 
		jsonObject.put("userType", eo.getUserType());            			 
		jsonObject.put("userStatus", eo.getUserStatus());            	  
		jsonObject.put("language", eo.getLanguage());            					 
		jsonObject.put("pwd", eo.getPwd());          								 
		jsonObject.put("country", eo.getCountry()); 									 
		jsonObject.put("mail", eo.getMail()); 											 
		jsonObject.put("mailInfo", eo.getMailInfo()); 									 
		jsonObject.put("phone", eo.getPhone()); 										  
		jsonObject.put("referManager", eo.getReferManager()); 							  
		jsonObject.put("referTech", eo.getReferTech()); 								 
		jsonObject.put("analyzerEnabled", eo.getAnalyzerEnabled()); 					  
		jsonObject.put("viewerEnabled", eo.getViewerEnabled()); 						 
		jsonObject.put("inspectorEnabled", eo.getInspectorEnabled()); 					 
		jsonObject.put("assesmentEnabled", eo.getAssesmentEnabled()); 					  
		jsonObject.put("countLogin", eo.getCountLogin()); 									    			
		jsonObject.put("dtActivation", eo.getDtActivation());  							 
		jsonObject.put("tmActivation", eo.getTmActivation());  						     
		jsonObject.put("dtExpiration", eo.getDtExpiration());  						 
		jsonObject.put("tmExpiration", eo.getTmExpiration());  							 
		jsonObject.put("dtFirstLogin", eo.getDtFirstLogin());  							  
		jsonObject.put("tmFirstLogin", eo.getTmFirstLogin());  							 
		jsonObject.put("dtLastLogin", eo.getDtLastLogin()); 							 
		jsonObject.put("tmLastLogin", eo.getTmLastLogin()); 							 

	    jsonObject.put("pathConfigFile", eo.getPathConfigFile());                        
	    jsonObject.put("pathRoot", eo.getPathRoot());                                	 
	    jsonObject.put("pathUser", eo.getPathUser());                                	 
	    jsonObject.put("pathPilot", eo.getPathPilot());                               	  
	 	
		// Directories relative a root o WEB-INF da file di configurazione generale
		jsonObject.put("dirResources", eo.getDirResources());                            
		jsonObject.put("dirWork", eo.getDirWork());                                 	 
		jsonObject.put("dirDatabase", eo.getDirDatabase());                                             
		jsonObject.put("dirJclInput", eo.getDirJclInput());                                   
		jsonObject.put("dirCobolSrcPgmInput", eo.getDirCobolSrcPgmInput());               
		jsonObject.put("dirCobolSrcCopyInput", eo.getDirCobolSrcCopyInput());             
		jsonObject.put("dirCobolPgm", eo.getDirCobolPgm());                               		 
		jsonObject.put("dirCobolCopy", eo.getDirCobolCopy());                             
		jsonObject.put("dirJcl", eo.getDirJcl());                                  		  
		jsonObject.put("dirSqlScript", eo.getDirSqlScript());                             	 
		jsonObject.put("dirCobolGraph", eo.getDirCobolGraph());                           
		jsonObject.put("dirPilot", eo.getDirPilot());                                	 
		jsonObject.put("dirLog", eo.getDirLog());                                  		 
		jsonObject.put("dirOutput", eo.getDirOutput());                              	 

		// Ottimizzazione processi ed elaborazioni, allocazione di arrays, collections, map
		jsonObject.put("limitMaxLinesScanFindingSourceType", eo.getLimitMaxLinesScanFindingSourceType());  
		jsonObject.put("limitMaxSources", eo.getLimitMaxSources());                    
		jsonObject.put("limitMaxSourcesInput", eo.getLimitMaxSourcesInput());                
		jsonObject.put("limitMaxSourcesToProcess", eo.getLimitMaxSourcesToProcess());        
		jsonObject.put("limitMaxObjects", eo.getLimitMaxObjects());                    
		jsonObject.put("limitMaxObjectsInput", eo.getLimitMaxObjectsInput());                
		jsonObject.put("limitMaxObjectsToProcess", eo.getLimitMaxObjectsToProcess());        
		jsonObject.put("debugThresholdMemoryGarbage", eo.getDebugThresholdMemoryGarbage()); 
		jsonObject.put("debugSourcesDetectedFreqGarbage", eo.getDebugSourcesDetectedFreqGarbage());  
		jsonObject.put("debugActive", eo.getDebugActive());                             
		jsonObject.put("logVerbose", eo.getLogVerbose());                               
		jsonObject.put("preferredVisitMethod", eo.getPreferredVisitMethod());            
		jsonObject.put("preferredCachingLevel", eo.getPreferredCachingLevel());         
		jsonObject.put("preferredCachingSupport", eo.getPreferredCachingSupport());      
		
		//  Database
		jsonObject.put("dataBaseType", eo.getDataBaseType());                             
		jsonObject.put("dataBaseName", eo.getDataBaseName());                             
		jsonObject.put("dataBaseUser", eo.getDataBaseUser());                             
		jsonObject.put("dataBasePwd", eo.getDataBasePwd());                              
		jsonObject.put("dataBaseDriver", eo.getDataBaseDriver());                        
		jsonObject.put("dataBaseAccessType", eo.getDataBaseAccessType());                
		jsonObject.put("dataBaseUrl", eo.getDataBaseUrl());                              
		jsonObject.put("dataBaseMaxConn", eo.getDataBaseMaxConn());                          
		jsonObject.put("dataBaseCommitBlockUpdates", eo.getDataBaseCommitBlockUpdates());    
		jsonObject.put("dataBaseLogAnySql", eo.getDataBaseLogAnySql());                 
	    	
		// Controllo analisi, identificazione oggetti analizzati/processati, piloti sources, filtri e processi'
		jsonObject.put("pilotDefaultSource", eo.getPilotDefaultSource());                 
		jsonObject.put("pilotDefaultProcess", eo.getPilotDefaultProcess());               
		jsonObject.put("userExitClass", eo.getUserExitClass());                           
				
		jsonObject.put("userTypetOrdinal", eo.getUserType().ordinal());
		jsonObject.put("userStatusOrdinal", eo.getUserStatus().ordinal());
	}	


	private void populateJsonObject(EntityObject eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("typeSourceOrdinal", eo.getTypeSource().ordinal());
		jsonObject.put("idObjectExtended", eo.getIdObjectExtended());
		jsonObject.put("librarySourceObject", eo.getLibrarySourceObject());
		jsonObject.put("librarySource", eo.getLibrarySource());
		jsonObject.put("fileSource", eo.getFileSource());
		jsonObject.put("suffixFileSource", eo.getSuffixFileSource());
		jsonObject.put("libraryDir", eo.getLibraryDir());
		jsonObject.put("status", eo.getStatus());
		jsonObject.put("author", eo.getAuthor());
		jsonObject.put("idObjectDescriptor", eo.getIdObjectDescriptor());
		jsonObject.put("typeObjectDescriptor", eo.getTypeObjectDescriptor());
		jsonObject.put("systemOwner", eo.getSystemOwner());
		jsonObject.put("subSystemOwner", eo.getSubSystemOwner());
		jsonObject.put("dateWritten", eo.getDateWritten());
		jsonObject.put("pathDocFile", eo.getPathDocFile());
		jsonObject.put("dtFirstAnalysis", eo.getDtFirstAnalysis());
		jsonObject.put("tmFirstAnalysis", eo.getTmFirstAnalysis());
		jsonObject.put("dtLastAnalysis", eo.getDtLastAnalysis());
		jsonObject.put("tmLastAnalysis", eo.getTmLastAnalysis());
		jsonObject.put("typeSource", eo.getTypeSource());
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("statusOrdinal", eo.getStatus().ordinal());
	}

	private void populateJsonObjectShort(EntityObjectShort eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("subSystemOwner", eo.getSubSystemOwner());
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("typeSourceOrdinal", eo.getTypeSource().ordinal());
		jsonObject.put("typeSource", eo.getTypeSource());
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("statusOrdinal", eo.getStatus().ordinal());
	}
	

	private void populateJsonObjectOption(EntityObjectOption eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("option", eo.getOption());
		jsonObject.put("textValue", eo.getTypeSource());
	}

	private void populateJsonRelation(EntityRelation e, JSONObject jsonObject, String subSysOwner) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("subSysOwner", subSysOwner);
		jsonObject.put("idObjectA", e.getIdObjectA());
		jsonObject.put("typeObjectA", e.getTypeObjectA());
		jsonObject.put("typeSourceA", e.getTypeSourceA());
		jsonObject.put("relation", e.getRelation());
		jsonObject.put("idObjectB", e.getIdObjectB());
		jsonObject.put("typeObjectB", e.getTypeObjectB());
		jsonObject.put("typeSourceB", e.getTypeSourceB());
		jsonObject.put("typeObjectAOrdinal", e.getTypeObjectA().ordinal());
		jsonObject.put("typeSourceAOrdinal", e.getTypeSourceA().ordinal());
		jsonObject.put("relationOrdinal", e.getRelation().ordinal());
		jsonObject.put("typeObjectBOrdinal", e.getTypeObjectB().ordinal());
		jsonObject.put("typeSourceBOrdinal", e.getTypeSourceB().ordinal());
	}	

	private void populateJsonRelationOrigin(EntityRelationOrigin e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObjectA", e.getIdObjectRelA());  
		jsonObject.put("typeObjectA", e.getTypeObjectA());
		jsonObject.put("typeSourceA", e.getTypeSourceA());
		jsonObject.put("relation", e.getRelation());
		jsonObject.put("idObjectB", e.getIdObjectRelB());
		jsonObject.put("typeObjectB", e.getTypeObjectB());
		jsonObject.put("typeSourceB", e.getTypeSourceB());
		jsonObject.put("numInstrOrigin", e.getNumInstrOrigin());	
		jsonObject.put("rowStartOrigin", e.getRowStart());	
		jsonObject.put("rowEndOrigin", e.getRowEnd());	
		jsonObject.put("copyOrigin", e.getCopyOrigin());	
		jsonObject.put("rowStartInCopy", e.getRowStartInCopy());	
		jsonObject.put("rowEndInCopy", e.getRowEndInCopy());	
		
		// Data    
		jsonObject.put("relationType", e.getRelationType());
		jsonObject.put("instrProgramArea", e.getInstrProgramArea());
		jsonObject.put("instrCategory", e.getInstrCategory());
		jsonObject.put("relationSource", e.getRelationSource());
		jsonObject.put("instrLang", e.getInstrLang());
		jsonObject.put("instrTypePrecompiler", e.getInstrTypePrecompiler());
		jsonObject.put("idObjectOrigin", e.getIdObjectOrigin());
		jsonObject.put("typeObjectOrigin", e.getTypeObjectOrigin());
		jsonObject.put("rowStart", e.getRowStart());
		jsonObject.put("rowEnd", e.getRowEnd());
		jsonObject.put("rowStartInCopy", e.getRowStartInCopy());
		jsonObject.put("rowEndInCopy", e.getRowEndInCopy());
		jsonObject.put("copyOrigin", e.getCopyOrigin());
		
		// ** Informazioni valide solo per alcune relazioni **
		jsonObject.put("librarySourceObject", e.getLibrarySourceObject());		
		jsonObject.put("librarySourcePath", e.getLibrarySourcePath());
		jsonObject.put("fileSource", e.getFileSource());
		jsonObject.put("typeObjectCross", e.getTypeObjectCross());
		jsonObject.put("idObjectCross", e.getIdObjectCross());
		jsonObject.put("info1Cross", e.getInfo1Cross());
		jsonObject.put("info2Cross", e.getInfo2Cross());
		jsonObject.put("info3Cross", e.getInfo3Cross());
		
		jsonObject.put("typeObjectAOrdinal", e.getTypeObjectA().ordinal());
		jsonObject.put("typeSourceAOrdinal", e.getTypeSourceA().ordinal());
		jsonObject.put("relationOrdinal", e.getRelation().ordinal());
		jsonObject.put("typeObjectBOrdinal", e.getTypeObjectB().ordinal());
		jsonObject.put("typeSourceBOrdinal", e.getTypeSourceB().ordinal());
		
		jsonObject.put("relationTypeOrdinal", e.getRelationType().ordinal());
		jsonObject.put("instrProgramAreaOrdinal", e.getInstrProgramArea().ordinal());
		jsonObject.put("instrCategoryOrdinal", e.getInstrCategory().ordinal());
		jsonObject.put("relationSourceOrdinal", e.getRelationSource().ordinal());
		jsonObject.put("instrLangOrdinal", e.getInstrLang().ordinal());
		jsonObject.put("instrTypePrecompilerOrdinal", e.getInstrTypePrecompiler().ordinal());
		jsonObject.put("typeObjectOriginOrdinal", e.getTypeObjectOrigin().ordinal());
		jsonObject.put("typeObjectCrossOrdinal", e.getTypeObjectCross().ordinal());
	}	

	private void populateJsonCopyEntityDefinition(EntityCopyEntityDefinition e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numSeq", e.getNumSeq());
		jsonObject.put("idField", e.getIdField());

		jsonObject.put("level", e.getLevel());
		jsonObject.put("occurs", e.getOccurs());
		jsonObject.put("lngBytes", e.getLngBytes());
		jsonObject.put("pos", e.getPos());
		jsonObject.put("numInt", e.getNumInt());
		jsonObject.put("numDec", e.getNumDec());
		jsonObject.put("signed", e.getSigned());
		jsonObject.put("groupField", e.getGroupField());
		jsonObject.put("itemLang", e.getItemLang());
		jsonObject.put("itemType", e.getItemType());

		jsonObject.put("numDigit", e.getNumDigit());
		jsonObject.put("scale", e.getScale());
		jsonObject.put("notNull", e.getNotNull());
		jsonObject.put("withDefault", e.getWithDefault());
		jsonObject.put("defaultValue", e.getDefaultValue());

		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("itemLangOrdinal", e.getItemLang().ordinal());
		jsonObject.put("itemTypeOrdinal", e.getItemType().ordinal());
	}	


	private void populateJsonWhereUsed(EntityWhereUsedItem e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numSeq", e.getNumSeq());
		jsonObject.put("idField", e.getIdField());
		jsonObject.put("idObjectRefer", e.getIdObjectRefer());
		jsonObject.put("typeObjectRefer", e.getTypeObjectRefer());
		jsonObject.put("numInstrRefer", e.getNumInstrRefer());

		jsonObject.put("idAliasLocal", e.getIdAliasLocal());
		jsonObject.put("idIoarea", e.getIdIoarea());
		jsonObject.put("idObjectCopy", e.getIdObjectCopy());
		jsonObject.put("typeWhereUsed", e.getTypeWhereUsed());
		jsonObject.put("typeAlias", e.getTypeAlias());
		jsonObject.put("posInIoarea", e.getPosInIoarea());
		jsonObject.put("usedBytes", e.getUsedBytes());
		jsonObject.put("numInt", e.getNumInt());
		jsonObject.put("numDec", e.getNumDec());
		jsonObject.put("signed", e.getSigned());
		jsonObject.put("instrLang", e.getInstrLang());
		jsonObject.put("instrType", e.getInstrType());
		jsonObject.put("rowStart", e.getRowStart());
		jsonObject.put("rowEnd", e.getRowEnd());
		jsonObject.put("idObjectCopy", e.getIdObjectCopy());
		jsonObject.put("rowStartInCopy", e.getRowStartInCopy());
		jsonObject.put("rowEndInCopy", e.getRowEndInCopy());
		
		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeObjectReferOrdinal", e.getTypeObjectRefer().ordinal());
		jsonObject.put("typeWhereUsedOrdinal", e.getTypeWhereUsed().ordinal());
		jsonObject.put("instrLangOrdinal", e.getInstrLang().ordinal());
		jsonObject.put("instrTypeOrdinal", e.getInstrType().ordinal());
	}	
	
	private void populateJsonDynamicField(EntityDynamicField e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numInstr", e.getNumInstr());
		jsonObject.put("idField", e.getIdField());
		
		jsonObject.put("numField", e.getNumField());
		jsonObject.put("instrCobolType", e.getInstrCobolType());
		jsonObject.put("instrPrecompType", e.getInstrPrecompType());
		jsonObject.put("instrPrecompOprndType", e.getInstrPrecompOprndType());
		jsonObject.put("light", e.getLight());
		jsonObject.put("solved", e.getSolved());
		jsonObject.put("solvedFull", e.getSolvedFull());
		jsonObject.put("waitingForData", e.getWaitingForData());
		jsonObject.put("spreaded", e.getSpreaded());
		
		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("instrCobolTypeOrdinal", e.getInstrCobolType().ordinal());
		jsonObject.put("instrPrecompTypeOrdinal", e.getInstrPrecompType().ordinal());
		jsonObject.put("instrPrecompOprndTypeOrdinal", e.getInstrPrecompOprndType().ordinal());
	}	
	
	private void populateJsonDynamicFieldSub(EntityDynamicFieldSub e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numInstr", e.getNumInstr());
		jsonObject.put("idField", e.getIdField());		
		jsonObject.put("idSubField", e.getIdSubField());
		
		jsonObject.put("numField", e.getNumField());
		jsonObject.put("numSubField", e.getNumSubField());
		jsonObject.put("sizeSubField", e.getSizeSubField());
		jsonObject.put("typeSubField", e.getTypeSubField());	
		
		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeSubFieldOrdinal", e.getTypeSubField().ordinal());
	}

	private void populateJsonDynamicFieldSubSetting(EntityDynamicFieldSubSetting e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numInstr", e.getNumInstr());
		jsonObject.put("idField", e.getIdField());		
		jsonObject.put("idSubField", e.getIdSubField());
		jsonObject.put("idPgmSet", e.getIdPgmSet());
		jsonObject.put("numChain", e.getNumChain());
		jsonObject.put("progr", e.getProgr());
		jsonObject.put("numInstrSet", e.getNumInstrSet());
		
		jsonObject.put("numField", e.getNumField());
		jsonObject.put("numSubField", e.getNumSubField());
		// Porzione di sottocampo oggetto dell'impostazione 
		jsonObject.put("posInSubField", e.getPosInSubField());
		jsonObject.put("lngInSubField", e.getLngInSubField());
		// Informazioni di impostazione, programma, path e modalità di assegnazione e valore se disponibile (literal, campo di tabella)
		jsonObject.put("numInstrOriginSpreaded", e.getNumInstrOriginSpreaded());
		jsonObject.put("typeObjectPgmSet", e.getTypeObjectPgmSet());
		jsonObject.put("numPath", e.getNumPath());
		jsonObject.put("setMode", e.getSetMode());
		jsonObject.put("numUsingParm", e.getNumUsingParm());
		jsonObject.put("dspFieldInUsingParm", e.getDspFieldInUsingParm());
		jsonObject.put("dspFieldInLinkageArea", e.getDspFieldInLinkageArea());
		jsonObject.put("typePointerArea", e.getTypePointerArea());
		jsonObject.put("dspPointerInLinkageArea", e.getDspPointerInLinkageArea());
		jsonObject.put("numUsingParmPointer", e.getNumUsingParmPointer());
		jsonObject.put("dspPointerInUsingParm", e.getDspPointerInUsingParm());
		jsonObject.put("value", e.getValue());
		// Campo input in assegnazione di trasformazione.
		// Posizione e lunghezza per il sender sono quelli espressi da reference modification (posSnd:lngSnd) se indicato.
	    // Posizione e lunghezza sono sempre valorizzati e se non presenti sono inizializzati (1:size(campo sender))
		jsonObject.put("fieldSenderId", e.getFieldSenderId());
		jsonObject.put("fieldSenderNum", e.getFieldSenderNum());
		jsonObject.put("fieldSenderPos", e.getFieldSenderPos());
		jsonObject.put("fieldSenderLng", e.getFieldSenderLng());
	    // Campo risultato in assegnazione di trasformazione o campo receiver senza trasformazioni. 
	    // La posizione è quella iniziale interessata alla trasformazione.
	    // La lunghezza è quella del sottocampo origine di cui trovare i valori.
	    // Posizione e lunghezza sono sempre valorizzati ed inizializzati nel processo a 1, size(campo receiver)
	    // Se l'istruzione Move che ha generato l'assegnazione contiene anche reference modification (pos:lng), l'informazione
	    // è utilizzata solo per determinare se il receiver è influenzato dalla trasformazione, ma NON viene memorizzata.
		jsonObject.put("fieldReceiverId", e.getFieldReceiverId());
		jsonObject.put("fieldReceiverNum", e.getFieldReceiverNum());
		jsonObject.put("fieldReceiverPos", e.getFieldReceiverPos());
		jsonObject.put("fieldReceiverLng", e.getFieldReceiverLng());
		// Oggetto alla cui ioarea appartiene il campo ultima trasformazione, di cui trovare i valori esternamente
	    // (prima assegnazione nella catena)
		jsonObject.put("idObjExt", e.getIdObjExt());
		jsonObject.put("typeObjExt", e.getTypeObjExt());
		jsonObject.put("objExtSystem", e.getObjExtSystem());
		jsonObject.put("objExtColField", e.getObjExtColField());
		jsonObject.put("objExtIdCopy", e.getObjExtIdCopy());
		jsonObject.put("objExtPosCol", e.getObjExtPosCol());
		jsonObject.put("objExtLengthCol", e.getObjExtLengthCol());
		// Indicatori di soluzione e di valori disponibili
		jsonObject.put("solvedObjExt", e.getSolvedObjExt());
		jsonObject.put("waitingForExternalData", e.getWaitingForExternalData());

		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeObjectPgmSetOrdinal", e.getTypeObjectPgmSet().ordinal());
		jsonObject.put("setModeOrdinal", e.getSetMode().ordinal());
		jsonObject.put("typePointerAreaOrdinal", e.getTypePointerArea().ordinal());
		jsonObject.put("typeObjExtOrdinal", e.getTypeObjExt().ordinal());
		
	}
	

	
	private void populateJsonDynamicFieldSubValue(EntityDynamicFieldSubValue e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("numInstr", e.getNumInstr());
		jsonObject.put("idField", e.getIdField());		
		jsonObject.put("idSubField", e.getIdSubField());
		jsonObject.put("progr", e.getProgr());
		
		jsonObject.put("posInSubField", e.getPosInSubField());
		jsonObject.put("lngInSubField", e.getLngInSubField());
		jsonObject.put("typeObjectFrom", e.getTypeObjectFrom());
		jsonObject.put("numInstrFrom", e.getNumInstrFrom());	
		jsonObject.put("value", e.getValue());	
		
		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeObjectFromOrdinal", e.getTypeObjectFrom().ordinal());
	}
	
	private void populateJsonObjectAnalysisError(EntityObjectAnalysisError e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("typeSource", e.getTypeSource());
		jsonObject.put("rowNum", e.getRowNum());
		
		jsonObject.put("numInstr", e.getNumInstr());
		jsonObject.put("sourceInstr", e.getSourceInstr());
		jsonObject.put("tokenError", e.getTokenError());
		jsonObject.put("rowNumInstrBegin", e.getRowNumInstrBegin());
		jsonObject.put("rowNumInstrFinal", e.getRowNumInstrFinal());
		jsonObject.put("copyName", e.getCopyName());
		jsonObject.put("msgCode", e.getMsgCode());
		jsonObject.put("msgText", e.getMsgText());
		jsonObject.put("stackTrace", e.getStackTrace());
		jsonObject.put("ecxpDescription", e.getExcpDescription());
		jsonObject.put("ecxp", e.getExcp());
		jsonObject.put("parsingError", e.getParsingError());
		jsonObject.put("semanticError", e.getSemanticError());
		jsonObject.put("warning", e.getWarning());
		
		jsonObject.put("typeProcessAnalysis", e.getTypeProcessAnalysis());
		jsonObject.put("activeCobolDivision", e.getActiveCobolDivision());
		jsonObject.put("activeCobolSection", e.getActiveCobolSection());
		jsonObject.put("typeInstr", e.getTypeInstr());
		jsonObject.put("msgType", e.getMsgType());
		
		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeSourceOrdinal", e.getTypeSource().ordinal());
		jsonObject.put("typeProcessAnalysisOrdinal", e.getTypeProcessAnalysis().ordinal());
		jsonObject.put("activeCobolDivisionOrdinal", e.getActiveCobolDivision().ordinal());
		jsonObject.put("activeCobolSectionOrdinal", e.getActiveCobolSection().ordinal());
		jsonObject.put("typeInstrOrdinal", e.getTypeInstr().ordinal());
		jsonObject.put("msgTypeOrdinal", e.getMsgType().ordinal());
	}

	private void populateJsonObjectAnalysisInfo(EntityObjectAnalysisInfo e, JSONObject jsonObject) {
		jsonObject.put("sys", e.getSystem());
		jsonObject.put("subSystem", e.getSubSystem());
		jsonObject.put("idObject", e.getIdObject());
		jsonObject.put("typeObject", e.getTypeObject());
		jsonObject.put("typeSource", e.getTypeSource());

		jsonObject.put("sizeBytes", e.getSizeBytes()); 
		jsonObject.put("numRowsCommTot", e.getNumRowsCommTot()); 
		jsonObject.put("numRowsTot", e.getNumRowsTot()); 
		jsonObject.put("numRowsEmptyTot", e.getNumRowsEmptyTot()); 
		jsonObject.put("numRowsBlankTot", e.getNumRowsBlankTot()); 
		jsonObject.put("numRowsCommData", e.getNumRowsCommData()); 
		jsonObject.put("numRowsData", e.getNumRowsData()); 
		jsonObject.put("numRowsEmptyData", e.getNumRowsEmptyData()); 
		jsonObject.put("numRowsBlankData", e.getNumRowsBlankData()); 
		jsonObject.put("numRowsCommProc", e.getNumRowsCommProc()); 
		jsonObject.put("numRowsProc", e.getNumRowsProc()); 
		jsonObject.put("numRowsEmptyProc", e.getNumRowsEmptyProc()); 
		jsonObject.put("numRowsBlankProc", e.getNumRowsBlankProc()); 
		jsonObject.put("numStmtProc", e.getNumStmtProc()); 
		jsonObject.put("numDefData", e.getNumDefData()); 
		jsonObject.put("numDefDataProgram", e.getNumDefDataProgram()); 
		jsonObject.put("numDefDataCopy", e.getNumDefDataCopy()); 
		jsonObject.put("dtAnalysis", e.getDtAnalysis()); 
		jsonObject.put("tmStartAnalysis", e.getTmStartAnalysis()); 
		jsonObject.put("tmEndAnalysis", e.getTmEndAnalysis()); 
		jsonObject.put("msElapsedTot", e.getMsElapsedTot()); 
		jsonObject.put("msParsing", e.getMsParsing()); 
		jsonObject.put("msPostParsingOperations", e.getMsPostParsingOperations()); 
		jsonObject.put("msGraphCreation", e.getMsGraphCreation()); 
		jsonObject.put("msDynamicCodeSolving", e.getMsDynamicCodeSolving()); 
		jsonObject.put("msMetricComputing", e.getMsMetricComputing()); 
		jsonObject.put("msViolationDetecting", e.getMsViolationDetecting()); 
		jsonObject.put("msSerializationPgm", e.getMsSerializationPgm()); 
		jsonObject.put("msUpdObject", e.getMsUpdObject()); 
		jsonObject.put("msInsRelation", e.getMsInsRelation()); 
		jsonObject.put("msInsRelationOrigin", e.getMsInsRelationOrigin()); 
		jsonObject.put("msInsWhereUsed", e.getMsInsWhereUsed()); 
		jsonObject.put("msInsCopyEntity", e.getMsInsCopyEntity()); 
		jsonObject.put("msInsMetric", e.getMsInsMetric()); 
		jsonObject.put("msInsMetricViolation", e.getMsInsMetricViolation()); 
		jsonObject.put("msInsDynamicField", e.getMsInsDynamicField()); 
		jsonObject.put("msDbDelete", e.getMsDbDelete()); 
		jsonObject.put("msDbUpdate", e.getMsDbUpdate()); 
		jsonObject.put("cntInsObject", e.getCntInsObject()); 
		jsonObject.put("cntUpdObject", e.getCntUpdObject()); 
		jsonObject.put("cntInsRelation", e.getCntInsRelation()); 
		jsonObject.put("cntInsRelationOrigin", e.getCntInsRelationOrigin()); 
		jsonObject.put("cntInsWhereUsed", e.getCntInsWhereUsed()); 
		jsonObject.put("cntInsCopyEntity", e.getCntInsCopyEntity()); 
		jsonObject.put("cntInsMetric", e.getCntInsMetric()); 
		jsonObject.put("cntInsMetricViolation", e.getCntInsMetricViolation()); 
		jsonObject.put("cntInsDynamicField", e.getCntInsDynamicField()); 
		jsonObject.put("withException", e.getWithException()); 
		jsonObject.put("withAnyCobolParsingErrors", e.getWithAnyCobolParsingErrors()); 
		jsonObject.put("withAnySqlParsingErrors", e.getWithAnySqlParsingErrors()); 
		jsonObject.put("withAnyCicsParsingErrors", e.getWithAnyCicsParsingErrors()); 
		jsonObject.put("withAnyDL1ParsingErrors", e.getWithAnyDL1ParsingErrors()); 

		jsonObject.put("typeObjectOrdinal", e.getTypeObject().ordinal());
		jsonObject.put("typeSourceOrdinal", e.getTypeSource().ordinal());

	}

	private void populateJsonMetricViolationConfig(EntityMetricViolationConfig eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("configName", eo.getConfigName());
		jsonObject.put("typeViolation", eo.getTypeViolation());

		jsonObject.put("isRuleEnabled", eo.getRuleEnabled());		
		jsonObject.put("violationDesc", eo.getViolationDesc());
		jsonObject.put("violationSeverity", eo.getViolationSeverity());
		jsonObject.put("ruleType", eo.getRuleType());
		jsonObject.put("qualityFactor", eo.getQualityFactor());
		jsonObject.put("qualityCharacteristic", eo.getQualityCharacteristic());
		jsonObject.put("remediationCost", eo.getRemediationCost());
		jsonObject.put("remediationUnit", eo.getRemediationUnit());
		jsonObject.put("threshold", eo.getThreshold());
		jsonObject.put("thresholdGood", eo.getThresholdGood());
		jsonObject.put("thresholdLow", eo.getThresholdLow());
		jsonObject.put("thresholdHigh", eo.getThresholdHigh());
		
		jsonObject.put("typeViolationOrdinal", eo.getTypeViolation().ordinal());
		jsonObject.put("ruleTypeOrdinal", eo.getRuleType().ordinal());
		jsonObject.put("violationSeverityOrdinal", eo.getViolationSeverity().ordinal());
		jsonObject.put("remediationUnitOrdinal", eo.getRemediationUnit().ordinal());
		jsonObject.put("qualityFactorOrdinal", eo.getQualityFactor().ordinal());
		jsonObject.put("qualityCharacteristicOrdinal", eo.getQualityCharacteristic().ordinal());
		jsonObject.put("remediationUnitOrdinal", eo.getRemediationUnit().ordinal());
		jsonObject.put("thresholdOrdinal", eo.getThreshold().ordinal());
	}	
	
	private void populateJsonMetricViolation(EntityMetricViolation eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("scope", eo.getScope());
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("section", eo.getSection());
		jsonObject.put("typeViolation", eo.getTypeViolation());

		jsonObject.put("severityViolation", eo.getSeverityViolation());
		jsonObject.put("originViolation", eo.getOriginViolation());
		jsonObject.put("originViolationRows", eo.getOriginViolationRows());
		jsonObject.put("originViolationRowsCopy", eo.getOriginViolationRowsCopy());
		jsonObject.put("cntViolations", eo.getCntViolations());
		jsonObject.put("value", eo.getValue());
		jsonObject.put("remediationCost", eo.getRemediationCost());
		jsonObject.put("remediationUnit", eo.getRemediationUnit());
		jsonObject.put("qualityCharacteristic", eo.getQualityCharacteristic());
		
		jsonObject.put("scopeOrdinal", eo.getScope().ordinal());
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("typeViolationOrdinal", eo.getTypeViolation().ordinal());
		jsonObject.put("severityViolationOrdinal", eo.getSeverityViolation().ordinal());
		jsonObject.put("remediationUnitOrdinal", eo.getRemediationUnit().ordinal());
		jsonObject.put("qualityCharacteristicOrdinal", eo.getQualityCharacteristic().ordinal());
	}	
	
	// Richiamato anche da RestProgramService
	public static void populateJsonMetricValue(EntityMetricValue eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem());
		jsonObject.put("scope", eo.getScope());
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("section", eo.getSection());

	    // Colonne senza vincoli di chiave (No key)
        jsonObject.put("sectionThru", eo.getSectionThru()); 
	    // Misure di conteggio sorgenti
	    jsonObject.put("cntPgmAnalyzed", eo.getCntPgmAnalyzed());            
		jsonObject.put("cntPgmExecuted", eo.getCntPgmExecuted());           
		jsonObject.put("cntSubPgmAnalyzed", eo.getCntSubPgmAnalyzed());        
		jsonObject.put("cntSubPgmExecuted", eo.getCntSubPgmExecuted());        
		jsonObject.put("cntCopyDefined", eo.getCntCopyDefined());           
		jsonObject.put("cntJclJob", eo.getCntJclJob());                
		jsonObject.put("cntJclInclude", eo.getCntJclInclude());            
		jsonObject.put("cntJclProc", eo.getCntJclProc());               
		// Misure dimensionali sorgenti
		jsonObject.put("sizeLinesCodeLogical", eo.getSizeLinesCodeLogical());     
		jsonObject.put("sizeLinesCodePhisical", eo.getSizeLinesCodePhisical());    
		jsonObject.put("sizeLinesBlank", eo.getSizeLinesBlank());           
		jsonObject.put("sizeLinesBlankProc", eo.getSizeLinesBlankProc());       
		jsonObject.put("sizeLinesBlankData", eo.getSizeLinesBlankData());       
		jsonObject.put("sizeLinesComment", eo.getSizeLinesComment());        
		jsonObject.put("sizeLinesCommentProc", eo.getSizeLinesCommentProc());     
		jsonObject.put("sizeLinesCommentData", eo.getSizeLinesCommentData());     
		jsonObject.put("sizeInstr", eo.getSizeInstr());               
		// Misure stimate con sizeLinesCodeLogical
		jsonObject.put("backFiredFunctionPoint", eo.getBackFiredFunctionPoint());   
		jsonObject.put("timeDevelopment", eo.getTimeDevelopment());          
		// Misure definizione dati
		jsonObject.put("defFields", eo.getDefFields());                
		jsonObject.put("defFieldsInCopy", eo.getDefFieldsInCopy());          
		jsonObject.put("defLiterals", eo.getDefLiterals());                  
		// Misure di documentazione
		jsonObject.put("percComByLogical", eo.getPercComByLogical());        
		jsonObject.put("percComByPhisical", eo.getPercComByPhisical());        
		jsonObject.put("percComByInstruction", eo.getPercComByInstruction());     
		jsonObject.put("percBlankByPhisical", eo.getPercBlankByPhisical());      
		jsonObject.put("percBlankByInstruction", eo.getPercBlankByInstruction());   
		// Misure di codice dinamico
		jsonObject.put("dynamicPgm", eo.getDynamicPgm());               
		jsonObject.put("dynamicInstr", eo.getDynamicInstr());             
		jsonObject.put("dynamicInstrLight", eo.getDynamicInstrLight());        
		jsonObject.put("percDynamicInstr", eo.getPercDynamicInstr());         
		jsonObject.put("percDynamicInstrLight", eo.getPercDynamicInstrLight());    
		// Misure violazioni
		jsonObject.put("violations", eo.getViolations());              
		jsonObject.put("percViolationsByLogical", eo.getPercViolationsByLogical()); 
		jsonObject.put("percViolationsByPhisical", eo.getPercViolationsByPhisical());
		jsonObject.put("percViolationsByInstruction", eo.getPercViolationsByInstruction()); 
		// Misure di codice morto
		jsonObject.put("deadFields", eo.getDeadFields());               
		jsonObject.put("deadSubGraph", eo.getDeadSubGraph());             
		jsonObject.put("deadInstr", eo.getDeadInstr());                
		jsonObject.put("deadLabels", eo.getDeadLabels());               
		jsonObject.put("deadCopyData", eo.getDeadCopyData());             
		jsonObject.put("deadCopyProc", eo.getDeadCopyProc());             
		// Misure di jcl
		jsonObject.put("jclDD", eo.getJclDD());                    
		jsonObject.put("jclStepDefined", eo.getJclStepDefined());           
		jsonObject.put("jclStepUpdate", eo.getJclStepUpdate());            
		jsonObject.put("jclDsname", eo.getJclDsname());               
		jsonObject.put("jclDsnameReferenced", eo.getJclDsnameReferenced());      
		jsonObject.put("jclDsnameUnReferenced", eo.getJclDsnameUnReferenced());    
		jsonObject.put("jclIncludeCalled", eo.getJclIncludeCalled());         
		jsonObject.put("jclProcCalled", eo.getJclProcCalled());            
		// Misure di complessità strutturale
		jsonObject.put("structFanIn", eo.getStructFanIn());              
		jsonObject.put("structFanOut", eo.getStructFanOut());            
		jsonObject.put("structSections", eo.getStructSections());           
		jsonObject.put("structParagraphs", eo.getStructParagraphs());         
		// Misure di complessità funzionale generiche
		jsonObject.put("funcObjects", eo.getFuncObjects());              
		jsonObject.put("funcRelations", eo.getFuncRelations());            
		jsonObject.put("funcTranInternal", eo.getFuncTranInternal());         
		jsonObject.put("funcTranExternal", eo.getFuncTranExternal());         
		jsonObject.put("funcMap", eo.getFuncMap());                  
		jsonObject.put("funcCallInternal", eo.getFuncCallInternal());         
		jsonObject.put("funcCallExternal", eo.getFuncCallExternal());         
		jsonObject.put("funcAccEntityInternal", eo.getFuncAccEntityInternal());    
		jsonObject.put("funcAccEntityExternal", eo.getFuncAccEntityExternal());    
		jsonObject.put("funcAccMediaInternal", eo.getFuncAccMediaInternal());     
		jsonObject.put("funcAccMediaExternal", eo.getFuncAccMediaExternal());     
		// Misure di complessità funzionale Function Point
		jsonObject.put("fpExternalOutputEO", eo.getFpExternalOutputEO());       
		jsonObject.put("fpExternalInputEI", eo.getFpExternalInputEI());        
		jsonObject.put("fpExternalInquiryEQ", eo.getFpExternalInquiryEQ());      
		jsonObject.put("fpInternalLogicalFileILF", eo.getFpInternalLogicalFileILF()); 
		jsonObject.put("fpExternalInterfaceFileEIF", eo.getFpExternalInterfaceFileEIF());   
		// Misure di complessità funzionale/tecnica per rehosting 
		jsonObject.put("rhRateObjectRelation", eo.getRhRateObjectRelation());     
		jsonObject.put("rhObjectsInternal", eo.getRhObjectsInternal());        
		jsonObject.put("rhObjectsExternal", eo.getRhObjectsExternal());        
		jsonObject.put("rhObjectsUnportable", eo.getRhObjectsUnportable());      
		jsonObject.put("rhFilesBynary", eo.getRhFilesBynary());            
		// Misure di complessità ciclomatica
		jsonObject.put("mcCabeArcs", eo.getMcCabeArcs());               
		jsonObject.put("mcCabeNodes", eo.getMcCabeNodes());              
		jsonObject.put("mcCabeGraphConn", eo.getMcCabeGraphConn());          
		jsonObject.put("mcCabeOperatorsOrAnd", eo.getMcCabeOperatorsOrAnd());     
	    // Misure di complessità di Halstead (o Software Science)
		jsonObject.put("halsteadOperators", eo.getHalsteadOperators());        
		jsonObject.put("halsteadOperands", eo.getHalsteadOperands());         
		jsonObject.put("halsteadOperatorsOcc", eo.getHalsteadOperatorsOcc());     
		jsonObject.put("halsteadOperandsOcc", eo.getHalsteadOperandsOcc());      
		jsonObject.put("halsteadLengthPgm", eo.getHalsteadLengthPgm());        
		jsonObject.put("halsteadVocabularyPgm", eo.getHalsteadVocabularyPgm());       
		jsonObject.put("halsteadVolumePgm", eo.getHalsteadVolumePgm());        
		jsonObject.put("halsteadDifficultPgm", eo.getHalsteadDifficultPgm());     
		jsonObject.put("halsteadEffortPgm", eo.getHalsteadEffortPgm());        
		jsonObject.put("halsteadTimeWriting", eo.getHalsteadTimeWriting());      
		// Indici di complessita/manutenibilità medi 
		jsonObject.put("idxMIAvg", eo.getIdxMIAvg());                 
		jsonObject.put("idxFPAvg", eo.getIdxFPAvg());                 
		jsonObject.put("idxMcCabeAvg", eo.getIdxMcCabeAvg());             
		jsonObject.put("idxReHostingAvg", eo.getIdxReHostingAvg());          
		// Indici di complessita/manutenibilità massimi 
		jsonObject.put("idxMIHigh", eo.getIdxMIHigh());                
		jsonObject.put("idxFPHigh", eo.getIdxFPHigh());                
		jsonObject.put("idxMcCabeHigh", eo.getIdxMcCabeHigh());            
		jsonObject.put("idxReHostingHigh", eo.getIdxReHostingHigh());         
		// Indici di complessita/manutenibilità minimi
		jsonObject.put("idxMILow", eo.getIdxMILow());                 
		jsonObject.put("idxFPLow", eo.getIdxFPLow());                 
		jsonObject.put("idxMcCabeLow", eo.getIdxMcCabeLow());             
		jsonObject.put("idxReHostingLow", eo.getIdxReHostingLow());          
		// Indici di complessita/manutenibilità totali 
		jsonObject.put("idxMITot", eo.getIdxMITot());                 
		jsonObject.put("idxFPTot", eo.getIdxFPTot());                 
		jsonObject.put("idxMcCabeTot", eo.getIdxMcCabeTot());             
		jsonObject.put("idxReHostingTot", eo.getIdxReHostingTot());          
		// Sistema di qualità SQUALE numero violazioni per categoria gravità
		jsonObject.put("squaleViolationsBlocker", eo.getSqualeViolationsBlocker());  
		jsonObject.put("squaleViolationsCritical", eo.getSqualeViolationsCritical()); 
		jsonObject.put("squaleViolationsMajor", eo.getSqualeViolationsMajor());    
		jsonObject.put("squaleViolationsMinor", eo.getSqualeViolationsMinor());    
		jsonObject.put("squaleViolationsInfo", eo.getSqualeViolationsInfo());     
		// Sistema di qualità SQUALE valori generali
		jsonObject.put("squaleSSRL", eo.getSqualeSSRL());             
		jsonObject.put("squaleSSRI", eo.getSqualeSSRI());               
		jsonObject.put("squaleSSCI", eo.getSqualeSSCI());               
		jsonObject.put("squaleSSQI", eo.getSqualeSSQI());               
		// Sistema di qualità SQUALE valori di dettaglio indice qualità assoluto SQI SQxI
		jsonObject.put("squaleSQTI", eo.getSqualeSQTI());               
		jsonObject.put("squaleSQRI", eo.getSqualeSQRI());               
		jsonObject.put("squaleSQCI", eo.getSqualeSQCI());               
		jsonObject.put("squaleSQEI", eo.getSqualeSQEI());               
		jsonObject.put("squaleSQSI", eo.getSqualeSQSI());               
		jsonObject.put("squaleSQMI", eo.getSqualeSQMI());               
		jsonObject.put("squaleSQPI", eo.getSqualeSQPI());               
		// Sistema di qualità SQUALE valori di dettaglio indici consolidati SCTx
		jsonObject.put("squaleSCTI", eo.getSqualeSCTI());               
		jsonObject.put("squaleSCRI", eo.getSqualeSCRI());              
		jsonObject.put("squaleSCCI", eo.getSqualeSCCI());               
		jsonObject.put("squaleSCEI", eo.getSqualeSCEI());              
		jsonObject.put("squaleSCSI", eo.getSqualeSCSI());               
		jsonObject.put("squaleSCMI", eo.getSqualeSCMI());               
		jsonObject.put("squaleSCPI", eo.getSqualeSCPI());               
		// Sistema di qualità SQUALE valori di dettaglio indici di densita SDxI
		jsonObject.put("squaleSDTI", eo.getSqualeSDTI());               
		jsonObject.put("squaleSDRI", eo.getSqualeSDRI());               
		jsonObject.put("squaleSDCI", eo.getSqualeSDCI());              
		jsonObject.put("squaleSDEI", eo.getSqualeSDEI());               
		jsonObject.put("squaleSDSI", eo.getSqualeSDSI());               
		jsonObject.put("squaleSDMI", eo.getSqualeSDMI());               
		jsonObject.put("squaleSDPI", eo.getSqualeSDPI());               
		// Sistema di qualità SQUALE valori di dettaglio indici squale rating SRxI 
		jsonObject.put("squaleSRTI", eo.getSqualeSRTI());               
		jsonObject.put("squaleSRRI", eo.getSqualeSRRI());               
		jsonObject.put("squaleSRCI", eo.getSqualeSRCI());               
		jsonObject.put("squaleSREI", eo.getSqualeSREI());               
		jsonObject.put("squaleSRSI", eo.getSqualeSRSI());
		jsonObject.put("squaleSRMI", eo.getSqualeSRMI());               
		jsonObject.put("squaleSRPI", eo.getSqualeSRPI());               
		// Sistema di qualità SQUALE valori di dettaglio livelli squale rating SRxL 
		jsonObject.put("squaleSRTL", eo.getSqualeSRTL());               
		jsonObject.put("squaleSRRL", eo.getSqualeSRRL());               
		jsonObject.put("squaleSRCL", eo.getSqualeSRCL());              
		jsonObject.put("squaleSREL", eo.getSqualeSREL());               
		jsonObject.put("squaleSRSL", eo.getSqualeSRSL());               
		jsonObject.put("squaleSRML", eo.getSqualeSRML());               
		jsonObject.put("squaleSRPL", eo.getSqualeSRPL());               		
		
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("scopeOrdinal", eo.getScope().ordinal());
		jsonObject.put("squaleSSRLOrdinal", eo.getSqualeSSRL().ordinal());
		jsonObject.put("squaleSRTLOrdinal", eo.getSqualeSRTL().ordinal());
		jsonObject.put("squaleSRRLOrdinal", eo.getSqualeSRRL().ordinal());
		jsonObject.put("squaleSRCLOrdinal", eo.getSqualeSRCL().ordinal());
		jsonObject.put("squaleSRELOrdinal", eo.getSqualeSREL().ordinal());
		jsonObject.put("squaleSRSLOrdinal", eo.getSqualeSRSL().ordinal());
		jsonObject.put("squaleSRMLOrdinal", eo.getSqualeSRML().ordinal());
		jsonObject.put("squaleSRPLOrdinal", eo.getSqualeSRPL().ordinal());
	}	

	private void populateJsonDynamicFieldSubWaitExt(EntityDynamicFieldSubWaitExt eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("numInstr", eo.getNumInstr());
		jsonObject.put("idField", eo.getIdField());
		jsonObject.put("idSubField", eo.getIdSubField());
		jsonObject.put("numProgr", eo.getNumProgr());

		// Data
		jsonObject.put("typeObjectExternal", eo.getTypeObjectExternal());
		jsonObject.put("idObjectExternal", eo.getIdObjectExternal());
		jsonObject.put("dsnameExternal", eo.getDsnameExternal());
		jsonObject.put("typeSystemFieldExternal", eo.getTypeSystemFieldExternal());
		jsonObject.put("cicsNameExternal", eo.getCicsNameExternal());
		jsonObject.put("idFieldExternal", eo.getIdFieldExternal());
		jsonObject.put("posColumnExternal", eo.getPosColumnExternal());
		jsonObject.put("lengthColumnExternal", eo.getLengthColumnExternal());
		
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("typeObjectExternalOrdinal", eo.getTypeObjectExternal().ordinal());
		jsonObject.put("typeSystemFieldExternalOrdinal", eo.getTypeSystemFieldExternal().ordinal());
	}	

	private void populateJsonDynamicValueExt(EntityDynamicValueExt eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("typeObjectExternal", eo.getTypeObjectExternal());
		jsonObject.put("idObjectExternal", eo.getIdObjectExternal());
		jsonObject.put("dsnameExternal", eo.getDsnameExternal());
		jsonObject.put("idFieldColumnExternal", eo.getIdFieldColumnExternal());
		jsonObject.put("typeSystemFieldExternal", eo.getTypeSystemFieldExternal());
		jsonObject.put("cicsNameExternal", eo.getCicsNameExternal());
		jsonObject.put("numProgr", eo.getNumProgr());
		
		// Data
		jsonObject.put("posColumnExternal", eo.getPosColumnExternal());
		jsonObject.put("lengthColumnExternal", eo.getLengthColumnExternal());
		jsonObject.put("value", eo.getValue());
		
		jsonObject.put("typeObjectExternalOrdinal", eo.getTypeObjectExternal().ordinal());
		jsonObject.put("typeSystemFieldExternalOrdinal", eo.getTypeSystemFieldExternal().ordinal());
	}	

	private void populateJsonDynamicCicsMapping(EntityDynamicCicsMapping eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("cicsName", eo.getCicsName());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("externalName", eo.getExternalName());
		
		// Data
		jsonObject.put("dsname", eo.getDsname());
		
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
	}	

	public static void populateJsonMapDescriptor(EntityMapDescriptor eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());

		// Data
		jsonObject.put("idObjectMapset", eo.getIdObjectMapset());
		jsonObject.put("typeObjectMapset", eo.getTypeObjectMapset());
		jsonObject.put("idObjectMap", eo.getIdObjectMap());
		jsonObject.put("typeObjectMap", eo.getTypeObjectMap());
		jsonObject.put("idObjectBmsSource", eo.getIdObjectBmsSource());
		jsonObject.put("typeObjectBmsSource", eo.getTypeObjectBmsSource());
		jsonObject.put("fieldCursor", eo.getFieldCursor());
		jsonObject.put("rowsSize", eo.getRowsSize());
		jsonObject.put("colsSize", eo.getColsSize());
		jsonObject.put("rowBegin", eo.getRowBegin());
		jsonObject.put("colBegin", eo.getColBegin());
		
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("typeObjectMapOrdinal", eo.getTypeObjectMap().ordinal());
		jsonObject.put("typeObjectMapsetOrdinal", eo.getTypeObjectMapset().ordinal());
		jsonObject.put("typeObjectBmsSourceOrdinal", eo.getTypeObjectBmsSource().ordinal());
	}	

	public static void populateJsonMapItem(EntityMapItem eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("row", eo.getRow());
		jsonObject.put("col", eo.getCol());

		// Data
		jsonObject.put("bmsIdField", eo.getBmsIdField());
		jsonObject.put("bmsDescField", eo.getBmsDescField());
		jsonObject.put("bmsFlgData", eo.getBmsFlgData());
		jsonObject.put("bmsLength", eo.getBmsLength());
		jsonObject.put("bmsNumDec", eo.getBmsNumDec());
		jsonObject.put("bmsInitial", eo.getBmsInitial());
		jsonObject.put("bmsNumGrp", eo.getBmsNumGrp());
		jsonObject.put("bmsNumGrpOccurs", eo.getBmsNumGrpOccurs());
		jsonObject.put("bmsNumericSeparator", eo.getBmsNumericSeparator());
		jsonObject.put("bmsCtrlMinMax", eo.getBmsCtrlMinMax());
		jsonObject.put("bmsFlgIc", eo.getBmsFlgIc());
		jsonObject.put("bmsFlgProt", eo.getBmsFlgProt());
		jsonObject.put("bmsFlgUnprot", eo.getBmsFlgUnprot());
		jsonObject.put("bmsFlgAskip", eo.getBmsFlgAskip());
		jsonObject.put("bmsFlgNorm", eo.getBmsFlgNorm());
		jsonObject.put("bmsFlgBrt", eo.getBmsFlgBrt());
		jsonObject.put("bmsFlgDark", eo.getBmsFlgDark());
		jsonObject.put("bmsFlgFset", eo.getBmsFlgFset());
		
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
	}	

	private void populateJsonImpactPlan(EntityImpactPlan eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("idPlan", eo.getIdPlan());
		jsonObject.put("idObjectOrigin", eo.getIdObjectOrigin());
		jsonObject.put("typeObjectOrigin", eo.getTypeObjectOrigin());
		jsonObject.put("typeImpactChange", eo.getTypeImpactChange());
		jsonObject.put("numOp", eo.getNumOp());

		// Data
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("fieldColumn", eo.getFieldColumn()); 
		jsonObject.put("fromLength", eo.getFromLength());
		jsonObject.put("toLength", eo.getToLength());
		jsonObject.put("fromInt", eo.getFromInt());
		jsonObject.put("toInt", eo.getToInt());
		jsonObject.put("fromDec", eo.getFromDec());
		jsonObject.put("toDec", eo.getToDec());
		jsonObject.put("fromDataType", eo.getFromDataType());
		jsonObject.put("toDataType", eo.getToDataType());
		jsonObject.put("fromDataTypePic", eo.getFromDataTypePic());
		jsonObject.put("toDataTypePic", eo.getToDataTypePic());
		jsonObject.put("fromDefaultValue", eo.getFromDefaultValue());
		jsonObject.put("toDefaultValue", eo.getToDefaultValue());
		jsonObject.put("fromSign", eo.getFromSign());
		jsonObject.put("toSign", eo.getToSign());
												
		jsonObject.put("typeObjectOriginOrdinal", eo.getTypeObjectOrigin().ordinal());
		jsonObject.put("typeImpactChangeOrdinal", eo.getTypeImpactChange().ordinal());
		jsonObject.put("fromDataTypeOrdinal", eo.getFromDataType().ordinal());
		jsonObject.put("toDataTypeOrdinal", eo.getFromDataType().ordinal());
	}	
	
	private void populateJsonImpactObject(EntityImpactObject eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idPlan", eo.getIdPlan());
		jsonObject.put("numOp", eo.getNumOp());
		jsonObject.put("typeObjectOrigin", eo.getTypeObjectOrigin());
		jsonObject.put("idObjectOrigin", eo.getIdObjectOrigin());
		jsonObject.put("typeObjectTarget", eo.getTypeObjectTarget());
		jsonObject.put("idObjectTarget", eo.getIdObjectTarget());
		jsonObject.put("rowStart", eo.getRowStart());

		// Data
		jsonObject.put("rowEnd", eo.getRowEnd());
		jsonObject.put("numInstr", eo.getNumInstr());
		jsonObject.put("cobolDivision", eo.getCobolDivision());
		
		jsonObject.put("typeObjectOriginOrdinal", eo.getTypeObjectOrigin().ordinal());
		jsonObject.put("typeObjectTargetOrdinal", eo.getTypeObjectTarget().ordinal());
		jsonObject.put("cobolDivisionOrdinal", eo.getCobolDivision().ordinal());
	}	
	
	private void populateJsonImpactCompile(EntityImpactCompile eo, JSONObject jsonObject) {
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idPlan", eo.getIdPlan());
		jsonObject.put("typeObjectCompile", eo.getTypeObjectCompile());
		jsonObject.put("idObjectCompile", eo.getIdObjectCompile());

		// Data
		
		jsonObject.put("typeObjectCompileOrdinal", eo.getTypeObjectCompile().ordinal());
	}	
	
	private void populateJsonEntityprocessLog(EntityProcessLog eo, JSONObject jsonObject) {
		jsonObject.put("user", eo.getUser());
		jsonObject.put("sys", eo.getSystem());
		jsonObject.put("subSystem", eo.getSubSystem()); 
		jsonObject.put("idObject", eo.getIdObject());
		jsonObject.put("typeObject", eo.getTypeObject());
		jsonObject.put("typeProcess", eo.getTypeProcess());
		jsonObject.put("dtStartProcess", eo.getDtStartProcess());
		jsonObject.put("tmStartProcess", eo.getTmStartProcess());

		// Data
		jsonObject.put("tmEndProcess", eo.getTmEndProcess());
		jsonObject.put("msDuration", eo.getMsDuration());
		jsonObject.put("statusProcess", eo.getStatusProcess());
		jsonObject.put("msgError", eo.getMsgError());
		jsonObject.put("idExcpError", eo.getIdExcpError());
		jsonObject.put("excpStackTrace", eo.getExcpStackTrace());
		jsonObject.put("threadNameError", eo.getThreadNameError());
		jsonObject.put("javaClassError", eo.getJavaClassError());
		
		jsonObject.put("typeProcessOrdinal", eo.getTypeProcess().ordinal());
		jsonObject.put("typeObjectOrdinal", eo.getTypeObject().ordinal());
		jsonObject.put("statusProcessOrdinal", eo.getStatusProcess().ordinal());
		
	}	
	

}  
