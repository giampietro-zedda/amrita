package rest;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import analyzer.AmritaConstants;
import analyzer.AmritaStartup;
import analyzer.DataBaseConnections;
import analyzer.DataItem;
import analyzer.DataItemCobolIdentifier;
import analyzer.ExecutionObject;
import analyzer.ExecutionObjectsToProcess;
import analyzer.Instruction;
import analyzer.InstructionCics;
import analyzer.InstructionCobol;
import analyzer.InstructionCobolDataItem;
import analyzer.InstructionCobolProcedure;
import analyzer.InstructionSql;
import analyzer.LogicDynamicField;
import analyzer.LogicDynamicFieldSub;
import analyzer.LogicDynamicFieldSubSetting;
import analyzer.LogicDynamicInstruction;
import analyzer.LogicDynamicValue;
import analyzer.LogicInfoDynamic;
import analyzer.LogicSamePgm;
import analyzer.LogicTools;
import analyzer.LogicWorkProcess;
import analyzer.ProgramCobol;
import analyzer.ProgramCobolEntry;
import analyzer.ProgramPath;
import analyzer.ProgramPathFieldIO;
import analyzer.ProgramPathEntry;
import analyzer.SourceManager;
import analyzer.UserActive;
import analyzer.UserConfiguration;
import analyzer.InstructionCobolDataItem.InnerConditionValueEntry;
import analyzer.InstructionCobolDataItem.InnerOccursKeyEntry;
import dao.IDAOObject;
import dao.IDAOObjectOption;
import dao.IDAORelation;
import dao.IDAORelationOrigin;
import dao.IDAOSqlGeneric;
import dao.MySQLDAOFactory;
import dao.DAOFactory;
import dao.DAOImplDynamicField;
import dao.DAOImplDynamicFieldSub;
import dao.DAOImplDynamicFieldSubSetting;
import dao.DAOImplDynamicFieldSubValue;
import dao.DAOImplMapDescriptor;
import dao.DAOImplMapItem;
import dao.DAOImplMetricValue;
import dao.DAOImplObject;
import dao.DAOImplObjectOption;
import dao.DAOImplRelation;
import dao.DAOImplRelationOrigin;
import dao.IDAODynamicField;
import dao.IDAODynamicFieldSub;
import dao.IDAOMapDescriptor;
import dao.IDAOMapItem;
import dao.IDAOMetricValue;
import entities.EntityDynamicField;
import entities.EntityDynamicFieldSub;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import entities.EntityMapDescriptor;
import entities.EntityMapItem;
import entities.EntityMetricValue;
import entities.EntityObject;
import entities.EntityObjectOption;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import entities.EntitySqlGeneric;
import enums.EnumAmritaExceptionError;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumMetricsScope;
import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumPrecompilerReservedWords;
import enums.EnumRelation;
import enums.EnumSymbolType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaSqlError;
import utilities.StringService;
import utilities.SystemService;


/*
 * RestSource
 * 
 * Gestione accesso a sources, pilot di esecuzione, oggetti serializzati.
 * Funzioni di interrogazione oggetto programma
 * Funzioni di attivazione logiche dinamiche stesso programma
 * 
 */
@Path("/")   
public class RestProgramService implements AmritaConstants {;

final String OBJECT_PGM_COBOL="1";             		    // 01 Programma COBOL
final String OBJECT_COPY_COBOL_PROC="9";	  			// 09 Sorgenti Copy Cobol Procedure Division
final String OBJECT_COPY_COBOL_DATA="10";				// 10 Sorgenti Copy Cobol Data Division
final String OBJECT_CICS_BMS="70";                    	// 70 BMS source name
final String OBJECT_SQL_SCRIPT="42";                	// 42 DB2 Script Sql con DDL, Commands etc., membro di libreria
final String OBJECT_JCL_JOB="54";          	            // 54 JCL source name con scheda job non definizione proc e non include
final String OBJECT_JCL_PROC="55";           	        // 55 JCL source name con definizione proc  
final String OBJECT_JCL_INCLUDE="56";           		// 56 JCL source incluso in jcl o altre include


/*
 * COMMON
 * 
 * Get and cache the program to be available for next requests
 */
@PUT  
@Produces("application/json")
@Path("programPutOnCache/{user}/{idObject}")  
public Response getProgramPutOnCache(
								  @PathParam("user")         String user
								, @PathParam("idObject")     String idProgram 
						
								) throws ExceptionAmrita  {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	String resultJson = "";
	
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	try {
			programCobol = getProgram(user, idProgram);
			resultJson = "'OK";
		} catch (ExceptionAmrita e) {
			resultJson = "KO" + e.getMessage();
		}  
		AmritaStartup.cache.put(idProgram, programCobol);
	}		
	    
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


/*
 * INSPECTOR
 * 
 * Retrieve the program structure
 */
@GET  
@Produces("application/json")
@Path("programStructure/{user}/{idObject}")  
public Response getProgramStructure(
									  @PathParam("user")         String user
									, @PathParam("idObject")     String idProgram 
							
									) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	List<ProgramStructure> al_pgmStr = null;
	Map<String, String> map_paragraphSection = null;   // To keep Stop run/Goback/.. stmt info
	JSONArray jsonArray = null; 		
	JSONObject jsonObject = null; 
	String resultJson = "";
	
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
    
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
    ProgramCobolEntry<? extends Instruction>[] arProcEntries = programCobol.entriesProcedure();
    
    al_pgmStr = new ArrayList<ProgramStructure>();
    map_paragraphSection = new HashMap<String, String>();
    
	// Attivazione metodo ricorsivo sulle istruzioni di mainline
	createProgramstructure(arProcEntries, 0, al_pgmStr, 0, programCobol.getNumLastInstrMainline(), map_paragraphSection);

    // creazione json con struttura programma
	jsonArray = new JSONArray();
	
	// Populate Json
	for (ProgramStructure oPgmStr : al_pgmStr) {
		jsonObject = new JSONObject(oPgmStr);
		jsonObject.put("showText", oPgmStr.showText);
		jsonObject.put("isPerform", oPgmStr.isPerform);
		jsonObject.put("isFromSection", oPgmStr.isFromSection);
		jsonObject.put("isPerformUnderIf", oPgmStr.isPerformUnderIf);
		jsonObject.put("isWithPerformsInside", oPgmStr.isWithPerformsInside);
		jsonObject.put("isParagraphSectionWithStopRun", oPgmStr.isParagraphSectionWithStopRun);
		jsonObject.put("performFrom", oPgmStr.performFrom);
		jsonObject.put("performThru", oPgmStr.performThru);
		jsonObject.put("numInstrFrom", oPgmStr.numInstrFrom);
		jsonObject.put("numInstrThru", oPgmStr.numInstrThru);
		jsonObject.put("rowFrom", oPgmStr.rowFrom);
		jsonObject.put("rowThru", oPgmStr.rowThru);
		jsonObject.put("numInstr", oPgmStr.numInstr);
		jsonObject.put("rowNumStart", oPgmStr.rowNumStart);
		jsonObject.put("rowNumEnd", oPgmStr.rowNumEnd);
		jsonObject.put("inCopy", oPgmStr.inCopy);
		jsonObject.put("levelNesting", oPgmStr.levelNesting);
		
		jsonArray.put(jsonObject);
	}

	// Return json object
	resultJson = jsonArray.toString();		
	    
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


// Metodo ricorsivo per estrarre la struttura del programma
private void createProgramstructure(ProgramCobolEntry<? extends Instruction>[] arProcEntries, int levelNesting, List<ProgramStructure> al_pgmStr, int iFrom, int iThru, Map<String, String> map_paragraphSection) {
	ProgramStructure pgmStr = null;
	ProgramCobolEntry<? extends Instruction> entryProcedure = null;
	ProgramCobolEntry<? extends Instruction> entryProcedureFromX = null;
	InstructionCobolProcedure instr = null;
	InstructionCobol instrCopy = null;
	InstructionSql instrSql = null;
	InstructionCics instrCics = null;
	EnumCobolReservedWords typeInstr = null;
	String performFrom = "";
	String performThru = "";
	String showText = "";
	String inCopy = "";
	boolean isPerform = false;
	boolean isPerformWithInstrToShow = false;
	boolean isPerformUnderIf = false;
	boolean isInnerPerform = false;
	boolean isInstrToShow = false;
	boolean isParagraphSectionWithStopRun = false;
	int numInstrFrom = 0;
	int numInstrThru = 0;
	int rowFrom = 0;
	int rowThru = 0;
	int levelNestingNew = 0;
	int rowNumStart = 0;
	int rowNumEnd = 0;
	int numInstr = 0;
	int i = 0;


	// Scan paragraph/section instructions (mainline if level 0 e flagMainline false)
	for (i = iFrom; i < iThru; i++) {
	        entryProcedure = arProcEntries[i]; 
	        
	        // Fine mainline
   			if (levelNesting == 0 && entryProcedure.isMainline() == false) {
				break;
			}
   			
	        isInstrToShow = false;
	        
	        //
	   		// Gestione istruzioni strutturali da mostrare
	        //
	   		showText = "";
	   		isPerform = false;
	   		performFrom = "";
	   		performThru = "";
	   		numInstrFrom = 0;
	   		numInstrThru = 0;
	   		isInnerPerform = false;
	   		rowFrom = 0;
	   		rowThru = 0;
	   		rowNumStart = 0;
	   		rowNumEnd = 0;
	   		numInstr = 0;
	   		inCopy = "";
	   		
	   		typeInstr = entryProcedure.getTypeInstr();
	   		if (typeInstr == EnumCobolReservedWords.PROC_PERFORM) {
	   				   			
	   			isInstrToShow = true;	
	   			
		   		// Estraggo informazioni istruzione
		   		instr =  (InstructionCobolProcedure) entryProcedure.getInstruction();
		   		numInstr = instr.getNumInstr();
		   		rowNumStart = instr.getRowStartSource();
		   		rowNumEnd = instr.getRowEndSource();
	   			showText = "";
	   			isPerform = true;
	   			isPerformUnderIf = false;
		   		performFrom = instr.performGetFrom();
		   		performThru = instr.performGetThru();		   		
		   		numInstrFrom = instr.performGetFromNumInstr();
		   		numInstrThru = instr.performGetThruNumInstr();
		   		if (numInstrThru == 0) {
		   			numInstrThru = getLabelNumInstr(numInstrFrom, arProcEntries);
		   			performThru = getLabelName(numInstrThru, arProcEntries);
				}
		   		isInnerPerform = instr.performIsInnerPerform();	  // Perform ... end-perform   		
		   		entryProcedureFromX = arProcEntries[numInstrFrom];
		   		if (entryProcedureFromX.isUnderCopy()) {
		   			inCopy = entryProcedureFromX.getUnderCopyName();
				}		   		
		   		rowFrom = entryProcedureFromX.getInstruction().getRowStartSource();
		   		entryProcedureFromX = arProcEntries[numInstrThru];
		   		rowThru = entryProcedureFromX.getInstruction().getRowStartSource();	  
		   		// Informazioni su paragrafo/section richiamato
		   		if (!isInnerPerform) {
					isPerformWithInstrToShow = isParagraphSectionWithInstrToShow(arProcEntries, numInstrFrom, numInstrThru);
					isParagraphSectionWithStopRun = isParagraphSectionWithStopRun(arProcEntries, numInstrFrom, numInstrThru);
					if (isParagraphSectionWithStopRun) {
						map_paragraphSection.put(performFrom, "STOP");
					} else {
						map_paragraphSection.put(performFrom, "");
					}
				}
		   		if (entryProcedure.getNumEntryOwnerConditional() > 0) {
					isPerformUnderIf = true;
				}
		   		
			// Call 
	   		} else if (entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_CALL) {
				isInstrToShow = true;
				instr = (InstructionCobolProcedure) entryProcedure.getInstruction();
				numInstr = instr.getNumInstr();
				rowNumStart = instr.getRowStartSource();
				rowNumEnd = instr.getRowEndSource();
				showText = "CALL " + instr.callGetProgram().getNameIdentifier();
				
	  		// Cancel
			} else if (entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_CANCEL) {
				isInstrToShow = true;
				instr = (InstructionCobolProcedure) entryProcedure.getInstruction();
				numInstr = instr.getNumInstr();
				rowNumStart = instr.getRowStartSource();
				rowNumEnd = instr.getRowEndSource();
			    ArrayList<DataItemCobolIdentifier> x = instr.callCancelGetPgmFieldIdentifiers();
			    showText = "CANCEL " + x.get(0).getNameIdentifier();
				showText = "CANCEL " + instr.callCancelGetPgmFieldIdentifiers().get(0).getNameIdentifier();
				if ( instr.callCancelGetPgmFieldIdentifiers().size() > 1) {showText+= "...";}
				
	   		// Copy
			} else if (entryProcedure.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY) {
				isInstrToShow = true;
				instrCopy = (InstructionCobol) entryProcedure.getInstruction();
				numInstr = instrCopy.getNumInstr();
				rowNumStart = instrCopy.getRowStartSource();
				rowNumEnd = instrCopy.getRowEndSource();
				showText = "COPY " + instrCopy.copyGetName();
				
	    	// Sql precompiler
			} else if (entryProcedure.getInstruction().getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) {
				isInstrToShow = true;
				instrSql = (InstructionSql) entryProcedure.getInstruction();
				numInstr = instrSql.getNumInstr();
				rowNumStart = instrSql.getRowStartSource();
				rowNumEnd = instrSql.getRowEndSource();
				showText = "SQL " + instrSql.getTypeInstrPrecompiler().getValueText1();
				// Sql Include
				if (instrSql.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE) {
	   				showText+= " " + instrSql.sqlIncludeGetName();
				}
				
			// Cics precompiler
			} else if (entryProcedure.getInstruction().getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_CICS) {
				isInstrToShow = true;
				instrCics = (InstructionCics) entryProcedure.getInstruction();
				rowNumStart = instrCics.getRowStartSource();
				rowNumEnd = instrCics.getRowEndSource();
				numInstr = instrCics.getNumInstr();
				showText = "CICS " + instrCics.getTypeInstrPrecompiler().getValueText1();
				
			// Inizio programma	
			} else if (entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_DIVISION) {
				isInstrToShow = true;
				isPerform = true;
				instr = (InstructionCobolProcedure) entryProcedure.getInstruction();
				numInstr = instr.getNumInstr();
				rowNumStart = instr.getRowStartSource();
				rowNumEnd = instr.getRowEndSource();
				performFrom = "= BEGIN MAINLINE =";
				
			// Istruzione di fine (dovrebbe essere in mainline ma può essere dentro una perform)	
			} else if (entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_GOBACK
				   ||  entryProcedure.getTypeInstr() == EnumCobolReservedWords.PROC_STOP) {
				isInstrToShow = true;
				isPerform = false;
				instr = (InstructionCobolProcedure) entryProcedure.getInstruction();
				numInstr = instr.getNumInstr();
				rowNumStart = instr.getRowStartSource();
				rowNumEnd = instr.getRowEndSource();
				showText = "";
				performFrom = instr.getTypeInstr().toString();
			}
			
	   		// Load in structure se istruzione da mostrare
	   		if (isInstrToShow) {
		   		pgmStr = new ProgramStructure();
		   		pgmStr.showText = showText;
		   		pgmStr.isPerform = isPerform;
		   		pgmStr.performFrom = performFrom;
		   		pgmStr.performThru = performThru;
		   		pgmStr.numInstrFrom = numInstrFrom;
		   		pgmStr.numInstrThru = numInstrThru;
		   		pgmStr.isWithPerformsInside = isPerformWithInstrToShow;   // Istruzioni perform dentro paragrafo/section
		   		pgmStr.isPerformUnderIf = isPerformUnderIf;               // Istruzione perform sotto condizione
		   		pgmStr.isParagraphSectionWithStopRun = isParagraphSectionWithStopRun;
		   		pgmStr.rowFrom = rowFrom;
		   		pgmStr.rowThru = rowThru;
		   		pgmStr.numInstr = numInstr;
		   		pgmStr.rowNumStart = rowNumStart;
		   		pgmStr.rowNumEnd = rowNumEnd;
		   		pgmStr.inCopy = inCopy;
		   		pgmStr.levelNesting = levelNesting;	   		
		   		al_pgmStr.add(pgmStr);	
		   		
				// Paragrafo/section con perform statements inside, attivazione ricorsiva 	
				if (typeInstr == EnumCobolReservedWords.PROC_PERFORM  
				&&  !isInnerPerform
				&&   isPerformWithInstrToShow) {					
					levelNestingNew = levelNesting + 1;
					createProgramstructure(arProcEntries, levelNestingNew, al_pgmStr, numInstrFrom, numInstrThru, map_paragraphSection);
				}				
		   	} // if
	  } // for
	
	// Entry di fine mainline solo se levelNestingNew = 0
	if (levelNesting > 0) { return;}
	pgmStr = new ProgramStructure();
	pgmStr.showText = showText;
	pgmStr.isPerform = false;
	pgmStr.performFrom = "= END MAINLINE =";
	pgmStr.performThru = "";
	pgmStr.numInstrFrom = 0;
	pgmStr.numInstrThru = 0;
	pgmStr.isWithPerformsInside = false;
	pgmStr.isPerformUnderIf = false;
	pgmStr.isParagraphSectionWithStopRun = false;
	pgmStr.rowFrom = 0;
	pgmStr.rowThru = 0;
	pgmStr.numInstr = 0;
	pgmStr.rowNumStart = 0;
	pgmStr.rowNumEnd = 0;
	pgmStr.inCopy = "";
	pgmStr.levelNesting = 0;	   		
	al_pgmStr.add(pgmStr);	
	
	return;
}

// Check if there are perform statements inside
private boolean isParagraphSectionWithInstrToShow(ProgramCobolEntry<? extends Instruction>[] arProcEntries, int numInstrFrom, int numInstrThru) {
	ProgramCobolEntry<? extends Instruction> entryProcedure = null;
	EnumCobolReservedWords typeInstr = null;
	 
	for (int i = numInstrFrom; i < numInstrThru; i++) {
        entryProcedure = arProcEntries[i];          
        typeInstr = entryProcedure.getTypeInstr();
   		if (typeInstr == EnumCobolReservedWords.PROC_PERFORM
   		||  typeInstr == EnumCobolReservedWords.PROC_CALL		
   		||  typeInstr == EnumCobolReservedWords.PROC_CANCEL		
   		||  typeInstr == EnumCobolReservedWords.DIR_COMPILER_COPY		
   		||  typeInstr == EnumCobolReservedWords.PRECOMPILER_SQL		
   		||  typeInstr == EnumCobolReservedWords.PRECOMPILER_CICS) {
   			return true;	
    		}      
	}
	return false;
}

// Check for stop run / goback / exit program / Cics return transid
private boolean isParagraphSectionWithStopRun(ProgramCobolEntry<? extends Instruction>[] arProcEntries, int numInstrFrom, int numInstrThru) {
	boolean isExitStmt = false;
	
	ProgramCobolEntry<? extends Instruction> entryProcedure = null;
	EnumCobolReservedWords typeInstr = null;
	
	for (int i = numInstrFrom; i < numInstrThru; i++) {
        entryProcedure = arProcEntries[i];          
        typeInstr = entryProcedure.getTypeInstr();
		if (typeInstr == EnumCobolReservedWords.PROC_STOP
		||  typeInstr == EnumCobolReservedWords.PROC_GOBACK
		||  typeInstr == EnumCobolReservedWords.PROC_EXIT) {
			isExitStmt = true;
			break;				 
		}      
	}
	
	return isExitStmt;
}

// Find the next label instruction
private int getLabelNumInstr(int numInstrFrom, ProgramCobolEntry<? extends Instruction>[] arProcEntries) {
	ProgramCobolEntry<? extends Instruction> entryProcedure = null;
	EnumCobolReservedWords typeInstr = null;
	
	for (int i = numInstrFrom + 1; i < arProcEntries.length; i++) {
        entryProcedure = arProcEntries[i];          
        typeInstr = entryProcedure.getTypeInstr();
   		if (typeInstr == EnumCobolReservedWords.PROC_LABEL) {
   			return i;	
   		}      
	}
	return numInstrFrom;
}

// Get the label name
private String getLabelName(int numInstrThru, ProgramCobolEntry<? extends Instruction>[] arProcEntries) {
	ProgramCobolEntry<? extends Instruction> entryProcedure = null;
	InstructionCobolProcedure instrLabel = null;
	entryProcedure = arProcEntries[numInstrThru];
	instrLabel = (InstructionCobolProcedure) entryProcedure.getInstruction();
	return instrLabel.labelGetName();
}


/*
 * INSPECTOR
 * 
 * Retrieve program/Label sections
 */
@GET  
@Produces("application/json")
@Path("programLabels/{user}/{idProgram}")  
public Response getProgramLabelsSections(
		  @PathParam("user")         String user
		, @PathParam("idProgram")     String idProgram 

		) throws JSONException, SQLException, ExceptionAmrita {

 
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction> procEntry = null;
	Instruction instr = null;
	JSONObject jsonObject = null; 
	JSONArray jsonArray = null; 		
	List<LabelSection> alLabelSection = null;
	LabelSection labelSection = null;
	String[] arLabels = null;;
	String[] arSections = null;;
	String resultJson = "";
	
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			

	alLabelSection = new ArrayList<LabelSection>();
	arLabels = programCobol.labelNames();
	arSections = programCobol.sectionNames();
	
	// Labels
	for (String label : arLabels) {
		labelSection = new LabelSection();
		labelSection.name = label;
		labelSection.isSection = false;
		labelSection.numInstr = programCobol.labelPointer(label);
		procEntry = programCobol.entryProcedure(labelSection.numInstr);
	    instr = procEntry.getInstruction();
	    labelSection.rowStart = instr.getRowStartSource();
	    labelSection.rowEnd = instr.getRowEndSource();
	    labelInCopyManagement(labelSection, programCobol, procEntry);
		alLabelSection.add(labelSection);
	}
	
	// Sections
	for (String section : arSections) {
		labelSection = new LabelSection();
		labelSection.name = section;
		labelSection.isSection = true;
		labelSection.numInstr = programCobol.labelPointer(section);
	    labelSection.rowStart = instr.getRowStartSource();
	    labelSection.rowEnd = instr.getRowEndSource();
	    labelInCopyManagement(labelSection, programCobol, procEntry);
		alLabelSection.add(labelSection);
	}
	Collections.sort(alLabelSection);

	// No data
	jsonArray = new JSONArray();

	// Populate Json
	for (LabelSection o : alLabelSection) {
		jsonObject = new JSONObject(o);
		jsonObject.put("name", o.name);
		jsonObject.put("isSection", o.isSection);
		jsonObject.put("numInstr", o.numInstr);
		jsonObject.put("rowStart", o.rowStart);
		jsonObject.put("rowEnd", o.rowEnd);
		jsonObject.put("inCopy", o.inCopy);
		jsonObject.put("copyInstr", o.copyInstr);
		jsonObject.put("copyRowStart", o.copyRowStart);
		jsonObject.put("copyRowEnd", o.copyRowEnd);
		jsonArray.put(jsonObject);
	}

	// Return json object
	resultJson = jsonArray.toString();		
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();

}

/* Se Label in copy estrae nome copy e numero riga sorgente label in copy */
private void labelInCopyManagement(LabelSection labelSection, ProgramCobol programCobol, ProgramCobolEntry<? extends Instruction> procEntry) {
	int i = 0;
//	if (procEntry.getUnderCopyName().isBlank()) {return;}
	if (procEntry.getUnderCopyName().trim().length() == 0) {return;}
	labelSection.inCopy = procEntry.getUnderCopyName();
    labelSection.copyRowStart = procEntry.getInstruction().getRowStartSource();
    labelSection.copyRowEnd = procEntry.getInstruction().getRowEndSource();
	
	// Position on copy statement
	for (i = labelSection.numInstr - 1; i < 1; i--) {
	    procEntry = programCobol.entryProcedure(i);
	    if (procEntry.getInstruction().getTypeInstr() != EnumCobolReservedWords.DIR_COMPILER_COPY) {
			continue;
		}
	    // Copy dove la label si trova  
	    labelSection.copyInstr = procEntry.getInstruction().getNumInstr();
	    break;
	}
}


/*
 * INSPECTOR
 * 
 * Retrieve execution paths thru LogicTool services
 * Only the minimal perform stack EXPAND_PERFORM_STACK_ONLY will be generated  
 * All information to get instruction number/source row of perform and paragraph called are returned to the caller.
 * Extracted paths are cached to be used by getprogramPathShow() web service
 */
@GET  
@Produces("application/json")
@Path("programPaths/{user}/{idProgram}/{idFromNumInstr}/{idToNumInstr}")  
public Response getProgramPaths(
							      @PathParam("user")         	String user
								, @PathParam("idProgram")    	String idProgram 
								, @PathParam("idFromNumInstr")  String idFromNumInstr 
								, @PathParam("idToNumInstr")    String idToNumInstr						
							) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramPath paths[] = null;
	LogicTools logicTools = null;
	JSONArray jsonArray = null; 		
	JSONObject jsonObjectPath = null;			 				 		
	String resultJson = "";	
	String keyPaths = "";
	int fromNumInstr = 0;
	int toNumInstr = 0;
	 	
	// Login should be done
	// If not take default properties	 
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
 
	// Get object program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
	logicTools = new LogicTools(programCobol, false, ucfg);

	fromNumInstr = Integer.parseInt(idFromNumInstr);
	toNumInstr = Integer.parseInt(idToNumInstr);

	// Minimal paths generation.
	// Specific path will be expanded when required
	paths = logicTools.paths(programCobol, fromNumInstr, toNumInstr, LogicTools.EXPAND_PERFORM_STACK_ONLY);
	keyPaths = idProgram+"_PATHS_"  + LogicTools.EXPAND_PERFORM_STACK_ONLY;
	AmritaStartup.cache.put(keyPaths, paths);

    // creazione json con struttura paths
	jsonArray = new JSONArray();

	// Scan paths
	for (ProgramPath path : paths) {
		
		jsonObjectPath = new JSONObject();
		jsonObjectPath.put("idPath", path.getIdPath());
		jsonObjectPath.put("fromInstr", path.getFromInstr());
		jsonObjectPath.put("toInstr", path.getToInstr());	
		
		jsonArray.put(jsonObjectPath);
	}
	
	// Return json object
	resultJson = jsonArray.toString();		
	    	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


/*
 * INSPECTOR
 * 
 * 1) Expand supplied path
 *    EXPAND_PERFORM_STACK_ONLY = "S";             // Minimal already available
 *    EXPAND_PERFORM_TO_PARAGRAPH = "P";		   // + instructions from begin paragraph to perform
 *    EXPAND_PERFORM_DETAIL_ANY_LEVEL = "D";	   // + expanded perform at any deep level  
 * 2) Retrieve fields in Input or Output 
 */
@GET  
@Produces("application/json")
@Path("programPathShow/{user}/{idProgram}/{pathFrom}/{pathTo}/{numPathToDeal}/{typeExpand}")  
public Response getprogramPathShow(
							      @PathParam("user")         	String user
								, @PathParam("idProgram")    	String idProgram 
								, @PathParam("numPathToDeal")   String numPathToDeal	// Numero Path to considerare (0-based)	
								, @PathParam("typeExpand") 		String typeExpand	    // S/P/D	
							) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramPath paths[] = null;
	ProgramPath path = null;
	ProgramPathEntry pathEntries[] = null;
	ProgramPathEntry ppe = null;
	ProgramPathFieldIO  arPathFieldIO[] = null;
	LogicTools logicTools = null;
	JSONObject jsonObjectPath = null;
	JSONObject jsonObjectPathEntry = null; 
	JSONArray jsonArrayPathEntry = null; 		
	JSONArray jsonArray = null; 
	JSONArray jsonArrayXref = null; 		 		
	JSONObject jsonObjectXref = null;  		
	String resultJson = "";	
	String keyPaths = "";
	int numPath = 0;
	
	// Login should be done
	// If not take default properties	 
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
 
	// Get object program 
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
	logicTools = new LogicTools(programCobol, false, ucfg);
	
	numPath = Integer.parseInt(numPathToDeal);
	
	// Get minimal paths already generated and re-generate paths
	keyPaths = idProgram +"_PATHS_" + LogicTools.EXPAND_PERFORM_STACK_ONLY;
	paths =  (ProgramPath[]) AmritaStartup.cache.get(keyPaths);
	path = paths[numPath];	
	paths = logicTools.paths(programCobol, path.getFromInstr(), path.getToInstr(), LogicTools.EXPAND_PERFORM_STACK_ONLY);
 	path = paths[numPath];	
	
    // Richiesto maggiore dettaglio path
    if (!typeExpand.equals(LogicTools.EXPAND_PERFORM_STACK_ONLY) ) {   	
    	path = logicTools.pathExpand(programCobol, path, typeExpand);
	}
    
    ///////////////////////////////////////////
    // Info path e istruzioni path    
    ///////////////////////////////////////////

	jsonObjectPath = new JSONObject();
	jsonObjectPath.put("idPath", path.getIdPath());
	jsonObjectPath.put("fromInstr", path.getFromInstr());
	jsonObjectPath.put("toInstr", path.getToInstr());		

    pathEntries = path.getEntries();	
	jsonArrayPathEntry = new JSONArray();

	// Scan instructions in path
	for (int i = 0; i < pathEntries.length; i++) {
		
		ppe = pathEntries[i];
		
		jsonObjectPathEntry = new JSONObject();			
		jsonObjectPathEntry.put("lvlDeep", ppe.getLvlDeep());
		jsonObjectPathEntry.put("sourceInstr", ppe.getSourceInstr());
		jsonObjectPathEntry.put("isPerform", ppe.isPerform());
		jsonObjectPathEntry.put("numInstr", ppe.getNumInstr());
		jsonObjectPathEntry.put("rowStartSource", ppe.getRowStartSource());
		jsonObjectPathEntry.put("rowEndSource", ppe.getRowEndSource());
		jsonObjectPathEntry.put("underCopyName", ppe.getUnderCopyName());
		jsonObjectPathEntry.put("numInstrIfOwner", ppe.getNumInstrIfOwner());
		jsonObjectPathEntry.put("numInstrStartParagrah", ppe.getNumInstrStartParagrah());
		jsonObjectPathEntry.put("numInstrEndParagrah", ppe.getNumInstrEndParagrah());
		jsonObjectPathEntry.put("rowStartParagrah", ppe.getRowStartParagrah());
		jsonObjectPathEntry.put("rowEndParagrah", ppe.getRowEndParagrah());
		jsonObjectPathEntry.put("idParagraph", ppe.getIdParagrah());
		jsonObjectPathEntry.put("idParagraphThru", ppe.getIdParagrahThru());
		jsonObjectPathEntry.put("paragraphUnderCopy", ppe.getParagraphUnderCopy());
		
		jsonArrayPathEntry.put(jsonObjectPathEntry);		
	}
	
	jsonObjectPath.put("arPathEntry", jsonArrayPathEntry);

    ///////////////////////////////////////////
    // Fields I/O    
    ///////////////////////////////////////////
	
	// Campi in I/O path richiesto
	arPathFieldIO = logicTools.pathFieldsIO(path);
	
    // creazione jsons con array utilizzi campi I/O
	jsonArray = new JSONArray();

	// Scan Xref fields
	for (ProgramPathFieldIO pathFieldIO : arPathFieldIO) {
		
		jsonObjectXref = new JSONObject();
		jsonObjectXref.put("fieldName", pathFieldIO.getFieldName());
		jsonObjectXref.put("numInstrDef", pathFieldIO.getNumInstrDef());
		jsonObjectXref.put("section", pathFieldIO.getSection());
		
		// Scan Xref input
		jsonArrayXref = new JSONArray();
		for (Integer xrefNum : pathFieldIO.getXrefInput()) {
			jsonArrayXref.put(xrefNum);
		}
		jsonObjectXref.put("arXrefInput", jsonArrayXref);	
		
		// Scan Xref input
		jsonArrayXref = new JSONArray();
		for (Integer xrefNum : pathFieldIO.getXrefOutput()) {
			jsonArrayXref.put(xrefNum);
		}
		jsonObjectXref.put("arXrefOutput", jsonArrayXref);
		
		jsonArray.put(jsonObjectXref);
	}
	
	jsonObjectPath.put("arFieldsIO", jsonArray);
	
	
	// Return json object
	resultJson = jsonObjectPath.toString();		
	    	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * INSPECTOR
 * 
 * Retrieve the dead code from the program
 * 
 */
@GET  
@Produces("application/json")
@Path("programDeadCode/{user}/{idObject}")  
public Response getProgramDeadCode(
									  @PathParam("user")         String user
									, @PathParam("idObject")     String idProgram 
									) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	// Arrays dead code
    ArrayList<String> al_deadCodeCopyDataCD = null;
    ArrayList<String> al_deadCodeCopyProcCP = null;
    ArrayList<Integer> al_deadCodeDataItemNumbersDD = null;
    ArrayList<Integer> al_deadCodeInstrNumbersIP = null;
    ArrayList<Integer> al_deadCodeUnreachableUP = null;
    ArrayList<String> al_deadCodeLabelsLB = null;
    ArrayList<String> al_deadCodeParagraphsPH = null;
    ArrayList<String> al_deadCodeSectionsSE = null;
    // Arrays dead code (istruzioni/nomi campi)
    ArrayList<Integer> al_deadCodeCopyDataCD_N = null;
    ArrayList<Integer> al_deadCodeCopyProcCP_N = null;
    ArrayList<String> al_deadCodeDataItemNumbersDD_X = null;
    ArrayList<Integer> al_deadCodeLabelsLB_N = null;
    ArrayList<Integer> al_deadCodeParagraphsPH_N = null;
    ArrayList<Integer> al_deadCodeSectionsSE_N = null;
    
    
    Integer ar_num[] = null;
    String ar_string[] = null;
	JSONObject jsonObject = null; 
	String resultJson = "";

	// Allocazione arrays 
    al_deadCodeCopyDataCD = new ArrayList<String>();
    al_deadCodeCopyProcCP =  new ArrayList<String>();
    al_deadCodeDataItemNumbersDD =  new ArrayList<Integer>();
    al_deadCodeInstrNumbersIP = new ArrayList<Integer>();
    al_deadCodeUnreachableUP = new ArrayList<Integer>();
    al_deadCodeLabelsLB =  new ArrayList<String>();
    al_deadCodeParagraphsPH =  new ArrayList<String>();
    al_deadCodeSectionsSE =  new ArrayList<String>();
    // Allocazione arrays 
    al_deadCodeCopyDataCD_N = new ArrayList<Integer>();
    al_deadCodeCopyProcCP_N =  new ArrayList<Integer>();
    al_deadCodeDataItemNumbersDD_X =  new ArrayList<String>();   
    al_deadCodeLabelsLB_N =  new ArrayList<Integer>();    
    al_deadCodeParagraphsPH_N =  new ArrayList<Integer>();
    al_deadCodeSectionsSE_N =  new ArrayList<Integer>();

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
    
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			

    al_deadCodeCopyDataCD = programCobol.deadCodeCopyData();
    al_deadCodeCopyProcCP = programCobol.deadCodeCopyProc();
    al_deadCodeDataItemNumbersDD = programCobol.deadCodeDataItemNumbers();    
    al_deadCodeInstrNumbersIP = programCobol.deadCodeInstrNumbers();   
    al_deadCodeUnreachableUP = programCobol.deadCodeUnreachable();
    al_deadCodeLabelsLB = programCobol.deadCodeLabels();
    al_deadCodeParagraphsPH = programCobol.deadCodeParagraphs();
    al_deadCodeSectionsSE = programCobol.deadCodeSections();
    
    // Impostazione numeri istruzione/nomi campi
    al_deadCodeCopyDataCD_N = getCopyDataInstr(programCobol, al_deadCodeCopyDataCD);              // Numero definizione copy Data division
    al_deadCodeCopyProcCP_N = getCopyProcInstr(programCobol, al_deadCodeCopyProcCP);              // Numero definizione copy proc division
    al_deadCodeDataItemNumbersDD_X = getDataName(programCobol, al_deadCodeDataItemNumbersDD);     // Nome campo in numero definizione    
    al_deadCodeLabelsLB_N = getProcNumInstr(programCobol, al_deadCodeLabelsLB); 
    al_deadCodeParagraphsPH_N = getProcNumInstr(programCobol, al_deadCodeParagraphsPH); 
    al_deadCodeSectionsSE_N = getProcNumInstr(programCobol, al_deadCodeSectionsSE); 
      
    // creazione json con struttura programma
	jsonObject = new JSONObject();
	
	// Populate Json
	
	//1 Copy Data Division
	ar_string = new String[al_deadCodeCopyDataCD.size()] ;
	ar_string = al_deadCodeCopyDataCD.toArray(ar_string);
	jsonObject.put("arDeadCodeCopyDataCD", ar_string);
	
	//2 Copy Data Procedure
	ar_string = new String[al_deadCodeCopyProcCP.size()] ;
	ar_string = al_deadCodeCopyProcCP.toArray(ar_string);	
	jsonObject.put("arDeadCodeCopyProcCP", ar_string);
	
	//3 Data Division Item Numbers
	ar_num = new Integer[al_deadCodeDataItemNumbersDD.size()] ;
	ar_num = al_deadCodeDataItemNumbersDD.toArray(ar_num);	
	jsonObject.put("arDeadCodeDataItemNumbersDD", ar_num);
	
	//4 Procedure Division Instruction Numbers
	ar_num = new Integer[al_deadCodeInstrNumbersIP.size()] ;
	ar_num = al_deadCodeInstrNumbersIP.toArray(ar_num);	
	jsonObject.put("arDeadCodeInstrNumbersIP", ar_num);
	
	//5 Procedure Division Not reacheble Instruction Numbers
	ar_num = new Integer[al_deadCodeUnreachableUP.size()] ;
	ar_num = al_deadCodeUnreachableUP.toArray(ar_num);	
	jsonObject.put("ardeadCodeUnreachableUP", ar_num);

	//6 Procedure Division Labels
	ar_string = new String[al_deadCodeLabelsLB.size()] ;
	ar_string = al_deadCodeLabelsLB.toArray(ar_string);	
	jsonObject.put("arDeadCodeLabelsLB", ar_string);
	
	//7 Procedure Division Paragraphs
	ar_string = new String[al_deadCodeParagraphsPH.size()] ;
	ar_string = al_deadCodeParagraphsPH.toArray(ar_string);	
	jsonObject.put("arDeadCodeParagraphsPH", ar_string);
	
	//8 Procedure Division Sections
	ar_string = new String[al_deadCodeSectionsSE.size()] ;
	ar_string = al_deadCodeSectionsSE.toArray(ar_string);	
	jsonObject.put("arDeadCodeSectionsSE", ar_string);	

	//9(1) Copy Data Division item number
	ar_num = new Integer[al_deadCodeCopyDataCD_N.size()] ;
	ar_num = al_deadCodeCopyDataCD_N.toArray(ar_num);
	jsonObject.put("arDeadCodeCopyDataCD_N", ar_num);
	
	//10(2) Copy Data Procedure item number
	ar_num = new Integer[al_deadCodeCopyProcCP_N.size()] ;
	ar_num = al_deadCodeCopyProcCP.toArray(ar_num);	
	jsonObject.put("arDeadCodeCopyProcCP_N", ar_num);

	//11(3) Data Division Item name
	ar_string = new String[al_deadCodeDataItemNumbersDD_X.size()] ;
	ar_string = al_deadCodeDataItemNumbersDD_X.toArray(ar_string);	
	jsonObject.put("arDeadCodeDataItemNumbersDD_X", ar_string);

	//12(6) Procedure Division Labels instr number
	ar_num = new Integer[al_deadCodeLabelsLB_N.size()] ;
	ar_num = al_deadCodeLabelsLB_N.toArray(ar_num);	
	jsonObject.put("arDeadCodeLabelsLB_N", ar_num);
	
	//13(7) Procedure Division Paragraphs instr number
	ar_num = new Integer[al_deadCodeParagraphsPH_N.size()] ;
	ar_num = al_deadCodeParagraphsPH_N.toArray(ar_num);	
	jsonObject.put("arDeadCodeParagraphsPH_N", ar_num);

	//14(8) Procedure Division Section instr number
	ar_num = new Integer[al_deadCodeSectionsSE_N.size()] ;
	ar_num = al_deadCodeSectionsSE_N.toArray(ar_num);	
	jsonObject.put("arDeadCodeParagraphsSE_N", ar_num);

	// Return json object
	resultJson = jsonObject.toString();		
	    
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

// Numero istruzione definizione copy Data
private ArrayList<Integer> getCopyDataInstr(ProgramCobol programCobol, ArrayList<String> al_deadCodeCopyDataCD) {
	ArrayList<Integer> al_deadCodeCopyDataCD_N = null;
	ProgramCobolEntry<? extends Instruction>[] entriesData = programCobol.entriesData();
	ProgramCobolEntry<? extends Instruction> entryData = null;
	String copyName = "";
	String copyNameData = ""; 
	int numDef = 0;

	al_deadCodeCopyDataCD_N = new ArrayList<Integer>();
	
	// Scan nomi copy in input
	for (int i = 0; i < al_deadCodeCopyDataCD.size(); i++) {
		copyName = al_deadCodeCopyDataCD.get(i);
		// Scan data entries
		for (int i2 = 0; i2 < entriesData.length; i2++) {
			entryData = entriesData[i2];
			copyNameData = entryData.getUnderCopyName();
			// Prima definizione dentro copy
//			if (!copyName.isBlank() && copyName.equals(copyNameData)) {
			if (copyName.trim().length() != 0 && copyName.equals(copyNameData)) {
				numDef = i2 - 1;
				al_deadCodeCopyDataCD_N.add(numDef);
				i2 = entriesData.length;
			}
		}
	}	
	return al_deadCodeCopyDataCD_N;
}

// Numero istruzione definizione copy Proc
private ArrayList<Integer> getCopyProcInstr(ProgramCobol programCobol, ArrayList<String> al_deadCodeCopyDataCP) {
	ArrayList<Integer> al_deadCodeCopyDataCP_N = new ArrayList<Integer>(al_deadCodeCopyDataCP.size());
	ProgramCobolEntry<? extends Instruction>[] entriesData = programCobol.entriesProcedure();
	ProgramCobolEntry<? extends Instruction> entryData = null;
	String copyName = "";
	String copyNameProc = ""; 

	// Scan nomi copy in input
	for (int i = 0; i < al_deadCodeCopyDataCP.size(); i++) {
		copyName = al_deadCodeCopyDataCP.get(i);
		// Scan data entries
		for (int i2 = 0; i < entriesData.length; i2++) {
			entryData = entriesData[i2];
			copyNameProc = entryData.getUnderCopyName();
			// Prima definizione dentro copy
//			if (!copyName.isBlank() && copyName.equals(copyNameProc)) {
			if (copyName.trim().length() != 0 && copyName.equals(copyNameProc)) {
				al_deadCodeCopyDataCP_N.set(i, i2 - 1);
			}
		}
	}	
	return al_deadCodeCopyDataCP_N;
}

// Numero istruzione definizione Label/Section/Paragrafo
private ArrayList<Integer> getProcNumInstr(ProgramCobol programCobol, ArrayList<String> al_deadCodeLabelsLB) {
	ArrayList<Integer> al_deadCodeLabelsLB_N = new ArrayList<Integer>();
	Integer numDefinition = 0;	
	// Numero definizione
	for (String labelSection : al_deadCodeLabelsLB) {
		numDefinition = programCobol.labelPointer(labelSection);
		if (numDefinition < 0) {
			numDefinition = programCobol.sectionPointer(labelSection);
		}
		al_deadCodeLabelsLB_N.add(numDefinition);
	}	
	return al_deadCodeLabelsLB_N;
}

// Nome campo data a un certo numero istruzione
private ArrayList<String> getDataName(ProgramCobol programCobol, ArrayList<Integer> al_deadCodeDataItemNumbersDD) {
	ArrayList<String> al_deadCodeDataItemNumbersDD_X = new ArrayList<String>();
	String dataName = "";	
	// Nome campo
	for (Integer numDefinition : al_deadCodeDataItemNumbersDD) {
		dataName = programCobol.dataItemName(numDefinition);
		al_deadCodeDataItemNumbersDD_X.add(dataName);
	}	
	return al_deadCodeDataItemNumbersDD_X;
}


/*
 * INSPECTOR
 * 
 * Retrieve copy programs and sql include.
 * All copy are returned with the Division (I,E, D, P) and Type (C, I) for copy or include
 */
@GET  
@Produces("application/json")
@Path("programCopyInclude/{user}/{idProgram}/{division}/{typeCopy}")  
public Response getProgramCopySqlIncludeList(
										    @PathParam("user")       String user
										   ,@PathParam("idProgram")  String idProgram
										   ,@PathParam("division")   String division
										   ,@PathParam("typeCopy")   String typeCopy 
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	List<ProgramCopyInclude> al_pgmCopyInclude = null;
	JSONArray jsonArray = null; 		
	JSONObject jsonObject = null; 
	String resultJson = "";
	
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
 

    al_pgmCopyInclude = new ArrayList<ProgramCopyInclude> ();

    if (division.equals("I") || division.equals("*")) {
        programCopyIncludeExtract(division, typeCopy, programCobol.entriesIdentification(), al_pgmCopyInclude);
	}
    if (division.equals("E") || division.equals("*")) {
        programCopyIncludeExtract(division, typeCopy, programCobol.entriesEnvironment(), al_pgmCopyInclude);
	}
    if (division.equals("D") || division.equals("*")) {
        programCopyIncludeExtract(division, typeCopy, programCobol.entriesData(), al_pgmCopyInclude);
	}
    if (division.equals("P") || division.equals("*")) {
        programCopyIncludeExtract(division, typeCopy, programCobol.entriesProcedure(), al_pgmCopyInclude);
	}
    
	al_pgmCopyInclude.sort(null);
    
    // creazione json con struttura programma
 	jsonArray = new JSONArray();
 	
 	// Populate Json
 	for (ProgramCopyInclude oPgmStr : al_pgmCopyInclude) {
 		jsonObject = new JSONObject();
 		jsonObject.put("type", oPgmStr.type);
		jsonObject.put("name", oPgmStr.name);
		jsonObject.put("division", oPgmStr.division);
		jsonObject.put("numInstr", oPgmStr.numInstr);
		jsonObject.put("rowStart", oPgmStr.rowStart);
		jsonObject.put("rowEnd", oPgmStr.rowEnd);
 		jsonArray.put(jsonObject);
 	}	
 	
	// Return json object
	resultJson = jsonArray.toString();		
		
	return Response
			.status(200)
			.entity(resultJson)
			.build();}

private void programCopyIncludeExtract(String division, String typeCopy, ProgramCobolEntry<? extends Instruction>[] ar_entryDivision, List<ProgramCopyInclude> al_pgmCopyInclude) {
	ProgramCopyInclude programCopyInclude = null;
	ProgramCobolEntry<? extends Instruction> entryDivision = null;
	Instruction instr = null;
	InstructionCobol instrCobol = null;
	InstructionSql instrSql = null;
	String idCopyInclude = "";
	
    // Ricerca copy/include in division  
	for (int i = 0; i < ar_entryDivision.length; i++) {	
		entryDivision = ar_entryDivision[i];
		if (division.contentEquals("*") 
		|| (division.contentEquals("I") && entryDivision.getProgramDivision() == EnumCobolReservedWords.ID_DIVISION)
		|| (division.contentEquals("E") && entryDivision.getProgramDivision() == EnumCobolReservedWords.ENV_DIVISION)
		|| (division.contentEquals("D") && entryDivision.getProgramDivision() == EnumCobolReservedWords.DATA_DIVISION)
		|| (division.contentEquals("P") && entryDivision.getProgramDivision() == EnumCobolReservedWords.PROC_DIVISION)) {
			instr = entryDivision.getInstruction();
			if (entryDivision.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY ){
				programCopyInclude = new ProgramCopyInclude();
				programCopyInclude.division = entryDivision.getProgramDivision().getValueText1().substring(0, 1) ;
				if (typeCopy.equals("*") || typeCopy.equals("C")) {
					instrCobol = (InstructionCobol) entryDivision.getInstruction();
					idCopyInclude = instrCobol.copyGetName();
					programCopyInclude.type = "C";
					programCopyInclude.name = idCopyInclude;
					programCopyInclude.numInstr = instrCobol.getNumInstr();
					programCopyInclude.rowStart = instrCobol.getRowStartSource();
					programCopyInclude.rowEnd = instrCobol.getRowEndSource();
				    al_pgmCopyInclude.add(programCopyInclude);
				    continue;
				}
			}

			if (entryDivision.getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL 
			&& instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE){
				programCopyInclude = new ProgramCopyInclude();
				programCopyInclude.division = entryDivision.getProgramDivision().getValueText1().substring(0, 1) ;
				programCopyInclude.name = idCopyInclude;
				instrSql = (InstructionSql) entryDivision.getInstruction();
				idCopyInclude = instrSql.sqlIncludeGetName();
				programCopyInclude.type = "I";
				programCopyInclude.name = idCopyInclude;
				programCopyInclude.numInstr = instrSql.getNumInstr();
				programCopyInclude.rowStart = instrSql.getRowStartSource();
				programCopyInclude.rowEnd = instrSql.getRowEndSource();
				al_pgmCopyInclude.add(programCopyInclude);	
				
	  		} // end-if 
		} // end-if
	}  // end-for 
	
}


/*
 * LOGIC
 * 
 * Retrieve program Dynamic instructions
 * Every instruction describes the field of which to find values
 * It's possible to have more then one field in a dynamic instruction
 */
@GET  
@Produces("application/json")
@Path("programDynamicInstr/{user}/{sys}/{subSys}/{idObject}")  
public Response getProgramDynamicInstr(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @PathParam("subSys")       String subSys
										, @PathParam("idObject")     String idProgram 
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	List<EntityDynamicField> ls_dynamicField = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	String resultJson = "";
	
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Initial
	jsonArray = new JSONArray();

	Connection conn = DataBaseConnections.getConnection();
	IDAODynamicField efDAO = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false, false, ucfg);
 
    // Get dynamicFields  
    ls_dynamicField=efDAO.findAll(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL);
    for (EntityDynamicField edf : ls_dynamicField) {
    	jsonObject = new JSONObject();
    	
    	putJsonDynamicInstr(jsonObject, edf);
    	
   	    jsonArray.put(jsonObject);
	}
   
	// Close connection
	DataBaseConnections.releaseConnection(conn);
	efDAO.setConn(null);

	resultJson = jsonArray.toString();
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Caricamento oggetto Json con dati da db
 */
private void putJsonDynamicInstr(JSONObject jsonObject, EntityDynamicField edf) {
  	jsonObject.put("numInstr", edf.getNumInstr());   	
  	jsonObject.put("idField", edf.getIdField());   	
	jsonObject.put("numField", edf.getNumField());
   	jsonObject.put("isLight", edf.getLight());
  	jsonObject.put("isSolved", edf.getSolved());
  	jsonObject.put("isSolvedFull", edf.getSolvedFull());
   	jsonObject.put("isSpread", edf.getSpreaded());
 	jsonObject.put("isWaitingForData", edf.getWaitingForData());
  	jsonObject.put("instrCobolType", edf.getInstrCobolType());
    jsonObject.put("instrPrecompType", edf.getInstrPrecompType());
   	jsonObject.put("instrPrecompOprndType", edf.getInstrPrecompOprndType());
   	jsonObject.put("instrCobolTypeOrdinal", edf.getInstrCobolType().ordinal());
    jsonObject.put("instrPrecompTypeOrdinal", edf.getInstrPrecompType().ordinal());
    jsonObject.put("instrPrecompOprndTypeOrdinal", edf.getInstrPrecompOprndType().ordinal()); 
}

/*
 * LOGIC
 * 
 * Retrieve program Dynamic subFields
 * Given a dynamic instruction number and a field name
 * all subFields are returned
 */
@GET  
@Produces("application/json")
@Path("programDynamicSubFields/{user}/{sys}/{subSys}/{idObject}/{numInstr}/{idField}")  
public Response getProgramDynamicSubFields(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @PathParam("subSys")       String subSys
										, @PathParam("idObject")     String idProgram 
										, @PathParam("numInstr")     String numInstrX 
										, @PathParam("idField")      String idField 
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	List<EntityDynamicFieldSub> ls_dynamicFieldSub = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	String resultJson = "";
	int numInstr = 0;

	numInstr = Integer.parseInt(numInstrX);

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Initial
	Connection conn = DataBaseConnections.getConnection();
 	IDAODynamicFieldSub esDAO = (DAOImplDynamicFieldSub) AmritaStartup.sqlFactory.getDAODynamicFieldSub(conn, false, false, ucfg);
	ls_dynamicFieldSub=esDAO.findAll(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, numInstr, idField);
	
	jsonArray = new JSONArray();
	 	
   	// Get dynamicSubFields 
	for (EntityDynamicFieldSub eds  : ls_dynamicFieldSub) {
		
		jsonObject = new JSONObject();
		
		putJsonDynamicSubField(jsonObject, eds);
		
		jsonArray.put(jsonObject);
	}

 	// Close connection
	DataBaseConnections.releaseConnection(conn);
	esDAO.setConn(null);

	resultJson = jsonArray.toString();
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Caricamento oggetto Json con dati da db
 */
private void putJsonDynamicSubField(JSONObject jsonObject, EntityDynamicFieldSub eds) {
	jsonObject.put("idObject", eds.getIdObject());
	jsonObject.put("numInstr", eds.getNumInstr());
	jsonObject.put("idField", eds.getIdField());
	jsonObject.put("idSubField", eds.getIdSubField());
	jsonObject.put("numField", eds.getNumField());
	jsonObject.put("numSubField", eds.getNumSubField());
	jsonObject.put("sizeSubField", eds.getSizeSubField());
	jsonObject.put("posSubField", eds.getPosSubField());
	jsonObject.put("typeSubField", eds.getTypeSubField());
	jsonObject.put("light", eds.getLight());
	jsonObject.put("solved", eds.getSolved());
	jsonObject.put("waitingForData", eds.getWaitingForData());
	jsonObject.put("spreaded", eds.getSpreaded());  
	jsonObject.put("typeSubFieldOrdinal", eds.getTypeSubField().ordinal());		
}		

/*
 * LOGIC
 * 
 * Retrieve program Dynamic values of a field or subfield from database.
 * If the supplied subField is space all field values will be returned
 * otherwise all subField values.
 */
@GET  
@Produces("application/json")
@Path("programDynamicValues/{user}/{sys}/{subSys}/{idObject}/{numInstr}/{idField}/{idSubField}")  
public Response getProgramDynamicValues(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @PathParam("subSys")       String subSys
										, @PathParam("idObject")     String idProgram 
										, @PathParam("numInstr")     String numInstrX 
										, @PathParam("idField")      String idField 
										, @PathParam("idSubField")   String idSubField 
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	List<EntityDynamicFieldSubValue> ls_dynamicFieldSubValue = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	String resultJson = "";
	int numInstr = 0;

	// Normalize input
	numInstr = Integer.parseInt(numInstrX);
    if (idSubField.equals("*")) {
    	idSubField = "";
	}
    
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Initial
	Connection conn = DataBaseConnections.getConnection();
 	DAOImplDynamicFieldSubValue esDAO = (DAOImplDynamicFieldSubValue) AmritaStartup.sqlFactory.getDAODynamicFieldSubValue(conn, false, false, ucfg);
 	ls_dynamicFieldSubValue=esDAO.findAll(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, numInstr, idField, idSubField);
	
	jsonArray = new JSONArray();
	 	
   	// Get dynamicSubValue
	for (EntityDynamicFieldSubValue edv  : ls_dynamicFieldSubValue) {
		
		jsonObject = new JSONObject();
		
		putJsonDynamicValue(jsonObject, edv);
				
		jsonArray.put(jsonObject);
	}

 	// Close connection
	DataBaseConnections.releaseConnection(conn);
	esDAO.setConn(null);

	resultJson = jsonArray.toString();
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Caricamento oggetto Json con dati da db
 */
private void putJsonDynamicValue(JSONObject jsonObject, EntityDynamicFieldSubValue edv) {
	jsonObject.put("idObject", edv.getIdObject());
	jsonObject.put("numInstr", edv.getNumInstr());
	jsonObject.put("idField", edv.getIdField());
	jsonObject.put("idSubField", edv.getIdSubField());
	jsonObject.put("progr", edv.getProgr());
	jsonObject.put("value", edv.getValue());
	jsonObject.put("posInSubField", edv.getPosInSubField());
	jsonObject.put("lngInSubField", edv.getLngInSubField());
	jsonObject.put("typeObjectFrom", edv.getTypeObjectFrom());
	jsonObject.put("idObjectFrom", edv.getIdObjectFrom());
	jsonObject.put("numInstrFrom", edv.getNumInstrFrom());
	jsonObject.put("typeObjectFromOrdinal", edv.getTypeObjectFrom().ordinal());	
}

/*
 * LOGIC
 * 
 * Retrieve program Dynamic subfield settings and trasformations for a specific program
 * Setting can be spreaded in callers/called programs.
 */
@GET  
@Produces("application/json")
@Path("programDynamicSubFieldSetting/{user}/{sys}/{subSys}/{idObject}/{numInstr}/{idField}/{idSubField}")  
public Response getProgramDynamicSubFieldSetting(
										  @PathParam("user")         String user
										, @PathParam("sys")          String sys
										, @PathParam("subSys")       String subSys
										, @PathParam("idObject")     String idProgram 
										, @PathParam("numInstr")     String numInstrX 
										, @PathParam("idField")      String idField 
										, @PathParam("idSubField")   String idSubField 
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	List<EntityDynamicFieldSubSetting> ls_dynamicFieldSubSetting = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	String resultJson = "";
	int numInstr = 0;

	// Normalize input
	numInstr = Integer.parseInt(numInstrX);
    if (idSubField.equals("*")) {
    	idSubField = "";
	}
    
	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
		
	// Initial
	Connection conn = DataBaseConnections.getConnection();
 	DAOImplDynamicFieldSubSetting edsfs = (DAOImplDynamicFieldSubSetting) AmritaStartup.sqlFactory.getDAODynamicFieldSubSetting(conn, false, false, ucfg);
 	ls_dynamicFieldSubSetting=edsfs.findAll(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, numInstr, idField, idSubField);
	
	jsonArray = new JSONArray();
	 	
   	// Get dynamicSubFields 
	for (EntityDynamicFieldSubSetting edss  : ls_dynamicFieldSubSetting) {
		
		jsonObject = new JSONObject();
		
		putJsonDynamicFieldSubSetting(jsonObject, edss );
		
		jsonArray.put(jsonObject);
	}

 	// Close connection
	DataBaseConnections.releaseConnection(conn);
	edsfs.setConn(null);

	resultJson = jsonArray.toString();
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
* LOGIC
* 
* Retrieve program Dynamic subfield settings for spread logic in callers/called
* 
* 	LAST_SET_BY_COBOL_USING_PARM   		// 06 Move FieldInUsing     
*   LAST_SET_BY_COBOL_LINKAGE 			// 07 Move FieldInLinkage   
*   LAST_SET_BY_CICS_DFHCOMMAREA,       // 25 Campo in DFHCOMMAREA 
*
*/
@GET  
@Produces("application/json")
@Path("programDynamicSubFieldSettingLastSetSpread/{user}/{sys}/{subSys}/{onlyLastSetUnresolved}")  
public Response getProgramDynamicSubFieldSettingLastSetSpread(
										  @PathParam("user")         			String user
										, @PathParam("sys")          			String sys
										, @PathParam("subSys")       			String subSys
										, @PathParam("onlyLastSetUnresolved")  	String onlyLastSetUnresolved     // Y/N
								
										) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	List<EntityDynamicFieldSubSetting> ls_dynamicFieldSubSetting = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	String resultJson = "";
	boolean getOnlyLastSetUnresolved = false;

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	if (onlyLastSetUnresolved.equals("Y")) {
		getOnlyLastSetUnresolved = true;
	}
	
	// Initial
	Connection conn = DataBaseConnections.getConnection();
	DAOImplDynamicFieldSubSetting edsfs = (DAOImplDynamicFieldSubSetting) AmritaStartup.sqlFactory.getDAODynamicFieldSubSetting(conn, false, false, ucfg);
	
	if (subSys.equals("*")) {
		ls_dynamicFieldSubSetting=edsfs.findAll(sys, getOnlyLastSetUnresolved);
	} else {
		ls_dynamicFieldSubSetting=edsfs.findAll(sys, subSys, getOnlyLastSetUnresolved);
	}
	
	jsonArray = new JSONArray();
	 	
  	// Get dynamicSubFields 
	for (EntityDynamicFieldSubSetting edss  : ls_dynamicFieldSubSetting) {
		
		jsonObject = new JSONObject();
		
		putJsonDynamicFieldSubSetting(jsonObject, edss );
		
		jsonArray.put(jsonObject);
	}

	// Close connection
	DataBaseConnections.releaseConnection(conn);
	edsfs.setConn(null);

	resultJson = jsonArray.toString();
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Caricamento oggetto Json con dati da db
 */
private void putJsonDynamicFieldSubSetting(JSONObject jsonObject, EntityDynamicFieldSubSetting edss ) {
	jsonObject.put("subSys", edss.getSubSystem());
	jsonObject.put("idObject", edss.getIdObject());
	jsonObject.put("numInstr", edss.getNumInstr());
	jsonObject.put("idField", edss.getIdField());
	jsonObject.put("idSubField", edss.getIdSubField());
	jsonObject.put("idPgmSet", edss.getIdPgmSet());
	jsonObject.put("numChain", edss.getNumChain());
	jsonObject.put("numInstrSet", edss.getNumInstrSet());
	jsonObject.put("numField", edss.getNumField());
	jsonObject.put("numSubField", edss.getNumSubField());
	// Porzione di sottocampo oggetto dell'impostazione 
	jsonObject.put("posInSubField", edss.getPosInSubField());
	jsonObject.put("lngInSubField", edss.getLngInSubField());
	// Informazioni di impostazione, programma, path e modalità di assegnazione e valore se disponibile (literal, campo di tabella)
	jsonObject.put("numInstrOriginSpreaded", edss.getNumInstrOriginSpreaded());
	jsonObject.put("typeObjectPgmSet", edss.getTypeObjectPgmSet());
	jsonObject.put("typeObjectPgmSetOrdinal", edss.getTypeObjectPgmSet().ordinal());
	jsonObject.put("numPath", edss.getNumPath());
	jsonObject.put("setMode", edss.getSetMode());
	jsonObject.put("numUsingParm", edss.getNumUsingParm());
	jsonObject.put("dspFieldInUsingParm", edss.getDspFieldInUsingParm());
	jsonObject.put("dspFieldInLinkageArea", edss.getDspFieldInLinkageArea());
	jsonObject.put("typePointerArea", edss.getTypePointerArea());
	jsonObject.put("typePointerAreaOrdinal", edss.getTypePointerArea().ordinal());
	jsonObject.put("dspPointerInLinkageArea", edss.getDspPointerInLinkageArea());
	jsonObject.put("numUsingParmPointer", edss.getNumUsingParmPointer());
	jsonObject.put("dspPointerInUsingParm", edss.getDspPointerInUsingParm());
	jsonObject.put("value", edss.getValue());
	// Campo input in assegnazione di trasformazione.
	// Posizione e lunghezza per il sender sono quelli espressi da reference modification (posSnd:lngSnd) se indicato.
    // Posizione e lunghezza sono sempre valorizzati e se non presenti sono inizializzati (1:size(campo sender))
	jsonObject.put("fieldSenderId", edss.getFieldSenderId());
	jsonObject.put("fieldSenderNum", edss.getFieldSenderNum());
	jsonObject.put("fieldSenderPos", edss.getFieldSenderPos());
	jsonObject.put("fieldSenderLng", edss.getFieldSenderLng());
    // Campo risultato in assegnazione di trasformazione o campo receiver senza trasformazioni. 
    // La posizione è quella iniziale interessata alla trasformazione.
    // La lunghezza è quella del sottocampo origine di cui trovare i valori.
    // Posizione e lunghezza sono sempre valorizzati ed inizializzati nel processo a 1, size(campo receiver)
    // Se l'istruzione Move che ha generato l'assegnazione contiene anche reference modification (pos:lng), l'informazione
    // è utilizzata solo per determinare se il receiver è influenzato dalla trasformazione, ma NON viene memorizzata.
	jsonObject.put("fieldReceiverId", edss.getFieldReceiverId());
	jsonObject.put("fieldReceiverNum", edss.getFieldReceiverNum());
	jsonObject.put("fieldReceiverPos", edss.getFieldReceiverPos());
	jsonObject.put("fieldReceiverLng", edss.getFieldReceiverLng());
	// Oggetto alla cui ioarea appartiene il campo ultima trasformazione, di cui trovare i valori esternamente
    // (prima assegnazione nella catena)
	jsonObject.put("idObjExt", edss.getIdObjExt());
	jsonObject.put("typeObjExt", edss.getTypeObjExt());
	jsonObject.put("objExtSqlTableColumn", edss.getObjExtSqlTableColumn());
	jsonObject.put("typeObjExtOrdinal", edss.getTypeObjExt().ordinal());
	jsonObject.put("objExtSystem", edss.getObjExtSystem());
	jsonObject.put("objExtColField", edss.getObjExtColField());
	jsonObject.put("objExtIdCopy", edss.getObjExtIdCopy());
	jsonObject.put("objExtPosCol", edss.getObjExtPosCol());
	jsonObject.put("objExtLengthCol", edss.getObjExtLengthCol());
	// Indicatori di soluzione e di valori disponibili
	jsonObject.put("solvedObjExt", edss.getSolvedObjExt());
	jsonObject.put("waitingForExternalData", edss.getWaitingForExternalData());	
	
	jsonObject.put("setModeOrdinal", edss.getSetMode().ordinal());
	
}

/*
 * SOURCE
 * 
 * Get Source starting from serialized Program object
 * 
 */
@GET  
@Produces("application/json")
@Path("sourceProgram/{user}/{sys}/{subSys}/{idObject}/{typeObject}")  
public Response getSourceFromProgramObject(
											  @PathParam("user")         String user
											, @PathParam("sys")          String sys
											, @PathParam("subSys")       String subSys
											, @PathParam("idObject")     String idObject 
											, @PathParam("typeObject")   String type 
									
											) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}


	String resultJson=getResultJsonObjectSource(user, sys, subSys, idObject, type);

	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Build the file (a single string) with instruction index row start/end + instruction number + Division + expand flag for copy nested
 * for a SPECIFIC COPY of a division  inside a program.
 * The index file will be composed directly from the program.
  */
@GET  
@Produces("application/json")
@Path("idxProgramCopyInstr/{user}/{division}/{idProgram}/{idCopy}")  
public Response getIdxCopyInstr(
							      @PathParam("user")         String user
								, @PathParam("division")     String division     // I E D P 	(where to find the copy)
								, @PathParam("idProgram")    String idProgram    // Program		(
								, @PathParam("idCopy")       String idCopy       // Copy name
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction>[] ar_entryDivision= null;
	ProgramCobolEntry<? extends Instruction> entryDivision = null;
	Instruction instr = null;
	InstructionCobol instrCobol = null;
	InstructionSql instrSql = null;
	String idCopyProc = "";
	String idxFile = "";
	String sep = "";
	String expand = "";
	int i;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			

    // Select ptoper division
    switch (division) {  
	    // Identification
		case "I":
			ar_entryDivision = programCobol.entriesIdentification();
			break;
		// Environment
		case "E":		
			ar_entryDivision = programCobol.entriesEnvironment();	 
			break;
	    // Data
		case "D":
			ar_entryDivision = programCobol.entriesData();	 
			break;
		case "W":
			ar_entryDivision = programCobol.entriesData();	 
			break;
		case "L":
			ar_entryDivision = programCobol.entriesData();	 
			break;
		case "P":	
			ar_entryDivision = programCobol.entriesProcedure();
			break;
		default:
			break;
	}
    
	// Ricerca copy/include in division  
	for (i = 0; i < ar_entryDivision.length; i++) {	
		entryDivision = ar_entryDivision[i];
		instr = entryDivision.getInstruction();
		if (entryDivision.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY
		|| (entryDivision.getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) && instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE){
			if (instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE) {
				instrSql = (InstructionSql) entryDivision.getInstruction();
				idCopyProc = instrSql.sqlIncludeGetName();
			} else {
				instrCobol = (InstructionCobol) entryDivision.getInstruction();
				idCopyProc = instrCobol.copyGetName();
			}
            if (idCopyProc.equals(idCopy)) {
				break;
			}
		}
	}
	
	// Scan istruzioni del copy dentro il programma
	for (i = i+1; i < ar_entryDivision.length; i++) {			
		entryDivision = ar_entryDivision[i];
		instr = entryDivision.getInstruction();
		expand = "-";
		
		// Fine istruzioni in copy
		if (entryDivision.getUnderCopyName().equals("")) {
			break;
		}		
		// Copy cambiato	
		if (!entryDivision.getUnderCopyName().equals(idCopy)) {
			break;
		}  
		
		// Istruzione dentro il copy richiesto
		expand = "-";
		idxFile = idxFile + sep + instr.getRowStartSource() + " " + instr.getRowEndSource() + " "  + instr.getNumInstr() + " P " + expand;
		sep = "|";
		
	} // end-for 		 
	
	return Response
			.status(200)
			.entity(idxFile)
			.build();
}

/*
 * Returns Indexes start/end source/instr division/section in the program
   */
@GET  
@Produces("application/json")
@Path("idxDivisionSections/{user}/{idProgram}")  
public Response getDivisionSectionsPointers(
							      @PathParam("user")         String user
								, @PathParam("idProgram")    String idProgram    // Program		 
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction>[] ar_entry = null;
	ProgramCobolEntry<? extends Instruction> entry = null;
	Instruction instr = null;
	JSONObject jsonObject = null; 
	String resultJson = "";
	
	// Division  start & end rows source numbers
	int iDivIdRowStart = -1;
	int iDivIdRowEnd = -1;
	int iDivEnvRowStart = -1;
	int iDivEnvRowEnd = -1;
	int iDivDataRowStart = -1;
	int iDivDataRowEnd = -1;
	int iDivProcRowStart = -1;
	int iDivProcRowEnd = -1;
	
	// Division  start & end rows instruction numbers
	int iDivIdInstrStart = -1;
	int iDivIdInstrEnd = -1;
	int iDivEnvInstrStart = -1;
	int iDivEnvInstrEnd = -1;
	int iDivDataInstrStart = -1;
	int iDivDataInstrEnd = -1;
	int iDivProcInstrStart = -1;
	int iDivProcInstrEnd = -1;
	
	// Sections start & end rows source numbers
	int iSecWorkingRowStart = -1;
	int iSecWorkingRowEnd = -1;
	int iSecLinkageRowStart = -1;
	int iSecLinkageRowEnd = -1;
	int iSecFileRowStart = -1;
	int iSecFileRowEnd = -1;

	// Sections start & end instruction numbers
	int iSecWorkingInstrStart = -1;
	int iSecWorkingInstrEnd = -1;
	int iSecLinkageInstrStart = -1;
	int iSecLinkageInstrEnd = -1;
	int iSecFileInstrStart = -1;
	int iSecFileInstrEnd = -1;

	int i;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
    
    // Identification division
    ar_entry = programCobol.entriesIdentification();
    entry = ar_entry[0];
    instr = entry.getInstruction();
    iDivIdRowStart = instr.getRowStartSource();
    iDivIdInstrStart = instr.getNumInstr();
    entry = ar_entry[ar_entry.length - 1];
    instr = entry.getInstruction();
    iDivIdRowEnd = instr.getRowEndSource();
    iDivIdInstrEnd = instr.getNumInstr();
    
    // Environment division
    ar_entry = programCobol.entriesEnvironment();
    entry = ar_entry[0];
    instr = entry.getInstruction();
    iDivEnvRowStart = instr.getRowStartSource();
    iDivEnvInstrStart = instr.getNumInstr();
    entry = ar_entry[ar_entry.length - 1];
    instr = entry.getInstruction();
    iDivEnvRowEnd = instr.getRowEndSource();
    iDivEnvInstrEnd = instr.getNumInstr();
    
    // Data division
    ar_entry = programCobol.entriesData();
    entry = ar_entry[0];
    instr = entry.getInstruction();
    iDivDataRowStart = instr.getRowStartSource();
    iDivDataInstrStart = instr.getNumInstr();
    entry = ar_entry[ar_entry.length - 1];
    instr = entry.getInstruction();
    iDivDataRowEnd = instr.getRowEndSource();
    iDivDataInstrEnd = instr.getNumInstr();
    // Ricerca section Working/Linkage/File
    for (i = 0; i < ar_entry.length; i++) {
    	entry = ar_entry[i];
        if (entry.getProgramSection() == EnumCobolReservedWords.DATA_DIV_WS_SECTION && !entry.isUnderCopy()) {
			if (iSecWorkingRowStart == - 1) {
				iSecWorkingRowStart = entry.getInstruction().getRowStartSource();
				iSecWorkingInstrStart = entry.getInstruction().getNumInstr();
			}
			iSecWorkingRowEnd = entry.getInstruction().getRowEndSource();
			iSecWorkingInstrEnd = entry.getInstruction().getNumInstr();
    	}
       	if (entry.getProgramSection() == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION && !entry.isUnderCopy()) {
			if (iSecLinkageRowStart == - 1) {
				iSecLinkageRowStart = entry.getInstruction().getRowStartSource();
				iSecLinkageInstrStart = entry.getInstruction().getNumInstr();
			}
			iSecLinkageRowEnd = entry.getInstruction().getRowEndSource();
			iSecLinkageInstrEnd = entry.getInstruction().getNumInstr();
    	}
       	if (entry.getProgramSection() == EnumCobolReservedWords.DATA_DIV_FILE_SECTION && !entry.isUnderCopy()) {
			if (iSecFileRowStart == - 1) {
				iSecFileRowStart = entry.getInstruction().getRowStartSource();
				iSecFileInstrStart = entry.getInstruction().getNumInstr();
			}
			iSecFileRowEnd = entry.getInstruction().getRowEndSource();
			iSecFileInstrEnd = entry.getInstruction().getNumInstr();
    	}   	
  	}
    
    // Proc division
    ar_entry = programCobol.entriesProcedure();
    entry = ar_entry[0];
    instr = entry.getInstruction();
    iDivProcRowStart = instr.getRowStartSource();
    iDivProcInstrStart = instr.getNumInstr();
    entry = ar_entry[ar_entry.length - 1];
    instr = entry.getInstruction();
    iDivProcRowEnd = instr.getRowEndSource();
    iDivProcInstrEnd = instr.getNumInstr();
    
    jsonObject = new JSONObject();
    jsonObject.put("iDivIdRowStart", iDivIdRowStart);
    jsonObject.put("iDivIdRowEnd", iDivIdRowEnd);
    jsonObject.put("iDivEnvRowStart", iDivEnvRowStart);
    jsonObject.put("iDivEnvRowEnd", iDivEnvRowEnd);
    jsonObject.put("iDivDataRowStart", iDivDataRowStart);
    jsonObject.put("iDivDataRowEnd", iDivDataRowEnd);
    jsonObject.put("iDivProcRowStart", iDivProcRowStart);
    jsonObject.put("iDivProcRowEnd", iDivProcRowEnd);
    
    jsonObject.put("iSecWorkingRowStart", iSecWorkingRowStart);
    jsonObject.put("iSecWorkingRowEnd", iSecWorkingRowEnd);
    jsonObject.put("iSecLinkageRowStart", iSecLinkageRowStart);
    jsonObject.put("iSecLinkageRowEnd", iSecLinkageRowEnd);
    jsonObject.put("iSecFileRowStart", iSecFileRowStart);
    jsonObject.put("iSecFileRowEnd", iSecFileRowEnd);
    
    jsonObject.put("iDivIdInstrStart", iDivIdInstrStart);
    jsonObject.put("iDivIdInstrEnd", iDivIdInstrEnd);
    jsonObject.put("iDivEnvInstrStart", iDivEnvInstrStart);
    jsonObject.put("iDivEnvInstrEnd", iDivEnvInstrEnd);
    jsonObject.put("iDivDataInstrStart", iDivDataInstrStart);
    jsonObject.put("iDivDataInstrEnd", iDivDataInstrEnd);
    jsonObject.put("iDivProcInstrStart", iDivProcInstrStart);
    jsonObject.put("iDivProcInstrEnd", iDivProcInstrEnd);
    
    jsonObject.put("iSecWorkingInstrStart", iSecWorkingInstrStart);
    jsonObject.put("iSecWorkingInstrEnd", iSecWorkingInstrEnd);
    jsonObject.put("iSecLinkageInstrStart", iSecLinkageInstrStart);
    jsonObject.put("iSecLinkageInstrEnd", iSecLinkageInstrEnd);
    jsonObject.put("iSecFileInstrStart", iSecFileInstrStart);
    jsonObject.put("iSecFileInstrEnd", iSecFileInstrEnd);
    
    resultJson = jsonObject.toString();

 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}
/*
 * Returns references for fields to instructions in procedure in input/output
 */
@GET  
@Produces("application/json")
@Path("programFieldXref/{user}/{idProgram}/{numInstr}")  
public Response geProgramFieldXref(
							          @PathParam("user")         String user
									, @PathParam("idProgram")    String idProgram    // Program		 
									, @PathParam("numInstr")     String numInstr     // Instruction number of the definition in Data division 		 
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction>[] ar_entry = null;
	ProgramCobolEntry<? extends Instruction> entry = null;
	DataItem instrDataItem = null;
	JSONArray jsonArray = null; 		
	JSONObject jsonObject = null;
	int[] arXrefInput = null;
	int[] arXrefOutput = null;
	String resultJson = "";
	String fieldName = "";
	int num = 0;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
    
    // Identification division
    ar_entry = programCobol.entriesData();
    num = Integer.parseInt(numInstr);
    entry = ar_entry[num];
    if (entry.getEntryType() == EnumInstrDataCategory.COBOL_DATA_ITEM) {
	    instrDataItem = (DataItem) entry.getInstruction();    
	    fieldName = instrDataItem.getDataName();
		arXrefInput = programCobol.getXrefSymbolInpProc(fieldName);
		arXrefOutput = programCobol.getXrefSymbolOutProc(fieldName);
	} else {
		arXrefInput = new int[0];
		arXrefOutput =  new int[0];
	} 
	
	// No data
	jsonArray = new JSONArray();

	// Xref Input
    for (int j = 0; j < arXrefInput.length; j++) {
        jsonObject = new JSONObject();
        jsonObject.put("io", "I");
        jsonObject.put("numInstr", arXrefInput[j]);
        jsonObject.put("numInstr", arXrefInput[j]);
        jsonObject.put("inCopy", entry.getUnderCopyName());
        jsonArray.put(jsonObject);
	}
    
    // Xref Output
    for (int j = 0; j < arXrefOutput.length; j++) {
        jsonObject = new JSONObject();
        jsonObject.put("io", "O");
        jsonObject.put("numInstr", arXrefOutput[j]);
        jsonObject.put("inCopy", entry.getUnderCopyName());
        jsonArray.put(jsonObject);
	}
    
    resultJson = jsonArray.toString();

 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}
/*
 * Returns information about procedure instruction
 *  row start/end 
 *  under copy name
 *  ...
 */
@GET  
@Produces("application/json")
@Path("programProcInstructionInfo/{user}/{idProgram}/{numInstr}")  
public Response getInfoProcInstruction(
							          @PathParam("user")         String user
									, @PathParam("idProgram")    String idProgram    // Program		 
									, @PathParam("numInstr")     String numInstr     // Instruction number of procedure		 
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction>[] ar_entry = null;
	ProgramCobolEntry<? extends Instruction> entry = null;
	ProgramCobolEntry<? extends Instruction> entryParagraph = null;
	Instruction instr = null;	
	InstructionCobolProcedure instrProc = null;	
	JSONObject jsonObject = null;
 
	String resultJson = "";
	int num = 0;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
    
    // Procedure division
    ar_entry = programCobol.entriesProcedure();
    num = Integer.parseInt(numInstr);
    entry = ar_entry[num];
    instr =(Instruction) entry.getInstruction();       
    jsonObject = new JSONObject();
    jsonObject.put("isEntryPrecompiler", entry.isEntryPrecompiler());
    jsonObject.put("isDeadCode", entry.isDeadCode());
    jsonObject.put("isDeadCodeUnreachable", entry.isDeadCodeUnreachable());
    jsonObject.put("numInstrRelated", entry.getNumInstrRelated());
    jsonObject.put("rowStartSource", instr.getRowStartSource());
    jsonObject.put("rowEndSource", instr.getRowEndSource());
    jsonObject.put("inCopy", entry.getUnderCopyName());
    if (instr.getTypeInstr() == EnumCobolReservedWords.PROC_PERFORM) {
       instrProc =  (InstructionCobolProcedure) entry.getInstruction();     
       jsonObject.put("isPerform", true);
       jsonObject.put("idFrom", instrProc.performGetFrom());
       jsonObject.put("idThru", instrProc.performGetThru());
       jsonObject.put("fromNumInstr", instrProc.performGetFromNumInstr());
       jsonObject.put("thruNumInstr", instrProc.performGetThruNumInstr());
       entryParagraph =  ar_entry[instrProc.performGetFromNumInstr()];
       jsonObject.put("rowStartFrom", entryParagraph.getInstruction().getRowStartSource());
       jsonObject.put("rowEndThru", entryParagraph.getInstruction().getRowEndSource());
       jsonObject.put("paragraphInCopy", entryParagraph.getUnderCopyName());
	} else {
       jsonObject.put("isPerform", false);
       jsonObject.put("idFrom", "");
       jsonObject.put("idThru", "");
       jsonObject.put("fromNumInstr", 0);
       jsonObject.put("thruNumInstr", 0);
       jsonObject.put("rowStartFrom", 0);
       jsonObject.put("rowEndThru", 0);	
       jsonObject.put("paragraphInCopy","");
	}
  
    resultJson = jsonObject.toString();
 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Returns information about procedure instruction
 *  row start/end 
 *  under copy name
 *  ...
 */
@GET  
@Produces("application/json")
@Path("programDataInstructionInfo/{user}/{idProgram}/{fieldName}/{numInstr}")  
public Response getInfoDataInstruction(
							          @PathParam("user")         String user
									, @PathParam("idProgram")    String idProgram    // Program		 
									, @PathParam("fieldName")    String fieldName    // Field name	 
									, @PathParam("numInstr")  	 String numInstr     // Pointer to Data Division Definition	 
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction> ar_entryData[] = null;
	ProgramCobolEntry<? extends Instruction> entryData = null;
	ProgramCobolEntry<? extends Instruction> entryDataX = null;
	EnumCobolReservedWords programSection = null;
	InstructionCobolDataItem instrData = null;
	Instruction instrCommon = null;
	Instruction instr = null;
	JSONObject jsonObject = null;
	String resultJson = "";
	String section = "";
	int numDefInstr = 0;
	int numDefInstrCopy = 0;
	int rowStartSourceCopy = 0;
	int rowEndSourceCopy = 0;
	boolean isDataDefined = false;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}
    
    numDefInstr = Integer.parseInt(numInstr);
    
    // Fornito numero definizione in Data Division
    if (numDefInstr > 0) {
    	entryData = programCobol.entryDataDivision(numDefInstr);
    	if (entryData.getInstruction() instanceof InstructionCobolDataItem) {
        	instrData = (InstructionCobolDataItem) entryData.getInstruction();
		} else {
			instrCommon = (Instruction) entryData.getInstruction();
		}
    	isDataDefined = true;
    // Fornito il nome del campo
	} else {
	    instrData = programCobol.dataItemDefinition(fieldName);
	    // Campo non definito o CICS non presente nel sorgente (EIB...)
	    if (instrData != null ) {
		    numDefInstr = instrData.getNumInstr();
		    entryData = programCobol.entryDataDivision(numDefInstr); 
		    isDataDefined = true;
		}
	}
   
    // Cerco section e definizione
    if (isDataDefined) {
        // Impostazione section Cobol
        programSection = entryData.getProgramSection();
        if (programSection == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
        	section = "L";
    	} else if (programSection == EnumCobolReservedWords.DATA_DIV_WS_SECTION) {
    		section = "W";
    	} else if (programSection == EnumCobolReservedWords.DATA_DIV_FILE_SECTION) {
    		section = "F";
    	}
        // Ricerca numero istruzione Copy dove il campo è definito
//      if (!entryData.getUnderCopyName().isBlank()) {
        if (entryData.getUnderCopyName().trim().length() != 0) {
        	ar_entryData = programCobol.entriesData();
    		for (int i = numDefInstr - 1; i > 0; i--) {
    			entryDataX = ar_entryData[i];
    			instr = entryDataX.getInstruction();
    			if (entryDataX.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY
    			|| (entryDataX.getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) && instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE){
    			    numDefInstrCopy = instr.getNumInstr();
    			    rowStartSourceCopy = instr.getRowStartSource();
    			    rowEndSourceCopy = instr.getRowEndSource();
    			    break;
    			}		
    		}
    	}
        jsonObject = new JSONObject();
        jsonObject.put("numDefField", numDefInstr);
        if (instrCommon != null) {
            jsonObject.put("rowStartDefField", instrCommon.getRowStartSource());
            jsonObject.put("rowEndDefField", instrCommon.getRowEndSource());
    	} else {
    	    jsonObject.put("rowStartDefField", instrData.getRowStartSource());
    	    jsonObject.put("rowEndDefField", instrData.getRowEndSource() );
    	}
        jsonObject.put("underCopyName", entryData.getUnderCopyName());
        jsonObject.put("numDefCopy", numDefInstrCopy);
        jsonObject.put("rowStartDefCopy", rowStartSourceCopy);  // Nel programma
        jsonObject.put("rowEndDefCopy", rowEndSourceCopy);      // Nel programma
        jsonObject.put("section", section);
        jsonObject.put("isDeadCode", entryData.isDeadCode());
        jsonObject.put("itemType",          (instrData != null) ?  instrData.getItemType() : "");
        jsonObject.put("levelNumber",       (instrData != null) ?  instrData.getLevelNumber() : "");
        jsonObject.put("numDec",            (instrData != null) ?  instrData.getNumDec() : "");
        jsonObject.put("numInt",            (instrData != null) ?  instrData.getNumInt() : "");
        jsonObject.put("sizeBytes",         (instrData != null) ?  instrData.getSizeBytes() : "");
        jsonObject.put("usage",             (instrData != null) ?  instrData.getUsage() : "");
        jsonObject.put("redefinesDataName", (instrData != null) ?  instrData.getRedefinesDataName() : "");
        jsonObject.put("picture",           (instrData != null) ?  instrData.getPicture() : "");
        jsonObject.put("redefinesDataName", (instrData != null) ?  instrData.getRedefinesDataName() : "");
        jsonObject.put("itemTypeOrdinal",   (instrData != null) ?  instrData.getItemType().ordinal() : "");
    } else {
    	jsonObject = new JSONObject();
	}

    resultJson = jsonObject.toString();
 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Returns information about the symbol name
 *  division (D, P)
 *  section (L, W, F)
 *  numInstr
 *  rowStart
 *  rowEnd
 */
@GET  
@Produces("application/json")
@Path("programSymbolNameInfo/{user}/{idProgram}/{symbolName}")  
public Response getInfoSymbolName(
						          @PathParam("user")         String user
								, @PathParam("idProgram")    String idProgram     // Program		 
								, @PathParam("symbolName")   String symbolName    // Symbol name	 
							) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction> ar_entryData[] = null;
	ProgramCobolEntry<? extends Instruction> entryData = null;
	ProgramCobolEntry<? extends Instruction> entryDataX = null;
	EnumCobolReservedWords programSection = null;
	InstructionCobolDataItem instrData = null;
	Instruction instr = null;
	JSONObject jsonObject = null;
	String resultJson = "";
	String division = "";
	String section = "";
	int numDefInstr = 0;
	int numDefInstrCopy = 0;
	int rowStartSourceCopy = 0;
	int rowEndSourceCopy = 0;
	boolean isDataDefined = false;
	boolean isLabel = false;
	boolean isSection = false;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}
       
    instrData = programCobol.dataItemDefinition(symbolName);
    
    // Campo non definito o CICS non presente nel sorgente (EIB...)
    if (instrData != null ) {
	    numDefInstr = instrData.getNumInstr();
	    entryData = programCobol.entryDataDivision(numDefInstr);
	    division = "D";
	    isDataDefined = true;
	} else {
		isLabel = programCobol.isLabel(symbolName);
		if (!isLabel) {
			isSection = programCobol.isSection(symbolName);
			division = "P";
		} else {
			division = "P";
		}
	}
  
    // Cerco section e definizione
    if (isDataDefined) {
        // Impostazione section Cobol
        programSection = entryData.getProgramSection();
        if (programSection == EnumCobolReservedWords.DATA_DIV_LINKAGE_SECTION) {
        	section = "L";
    	} else if (programSection == EnumCobolReservedWords.DATA_DIV_WS_SECTION) {
    		section = "W";
    	} else if (programSection == EnumCobolReservedWords.DATA_DIV_FILE_SECTION) {
    		section = "F";
    	}
        // Ricerca numero istruzione Copy dove il campo è definito
//      if (!entryData.getUnderCopyName().isBlank()) {
        if (entryData.getUnderCopyName().trim().length() != 0) {
        	ar_entryData = programCobol.entriesData();
    		for (int i = numDefInstr - 1; i > 0; i--) {
    			entryDataX = ar_entryData[i];
    			instr = entryDataX.getInstruction();
    			if (entryDataX.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY
    			|| (entryDataX.getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) && instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE){
    			    numDefInstrCopy = instr.getNumInstr();
    			    rowStartSourceCopy = instr.getRowStartSource();
    			    rowEndSourceCopy = instr.getRowEndSource();
    			    break;
    			}		
    		}
    	}
        jsonObject = new JSONObject();
        jsonObject.put("division", division);
        jsonObject.put("section", section);
        jsonObject.put("isDataDefined", isDataDefined);
        jsonObject.put("isLabel", isLabel);
        jsonObject.put("isSection", isSection);
        jsonObject.put("numDefField", numDefInstr);
    	jsonObject.put("rowStartDefField", instrData.getRowStartSource());
    	jsonObject.put("rowEndDefField", instrData.getRowEndSource() );
        jsonObject.put("underCopyName", entryData.getUnderCopyName());
        jsonObject.put("numDefCopy", numDefInstrCopy);
        jsonObject.put("rowStartDefCopy", rowStartSourceCopy);  // Nel programma
        jsonObject.put("rowEndDefCopy", rowEndSourceCopy);      // Nel programma
        jsonObject.put("section", section);
        jsonObject.put("isDeadCode", entryData.isDeadCode());
        jsonObject.put("itemType",          (instrData != null) ?  instrData.getItemType() : "");
        jsonObject.put("levelNumber",       (instrData != null) ?  instrData.getLevelNumber() : "");
        jsonObject.put("numDec",            (instrData != null) ?  instrData.getNumDec() : "");
        jsonObject.put("numInt",            (instrData != null) ?  instrData.getNumInt() : "");
        jsonObject.put("sizeBytes",         (instrData != null) ?  instrData.getSizeBytes() : "");
        jsonObject.put("usage",             (instrData != null) ?  instrData.getUsage() : "");
        jsonObject.put("redefinesDataName", (instrData != null) ?  instrData.getRedefinesDataName() : "");
        jsonObject.put("picture",           (instrData != null) ?  instrData.getPicture() : "");
        jsonObject.put("redefinesDataName", (instrData != null) ?  instrData.getRedefinesDataName() : "");
        jsonObject.put("itemTypeOrdinal",   (instrData != null) ?  instrData.getItemType().ordinal() : "");
    } else {
    	jsonObject = new JSONObject();
	}

    resultJson = jsonObject.toString();
 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Returns all programs called and callers the supplied program name
 * For each program is returned the list of:
 *  row start/end origin
 *  under copy name
 *  ... 
 */
@GET  
@Produces("application/json")
@Path("programCalledCaller/{user}/{sys}/{subSys}/{idProgram}")  
public Response getProgramCalledCaller(
								        @PathParam("user")         String user
								      , @PathParam("sys")          String sys
								      , @PathParam("subSys")       String subSys
								      , @PathParam("idProgram")    String idProgram      
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	JSONObject jsonObject = null;
	JSONObject jsonObjectOrigin = null;
	JSONArray jsonArray = null;
	JSONArray jsonArrayOrigin = null;
	List<EntityRelationOrigin> ls_object = null;
	List<EntityRelationOrigin> ls_objectCall = null;
	List<EntityRelationOrigin> ls_objectCicsLink = null;
	List<EntityRelationOrigin> ls_objectCicsXctl = null;
	List<EntityRelationOrigin> ls_objectXctl = null;
	String resultJson = "";
	String idObjectPrev = "";
	EnumObjectStatus objectStatus = null;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			

	Connection conn = DataBaseConnections.getConnection();
	IDAORelationOrigin eoDAO = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false, false, ucfg);
	
	// Initial
	jsonArray = new JSONArray();
	jsonObject = new JSONObject();
	
	//---------------------------
	// Relazioni dirette (Called)
	//---------------------------
	
    // Get relationOrigin PGM-CALLED-PGM
    ls_objectCall=eoDAO.findAllOrigin(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, EnumRelation.PGM_CALLED_PGM, EnumObject.OBJECT_PGM_COBOL);
    if (ls_objectCall.size() > 0) {
     	// Ahead
    	idObjectPrev = ls_objectCall.get(0).getIdObjectRelB();	
      	objectStatus = getObjectStatus(conn, ucfg, sys, subSys, idObjectPrev);
     	jsonArrayOrigin = new JSONArray();
    	jsonObjectOrigin = new JSONObject();
		jsonObject = new JSONObject();
		jsonObject.put("program", idObjectPrev);
		jsonObject.put("type", "called");
		if (objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS 
		|| objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION
		|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED
		|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ANALYZED) {
			jsonObject.put("status", "U");
		} else {
			jsonObject.put("status", " ");
		}
		jsonObject.put("arOrigin", jsonArrayOrigin);
    	
    	// Populate Json
    	for (EntityRelationOrigin relationOrigin : ls_objectCall) {
    		
    		if (!idObjectPrev.equals(relationOrigin.getIdObjectRelB())) {
     			idObjectPrev = relationOrigin.getIdObjectRelB();
       			objectStatus = getObjectStatus(conn, ucfg, sys, subSys, idObjectPrev);
    			jsonArray.put(jsonObject);
    			jsonObject = new JSONObject();
    			jsonArrayOrigin = new JSONArray();  			
    			jsonObject.put("program", idObjectPrev);
    			jsonObject.put("type", "called");
    			if (objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS 
				|| objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION
				|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED
				|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ANALYZED) {
					jsonObject.put("status", "U");
				} else {
					jsonObject.put("status", " ");
				}
    			jsonObject.put("arOrigin", jsonArrayOrigin);
    		}
    		jsonObjectOrigin = new JSONObject();
    		jsonObjectOrigin.put("programCalled", relationOrigin.getIdObjectRelB());
    		jsonObjectOrigin.put("numInstrOrigin", relationOrigin.getNumInstrOrigin());
    		jsonObjectOrigin.put("rowStart", relationOrigin.getRowStart());
    		jsonObjectOrigin.put("rowEnd", relationOrigin.getRowEnd());
    		jsonObjectOrigin.put("inCopy", relationOrigin.getCopyOrigin());
    		jsonObjectOrigin.put("rowStartInCopy", relationOrigin.getRowStartInCopy());
    		jsonObjectOrigin.put("rowEndInCopy", relationOrigin.getRowEndInCopy());
    		jsonArrayOrigin.put(jsonObjectOrigin);
    	}
     
        // Last program 
    	jsonArray.put(jsonObject);	
    }

    //---------------------------
	// Relazioni inverse (Caller)
	//---------------------------
    
    // Get relationOrigin PGM-CALLED-PGM
    ls_objectCall=eoDAO.findAllOrigin(sys, subSys, EnumObject.OBJECT_PGM_COBOL, EnumRelation.PGM_CALLED_PGM, idProgram, EnumObject.OBJECT_PGM_COBOL);
    ls_objectCicsLink=eoDAO.findAllOrigin(sys, subSys, EnumObject.OBJECT_PGM_COBOL, EnumRelation.PGM_XCTL_PGM, idProgram, EnumObject.OBJECT_PGM_COBOL);
    ls_objectCicsXctl=eoDAO.findAllOrigin(sys, subSys, EnumObject.OBJECT_PGM_COBOL, EnumRelation.PGM_CICS_LINK_PGM, idProgram, EnumObject.OBJECT_PGM_COBOL);
    ls_objectXctl=eoDAO.findAllOrigin(sys, subSys, EnumObject.OBJECT_PGM_COBOL, EnumRelation.PGM_CICS_XCTL_PGM, idProgram, EnumObject.OBJECT_PGM_COBOL);
    
    ls_object = ls_objectCall;
    ls_object.addAll(ls_objectCicsLink);
    ls_object.addAll(ls_objectCicsXctl);
    ls_object.addAll(ls_objectXctl);
    
	// There are callers
    if (ls_object.size() > 0) {
    	// Ahead
    	idObjectPrev = ls_object.get(0).getIdObjectRelA();
    	objectStatus = getObjectStatus(conn, ucfg, sys, subSys, idObjectPrev);
    	jsonArrayOrigin = new JSONArray();
    	jsonObjectOrigin = new JSONObject();
		jsonObject = new JSONObject();
		jsonObject.put("program", idObjectPrev);
		jsonObject.put("type", "caller");
		if (objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS 
		|| objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION
		|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED
		|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ANALYZED) {
			jsonObject.put("status", "U");
		} else {
			jsonObject.put("status", " ");
		}
		jsonObject.put("arOrigin", jsonArrayOrigin);
    	
    	// Populate Json
    	for (EntityRelationOrigin relationOrigin : ls_object) {
    		
    		if (!idObjectPrev.equals(relationOrigin.getIdObjectRelA())) {
    			idObjectPrev = relationOrigin.getIdObjectRelA();
    			objectStatus = getObjectStatus(conn, ucfg, sys, subSys, idObjectPrev);
    			jsonArray.put(jsonObject);
    			jsonObject = new JSONObject();
    			jsonArrayOrigin = new JSONArray();
    			jsonObject.put("program", idObjectPrev);
    			jsonObject.put("type", "caller");
    			if (objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_ERRORS 
				|| objectStatus == EnumObjectStatus.OBJECT_ANALYZED_WITH_EXCEPTION
				|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ACQUIRED
				|| objectStatus == EnumObjectStatus.OBJECT_TO_BE_ANALYZED) {
					jsonObject.put("status", "U");
				} else {
					jsonObject.put("status", " ");
				}
    			jsonObject.put("arOrigin", jsonArrayOrigin);
    		}
    		jsonObjectOrigin = new JSONObject();
    		jsonObjectOrigin.put("programCalled", relationOrigin.getIdObjectRelA());
    		jsonObjectOrigin.put("numInstrOrigin", relationOrigin.getNumInstrOrigin());
    		jsonObjectOrigin.put("rowStart", relationOrigin.getRowStart());
    		jsonObjectOrigin.put("rowEnd", relationOrigin.getRowEnd());
    		jsonObjectOrigin.put("inCopy", relationOrigin.getCopyOrigin());
    		jsonObjectOrigin.put("rowStartInCopy", relationOrigin.getRowStartInCopy());
    		jsonObjectOrigin.put("rowEndInCopy", relationOrigin.getRowEndInCopy());
    		jsonArrayOrigin.put(jsonObjectOrigin);
    	}	
    	
        // Last program 
    	jsonArray.put(jsonObject);	
    }

	// Close connection
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);
   
    resultJson = jsonArray.toString();
 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


/*
 * Returns all programs callers with call/Link/Xctl
 * On demand 
 */
@GET  
@Produces("application/json")
@Path("programCallers/{user}/{sys}/{subSys}/{idProgram}")  
public Response getProgramCallers(
							        @PathParam("user")         String user
							      , @PathParam("sys")          String sys
							      , @PathParam("subSys")       String subSys
							      , @PathParam("idProgram")    String idProgram      
							) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	List<EntityRelation> ar_Relation = null;
	String resultJson = "";
	String whereCondition = "";									// Condizione di where
	String orderBy = "";							    		// Order by
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	// Operazioni per accesso al database
	Connection conn = DataBaseConnections.getConnection();
	IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);

	// Composizione condizione Where 
	whereCondition =                      "      sys  =  '" 	 + sys    + "'";
	whereCondition = whereCondition +	  " AND  subSys  =  '" 	 + subSys + "'";
	whereCondition = whereCondition +     " AND (relation  =   " + EnumRelation.PGM_CALLED_PGM.ordinal()    + " OR ";
	whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_XCTL_PGM.ordinal()      + " OR ";
	whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_LINK_PGM.ordinal() + " OR ";
	whereCondition = whereCondition +     "      relation  =   " + EnumRelation.PGM_CICS_XCTL_PGM.ordinal();
	whereCondition = whereCondition +     "     ) ";
	whereCondition = whereCondition +     " AND  typeObjectB  =   " + EnumObject.OBJECT_PGM_COBOL.ordinal();
	whereCondition = whereCondition +     " AND  idObjectB  =  '" + idProgram + "'";
	
    // Ordinamento per numero istruzione
	orderBy = " ORDER BY idObjectA";
	
    // Esecuzione query su Relation con condizione fornita
	ar_Relation = eoDAO.readSetEntityWhere(whereCondition, orderBy);
	
	jsonArray = new JSONArray();
	
	// Scan relazioni e accodamento nomi programma
	for (EntityRelation o : ar_Relation) {
		jsonObject = new JSONObject();
		jsonObject.put("pgmCaller", o.getIdObjectB());
		jsonObject.put("relation", o.getRelation());		
		jsonArray.put(jsonObject);
	}

	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);
		
    resultJson = jsonArray.toString();
 
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


// Recupero status oggetto
private EnumObjectStatus getObjectStatus(Connection conn, UserConfiguration ucfg, String sys, String subSys, String idProgram) throws SQLException, ExceptionAmrita {
	EntityObject eo = null;
	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
	
	eo = new EntityObject();
	eo.setSystem(sys);
	eo.setSubSystem(subSys);
	eo.setIdObject(idProgram);
	eo.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
	eoDAO.read(eo);
	return eo.getStatus();
}


/*
 * Get the name of the copy at a specific instruction number.
 * The division is supplied.
 */
@GET  
@Produces("application/json")
@Path("programCopyName/{user}/{division}/{idProgram}/{numInstr}")  
public Response getCopyName(
							      @PathParam("user")         String user
								, @PathParam("division")     String division     // I E D P
								, @PathParam("idProgram")    String idProgram    // Program
								, @PathParam("numInstr")     String numInstr     // Copy instruction number
								) throws JSONException, SQLException, ExceptionAmrita {
	
	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	ProgramCobolEntry<? extends Instruction>[] ar_entryDivision= null;
	ProgramCobolEntry<? extends Instruction> entryDivision = null;
	Instruction instr = null;
	InstructionCobol instrCobol = null;
	InstructionSql instrSql = null;
	String idCopy = "";
	int numInstrCopy = 0;
	
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			

    // Select ptoper division
    switch (division) {  
	    // Identification
		case "I":
			ar_entryDivision = programCobol.entriesIdentification();
			break;
		// Environment
		case "E":		
			ar_entryDivision = programCobol.entriesEnvironment();	 
			break;
	    // Data
		case "D":
			ar_entryDivision = programCobol.entriesData();	 
			break;
		case "P":	
			ar_entryDivision = programCobol.entriesProcedure();
			break;
		default:
			break;
	}

    numInstrCopy = Integer.parseInt(numInstr);
    
	entryDivision = ar_entryDivision[numInstrCopy];
	instr = entryDivision.getInstruction();
	if (entryDivision.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY
	|| (entryDivision.getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) && instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE){
		if (instr.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE) {
			instrSql = (InstructionSql) entryDivision.getInstruction();
			idCopy = instrSql.sqlIncludeGetName();
		} else {
			instrCobol = (InstructionCobol) entryDivision.getInstruction();
			idCopy = instrCobol.copyGetName();
		}
	}
	
		 	
	return Response
			.status(200)
			.entity(idCopy)
			.build();
}
/*
 * Get the file (a single string) with instruction index row start/end + instruction number + Division + expand flag for copy
 * 
 */
@GET  
@Produces("application/json")
@Path("idxProgramInstr/{user}/{idObject}")  
public Response getIdxProgramInstr(
		  @PathParam("user")         String user
		, @PathParam("idObject")     String idProgram     
		) throws JSONException, SQLException, ExceptionAmrita {
	
    final String SUFFIX_SERIALIZED_PGM_IDX = "idx";
	UserConfiguration ucfg = null;
	String resultJson="";
	String path = "";

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}


	List<String> listFiles = null;;
	
	try {
		path = ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm() + File.separator + idProgram + "." + SUFFIX_SERIALIZED_PGM_IDX;
		listFiles = Files.readAllLines(Paths.get(path),StandardCharsets.ISO_8859_1);
		resultJson = listFiles.get(0);
	} catch (IOException e) {
		// Empty list
		resultJson = "KO" + e.getMessage();
	}
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Get the Program source from folder (stored on db in object table)
 * inside a specific system/subsystem.
 * The object is read to get the source path.
 * The program index file to match row number and instruction nimber is returned too.
 */
@GET  
@Produces("application/json")
@Path("sourceAndPgmIdx/{user}/{sys}/{subSys}/{typeObject}/{idObject}")  
public Response getSourceAndPgmIdxFromFileSystem(   
		  @PathParam("user")        String user
		, @PathParam("sys")   		String sys 
		, @PathParam("subSys")   	String subSys 
		, @PathParam("typeObject")  String typeObject 
		, @PathParam("idObject")    String idObject 				
		) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	SourceManager sm = null;
	JSONObject jsonObjectTotal = null; 
	JSONObject jsonObject = null; 
	JSONArray jsonArray = null; 		
	List<String> sourceRows = null;
	EntityObject eo = null;
	EnumObject enumObject = null;
	String pathSource = "";
	String suffixFileSource = "";
	String resultJson = "";
	String path = "";
	String idxProgramFile = "";
	boolean isFound = false;

	// Login should be donejsonObjectTotal
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	// Instance SourceManager
	sm = new SourceManager(ucfg);

	enumObject= EnumObject.values()[Integer.parseInt(typeObject.trim())];
	

	// Get program object
	Connection conn = DataBaseConnections.getConnection();
	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, true, true, ucfg);
	eo = new EntityObject();
	eo.setSystem(sys);
	eo.setSubSystem(subSys);
	eo.setTypeObject(enumObject);
	eo.setIdObject(idObject);
	isFound=eoDAO.read(eo);	
    
	if (isFound) {
		// Get the idxProgramInstr file
		List<String> listFiles = null;;		
		try {
			path = ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm() + File.separator + idObject + "." + SUFFIX_SERIALIZED_PGM_IDX;
			listFiles = Files.readAllLines(Paths.get(path),StandardCharsets.ISO_8859_1);
			idxProgramFile = listFiles.get(0);
		} catch (IOException e) {
			// Empty list
			idxProgramFile = "";
		}

		jsonArray = new JSONArray();

		jsonObjectTotal = new JSONObject();
		
		jsonObject = new JSONObject();
		jsonObject.put("idxSource", idxProgramFile);
		jsonObjectTotal.put("objIdxSource", jsonObject);

		suffixFileSource=eo.getSuffixFileSource();
		pathSource=eo.getLibraryDir() + File.separator + idObject;
//		if (!eo.getSuffixFileSource().isBlank()) {
		if (eo.getSuffixFileSource().trim().length() != 0) {
			pathSource += "." + suffixFileSource;
		}
		sourceRows =sm.getFile(pathSource);

		// Populate Json with source
		for (int i = 0; i < sourceRows.size(); i++) {
			jsonObject = new JSONObject();
			jsonObject.put("row", sourceRows.get(i));
			jsonArray.put(jsonObject);
		}
		
		jsonObject = new JSONObject();
		jsonObject.put("arSource", jsonArray);
		jsonObjectTotal.put("objSource", jsonObject);
		
		// Return json object
		resultJson = jsonObjectTotal.toString();		
	} else {
		resultJson="KOFile " + idObject + " Not Found";
	}

	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);

	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


/*
 * Get a generic source from folder stored on db in object table
 * inside a specific system/subsystem.
 * The object is read to get the source path.
 */
@GET  
@Produces("application/json")
@Path("source/{user}/{sys}/{subSys}/{typeObject}/{idObject}")  
public Response getSourceFromFileSystem(   
		  @PathParam("user")        String user
		, @PathParam("sys")   		String sys 
		, @PathParam("subSys")   	String subSys 
		, @PathParam("typeObject")  String typeObject 
		, @PathParam("idObject")    String idObject 				
		) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	JSONObject jsonObject = null; 
	JSONArray jsonArray = null; 		
	List<String> sourceRows = null;
	EnumObject enumObject = null;
	String resultJson = "";

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	enumObject= EnumObject.values()[Integer.parseInt(typeObject.trim())];

	sourceRows = getSourceRowsFromFileSystem(sys, subSys, enumObject, idObject, ucfg);
	
	if (sourceRows != null) {

		jsonArray = new JSONArray();

		// Populate Json
		for (int i = 0; i < sourceRows.size(); i++) {
			jsonObject = new JSONObject();
			jsonObject.put("row", sourceRows.get(i));
			jsonArray.put(jsonObject);
		}

		// Return json object
		resultJson = jsonArray.toString();		
	} else {
		resultJson="KOFile " + idObject + " Not Found";
	}

	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Restituisce le righe sorgente dell'oggetto selezionato
 * Se l'oggetto non esiste si restituisce un array vuoto
 * Se l'oggetto esiste ma il sorgente non è trovato si restituisce null
 * Viene prima letto l'oggetto su db
 * Poi si sfrutta SourceManager per leggere il sorgente al path specificato
 * Il path è sempre quello dal quale il sorgente è stato analizzato.
 * 
 * Metodo richiamato anche da PgmSummary
 */
private List<String> getSourceRowsFromFileSystem(String sys, String subSys, EnumObject typeObject, String idObject, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
	
	List<String> sourceRows = null;
	EntityObject eo = null;
	String pathSource = "";
	String suffixFileSource = "";
	boolean isFound = false;
	
	// Get object descriptor
	Connection conn = DataBaseConnections.getConnection();
	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, true, true, ucfg);
	eo = new EntityObject();
	eo.setSystem(sys);
	eo.setSubSystem(subSys);
	eo.setTypeObject(typeObject);
	eo.setIdObject(idObject);
	
	isFound=eoDAO.read(eo);	
	
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);	
   
	// Non è un oggetto in db
	if (!isFound) {
		return new ArrayList<String>();
	}
	
    // Composizione path completo
	suffixFileSource=eo.getSuffixFileSource();
	pathSource=eo.getLibraryDir() + File.separator + idObject;
//	if (!eo.getSuffixFileSource().isBlank()) {
	if (eo.getSuffixFileSource().trim().length() != 0) {
		pathSource += "." + suffixFileSource;
	}
	sourceRows = SourceManager.getFile(pathSource);
	
	return sourceRows;
}


/*
 * Gets the list of objects to be processed by Analyzer generated by the pilot.
 */
@GET  
@Produces("application/json")
@Path("listSourcesToProcess/{user}/{pilotName}")  
public Response getListSourcesToProcess(  @PathParam("user")      String user
										 ,@PathParam("pilotName") String pilotName
										) throws ExceptionAmrita {
	
 	UserConfiguration ucfg = null;
 	ExecutionObjectsToProcess exObj = null;
 	JSONArray jsonArray = null;
 	JSONObject jsonObject = null;
 	ArrayList<ExecutionObject> ls_objects = null;
	String resultJson="";

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	exObj = new ExecutionObjectsToProcess(ucfg);

	try {
		ls_objects = exObj.getObjectList(pilotName);
		jsonArray = new JSONArray();		
		for (ExecutionObject eo : ls_objects) {
			jsonObject = new JSONObject();
			jsonObject.put("idObject", eo.idObject);
			jsonObject.put("typeObject", eo.typeObject);
			jsonObject.put("typeObjectOrdinal", eo.typeObject.ordinal());
			jsonArray.put(jsonObject);
		}
		resultJson = jsonArray.toString();
	} catch (Exception e) {
		resultJson ="KO"+e.getMessage();
	}
	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}


/*
 * LOGIC
 * 
 * Execution of dynamic values detection, exactly as in LogicSamePgm.
 * With input program, field name and instruction number the standard process for LogicSamePgm is activated.
 * All dynamic information detected are returned from memory structure, as they where get from sql tables.
 * 
 *  EntityDynamicField
 *  EntityDynamicSubField
 *  EntityDynamicSubFieldValue
 *  EntityDynamicSubFieldWaitExt
 *  EntityDynamicValueExt  
 *  
 *  
 */
@GET  
@Produces("application/json")
@Path("programLogicDynamicInfo/{user}/{sys}/{subSys}/{idProgram}/{idField}/{numInstr}")  
public Response getProgramDynamicLogicInfo(@PathParam("user")      String user
										  ,@PathParam("sys")       String sys
										  ,@PathParam("subSys")    String subSys
										  ,@PathParam("idProgram") String idProgram
							    		  ,@PathParam("idField")   String idField
							    		  ,@PathParam("numInstr")  String numInstrX
										) throws ExceptionAmrita, SQLException {
	
 	UserConfiguration ucfg = null;
 	ProgramCobol programCobol = null;
 	LogicInfoDynamic logicInfoDynamic = null;
 	LogicDynamicInstruction logicDynamicIntruction = null;
 	LogicSamePgm logicSamePgm = null;
 	LogicDynamicField logicDynamicField = null;
 	LogicWorkProcess ilpw = null;                                 // Area di controllo generata nel processo logiche stesso programma
 	Instruction instruction = null;
 	DataItemCobolIdentifier identifierField = null;
 	InstructionCobolDataItem dataItem = null;
	JSONArray jsonArrayFieldValue = null;
	JSONArray jsonArrayFieldSub = null;
	JSONArray jsonArrayFieldSubValue = null;
	JSONArray jsonArrayFieldSubSetting = null;
	JSONObject jsonObject = null;
	JSONObject jsonObjectField = null;
	JSONObject jsonObjectFieldValue = null;
	JSONObject jsonObjectFieldSub = null;
	JSONObject jsonObjectFieldSubValue = null;
	JSONObject jsonObjectFieldSubSetting = null;
 	ArrayList<String> al_values = null;
 	int[] arDataItemDef = null;
 	String resultJson="";
	int numInstr = 0;
	int numInstrDef = 0;

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}
    logicInfoDynamic = programCobol.getLogicInfoDynamic();
    logicSamePgm = new LogicSamePgm(ucfg, null, programCobol);
    numInstr = Integer.parseInt(numInstrX);
    instruction = (Instruction) programCobol.instructionProcedure(numInstr);
    
    // Definizione campo
    arDataItemDef = programCobol.dataItemPointers(idField);  
    numInstrDef = arDataItemDef[0];
    
    // Prepara field identifier
    identifierField = new DataItemCobolIdentifier();
    dataItem = programCobol.dataItemDefinition(numInstrDef);
    identifierField.setNumInstr(numInstrDef);
    identifierField.setDataItem(dataItem);
    identifierField.setNameIdentifier(idField);
    identifierField.setIdentifierType(EnumCobolReservedWords.DATA_DIV_DATA_ITEM);
 
    // Simulazione istruzione dinamica
    logicDynamicIntruction = new LogicDynamicInstruction();

    // Prepara LogicDynamicField, con campo ed eventuali sottocampi
    logicDynamicField = new LogicDynamicField("", "");
    logicDynamicField.dataItemCobolIdentifier = identifierField;

       
    // Completa istruzione dinamica e la inserisce in struttura dinamica programma
	// Impostazioni per LogicInfoDynamic 
    logicDynamicIntruction = logicInfoDynamic.getDynamicInstr(instruction.getNumInstr());
	if (logicDynamicIntruction == null) {
		logicDynamicIntruction = new LogicDynamicInstruction();
		logicDynamicIntruction.programName = programCobol.getProgramName();
		logicDynamicIntruction.setDynamicInstr(instruction);
	}
	logicDynamicIntruction.al_dynamicField.add(logicSamePgm.getDynamicFieldToSolve());				
	// Accodamento istruzione dinamica in struttura associata al programma
	logicInfoDynamic.addDynamicInstr(instruction, logicDynamicIntruction);	
      
    al_values = logicSamePgm.dynamicValues(programCobol, instruction, identifierField);   
        
    ilpw = logicSamePgm.getIlpw();
 
    // Oggetto Json finale
    jsonObject = new JSONObject();
    
    // Field
    jsonObjectField = new JSONObject();
    putJsonDynamicInstr(jsonObjectField, ilpw.dynamicFieldToSolve.entityDynamicField);
  
    // Field value
    jsonArrayFieldValue = new JSONArray();
    for (String fieldValue : al_values) {
    	jsonObjectFieldValue = new JSONObject();
    	jsonObjectFieldValue.put("value", fieldValue);            // Per compatibilita con valori sottocampo
    	jsonArrayFieldValue.put(jsonObjectFieldValue);
	}
    
    // Subfield
    jsonArrayFieldSub = new JSONArray();
    for (LogicDynamicFieldSub subField : ilpw.dynamicFieldToSolve.al_FieldSub) {
    	
     	// Scan subField values
    	jsonArrayFieldSubValue = new JSONArray();
    	for (LogicDynamicValue subFieldValue : subField.al_value) {
    		jsonObjectFieldSubValue = new JSONObject();
    		putJsonDynamicValue(jsonObjectFieldSubValue, subFieldValue.entityDynamicValue );
        	jsonArrayFieldSubValue.put(jsonObjectFieldSubValue);
		}    	
    	
    	// Scan subfield chains
    	jsonArrayFieldSubSetting = new JSONArray();
        for (ArrayList<LogicDynamicFieldSubSetting> chainSetting : subField.al_al_chainSetSubField) {
 			// Scan subField chain settings           	
        	for (LogicDynamicFieldSubSetting fieldSubSetting : chainSetting) {
        		jsonObjectFieldSubSetting = new JSONObject();
            	putJsonDynamicFieldSubSetting(jsonObjectFieldSubSetting, fieldSubSetting.entityDynamicFieldSetting );
            	jsonArrayFieldSubSetting.put(jsonObjectFieldSubSetting);
			}
		} 
         
       	jsonObjectFieldSub = new JSONObject();       
    	putJsonDynamicSubField(jsonObjectFieldSub, subField.entityDynamicFieldSub);
    	jsonObjectFieldSub.put("arFieldSubValue", jsonArrayFieldSubValue);
        jsonObjectFieldSub.put("arFieldSubSetting", jsonArrayFieldSubSetting);
    	
    	jsonArrayFieldSub.put(jsonObjectFieldSub);
	}
    
    // Composizione finale oggetto da restituire
    jsonObject.put("field", jsonObjectField);
    jsonObject.put("arFieldSub", jsonArrayFieldSub);          // Dati subField, arValue, arSetting
    jsonObject.put("arFieldValue", jsonArrayFieldValue);
     
    resultJson = jsonObject.toString();
     
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * COMMON
 * 
 * Retrieve the complete program coded.
 * The source will be read from the file system
 * Information of each instruction will be returned 
 * Code shared with PgmSummary
 */
@GET  
@Produces("application/json")
@Path("programCoded/{user}/{idProgram}")  
public Response getProgramCoded(
							      @PathParam("user")        String user
								, @PathParam("sys")    		String sys 
								, @PathParam("subSys")    	String subSys 
								, @PathParam("idProgram")   String idProgram 
							) throws JSONException, SQLException, ExceptionAmrita {

	UserConfiguration ucfg = null;
	ProgramCobol programCobol = null;
	JSONObject jsonObjectPgm = null;
	String COPY_SOURCE = "N";			// In output i copy sorgente invece che la loro codifica
	String resultJson = "";	
	 	
	// Login should be done
	// If not take default properties	 
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
 
	// Get object program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}			
   
    jsonObjectPgm = new JSONObject();
    
    getJsonProgramCoded(ucfg, sys, subSys, programCobol, user, idProgram, COPY_SOURCE, jsonObjectPgm);
	
	// Return json object
	resultJson = jsonObjectPgm.toString();		
	    	
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * Restituisce un oggetto JSON con tutto il programma codificato.
 * Le righe sorgente sono incluse, a livello di ogni singola istruzione.
 * Se isCopySource = "Y" viene incluso il sorgente dei copy invece che le righe codificate
 */
private void getJsonProgramCoded(UserConfiguration ucfg, String sys, String subSys, ProgramCobol programCobol, String user, String idProgram, String isCopySource, JSONObject jsonObjectPgm) throws SQLException, ExceptionAmrita {

	JSONArray jsonArrayPgmInstrI = null;
	JSONArray jsonArrayPgmInstrE = null;
	JSONArray jsonArrayPgmInstrD = null;
	JSONArray jsonArrayPgmInstrP = null;
	List<String> al_rowSource = null;
	
	jsonArrayPgmInstrI = new JSONArray();
	jsonArrayPgmInstrE = new JSONArray();
	jsonArrayPgmInstrD = new JSONArray();
	jsonArrayPgmInstrP = new JSONArray();
	
	al_rowSource = getSourceProgram(ucfg, sys, subSys, idProgram);
	
	// Estrazione e output delle istruzioni codificate nelle varie divisioni
	putSourceCodedByCobolDivision("I", programCobol, programCobol.entriesIdentification(), al_rowSource, jsonArrayPgmInstrI, ucfg);
	putSourceCodedByCobolDivision("E", programCobol, programCobol.entriesEnvironment(),    al_rowSource, jsonArrayPgmInstrE, ucfg);
	putSourceCodedByCobolDivision("D", programCobol, programCobol.entriesData(), 		   al_rowSource, jsonArrayPgmInstrD, ucfg);
	putSourceCodedByCobolDivision("P", programCobol, programCobol.entriesProcedure(), 	   al_rowSource, jsonArrayPgmInstrP, ucfg);
		
	// Output programma codificato
	// Ogni istruzione ha associato un array con le righe sorgente originali
	jsonObjectPgm.put("idProgram", idProgram);
	jsonObjectPgm.put("arInstrDivId", jsonArrayPgmInstrI);
	jsonObjectPgm.put("arInstrDivEnv", jsonArrayPgmInstrE);
	jsonObjectPgm.put("arInstrDivData", jsonArrayPgmInstrD);
	jsonObjectPgm.put("arInstrDivProc", jsonArrayPgmInstrP);
	
	return;
}


/*
 * VIEWER
 * 
 * Restituisce tutte le informazioni di documentazione del programma divise per sezioni
 * in formato HTML pronto per essere inserito nelle <div> della pagina
 * 
 * I moduli copy possono essere esplosi come dettaglio codificato oppure come sorgente originale.
 * In caso di dettaglio codificato viene incluso un oggetto Json per ogni riga fi dati o programma codificato
 * Altrimenti viene incluso un arrayList con il sorgente originario del copy, associato all'entry COPY statement   
 */
@GET  
@Produces("application/json")
@Path("programPgmSummary/{user}/{sys}/{subSys}/{idProgram}/{isCopySource}")  
public Response getProgramSummary(@PathParam("user")      	 String user
								 ,@PathParam("sys")       	 String sys
								 ,@PathParam("subSys")    	 String subSys
								 ,@PathParam("idProgram") 	 String idProgram
								 ,@PathParam("isCopySource") String isCopySource    // Y=copy sorgente incluso; N=copy codificato incluso
								) throws ExceptionAmrita, SQLException {
	
 	UserConfiguration ucfg = null;
 	ProgramCobol programCobol = null;
	JSONObject jsonObject = null;
	
 	String resultJson="";

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}
    
    // Produzione generalizzata program summary
    jsonObject = getProgramSummaryJsonObject(user, sys, subSys, idProgram, programCobol, ucfg, isCopySource);
           
    resultJson = jsonObject.toString();
     
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}



/*
 * Restituisce un oggetto Json con il programSummary completo diviso per sezioni
 */
private JSONObject getProgramSummaryJsonObject(String user, String sys, String subSys, String idProgram,ProgramCobol programCobol, UserConfiguration ucfg, String isCopySource) throws SQLException, ExceptionAmrita {
	
	JSONObject jsonObjectPgm = new JSONObject();
	JSONObject objInfoAmrita = new JSONObject();
	JSONObject objInfoSource = new JSONObject();
	JSONObject objProgramOptions = new JSONObject();
	JSONObject objRelationships = new JSONObject();
	JSONObject objCopySqlInclude = new JSONObject();
	JSONObject objCrudMatrix = new JSONObject();
	JSONObject objFileSystemIO = new JSONObject();
	JSONObject objSourceCoded = new JSONObject();
	JSONObject objScreenLayout = new JSONObject();
	JSONObject objDynamicCode = new JSONObject();
	JSONObject objParSectionXref = new JSONObject();
	JSONObject objSymbolsXref = new JSONObject();
	JSONObject objDeadCode = new JSONObject();
	JSONObject objMetrics = new JSONObject();
	JSONObject objQuality = new JSONObject();
	JSONObject objViolations = new JSONObject();
	
    jsonObjectPgm = new JSONObject();
    
    // Popola oggetti json con singole sezioni program summary
    programSummaryInfoAmrita(user, sys, subSys, idProgram, programCobol, objInfoAmrita, ucfg);
    programSummaryInfoSource(user, sys, subSys, idProgram, programCobol, objInfoSource, ucfg);
    programSummaryProgramOptions(user, sys, subSys, idProgram, programCobol, objProgramOptions,ucfg);
    programSummaryRelationships(user, sys, subSys, idProgram, programCobol, objRelationships, ucfg);
    programSummaryCopySqlInclude(user, sys, subSys, idProgram, programCobol, objCopySqlInclude, ucfg);
    programSummaryCrudMatrix(user, sys, subSys, idProgram, programCobol, objCrudMatrix, ucfg);
    programSummaryFileSystemIO(user, sys, subSys, idProgram, programCobol, objFileSystemIO, ucfg);
    programSummarySourceCoded(user, sys, subSys, idProgram, programCobol, objSourceCoded, isCopySource, ucfg);
//  programSummaryScreenLayout(user, sys, subSys, idProgram, programCobol, objScreenLayout, ucfg);
    programSummaryParSectionXref(user, sys, subSys, idProgram, programCobol, objParSectionXref);
    programSummaryParSymbolsXref(user, sys, subSys, idProgram, programCobol, objSymbolsXref);
    programSummaryDynamicCode(user, sys, subSys, idProgram, programCobol, objDynamicCode);
    programSummaryDeadCode(user, sys, subSys, idProgram, programCobol, objDeadCode);
    programSummaryMetrics(user, sys, subSys, idProgram, programCobol, objMetrics, ucfg);
    programSummaryQuality(user, sys, subSys, idProgram, programCobol, objQuality, ucfg);
    programSummaryViolations(user, sys, subSys, idProgram, programCobol, objViolations, ucfg);
    
    // Composizione finale oggetto da restituire
    jsonObjectPgm.put("infoAmrita", objInfoAmrita);
    jsonObjectPgm.put("infoSource", objInfoSource);
    jsonObjectPgm.put("programOptions", objProgramOptions);
    jsonObjectPgm.put("relationships", objRelationships);
    jsonObjectPgm.put("copySqlInclude", objCopySqlInclude);
    jsonObjectPgm.put("crudMatrix", objCrudMatrix);
    jsonObjectPgm.put("fileSystemIO", objFileSystemIO);
    jsonObjectPgm.put("sourceCoded", objSourceCoded);
    jsonObjectPgm.put("screenLayout", objScreenLayout);
    jsonObjectPgm.put("dynamicCode", objDynamicCode);
    jsonObjectPgm.put("parSectionXref", objParSectionXref);
    jsonObjectPgm.put("symbolsXref", objSymbolsXref);
    jsonObjectPgm.put("deadCode", objDeadCode);
    jsonObjectPgm.put("metrics", objMetrics);
    jsonObjectPgm.put("quality", objQuality);
    jsonObjectPgm.put("violations", objViolations);
    
	return jsonObjectPgm;
}

/*
 * VIEWER
 * 
 * Restituisce tutte le informazioni di documentazione del programma divise per sezioni
 * in formato HTML pronto per essere inserito nelle <div> della pagina
 * 
 * Viene richiamato il codice del web service "programPgmSummary" per ottenere il programma codificato e il sorgente
 * Sul questa base si costruiscono tutti gli output HTML  
 */
@GET  
@Produces("application/json")
@Path("programPgmSummaryHTML/{user}/{sys}/{subSys}/{idProgram}/{isCopySource}")  
public Response getProgramSummaryHTML(@PathParam("user")  				String user
									 ,@PathParam("sys")       			String sys
									 ,@PathParam("subSys")    			String subSys
									 ,@PathParam("idProgram") 			String idProgram
									 ,@PathParam("isCopySource")	    String isCopySource    // Y=inclusione source copy
									) throws ExceptionAmrita, SQLException {
	
	UserConfiguration ucfg = null;
 	ProgramCobol programCobol = null;
	JSONObject jsonObjectPgmCoded = null;
	JSONObject jsonObjectDivHTML = null;

	JSONObject objInfoAmrita = null;
	JSONObject objInfoSource= null;
	JSONObject objProgramOptions = null;
	JSONObject objRelationships = null;
	JSONObject objCopySqlInclude = null;
	JSONObject objCrudMatrix = null;
	JSONObject objFileSystemIO = null;
	JSONObject objSourceCoded = null;
	JSONObject objScreenLayout = null;
	JSONObject objDynamicCode = null;
	JSONObject objParSectionXref = null;
	JSONObject objSymbolsXref = null;
	JSONObject objDeadCode = null;
	JSONObject objMetrics = null;
	JSONObject objQuality = null;
	JSONObject objViolations = null;

	String divHTMLInfoAmrita = null;
	String divHTMLInfoSource= null;
	String divHTMLProgramOptions = null;
	String divHTMLRelationships = null;
	String divHTMLCopySqlInclude = null;
	String divHTMLCrudMatrix = null;
	String divHTMLFileSystemIO = null;
	String divHTMLSourceCoded = null;
	String divHTMLScreenLayout = null;
	String divHTMLDynamicCode = null;
	String divHTMLParSectionXref = null;
	String divHTMLSymbolsXref = null;
	String divHTMLDeadCode = null;
	String divHTMLMetrics = null;
	String divHTMLQuality = null;
	String divHTMLViolations = null;
	
 	String resultJson="";

	// Login should be done
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}
	
	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idProgram);
    if (programCobol == null) {
    	programCobol = getProgram(user, idProgram);  
		AmritaStartup.cache.put(idProgram, programCobol);
	}
  
    // Produzione generalizzata program summary
    jsonObjectPgmCoded = getProgramSummaryJsonObject(user, sys, subSys, idProgram, programCobol, ucfg, isCopySource);
               
    // Recupero oggetti json sezioni di documentazione
    objInfoAmrita 		= (JSONObject) jsonObjectPgmCoded.get("infoAmrita");
    objInfoSource 		= (JSONObject) jsonObjectPgmCoded.get("infoSource");
    objProgramOptions 	= (JSONObject) jsonObjectPgmCoded.get("programOptions");
    objRelationships 	= (JSONObject) jsonObjectPgmCoded.get("relationships");
    objCopySqlInclude 	= (JSONObject) jsonObjectPgmCoded.get("copySqlInclude");
    objCrudMatrix 		= (JSONObject) jsonObjectPgmCoded.get("crudMatrix");
    objFileSystemIO 	= (JSONObject) jsonObjectPgmCoded.get("fileSystemIO");
    objSourceCoded 		= (JSONObject) jsonObjectPgmCoded.get("sourceCoded");
    objScreenLayout 	= (JSONObject) jsonObjectPgmCoded.get("screenLayout");
    objDynamicCode 		= (JSONObject) jsonObjectPgmCoded.get("dynamicCode");
    objParSectionXref 	= (JSONObject) jsonObjectPgmCoded.get("parSectionXref");
    objSymbolsXref 		= (JSONObject) jsonObjectPgmCoded.get("symbolsXref");
    objDeadCode 		= (JSONObject) jsonObjectPgmCoded.get("deadCode");
    objMetrics 		    = (JSONObject) jsonObjectPgmCoded.get("metrics");
    objQuality 		    = (JSONObject) jsonObjectPgmCoded.get("quality");
    objViolations 		= (JSONObject) jsonObjectPgmCoded.get("violations");
      
    // Composizione HTML per ogni sezione
    divHTMLInfoAmrita 		= programSummaryInfoAmritaToHTML(objInfoAmrita);
    divHTMLInfoSource 		= programSummaryInfoSourceToHTML(objInfoSource);
    divHTMLProgramOptions 	= programSummaryProgramOptionsToHTML(objProgramOptions);
    divHTMLRelationships 	= programSummaryRelationshipsToHTML(objRelationships);
    divHTMLCopySqlInclude 	= programSummaryCopySqlIncludeToHTML(objCopySqlInclude);
    divHTMLCrudMatrix 		= programSummaryCrudMatrixToHTML(objCrudMatrix);
    divHTMLFileSystemIO 	= programSummaryFileSystemIOToHTML(objFileSystemIO);
    divHTMLSourceCoded 		= programSummarySourceCodedToHTML(objSourceCoded);
//    divHTMLScreenLayout 	= programSummaryScreenLayoutToHTML(objScreenLayout);
    divHTMLDynamicCode 		= programSummaryDynamicCodeToHTML(objDynamicCode);
    divHTMLParSectionXref 	= programSummaryParSectionXrefToHTML(objParSectionXref);
    divHTMLSymbolsXref 		= programSummarySymbolsXrefToHTML(objSymbolsXref);
    divHTMLDeadCode 		= programSummaryDeadCodeToHTML(objDeadCode);
    divHTMLMetrics 		    = programSummaryMetricsToHTML(objMetrics);
    divHTMLQuality 		    = programSummaryQualityToHTML(objQuality);
    divHTMLViolations 		= programSummaryViolationsToHTML(objViolations);
    
    // Json object da restituire al chiamante
    jsonObjectDivHTML = new JSONObject();
    jsonObjectDivHTML.put("divHTMLInfoAmrita", 		divHTMLInfoAmrita);
    jsonObjectDivHTML.put("divHTMLInfoSource", 		divHTMLInfoSource);
    jsonObjectDivHTML.put("divHTMLProgramOptions", 	divHTMLProgramOptions);
    jsonObjectDivHTML.put("divHTMLRelationships", 	divHTMLRelationships);
    jsonObjectDivHTML.put("divHTMLCopySqlInclude", 	divHTMLCopySqlInclude);
    jsonObjectDivHTML.put("divHTMLCrudMatrix", 		divHTMLCrudMatrix);
    jsonObjectDivHTML.put("divHTMLFileSystemIO", 	divHTMLFileSystemIO);
    jsonObjectDivHTML.put("divHTMLSourceCoded", 	divHTMLSourceCoded);
    jsonObjectDivHTML.put("divHTMLScreenLayout", 	divHTMLScreenLayout);
    jsonObjectDivHTML.put("divHTMLDynamicCode", 	divHTMLDynamicCode);
    jsonObjectDivHTML.put("divHTMLParSectionXref", 	divHTMLParSectionXref);
    jsonObjectDivHTML.put("divHTMLSymbolsXref", 	divHTMLSymbolsXref);
    jsonObjectDivHTML.put("divHTMLDeadCode", 		divHTMLDeadCode);
    jsonObjectDivHTML.put("divHTMLMetrics", 		divHTMLMetrics);
    jsonObjectDivHTML.put("divHTMLQuality", 		divHTMLQuality);
    jsonObjectDivHTML.put("divHTMLViolations", 		divHTMLViolations);
    
    resultJson = jsonObjectDivHTML.toString();
     
	return Response
			.status(200)
			.entity(resultJson)
			.build();
}

/*
 * HTML informazioni generali
 */
private String programSummaryInfoAmritaToHTML(JSONObject objInfoAmrita) {
	Set<String> setJsonKeys = null;
    String tbl = "";
    String tbltop = "";
    String main = "";
	String tr = "";
    String tblBottom = "";
    String divContainer = "";    
	
	// Info Amrita
    tbltop = "<table id='tbInfoAmrita' class='tbInfoAmrita tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Name</th>"
            +         "<th>Value</th>"
            +      "</tr>"
    ;
	setJsonKeys = objInfoAmrita.keySet();
	for (String keyJson : setJsonKeys) {
	 	tr  =   "<tr>"
            +       "<td>"  + keyJson  + "</td>" 
            +       "<td>"  + objInfoAmrita.getString(keyJson)  + "</td>" 
            +    "</tr>"
	 	;
	 	
		main += tr;
	}
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;
    
    divContainer ="<div class='divTbInfoAmrita' id='divTbInfoAmrita'>"
		         +    tbl
		         + "</div>"
		         ;	
    
	return divContainer;
}

/*
 * HTML informazioni su programma sorgente
 */
private String programSummaryInfoSourceToHTML(JSONObject objInfoSource) {
	Set<String> setJsonKeys = null;
    String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
    String tr = "";
    String divContainer = "";    
	
	// Info pgm sorgente
    tbltop = "<table id='tbInfoSource' class='tbInfoSource tbPgmSummary'>"
           +      "<tr>"
           +         "<th>Name</th>"
           +         "<th>Value</th>"
           +      "</tr>"
    ;
	setJsonKeys = objInfoSource.keySet();
	for (String keyJson : setJsonKeys) {
	 	tr  =   "<tr>"
            +       "<td>"  + keyJson  + "</td>" 
            +       "<td>"  + objInfoSource.get(keyJson)  + "</td>" 
            +    "</tr>"
	 	;
	 	main += tr;
	}
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbInfoSource' id='divTbInfoSource'>"
		         +    tbl
		         + "</div>"
		         ;	

    return divContainer;
}

/*
 * HTML opzioni di programma
 */
private String programSummaryProgramOptionsToHTML(JSONObject objProgramOptions) {
    JSONArray jsonArOption = null;
    JSONObject jsonOption = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
    String divContainer = ""; 
	
	// Info pgm sorgente
    tbltop = "<table id='tbOption' class='tbOption tbPgmSummary'>"
           +      "<tr>"
           +         "<th>Option</th>"
           +      "</tr>"
    ;
 	
    jsonArOption = objProgramOptions.getJSONArray("arOption");
    for (Object option : jsonArOption) {
    	jsonOption = (JSONObject) option;
	 	tr  =   "<tr>"
            +       "<td>"  + jsonOption.get("option") + "</td>" 
            +    "</tr>"
	 	;			
	    main += tr;
	}
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbOption' id='divTbOption'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
}

/*
 * HTML Relazioni dirette/inverse del programma
 */
private String programSummaryRelationshipsToHTML(JSONObject objRelationships) {
    JSONArray jsonArRelationship = null;
    JSONArray jsonArRelationshipOrigin = null;
    JSONObject jsonRelationship = null;
    JSONObject jsonRelationshipOrigin = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
	String relation = "";
	String copyOrigin = "";
	String instrProgramArea = "";
	String relationType = "";
	String relationSource = "";
	String idObjectRelated = "";
	int numInstrOrigin = 0;
	int rowStartInCopy = 0;
	
	// Relationships
    tbltop = "<table id='tbRelation' class='tbRelation tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Relation</th>"
            +         "<th>IdObject</th>"
            +         "<th>NumInstr</th>"
            +         "<th>InCopy</th>"
            +         "<th>CopyRow</th>"
            +         "<th>Area</th>"
            +         "<th>RelationType</th>"
            +         "<th>RelationSource</th>"
            +      "</tr>"
    ;
 	
    // Relazioni
    jsonArRelationship = objRelationships.getJSONArray("arRelation");
    for (Object relationship : jsonArRelationship) {
    	jsonRelationship = (JSONObject) relationship;
    	relation = jsonRelationship.get("relation").toString();
    	idObjectRelated = jsonRelationship.getString("idObjectRelated");
    	
	 	// Origine Relazioni
	 	jsonArRelationshipOrigin = (JSONArray) jsonRelationship.getJSONArray("arRelationOrigin");
	 	for (Object relationshipOrigin : jsonArRelationshipOrigin) {
	 	   	jsonRelationshipOrigin = (JSONObject) relationshipOrigin;

	    	numInstrOrigin = jsonRelationshipOrigin.getInt("numInstrOrigin");
	    	copyOrigin = jsonRelationshipOrigin.getString("copyOrigin");
	    	rowStartInCopy = jsonRelationshipOrigin.getInt("rowStartInCopy");
	    	instrProgramArea = jsonRelationshipOrigin.get("instrProgramArea").toString();
	    	instrProgramArea = instrProgramArea.substring(0, 4);
	    	relationType = jsonRelationshipOrigin.get("relationType").toString();
	    	relationSource = jsonRelationshipOrigin.get("relationSource").toString() ;
	 	   	
		 	tr  =   "<tr>"
		        +       "<td>"  + relation  		+ "</td>" 
		        +       "<td>"  + idObjectRelated   + "</td>" 
 	            +       "<td>"  + numInstrOrigin  	+ "</td>" 
 	            +       "<td>"  + copyOrigin  		+ "</td>" 
	            +       "<td>"  + rowStartInCopy  	+ "</td>" 
	            +       "<td>"  + instrProgramArea  + "</td>" 
	            +       "<td>"  + relationType  	+ "</td>" 
	            +       "<td>"  + relationSource  	+ "</td>" 
 	            +    "</tr>"
	 		    ;
		 	
		 	main += tr;
		 	
	    	relation = "";
		}
	}
    
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbRelation' id='divTbRelation'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
}

/*
 * HTML Copy/Sql include dichiarate nel programma
 */
private String programSummaryCopySqlIncludeToHTML(JSONObject objCopySqlInclude) {
    JSONArray jsonArCopy = null;
    JSONArray jsonArSqlInclude = null;
    JSONObject jsonCopyInclude = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
	
	// Copy/Sql include
    tbltop = "<table id='tbCopyInclude' class='tbCopyInclude tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Type</th>"
            +         "<th>Name</th>"
            +      "</tr>"
    ;
 	
    // Copy
    jsonArCopy = objCopySqlInclude.getJSONArray("arCopy");
    for (Object copyInclude : jsonArCopy) {
    	jsonCopyInclude = (JSONObject) copyInclude;
	 	tr  +=   "<tr>"
            +       "<td>"  + "Copy" + "</td>" 
            +       "<td>"  + jsonCopyInclude.getString("name")  + "</td>" 
            +    "</tr>"
        ;
	}
 
    // Include
    jsonArSqlInclude = objCopySqlInclude.getJSONArray("arSqlInclude");
    for (Object copyInclude : jsonArSqlInclude) {
    	jsonCopyInclude = (JSONObject) copyInclude;
	 	tr  +=   "<tr>"
            +       "<td>"  + "Include"  + "</td>" 
            +       "<td>"  + jsonCopyInclude.getString("name")  + "</td>" 
            +    "</tr>"
        ;
	}

    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbCopyInclude' id='divTbCopyInclude'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
 }

/*
 * HTML CRUD matrix accessi nel programma
 */
private String programSummaryCrudMatrixToHTML(JSONObject objCrudMatrix) {
    JSONArray jsonArCRUD = null;
    JSONObject jsonCRUD = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";

	String strCreate = ""	;
	String strRead = ""	;
	String strUpdate = ""	;
	String strDelete = ""	;
	
	// Crud matrix
    tbltop = "<table id='tbCrudMatrix' class='tbCrudMatrix tbPgmSummary'>"
           +      "<tr>"
           +         "<th>Name</th>"
           +         "<th>Type</th>"
           +         "<th>Create</th>"
           +         "<th>Read</th>"
           +         "<th>Update</th>"
           +         "<th>Delete</th>"
           +      "</tr>"
    ;
 	 	
    // CRUD
    jsonArCRUD = objCrudMatrix.getJSONArray("arCRUD");
    for (Object crud : jsonArCRUD) {
    	jsonCRUD = (JSONObject) crud;
    	strCreate = "N" ;
    	strRead = "N"	;
    	strUpdate = "N"	;
    	strDelete = "N"	;

    	if (jsonCRUD.getBoolean("isCreate") == true) {
    		strCreate = "Y";
		}
    	if (jsonCRUD.getBoolean("isRead") == true) {
    		strRead = "Y";
		}
    	if (jsonCRUD.getBoolean("isUpdate") == true) {
    		strUpdate = "Y";
		}
    	if (jsonCRUD.getBoolean("isDelete") == true) {
    		strDelete = "Y";
		}
    	 
	 	tr  +=   "<tr>"
            +       "<td>"  + jsonCRUD.getString("entity")  + "</td>" 
            +       "<td>"  + "SQL Table"     	+ "</td>" 
            +       "<td>"  + strCreate  		+ "</td>" 
            +       "<td>"  + strRead  			+ "</td>" 
            +       "<td>"  + strUpdate  		+ "</td>" 
            +       "<td>"  + strDelete  		+ "</td>" 
            +    "</tr>"
        ;
	    main += tr;

	}
 
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbCrudMatrix' id='divTbCrudMatrix'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
    
}

/*
 * HTML I/O file system nel programma
 */
private String programSummaryFileSystemIOToHTML(JSONObject objFileSystemIO) {
    JSONArray jsonArFileSystemIO = null;
    JSONObject jsonFileSystemIO = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
	String strCreate = "";
	String strRead = ""	;
	String strUpdate = "";
	String strDelete = "";
	
	// File/crud/jcl/..
    tbltop = "<table id='tbFileSysteIO' class='tbFileSysteIO tbPgmSummary'>"
           +      "<tr>"
           +         "<th>InternalName</th>"
           +         "<th>Type</th>"
           +         "<th>Create</th>"
           +         "<th>Read</th>"
           +         "<th>Update</th>"
           +         "<th>Delete</th>"
           +         "<th>DDName</th>"
           +         "<th>phisicalName</th>"
           +         "<th>jclName</th>"
           +         "<th>jclStep</th>"
           +         "<th>jclProc</th>"
           +         "<th>jclLibrary</th>"
           +         "<th>jclLibraryPath</th>"
           +      "</tr>"
    ;
 	
    // File/crud/jcl/..
    jsonArFileSystemIO = objFileSystemIO.getJSONArray("arFileSystemIO");
    for (Object fileSystemIO : jsonArFileSystemIO) {
    	jsonFileSystemIO = (JSONObject) fileSystemIO;
    	strCreate = "";
    	strRead = "";
    	strUpdate = "";
    	strDelete = "";
    	if (jsonFileSystemIO.getBoolean("isCreate") == true) {
    		strCreate = "Y";
		}
    	if (jsonFileSystemIO.getBoolean("isRead") == true) {
    		strRead = "Y";
		}
    	if (jsonFileSystemIO.getBoolean("isUpdate") == true) {
    		strUpdate = "Y";
		}
    	if (jsonFileSystemIO.getBoolean("isDelete") == true) {
    		strDelete = "Y";
		}
    	
	 	tr  =   "<tr>"
            +       "<td>"  + jsonFileSystemIO.getString("internalName")     + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("internalNameType") + "</td>" 
            +       "<td>"  + strCreate + "</td>" 
            +       "<td>"  + strRead   + "</td>" 
            +       "<td>"  + strUpdate + "</td>" 
            +       "<td>"  + strDelete + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("externalName") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("phisicalName") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("jclName") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("jclStep") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("jclProc") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("jclLibrary") + "</td>" 
            +       "<td>"  + jsonFileSystemIO.getString("jclLibraryPath") + "</td>" 
            +    "</tr>"
        ;
	    main += tr;
	}
 
    // Tabella vuota
//  if (main.isBlank()) {
    if (main.trim().length() == 0) {
	 	tr  =   "<tr>"
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +       "<td> . </td>" 
	            +    "</tr>"
	        ;
		    main += tr;
	}
    
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

    divContainer = "<div class='divTbFileSysteIO' id='divTbFileSysteIO'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
}

/*
 * HTML source coded programma
 * 
 */
private String programSummarySourceCodedToHTML(JSONObject objSourceCoded) {
	JSONArray jsonArInstrDivId = null;
	JSONArray jsonArInstrDivEnv = null;
	JSONArray jsonArInstrDivData = null;
	JSONArray jsonArInstrDivProc = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String divContainer = "";

	tbltop = "<table class='tbSourceRows' id='tbSourceRows" + "_" + objSourceCoded.getString("idProgram") + "'>";
	
	jsonArInstrDivId = objSourceCoded.getJSONArray("arInstrDivId");
	jsonArInstrDivEnv = objSourceCoded.getJSONArray("arInstrDivEnv");
	jsonArInstrDivData = objSourceCoded.getJSONArray("arInstrDivData");
	jsonArInstrDivProc = objSourceCoded.getJSONArray("arInstrDivProc");
		
	main  = main + programSummarySourceCodedDivisionToHTML("I", jsonArInstrDivId);
	main  = main + programSummarySourceCodedDivisionToHTML("E", jsonArInstrDivEnv);
	main  = main + programSummarySourceCodedDivisionToHTML("D", jsonArInstrDivData);
	main  = main + programSummarySourceCodedDivisionToHTML("P", jsonArInstrDivProc);
	
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 
	
    divContainer = "<div id='divSourceCoded'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer; 
}


/*
 * HTML division programma
 * Righe sorgente programma
 */
private String programSummarySourceCodedDivisionToHTML(String division, JSONArray jsonArrayInstr) {
   JSONObject jsonInstr = null;
   String row = "";
   String main = "";
   String mainCopy = "";
   String tblCopy = "";
   String tblTopCopy = "";
   String tblBottomCopy = "";
   String expand = "";
   String idCopy = "";
   int numRow = 0;
   int numRowLast = 0;
   int numRowCopy = 0;
   int numInstr = 0;
   int i = 0;
   
   jsonInstr = jsonArrayInstr.getJSONObject(0);
   numRow = jsonInstr.getInt("rowStart");
   
   // Scan instructions
   for (Object objInstr : jsonArrayInstr) {
	   jsonInstr = (JSONObject) objInstr;
	   numInstr = jsonInstr.getInt("numInstr");
	   numRowLast = jsonInstr.getInt("rowEnd");
	   expand = "";
	   idCopy = "";
	   // Scan rows instructions
	   for (Object objRow : jsonInstr.getJSONArray("arRowSource")) {
		   if (jsonInstr.getBoolean("isCopyStatement") && numRow == numRowLast) {
			   expand = "+";
			   idCopy = jsonInstr.getString("underCopyName");
		   }
	       row = "<pre>" + (String) objRow + "</pre>";
	   	   main  +=   "<tr class='trAll'     id='" + division + "r_"      + numRow + "' tabindex='0' >"              + "</td>" 
    	         +      "<td class='nRow'    id='" + division + "nR_"     + numRow + "' tabindex='0' >" + numRow     + "</td>"  
    	         +      "<td class='nInstr'  id='" + division + "nI_"  	  + numRow + "' tabindex='0' >" + numInstr   + "</td>" 
     	         +      "<td class='expand'  id='" + division + "expand_" + numRow + "' tabindex='0'  onclick='onclick_expandProc(this.id)'" + " >"  + expand  	+ "</td>" 
     	         +      "<td class='row'     id='" + division + "row_"    + numRow + "' tabindex='0'>"  + row  	    + "</td>" 
    	         +    "</tr>";	   	   
	   	   numRow++;
	   }
	   
	   // Copy/Include inserisco riga+td+Div+table con il copy
       if (expand.equals("+")) {
    	   tblCopy = "";
	   	   main  += "<tr class='trDiv' id='" + division + "trDiv_" + numRowLast + "'>" 
                 +     "<td class='tdDiv' colspan='4'>" 
                 +        "<div id='" + division + "tdDiv_" + numRowLast + "'>";
           
	   	   // Inserimento righe copy
           tblTopCopy = "<table class='tbSourceCopyRows' id='" + division + "tbCopy_" + idCopy + "'"  + ">";
           mainCopy = "";
           numRowCopy = 0;
           for (Object objRowCopy : jsonInstr.getJSONArray("arSourceCopy")) {
        	   row = (String) objRowCopy;
        	   numInstr = 0;
			   mainCopy +=   "<tr class='trAll'    id='" + division + "r" 		+ idCopy + "_" 	+ numRowCopy  + "' tabindex='0' >"
			            +      "<td class='nRow'   id='" + division + "nR" 		+ idCopy + "_" 	+ numRowCopy  + "' tabindex='0' >"  + numRowCopy    + "</td>" 
			            +      "<td class='nInstr' id='" + division + "nI" 		+ idCopy + "_" 	+ numRowCopy  + "' tabindex='0' >"  + numInstr 		+ "</td>" 
			            +      "<td class='expand' id='" + division + "expand" 	+ idCopy + "_" 	+ numRowCopy  + "' tabindex='0' >"  + "|"     		+ "</td>" 
			            +      "<td class='row'    id='" + division + "row"    	+ idCopy + "_" 	+ numRowCopy  + "' tabindex='0' "   + " ><pre>" + row + "</pre></td>" 
 	     	            +    "</tr>";
			   numRowCopy++;
		   }
           mainCopy += "</table>";
           tblCopy = tblTopCopy + mainCopy + tblBottomCopy;
           main += tblCopy;
           
	   	   main  +=       "</div>" 
                 +     "</td>" 
	   			 + "</tr>";
	   }
       
	   i++;		   
   }
   return main;
}

/*
 * HTML Screen Layout mappe usate dal programma
 *  - Map layout
 *  - Map identification
 *  - Elenco campi BMS
 *  - BMS source
 */
private String programSummaryScreenLayoutToHTML(JSONObject objScreenLayout) {
	JSONArray jsonArrayMapDescriptor = null;
	JSONObject jsonMapDescriptor = null;
	String[] arRows  = null;
	String divContainer = "";
	String divScreenLayout = ""; 
	String divScreenMapIdentification = ""; 
	String divScreenFields = "";
	String divScreenBms = "";
	
	jsonArrayMapDescriptor = objScreenLayout.getJSONArray("mapDescriptor");
	
	for (Object mapDescriptor : jsonArrayMapDescriptor) {
		jsonMapDescriptor = (JSONObject) mapDescriptor; 
		
		// Div Screen Layout
		arRows = programSummaryScreenLayoutToHTMLUpdRows(jsonMapDescriptor, arRows);  // 24 rows updated 
		divScreenLayout = programSummaryScreenLayoutToHTMLdivScreenLayout(arRows);
		divContainer += divScreenLayout;

		// Div Screen Identification
		divScreenMapIdentification = programSummaryScreenLayoutToHTMLMapIdentification(jsonMapDescriptor);
		divContainer += divScreenMapIdentification;
		
		// Div Screen Fields
		divScreenFields = programSummaryScreenLayoutToHTMLFields(jsonMapDescriptor);
		divContainer += divScreenFields;
		
		// Div Screen Bms 
		divScreenBms = programSummaryScreenLayoutToHTMLBms(jsonMapDescriptor);
		divContainer += divScreenBms;
	}
		
	return divContainer;
}

/*
 * Produzione div Html campi mappa
 */
private String programSummaryScreenLayoutToHTMLFields(JSONObject jsonMapDescriptor) {
	String divScreenFields = "";
	String fldDesc = "";
	String fldInitial = "";
	String fldAttrb = "";
	int fldGrp = 0;
	int fldRow = 0;
	int fldCol = 0;
	int fldLng = 0;
	int fldOcc = 0;
	
	
	divScreenFields = "<div id='divHeader3' class='screenDescriptorHeader' >SCREEN FIELDS </div>"
					+ "<div id='divScreenFields'> "
						+ "<table id='tbScreenFields'>"  
							+ "<tr>"
								+ "<th>Field</th>"
								+ "<th>Desc</th>"
								+ "<th>Row</th>"
								+ "<th>Col</th>"
								+ "<th>Lng</th>"
								+ "<th>Attrb</th>"
								+ "<th>Grp</th>"
								+ "<th>Occ</th>"
								+ "<th>Initial</th>"
							+ "</tr>";
/*	
	  	  <tr>
		      <td  class='tdScreenFields bold' id='field_0' tabindex='0' > ...</td>
		      <td  class='tdScreenFields' id='desc_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='row_0' tabindex='0' > ...</td>
		      <td  class='tdScreenFields' id='col_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='lng_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='attrb_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='grp_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='occ_0' tabindex='0'> ...</td>
		      <td  class='tdScreenFields' id='initial_0' tabindex='0'> ...</td>
	      </tr>
	   </table>
	</div>
*/
	return divScreenFields;
}


/*
 * Produzione div Html identificazione mapp;
 */
private String programSummaryScreenLayoutToHTMLMapIdentification(JSONObject jsonMapDescriptor) {
	String divMapIdentification = "";
	String idMap = jsonMapDescriptor.getString("idObjectMap");
	String idMapset = jsonMapDescriptor.getString("idObjectMapset");
	String form = idMapset +"-" + idMap;
	String fieldCursor = jsonMapDescriptor.getString("fieldCursor");
	
	int rowBegin = jsonMapDescriptor.getInt("rowBegin");
	int rowSize = jsonMapDescriptor.getInt("rowsSize");
	int colBegin = jsonMapDescriptor.getInt("colBegin");
	int colSize = jsonMapDescriptor.getInt("colsSize");
	
	divMapIdentification += "<div id='divHeader2' class='screenDescriptorHeader' >MAP IDENTIFICATION </div>"
	+ "<div id='divScreenDescriptor'>" 
		+ "<table id='tbScreenDescriptor'>" 
		+ "<tr>" 
			+ "<td class='screenDescriptorDetailId'>Map</td>"  
			+ "<td class='screenDescriptorDetailValue' id='idObjectMap' onclick='onclick_showSourceBms(this.id)' >" + idMap + "</td>"                                     
			+ "<td class='screenDescriptorDetailId'>Row Begin</td>"  
			+ "<td class='screenDescriptorDetailValue' id='rowBegin'>" + rowBegin + "</td>"                                     
			+ "<td class='screenDescriptorDetailId'>Rows Size</td>"  
			+ "<td class='screenDescriptorDetailValue' id='rowsSize'>" + rowSize + "</td>"                                     
		+ "</tr>" 
		+ "<tr>"
			+ "<td class='screenDescriptorDetailId'>Mapset</td>" 
			+ "<td class='screenDescriptorDetailValue' id='idObjectMapset' onclick='onclick_showSourceBms(this.id)'>" + idMapset + "</td>"                                    
			+ "<td class='screenDescriptorDetailId'>Col Begin</td>" 
			+ "<td class='screenDescriptorDetailValue' id='colBegin'>" + colBegin + "</td>"                                               
			+ "<td class='screenDescriptorDetailId'>Cols Size</td>" 
			+ "<td class='screenDescriptorDetailValue' id='colsSize'>" + colSize + "</td>"                                    
		+ "</tr>"
			+ "<tr>"
				+ "<td class='screenDescriptorDetailId'>Bms Source</td>" 
				+ "<td class='screenDescriptorDetailValue' id='bmsSource' onclick='onclick_showSourceBms(this.id)'> </td>"                                    
				+ "<td class='screenDescriptorDetailId'>Form</td>" 
				+ "<td class='screenDescriptorDetailValue' id='form'>" + form + "</td>"                                                
				+ "<td class='screenDescriptorDetailId'>Field Cursor</td>" 
				+ "<td class='screenDescriptorDetailValue' id='fieldCursor'>" + fieldCursor + "</td>"                                    
				+ "</tr>"
		+ "</table>"
	+ "</div>";
	
	return divMapIdentification;
}


/*
 * Produzione div completo layout mappa
 */
private String programSummaryScreenLayoutToHTMLdivScreenLayout(String[] arRows) {
	String divScreenLayout = "";
	divScreenLayout = "<div id='divScreenLayout'>";
	divScreenLayout += "<table id='tbScreenLayout'>";
	divScreenLayout += "<tr class='trScreenLayout'> <td onclick='onclick_rowMap(this.id)' id='row_0'  class='tdScreenLayoutTop'> <pre>   </pre> </tr>";
	
	divScreenLayout += programSummaryScreenLayoutToHTMLTr(0, "class='tdScreenLayoutTop'", arRows[0]);
	for (int i = 1; i < 23; i++) {
		divScreenLayout += programSummaryScreenLayoutToHTMLTr(i, "class='tdScreenLayout'", arRows[i]);
	}
	divScreenLayout += programSummaryScreenLayoutToHTMLTr(23, "class='tdScreenLayoutBottom'", arRows[23]);
	
	divScreenLayout += "</table>";
	divScreenLayout += "</div>";
	return divScreenLayout;
}

/*
 * Restituisce una riga <tr> dello screen della mappa
 */
private String programSummaryScreenLayoutToHTMLTr(int i, String idClass, String row) {
	String tr = "";
	tr = "<tr class='trScreenLayout'> <td onclick='onclick_rowMap(this.id)' id='row_" + i + "' " + idClass + ">" + row + " <pre> </pre> </tr> ";	
	return tr;
}


/*
 * Update testo righe mappa 
 */
private String[] programSummaryScreenLayoutToHTMLUpdRows(JSONObject jsonMapDescriptor, String[] arRows) {
	String[] arRowsNew = new String[24];
	
	// TODO
	
	return arRowsNew;
}

/*
 * Produzione Html Bms Rows
 */
private String programSummaryScreenLayoutToHTMLBms(JSONObject jsonMapDescriptor) {
	// TODO Auto-generated method stub
	return null;
}




/*
 * HTML dynamic code nel programma
 * Si restituiscono 3 tabelle
 *   Campi dinamici con stato e valori
 *   Sottocampi con stato e valori
 *   Trasformazioni sottocampo senza dettagli
 */
private String programSummaryDynamicCodeToHTML(JSONObject objDynamicCode) {
    JSONObject jsonDynamicField = null;
    JSONObject jsonDynamicFieldSub = null;
    JSONArray jsonArDynamicFieldSubChains = null;
	String divContainer = "";                 // div header+div table for field/subfield/trasformation
	
	// Scan dynamic instructions
    for (Object dynamicField :  objDynamicCode.getJSONArray("arDynamicField")) {
    	jsonDynamicField = (JSONObject) dynamicField;
    	divContainer = programSummaryDynamicCodeToHTMLField(jsonDynamicField);
    	
    	// Scan subfields
    	for (Object dynamicFieldSub :  jsonDynamicField.getJSONArray("arDynamicFieldSub")) {
    		jsonDynamicFieldSub = (JSONObject) dynamicFieldSub;
     		divContainer += programSummaryDynamicCodeToHTMLFieldSub(jsonDynamicFieldSub);
     		
     		// Chains setting
     		jsonArDynamicFieldSubChains = jsonDynamicFieldSub.getJSONArray("arFieldSubChain");
     		divContainer += programSummaryDynamicCodeToHTMLFieldSubSetting(jsonArDynamicFieldSubChains);    		
    	}    	
    }
	
    return divContainer;     
}

/*
 * tbDynamicField div
 *   tbDynamicField table 
 */
private String programSummaryDynamicCodeToHTMLField(JSONObject objDynamicCode) {
    JSONArray jsonArDynamicField = null;
    JSONObject jsonDynamicField = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
    
	// Dynamic instr/field
    tbltop = "<table id='tbDynamicField' class='tbDynamicField tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Instr</th>"
            +         "<th>Field</th>"
            +         "<th>Solved Def</th>"
            +         "<th>SolvedFull</th>"
            +         "<th>Spread</th>"
            +         "<th>Waiting For Data</th>"
            +         "<th>Value</th>"
            +      "</tr>"
    ;
 	tr = "";
 	
	jsonArDynamicField = objDynamicCode.getJSONArray("arDynamicField");
    for (Object dynamicField : jsonArDynamicField) {
    	
    	jsonDynamicField = (JSONObject) dynamicField;
     	
	 	tr  +=   "<tr>"
            +       "<td>" + jsonDynamicField.get("numInstr") + "</td>" 
            +       "<td>" + jsonDynamicField.get("idField") + "</td>" 
            +       "<td>" + jsonDynamicField.get("isLight") + "</td>" 
            +       "<td>" + jsonDynamicField.get("isSolved") + "</td>" 
            +       "<td>" + jsonDynamicField.get("isSolvedFull") + "</td>" 
            +       "<td>" + jsonDynamicField.get("isWaitingForData") + "</td>" 
            +       "<td>" + jsonDynamicField.get("isWaitingForData") + "</td>" 
            +       "<td>" + "" + "</td>" 
            +    "</tr>"
        ;
	 	for (Object dynamicValue : jsonDynamicField.getJSONArray("arDynamicFieldValue")) {
		 	tr  +=   "<tr>"
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + "</td>" 
		            +       "<td>" + dynamicValue.toString() + "</td>" 
		            +    "</tr>"
		        ;
	 	}
	}
    
    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;		

	divContainer = "<div class='divDynamicField' id='divDynamicField'>"
		         +    tbl
		         + "</div>"
    ;	
	return divContainer;
}

/*
 * HTML singolo sottocampo
 * 
 * restituisce un div contenente:
 *   div con header FIELD SUB
 *   div con tbDynamicFieldSub
 */
private String programSummaryDynamicCodeToHTMLFieldSub(JSONObject objFieldSub) {
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
    
	// Dynamic field sub
    tbltop  = "<table id='tbDynamicFieldSub' class='tbDynamicFieldSub tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Instr</th>"
            +         "<th>Field</th>"
            +         "<th>Subfield</th>"
            +         "<th>Type</th>"
            +         "<th>Pos</th>"
            +         "<th>Length</th>"
            +         "<th>Light</th>"
            +         "<th>Solved</th>"
            +         "<th>Spread</th>"
            +         "<th>WaitingForData</th>"
            +         "<th>Value</th>"
            +      "</tr>"
    ;
 	tr  =   "<tr>"
        +       "<td>" + objFieldSub.get("numInstr") + "</td>" 
        +       "<td>" + objFieldSub.get("idField") + "</td>" 
        +       "<td>" + objFieldSub.get("idSubField") + "</td>" 
        +       "<td>" + objFieldSub.get("typeSubField") + "</td>" 
        +       "<td>" + objFieldSub.get("posSubField") + "</td>" 
        +       "<td>" + objFieldSub.get("sizeSubField") + "</td>" 
        +       "<td>" + objFieldSub.get("isLight") + "</td>" 
        +       "<td>" + objFieldSub.get("isSolved") + "</td>" 
        +       "<td>" + objFieldSub.get("isSolvedFull") + "</td>" 
        +       "<td>" + objFieldSub.get("isSpread") + "</td>" 
        +       "<td>" + objFieldSub.get("isWaitingForData") + "</td>" 
        +       "<td>" + "</td>" 
        +    "</tr>"
    ;
 	for (Object dynamicValue : objFieldSub.getJSONArray("arFieldSubValue")) {
	 	tr  =   "<tr>"
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + "</td>" 
            +       "<td>" + dynamicValue.toString() + "</td>" 
            +    "</tr>"
	        ;
 	}
    
    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;		

    divContainer = "<div class='divDynamicFieldSub' id='divDynamicFieldSub'>"
		         +    tbl
		         + "</div>"
    ;	
	
	return divContainer;
}

/*
 * HTML trasformazioni sottocampo
 * 
 * restituisce un div contenente:
 *   div con header Trasformation
 *   div con tbDynamicFieldSubSetting
 */
private String programSummaryDynamicCodeToHTMLFieldSubSetting(JSONArray arDynamicFieldSubChain) {
    JSONArray jsonArDynamicFieldSubSetting = null;
    JSONObject jsonFieldSubSetting = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String divContainer = "";
    
	// Dynamic field sub setting
    tbltop = "<table id='tbDynamicFieldSubSetting' class='tbDynamicFieldSubSetting tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Instr</th>"
            +         "<th>Field</th>"
            +         "<th>Subfield</th>"
            +         "<th>Chain</th>"
            +         "<th>SetMode</th>"
            +         "<th>PgmSet</th>"
            +         "<th>InstrSet</th>"
            +         "<th>Sender</th>"
            +         "<th>Receiver</th>"
            +      "</tr>"
    ;
 	tr = "";

	// Scan chains 
	for (Object fieldSubChain :  arDynamicFieldSubChain) {
		jsonArDynamicFieldSubSetting = (JSONArray) fieldSubChain;
		
		// Scan chain settings
		for (Object subFieldSetting : jsonArDynamicFieldSubSetting) {
			jsonFieldSubSetting = (JSONObject) subFieldSetting;
		 	tr  +=   "<tr>"
			        +       "<td>" + jsonFieldSubSetting.get("typeProgramOrigin") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("idField") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("idSubField") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("setMode") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("numChain") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("idPgmSet") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("numInstrSet") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("fieldSenderId") + "</td>" 
			        +       "<td>" + jsonFieldSubSetting.get("fieldReceiverId") + "</td>" 
		        +    "</tr>"
		        ;
		} // end-for chain settings		 
	}    		    		
    
    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;		

	divContainer = "<div class='divDynamicFieldSubSetting' id='divDynamicFieldSubSetting'>"
		         +    tbl
		         + "</div>"
    ;	
	return divContainer;
}
/*
 * HTML paragraph/labels/Sections nel programma
 */
private String programSummaryParSectionXrefToHTML(JSONObject objParSectionXref) {
    JSONArray jsonArLabelSection = null;
    JSONArray jsonArXref = null;
    JSONObject jsonLabelSection = null;
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String xref = "";
	String divContainer = "";
	String divContainerAll = "";
	
	// Paragraph/Section Xref
    tbltop = "<table id='tbParSectionXref' class='tbParSectionXref tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Type</th>"
            +         "<th>Def</th>"
            +         "<th>Name</th>"
           +         "<th>Xref</th>"
            +      "</tr>"
    ;
 	
    // Labels
    jsonArLabelSection = objParSectionXref.getJSONArray("arLabel");
    for (Object label : jsonArLabelSection) {
    	jsonLabelSection = (JSONObject) label;
    	
    	// Xref nel programma
    	jsonArXref = jsonLabelSection.getJSONArray("arXref");
    	xref = "";
    	for (Object xrefLabel : jsonArXref) {
    		xref += (int) xrefLabel + " ";
		}
    	   	
	 	tr  +=   "<tr>"
            +       "<td>"  	+ "Label" + "</td>" 
            +       "<td>"  	+ jsonLabelSection.getInt("numDefInstr")  + "</td>" 
            +       "<td><pre>" + jsonLabelSection.getString("label") 	  + "</pre></td>" 
            +       "<td>"      + xref + "</td>" 
            +    "</tr>"
        ;
	}

	divContainer = "<div class='divTbLabelXref' id='divTbLabelXref'>"
		         +    tbl
		         + "</div>"
    ;	
	divContainerAll = divContainer;
			
    // Sections
    jsonArLabelSection = objParSectionXref.getJSONArray("arSection");
    for (Object section : jsonArLabelSection) {
    	jsonLabelSection = (JSONObject) section;
    	
    	// Xref nel programma
    	jsonArXref = jsonLabelSection.getJSONArray("arXref");
    	xref = ""; 
    	for (Object xrefSection : jsonArXref) {
    		xref += (int) xrefSection + " ";
		}
    	   	
	 	tr  +=   "<tr>"
            +       "<td>"      + "Section" + "</td>" 
            +       "<td>"      + jsonLabelSection.getInt("numDefInstr")  + "</td>" 
            +       "<td><pre>" + jsonLabelSection.getString("section")   + "</pre></td>" 
            +       "<td>"      + xref + "</td>" 
            +    "</tr>"
	        ;
	}
    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 

	divContainer = "<div class='divTbSectionXref' id='divTbSectionXref'>"
		         +    tbl
		         + "</div>"
	;	

	divContainerAll = divContainerAll + divContainer;
    return divContainer;     
}

/*
 * HTML Xref simboli nel programma
 */
private String programSummarySymbolsXrefToHTML(JSONObject objSymbolsXref) {
    JSONObject jsonSymbol = null;
    JSONArray jsonArSymbol = null;
    JSONArray jsonArDef = null;               				// In Env|Data|Proc
    @SuppressWarnings("unused")
	JSONArray jsonArXrefToSymbolInEnvInp = null;            //  
    JSONArray jsonArXrefToSymbolInDataInp = null;           //  
    JSONArray jsonArXrefToSymbolInProcInp = null;           //  
    JSONArray jsonArXrefToSymbolInProcOut = null;           //  
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String xrefData = "";
	String xrefProcInp = "";
	String xrefProcOut = "";
	String divContainer = "";
	String symbol = "";
	String symbolType = "";
	String divDef = "";
	String instrDef = "";
	
    tbltop = "<table id='tbSymbols' class='tbSymbols tbPgmSummary'>"
            +      "<tr>"
            +         "<th>Symbol</th>"
            +         "<th>Type</th>"
            +         "<th>Area Def</th>"
            +         "<th>Instr</th>"
            +         "<th>Xref Data</th>"
            +         "<th>Xref Proc</th>"
            +         "<th>Instr</th>"
            +      "</tr>"
    ;
 	
    jsonArSymbol = objSymbolsXref.getJSONArray("arSymbol");
    for (Object oSymbol : jsonArSymbol) {
    	jsonSymbol = (JSONObject) oSymbol;
    	symbol = jsonSymbol.getString("symbol");
    	symbolType = jsonSymbol.get("symbolType").toString();   	
    	divDef = jsonSymbol.getString("divDef");
    	
    	if (divDef.equals("ENV")) {
    		jsonArDef = jsonSymbol.getJSONArray("arPointerToDefInEnv");
		}
    	if (divDef.equals("DATA")) {
    		jsonArDef = jsonSymbol.getJSONArray("arPointerToDefInData");
    		jsonArXrefToSymbolInDataInp = jsonSymbol.getJSONArray("arXrefToSymbolInDataInp");
 		}
    	if (divDef.equals("PROC")) {
    		jsonArDef = jsonSymbol.getJSONArray("arPointerToDefInProc");
		}
    	
    	instrDef = "";
    	for (Object numInstrDef : jsonArDef) {
    		instrDef += (int)numInstrDef + " ";
		}
    	xrefData = "";
    	for (Object numXrefData : jsonArXrefToSymbolInDataInp) {
    		xrefData += (int) numXrefData + " ";
		}
    	
    	xrefProcInp = "";
    	jsonArXrefToSymbolInProcInp = jsonSymbol.getJSONArray("arXrefToSymbolInProcInp");
    	for (Object numXrefProcInp : jsonArXrefToSymbolInProcInp) {
    		xrefProcInp += (int) numXrefProcInp + " ";
		}
    	
    	xrefProcOut = "";
    	jsonArXrefToSymbolInProcOut = jsonSymbol.getJSONArray("arXrefToSymbolInProcOut");
    	for (Object numXrefProcOut : jsonArXrefToSymbolInProcOut) {
    		xrefProcOut += (int) numXrefProcOut + " ";
		}
    	
//    	if (!xrefProcInp.isBlank()) {
        if (xrefProcInp.trim().length() != 0) {
    	 	tr  +=   "<tr>"
    	            +       "<td>" + symbol 	+ "</td>" 
    	            +       "<td>" + symbolType + "</td>" 
    	            +       "<td>" + divDef   	+ "</td>" 
    	            +       "<td>" + instrDef 	+ "</td>" 
    	            +       "<td>" + xrefData 	+ "</td>" 
    	            +       "<td>" + "Input"  	+ "</td>" 
    	            +       "<td>" + xrefProcInp + "</td>" 
    	            +    "</tr>"
    	        ;
    	 	symbol = "";
       	 	symbolType = "";
       	 	divDef = "";
    	 	instrDef = "";
    	 	xrefData = "";
		}
    	if (!xrefProcOut.isBlank()) {
    	 	tr  +=   "<tr>"
	            +       "<td>" + symbol 	+ "</td>" 
	            +       "<td>" + symbolType + "</td>" 
	            +       "<td>" + divDef   	+ "</td>" 
	            +       "<td>" + instrDef 	+ "</td>" 
	            +       "<td>" + xrefData 	+ "</td>" 
	            +       "<td>" + "Ouput" + "</td>" 
	            +       "<td>" + xrefProcOut + "</td>" 
	            +    "</tr>"
    	        ;
		}
	}
 
    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;
    
	divContainer = "<div class='divTbSymbolsXref' id='divTbSymbolsXref'>"
		         +    tbl
		         + "</div>"
    ;	

    return divContainer; 
    
}

/*
 * HTML dead code label/section/istruzioni
 */
private String programSummaryDeadCodeToHTML(JSONObject objDeadCode) {
	String tbl = "";
    String tbltop = "";
    String main = "";
    String tblBottom = "";
	String tr = "";
	String typeDead = "";
	String divContainer = "";
	
    tbltop = "<table id='tbDeadCode' class='tbDeadCode tbPgmSummary'>"
            +      "<tr>"
            +          "<th>Type</th>"
            +          "<th>Reference</th>"
            +      "</tr>"
    ;
 	
    typeDead = "FIELD";
    for (Object item : objDeadCode.getJSONArray("arDeadDataItem")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    typeDead = "LABEL";
    for (Object item : objDeadCode.getJSONArray("arDeadLabel")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    typeDead = "SECTION";
    for (Object item : objDeadCode.getJSONArray("arDeadSection")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    typeDead = "COPY DATA";
    for (Object item : objDeadCode.getJSONArray("arDeadCopyData")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    typeDead = "COPY PROC";
    for (Object item : objDeadCode.getJSONArray("arDeadCopyProc")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    typeDead = "PROC INSTR";
    for (Object item : objDeadCode.getJSONArray("arDeadProcInstr")) {
	 	tr  +=   "<tr>"
            +       "<td>" + typeDead + "</td>" 
            +       "<td>" + (String) item + "</td>" 
            +    "</tr>"
        ;
	 	typeDead = "";
	}

    main = tr;
    tblBottom = "</table>";
    tbl = tbltop + main + tblBottom;	 
	
    divContainer = "<div class='divDeadCode' id='divDeadCode'>"
		         +    tbl
		         + "</div>"
		         ;	
    return divContainer;   
}

/*
 * HTML metriche
 */
private String programSummaryMetricsToHTML(JSONObject objMetrics) {
	StringBuffer sb =null;
	String divContainer = "";
	
	sb = new StringBuffer();
	
	// Sources Counters
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Sources Counters</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Pgms Analyzed</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntPgmAnalyzed'>" + objMetrics.get("cntPgmAnalyzed") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Pgms Called By Cics Link/Xctl/Batch Exec</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntPgmExecuted'>" + objMetrics.get("cntPgmExecuted") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Subprograms Analyzed</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntSubPgmAnalyzed'>" + objMetrics.get("cntSubPgmAnalyzed") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Subprograms Executed By Call</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntSubPgmExecuted'>" + objMetrics.get("cntSubPgmExecuted") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Copy Defined</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntCopyDefined'>" + objMetrics.get("cntCopyDefined") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Sources Containing Jcl Jobs</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntJclJob'>" + objMetrics.get("cntJclJob") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Sources Containing Jcl Include</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntJclInclude'>" + objMetrics.get("cntJclInclude") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Sources Containing Jcl Proc</td>");
	sb.append("<td class='clsC2MetricValue'><div id='cntJclProc'>" + objMetrics.get("cntJclProc") + "</div></td>");
	sb.append("</tr>");	
	sb.append("</table>");

	// Sources Dimensional Counters
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Sources Dimensional Counters</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Logical Code lines, no comments no blank</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesCodeLogical'>" + objMetrics.get("sizeLinesCodeLogical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Phisical Code lines, yes comments yes blank</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesCodePhisical'>" + objMetrics.get("sizeLinesCodePhisical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blank Code Lines</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesBlank'>" + objMetrics.get("sizeLinesBlank") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blank Code Lines in Procedure Division</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesBlankProc'>" + objMetrics.get("sizeLinesBlankProc") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blank Code Lines in Data Division</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesBlankData'>" + objMetrics.get("sizeLinesBlankData") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comment Code Lines</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesComment'>" + objMetrics.get("sizeLinesComment") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comment Code Lines in Procedure Division</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesCommentProc'>" + objMetrics.get("sizeLinesCommentProc") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comment Code Lines in Data Division</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeLinesCommentData'>" + objMetrics.get("sizeLinesCommentData") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Procedure Division Instructions</td>");
	sb.append("<td class='clsC2MetricValue'><div id='sizeInstr'>" + objMetrics.get("sizeInstr") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");		

	// Estimated Development Time
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Estimated Development Time</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Estimated Function Point</td>");
	sb.append("<td class='clsC2MetricValue'><div id='backFiredFunctionPoint'>" + objMetrics.get("backFiredFunctionPoint") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Estimated Dev Days By Avg Productivity</td>");
	sb.append("<td class='clsC2MetricValue'><div id='timeDevelopment'>" + objMetrics.get("timeDevelopment") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");				

	// Data Definition Measures
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Data Definition Measures</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Fields Defined</td>");
	sb.append("<td class='clsC2MetricValue'><div id='defFields'>" + objMetrics.get("defFields") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Fields defined Hidden In Copy Books</td>");
	sb.append("<td class='clsC2MetricValue'><div id='defFieldsInCopy'>" + objMetrics.get("defFieldsInCopy") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Literals Found in Instructions</td>");
	sb.append("<td class='clsC2MetricValue'><div id='defLiterals'>" + objMetrics.get("defLiterals") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");	
	
    // Documentation Data
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Documentation Data</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comm % Based On Logical Sources Instr Rows</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percComByLogical'>" + objMetrics.get("percComByLogical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comm % Based On Phisical Sources Instr Rows</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percComByPhisical'>" + objMetrics.get("percComByPhisical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Comm % By Instruction</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percComByInstruction'>" + objMetrics.get("percComByInstruction") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blank Rows % Based On Phisical Sources Instr Rows</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percBlankByPhisical'>" + objMetrics.get("percBlankByPhisical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blank Rows % By Instruction</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percBlankByInstruction'>" + objMetrics.get("percBlankByInstruction") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");	

	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Dynamic Code Information</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Pgms With Dynamic Code</td>");
	sb.append("<td class='clsC2MetricValue'><div id='dynamicPgm'>" + objMetrics.get("dynamicPgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Dynamic Instructions</td>");
	sb.append("<td class='clsC2MetricValue'><div id='dynamicInstr'>" + objMetrics.get("dynamicInstr") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Dynamic Instructions Light (With Value)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='dynamicInstrLight'>" + objMetrics.get("dynamicInstrLight") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Percent Dynamic Instructions</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percDynamicInstr'>" + objMetrics.get("percDynamicInstr") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Percent Dynamic Instructions Light</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percDynamicInstrLight'>" + objMetrics.get("percDynamicInstrLight") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");		

	// Violations Detected
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Violations Detected </th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Violations Detected</td>");
	sb.append("<td class='clsC2MetricValue'><div id='violations'>" + objMetrics.get("violations") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Violations % Detected Based On Logical Instr</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percViolationsByLogical'>" + objMetrics.get("percViolationsByLogical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Violation % Detected Based On Phisical Instr</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percViolationsByPhisical'>" + objMetrics.get("percViolationsByPhisical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Violations % By Instruction</td>");
	sb.append("<td class='clsC2MetricValue'><div id='percViolationsByInstruction'>" + objMetrics.get("percViolationsByInstruction") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");

	// Dead Code Measures
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Dead Code Measures </th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Unused Data Fields Defined</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadFields'>" + objMetrics.get("deadFields") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Unreferenced Procedures</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadSubGraph'>" + objMetrics.get("deadSubGraph") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Dead Instructions Never Reached</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadInstr'>" + objMetrics.get("deadInstr") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Unreferenced Labels</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadLabels'>" + objMetrics.get("deadLabels") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Unreferenced Copy Books in Data Div</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadCopyData'>" + objMetrics.get("deadCopyData") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Unreferenced Copy Books in Proc Div</td>");
	sb.append("<td class='clsC2MetricValue'><div id='deadCopyProc'>" + objMetrics.get("deadCopyProc") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");
	
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Jcl Counters</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>DD defined </td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclDD'>" + objMetrics.get("jclDD") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Steps Of Execution</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclStepDefined'>" + objMetrics.get("jclStepDefined") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>TSteps In Update</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclStepUpdate'>" + objMetrics.get("jclStepUpdate") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Dsname Declared</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclDsname'></div>" + objMetrics.get("jclDsname") + "</td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Dsname Referenced</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclDsnameReferenced'>" + objMetrics.get("jclDsnameReferenced") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Dsname UnReferenced</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclDsnameUnReferenced'>" + objMetrics.get("jclDsnameUnReferenced") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Jcl Include</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclIncludeCalled'>" + objMetrics.get("jclIncludeCalled") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Jcl Proc Called</td>");
	sb.append("<td class='clsC2MetricValue'><div id='jclProcCalled'>" + objMetrics.get("jclProcCalled") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");	

    // Structural And Functional Complexity
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>Structural And Functional Complexity</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Fan-In Pgms Called By</td>");
	sb.append("<td class='clsC2MetricValue'><div id='structFanIn'>" + objMetrics.get("structFanIn") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Fan-Out Pgms Called</td>");
	sb.append("<td class='clsC2MetricValue'><div id='structFanOut'>" + objMetrics.get("structFanOut") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Cobol Procedure Sections</td>");
	sb.append("<td class='clsC2MetricValue'><div id='structSections'>" + objMetrics.get("structSections") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Cobol Procedure Paragraph</td>");
	sb.append("<td class='clsC2MetricValue'><div id='structParagraphs'>" + objMetrics.get("structParagraphs") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");		

	// Generic Functional Complexity
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>Generic Functional Complexity</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Objects</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcObjects'>" + objMetrics.get("funcObjects") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Relations Between Objects</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcRelations'>" + objMetrics.get("funcRelations") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Internal Cics Trnids By Cics Start/Return Transid</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcTranInternal'>" + objMetrics.get("funcTranInternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>External Cics Trnids By Cics Start/Return Transid</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcTranExternal'>" + objMetrics.get("funcTranExternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Screens (Cics Bms)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcMap'>" + objMetrics.get("funcMap") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Procedure Calls To Internal Modules</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcCallInternal'>" + objMetrics.get("funcCallInternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Procedure Calls To External Modules</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcCallExternal'>" + objMetrics.get("funcCallExternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Db Internal Table Accesses</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcAccEntityInternal'>" + objMetrics.get("funcAccEntityInternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Db External Table Accesses</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcAccEntityExternal'>" + objMetrics.get("funcAccEntityExternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total Internal Vsam/Seq File System  Accesses</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcAccMediaInternal'>" + objMetrics.get("funcAccMediaInternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Total External Vsam/Seq File System  Accesses</td>");
	sb.append("<td class='clsC2MetricValue'><div id='funcAccMediaExternal'>" + objMetrics.get("funcAccMediaExternal") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");	
	
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>Function Points Functional Complexity</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td  class='clsC1MetricValue'>External Tables/files Output</td>");	
	sb.append("<td  class='clsC2MetricValue'><div id='fpExternalOutputEO'>" + objMetrics.get("fpExternalOutputEO") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td  class='clsC1MetricValue'>Internal Tables/Files Input</td>");	
	sb.append("<td  class='clsC2MetricValue'><div id='fpExternalInputEI'>" + objMetrics.get("fpExternalInputEI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td  class='clsC1MetricValue'>External Tables/Files Inquiry (By ILF, EIF)</td>");	
	sb.append("<td  class='clsC2MetricValue'><div id='fpExternalInquiryEQ'>" + objMetrics.get("fpExternalInquiryEQ") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td  class='clsC1MetricValue'>Internal Tables/Files Accesses (ILF)</td>");	
	sb.append("<td  class='clsC2MetricValue'><div id='fpInternalLogicalFileILF'>" + objMetrics.get("fpInternalLogicalFileILF") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td  class='clsC1MetricValue'>External Tables/Files Accesses (EIF)</td>");	
	sb.append("<td  class='clsC2MetricValue'><div id='fpExternalInterfaceFileEIF'>" + objMetrics.get("fpExternalInterfaceFileEIF") + "</div></td>");	
	sb.append("</tr>");
	sb.append("</table>");				  

	// Rehosting  Functional/Technical Complexity
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
    sb.append("<tr>");
    sb.append("<th colspan='2'>Rehosting  Functional/Technical Complexity</th>");
    sb.append("</tr>");
    sb.append("<tr>");
    sb.append("<td class='clsC1MetricValue'>Rate Objects/Relations</td>");
    sb.append("<td class='clsC2MetricValue'><div id='rhRateObjectRelation'>" + objMetrics.get("rhRateObjectRelation") + "</div></td>");
    sb.append("</tr>");
    sb.append("<tr>");
    sb.append("<td class='clsC1MetricValue'>Total Objects internal to Scope</td>");
    sb.append("<td class='clsC2MetricValue'><div id='rhObjectsInternal'>" + objMetrics.get("rhObjectsInternal") + "</div></td>");
    sb.append("</tr>");
    sb.append("<tr>");
    sb.append("<td class='clsC1MetricValue'>Total Objects External to Scope</td>");
    sb.append("<td class='clsC2MetricValue'><div id='rhObjectsExternal'>" + objMetrics.get("rhObjectsExternal") + "</div></td>");
    sb.append("</tr>");
    sb.append("<tr>");
    sb.append("<td class='clsC1MetricValue'>Not Portable Objects (Asm, PL/1, Load Module,..)</td>");
    sb.append("<td class='clsC2MetricValue'><div id='rhObjectsUnportable'>" + objMetrics.get("rhObjectsUnportable") + "</div></td>");
    sb.append("</tr>");
    sb.append("<tr>");
    sb.append("<td class='clsC1MetricValue'>Binary Files</td>");
    sb.append("<td class='clsC2MetricValue'><div id='rhFilesBynary'>" + objMetrics.get("rhFilesBynary") + "</div></td>");
    sb.append("</tr>");
    sb.append("</table>");		

    // Cyclomatic Complexity
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>Cyclomatic Complexity</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Arcs Number</td>");
	sb.append("<td class='clsC2MetricValue'><div id='mcCabeArcs'>" + objMetrics.get("mcCabeArcs") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Nodes Number</td>");
	sb.append("<td class='clsC2MetricValue'><div id='mcCabeNodes'>" + objMetrics.get("mcCabeNodes") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Connected Subgraphs (Cobol Section/paragraph)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='mcCabeGraphConn'>" + objMetrics.get("mcCabeGraphConn") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Conditional Operators OR AND</td>");
	sb.append("<td class='clsC2MetricValue'><div id='mcCabeOperatorsOrAnd'>" + objMetrics.get("mcCabeOperatorsOrAnd") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");			

	// Halstead Complexity (Software Science)
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>Halstead Complexity (Software Science)</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Distinct Program Operators (n1)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadOperators'>" + objMetrics.get("halsteadOperators") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Distinct Program Operands (n2)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadOperands'>" + objMetrics.get("halsteadOperands") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Operators Occurrences (N1)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadOperatorsOcc'>" + objMetrics.get("halsteadOperatorsOcc") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Operands Occurrences (N2)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadOperandsOcc'>" + objMetrics.get("halsteadOperandsOcc") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Program Length</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadLengthPgm'>" + objMetrics.get("halsteadLengthPgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Program Vocabulary</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadVocabularyPgm'>" + objMetrics.get("halsteadVocabularyPgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Program Volume</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadVolumePgm'>" + objMetrics.get("halsteadVolumePgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Program difficulty</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadDifficultPgm'>" + objMetrics.get("halsteadDifficultPgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Program Effort</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadEffortPgm'>" + objMetrics.get("halsteadEffortPgm") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Estimated Writing Time (sec)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='halsteadTimeWriting'>" + objMetrics.get("halsteadTimeWriting") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");			
	
	// Total Complexity/Maintenance/Testability Indexes (sum internal procedures)
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Total Complexity/Maintenance/Testability Indexes (sum internal procedures)</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Maintenability (By Oman and Hagemeister 1991)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMITot'>" + objMetrics.get("idxMITot") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Function Points</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxFPTot'>" + objMetrics.get("idxFPTot") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>McCabe Cyclomatic Complexity</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMcCabeTot'>" + objMetrics.get("idxMcCabeTot") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rehosting Effort</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxReHostingTot'>" + objMetrics.get("idxReHostingTot") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");

	// Max Complexity/Maintenance/Testability Indexes
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Max Complexity/Maintenance/Testability Indexes</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Maintenability (By Oman and Hagemeister 1991)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMIHigh'>" + objMetrics.get("idxMIHigh") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Function Points</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxFPHigh'>" + objMetrics.get("idxFPHigh") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>McCabe Cyclomatic Complexity</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMcCabeHigh'>" + objMetrics.get("idxMcCabeHigh") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rehosting Effort</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxReHostingHigh'>" + objMetrics.get("idxReHostingHigh") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");			  

	// Min Complexity/Maintenance/Testability Indexes
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Min Complexity/Maintenance/Testability Indexes</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Maintenability (By Oman and Hagemeister 1991)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMILow'>" + objMetrics.get("idxMILow") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Function Points</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxFPLow'>" + objMetrics.get("idxFPLow") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>McCabe Cyclomatic Complexity</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMcCabeLow'>" + objMetrics.get("idxMcCabeLow") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rehosting Effort</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxReHostingLow'>" + objMetrics.get("idxReHostingLow") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");		

	// Avg Complexity/Maintenance/Testability Indexes
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>Avg Complexity/Maintenance/Testability Indexes</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Maintenability (By Oman and Hagemeister 1991)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMIAvg'>" + objMetrics.get("idxMIAvg") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Function Points</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxFPAvg'>" + objMetrics.get("idxFPAvg") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>McCabe Cyclomatic Complexity</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxMcCabeAvg'>" + objMetrics.get("idxMcCabeAvg") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rehosting Effort</td>");
	sb.append("<td class='clsC2MetricValue'><div id='idxReHostingAvg'>" + objMetrics.get("idxReHostingAvg") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");			
	
	// SQUALE Violations Counters by category and severity
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Violations Counters by category and severity</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Blocking </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleViolationsBlocker'>" + objMetrics.get("squaleViolationsBlocker") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Critical</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleViolationsCritical'>" + objMetrics.get("squaleViolationsCritical") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Major</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleViolationsMajor'>" + objMetrics.get("squaleViolationsMajor") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Minor</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleViolationsMinor'>" + objMetrics.get("squaleViolationsMinor") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Info</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleViolationsInfo'>" + objMetrics.get("squaleViolationsInfo") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");	
	
	// SQUALE General Values
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE General Values</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rating Level A-E</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSSRL'>" + objMetrics.get("squaleSSRL") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rating Value (SQI/Estimated Dev Time)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSSRI'>" + objMetrics.get("squaleSSRI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Rule Compliance</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSSCI'>" + objMetrics.get("squaleSSCI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>Absolute Remediation Cost (SQTI+SQRI+..SQPI)</td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSSQI'>" + objMetrics.get("squaleSSQI") + "</div></td>");
	sb.append("</tr>");
    sb.append("</table>");	
	
    // SQUALE Absolute Indexs Detail Values SSQI SQxI
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Absolute Indexs Detail Values SSQI SQxI</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQTI)Testability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQTI'>" + objMetrics.get("squaleSQTI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQRI)Reliability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQRI'>" + objMetrics.get("squaleSQRI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQCI)Changeability</td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQCI'>" + objMetrics.get("squaleSQCI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQEI)Efficiency </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQEI'>" + objMetrics.get("squaleSQEI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQSI)Security </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQSI'>" + objMetrics.get("squaleSQSI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQMI)Maintenability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQMI'>" + objMetrics.get("squaleSQMI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SQPI)Portability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSQPI'>" + objMetrics.get("squaleSQPI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("</table>");		
 
	// SQUALE Consolidated Index Detail Values SCxI
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Consolidated Index Detail Values SCxI</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCTI)Testability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCTI'>" + objMetrics.get("squaleSCTI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCRI)Reliability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCRI'>" + objMetrics.get("squaleSCRI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCCI)Changeability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCCI'>" + objMetrics.get("squaleSCCI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCEI)Efficiency </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCEI'>" + objMetrics.get("squaleSCEI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCSI)Security </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCSI'>" + objMetrics.get("squaleSCSI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCMI)Maintenability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCMI'>" + objMetrics.get("squaleSCMI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCPI)Portability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSCPI'>" + objMetrics.get("squaleSCPI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("</table>");			

	// SQUALE Density Index Detail Values SDxI
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Density Index Detail Values SDxI</th>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SDTI)Testability </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDTI'>" + objMetrics.get("squaleSDTI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SDRI)Reliability </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDRI'>" + objMetrics.get("squaleSDRI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SDCI)Changeability </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDCI'>" + objMetrics.get("squaleSDCI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCEI)Efficiency </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDEI'>" + objMetrics.get("squaleSDEI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SCEI)Security </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDSI'>" + objMetrics.get("squaleSDSI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SDMI)Maintenability </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDMI'>" + objMetrics.get("squaleSDMI") + "</div></td>");
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SDPI)Portability </td>");
	sb.append("<td class='clsC2MetricValue'><div id='squaleSDPI'>" + objMetrics.get("squaleSDPI") + "</div></td>");
	sb.append("</tr>");
	sb.append("</table>");			
    
	// SQUALE Rating Index Detail Values SRxI
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Rating Index Detail Values SRxI</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRTI)Testability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRTI'>" + objMetrics.get("squaleSRTI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRRI)Reliability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRRI'>" + objMetrics.get("squaleSRRI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRCI)Changeability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRCI'>" + objMetrics.get("squaleSRCI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SREI)Efficiency </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSREI'>" + objMetrics.get("squaleSREI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRSI)Security </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRSI'>" + objMetrics.get("squaleSRSI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRMI)Maintenability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRMI'>" + objMetrics.get("squaleSRMI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRPI)Portability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRPI'>" + objMetrics.get("squaleSRPI") + "</div></td>");	
	sb.append("</tr>");
	sb.append("</table>");			
	
	// SQUALE Level Detail Values SRxL (Rating Level A-E)
	sb.append("<table class='tbMetricSection' id='ObjectMetricValueTable'>");	
	sb.append("<tr>");
	sb.append("<th colspan='2'>SQUALE Level Detail Values SRxL (Rating Level A-E)</th>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRTL)Testability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRTL'>" + objMetrics.get("squaleSRTL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRRL)Reliability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRRL'>" + objMetrics.get("squaleSRRL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRCL)Changeability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRCL'>" + objMetrics.get("squaleSRCL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SREL)Efficiency </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSREL'>" + objMetrics.get("squaleSREL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRSL)Security </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRSL'>" + objMetrics.get("squaleSRSL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRML)Maintenability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRML'>" + objMetrics.get("squaleSRML") + "</div></td>");	
	sb.append("</tr>");
	sb.append("<tr>");
	sb.append("<td class='clsC1MetricValue'>(SRPL)Portability </td>");	
	sb.append("<td class='clsC2MetricValue'><div id='squaleSRPL'>" + objMetrics.get("squaleSRPL") + "</div></td>");	
	sb.append("</tr>");
	sb.append("</table>");		  	
	
	divContainer = sb.toString();
	return divContainer;
}

/*
 * HTML Quality
 *  - Fattori di qualita
 *  - Caratteristiche di qualita
 */
private String programSummaryQualityToHTML(JSONObject objQuality) {
	String divContainer = "";
	// TODO
	return divContainer;
}

/*
 * HTML Violations
 *  - Rule violated
 */
private String programSummaryViolationsToHTML(JSONObject objViolations) {
	String divContainer = "";
	// TODO
	return divContainer;
}


/*
 *info generali amrita
 */
private void programSummaryInfoAmrita(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) {
	String amritaVersion = "";
	String amritaLastMod = "";
//	amritaVersion = ucfg.getAmritaVersion();
//	amritaLastMod = ucfg.getAmritaLastMod();	
	obj.put("version", amritaVersion);
	obj.put("lastMod", amritaLastMod);
}

/*
 * Informazioni generali source
 */
private void programSummaryInfoSource(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
    EntityObject entityObject = null;
    boolean isPgmFound = false;
	
	Connection conn = DataBaseConnections.getConnection();
	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
	
	entityObject = new EntityObject();
	entityObject.setSystem(sys);
	entityObject.setSubSystem(subSys);
	entityObject.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
	entityObject.setIdObject(idProgram);
	
	isPgmFound = eoDAO.read(entityObject);
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);
	
	// Programma non trovato, errore di programma
	if (!isPgmFound) {
		throw new ExceptionAmrita(EnumAmritaExceptionError.ERROR_SQL);
	}
	
	obj.put("companyCode", ucfg.getCompanyCode());
	obj.put("company", ucfg.getCompany());
	obj.put("sys", entityObject.getSystem());
	obj.put("subSys", entityObject.getSubSystem());
	obj.put("subSysOwner", entityObject.getSubSystemOwner());
	obj.put("pgmName", entityObject.getIdObject());
	obj.put("pgmType", entityObject.getTypeObject());
	obj.put("status", entityObject.getStatus());
	obj.put("author", entityObject.getAuthor());
	obj.put("dateWritten", entityObject.getDateWritten());
	obj.put("sourceFileName", entityObject.getFileSource());                      // File source name
	obj.put("sourceFileSuffix", entityObject.getSuffixFileSource());              // File source suffix
	obj.put("sourceLibraryDir", entityObject.getLibraryDir());                    // Library source Path
	obj.put("sourceLibraryCode", entityObject.getLibrarySourceObject());          // Library source code where the pgm has been found
	obj.put("sourceLibraryCode", entityObject.getLibrarySource());                // Library source code Path
	obj.put("dtFirstAnalysis", entityObject.getDtFirstAnalysis());
	obj.put("dtFirstAnalysis", entityObject.getDtLastAnalysis());
	obj.put("tmFirstAnalysis", entityObject.getTmFirstAnalysis());
	obj.put("tmLastAnalysis", entityObject.getTmLastAnalysis());
	
	obj.put("statusOrdinal", entityObject.getStatus().ordinal());
	
}

/*
 * Opzioni programma
 */
private void programSummaryProgramOptions(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
    ArrayList<EntityObjectOption> al_entityObjectOption = null;
    JSONArray arJsonOption = null;
    JSONObject jsonObject = null;
	
	Connection conn = DataBaseConnections.getConnection();
	IDAOObjectOption eoDAO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false, false, ucfg);
	
	al_entityObjectOption = (ArrayList<EntityObjectOption>) eoDAO.findAll(sys, subSys, EnumObject.OBJECT_PGM_COBOL, idProgram);
	
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);

	arJsonOption = new JSONArray();
	
	for (EntityObjectOption eo: al_entityObjectOption) {
		jsonObject = new JSONObject();
		jsonObject.put("option", eo.getOption());
		jsonObject.put("optionOrdinal", eo.getOption().ordinal());
		arJsonOption.put(jsonObject);
	}	
	obj.put("arOption", arJsonOption);
}

/*
 * Relazioni programma con relative origini
 */
private void programSummaryRelationships(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
    ArrayList<EntityRelation> al_entityRelation = null;
    ArrayList<EntityRelation> al_entityRelationReverse = null;
    JSONArray arJsonRelation = null;
    JSONArray arJsonRelationOrigin = null;
    JSONObject jsonObject = null;
	
	Connection conn = DataBaseConnections.getConnection();
	IDAORelation eoDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false, false, ucfg);
	
	al_entityRelation = (ArrayList<EntityRelation>)        eoDAO.findAllBySubSysIdA(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, null);
	al_entityRelationReverse = (ArrayList<EntityRelation>) eoDAO.findAllBySubSysIdB(sys, subSys, idProgram, EnumObject.OBJECT_PGM_COBOL, null);
	al_entityRelation.addAll(al_entityRelationReverse);
	
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);

	arJsonRelation = new JSONArray();
	
	for (EntityRelation eo: al_entityRelation) {
		jsonObject = new JSONObject();
		
		// Relazione
		jsonObject.put("relation", eo.getRelation());
		jsonObject.put("idObjectRelated", eo.getIdObjectB());
		jsonObject.put("relationOrdinal", eo.getRelation().ordinal());
		
		// Origini relazione
		arJsonRelationOrigin = new JSONArray();
 		programSummaryRelationOrigin(arJsonRelationOrigin, eo, ucfg);
		jsonObject.put("arRelationOrigin", arJsonRelationOrigin);
		
		arJsonRelation.put(jsonObject);
	}	
	obj.put("arRelation", arJsonRelation);
}

/*
 * Origine specifica relazione
 */
private void programSummaryRelationOrigin(JSONArray arJsonRelationOrigin, EntityRelation eor, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
    ArrayList<EntityRelationOrigin> al_entityRelationOrigin = null;
    JSONObject jsonObject = null;
    
	Connection conn = DataBaseConnections.getConnection();
	IDAORelationOrigin eoDAO = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false, false, ucfg);
	
	al_entityRelationOrigin = (ArrayList<EntityRelationOrigin>) eoDAO.findAllOrigin(eor.getSystem(), eor.getSubSystem(), eor.getIdObjectA(), eor.getTypeObjectA(), eor.getRelation(), eor.getIdObjectB(), eor.getTypeObjectB());
	
	DataBaseConnections.releaseConnection(conn);
	eoDAO.setConn(null);
	
	// Scan origini relazione
	for (EntityRelationOrigin eoo : al_entityRelationOrigin) {
		jsonObject = new JSONObject();
		jsonObject.put("numInstrOrigin", eoo.getNumInstrOrigin());
		jsonObject.put("rowStart", eoo.getRowStart());
		jsonObject.put("rowEnd", eoo.getRowEnd());
		jsonObject.put("copyOrigin", eoo.getCopyOrigin());
		jsonObject.put("rowStartInCopy", eoo.getRowStartInCopy());
		jsonObject.put("rowEndInCopy", eoo.getRowEndInCopy());
		jsonObject.put("instrCategory", eoo.getInstrCategory());
		jsonObject.put("instrProgramArea", eoo.getInstrProgramArea());
		jsonObject.put("instrTypePrecompiler", eoo.getInstrTypePrecompiler());
		jsonObject.put("instrLang", eoo.getInstrLang());
		jsonObject.put("relationType", eoo.getRelationType());
		jsonObject.put("relationSource", eoo.getRelationSource());	
		jsonObject.put("instrCategoryOrdinal", eoo.getInstrCategory().ordinal());
		jsonObject.put("instrProgramAreaOrdinal", eoo.getInstrProgramArea().ordinal());
		jsonObject.put("instrTypePrecompilerOrdinal", eoo.getInstrTypePrecompiler().ordinal());
		jsonObject.put("relationTypeOrdinal", eoo.getRelationType().ordinal());
		jsonObject.put("relationSourceOrdinal", eoo.getRelationSource().ordinal());
		arJsonRelationOrigin.put(jsonObject);
	}
}

/*
 * Copy Cobol & Sql Include in procedure/data
 */
private void programSummaryCopySqlInclude(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) {
	List<ProgramCopyInclude> al_pgmCopyInclude = null;
	JSONArray jsonArrayCopy = null;
	JSONArray jsonArraySqlInclude = null;
	JSONObject jsonObject = null;
	String division = "*";      // Copy di tutte le division cobol
    String typeCopy = "C";      // Copy + Sql Include
    
	al_pgmCopyInclude = new ArrayList<ProgramCopyInclude> ();
	programCopyIncludeExtract(division, typeCopy, programCobol.entriesIdentification(), al_pgmCopyInclude);
	programCopyIncludeExtract(division, typeCopy, programCobol.entriesEnvironment(), al_pgmCopyInclude);
	programCopyIncludeExtract(division, typeCopy, programCobol.entriesData(), al_pgmCopyInclude);
	programCopyIncludeExtract(division, typeCopy, programCobol.entriesProcedure(), al_pgmCopyInclude);
	al_pgmCopyInclude.sort(null);
	
	jsonArrayCopy = new JSONArray();
	jsonArraySqlInclude = new JSONArray();
	
 	// Scan all copy
 	for (ProgramCopyInclude oCopyInclude : al_pgmCopyInclude) {
 		jsonObject = new JSONObject();
 		
		jsonObject.put("division", oCopyInclude.division);
 		jsonObject.put("type", oCopyInclude.type);
		jsonObject.put("name", oCopyInclude.name);
		jsonObject.put("numInstr", oCopyInclude.numInstr);
		jsonObject.put("rowStart", oCopyInclude.rowStart);
		jsonObject.put("rowEnd", oCopyInclude.rowEnd);
		
		// Put in specific array
		if (oCopyInclude.type.equals("C")) {
			jsonArrayCopy.put(jsonObject);
		} else {
			jsonArraySqlInclude.put(jsonObject);
		}
 	}	
 	
 	// Output arrays
 	obj.put("arCopy", jsonArrayCopy);
 	obj.put("arSqlInclude", jsonArraySqlInclude);
}

/*
 * Crud Matrix 
 */
private void programSummaryCrudMatrix(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
	JSONArray jsonArray =  null;
	JSONObject jsonObject = new JSONObject();
	List<EntitySqlGeneric> ls_object = null;
	String entityPrec = "";
	
	MySQLDAOFactory sqlFactory = (MySQLDAOFactory) DAOFactory.getDAOFactory(DAOFactory.MYSQL);
	Connection conn = DataBaseConnections.getConnection();
	IDAOSqlGeneric<EntitySqlGeneric> dao = (IDAOSqlGeneric<EntitySqlGeneric>) sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);

	ls_object=dao.sqlCrudMatrix(sys, subSys, idProgram, idProgram, "A", "Z");	
	
	DataBaseConnections.releaseConnection(conn);
	dao.setConn(null);

    jsonArray = new JSONArray();
     
	// Populate Json
	for (EntitySqlGeneric e : ls_object) {
		if (entityPrec.trim().length() == 0) {
			entityPrec = e.getEntity();
			jsonObject.put("entity", e.getEntity());
           	jsonObject.put("isCreate", false);
           	jsonObject.put("isRead", false);
          	jsonObject.put("isUpdate", false);
          	jsonObject.put("isDelete", false);
		}
        if (!entityPrec.equals(e.getEntity()) ) {
        	// Put entity CRUD on array 
        	jsonArray.put(jsonObject);
        	
        	// Set for next entity
        	jsonObject.put("entity", e.getEntity());
       		entityPrec = e.getEntity();
        	jsonObject = new JSONObject();
			jsonObject.put("entity", e.getEntity());
           	jsonObject.put("isCreate", false);
           	jsonObject.put("isRead", false);
          	jsonObject.put("isUpdate", false);
          	jsonObject.put("isDelete", false);
 		}
        if (e.getRelation() == EnumRelation.PGM_ENTITY_INSERT) {
      		jsonObject.put("isCreate", true);
     		jsonObject.put("relationCreate", e.getRelation());
     		jsonObject.put("relationCreateOrdinal", e.getRelation().ordinal());
		}
        if (e.getRelation() == EnumRelation.PGM_ENTITY_READ) {
      		jsonObject.put("isRead", true);
     		jsonObject.put("relationRead", e.getRelation());
     		jsonObject.put("relationReadOrdinal", e.getRelation().ordinal());
		}
        if (e.getRelation() == EnumRelation.PGM_ENTITY_UPDATE) {
      		jsonObject.put("isUpdate", true);
     		jsonObject.put("relationUpdate", e.getRelation());
     		jsonObject.put("relationUpdateOrdinal", e.getRelation().ordinal());
		}
        if (e.getRelation() == EnumRelation.PGM_ENTITY_DELETE) {
      		jsonObject.put("isDelete", true);
     		jsonObject.put("relationDelete", e.getRelation());
     		jsonObject.put("relationDeleteOrdinal", e.getRelation().ordinal());
		}
  		jsonObject.put("relation", e.getRelation());
 		jsonObject.put("relationOrdinal", e.getRelation().ordinal());
	}
    
	// Last pending CRUD
	jsonArray.put(jsonObject);
    
	// Output array with entities
	obj.put("arCRUD", jsonArray);
}

/*
 * File System I/O con indicazione CRUD internalFile/ExternalFile/Job/step
 */
private void programSummaryFileSystemIO(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
	JSONObject jsonObject = null;
	JSONArray jsonArray = null;
	ResultSet rs = null;
	EnumObject en_internalNameType = null;
    List<EntityRelation> ar_objEntityRelation = null;
    List<EntityRelationOrigin> ar_objEntityRelationOrigin = null;

	EntityRelationOrigin entityRelationOrigin = null;
	
	String internalName = "";
	String internalNameType = "";
	String externalName = "";
	String phisicalName = "";
	String jclName = "";
	String jclStep = "";
	String jclProc = "";
	String jclLibrary = "";
	String jclLibraryPath = "";
	String strSql = "";
    String whereCondition = "";
	boolean isCrudCreate = false;
	boolean isCrudRead = false;
	boolean isCrudUpdate = false;
	boolean isCrudDelete = false;
	int internalNameTypeNum = 0;

	jsonArray = new JSONArray();
	
    Connection conn = DataBaseConnections.getConnection();
	IDAORelation eoDAORelation = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
	IDAORelationOrigin eoDAORelationOrigin = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);

	// Recupero internal files relazionati al programma 
	strSql = "SELECT idObjectB, typeObjectB FROM Relation"; 
	strSql = strSql + " WHERE  sys = '" 		+ sys + "'";
	strSql = strSql +  " AND   subSys = '" 		+ subSys + "'";
	strSql = strSql +  " AND   idObjectA = '" 	+ idProgram + "'";
	strSql = strSql +  " AND   typeObjectA = " 	+ EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	strSql = strSql +  " AND  (relation = " 	+ EnumRelation.PGM_INTERNAL_FILE.ordinal()+"";
	strSql = strSql +  "      ) ";
	strSql = strSql +  " GROUP BY idObjectB, typeObjectB";
	strSql = strSql +  " ORDER BY idObjectB";

	// Esecuzione query e produzione  ResultSet
	rs = eoDAORelation.execSqlGeneric(strSql);
     
	// Scan Internal files trovati ordinati per nome
	while (rs.next()) {
		
		internalName = rs.getString(1);
		internalName = StringService._pad(internalName, ' ', 20, StringService.PAD_RIGHT);

		internalNameTypeNum = rs.getInt(2);
		en_internalNameType = EnumObject.values()[internalNameTypeNum];
		internalNameType = en_internalNameType.toString();
		
        // Lettura relazioni di programma con internal file in insert
		whereCondition = getWhereIOFileSystem(sys, subSys, idProgram, internalName, EnumRelation.PGM_INTERNAL_FILE_INSERT.ordinal());

 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
 		isCrudCreate = false;
 		if (ar_objEntityRelation.size() > 0) {
 			isCrudCreate = true;
		}
 		
        // Lettura relazioni di programma con internal file in lettura
		whereCondition = getWhereIOFileSystemRead(sys, subSys, idProgram, internalName);

 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
 		isCrudRead = false;
 		if (ar_objEntityRelation.size() > 0) {
 			isCrudRead = true;
		}

        // Lettura relazioni di programma con internal file in update
		whereCondition = getWhereIOFileSystem(sys, subSys, idProgram, internalName, EnumRelation.PGM_INTERNAL_FILE_UPDATE.ordinal());

 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
 		isCrudUpdate = false;
 		if (ar_objEntityRelation.size() > 0) {
 			isCrudUpdate = true;
		}

        // Lettura relazioni di programma con internal file in Delete
		whereCondition = getWhereIOFileSystem(sys, subSys, idProgram, internalName, EnumRelation.PGM_INTERNAL_FILE_DELETE.ordinal());
 		 
 		ar_objEntityRelation = eoDAORelation.readSetEntityWhere(whereCondition, "");
 		isCrudDelete = false;
 		if (ar_objEntityRelation.size() > 0) {
 			isCrudDelete = true;
 		}
 		
 		// Individuazione external file a partire dall'internal file.
 		// Si legge l'origine della relazione INTERNAL_FILE_EXTERNAL_FILE originata dal programma corrente
 		// Questa relazione e la sua origine viene generata in fase di analisi del programma
 		// Possono esserci più external file per un internal file.
		whereCondition = "";
 		whereCondition = whereCondition + "       sys = '"              + sys + "'";
 		whereCondition = whereCondition + " AND   subSys = '"           + subSys + "'";
 		whereCondition = whereCondition + " AND   idObjectA = '"        + internalName + "'";
 		whereCondition = whereCondition + " AND   typeObjectA =  "      + EnumObject.OBJECT_INTERNAL_FILE.ordinal()+"";
 		whereCondition = whereCondition + " AND   typeObjectB =  "      + EnumObject.OBJECT_EXTERNAL_FILE.ordinal()+"";
 		whereCondition = whereCondition + " AND   relation =  "         + EnumRelation.INTERNAL_FILE_EXTERNAL_FILE.ordinal()+"";
 		whereCondition = whereCondition + " AND   idObjectOrigin = '"   + idProgram + "'";
 		whereCondition = whereCondition + " AND   typeObjectOrigin =  " + EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
 		 
 		ar_objEntityRelationOrigin = eoDAORelationOrigin.readSetEntityWhere(whereCondition, "");
 
 		// Presenti internal file nel programma
 		if (ar_objEntityRelationOrigin.size() > 0) {
 			entityRelationOrigin = ar_objEntityRelationOrigin.get(0);
			externalName = entityRelationOrigin.getIdObjectRelB();
 		
	 		// Individuazione jcl in cui l'external file è utilizzato dal programma.
	 		// L'external file, per il programma che lo utilizza, può trovarsi in + di un jcl .
			// Si utilizza una sola relazione generata nell'analisi del jcl.
			// E' sufficiente la relazione EXTERNAL_FILE_JCL_SOURCE con tutte le info di incrocio
			//       RELOTYPC = OBJECT-PHISICAL-FILE 
			//       RELOIDOC = phisical file name 
			//       info1Cross = step
			//       info2Cross = Proc
			//       info3Cross = exec pgm
			// Composizione condizione Where 
			whereCondition =                      "      sys  = '" 			+ sys + "'";
			whereCondition = whereCondition +	  " AND  subSys  = '" 		+ subSys + "'";
			whereCondition = whereCondition +     " AND  relation  =  " 	+ EnumRelation.EXTERNAL_FILE_JCL_JOB.ordinal();
			whereCondition = whereCondition +     " AND  typeObjectA  =  " 	+ EnumObject.OBJECT_EXTERNAL_FILE.ordinal();
			whereCondition = whereCondition +     " AND  idObjectA  = '" 	+ externalName + "'";
			whereCondition = whereCondition +     " AND  info3Cross  = '" 	+ idProgram + "'";
	 		 
	 		ar_objEntityRelationOrigin = eoDAORelationOrigin.readSetEntityWhere(whereCondition, "");
	 		
	 		// External file utilizzato dal programma in qualche jcl
	 		if (ar_objEntityRelationOrigin.size() > 0) {
	 			
	            // Scan origine relazioni con jcl in cui il file esterno è dichiarato per il programma
	 		    for (int i = 0; i < ar_objEntityRelationOrigin.size(); i++) {
		 			
	 		    	entityRelationOrigin = ar_objEntityRelationOrigin.get(i);
			 	 	phisicalName = entityRelationOrigin.getIdObjectCross();
		 	 		jclName = entityRelationOrigin.getIdObjectOrigin();
		 	 		jclStep = entityRelationOrigin.getInfo1Cross();
		 	 		jclProc = entityRelationOrigin.getInfo2Cross();
		 	 		jclLibrary = entityRelationOrigin.getLibrarySourceObject();
		 	 		jclLibraryPath = entityRelationOrigin.getLibrarySourcePath();
		 			
		 			// Put on object
		 			jsonObject = new JSONObject();
		 			jsonObject.put("internalName", internalName);
		 			jsonObject.put("internalNameType", internalNameType);
		 			jsonObject.put("isCrudCreate", isCrudCreate);
		 			jsonObject.put("isCrudRead", isCrudRead);
		 			jsonObject.put("isCrudUpdate", isCrudUpdate);
		 			jsonObject.put("isCrudDelete", isCrudDelete);
		 			jsonObject.put("externalName", externalName);
		 			jsonObject.put("phisicalName", phisicalName);
		 			jsonObject.put("jclName", jclName);
		 			jsonObject.put("jclStep", jclStep);
		 			jsonObject.put("jclProc", jclProc);
		 			jsonObject.put("jclLibrary", jclLibrary);
		 			jsonObject.put("jclLibraryPath", jclLibraryPath);
				
		 			// Push on array
		 			jsonArray.put(jsonObject);
		 			
	 		    }  // end-for phisical file  

	 		} // end-if
	 		
		} // end-if
			
	} // end-for internal file

	DataBaseConnections.releaseConnection(conn);
	eoDAORelation.setConn(null);
	eoDAORelationOrigin.setConn(null);
	
	// Otput 
	obj.put("arFileSystemIO", jsonArray);
}

/*
 * Costruzione where per informazioni CRUD
 * 
 */
private String getWhereIOFileSystem(String sys, String subSys, String idProgram, String internalName, int ordinal) {
	String whereCondition = "";

	whereCondition = whereCondition +        "sys = '" 			+ sys + "'";
	whereCondition = whereCondition + " AND   subSys = '" 		+ subSys + "'";
	whereCondition = whereCondition + " AND   idObjectA = '" 	+ idProgram + "'";
	whereCondition = whereCondition + " AND   idObjectB = '" 	+ internalName + "'";
	whereCondition = whereCondition + " AND   typeObjectA = " 	+ EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	whereCondition = whereCondition + " AND   relation = " 		+ ordinal+"";		
	return whereCondition;
}

/*
 * Costruzione where per informazioni CRUD di lettura
 * 
 */
private String getWhereIOFileSystemRead(String sys, String subSys, String idProgram, String internalName) {
	String whereCondition = "";

	whereCondition = whereCondition +        "sys = '" 			+ sys + "'";
	whereCondition = whereCondition + " AND   subSys = '" 		+ subSys + "'";
	whereCondition = whereCondition + " AND   idObjectA = '" 	+ idProgram + "'";
	whereCondition = whereCondition + " AND   idObjectB = '" 	+ internalName + "'";
	whereCondition = whereCondition + " AND   typeObjectA = " 	+ EnumObject.OBJECT_PGM_COBOL.ordinal()+"";
	whereCondition = whereCondition + " AND  (relation = " 		+ EnumRelation.PGM_INTERNAL_FILE_READ.ordinal()+"";
	whereCondition = whereCondition + "    OR relation = " 		+ EnumRelation.PGM_INTERNAL_FILE_READNEXT.ordinal()+"";
	whereCondition = whereCondition + "    OR relation = " 		+ EnumRelation.PGM_INTERNAL_FILE_READPREV.ordinal()+"";
	whereCondition = whereCondition + "    OR relation = " 		+ EnumRelation.PGM_INTERNAL_FILE_SORT.ordinal()+"";
	whereCondition = whereCondition + "    OR relation = " 		+ EnumRelation.PGM_INTERNAL_FILE_MERGE.ordinal()+"";
	whereCondition = whereCondition + "      )";
	return whereCondition;
}

/*
 * Produzione programma completo codificato
 * Se isCopySource="Y" verranno inclusi i sorgenti dei moduli copy invece che il sorgente codificato
 */
private void programSummarySourceCoded(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, String isCopySource, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
	getJsonProgramCoded(ucfg, sys, subSys, programCobol, user, idProgram, isCopySource, obj);	
}

/*
 * Recupero source programma
 */
private List<String> getSourceProgram(UserConfiguration ucfg, String sys, String subSys, String idProgram) throws SQLException, ExceptionAmrita {
	EntityObject eo = null;
	List<String> al_sourceRows = null;
	String pathSource = "";
	String suffixFileSource = "";
	boolean isFound = false;
	
	Connection conn = DataBaseConnections.getConnection();
	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, true, true, ucfg);
	eo = new EntityObject();
	eo.setSystem(sys);
	eo.setSubSystem(subSys);
	eo.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
	eo.setIdObject(idProgram);
	isFound=eoDAO.read(eo);	
	
	// Program not found
	if (!isFound) {
		throw new ExceptionAmrita();
	}

	// Composizione path programma
	suffixFileSource=eo.getSuffixFileSource();
	pathSource=eo.getLibraryDir() + File.separator + idProgram;
//	if (!eo.getSuffixFileSource().isBlank()) {
	if (eo.getSuffixFileSource().trim().length() != 0) {
		pathSource += "." + suffixFileSource;
	}
	al_sourceRows = SourceManager.getFile(pathSource);
	return al_sourceRows;
}

/*
 * Produzione Screen Layout mappe video utilizzate nel programma
 *  - Layout video
 *  - Elenco campi bms codificati
 *  - Source BMS
 */
private void programSummaryScreenLayout(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
	JSONObject jsonObjectMapDescriptor = null;
	JSONObject jsonObjectMapField = null;
	JSONArray jsonArrayMapDescriptor = null;
	JSONArray jsonArrayMapField = null;
	JSONArray jsonArrayMapBmsRow = null;
	List<EntityRelation> li_relation = null;
	List<EntityMapDescriptor> li_mapDescriptor = null;
	List<EntityMapItem> li_mapItem = null;
	List<String> li_bmsRow = null;
	String mapName = "";
		
	jsonArrayMapDescriptor = new JSONArray();
	jsonObjectMapField = new JSONObject();	
	jsonArrayMapBmsRow = new JSONArray();
	
	Connection conn = DataBaseConnections.getConnection();
	IDAORelation relationDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false, false, ucfg);	
	IDAOMapDescriptor mapDescriptorDAO = (DAOImplMapDescriptor) AmritaStartup.sqlFactory.getDAOMapDescriptor(conn, false, false, ucfg);	
	IDAOMapItem mapItemDAO = (DAOImplMapItem) AmritaStartup.sqlFactory.getDAOMapItem(conn, false, false, ucfg);

	// Nomi mappe utilizzate dal programma via relation
	li_relation = relationDAO.findAllBySubSysIdA(sys, subSys, idProgram, EnumRelation.PGM_CICS_MAP);
		
	// Scan mappe
	for (EntityRelation er : li_relation) {
		mapName = er.getIdObjectB();
		
		jsonObjectMapDescriptor = new JSONObject();
		jsonArrayMapField = new JSONArray();
		
		// Get and populate map descriptor
		li_mapDescriptor=mapDescriptorDAO.findAll(sys, subSys, mapName, EnumObject.OBJECT_CICS_MAP);
		RestDbAccessDAOEntity.populateJsonMapDescriptor(li_mapDescriptor.get(0), jsonObjectMapDescriptor);
         
        // Get and populate Campi mappa
		li_mapItem = mapItemDAO.findAll(sys, subSys, mapName, EnumObject.OBJECT_CICS_MAP);
		for (EntityMapItem entityMapItem : li_mapItem) {
			jsonObjectMapField = new JSONObject();
			RestDbAccessDAOEntity.populateJsonMapItem(entityMapItem, jsonObjectMapField);
			jsonArrayMapField.put(jsonObjectMapField);			
		}
        jsonObjectMapDescriptor.put("arMapField", jsonObjectMapDescriptor);
        
        // Lettura Recupero path sorgente BMS
        li_bmsRow = getSourceRowsFromFileSystem(sys, subSys, EnumObject.OBJECT_CICS_BMS_SOURCE, mapName, ucfg);
        if (li_bmsRow != null && li_bmsRow.size() > 0) {
        	jsonArrayMapBmsRow = new JSONArray(li_bmsRow);
		} else {
			jsonArrayMapBmsRow = new JSONArray();
		}
        jsonObjectMapDescriptor.put("arMapBmsRow", jsonArrayMapBmsRow);
        
        // Push descrittore completo mappa
        jsonArrayMapDescriptor.put(jsonObjectMapDescriptor);
	}
	
	DataBaseConnections.releaseConnection(conn);
	mapDescriptorDAO.setConn(null);

	obj.put("mapDescriptor", jsonObjectMapDescriptor);
}

/*
 * Produzione sezione di programma
 */
private void putSourceCodedByCobolDivision(String division, ProgramCobol programCobol, ProgramCobolEntry<? extends Instruction>[] entriesDivision, List<String> al_rowSource, JSONArray jsonArrayInstr, UserConfiguration ucfg) throws SQLException, ExceptionAmrita {
	JSONObject jsonObjectInstr = null;
	@SuppressWarnings("unused")
	JSONObject jsonObjectConditionNameValue = null;
	@SuppressWarnings("unused")
	JSONObject jsonObjectOccursKeyEntry = null;
	JSONArray jsonArrayRowsSourceInstr = null;
	JSONArray jsonArrayOccursIndexName = null;
	JSONArray jsonArrayConditionNameValue = null;
	JSONArray jsonArrayOccursKeyEntry = null;
	JSONArray jsonArrayRowSourceCopy  = null;
	
	ProgramCobolEntry<? extends Instruction> entryGeneric  = null;
	Instruction instr = null;
	InstructionCobolDataItem istrData = null;
	InstructionSql instrSql = null;
	InstructionCobol instrCopy = null;
	List<String> al_rowSourceCopy = null;
	EnumObject typeCopy = null;
	String sys = "";
	String subSys = "";
	String copyIncludeName = "";
   	
	sys = programCobol.getSysOwner();
	subSys = programCobol.getSubSysOwner();
		
	// Scan istruzioni divisione
	for (int i = 0; i < entriesDivision.length; i++) {
		
		jsonObjectInstr = new JSONObject();                         // Contiene tutte le informazioni dell'istruzione
		
		entryGeneric = entriesDivision[i];
		instr = entryGeneric.getInstruction();
		
		// Bypass istruzioni dentro copy, verranno esplose a richiesta
//		if (!entryGeneric.getUnderCopyName().isBlank()) {
		if (entryGeneric.getUnderCopyName().trim().length() != 0) {
			continue;
		}
		
		////////////////////////////////////////////
		// Righe sorgente commenti & istruzione
		////////////////////////////////////////////
		
		jsonArrayRowsSourceInstr = new JSONArray();                 // Contiene le righe sorgente commento + istruzione		
  		putSourceCodedByCobolDivisionRowsComm(division, programCobol, entriesDivision, i, instr, al_rowSource, jsonArrayRowsSourceInstr);
		putSourceCodedByCobolDivisionRowsInstr(instr, al_rowSource, jsonArrayRowsSourceInstr);
		jsonObjectInstr.put("arRowSource", jsonArrayRowsSourceInstr);
		        
		/////////////////////////////////////////////
		// Info instruction Common to all division
		/////////////////////////////////////////////
		
		jsonObjectInstr.put("division", division);
		jsonObjectInstr.put("numInstr", instr.getNumInstr());
		jsonObjectInstr.put("rowStart", instr.getRowStartSource());
		jsonObjectInstr.put("rowEnd", instr.getRowEndSource());
		jsonObjectInstr.put("posStart", instr.getPosStartInstr());
		jsonObjectInstr.put("posEnd", instr.getPosEndInstr());
		jsonObjectInstr.put("typeInstr", instr.getTypeInstr());
		jsonObjectInstr.put("typeInstrCategory", instr.getTypeInstrCategory());
		jsonObjectInstr.put("sourceInstr", instr.getSourceInstr());
		jsonObjectInstr.put("isUnderCopy", entryGeneric.isUnderCopy());                             	// True se istruzione dentro copy
		jsonObjectInstr.put("underCopyName", "");                      	                                // Eventually replaced later
		jsonObjectInstr.put("isCopyStatement", false);                       	                        // Eventually replaced later
		jsonObjectInstr.put("isCopyReplacedBy", entryGeneric.isReplacedBy());                       	// COPY replacing by
		jsonObjectInstr.put("lvlCopyNesting", entryGeneric.getLevelNestingCopy());                  	// Livello annidamento copy
		
		/////////////////////////////////////////////
		// Info Instruction if procedure division
		/////////////////////////////////////////////
		
		if (division.equals("P")) {
			jsonObjectInstr.put("entryType", entryGeneric.getEntryType());                              // Data item, statement copy, statement precompilatore, struttura
			jsonObjectInstr.put("entryTypeInstr", entryGeneric.getTypeInstr());						    // Codice istruzione
			jsonObjectInstr.put("isEntryPrecompiler", entryGeneric.isEntryPrecompiler());               // True se entry precompiler inserito manualmente
			jsonObjectInstr.put("isMainline", entryGeneric.isMainline());                               // True se mainline
			jsonObjectInstr.put("isDeadCode", entryGeneric.isDeadCode());                               // True se codice morto
			jsonObjectInstr.put("lvlNesting", entryGeneric.getLevelDeepEntry());                        // Livello annidamento istruzione
			jsonObjectInstr.put("isUnderCondition", entryGeneric.isUnderCondition());   				// True indica entry sotto ramo true/else, evaluate when o search when
			jsonObjectInstr.put("numInstrOwnerConditional", entryGeneric.getNumEntryOwnerConditional());// Numero entry owner. If se in ramo If/Else, Numero Evaluate/Search se in ramo When, Inner Perform e paragrafo
			jsonObjectInstr.put("numInstrRelated", entryGeneric.getNumInstrRelated());  				//  Istruzione di fine/inizio blocco, come END-IF per IF, IF per END-IF END-EVALUATE per EVALUATE 
			jsonObjectInstr.put("numInstrRelatedElse", entryGeneric.getNumInstrRelatedElse());  		// Istruzione di ELSE per IF
			jsonObjectInstr.put("isUnderProc", entryGeneric.isUnderProcInternal());		                // True se dentro paragrafo/section
			jsonObjectInstr.put("underProcNumInstr", entryGeneric.getUnderProcInternalPointer());       // Numero istruzione section/paragrafo
		
		/////////////////////////////////////////////
		// Info Instruction if data division
		/////////////////////////////////////////////
			
		} else if (division.equals("D") && entryGeneric.getInstruction() instanceof InstructionCobolDataItem ) {
			istrData = (InstructionCobolDataItem) entryGeneric.getInstruction(); 
			
			// Identificazione campo
			jsonObjectInstr.put("levelNumber", istrData.getLevelNumber());  							// Numero livello cobol
			jsonObjectInstr.put("levelType", istrData.getLevelType());   								// Classe livello: 77, 88, 66, altro ...
			jsonObjectInstr.put("fieldName", istrData.getDataName());  									// Nome Campo
			jsonObjectInstr.put("itemType", istrData.getItemType());  									// Tipologia specifica item  (T0008)
			jsonObjectInstr.put("isGroupField", istrData.isGroupField());  								// True indica campo di gruppo senza Picture, usage, ...
			jsonObjectInstr.put("sizeBytes", istrData.getSizeBytes());  								// Dimensioni campo in caratteri se alfanumerico
			jsonObjectInstr.put("numInt", istrData.getNumInt());  										// Numero interi definiti o massimi se campo numerico
			jsonObjectInstr.put("numDec", istrData.getNumDec());  										// Numero decimali definiti se campo numerico
			
			// Condition name (88)
			jsonObjectInstr.put("isConditionName", istrData.isConditionClause()); 						// True indica Condition name
			jsonArrayConditionNameValue = new JSONArray();
			for (InnerConditionValueEntry condValue : istrData.getConditionValues()) {
				jsonObjectConditionNameValue = new JSONObject();
				// TODO
			}
			jsonObjectInstr.put("arConditionNameValue", jsonArrayConditionNameValue); 
			// Occurs
			jsonObjectInstr.put("isUnderOccurs", istrData.isUnderOccurs());     						// True indica campo definito sotto una occurs
			jsonObjectInstr.put("dataNameOccursOwner", istrData.getDataNameOccursOwner()); 				// Nome campo occurs di gruppo sotto il quale la definizione corrente è collocata
			jsonObjectInstr.put("dataNameOccursOwnerNumInstr", istrData.getDataNameOccursOwnerOrdinal()); // Numero ordinale nella struttura generale di definizione
			jsonObjectInstr.put("isOccursClause", istrData.isOccursClause());   						// True indica occurs Fixed-Length o Variable-Length Tables
			jsonObjectInstr.put("occursNumber", istrData.getOccursNumber()); 							// Numero occorrenze fisse gruppo
			jsonObjectInstr.put("occursNumberFrom", istrData.getOccursNumberFrom()); 					// Numero occorrenze variabili gruppo start
			jsonObjectInstr.put("occursNumberTo", istrData.getOccursNumberTo()); 						// Numero occorrenze variabili gruppo to times
			for (InnerOccursKeyEntry occursKey : istrData.getOccursKeys()) {
				jsonObjectOccursKeyEntry = new JSONObject();
				// TODO
			}
			jsonObjectInstr.put("arOccursKeyEntry", jsonArrayOccursKeyEntry);
			jsonObjectInstr.put("occursDependingOnDataName", istrData.getOccursDependingOnDataName()); 	// Campo indicante le occorrenze effettive della tabella 
			jsonArrayOccursIndexName = new JSONArray();
			for (String indexName : istrData.getOccursIndexNames()) {
				jsonArrayOccursIndexName.put(indexName);
			}
			jsonObjectInstr.put("arOccursIndexName", jsonArrayOccursIndexName); 	               		// Occurs Index Name
	
		    // Redefines & renames
			jsonObjectInstr.put("isRedefines", istrData.isRedefinesClause()); 	 						// True indica redefines
			jsonObjectInstr.put("isRenames", istrData.isRenamesClause());	     						// True indica renames valido solo per il livello 66
			jsonObjectInstr.put("redefinesDataName", istrData.getRedefinesDataName());   				// Campo ridefinito 
			jsonObjectInstr.put("renamesDataName", istrData.getRenamesDataName());	 					// Campo rinominato di inizio (o il solo) 
			jsonObjectInstr.put("renamesDataNameThru", istrData.getRenamesDataNameThru());  			// Campo rinominato di fine  
		    // Value
			jsonObjectInstr.put("isValue", istrData.isValueClause());			 						// True indica value
			jsonObjectInstr.put("valueType", istrData.getValueType());	         						// Tipologia value
			jsonObjectInstr.put("valueString", istrData.getValueString());	     						// Se valore stringa
			jsonObjectInstr.put("valueNumeric", istrData.getValueNumeric());	    					// Se valore numerico
			jsonObjectInstr.put("valueFigurative", istrData.getValueFigurative());						// Se costante figurativa
		    // Picture
			jsonObjectInstr.put("isPicture", istrData.isPictureClause());		 						// True indica Picture
			jsonObjectInstr.put("picture", istrData.getPicture());			 							// Picture così come codificata nel sorgente            
		    // Usage
			jsonObjectInstr.put("isUsage", istrData.isUsageClause());			 						// True indica Usage
			jsonObjectInstr.put("usage", istrData.getUsage());			  	 							// Tipologia campo 
		    // Like
			jsonObjectInstr.put("isLike", istrData.isLikeClause());			 							// True indica Like
			jsonObjectInstr.put("likeDataName", istrData.getLikeDataName());							// Like campo
			jsonObjectInstr.put("likeDataNameInstr", istrData.getLikeInteger());						// Like campo(integer)
		    // Sign
			jsonObjectInstr.put("isSign", istrData.isSignClause());			 							// True indica Sign
			jsonObjectInstr.put("signType", istrData.getSignType());	 		 						// Leading/Trailing  COBOL_SIGN_LEADING;
		    jsonObjectInstr.put("isSignSeparateCharacter", istrData.isSignSeparateCharacter());// Separate Character
		    // Justified
		    jsonObjectInstr.put("isJustified", istrData.isJustifiedClause());		 					// True indica Justified
		    jsonObjectInstr.put("justifiedType", istrData.getJustifiedType());		 					// COBOL_JUSTIFIED_LEFT;							 
		    // Syncronized
		    jsonObjectInstr.put("isSincronized", istrData.isSincronizedClause());	 					// True indica Sincronized
		    jsonObjectInstr.put("syncronizedType", istrData.getSincronizedType());	 					// COBOL_SYNCRONIZED_LEFT|COBOL_SYNCRONIZED_RIGHT 		  
		    // Opzioni semplici senza valori
		    jsonObjectInstr.put("isBlankWhenZero", istrData.isBlankWhenZeroClause());					 
		    jsonObjectInstr.put("isGlobal", istrData.isGlobalClause());								 
		    jsonObjectInstr.put("isExternal", istrData.isExternalClause());	 				
			// Ordinal
			jsonObjectInstr.put("itemTypeOrdinal", istrData.getItemType().ordinal());	         						
			jsonObjectInstr.put("LevelTypeOrdinal", istrData.getLevelType().ordinal());	         						
			jsonObjectInstr.put("valueFigurativeOrdinal", istrData.getValueFigurative().ordinal());	         						
			jsonObjectInstr.put("usageOrdinal", istrData.getUsage().ordinal());	         									 			
		}
		
		/////////////////////////////////////////////
		// Gestione Source Copy/Sql Include & Bypass
		/////////////////////////////////////////////
		
		copyIncludeName = "";
		
		// Copy
		if (entryGeneric.getInstruction().getTypeInstr() == EnumCobolReservedWords.DIR_COMPILER_COPY) {
			instrCopy = (InstructionCobol) entryGeneric.getInstruction();
			copyIncludeName = instrCopy.copyGetName();
			jsonObjectInstr.put("underCopyName", copyIncludeName);                       
		//	Sql Precompiler statement
		} else if (entryGeneric.getInstruction().getTypeInstr() == EnumCobolReservedWords.PRECOMPILER_SQL) {
			instrSql = (InstructionSql) entryGeneric.getInstruction();
			// Sql Include
			if (instrSql.getTypeInstrPrecompiler() == EnumPrecompilerReservedWords.SQL_INCLUDE) {
   				copyIncludeName = instrSql.sqlIncludeGetName();
   				jsonObjectInstr.put("underCopyName", copyIncludeName);                       
			}		
		}
		// Copy o Sql Include: get source rows
//		if (!copyIncludeName.isBlank()) {
		if (copyIncludeName.trim().length() != 0) {
			jsonObjectInstr.put("isCopyStatement", true); 
			if (division.contentEquals("D")) {
				typeCopy = EnumObject.OBJECT_COPY_COBOL_DATA;
			} else if (division.contentEquals("P")) {
				typeCopy = EnumObject.OBJECT_COPY_COBOL_PROC;
			} else if (division.contentEquals("E")) {
				typeCopy = EnumObject.OBJECT_COPY_COBOL_ENV;
			} else if (division.contentEquals("I")) {
				typeCopy = EnumObject.OBJECT_COPY_COBOL_ID;
			}
			al_rowSourceCopy = getSourceRowsFromFileSystem(sys, subSys, typeCopy, copyIncludeName, ucfg);
			jsonArrayRowSourceCopy = new JSONArray(al_rowSourceCopy);
			jsonObjectInstr.put("arSourceCopy", jsonArrayRowSourceCopy); 
		}		
		
		// Push descrittore istruzione in array di output
		// Ogni oggetto contiene l'istruzione codificata e le righe sorgenti originali
		// In caso di statement copy, se richiesto, è valorizzato l'array delle righe sorgente del copy
		jsonArrayInstr.put(jsonObjectInstr);
		
	} // end-for program coded instr	
}

/*
 * Produzione righe istruzione
 */
private void putSourceCodedByCobolDivisionRowsInstr(Instruction instr, List<String> al_rowSource, JSONArray jsonArrayRowsSourceInstr) {
	int iStart = 0;
	int iEnd = 0;
	
	iStart = instr.getRowStartSource();
	iEnd = instr.getRowEndSource();

	for (int j = iStart; j <= iEnd; j++) {
		jsonArrayRowsSourceInstr.put(al_rowSource.get(j));
	}
}


/*
 * Produzione commenti precedenti l'istruzione
 */
private void putSourceCodedByCobolDivisionRowsComm(String division, ProgramCobol programCobol, ProgramCobolEntry<? extends Instruction>[] entriesDivision, int iDivEntry, Instruction instr, List<String> al_rowSource, JSONArray jsonArrayRowsSourceInstr) {
	ProgramCobolEntry<? extends Instruction>[] entriesDivWrk = null;
	ProgramCobolEntry<? extends Instruction> entryDivWrk = null;
	Instruction instrPrec = null;
	Instruction instrDivFirst = null;
	String copyName = "";
	int iStart = 0;
	int iEnd = 0;
	
	instrDivFirst = entriesDivision[0].getInstruction();
	
	// Prima istruzione division: recuper ultima istruzione division precedente
	if (instr.getNumInstr() == instrDivFirst.getNumInstr()) {
		if (division.equals("E")) {
			entriesDivWrk = programCobol.entriesIdentification();
			instrPrec = entriesDivWrk[entriesDivWrk.length - 1].getInstruction();
		} else if (division.equals("D")) {
			entriesDivWrk = programCobol.entriesEnvironment();
			instrPrec = entriesDivWrk[entriesDivWrk.length - 1].getInstruction();
		} else if (division.equals("P")) {
			entriesDivWrk = programCobol.entriesProcedure();
			instrPrec = entriesDivWrk[entriesDivWrk.length - 1].getInstruction();
		// Commenti prima di Identification Division non recuperabili.
		// Necessario aggiungere un campo in Instruction 
		} else {
			return;
		}
		
	// Recupero istruzione precedente in division fornita
    // Se sotto copy devo cercare lo statement precedente al copy
	} else {		
		instrPrec = entriesDivision[iDivEntry - 1].getInstruction();
		entryDivWrk = entriesDivision[iDivEntry - 1];
		copyName = entryDivWrk.getUnderCopyName();
		if (!copyName.equals("")) {
			for (int i = iDivEntry - 1; i >= 0; i--) {
				entryDivWrk = entriesDivision[i];
				if (!entryDivWrk.getUnderCopyName().equals(copyName)) {
					instrPrec = entryDivWrk.getInstruction();
					break;
				}
			}
		}
	}
	
	iStart = instrPrec.getRowEndSource() + 1;	
	iEnd = instr.getRowStartSource();
	for (int j = iStart; j < iEnd; j++) {
		jsonArrayRowsSourceInstr.put(al_rowSource.get(j));
	}

}

/*
 * Produzione Lista istruzioni dinamiche, campi, sottocampi, parametri risolti e valori individuati 
 * Si sfruttano le informazioni in LogicInfoDynamic memorizzate insieme al programma serializzato
 */
private void programSummaryDynamicCode(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj) {

	JSONObject jsonObjectDynamicInstr = null;
	JSONObject jsonObjectDynamicField = null;
	JSONObject jsonObjectDynamicFieldSub = null;
	JSONObject jsonObjectDynamicFieldSubSetting = null;
	JSONObject jsonObjectDynamicFieldSubValue = null;
	JSONArray jsonArrayDynamicInstr = null;
	JSONArray jsonArrayDynamicField = null;
	JSONArray jsonArrayDynamicFieldSub = null;
	JSONArray jsonArrayDynamicFieldValue = null;
	JSONArray jsonArrayDynamicFieldSubValue = null;
	JSONArray jsonArrayDynamicFieldSubChain = null;
	JSONArray jsonArrayDynamicFieldSubChainSetting = null;
	
	Instruction instr = null;
	LogicInfoDynamic logicInfoDynamic = null;
	ArrayList<LogicDynamicField> al_logicDynamicField = null;	
	ArrayList<LogicDynamicInstruction> al_dynamicInstr = null; 
	
	int numChain = 0;
	
	logicInfoDynamic = programCobol.getLogicInfoDynamic();
	al_dynamicInstr = logicInfoDynamic.getAllDynamicInstr();

	// Scan Dynamic instructions
	jsonArrayDynamicInstr = new JSONArray();
	for (LogicDynamicInstruction dynInstr : al_dynamicInstr) {
		jsonObjectDynamicInstr = new JSONObject();
		al_logicDynamicField = dynInstr.getDynamicFields();
		
		instr = dynInstr.dynamicInstr;
		putJsonDynamicInstr(instr, jsonObjectDynamicInstr);
		
		// Scan dynamic fields
		jsonArrayDynamicField = new JSONArray();
		for (LogicDynamicField dynamicField : al_logicDynamicField) {
			jsonObjectDynamicField = new JSONObject();
			programSummaryPutJsonDynamicField(dynamicField.entityDynamicField, jsonObjectDynamicField);
			
			// Scan field value
			jsonArrayDynamicFieldValue = new JSONArray();
			for (String fieldValue : dynamicField.al_valuesField) {
				jsonArrayDynamicFieldValue.put(fieldValue);
			}
			jsonObjectDynamicField.put("arDynamicFieldValue", jsonArrayDynamicFieldValue);
			
			// Scan fields sub
			jsonArrayDynamicFieldSub = new JSONArray();
			for (LogicDynamicFieldSub fieldSub : dynamicField.al_FieldSub) {
				
				jsonObjectDynamicFieldSub = new JSONObject();
				programSummaryPutJsonDynamicFieldSub(fieldSub, jsonObjectDynamicFieldSub);
				
				// Scan field sub values
				jsonArrayDynamicFieldSubValue = new JSONArray();
				for (LogicDynamicValue fieldSubValue : fieldSub.al_value) {
					
					jsonObjectDynamicFieldSubValue = new JSONObject();
					putJsonDynamicFieldSubValue(fieldSubValue, jsonObjectDynamicFieldSubValue);
					jsonArrayDynamicFieldSubValue.put(jsonObjectDynamicFieldSubValue);
				
				} // end-for fieldSubValue
				jsonObjectDynamicFieldSub.put("arFieldSubValue", jsonArrayDynamicFieldSubValue);
				
				// Scan catene di trasformazione sottocampo
				jsonArrayDynamicFieldSubChain = new JSONArray();
				for (ArrayList<LogicDynamicFieldSubSetting> al_LogicDynamicFieldSubSetting : fieldSub.al_al_chainSetSubField) {
					
					// Scan trasformazioni sottocampo
					jsonArrayDynamicFieldSubChainSetting = new JSONArray();
					for (LogicDynamicFieldSubSetting fieldsetting : al_LogicDynamicFieldSubSetting) {						
						jsonObjectDynamicFieldSubSetting = new JSONObject();
						putJsontDynamicFieldSubSetting(fieldsetting.entityDynamicFieldSetting, jsonObjectDynamicFieldSubSetting, numChain);
						jsonArrayDynamicFieldSubChainSetting.put(jsonObjectDynamicFieldSubSetting);						
					} // end-for fieldsetting
					jsonArrayDynamicFieldSubChain.put(jsonArrayDynamicFieldSubChainSetting);					
					
				} // end-for fielSubSetting
				jsonObjectDynamicFieldSub.put("arFieldSubChain", jsonArrayDynamicFieldSubChain);	
				
				jsonArrayDynamicFieldSub.put(jsonObjectDynamicFieldSub);
				
			} // end-for fieldSub
			jsonObjectDynamicField.put("arDynamicFieldSub", jsonArrayDynamicFieldSub);			
		
		} // end-dynamicField
		jsonArrayDynamicInstr.put(jsonArrayDynamicField);	
		
	} // end-dynInstr
	
	obj.put("arDynamicField", jsonArrayDynamicInstr);
}

/*
 *  Popolamento jsonObjectDynamicInstr da Instruction
 */ 
private void putJsonDynamicInstr(Instruction instr, JSONObject jsonObjectDynamicInstr) {
	jsonObjectDynamicInstr.put("numInstr", instr.getNumInstr());
	jsonObjectDynamicInstr.put("isDynamic", instr.isDynamic());
	jsonObjectDynamicInstr.put("isDynamicLight", instr.isDynamicLight());
	jsonObjectDynamicInstr.put("isDynamicSolved", instr.isDynamicSolved());
	jsonObjectDynamicInstr.put("isDynamicSolvedFull", instr.isDynamicSolvedFull());
	jsonObjectDynamicInstr.put("isDynamicSpreaded", instr.isDynamicSpreaded());
	jsonObjectDynamicInstr.put("isDynamicWaitingForData", instr.isDynamicWaitingForData());
}

/*
 *  Popolamento jsonObjectDynamicField da EntityDynamicField
 */ 
private void programSummaryPutJsonDynamicField(EntityDynamicField entityDynamicField, JSONObject jsonObjectDynamicField) {
	jsonObjectDynamicField.put("numInstr", entityDynamicField.getNumInstr());
	jsonObjectDynamicField.put("IdField", entityDynamicField.getIdField());
	jsonObjectDynamicField.put("numField", entityDynamicField.getNumField());
	jsonObjectDynamicField.put("isSolved", entityDynamicField.getSolved());
	jsonObjectDynamicField.put("isSolvedFull", entityDynamicField.getSolvedFull());
	jsonObjectDynamicField.put("isSpreaded", entityDynamicField.getSpreaded());
	jsonObjectDynamicField.put("isWaitingForData", entityDynamicField.getWaitingForData());
}


/*
 *  Popolamento jsonObjectDynamicFieldSub da LogicDynamicFieldSub
 */ 
private void programSummaryPutJsonDynamicFieldSub(LogicDynamicFieldSub dynamicFieldSub, JSONObject jsonObjectDynamicFieldSub) {
	jsonObjectDynamicFieldSub.put("numInstr", dynamicFieldSub.entityDynamicFieldSub.getNumInstr());
	jsonObjectDynamicFieldSub.put("idField", dynamicFieldSub.entityDynamicFieldSub.getIdField());
	jsonObjectDynamicFieldSub.put("idSubField", dynamicFieldSub.entityDynamicFieldSub.getIdSubField());
	jsonObjectDynamicFieldSub.put("typeSubField", dynamicFieldSub.entityDynamicFieldSub.getTypeSubField());
	jsonObjectDynamicFieldSub.put("numField", dynamicFieldSub.entityDynamicFieldSub.getNumField());
	jsonObjectDynamicFieldSub.put("numSubField", dynamicFieldSub.entityDynamicFieldSub.getNumSubField());
	jsonObjectDynamicFieldSub.put("posSubField", dynamicFieldSub.entityDynamicFieldSub.getPosSubField());
	jsonObjectDynamicFieldSub.put("sizeSubField", dynamicFieldSub.entityDynamicFieldSub.getSizeSubField());
	jsonObjectDynamicFieldSub.put("isLight", dynamicFieldSub.entityDynamicFieldSub.isLight());
	jsonObjectDynamicFieldSub.put("isSolved", dynamicFieldSub.entityDynamicFieldSub.isSolved());
	jsonObjectDynamicFieldSub.put("isSpread", dynamicFieldSub.entityDynamicFieldSub.isSpreaded());
	jsonObjectDynamicFieldSub.put("isWaitingForData", dynamicFieldSub.entityDynamicFieldSub.isWaitingForData());
}


/*
 *  Popolamento jsonObjectDynamicFieldSubValue da LogicDynamicValue
 */ 
private void putJsonDynamicFieldSubValue(LogicDynamicValue fieldSubValue, JSONObject jsonObjectDynamicFieldSubValue) {
	jsonObjectDynamicFieldSubValue.put("numChainSet", fieldSubValue.numChainSet);
	jsonObjectDynamicFieldSubValue.put("idField", fieldSubValue.entityDynamicValue.getIdField());
	jsonObjectDynamicFieldSubValue.put("idFieldSub", fieldSubValue.entityDynamicValue.getIdSubField());
	jsonObjectDynamicFieldSubValue.put("numInstr", fieldSubValue.entityDynamicValue.getNumInstr());
	jsonObjectDynamicFieldSubValue.put("numInstrSet", fieldSubValue.entityDynamicValue.getNumInstrFrom());
	jsonObjectDynamicFieldSubValue.put("idObjectFrom", fieldSubValue.entityDynamicValue.getIdObjectFrom());
	jsonObjectDynamicFieldSubValue.put("typeObjectFrom", fieldSubValue.entityDynamicValue.getTypeObjectFrom());
	jsonObjectDynamicFieldSubValue.put("idPgmSet", fieldSubValue.entityDynamicValue.getIdPgmFrom());
	jsonObjectDynamicFieldSubValue.put("isDefaultValue", fieldSubValue.entityDynamicValue.isDefaultValue());
	jsonObjectDynamicFieldSubValue.put("isFieldValueFull", fieldSubValue.entityDynamicValue.isFieldValueFull());
	jsonObjectDynamicFieldSubValue.put("defaultValue", fieldSubValue.entityDynamicValue.getDefaultValue());
	jsonObjectDynamicFieldSubValue.put("defaultValueFull", fieldSubValue.entityDynamicValue.getFieldValueFull());
	jsonObjectDynamicFieldSubValue.put("lngInSubField", fieldSubValue.entityDynamicValue.getLngInSubField());
	jsonObjectDynamicFieldSubValue.put("posInSubField", fieldSubValue.entityDynamicValue.getPosInSubField());
	jsonObjectDynamicFieldSubValue.put("value", fieldSubValue.entityDynamicValue.getValue());
	jsonObjectDynamicFieldSubValue.put("progr", fieldSubValue.entityDynamicValue.getProgr());
}


/*
 *  Popolamento jsonObjectDynamicFieldSubSetting da EntityDynamicFieldSubSetting
 */
private void putJsontDynamicFieldSubSetting(EntityDynamicFieldSubSetting edfss, JSONObject jsonObjectDynamicFieldSubSetting, int numChain) {
	
	jsonObjectDynamicFieldSubSetting.put("idProgramOrigin", edfss.getIdObject());           			// Program Origin
	jsonObjectDynamicFieldSubSetting.put("typeProgramOrigin", edfss.getTypeObject());       			// Type program Origin
    jsonObjectDynamicFieldSubSetting.put("numInstrPgmOrigin", edfss.getNumInstr()); 					// Num Instr in Pgm Origin
	
	// Transformation
	jsonObjectDynamicFieldSubSetting.put("setMode", edfss.getSetMode());   								// Type
	jsonObjectDynamicFieldSubSetting.put("numChain", edfss.getNumChain()); 								// Chain Number
	jsonObjectDynamicFieldSubSetting.put("idPgmSet", edfss.getIdPgmSet()); 								// Set Pgm
	jsonObjectDynamicFieldSubSetting.put("numInstrSet", edfss.getNumInstrSet()); 						// Set Num Instr in pgm origin/Caller/Called
	jsonObjectDynamicFieldSubSetting.put("progr", edfss.getProgr());                        			// Progr for order
	
	// Field/Subfield origin
	jsonObjectDynamicFieldSubSetting.put("idField", edfss.getIdField());         						// Field Name
	jsonObjectDynamicFieldSubSetting.put("numField", edfss.getNumField());       						// Field definition
	jsonObjectDynamicFieldSubSetting.put("idSubField", edfss.getIdSubField());   						// Sufield Name
	jsonObjectDynamicFieldSubSetting.put("numSubField", edfss.getNumSubField()); 						// Subfield Definition Instruction
	jsonObjectDynamicFieldSubSetting.put("posInSubField", edfss.getPosInSubField()); 					// Position in subfield (1-based)
	jsonObjectDynamicFieldSubSetting.put("lngInSubField", edfss.getLngInSubField()); 					// Length in subfield (1-based)
	jsonObjectDynamicFieldSubSetting.put("typeObjectPgmSet", edfss.getTypeObjectPgmSet()); 				// Assignment Object Program Type
	jsonObjectDynamicFieldSubSetting.put("numPath", edfss.getNumPath());              					// Assignment Path Number					
	
	// Sender Assignment
	jsonObjectDynamicFieldSubSetting.put("fieldSenderId", edfss.getFieldSenderId());  					// Sender Field Name
	jsonObjectDynamicFieldSubSetting.put("fieldSenderNum", edfss.getFieldSenderNum());					// Sender Definition Instruction
	jsonObjectDynamicFieldSubSetting.put("fieldSenderPos", edfss.getFieldSenderPos());					// Sender Field Position
	jsonObjectDynamicFieldSubSetting.put("fieldSenderLng", edfss.getFieldSenderLng());					// Sender Field Length						
	
	// Receiver Assignment
	jsonObjectDynamicFieldSubSetting.put("fieldReceiverId", edfss.getFieldReceiverId());				// Receiver Field Name
	jsonObjectDynamicFieldSubSetting.put("fieldReceiverNum", edfss.getFieldReceiverNum());				// Receiver Definition Instruction
	jsonObjectDynamicFieldSubSetting.put("fieldReceiverPos", edfss.getFieldReceiverPos());				// Receiver Field Position
	jsonObjectDynamicFieldSubSetting.put("fieldReceiverLng", edfss.getFieldReceiverLng());				// Receiver Field Length						
	
	// Last Assignment Detail
	jsonObjectDynamicFieldSubSetting.put("idObjExt", edfss.getIdObjExt());  							// Sql Table Name /File Name/Ts Name/..
	jsonObjectDynamicFieldSubSetting.put("objExtColField", edfss.getObjExtColField()); 					// Sql Table Column Name
	jsonObjectDynamicFieldSubSetting.put("numUsingParm", edfss.getNumUsingParm());           			// Using Parm Parameter Number
	jsonObjectDynamicFieldSubSetting.put("dspFieldInUsingParm", edfss.getDspFieldInUsingParm()); 		// Using Parm Parameter Displacement
	jsonObjectDynamicFieldSubSetting.put("dspFieldInLinkageArea", edfss.getDspFieldInLinkageArea());	// Linkage/TWA/CSA Field Displacement
	jsonObjectDynamicFieldSubSetting.put("typePointerArea", edfss.getTypePointerArea()); 				// Linkage Area Type Containing The Pointer
	jsonObjectDynamicFieldSubSetting.put("numUsingParmPointer", edfss.getNumUsingParmPointer()); 		// Linkage Using Parameter Number With Pointer
	jsonObjectDynamicFieldSubSetting.put("dspPointerInUsingParm", edfss.getDspPointerInUsingParm());	// Linkage Pointer Displacement In Using Parm
	jsonObjectDynamicFieldSubSetting.put("dspPointerInLinkageArea", edfss.getDspPointerInLinkageArea());// Linkage Pointer Displacement
	jsonObjectDynamicFieldSubSetting.put("getValue", edfss.getValue());  								// Literal Or Explicit Assignment Value, Length Formatted
	
	// External Value
	jsonObjectDynamicFieldSubSetting.put("typeObjExt", edfss.getTypeObjExt());                          // External Source Type (Table, Phis. File, Cics Field,..)
	jsonObjectDynamicFieldSubSetting.put("idObjExt2", edfss.getIdObjExt());  							// External Object Name, Ts, Table, File Origin
	jsonObjectDynamicFieldSubSetting.put("objExtSystem", edfss.getObjExtSystem());                      // Cics System Field Name (ie. EIBTRMID)
	jsonObjectDynamicFieldSubSetting.put("objExtColField", edfss.getObjExtColField());                  // Field/Column Name In Copy
	jsonObjectDynamicFieldSubSetting.put("objExtSqlTableColumn", edfss.getObjExtSqlTableColumn());      // Sql Table Column Name
	jsonObjectDynamicFieldSubSetting.put("objExtIdCopy", edfss.getObjExtIdCopy());           			// Table/File/.. Copy Name
	jsonObjectDynamicFieldSubSetting.put("objExtPosCol", edfss.getObjExtPosCol());						// Record area/copy/TS/TD Col Pos (If No Definition)						
	jsonObjectDynamicFieldSubSetting.put("objExtLengthCol", edfss.getObjExtLengthCol());				// Record area/copy/TS/TD Col lng (If No Definition)
	jsonObjectDynamicFieldSubSetting.put("solvedObjExt", edfss.getSolvedObjExt());  					// True For TS, File etc. Solved For External Object	
	jsonObjectDynamicFieldSubSetting.put("waitingForExternalData", edfss.getWaitingForExternalData()); 	// True When Waiting data For External Object		
	
	// Ordinal
	jsonObjectDynamicFieldSubSetting.put("typeProgramOriginOrdinal", edfss.getTypeObject().ordinal());    
	jsonObjectDynamicFieldSubSetting.put("setModeOrdinal", edfss.getSetMode().ordinal());
	jsonObjectDynamicFieldSubSetting.put("typeObjectPgmSetOrdinal", edfss.getTypeObjectPgmSet().ordinal()); 	
	jsonObjectDynamicFieldSubSetting.put("typeObjExtOrdinal", edfss.getTypeObjExt().ordinal());                   
	jsonObjectDynamicFieldSubSetting.put("typePointerAreaOrdinal", edfss.getTypePointerArea().ordinal()); 
}

/*
 * Labels/Sections e relativi Xref
 */
private void programSummaryParSectionXref(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj) {
	JSONObject jsonObjectLabelSection = null;
	JSONArray jsonArrayLabelSection = null;
	JSONArray jsonArrayLabelSectionXref = null;
	String ar_labelSectionName[] = null;
	int ar_XrefLabelSection[] = null;
    int numDefInstr = 0;
		
    // Labels 
    
    ar_labelSectionName = programCobol.labelNames();
	Arrays.sort(ar_labelSectionName);
	jsonArrayLabelSection = new JSONArray();
		
	// Scan label
	jsonArrayLabelSection = new JSONArray();
	for (String labelSection : ar_labelSectionName) {
		
		jsonObjectLabelSection = new JSONObject();				
		numDefInstr = programCobol.labelPointer(labelSection);
		if (numDefInstr < 0) {continue;}                    // Controllo di sicurezza
				
		ar_XrefLabelSection = programCobol.xrefToLabel(labelSection);
		jsonObjectLabelSection.put("arXref", ar_XrefLabelSection);
		
		// Composizion JsonArray Xref
		jsonArrayLabelSectionXref = new JSONArray();
		for (int i = 0; i < ar_XrefLabelSection.length; i++) {
			jsonArrayLabelSectionXref.put(ar_XrefLabelSection[i]);
		}  
					
		jsonObjectLabelSection.put("label", labelSection);
		jsonObjectLabelSection.put("numDefInstr", numDefInstr);
		jsonObjectLabelSection.put("arXref", jsonArrayLabelSectionXref);
		
		jsonArrayLabelSection.put(jsonObjectLabelSection);
	
	} // end-for
	
	obj.put("arLabel", jsonArrayLabelSection);
	
    // Sections 
    
    ar_labelSectionName = programCobol.sectionNames();
	Arrays.sort(ar_labelSectionName);
	jsonArrayLabelSection = new JSONArray();
		
	// Scan label
	jsonArrayLabelSection = new JSONArray();
	for (String labelSection : ar_labelSectionName) {
		
		jsonObjectLabelSection = new JSONObject();				
		numDefInstr = programCobol.sectionPointer(labelSection);
				
		ar_XrefLabelSection = programCobol.xrefToLabel(labelSection);
		jsonObjectLabelSection.put("arXref", ar_XrefLabelSection);
		
		// Composizion JsonArray Xref
		jsonArrayLabelSectionXref = new JSONArray();
		for (int i = 0; i < ar_XrefLabelSection.length; i++) {
			jsonArrayLabelSectionXref.put(ar_XrefLabelSection[i]);
		}  
					
		jsonObjectLabelSection.put("section", labelSection);
		jsonObjectLabelSection.put("numDefInstr", numDefInstr);
		jsonObjectLabelSection.put("arXref", jsonArrayLabelSectionXref);
		
		jsonArrayLabelSection.put(jsonObjectLabelSection);
	
	} // end-for
	
	obj.put("arSection", jsonArrayLabelSection);		
}

/*
 * Produzione cross reference simboli (costanti, campi)
 */
private void programSummaryParSymbolsXref(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj) {
	JSONObject jsonObjectSymbol = null;
	JSONArray jsonArraySymbol = null;
	JSONArray jsonArrayPointerToDefInEnv = null;
	JSONArray jsonArrayPointerToDefInData =  null;
	JSONArray jsonArrayPointerToDefInProc =  null;
	JSONArray jsonArrayXrefToSymbolInEnvInp =  null;
	JSONArray jsonArrayXrefToSymbolInDataInp =  null;
	JSONArray jsonArrayXrefToSymbolInProcInp =  null;
	JSONArray jsonArrayXrefToSymbolInProcOut =  null;
	EnumSymbolType symbolType = null;
	String ar_symbolName[] = null;	
	String divDef = "";												// E D P
	int ar_pointerToDefInEnv[] = null;
	int ar_pointerToDefInData[] = null;
	int ar_pointerToDefInProc[] = null;
	int ar_xrefToSymbolInEnvInp[] = null;
	int ar_xrefToSymbolInDataInp[] = null;
	int ar_xrefToSymbolInProcInp[] = null;
	int ar_xrefToSymbolInProcOut[] = null;
	
	ar_symbolName = programCobol.symbolNames();
	Arrays.sort(ar_symbolName);
    jsonArraySymbol = new JSONArray();
    
	// Scan symbol
	for (String symbol : ar_symbolName) {
		
		jsonObjectSymbol = new JSONObject();
		jsonArrayPointerToDefInEnv = new JSONArray();
		jsonArrayPointerToDefInData = new JSONArray();
		jsonArrayPointerToDefInProc = new JSONArray();
		jsonArrayXrefToSymbolInEnvInp = new JSONArray();
		jsonArrayXrefToSymbolInDataInp = new JSONArray();
		jsonArrayXrefToSymbolInProcInp = new JSONArray();
		jsonArrayXrefToSymbolInProcOut = new JSONArray();
		
		// Tipo simbolo e clear struttura cumulativa references per riga
		symbolType = programCobol.symbolType(symbol);
	
		divDef = "";

		// Estrazione di tutti i possibili cross references per il simbolo valorizzati in analisi
		ar_pointerToDefInEnv = programCobol.getXrefSymbolDefEnv(symbol);
		ar_pointerToDefInData = programCobol.getXrefSymbolDefData(symbol);
		ar_pointerToDefInProc = programCobol.getXrefSymbolDefProc(symbol);
		ar_xrefToSymbolInEnvInp = programCobol.xrefToSymbolInEnvDivision(symbol);
		ar_xrefToSymbolInDataInp = programCobol.xrefToSymbolInDataDivision(symbol);
		ar_xrefToSymbolInProcInp = programCobol.xrefToSymbolInProcedure(symbol, INSTR_USE_DATA_ITEM_INPUT);
		ar_xrefToSymbolInProcOut = programCobol.xrefToSymbolInProcedure(symbol, INSTR_USE_DATA_ITEM_OUTPUT);
		
		// Pointers a definizioni in Environment division
		if (ar_pointerToDefInEnv != null) {
			divDef = "ENV";
			// Push pointers a definizioni in Environment division
			for (int i = 0; i < ar_pointerToDefInEnv.length; i++) {
				jsonArrayPointerToDefInEnv.put(ar_pointerToDefInEnv[i]);
			} // end-for
		} // end-if

		// Pointers a definizioni in data division
		if (ar_pointerToDefInData != null) {
			divDef = "DATA";
			// Push pointers a definizioni in data division
			for (int i = 0; i < ar_pointerToDefInData.length; i++) {
				jsonArrayPointerToDefInData.put(ar_pointerToDefInData[i]);
			}  
		}  
		
		// Pointers a definizioni in data division
		if (ar_pointerToDefInProc != null) {
			divDef = "PROC";
			// Push pointers a definizioni in procedure division
			for (int i = 0; i < ar_pointerToDefInProc.length; i++) {
				jsonArrayPointerToDefInProc.put(ar_pointerToDefInProc[i]);
			}  
		}  

		// Xref a utilizzi in input in environment division
		if (ar_xrefToSymbolInEnvInp != null) {
			// Push pointers a utilizzi in environment division
			for (int i = 0; i < ar_xrefToSymbolInEnvInp.length; i++) {
				jsonArrayXrefToSymbolInEnvInp.put(ar_xrefToSymbolInEnvInp[i]);
			} 
		}  
		
		// Xref a utilizzi in input in data division
		if (ar_xrefToSymbolInDataInp != null) {
			// Push pointers a utilizzi in data division
			for (int i = 0; i < ar_xrefToSymbolInDataInp.length; i++) {
				jsonArrayXrefToSymbolInDataInp.put(ar_xrefToSymbolInDataInp[i]);
			}  
		}  
				
		// Xref a utilizzi in input in procedure division
		if (ar_xrefToSymbolInProcInp != null) {
			// Push pointers a utilizzi in input in procedure division
			for (int i = 0; i < ar_xrefToSymbolInProcInp.length; i++) {
				jsonArrayXrefToSymbolInProcInp.put(ar_xrefToSymbolInProcInp[i]);
			}  			
		}  
				
		// Xref a utilizzi in output in procedure division	
		if (ar_xrefToSymbolInProcOut != null) {
			// Push pointers a utilizzi in output in procedure division
			for (int i = 0; i < ar_xrefToSymbolInProcOut.length; i++) {
				jsonArrayXrefToSymbolInProcOut.put(ar_xrefToSymbolInProcOut[i]);
			} // end-for
		} // end-if

		jsonObjectSymbol.put("symbol", symbol);
		jsonObjectSymbol.put("symbolType", symbolType);
		jsonObjectSymbol.put("symbolTypeOrdinal", symbolType.ordinal());
		jsonObjectSymbol.put("divDef", divDef);
		
		jsonObjectSymbol.put("arPointerToDefInEnv", jsonArrayPointerToDefInEnv);
		jsonObjectSymbol.put("arPointerToDefInData", jsonArrayPointerToDefInData);
		jsonObjectSymbol.put("arPointerToDefInProc", jsonArrayPointerToDefInProc);
		jsonObjectSymbol.put("arXrefToSymbolInEnvInp", jsonArrayXrefToSymbolInEnvInp);
		jsonObjectSymbol.put("arXrefToSymbolInDataInp", jsonArrayXrefToSymbolInDataInp);
		jsonObjectSymbol.put("arXrefToSymbolInProcInp", jsonArrayXrefToSymbolInProcInp);
		jsonObjectSymbol.put("arXrefToSymbolInProcOut", jsonArrayXrefToSymbolInProcOut);
		
		jsonArraySymbol.put(jsonObjectSymbol);
		
	} // end-for
	
	// Output
    obj.put("arSymbol", jsonArraySymbol);
}

/*
 * Produzione Codice morto come label e section non referenziate o codice non raggiungibile
 */
private void programSummaryDeadCode(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj) {
	JSONArray jsonArrayDeadDataItem = null;
	JSONArray jsonArrayDeadProcInstr = null;
	JSONArray jsonArrayDeadSection = null;
	JSONArray jsonArrayDeadLabel = null;	
	JSONArray jsonArrayDeadCopyData = null;
	JSONArray jsonArrayDeadCopyProc = null;

	ProgramCobolEntry<? extends Instruction>[] ar_entryDataDivision= null;
	ProgramCobolEntry<? extends Instruction>[] ar_entryProcDivision= null;
	ProgramCobolEntry<? extends Instruction> entryProcDivision = null;
	ProgramCobolEntry<? extends Instruction> entryDataDivision = null;
	
	InstructionCobolDataItem dataItem = null;
	String ar_sectionName[] = null;
	String ar_labelName[] = null;
	ArrayList<String> al_copyName = null;
	ArrayList<String> al_deadCodeData = null;
   	String deadCodeFieldName = "";
   	
	// Definizione dati dead code non utilizzate
	al_deadCodeData = new ArrayList<String> ();
		
	// Estrazione campi dead code e ordinamento successivo
	jsonArrayDeadDataItem = new JSONArray();
	ar_entryDataDivision = programCobol.entriesData();
	for (int i = 0; i < ar_entryDataDivision.length; i++) {
		entryDataDivision = ar_entryDataDivision[i];
		
		// Interessano solo le definizioni dati
		if (!(entryDataDivision.getInstruction() instanceof InstructionCobolDataItem)) {
			continue;
		}
		// Interessa solo il dead code
		if (!entryDataDivision.isDeadCode()) {
			continue;
		}
		
		dataItem = (InstructionCobolDataItem) entryDataDivision.getInstruction();
		deadCodeFieldName = dataItem.getDataName();
		
		// Non interessano i filler
		if (deadCodeFieldName.equals("FILLER")) {
			continue;
		}
		
		al_deadCodeData.add(deadCodeFieldName);
	}
	Collections.sort(al_deadCodeData);
		
	ar_entryDataDivision = programCobol.entriesData();
	for (int i = 0; i < al_deadCodeData.size(); i++) {		
		deadCodeFieldName = al_deadCodeData.get(i);
		jsonArrayDeadDataItem.put(deadCodeFieldName);
	}
	
	// Istruzioni dead code non raggiungibili
	jsonArrayDeadProcInstr = new JSONArray();
	ar_entryProcDivision = programCobol.entriesProcedure();
	for (int i = 0; i < ar_entryProcDivision.length; i++) {
		
		entryProcDivision = ar_entryProcDivision[i];
		
		// Istruzione raggiungibile, non dead code: skip
		if (!entryProcDivision.isDeadCode()) {
			continue;
		}
		jsonArrayDeadProcInstr.put(i);
	}
	
	// Section non referenziate
	jsonArrayDeadSection = new JSONArray();
	ar_sectionName = programCobol.procInternalNames();
	Arrays.sort(ar_sectionName);	
	for (String sectionName : ar_sectionName) {		
		if (programCobol.isSectionReferenced(sectionName)) {continue;}   
		jsonArrayDeadSection.put(sectionName);
	}

	// Label non referenziate
	jsonArrayDeadLabel = new JSONArray();
	ar_labelName = programCobol.labelNames();
	Arrays.sort(ar_labelName);	
	for (String labelName : ar_labelName) {
		if (programCobol.isLabelReferenced(labelName)) {continue;}   
		jsonArrayDeadLabel.put(labelName);
	}

	// Copy Data Division definiti e non utilizzati
	jsonArrayDeadCopyData = new JSONArray();
	al_copyName = programCobol.deadCodeCopyData();
	Collections.sort(al_copyName);	
	for (String copyName : al_copyName) {
		jsonArrayDeadCopyData.put(copyName);
	}

	// Copy Procedure Division definiti e non utilizzati
	jsonArrayDeadCopyProc = new JSONArray();
	al_copyName = programCobol.deadCodeCopyProc();
	Collections.sort(al_copyName);
	
	for (String copyName : al_copyName) {
		jsonArrayDeadCopyProc.put(copyName);
	}

	// Output
	obj.put("arDeadDataItem", jsonArrayDeadDataItem);
	obj.put("arDeadSection",  jsonArrayDeadSection);
	obj.put("arDeadLabel",    jsonArrayDeadLabel);
	obj.put("arDeadCopyData", jsonArrayDeadCopyData);
	obj.put("arDeadCopyProc", jsonArrayDeadCopyProc);
	obj.put("arDeadProcInstr",jsonArrayDeadProcInstr);
}

/*
 * Produzione metriche complete di programma
 */
private void programSummaryMetrics(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
	List<EntityMetricValue> ls_object = null;
	EntityMetricValue eo = null;

	Connection conn = DataBaseConnections.getConnection();
	IDAOMetricValue eoDAO = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn, false, false, ucfg);

    ls_object = eoDAO.findAll(sys, subSys, EnumMetricsScope.SCOPE_LEVEL_OBJECT, idProgram, EnumObject.OBJECT_PGM_COBOL);

    eo = ls_object.get(0);
    
    // Metriche non disponibili
    if (ls_object.size() == 0) {
    	obj.put("available", false);
		return;
	}
    
    obj.put("available", true);
    RestDbAccessDAOEntity.populateJsonMetricValue(eo, obj);
}

/*
 * Produzione fattori qualità di programma
 */
private void programSummaryQuality(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
	// TODO
}

/*
 * Produzione violazioni alle regole di programma
 */
private void programSummaryViolations(String user, String sys, String subSys, String idProgram, ProgramCobol programCobol, JSONObject obj, UserConfiguration ucfg) throws ExceptionAmritaSqlError, SQLException {
	// TODO
	return;
}


private String getResultJsonObjectSource(String user, String sys, String subSys, String idObject, String type ) throws  JSONException, SQLException, ExceptionAmrita {

	ProgramCobol programCobol = null;
	JSONObject jsonObject = null; 
	JSONArray jsonArray = null; 		
	List<InnerRow> ls_rows = null;
	InnerRow row = null;
	String result = "";
	int nRow = 0;

	// Get program
	programCobol = (ProgramCobol) AmritaStartup.cache.get(idObject);
    if (programCobol == null) {
    	programCobol = getProgram(user, idObject);  
		AmritaStartup.cache.put(idObject, programCobol);
	}			

	// Entries correnti di procedure, entry e istruzione Cobol
	ProgramCobolEntry<? extends Instruction> ar_procEntry[] = null;
	ProgramCobolEntry<? extends Instruction> procEntry = null;
	Instruction instruction = null;

	ar_procEntry = programCobol.entriesProcedure();
	ls_rows=new ArrayList<InnerRow>();

	try {


		// Scan Entries di programma 
		for (int i = 0; i < ar_procEntry.length; i++) {

			// Estraggo istruzione e tipo istruzione
			procEntry = ar_procEntry[i];

			// Interessano solo le istruzioni di procedure division, non Copy, non precompiler ...
			if (!(procEntry.getInstruction() instanceof InstructionCobolProcedure)
			&&  !(procEntry.getInstruction() instanceof InstructionCics)
			&&  !(procEntry.getInstruction() instanceof InstructionSql)) {
			continue;
			}
			instruction = procEntry.getInstruction();

			row=new InnerRow(); 
			nRow=instruction.getRowStartSource() - instruction.getCommentsBefore().length;

			// Put comments rows before instruction
			for (int j = 0; j < instruction.getCommentsBefore().length; j++) {
				row.nInstr = instruction.getNumInstr();
				row.nRow = nRow;
				int lngComm=instruction.getCommentsBefore()[j].length();
				if (lngComm > 7) {
					row.row = instruction.getCommentsBefore()[j].substring(6);
					row.commLeft = instruction.getCommentsBefore()[j].substring(0,5);
					row.rowStartSource=instruction.getRowStartSource();
					row.rowEndSource=instruction.getRowEndSource();
					row.division="P";
					ls_rows.add(row);
					row=new InnerRow();  
					nRow++;
				}
			}

			// Put instruction rows
			nRow=instruction.getRowStartSource();
			row=new InnerRow(); 

			// Scan rows source instructions stored in the entry
			for (int j = 0; j < instruction.getRowsSource().length; j++) {

				// Populate row  
				row.plusMinus = "";  
				row.nInstr = instruction.getNumInstr();
				row.nRow = nRow;
//				row.commLeft = instruction.getCommentsLeft()[0];     		    
				row.row = instruction.getRowsSource()[j].substring(6);
//				if (instruction.getCommentsRight().length > 72) {
//					row.commRight = instruction.getCommentsRight()[73];
//				}

				// Data from source analysis
				row.copyName = "";
				row.isCopyStmt = false;
				if (procEntry.getEntryType() == EnumInstrDataCategory.COBOL_COPY_NAME_PROC) {
					row.copyName = procEntry.getUnderCopyName();
					row.isCopyStmt = true;
				}
				row.isDeadCode = procEntry.isDeadCode();
				row.isDeadCodeUnreachable = procEntry.isDeadCodeUnreachable();
				row.isEntryPrecompiler = procEntry.isEntryPrecompiler();
				row.isLabelOrSection = false;
				if (procEntry.getEntryType() == EnumInstrDataCategory.COBOL_PROC_LABEL
				|| procEntry.getEntryType() == EnumInstrDataCategory.COBOL_PROC_PROC_INTERNAL ) {
					row.isLabelOrSection = true;
				}

				row.numInstrRelated = procEntry.getNumInstrRelated();
				row.numEntryOwnerConditional = procEntry.getNumEntryOwnerConditional();
				row.rowStartSource=instruction.getRowStartSource();
				row.rowEndSource=instruction.getRowEndSource();
				row.division="P";

				ls_rows.add(row);

				row=new InnerRow();  
				nRow++;
			}

		} // end-for

	} catch (Exception e) {
		nRow=0;
	}


	// No data
	jsonArray = new JSONArray();

	// Populate Json
	for (InnerRow oRow : ls_rows) {
		jsonObject = new JSONObject(oRow);
		populateJsonObjectSourceRow(oRow, jsonObject);
		jsonArray.put(jsonObject);
	}

	// Return json object
	result = jsonArray.toString();		

	return result;
}  




private void populateJsonObjectSourceRow(InnerRow eo, JSONObject jsonObject) {
	jsonObject.put("division", eo.division);
	jsonObject.put("plusMinus", eo.plusMinus);
	jsonObject.put("nInstr", eo.nInstr);
	jsonObject.put("nRow", eo.nRow);
	jsonObject.put("commLeft", eo.commLeft);	
	jsonObject.put("row", eo.row);	
	jsonObject.put("commRight", eo.commRight);
	jsonObject.put("copyName", eo.copyName);
	jsonObject.put("isDeadCode", eo.isDeadCode);
	jsonObject.put("isDeadCodeUnreachable", eo.isDeadCodeUnreachable);
	jsonObject.put("isEntryPrecompiler", eo.isEntryPrecompiler);
	jsonObject.put("isLabelOrSection", eo.isLabelOrSection);
	jsonObject.put("isCopyStmt", eo.isCopyStmt);  
	jsonObject.put("rowStartSource", eo.rowStartSource);
	jsonObject.put("rowEndSource", eo.rowEndSource);
}


/*
 * Describe a single row returned to the caller
 */
class InnerRow {
	String division = "";    // Procedure, Data, Identification

	// Info to show the row
	String plusMinus = "";  
	int nInstr = 0;
	int nRow = 0;
	String commLeft = "";
	String row = "";
	String commRight = "";

	// Data from source analysis
	String copyName = "";
	boolean isDeadCode = false;
	boolean isDeadCodeUnreachable = false;
	boolean isEntryPrecompiler = false;
	boolean isLabelOrSection = false;
	boolean isCopyStmt = false;
	int numInstrRelated = 0;
	int numEntryOwnerConditional = 0;
	int rowStartSource = 0;
	int rowEndSource = 0;	
}	

/*
 * Recupero programma java serializzato da directory a livello di user
 * 
 */
public ProgramCobol getProgram(String user, String idObject) throws ExceptionAmrita  {
	UserConfiguration ucfg = null;
	SystemService ss = null;                  // Gestore generalizzato servizi di sistema
	ProgramCobol pgmCobol = null; 
	Object objUnserialized = null;

	// User mast be logged in
	// Login should be done	
	// If not take default properties
	ucfg = UserActive.getUser(user);
	if (ucfg == null) {
		ucfg = new UserConfiguration(user);
	}

	// Get Program or Copy object
	ss = new SystemService(ucfg, null);
	ss.setSourceObjectContainer(idObject);

	objUnserialized = ss.getSerialized(ucfg.getPathUser() + File.separator + ucfg.getDirCobolObjPgm(), idObject, SUFFIX_SERIALIZED_PGM);

	// Programma serializzato non trovato, impossibile produrre documentazione: messaggio già fornito
	if (objUnserialized != null && objUnserialized instanceof ProgramCobol) {
		pgmCobol = (ProgramCobol) objUnserialized;

	}
	return pgmCobol;

}

/*
 * Service class for copy/include list
 */
class ProgramCopyInclude  implements Comparable<ProgramCopyInclude> {
	String type = "";                               // C I (Copy, Include)
	String name = "";								// Copy name or Sql Include name
    String division = "";                           // I E D P
	int numInstr = 0;                               // Numero istruzione
	int rowStart = 0;								// Riga di inizio stmt  nel source
	int rowEnd = 0;                                 // Riga di fine   stmt nel source
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		ProgramCopyInclude o2 = (ProgramCopyInclude) o;
		return division.equals(o2.division) && name.equals(o2.name);
	}

	@Override
	public int compareTo(ProgramCopyInclude objToCompare) {
		if (this.division.compareTo(objToCompare.division) != 0) {
			return this.division.compareTo(objToCompare.division);
		}

		return this.name.compareTo(objToCompare.name);
	}

}
/*
 * Service class for labels/sections
 */
class ProgramStructure  {
	String showText = "";							// Da mostrare per istruzioni significative
	String performFrom = "";
	String performThru = "";
	String inCopy = "";								// Paragrafo/section definito dentro copy
	boolean isPerform = false;  		    		// True = perform, false = READ, UPDATE, LINK, RETURN, CALL,...
	boolean isFromSection = false;  				// Perform a Section
	boolean isWithPerformsInside = false; 			// Presenti perform dentro il paragrafo/section
	boolean isPerformUnderIf = false; 	    		// True = perform sotto condizione, false altrimenti
	boolean isParagraphSectionWithStopRun = false;  // True = paragrafo/section con dentro stop run/goback/ ..
	int numInstrFrom = 0;
	int numInstrThru = 0;
	int rowFrom = 0;
	int rowThru = 0;
	int levelNesting = 0;                   		// 0 = mainline
	int numInstr = 0;								// Numero istruzione di struttura dentro paragrafo/section
	int rowNumStart = 0;							// Numero riga inizio di struttura dentro paragrafo/section
	int rowNumEnd = 0;							    // Numero riga fine   di struttura dentro paragrafo/section
}

/*
 * Service class for labels/sections
 */
class LabelSection implements Comparable<LabelSection> {
	String name = "";
	String inCopy = "";
	int numInstr = 0;
	int rowStart = 0;
	int rowEnd = 0;
	int copyInstr = 0;
	int copyRowStart = 0;
	int copyRowEnd = 0;
	boolean isSection = false;
	
	@Override
	public int compareTo(LabelSection o) {
	return this.name.compareTo(o.name);	}
}

}


