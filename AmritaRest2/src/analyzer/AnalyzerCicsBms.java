
package analyzer;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import utilities.DateTimeService;
import entities.EntityMapDescriptor;
import entities.EntityMapItem;
import entities.EntityObject;
import entities.EntityObjectAnalysisError;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import enums.EnumInstrDataCategory;
import enums.EnumMessageType;
import enums.EnumModule;
import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumRelation;
import enums.EnumRelationSourceProcess;
import enums.EnumRelationType;
import enums.EnumSourceType;
import enums.EnumTypeProcessAnalysis;
import exception.ExceptionAmrita;

    
/**
 * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * AnalyzerJcl
 * </h1>
 * <p>
 * Questa classe gestisce l'analisi di un sorgente Cics BMS Completo. <br>
 * Non viene generato nessun oggetto Java serializzata ma vengono solo popolate le tabelle interne di gestione. <br>
 * <p>
 * Il source può contenere le seguenti tipologie di direttive Assembler:
 * <ul>
 * 
 *  <li><b>jDFHMSD</b></li><br>
 *  Rappresenta il mapset contenitore di una o più mappe
 *  
 *  <li><b>DFHMDI</b></li><br>
 *  Rappresenta la mappa video vera e propria e in un mapset possono esserci più DFHMDI<br>
 *  
 *  <li><b>END</b></li><br>
 *  Marca la fine delle definizioni<br>
 *  
 * </ul>

 * <p>
 * 
 * Vengono inseriti gli oggetti <br>
 * 
 * <ul>
 *  <li><b>OBJECT_CICS_BMS</b></li><br>
 *  Rappresenta il nome del sorgente
 *  
 *  <li><b>OBJECT_CICS_MAPSET</b></li><br>
 *  Rappresenta il nome del mapset
 * 
 *  <li><b>OBJECT_CICS_MAP</b></li><br>
 *  Rappresenta il nome della mappa bms, una mappa con lo stesso nome può comparire in mapset diversi. <br>
 *  Nei programmi Cics la mappa è sempre abbinata al suo mapset.
 * 
 *  <li><b>OBJECT_CICS_FORM</b></li><br>
 *  Identifica univocamente una mappa video con id <b>map-mapset</b><br>
 * </ul>
 *  
 * <p>
 * Vengono gestite le seguenti relazioni <br>
 * 
 * <ul>
 *  <li><b>CICS_MAP_MAPSET</b></li><br>
 *  <li><b>CICS_MAP_BMS_SOURCE</b></li><br>
 *  <li><b>CICS_MAPSET_BMS_SOURCE</b></li><br>
 *  <li><b>CICS_FORM_BMS_SOURCE</b></li><br>
 * </ul>
 *  
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 11/03/2021
*/
public class AnalyzerCicsBms extends Analyzer implements AmritaConstants {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali per analisi sorgente  jcl                                                 
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
    // Gestore centralizzato aggiornamenti db cumulativi.
    private AnalyzerDbInfo analyzerDbInfo = null;
   
 	// Valori correnti generali
    private String sourceNameUnderParsing = "" ;    				// Nome BMS attualmente sotto analisi           	
	private String sourceBmsName = "";              				// Coincide con sourceNameUnderParsing
    private String curDFH = "";                     				// DFHMSD DFHMDI DFHMDF
    private int curRow = 0;                     		    	    // Numero riga corrente
    private int curRowDFHMDI = 0;                     		    	// Numero riga con DFHMDI
	private int iCurRow = -1;                       				// Pointer a riga source da analizzare
	private String ar_RowsSourceInstr[] = null;     				// Righe del source origine 
	private String sourceInstrError = "";     					    // Istruzione sorgente in errore
	private int rowStartSourceError = 0;     					    // Riga inizio source in errore
	private int rowEndSourceError = 0;     					        // Riga fine source in errore
    private boolean isAnyInstructionErrorDetected = false;
    
    // Sistema/sottosistema del sorgente sotto analisi
    private UserExitInfo userExitInfoPgm = null;                                  
    
    
    /**
	 * Costruttore  per analisi sorgente BMS
	 */
	public AnalyzerCicsBms(UserConfiguration ucfg, ExecutionDirectives di, String sourceBmsName) {
		super(ucfg, di);
		 
	    // Inizializzazioni varie
		this.analyzerDbInfo = new AnalyzerDbInfo(ucfg, di, "");
		this.sourceBmsName = sourceBmsName;
	} 

 
	/**
	 * <h1>
	 * Analizza il sorgente del jcl memorizzato nell'istanza di SourceInput.<br>
	 * </h1>
	 * Nome jcl e istanza di SourceInput sono stati valorizzati dal costruttore di questa classe.<br>
	 * <p>
	 * @throws Exception 
	 */
	public void analyzeCicsBms(SourceInput si, String bmsSourceName) throws Exception {
		Map<String, ArrayList<String>> map_paramValueDFHMSD = null;	
		Map<String, ArrayList<String>> map_paramValueDFHMDI = null;	
		ArrayList<InnerField> al_fieldDFHMDF = null;
		ArrayList<String> al_parm = null;
	    String curMapsetDFHMSD = "";            				// mapset DFHMSD 
	    String curMapDFHMDI = "";               				// map DFHMDI 
	    String curFieldDFHMDF = "";             				// map DFHMDF 
		String sourceInstr = "";		
		InnerField field = null;
		
		initialOperations(si, bmsSourceName);   			// Operazioni iniziali per oggetto bms da analizzare
		
		// Mapset DFHMSD
		sourceInstr = getNextAssemblerDirective();          
        
		// Deve essere DFHMSD TYPE=DSECT
		if (!this.curDFH.equals("DFHMSD")) {
			this.sourceInstrError = sourceInstr;
			this.rowStartSourceError = this.iCurRow;
			this.rowEndSourceError = this.iCurRow;
			this.isAnyInstructionErrorDetected = true;
			finalOperations();   
			return;
		}
		curMapsetDFHMSD = sourceInstr.substring(0, 7);		
		map_paramValueDFHMSD = extractParamsAndValues(sourceInstr);     	// Extract parameters and values and populate map		
		
		// Source vuoto inizia con DFHMSD TYPE=FINAL
		al_parm = map_paramValueDFHMSD.get("TYPE");
		if (al_parm != null && al_parm.size() > 0 && al_parm.get(0).contentEquals("FINAL")) {
			prepareForUpdateObjectBmsSourceStatus(bmsSourceName);
			finalOperations();   
			return;
		}
		
		
		// Ahead Map DFHMDI
		sourceInstr = getNextAssemblerDirective();           		    			
		
		// Scan definizioni map
		while (this.curDFH.equals("DFHMDI")) {
			
			this.curRowDFHMDI = this.curRow;
			curMapDFHMDI = sourceInstr.substring(0, 7);
			al_fieldDFHMDF = new ArrayList<InnerField>();
			map_paramValueDFHMDI = extractParamsAndValues(sourceInstr);     // Extract parameters and values and populate map
			
			// Ahead field DFHMDI
			sourceInstr = getNextAssemblerDirective();       
			
			// Scan fields
			while (this.curDFH.equals("DFHMDF")) {	
				curFieldDFHMDF = sourceInstr.substring(0, 7);
				field = new InnerField();
				field.fieldName = curFieldDFHMDF;
				field.map_paramValueDFHMDF = extractParamsAndValues(sourceInstr);
				al_fieldDFHMDF.add(field);
				
				// next DFHMDF field
				sourceInstr = getNextAssemblerDirective();   				
			}

			// DFHMDF devono essere chiusi da altra definizione mappa DFHMDI o da DFHMSD TYPE=FINAL  
			if (!this.curDFH.equals("DFHMDI") && !this.curDFH.equals("DFHMSD")) {
				this.sourceInstrError = sourceInstr;
				this.rowStartSourceError = this.iCurRow;
				this.rowEndSourceError = this.iCurRow;
				this.isAnyInstructionErrorDetected = true;
				finalOperations();   
				return;
			}
			
			// Operazioni finali mappa corrente
			updateMapOnDb(bmsSourceName, map_paramValueDFHMSD, map_paramValueDFHMDI, curMapsetDFHMSD, curMapDFHMDI, al_fieldDFHMDF);

			// DFHMSD TYPE = FINAL
			if (this.curDFH.equals("DFHMSD")) {
				break;
			}
			
			// In loop next map
			
		} // end-while
		
		finalOperations();   
	}


	/*
	 * Inserimento oggetti e relazioni mappa/mapset/form/campi
	 */
	private void updateMapOnDb(String bmsSource, Map<String, ArrayList<String>> map_paramMapset, Map<String, ArrayList<String>> map_paramMap, String mapset, String map, ArrayList<InnerField> al_field) throws ExceptionAmrita, SQLException {
		ArrayList<InnerField> al_fieldNormalized = null;
		Map<String, ArrayList<String>> map_paramField = null;
		EntityObject eoBmsSource = null;
		EntityMapDescriptor eoMapDescriptor = null;
		EntityMapItem eoMapItem = null;
		EntityObject eo = null;
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
		Set<Entry<String, ArrayList<String>>> entry_map_paramField = null;
		ArrayList<String> al_paramValue = null;
		InnerField field = null;
		String paramName = "";
		@SuppressWarnings("unused")
		boolean isBmsSourceFound = false;
        
		// Delete preventivo relazioni e origini
		sqlInsDeleteRelationMap(bmsSource, map, mapset);
		
		///////////////////////////////////////////////////////////////////////
		// Mappa, mapset, form
		///////////////////////////////////////////////////////////////////////
		
		eoBmsSource = prepareForUpdateObjectBmsSourceStatus(bmsSource);
		
		// Inserimento object OBJECT_CICS_MAP
		eo = this.analyzerDbInfo.cloneObject(eoBmsSource);
		eo.setTypeObject(EnumObject.OBJECT_CICS_MAP);
		eo.setIdObject(map);
		eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
		this.analyzerDbInfo.addObjEntity(eo);
		
		// Inserimento object OBJECT_CICS_MAPSET
		eo = this.analyzerDbInfo.cloneObject(eoBmsSource);
		eo.setTypeObject(EnumObject.OBJECT_CICS_MAPSET);
		eo.setIdObject(mapset);
		eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
		this.analyzerDbInfo.addObjEntity(eo);

		// Inserimento object OBJECT_CICS_FORM
		eo = this.analyzerDbInfo.cloneObject(eoBmsSource);
		eo.setTypeObject(EnumObject.OBJECT_CICS_FORM);
		eo.setIdObject(map + "-" + mapset);
		eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
		this.analyzerDbInfo.addObjEntity(eo);
		
		// Inserimento mapDescriptor OBJECT_CICS_MAP
		eoMapDescriptor = new EntityMapDescriptor();
		eoMapDescriptor.setSystem(this.di.systemInput);
		eoMapDescriptor.setSubSystem(this.di.subSystemInput);
		eoMapDescriptor.setIdObject(map);
		eoMapDescriptor.setIdObjectMap(map);
		eoMapDescriptor.setIdObjectMapset(mapset);
		eoMapDescriptor.setIdObjectBmsSource(bmsSource);
		eoMapDescriptor.setTypeObject(EnumObject.OBJECT_CICS_MAP);
		eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
		this.analyzerDbInfo.addObjMapDescriptor(eoMapDescriptor);		
		
		// Inserimento relazione CICS_MAP_MAPSET, 
   		er = new EntityRelation();
		er.setSystem(this.di.systemInput);   						 
		er.setSubSystem(this.di.subSystemInput); 
  		er.setIdObjectA(map);
   		er.setIdObjectB(mapset);
   		er.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		er.setTypeObjectB(EnumObject.OBJECT_CICS_MAPSET);
   		er.setRelation(EnumRelation.CICS_MAP_MAPSET);
   		this.analyzerDbInfo.addObjEntityRelation(er);	
   		
   	    // Inserimento origine relazione CICS_MAP_MAPSET, 
   		ero = new EntityRelationOrigin();
   		ero.setSystem(this.di.systemInput);   						 
   		ero.setSubSystem(this.di.subSystemInput); 
   		ero.setIdObjectRelA(map);
   		ero.setIdObjectRelB(mapset);
   		ero.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		ero.setTypeObjectB(EnumObject.OBJECT_CICS_MAPSET);
  		ero.setRelation(EnumRelation.CICS_MAP_MAPSET);
  		ero.setNumInstrOrigin(0);
 		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
		ero.setIdObjectOrigin(bmsSource);
		ero.setTypeObjectOrigin(EnumObject.OBJECT_CICS_BMS_SOURCE);
		ero.setRowStart(this.curRowDFHMDI);
		ero.setRowEnd(this.curRowDFHMDI);
		this.analyzerDbInfo.addObjEntityRelationOrigin(ero);
		
		// Inserimento relazione CICS_MAP_BMS_SOURCE         
   		er = new EntityRelation();
		er.setSystem(this.di.systemInput);   						 
		er.setSubSystem(this.di.subSystemInput); 
  		er.setIdObjectA(map);
   		er.setIdObjectB(bmsSource);
   		er.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		er.setTypeObjectB(EnumObject.OBJECT_CICS_BMS_SOURCE);
   		er.setRelation(EnumRelation.CICS_MAP_BMS_SOURCE);
   		this.analyzerDbInfo.addObjEntityRelation(er);
   		
   	    // Inserimento origine relazione CICS_MAP_BMS_SOURCE
   		ero = new EntityRelationOrigin();
   		ero.setSystem(this.di.systemInput);   						 
   		ero.setSubSystem(this.di.subSystemInput); 
   		ero.setIdObjectRelA(map);
   		ero.setIdObjectRelB(bmsSource);
   		ero.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		ero.setTypeObjectB(EnumObject.OBJECT_CICS_BMS_SOURCE);
  		ero.setRelation(EnumRelation.CICS_MAP_BMS_SOURCE);
  		ero.setNumInstrOrigin(0);
 		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
		ero.setIdObjectOrigin(bmsSource);
		ero.setTypeObjectOrigin(EnumObject.OBJECT_CICS_BMS_SOURCE);
		ero.setRowStart(this.curRowDFHMDI);
		ero.setRowEnd(this.curRowDFHMDI);
		this.analyzerDbInfo.addObjEntityRelationOrigin(ero);
  		
		// Inserimento relazione  CICS_FORM_BMS_SOURCE,               
   		er = new EntityRelation();
		er.setSystem(this.di.systemInput);   						 
		er.setSubSystem(this.di.subSystemInput); 
  		er.setIdObjectA(map+"-"+mapset);
   		er.setIdObjectB(bmsSource);
   		er.setTypeObjectA(EnumObject.OBJECT_CICS_FORM);
   		er.setTypeObjectB(EnumObject.OBJECT_CICS_BMS_SOURCE);
   		er.setRelation(EnumRelation.CICS_FORM_BMS_SOURCE);
   		this.analyzerDbInfo.addObjEntityRelation(er);
   		
   	    // Inserimento origine relazione CICS_FORM_BMS_SOURCE     
   		ero = new EntityRelationOrigin();
   		ero.setSystem(this.di.systemInput);   						 
   		ero.setSubSystem(this.di.subSystemInput); 
   		ero.setIdObjectRelA(map+"-"+mapset);
   		ero.setIdObjectRelB(bmsSource);
   		ero.setTypeObjectA(EnumObject.OBJECT_CICS_FORM);
   		ero.setTypeObjectB(EnumObject.OBJECT_CICS_BMS_SOURCE);
  		ero.setRelation(EnumRelation.CICS_FORM_BMS_SOURCE);
  		ero.setNumInstrOrigin(0);
 		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
		ero.setIdObjectOrigin(bmsSource);
		ero.setTypeObjectOrigin(EnumObject.OBJECT_CICS_BMS_SOURCE);
		ero.setRowStart(this.curRowDFHMDI);
		ero.setRowEnd(this.curRowDFHMDI);
		this.analyzerDbInfo.addObjEntityRelationOrigin(ero);
		
		///////////////////////////////////////////////////////////////////////
		// Campi mappa  
		///////////////////////////////////////////////////////////////////////
        
//		al_fieldNormalized = adjustFieldsGroups(al_field);  // Groups and occurs
		al_fieldNormalized = al_field;
		
		// Scan campi definiti
		for (int i = 0; i < al_fieldNormalized.size(); i++) {
			field = al_fieldNormalized.get(i);
			
			// Caricamento EntityMapItem
			eoMapItem = new EntityMapItem();
			eoMapItem.setSystem(this.di.systemInput);
			eoMapItem.setSubSystem(this.di.subSystemInput);
			eoMapItem.setIdObject(map);
			eoMapItem.setTypeObject(EnumObject.OBJECT_CICS_MAP);
			eoMapItem.setBmsIdField(field.fieldName);
			eoMapItem.setRow(0);                              // Updated later by setMapItemParamValues
			eoMapItem.setCol(0);                              // Updated later by setMapItemParamValues
				
			// Scan field parameters
			map_paramField = field.map_paramValueDFHMDF;
			entry_map_paramField = map_paramField.entrySet();
			for (Entry<String, ArrayList<String>> entryParamField : entry_map_paramField) {
				paramName = entryParamField.getKey();
				al_paramValue = entryParamField.getValue();
				setMapItemParamValues(eoMapItem, paramName, al_paramValue);					
			} 				

			this.analyzerDbInfo.addObjMapItem(eoMapItem);   // Inserimento mapItem in struttutra db
						
		 } // for - next field		
	}

	
	@SuppressWarnings("unused")
	private EntityObject prepareForUpdateObjectBmsSourceStatus(String bmsSource) throws ExceptionAmrita, SQLException {
		EntityObject eoBmsSource = null;
		boolean isBmsSourceFound = false;
		
        // Source bms per recupero libreria e path source
	    eoBmsSource = new EntityObject();
		eoBmsSource.setSystem(this.di.systemInput);   						 
		eoBmsSource.setSubSystem(this.di.subSystemInput);    
		eoBmsSource.setTypeObject(EnumObject.OBJECT_CICS_BMS_SOURCE);
		eoBmsSource.setIdObject(bmsSource);
		eoBmsSource.setTypeSource(EnumSourceType.CICS_BMS);
		isBmsSourceFound = analyzerDbInfo.getObject(eoBmsSource);
		eoBmsSource.setStatus(EnumObjectStatus.OBJECT_ANALYZED_WITH_NO_ERRORS);   
        
		// Prima analisi
		if (eoBmsSource.getStatus() == EnumObjectStatus.OBJECT_TO_BE_ANALYZED) {
			this.analyzerDbInfo.completeInfoObject(eoBmsSource);
		// Update ultima analisi
		} else {
			eoBmsSource.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
			eoBmsSource.setTmLastAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");
		}
		this.analyzerDbInfo.addObjEntity(eoBmsSource);   // To be updated
        return eoBmsSource;
	}


	/*
	 * Detect occurrences of groups of fields
	 * Assign progressive number to each group
	 * Store occurrences number
	 * Delete non necessary fields
	 */
	private ArrayList<InnerField> adjustFieldsGroups(ArrayList<InnerField> al_field) {
		ArrayList<InnerField> al_fieldNormalized = null;
		InnerField field = null;
		InnerField field1 = null;
		InnerField field2 = null;
		String fieldName = "";
		String fieldName1 = "";
		String fieldName2 = "";
		int numGroup = 0;
		int numOcc = 0;
		int i1 = 0;
		int i2 = 0;
		int i3 = 0;
		
		// Scan fields
		for ( i1 = 0; i1 < al_field.size(); i1++) {
			field1 = al_field.get(i1);
			fieldName1 = field1.fieldName.substring(0, field1.fieldName.length() - 1);
			
			// Non è sicuramente un campo di inizio gruppo
			if (field1.fieldName.isBlank()) {
				continue;
			}
			
			// Scan next fields to find occurs groups
			for ( i2 = i1 + 1; i2 < al_field.size(); i2++) {
				field2 = al_field.get(i2);
				fieldName2 = field2.fieldName.substring(0, field2.fieldName.length() - 1);
				
				// Next field occurrence found
				if (fieldName1.equals(fieldName2)) {
					
					// Compute number of occurrences
					numOcc = 1;
					for ( i3 = i1 + 1; i3 < al_field.size(); i3++) {						 
						field = al_field.get(i3);
						fieldName = field.fieldName.substring(0, field.fieldName.length() - 1);
						if (fieldName.equals(fieldName1)) {
							numOcc++;
						}
					}

					// Mark with group owner
					numGroup++;
					for (i3 = i1; i3 < i2; i3++) {						 
						field = al_field.get(i3);
						field.fieldName = field.fieldName.substring(0, field.fieldName.length() - 1); // remove last char
						field.bmsNumGrp = numGroup;
						field.bmsNumGrpOccurs = numOcc;
					}					 
					
					i1 = i1 + numOcc - 1 + (numOcc - 1) * numGroup; // Point for last field occurs 
					
					// Mark all others groups fields to be deleted
					for (i3 = i1 + numOcc; i3 <= i1; i3++) {	
						field = al_field.get(i3);
						field.isFieldToDelete = true;
					}
					
				} // end-if
				
			} // end-for
			
		} // end-for
		
		// Delete all unnecessary fields groups
		al_fieldNormalized = new ArrayList<>();
		for ( i1 = 0; i1 < al_field.size(); i1++) {
			field = al_field.get(i1);
			if (field.isFieldToDelete) {
				continue;
			}
			al_fieldNormalized.add(field);
		}		
		return al_fieldNormalized;
	}


	/*
	 * Impostazione valori parametro campo di mappa
	 * 
	 * es. ATTRB=(ASKIP,FSET,BRT)
	 *     POS=(06,63)
	 *     LENGTH=017
	 *     INITIAL='Codice Societa'':'
	 *     JUSTIFY=(RIGHT,BLANK)
	 *     
	 *  Verifica se campo di gruppo e in questo caso impostazione numero grupp e numero occurs   
	 */
    private void setMapItemParamValues(EntityMapItem eoMapItem, String paramName, ArrayList<String> al_paramValue) {
		String str = "";
		
    	// POS
    	if (paramName.equals("POS")) {
    		eoMapItem.setRow(Integer.parseInt(al_paramValue.get(0)));
    		eoMapItem.setCol(Integer.parseInt(al_paramValue.get(1)));
    		return;
		}
    	
    	// LENGTH
    	if (paramName.equals("LENGTH")) {
    		eoMapItem.setBmsLength(Integer.parseInt(al_paramValue.get(0)));
     		return;
		}

    	// ATTRB
    	if (paramName.equals("ATTRB")) {
    		for (String paramValue : al_paramValue) {
				switch (paramValue) {
				case "IC":
					eoMapItem.setBmsFlgIc(true);
					break;
				case "ASKIP":	
					eoMapItem.setBmsFlgAskip(true);
					break;
				case "PROT":
					eoMapItem.setBmsFlgProt(true);
					break;
				case "UNPROT":	
					eoMapItem.setBmsFlgUnprot(true);
					break;
				case "NORM":
					eoMapItem.setBmsFlgNorm(true);
					break;
				case "BRT":	
					eoMapItem.setBmsFlgBrt(true);
					break;
				case "DARK":
					eoMapItem.setBmsFlgDark(true);
					break;
				case "FSET":
					eoMapItem.setBmsFlgFset(true);
					break;
				default:
					break;
				}
			}
     		return;
		}

    	// INITIAL
    	if (paramName.equals("INITIAL")) {
    		str = al_paramValue.get(0);
     		str = str.replace("''", "'");                     // Eventuali doppi apici
    		eoMapItem.setBmsInitial(str);
     		return;
		}
    	
	}


	/**
	 * Restituisce le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @return the userExitInfoPgm
	 */
	public UserExitInfo getUserExitInfoPgm() {
		return userExitInfoPgm;
	}


	/**
	 * Imposta le informazioni di sistema/sottosistema<br>
	 * <p>
	 * @param userExitInfoPgm the userExitInfoPgm to set
	 */
	public void setUserExitInfoPgm(UserExitInfo userExitInfoPgm) {
		this.userExitInfoPgm = userExitInfoPgm;
	}

	
	/*
	 * ----------------------------------------------------------------------------------------------------------
	 * Restituisce il successsivo statement Assembler
	 * 
	 * DFHMSD
	 * DFHMDI
	 * DFHMDF
	 * END 
	 * ----------------------------------------------------------------------------------------------------------
	 */
	private String getNextAssemblerDirective() {
		
		String sourceInstr = "";  					// Stringa istruzuine completa istruzione senza // in righe successive
		String sourceRow = "";                      //
		boolean isContinuation = false;             //
		
		// Prima riga sorgente utile	 
		sourceRow = getNextRowNotComment();  		//  		
		this.curDFH = sourceRow.substring(9, 15);
		this.curRow = this.iCurRow;
		
		// Fine istruzioni o errore sorgente in input: restituisco oggetto istruzione null
		if (sourceRow.isBlank()) {      
			return sourceInstr;
		}
        
		// Impostazione continuazione
		if (sourceRow.length() >= 72 && sourceRow.charAt(71) != ' ') {
			isContinuation = true; 
			sourceRow = sourceRow.substring(0, 71);
		}
		
		sourceInstr = sourceRow;
		
		// Nessuna continuazione: istruzione completa
		if (!isContinuation) {
			return sourceInstr;
		}
		
		// Istruzione con continuazione, accodamento righe successive
		sourceRow = getNextRowNotComment();  		 
		while (sourceRow.length() > 15 && sourceRow.substring(9, 15).isBlank()) {
			if (sourceRow.length() >= 72) {
				sourceRow = sourceRow.substring(15, 71);
			} else {
				sourceRow = sourceRow.substring(15);
			}
			sourceInstr = sourceInstr + sourceRow;
			sourceRow = getNextRowNotComment();  
		}
		
		this.iCurRow--;                // Due next instruction in loop
		return sourceInstr;
	}

  
    /*
     * Extraction an populate Map key=parameter code val=arrayList values
     */
    private Map<String, ArrayList<String>> extractParamsAndValues(String sourceInstr) {
    	Map<String, ArrayList<String>> map_paramValue = null;
    	String str = "";
    	String parmName = "";    	
    	String parmValues = "";
    	int i = 0;
       	int j = 0;
       	int k = 0;
       	int l = 0;
    	
       	map_paramValue = new HashMap<>();
       	
    	str = sourceInstr.substring(16);
		
    	// Scan direttiva assembler 
    	for (i = 0; i < str.length(); i++) {
    		
    		// is a parameter assignment
			if (str.charAt(i) == '=') {
				parmName = "";
				
				// Extract parameter name
				for (j = i - 1; j >= 0; j--) {
					if (j == 0 || str.charAt(j) == ' ' || str.charAt(j) == ',') {
						if (j == 0) {j--;}
						parmName = str.substring(j + 1, i).trim();
						break;
					}
				}
				
				// Extract parameter value (cerco successivo = o fine stringa)
				parmValues = "";
				for (k = i + 1; k < str.length(); k++) {
					if (str.substring(k, k + 1).isBlank()) {continue;}
					
					// Valore fra apici (come INITIAL)
					if (str.substring(k, k + 1).equals("'")) {
						for (l = k + 1; l < str.length(); l++) {
							// Doppio apice dentro value
							if ((l + 2) <= str.length() && str.substring(l, l + 2).equals("''")) {
								l++;
								continue;
							}
							// Estrazione valore
							if (str.substring(l, l + 1).equals("'")) {
								parmValues = str.substring(k, l + 1).trim();
								map_paramValue.put(parmName, extractValues(parmValues));
								i = l;
								k = str.length();
								break;
							}
						} // end-for
					} else {
						// Next param
						if (str.charAt(k) == '=' || k == str.length() - 1) {
							
							// Fine stringa
							if (k == str.length() - 1) {
								parmValues = str.substring(i + 1, str.length()).trim();
								map_paramValue.put(parmName, extractValues(parmValues));
								i = k;
								break;
							}	
						    // Trovato = di assegnazione di successivo parametro							 					
							k = findCommaReverse(str, k - 1);
							parmValues = str.substring(i + 1, k).trim();
							map_paramValue.put(parmName, extractValues(parmValues));
							i = k;
							k = str.length();							 				
						} // end-if
					} // end-if
															
				} // end-for ricerca successivo
				
			} // end-if assegnazione parm
			
		} // end-for ricerca nuovo parm
    	
    	return map_paramValue;
	}

    /* Scan reverse and find comma */
    private int findCommaReverse(String str, int k) {
    	int i = 0;
 
    	// Bypass first name
		for ( i = k; i > 0; i--) {
			if (str.charAt(i) != ' ' && str.charAt(i) != ',') {
				continue;
			}
			break;
		}
    	
    	// Bypass spaces ahead
		for (; i > 0; i--) {
			if (str.charAt(i) == ' ' ) {
				continue;
			}
			
			// Comma or end previous name
			break;

		}
 
		// Correct parameter comma delimited
		if (str.charAt(i) == ',' ) {
			return i;
		}
		
		// Error in the source comma not found: accept space as delimiter
		
		return i + 1;
	}


	/* ----------------------------------------
     * Estract valori parametro, già trimmato
     * ----------------------------------------
     * 
     * Restituisce ArrayList di stringhe di valori
     * 
     * INITIAL='F'
     * INITIAL='Codice Societa'':'
     * ATTRB=(ASKIP,BRT,FSET)
     * POS=(1,1)
     * LENGTH=4
     */
	private ArrayList<String> extractValues(String parmValues) {
		ArrayList<String> al_value = new ArrayList<>();
		String[] ar_value = null;
		
		// Literal
		if (parmValues.startsWith("'")) {
			parmValues = parmValues.substring(1);
			parmValues = parmValues.substring(0, parmValues.length() - 1);
			parmValues = parmValues.replace("''", "'");
			al_value.add(parmValues);
			return al_value;
		}
		
		// Valore singolo
		if (!parmValues.startsWith("(")) {
			al_value.add(parmValues);
			return al_value;
		}
		
		// Valori fra parentesi con "," delimiter
		parmValues = parmValues.substring(1);
		parmValues = parmValues.substring(0, parmValues.length() - 1);
		ar_value = parmValues.split(",");
		for (String value : ar_value) {
			al_value.add(value);
		}
				
		return al_value;
	}


	/*
     * restituisce la prima riga non commento o space se fine source
     */
	private String getNextRowNotComment() {
		String row = "";
		
		for (this.iCurRow++; this.iCurRow < this.ar_RowsSourceInstr.length; this.iCurRow++) {
			row = this.ar_RowsSourceInstr[this.iCurRow];						
			
			if (row.isBlank()) {continue;}
			
			// Commento o PRINT NOGEN
			if (row.startsWith("*") 
			|| (row.length() >= 15 && row.substring(9, 14).equals("PRINT"))) {
				continue;
			}  	
		
			break;
		}

		return row;
	}


	/*
	 * ------------------------------------------------ 
	 * Operazioni iniziali sul BMS da analizzare
	 * ------------------------------------------------
	 */
	private void initialOperations(SourceInput si, String bmsSourceName) throws Exception {
		// Initial ora di inizio e reset errori
		this.di.curTimeMsStart = System.currentTimeMillis();
		this.di.curObjectWithErrors = false;
		
		// Descrittore sorgente e nome sorgente
		this.si = si;				
		this.sourceNameUnderParsing = bmsSourceName;
		this.di.curObjectId = bmsSourceName;
		this.di.curObjectType = EnumObject.OBJECT_CICS_BMS_SOURCE;	
		this.ar_RowsSourceInstr = this.si.getArrayRowSource();
 	}


	/*
	 * ------------------------------------------------
	 * Operazioni finali di analisi del bms source.
	 * ------------------------------------------------
	 * 
	 * Segnalazione errori e messaggio di chiusura
	 * Database update
	 * 
	 */
	private void finalOperations() throws Exception {
  
		// Errori di parsing  
  		if (isAnyInstructionErrorDetected) {
  			// Errori di parsing
  			logMessage(EnumMessageType.ERROR_INPUT, "ET0048" , this.sourceBmsName, rowStartSourceError+"", rowEndSourceError+"", this.sourceInstrError);
  		    putErrorOnDbStructure(EnumMessageType.ERROR_INPUT, "ET0048", this.sourceBmsName, rowStartSourceError, rowEndSourceError, this.sourceInstrError);
  			this.di.isExecutionWithErrors = true;
		}
  		
  	    // Aggiornamenti complessivi effettivi base dati
    	analyzerDbInfo.sqlDeleteGenericLevel();
        analyzerDbInfo.update(true, false, this.sourceBmsName, null);
		
		// Calcolo tempo di elapsed complessivo per il programma
		this.di.curTimeMsEnd = System.currentTimeMillis();
		this.di.curTimeMsElapsed = this.di.curTimeMsEnd - this.di.curTimeMsStart;
		Long timeElapsed = Long.valueOf(this.di.curTimeMsElapsed);
		
		// Messaggio di fine elaborazione
        if (isAnyInstructionErrorDetected) {
        	logMessage(EnumMessageType.INFORMATION, "MI0317", this.sourceNameUnderParsing, timeElapsed.toString());    
		} else {
			logMessage(EnumMessageType.INFORMATION, "MI0318", this.sourceNameUnderParsing, timeElapsed.toString()); 
		}
		 
 	}
    
	/*
	 * Inserimento errore per update finali su db
	 */
 	private void putErrorOnDbStructure(EnumMessageType msgType, String msgCode, String sourceBmsName, int rowStartSourceError, int rowEndSourceError, String sourceInstrError) {
 		EntityObjectAnalysisError oae = null;
 		String msg = "";
 		
	 	// Inserimento informazioni da istruzione
	 	oae = new EntityObjectAnalysisError();
	
	 	// Update primary key ai valori del programma sotto analisi
	 	oae.setSystem(this.di.systemInput);
	 	oae.setSubSystem(this.di.subSystemInput);
	 	oae.setIdObject(sourceBmsName);
	 	oae.setTypeObject(EnumObject.OBJECT_CICS_BMS_SOURCE);
	 	oae.setRowNum(rowStartSourceError);
	
	 	// Update informazioni correnti errore
	 	oae.setExcp(false);
	 	oae.setTypeProcessAnalysis(EnumTypeProcessAnalysis.PARSING_SOURCE);
	 	oae.setTypeInstr(EnumInstrDataCategory.NOT_ASSIGNED);
	 	oae.setNumInstr(0);
	 	oae.setSourceInstr(sourceInstrError);
	 	oae.setTokenError("");
	 	oae.setRowNumInstrBegin(rowStartSourceError);
	 	oae.setRowNumInstrFinal(rowEndSourceError);
	 	oae.setMsgType(msgType);
	 	oae.setMsgCode(msgCode);
	 	msg = getMessageAtualized(EnumModule.ANALYZER, msgType, msgCode,  msgCode, sourceBmsName, rowStartSourceError+"", rowEndSourceError+"", sourceInstrError);
	 	oae.setMsgText(msg);
	 	oae.setParsingError(true);
	 	oae.setSemanticError(false);
	 	oae.setWarning(false);
	 	oae.setCopyName("");
	
	 	// Inserimento in struttura db X update a fine elaborazione
	 	analyzerDbInfo.getObjsAnalysisError().add(oae);	
	 }


	/*
	 * ------------------------------------------------------------------
 	 * Inserimento statement Sql di delete relazioni e origine relazioni
 	 * ------------------------------------------------------------------
  	 */
	private void sqlInsDeleteRelationMap(String bmsSource, String map, String mapSet) {
		String strSql = "";
						
		// Delete preventivo campi mappa
		strSql = "DELETE FROM mapItem  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + map + "'" +
		         " AND typeObject =  " + EnumObject.OBJECT_CICS_MAP.ordinal() ;
		this.analyzerDbInfo.al_sqlDelete.add(strSql);

		// Delete preventivo origine CICS_MAP_MAPSET
		strSql = "DELETE FROM relationOrigin  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAP.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAP_MAPSET.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_MAPSET.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);

		// Delete preventivo CICS_MAP_MAPSET
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAP.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAP_MAPSET.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_MAPSET.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);
		
		// Delete preventivo origine CICS_MAP_BMS_SOURCE
		strSql = "DELETE FROM relationOrigin  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAP.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAP_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_BMS_SOURCE.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);
		
		// Delete preventivo CICS_MAP_BMS_SOURCE
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAP.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAP_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_BMS_SOURCE.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);

		// Delete preventivo origine CICS_MAPSET_BMS_SOURCE
		strSql = "DELETE FROM relationOrigin  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + mapSet +  "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAPSET.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAPSET_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_BMS_SOURCE.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);
		

		// Delete preventivo CICS_MAPSET_BMS_SOURCE
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + mapSet + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_MAP.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_MAPSET_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_MAPSET.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);

		// Delete preventivo origine CICS_FORM_BMS_SOURCE
		strSql = "DELETE FROM relationOrigin  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map+"-"+mapSet + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_FORM.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_FORM_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_BMS_SOURCE.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);
		
		// Delete preventivo CICS_FORM_BMS_SOURCE
		strSql = "DELETE FROM relation  WHERE " + 
		         "     sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + map+"-"+mapSet + "'" +
		         " AND typeObjectA =  " + EnumObject.OBJECT_CICS_FORM.ordinal() +
		         " AND relation =  " + EnumRelation.CICS_FORM_BMS_SOURCE.ordinal() +
		         " AND idObjectB = '" + mapSet + "'" +
		         " AND typeObjectB =  " + EnumObject.OBJECT_CICS_BMS_SOURCE.ordinal();
		this.analyzerDbInfo.al_sqlDelete.add(strSql);

	}

	
	/*
	 * Classe contenitore di servizio descrivente il campo di mappa.
	 *   
	 * In fase di analisi per ogni proc embedded incontrata si istanzia un oggetto di questa classe.
	 * 
	 */
	@SuppressWarnings("unused")
	private class InnerField {		
		String fieldName = "";									     // Nome procedura embedded
		int bmsNumGrp = 0;
		int bmsNumGrpOccurs = 0;
		boolean isFieldToDelete = false;
		Map<String, ArrayList<String> > map_paramValueDFHMDF = null; // Opzione|Valori
		
		public InnerField() {
			super();
			map_paramValueDFHMDF = new HashMap<>();
		}
		
	}	
 }

