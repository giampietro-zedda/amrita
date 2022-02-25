
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
 * AnalyzerCicsPPT
 * </h1>
 * <p>
 * Questa classe gestisce l'analisi di un sorgente Cics PPT Completo. <br>
 * Non viene generato nessun oggetto Java serializzata ma vengono solo popolate le tabelle interne di gestione. <br>
 * <p>
 * 
 * Vengono inseriti gli oggetti <br>
 * 
 * <ul>
 *  <li><b>OBJECT_PGM_COBOL</b></li><br>
 *  Rappresenta il nome del programma cobol se PGMLANG=COBOL
 *  
 *  <li><b>OBJECT_PGM_GENERIC</b></li><br>
 *  Rappresenta il nome del programma se PGMLANG non valorizzato
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
 *  <li><b>LIBRARY_PGM</b></li><br>
 *  <li><b>LIBRARY_CICS_PPT</b></li><br>
 * </ul>
 *  
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 11/03/2021
*/
public class AnalyzerCicsPPT extends Analyzer implements AmritaConstants {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali per analisi sorgente  jcl                                                 
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
	
    // Gestore centralizzato aggiornamenti db cumulativi.
    private AnalyzerDbInfo analyzerDbInfo = null;
   
 	// Valori correnti generali
    private String sourceNameUnderParsing = "" ;    				// Nome BMS attualmente sotto analisi           	
	private String sourcePPTName = "";              				// Coincide con sourceNameUnderParsing
    private String curDFH = "";                     				// DFHxxx
    private int curRow = 0;                     		    	    // Numero riga corrente
    private int curRowDFHPPT = 0;                     		    	// Numero riga con DFHMDI
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
	public AnalyzerCicsPPT(UserConfiguration ucfg, ExecutionDirectives di, String sourcePPTName) {
		super(ucfg, di);
		 
	    // Inizializzazioni varie
		this.analyzerDbInfo = new AnalyzerDbInfo(ucfg, di, "");
		this.sourcePPTName = sourcePPTName;
	} 

 
	/**
	 * <h1>
	 * Analizza il sorgente del jcl memorizzato nell'istanza di SourceInput.<br>
	 * </h1>
	 * Nome jcl e istanza di SourceInput sono stati valorizzati dal costruttore di questa classe.<br>
	 * <p>
	 * @throws Exception 
	 */
	public void analyzeCicsPPT(SourceInput si, String pptSourceName) throws Exception {
		Map<String, ArrayList<String>> map_paramValueDFHPPT = null;	
		ArrayList<String> al_parm = null;
		String sys = "";
		String subSys = "";
		String sourceInstr = "";
		String libraryCode = "";
		String program = "";
		String pgmlang = "";
		
		initialOperations(si, pptSourceName);   			// Operazioni iniziali per oggetto bms da analizzare
		
		// Mapset DFHMSD
		sourceInstr = getNextAssemblerDirective();          
        
		// Deve essere DFHMSD TYPE=DSECT
		/*
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
			prepareForUpdateObjectBmsSourceStatus(pptSourceName);
			finalOperations();   
			return;
		}
	    */
		
		sourceInstr = getNextAssemblerDirective();   
		
		// Scan definizioni map
		while (!sourceInstr.isBlank()) {
						
			if (!this.curDFH.equals("DFHPPT")) {
				sourceInstr = getNextAssemblerDirective();   // -> this.curDFH
				continue;
			}
			
			this.curRowDFHPPT = this.curRow;
			map_paramValueDFHPPT = extractParamsAndValues(sourceInstr);     // Extract parameters and values and populate map
						
			al_parm = map_paramValueDFHPPT.get("TYPE");
			
			// Next definiton
			if (al_parm != null && al_parm.size() > 0 && !al_parm.get(0).equals("ENTRY")) {
				sourceInstr = getNextAssemblerDirective();   //  
				continue;
			}
			
			// DFHPPT TYPE=ENTRY,PROGRAM=PGMLN01,PMGLANG=COBOL
			al_parm = map_paramValueDFHPPT.get("PROGRAM");
			program = al_parm.get(0);
			al_parm = map_paramValueDFHPPT.get("PMGLANG");
			pgmlang = "";
			if (al_parm.size() > 0) {
				pgmlang = al_parm.get(0);
			}
			libraryCode = si.getLibraryCode();
// TODO			this.userExitGetSystemSubsystem(di, nameToEvaluate, sqlOwner, sqlEntityName);
// TODO			updateEntryPPTOnDb(sys, subSys, libraryCode, pptSourceName, program, pgmlang);

			sourceInstr = getNextAssemblerDirective();   // -> this.curDFH	
			
		} // end-while
		
		finalOperations();   
	}


	/*
	 * Inserimento oggetti e relazioni pgm
	 */
	private void updateEntryPPTOnDb(SourceInput si, String pptSource, Map<String, ArrayList<String>> map_paramValueDFHPPT) throws ExceptionAmrita, SQLException {
		
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
// TODO		sqlInsDeleteRelationMap(si.getLibraryCode(), pptSource);
		
		///////////////////////////////////////////////////////////////////////
		// Mappa, mapset, form
		///////////////////////////////////////////////////////////////////////
		
		eoBmsSource = prepareForUpdateObjectBmsSourceStatus(pptSource);
		
		// Inserimento object OBJECT_CICS_MAP
		eo = this.analyzerDbInfo.cloneObject(eoBmsSource);
		eo.setTypeObject(EnumObject.OBJECT_CICS_MAP);
// TODO		eo.setIdObject(map);
		eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);
		this.analyzerDbInfo.addObjEntity(eo);
		
	
		
		// Inserimento relazione CICS_MAP_MAPSET, 
   		er = new EntityRelation();
		er.setSystem(this.di.systemInput);   						 
		er.setSubSystem(this.di.subSystemInput); 
// TODO  		er.setIdObjectA(map);
		// TODO   		er.setIdObjectB(mapset);
   		er.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		er.setTypeObjectB(EnumObject.OBJECT_CICS_MAPSET);
   		er.setRelation(EnumRelation.CICS_MAP_MAPSET);
   		this.analyzerDbInfo.addObjEntityRelation(er);	
   		
   	    // Inserimento origine relazione CICS_MAP_MAPSET, 
   		ero = new EntityRelationOrigin();
   		ero.setSystem(this.di.systemInput);   						 
   		ero.setSubSystem(this.di.subSystemInput); 
   	// TODO   		ero.setIdObjectRelA(map);
   	// TODO  		ero.setIdObjectRelB(mapset);
   		ero.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		ero.setTypeObjectB(EnumObject.OBJECT_CICS_MAPSET);
  		ero.setRelation(EnumRelation.CICS_MAP_MAPSET);
  		ero.setNumInstrOrigin(0);
 		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
		ero.setIdObjectOrigin(pptSource);
		ero.setTypeObjectOrigin(EnumObject.OBJECT_CICS_BMS_SOURCE);
		ero.setRowStart(this.curRowDFHPPT);
		ero.setRowEnd(this.curRowDFHPPT);
		this.analyzerDbInfo.addObjEntityRelationOrigin(ero);
		
   		
   	    // Inserimento origine relazione CICS_MAP_BMS_SOURCE
   		ero = new EntityRelationOrigin();
   		ero.setSystem(this.di.systemInput);   						 
   		ero.setSubSystem(this.di.subSystemInput); 
   	// TODO   		ero.setIdObjectRelA(map);
   		ero.setIdObjectRelB(pptSource);
   		ero.setTypeObjectA(EnumObject.OBJECT_CICS_MAP);
   		ero.setTypeObjectB(EnumObject.OBJECT_CICS_BMS_SOURCE);
  		ero.setRelation(EnumRelation.CICS_MAP_BMS_SOURCE);
  		ero.setNumInstrOrigin(0);
 		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
		ero.setIdObjectOrigin(pptSource);
		ero.setTypeObjectOrigin(EnumObject.OBJECT_CICS_BMS_SOURCE);
		ero.setRowStart(this.curRowDFHPPT);
		ero.setRowEnd(this.curRowDFHPPT);
		this.analyzerDbInfo.addObjEntityRelationOrigin(ero);
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
	 * PRINT
	 * DFHXXX
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
     * Extraction anD populate Map key=parameter code val=arrayList values
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
	 * Operazioni iniziali sul PPT da analizzare
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
		this.di.curObjectType = EnumObject.OBJECT_CICS_PPT;	
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
  			logMessage(EnumMessageType.ERROR_INPUT, "ET0048" , this.sourcePPTName, rowStartSourceError+"", rowEndSourceError+"", this.sourceInstrError);
  		    putErrorOnDbStructure(EnumMessageType.ERROR_INPUT, "ET0048", this.sourcePPTName, rowStartSourceError, rowEndSourceError, this.sourceInstrError);
  			this.di.isExecutionWithErrors = true;
		}
  		
  	    // Aggiornamenti complessivi effettivi base dati
    	analyzerDbInfo.sqlDeleteGenericLevel();
        analyzerDbInfo.update(true, false, this.sourcePPTName, null);
		
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

