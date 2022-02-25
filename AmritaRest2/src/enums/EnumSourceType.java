package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumSourceType  
 * </h1>
 *  <p>
 * Questa enum elenca i possibili tipi di sorgenti analizzabili attraverso la classe Analyzer e
 * le sue figlie. <br>
 * Come parametro ogni elemento ha associata una enumerazione {@link EnumObject} per associare al tipo di
 * sorgente l'ggetto che viene scritto su database.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 15/02/2010
 * @see Analyzer
 * 
 */
@DataBaseMappedEnumeration
public enum EnumSourceType {

	 NOT_ASSIGNED,										// 00 
		
	 COBOL_COPY_PROC(EnumObject.OBJECT_COPY_COBOL_PROC),// 01 Sorgenti Copy Cobol Procedure Division
	 COBOL_COPY_DATA(EnumObject.OBJECT_COPY_COBOL_DATA),// 02 Sorgenti Copy Cobol Data Division
	 COBOL_COPY_ENV(EnumObject.OBJECT_COPY_COBOL_ENV),	// 03 Sorgenti Copy Cobol Environment Division
	 COBOL_COPY_ID(EnumObject.OBJECT_COPY_COBOL_ID),	// 04 Sorgenti Copy Cobol Identification Division
	 COBOL_PROGRAM(EnumObject.OBJECT_PGM_COBOL),  		// 05 Sorgenti programmi  Cobol
	 FREE_06,
	 FREE_07,
	 FREE_08,
	 FREE_09,
	 FREE_10,
	 SQL_SCRIPT(EnumObject.OBJECT_SQL_SCRIPT),        	// 11 06 Sorgenti definizioni DDL SQL
	 FREE_12,
	 FREE_13,
	 JCL_MVS_JOB(EnumObject.OBJECT_JCL_JOB),        	// 14 07 Sorgenti definizioni JCL JOB
	 JCL_MVS_PROC(EnumObject.OBJECT_JCL_PROC),  		// 15 08 Sorgenti definizioni JCL PROC
	 JCL_MVS_INCLUDE(EnumObject.OBJECT_JCL_INCLUDE),  	// 16 09 Sorgenti definizioni JCL INCLUDE
	 CICS_BMS(EnumObject.OBJECT_CICS_BMS_SOURCE),       // 17 70 Sorgenti definizioni BMS mappe
	 CICS_PCT(EnumObject.OBJECT_CICS_PCT),              // 18 75 Sorgenti definizioni PCT
	 CICS_PPT(EnumObject.OBJECT_CICS_PPT),              // 19 76 Sorgenti definizioni PPT
	 CICS_FCT(EnumObject.OBJECT_CICS_FCT),              // 20 77 Sorgenti definizioni FCT
	 CICS_TCT(EnumObject.OBJECT_CICS_TCT);              // 21 78 Sorgenti definizioni TCT
      
	 /////////////////////////////////////////////////////////////////////////////////////
	 //  Campi istanza enumerazione                                                     //
	 /////////////////////////////////////////////////////////////////////////////////////
	 
	 // Oggett a cui il sorgente si riferisce
	 EnumObject en_Object = EnumObject.NOT_ASSIGNED;
	 
	 /*
	 * Costruttore per enumerazioni senza informazioni
	 */
	 EnumSourceType() {
		 this.en_Object = EnumObject.NOT_ASSIGNED;
	 }               			

	 /*
	 * Costruttore per enumerazioni tipologie oggetto
	 */
	 EnumSourceType(EnumObject en_Object) {
		this.en_Object = en_Object;
	}               			
    
	public EnumObject getObjectType(){
		return this.en_Object;
	}
     
}
