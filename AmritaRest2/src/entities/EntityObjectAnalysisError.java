package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumCobolReservedWords;
import enums.EnumInstrDataCategory;
import enums.EnumMessageType;
import enums.EnumObject;
import enums.EnumSourceType;
import enums.EnumTypeProcessAnalysis;

/**
	 * Copyright (c) 2009-2011 e-Amrita - ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityObjectAnalysysError
	 * </h1>
	 *  <p>
	 * Questa classe descrive gli errori o i warning rilevati durante l'analisi di un oggetto.<br>
	 * Si tratta di informazioni rilevate al momento dell'analisi quali numero riga sorgente in errore,
	 * area programma attiva, ultima istruzione valida, stack exception in caso di abend. 
	 * <p>
	 * Queste informazioni rappresentano un'estensione di un oggetto analizzato e mantengono la<br>
	 * prima parte di chiave. <br>
	 * l'ultimo attributo della chiave è il numero di riga sotto analisi.<br>
	 * Se zero indica un errore post parsing, durante la valutazione delle metriche, delle violazioni<br>
	 * e di qualsiasi altra operazione post analisi.<br>
	 * <p>
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObjectAnalysisError 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityRelation
	 * @see EntityRelationOrigin
	 * @see EntityCopyEntityDefinition
	 * @see EntityWhereUsedItem
	 * @see EntityMapDescriptor
	 * @see EntityMapItem
	 * @see EntityTagValue
	 * @see EntityDynamicValueExt
	 * @see EntityScopeHeader
	 * @see EntityScopeSection
	 * @see EntityScopeItem
	 * @see EntityScopeChild
	 * @see EntityScopeProgram
	 * @see EntityScopeObject
	 * @see EntityScopeRelation
	 * @see EntityProcessLog
	 * @see EntityMetric
	 * @see EntityTableHeader    
	 * @see EntityTableStructure   
	 * @see EntityTableData 
	 * @see EntityDynamicField
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicValue
	 * @see EntityDynamicFieldSubSetting
*/

@DataBaseMappedTable("ObjectAnalysisError")
@DataBaseMappedColumns(java_db_col = {  
		                   // Colonne primary key
						    "system 			  		sys  	             PK"
						   ,"subSystem 			  		subSys  	         PK"    
						   ,"idObject 			  		idObject 	         PK"    
						   ,"typeObject 		  		typeObject 	         PK" 
						   ,"typeSource 		  		typeSource 	         PK"  
						   ,"rowNum                     rowNum               PK" 
						   // Colonne senza vincoli di chiave (No key)
						   ,"typeProcessAnalysis        typeProcessAnalysis  NK" 
						   ,"activeCobolDivision        activeCobolDivision  NK" 
						   ,"activeCobolSection         activeCobolSection   NK" 
						   ,"copyName                   copyName             NK"
						   ,"typeInstr                  typeInstr            NK" 
						   ,"numInstr                   numInstr             NK" 
						   ,"sourceInstr                sourceInstr          NK" 
						   ,"sourceInstrLastGood        sourceInstrLastGood  NK" 
						   ,"tokenError                 tokenError           NK" 
						   ,"rowNumInstrBegin           rowNumInstrBegin     NK"    
						   ,"rowNumInstrFinal           rowNumInstrFinal     NK"    
						   ,"msgType                    msgType              NK"   
						   ,"msgCode                    msgCode  	         NK"    
						   ,"msgText                    msgText              NK"   
						   ,"stackTrace                 stackTrace           NK"  
						   ,"excpDescription            excpDescription      NK"    
						   ,"excp                       excp                 NK"   
						   ,"excpWhenCopyAnalysis       excpWhenCopyAnalysis NK"   
						   ,"parsingError               parsingError   	     NK"    
						   ,"semanticError              semanticError        NK"   
						   ,"warning                    warning              NK"   
                       }
         )
@Entity(name="ObjectAnalysisError")
public class EntityObjectAnalysisError {

	///////////////////////////////////////////////////////////////////////
    // Data Items Object                                                 //                                                        //
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            						// OBJESYST(PK) Sistema applicativo
	@Id	
    @Column(name="subSys")
	private String subSystem = "";         						// OBJESUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          						// OBJEIDOB(PK) Nome oggetto (es. programma, copy,..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       				// OBJETYPO(PK) Tipologia oggetto (T0001)
	@Id
    @Column(name="typeSource")
	private EnumSourceType typeSource; 				            // OBJTTYPS     Tipologia sorgente (T0002)			
	@Id
    @Column(name="rowNum")
	private int rowNum = 0;       			    				// OBJENROW(PK) Numero riga sorgente in errore (in analisi)
	
	// Data 
    @Column(name="typeProcessAnalysis")
	private EnumTypeProcessAnalysis typeProcessAnalysis = null; // OBJEPROC     Tipo processo attivo al momento dell'errore (T0025)
    @Column(name="activeCobolDivision")
	private EnumCobolReservedWords activeCobolDivision = null; 	// OBJEDIVI     Divisione cobol attiva (T0029)	
    @Column(name="activeCobolSection")
	private EnumCobolReservedWords activeCobolSection = null; 	// OBJESECT     Section cobol attiva come Workink, Linkage, etc. (T0029)	
    @Column(name="typeInstr")
	private EnumInstrDataCategory typeInstr = null;             // OBJETYPI     Categoria istruzione in errore (T0018)
    @Column(name="copyName")
	private String copyName = "";       						// OBJECOPY     Copy in errore in analisi programma
    @Column(name="numInstr")
	private int numInstr = 0;       						    // OBJEISTN     Numero istruzione in struttura pgm/dati in errore
    @Column(name="sourceInstr")
	private String sourceInstr = "";       				        // OBJESRCE     Source istruzione errata
    @Column(name="sourceInstrLastGood")
	private String sourceInstrLastGood = "";       				// OBJISRCL     Source ultima istruzione analizzata senza errori
    @Column(name="tokenError")
	private String tokenError = "";       						// OBJETOKN     Token in errore
    @Column(name="rowNumInstrBegin")
	private int rowNumInstrBegin = 0;       					// OBJEROWB     Numero riga inizio istruzione
    @Column(name="rowNumInstrFinal")
	private int rowNumInstrFinal = 0;       					// OBJEROWF     Numero riga fine   istruzione
    @Column(name="msgType")
	private EnumMessageType msgType = null;						// OBJEMSGT     Tipo messaggio (T0024)
    @Column(name="msgCode")
	private String msgCode = "";                      			// OBJEMSGC     Codice messaggio di errore o informativo o di warning
    @Column(name="msgText")
	private String msgText = "";                      			// OBJEMSGX     Testo messaggio di errore o informativo o di warning
    @Column(name="stackTrace")
	private String stackTrace = "";                      		// OBJESTKT     Stack trace completo	
    @Column(name="excpDescription")
	private String excpDescription = "";                      	// OBJEEXCD     Descritione exception
    @Column(name="excpWhenCopyAnalysis")
	private boolean excpWhenCopyAnalysis = false;               // OBJIEXCC     True indica exception durante analisi modulo copy analizzato contestualmente al pgm
    @Column(name="excp")
	private boolean excp = false;                      		    // OBJEEXCF     True indica entry a fronte di exception
    @Column(name="parsingError")
	private boolean parsingError = false;                       // OBJEPARE     True indica parsing error su istruzione
    @Column(name="semanticError")
	private boolean semanticError = false;                      // OBJESEME     True indica semantic error su istruzione
    @Column(name="warning")
	private boolean warning = false;                    		// OBJEWRNG     True indica warning su istruzione
	
	
	// ** Info stato elaborazione se in exception **
	
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityObjectAnalysisError() {
		super();
		
		typeObject = EnumObject.NOT_ASSIGNED;	
		typeSource = EnumSourceType.NOT_ASSIGNED;
		typeProcessAnalysis=EnumTypeProcessAnalysis.NOT_ASSIGNED;
		activeCobolDivision=EnumCobolReservedWords.NOT_ASSIGNED;
		activeCobolSection=EnumCobolReservedWords.NOT_ASSIGNED;
		typeInstr=EnumInstrDataCategory.NOT_ASSIGNED;
		msgType=EnumMessageType.NOT_ASSIGNED;
	}
	
	
	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}
	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}
	/**
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}
	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}
	/**
	 * @return the idObject
	 */
	public String getIdObject() {
		return idObject;
	}
	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}
	/**
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}
	/**
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
		if (typeObject == EnumObject.OBJECT_COPY_COBOL_DATA ) {
			this.typeSource = EnumSourceType.COBOL_COPY_DATA;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_PROC) {
			this.typeSource = EnumSourceType.COBOL_COPY_PROC;
		} else if (typeObject == EnumObject.OBJECT_COPY_COBOL_ENV) {
			this.typeSource = EnumSourceType.COBOL_COPY_ENV;
		} else if (typeObject == EnumObject.OBJECT_PGM_COBOL) {
			this.typeSource = EnumSourceType.COBOL_PROGRAM;
		} else if (typeObject == EnumObject.OBJECT_CICS_BMS_SOURCE) {
			this.typeSource = EnumSourceType.CICS_BMS;
		} else if (typeObject == EnumObject.OBJECT_SQL_SCRIPT
				|| typeObject == EnumObject.OBJECT_ENTITY_SQL) {
			this.typeSource = EnumSourceType.SQL_SCRIPT;
		} else {
			this.typeSource = EnumSourceType.NOT_ASSIGNED;
		}	
	}
	
	/**
	 * @return the typeSource
	 */
	public EnumSourceType getTypeSource() {
		return typeSource;
	}
	/**
	 * @param typeSource the typeSource to set
	 */
	public void setTypeSource(EnumSourceType typeSource) {
		this.typeSource = typeSource;
	}
	

	/**
	 * @return the copyName
	 */
	public String getCopyName() {
		return copyName;
	}


	/**
	 * @param copyName the copyName to set
	 */
	public void setCopyName(String copyName) {
		this.copyName = copyName;
	}


	/**
	 * @return the numRow
	 */
	public int getRowNum() {
		return rowNum;
	}


	/**
	 * @param numRow the numRow to set
	 */
	public void setRowNum(int numRow) {
		this.rowNum = numRow;
	}



	/**
	 * @return the typeProcessAnalysis
	 */
	public EnumTypeProcessAnalysis getTypeProcessAnalysis() {
		return typeProcessAnalysis;
	}


	/**
	 * @param typeProcessAnalysis the typeProcessAnalysis to set
	 */
	public void setTypeProcessAnalysis(EnumTypeProcessAnalysis typeProcessAnalysis) {
		this.typeProcessAnalysis = typeProcessAnalysis;
	}


	/**
	 * @return the typeInstr
	 */
	public EnumInstrDataCategory getTypeInstr() {
		return typeInstr;
	}


	/**
	 * @param typeInstr the typeInstr to set
	 */
	public void setTypeInstr(EnumInstrDataCategory typeInstr) {
		this.typeInstr = typeInstr;
	}


	/**
	 * @return the activeCobolDivision
	 */
	public EnumCobolReservedWords getActiveCobolDivision() {
		return activeCobolDivision;
	}


	/**
	 * @param activeCobolDivision the activeCobolDivision to set
	 */
	public void setActiveCobolDivision(EnumCobolReservedWords activeCobolDivision) {
		this.activeCobolDivision = activeCobolDivision;
	}


	/**
	 * @return the activeCobolSection
	 */
	public EnumCobolReservedWords getActiveCobolSection() {
		return activeCobolSection;
	}


	/**
	 * @param activeCobolSection the activeCobolSection to set
	 */
	public void setActiveCobolSection(EnumCobolReservedWords activeCobolSection) {
		this.activeCobolSection = activeCobolSection;
	}


	/**
	 * @return the sourceInstrInError
	 */
	public String getSourceInstrInError() {
		return sourceInstr;
	}


	/**
	 * @param sourceInstrInError the sourceInstrInError to set
	 */
	public void setSourceInstr(String sourceInstrInError) {
		this.sourceInstr = sourceInstrInError;
	}



	/**
	 * @return the tokenError
	 */
	public String getTokenError() {
		return tokenError;
	}


	/**
	 * @param tokenError the tokenError to set
	 */
	public void setTokenError(String tokenError) {
		this.tokenError = tokenError;
	}


	/**
	 * @return the rowNumInstrBegin
	 */
	public int getRowNumInstrBegin() {
		return rowNumInstrBegin;
	}


	/**
	 * @param rowNumInstrBegin the rowNumInstrBegin to set
	 */
	public void setRowNumInstrBegin(int rowNumInstrBegin) {
		this.rowNumInstrBegin = rowNumInstrBegin;
	}


	/**
	 * @return the rowNumInstrFinal
	 */
	public int getRowNumInstrFinal() {
		return rowNumInstrFinal;
	}


	/**
	 * @param rowNumInstrFinal the rowNumInstrFinal to set
	 */
	public void setRowNumInstrFinal(int rowNumInstrFinal) {
		this.rowNumInstrFinal = rowNumInstrFinal;
	}


	/**
	 * @return the msgType
	 */
	public EnumMessageType getMsgType() {
		return msgType;
	}


	/**
	 * @param msgType the msgType to set
	 */
	public void setMsgType(EnumMessageType msgType) {
		this.msgType = msgType;
	}


	/**
	 * @return the msgCode
	 */
	public String getMsgCode() {
		return msgCode;
	}


	/**
	 * @param msgCode the msgCode to set
	 */
	public void setMsgCode(String msgCode) {
		this.msgCode = msgCode;
	}


	/**
	 * @return the msgText
	 */
	public String getMsgText() {
		return msgText;
	}


	/**
	 * @param msgText the msgText to set
	 */
	public void setMsgText(String msgText) {
		this.msgText = msgText;
	}


	/**
	 * @return the stackTrace
	 */
	public String getStackTrace() {
		return stackTrace;
	}


	/**
	 * @param stackTrace the stackTrace to set
	 */
	public void setStackTrace(String stackTrace) {
		this.stackTrace = stackTrace;
	}


	/**
	 * @return the excpDescription
	 */
	public String getExcpDescription() {
		return excpDescription;
	}


	/**
	 * @param excpDescription the excpDescription to set
	 */
	public void setExcpDescription(String excpDescription) {
		this.excpDescription = excpDescription;
	}


	/**
	 * @return the numInstr
	 */
	public int getNumInstr() {
		return numInstr;
	}


	/**
	 * @param numInstr the numInstr to set
	 */
	public void setNumInstr(int numInstr) {
		this.numInstr = numInstr;
	}


	/**
	 * @return the sourceInstr
	 */
	public String getSourceInstr() {
		return sourceInstr;
	}


	/**
	 * @return the sourceInstrLastGood
	 */
	public String getSourceInstrLastGood() {
		return sourceInstrLastGood;
	}


	/**
	 * @param sourceInstrLastGood the sourceInstrLastGood to set
	 */
	public void setSourceInstrLastGood(String sourceInstrLastGood) {
		this.sourceInstrLastGood = sourceInstrLastGood;
	}



	/**
	 * @return the excpWhenCopyAnalysis
	 */
	public boolean getExcpWhenCopyAnalysis() {
		return excpWhenCopyAnalysis;
	}


	/**
	 * @param excpWhenCopyAnalysis the excpWhenCopyAnalysis to set
	 */
	public void setExcpWhenCopyAnalysis(boolean excpWhenCopyAnalysis) {
		this.excpWhenCopyAnalysis = excpWhenCopyAnalysis;
	}


	/**
	 * @return the isExcp
	 */
	public boolean getExcp() {
		return excp;
	}


	/**
	 * @param isExcp the isExcp to set
	 */
	public void setExcp(boolean excp) {
		this.excp = excp;
	}


	/**
	 * @return the isParsingError
	 */
	public boolean getParsingError() {
		return parsingError;
	}


	/**
	 * @param isParsingError the isParsingError to set
	 */
	public void setParsingError(boolean parsingError) {
		this.parsingError = parsingError;
	}


	/**
	 * @return the isSemanticError
	 */
	public boolean getSemanticError() {
		return semanticError;
	}


	/**
	 * @param isSemanticError the isSemanticError to set
	 */
	public void setSemanticError(boolean semanticError) {
		this.semanticError = semanticError;
	}


	/**
	 * @return the isWarning
	 */
	public boolean getWarning() {
		return warning;
	}


	/**
	 * @param isWarning the isWarning to set
	 */
	public void setWarning(boolean warning) {
		this.warning = warning;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  typeObject.toString() + " IdObject:" + idObject  + " rowNum:" + rowNum;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityObjectAnalysisError objEntityObject = null;
		
		objEntityObject = (EntityObjectAnalysisError) obj;
		 
		return this.system.equals(objEntityObject.system)
		 &&    this.subSystem.equals(objEntityObject.subSystem)
		 &&    this.idObject.equals(objEntityObject.idObject)
		 &&    this.typeObject == objEntityObject.typeObject
		 &&    this.rowNum == objEntityObject.rowNum;
	}





}
