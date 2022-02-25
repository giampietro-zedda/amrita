package analyzer;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import analyzer.AnalyzerCobol.InnerContextAnalysis;
import dao.IDAOCopyEntityDefinition;
import dao.IDAODynamicField;
import dao.IDAODynamicFieldSub;
import dao.IDAODynamicFieldSubSetting;
import dao.IDAODynamicFieldSubValue;
import dao.IDAODynamicFieldSubWaitExt;
import dao.IDAODynamicValueExt;
import dao.IDAOIndexItem;
import dao.IDAOMapDescriptor;
import dao.IDAOMapItem;
import dao.IDAOMetricValue;
import dao.IDAOMetricViolation;
import dao.IDAOObject;
import dao.IDAOObjectAnalysisError;
import dao.IDAOObjectAnalysisInfo;
import dao.IDAOObjectOption;
import dao.IDAORelation;
import dao.IDAORelationOrigin;
import dao.IDAOTagValue;
import dao.IDAOWhereUsedItem;
import dao.DAOImplCopyEntityDefinition;
import dao.DAOImplDynamicField;
import dao.DAOImplDynamicFieldSub;
import dao.DAOImplDynamicFieldSubSetting;
import dao.DAOImplDynamicFieldSubValue;
import dao.DAOImplDynamicFieldSubWaitExt;
import dao.DAOImplDynamicValueExt;
import dao.DAOImplIndexItem;
import dao.DAOImplMapDescriptor;
import dao.DAOImplMapItem;
import dao.DAOImplMetricValue;
import dao.DAOImplMetricViolation;
import dao.DAOImplObject;
import dao.DAOImplObjectAnalysisError;
import dao.DAOImplObjectAnalysisInfo;
import dao.DAOImplObjectOption;
import dao.DAOImplRelation;
import dao.DAOImplRelationOrigin;
import dao.DAOImplSqlGeneric;
import dao.DAOImplTagValue;
import dao.DAOImplWhereUsedItem;
import utilities.DateTimeService;
import entities.EntityCopyEntityDefinition;
import entities.EntityDynamicField;
import entities.EntityDynamicFieldSub;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import entities.EntityDynamicValueExt;
import entities.EntityDynamicFieldSubWaitExt;
import entities.EntityIndexItem;
import entities.EntityMapDescriptor;
import entities.EntityMapItem;
import entities.EntityMetricValue;
//import entities.EntityMetricPgm;
import entities.EntityMetricViolation;
import entities.EntityObject;
import entities.EntityObjectAnalysisError;
import entities.EntityObjectAnalysisInfo;
import entities.EntityObjectOption;
import entities.EntityRelation;
import entities.EntityRelationOrigin;
import entities.EntityTagValue;
import entities.EntityWhereUsedItem;
import enums.EnumCobolReservedWords;
import enums.EnumDataItemType;
import enums.EnumIndexOrder;
import enums.EnumInstrDataCategory;
import enums.EnumLanguageItemInstr;
import enums.EnumMetricsScope;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumPrecompilerReservedWords;
import enums.EnumRelation;
import enums.EnumRelationSourceProcess;
import enums.EnumRelationType;
import enums.EnumSourceType;
import enums.EnumUserExit;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * AnalyzerDbInfo
 * </h1>
 * <p>
 * Vengono modellate tutte le informazioni relative all'aggiornamento del data base
 * s fronte dell'analisi di un programma.<br>
 * In particolare sono codificate, pronte per l'inserimento su db, tutte le strutture relative
 * alle entity di Object, ObjectRelation, ObjectOrigin etc.<vr>
 * In pratica questa rappresenta una classe contenitore con gli aggiornamenti al db da effettuare
 * in soluzione unica.
 * <p>
 *  <p>
  * Questa classe  descrive le strutture del programma che rispecchiano le informazioni memorizzate
  * su db, a fronte dell'analisi preliminare del sorgente del programma.<br>
  * Queste strutture vengono serializzate insieme al programma e permettono di conoscere, senza
  * considerare il database, oggetti, relazioni, where-used, istruzioni dinamiche etc. del programma.<br>
  * Particolare interesse rivestono le strutture di definizione e di relativi where-used,
  * dei tracciati dei moduli copy e dei files, di cui si conosce solo il nome esterno (DD Name).<br>
  * In questo caso l'associazione fra il file utilizzato dal programma eil file effettivo, con relativi
  * where used, può avvenire solo durante l'associazione del nome del file interno al programma con il 
  * file effettivo, per esempio nell'analisi del jcl.

 *
 * @author Giampietro Zeddacreate
 * @version 1.0.0
 * @since 01/giu/2010 
 * @see AnalyzerDbUpdates
 * @see AnalyzerCobolProgram
 *
*/

public class AnalyzerDbInfo extends ExecutionShared {

	//////////////////////////////////////////////////////////////////////////////////// 
    // Variabili di istanza di reference a servizi centralizzati e a oggetti condivisi             
	//////////////////////////////////////////////////////////////////////////////////// 
    
    
	/////////////////////////////////////////////////////////////////////////////
	// Strutture per aggiornamento finale data base caricate durante l'analisi
    /////////////////////////////////////////////////////////////////////////////
	
	// Programma sorgente da relazionare con
	public String pgmActive = "";                                                       // Programma corrente sotto analisi

    // Statement di Sql delete  
    public ArrayList<String> al_sqlDelete = null;					      		        // Sql DELETE da eseguire prima degli inserimenti

    // Statement di Sql update  
    public ArrayList<String> al_sqlUpdate = null;					      		        // Sql UPDATE da eseguire dopo gli inserimenti

    // Object info e object errors
    public EntityObjectAnalysisInfo dbObjectAnalysisInfo = null;						// Informazioni su analisi oggetto processato
    public ArrayList<EntityObjectAnalysisError> al_DbObjectAnalysisError = null;		// Informazioni su errori analisi oggetto
    
    // Oggetti da inserire 
    public ArrayList<EntityObject> al_DbObject = null;					      		    // Object da inserire su db
    
    // Opzioni, relazioni, where used .. relaziona al programma o agli oggetti da inserire
	public ArrayList<EntityObjectOption> al_DbObjectOption = null;					    // ObjectOption da inserire su db
	public ArrayList<EntityRelation> al_DbRelation = null;				      		    // Relation da inserire su db
	public ArrayList<EntityRelationOrigin> al_DbRelationOrigin = null;	      		    // RelationOrigin da inserire su db
	public ArrayList<EntityCopyEntityDefinition> al_DbCopyEntityDefinition = null;      // CopyEntityDefinition da inserire su db
	public ArrayList<EntityIndexItem> al_DbIndexItem = null;                            // IndexItem da inserire su db
	public ArrayList<EntityWhereUsedItem> al_DbWhereUsed = null;				      	// WhereUsed da inserire su db
	public ArrayList<EntityTagValue> al_DbTagValue = null;   					   	    // TagValue da inserire su db
	public ArrayList<EntityMetricValue> al_DbMetric = null;   					   	    // Metric da inserire su db
	public ArrayList<EntityMetricViolation> al_DbMetricViolation = null;   				// Metric violations da inserire su db
	public ArrayList<EntityMapDescriptor> al_DbMapDescriptor = null;   	                // MapDescriptor da inserire su db
	public ArrayList<EntityMapItem> al_DbMapItem = null;   	                            // MapItem da inserire su db
	public ArrayList<EntityDynamicField> al_DbDynamicField = null;			            // DynamicField da inserire su db
	public ArrayList<EntityDynamicFieldSub> al_DbDynamicFieldSub = null;			    // DynamicFieldSub da inserire su db
	public ArrayList<EntityDynamicFieldSubSetting> al_DbDynamicFieldSubSetting = null;	// DynamicFieldSubSetting da inserire su db
	public ArrayList<EntityDynamicFieldSubValue> al_DbDynamicFieldSubValue = null;	    // DynamicFieldSubValue da inserire su db
	public ArrayList<EntityDynamicFieldSubWaitExt> al_DbDynamicFieldSubWaitExt = null;  // EntityDynamicFieldSubWaitExt da inserire su db
	public ArrayList<EntityDynamicValueExt> al_DbDynamicValueExt = null;   	            // DynamicValueExt da inserire su db
    
	
	/**
	 * 
	 * Costruttore
	 * 
	 */
	public AnalyzerDbInfo(UserConfiguration ucfg, ExecutionDirectives di, String pgmActive) {
		super(ucfg, di);
		
		// Programma corrente sotto analisi
		this.pgmActive = pgmActive;
		
		// Array list entities per inserimento su data base a fine elaborazione
	    this.dbObjectAnalysisInfo = new EntityObjectAnalysisInfo ();						 
	    this.al_DbObjectAnalysisError = new ArrayList<EntityObjectAnalysisError> ();		 
		this.al_DbObject = new ArrayList<EntityObject> ();	
		this.al_sqlDelete = new ArrayList<String> ();
		this.al_sqlUpdate = new ArrayList<String> ();
	    this.al_DbObjectOption = new ArrayList<EntityObjectOption> ();					   
	    this.al_DbRelation = new ArrayList<EntityRelation> ();				    
	    this.al_DbRelationOrigin = new ArrayList<EntityRelationOrigin> ();				    
	    this.al_DbCopyEntityDefinition = new ArrayList<EntityCopyEntityDefinition> ();    
	    this.al_DbIndexItem = new ArrayList<EntityIndexItem> ();    
	    this.al_DbWhereUsed = new ArrayList<EntityWhereUsedItem> ();				    
	    this.al_DbTagValue = new ArrayList<EntityTagValue> ();  
	    this.al_DbMetric = new ArrayList<EntityMetricValue> ();  
 	    this.al_DbMetricViolation = new ArrayList<EntityMetricViolation> ();  
	    this.al_DbDynamicField = new ArrayList<EntityDynamicField> ();	
	    this.al_DbDynamicFieldSub = new ArrayList<EntityDynamicFieldSub> ();	
	    this.al_DbDynamicFieldSubSetting = new ArrayList<EntityDynamicFieldSubSetting> ();	
	    this.al_DbDynamicFieldSubValue = new ArrayList<EntityDynamicFieldSubValue> ();
	    this.al_DbDynamicFieldSubWaitExt = new ArrayList<EntityDynamicFieldSubWaitExt> ();				    
	    this.al_DbDynamicValueExt = new ArrayList<EntityDynamicValueExt> ();	
		this.al_DbMapDescriptor = new ArrayList<EntityMapDescriptor>();
		this.al_DbMapItem = new ArrayList<EntityMapItem>();
	}

	
	/**
     * 
     * Inserimento in strutture per update db a fine analisi, per oggetto relazionato ad altro oggetto. <br>
     * <p>
     * Viene inserito l'oggetto relazionato, la relazione e l'origine della relazione.
     * Se il numero di definizione è minore di 0, lorigine della relazione non viene inserita.
     * L'origine è gestita solo per la data division e la procedure division, istruzioni
     * select in environment division non devono avere una origine relazione codificata.
     * Se l'oggetto relazionato implica un programma batch viene inserita l'opzione di programma.
     * Se la relazione è di aggiornamento viene inserita l'opzione di programma.
     * Metodo utilizzato per Read, Write, Rewrite e Delete, Call, Cancel.
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
     * 
     */
	public void prepareForDbObjectRelatedToAny(EnumObject typeObject								// Tipo  oggetto da relazionare con (di solito è il programma corrente)
											 , String idObject 										// Id    oggetto da relazionare con
											 , EnumObject typeObjectRelated							// Tipo  oggetto relazionato
											 , String idObjectRelated								// Id    oggetto relazionato
											 , EnumObjectStatus objectStatus						// Stato oggetto relazionato
											 , EnumRelation typeRelation							// Tipo relazione					
											 , EnumRelationSourceProcess relationSourceProcess		// Processo origine relazione
											 , String programName									// Programma sotto analisi
											 , int numInstrOrDef									// Numero istruzione o definizione
											 , int rowStart                                         // Riga inizio istruzione
											 , int rowEnd                                           // Riga fine istruzione
											 , String copyOrigin                                    // Copy dove la relazione è originata
											 , int rowStartInCopy  									// Riga inizio in copy
											 , int rowEndInCopy 									// Riga fine in copy
											 , EnumInstrDataCategory instrCategory                  // Categoria istruzione
											 , EnumCobolReservedWords activeDivisionArea            // Divisione attiva
											 , boolean ioSequential									// Per inserimento opzione
											 , boolean ioVsam										// Per inserimento opzione
											 , boolean isDynamic							        // Per inserimento dynamic code option
											 , UserExitInfo useExitInfo
										  	   ) {

		 
		EntityObjectOption eoo = null; 
		EntityObject eo = null; 
		EntityRelation er = null;
		EntityRelationOrigin ero = null;
 
		
		
		// Inserimento oggetto da relazionare con (se non è il programma sotto analisi)
		if (typeObject != EnumObject.OBJECT_PGM_COBOL
		&& !idObject.equals(programName)) {
			eo = new EntityObject();
	        completeInfoObject(eo);
	        eo.setSystem(useExitInfo.getSystem());
	        eo.setSubSystem(useExitInfo.getSubSystem());
	        eo.setSystemOwner(useExitInfo.getSystemOwner());
	        eo.setSubSystemOwner(useExitInfo.getSubSystemOwner());
			eo.setIdObject(idObject);
			eo.setTypeObject(typeObject);
			eo.setLibrarySourceObject("");   	 
			eo.setLibrarySource("");         		 
			eo.setFileSource("");            	 
			eo.setTypeSource(EnumSourceType.NOT_ASSIGNED);       
	        addObjEntity(eo);
		}
				
		// Inserimento relazione fra oggetto da relazionare e oggetto relazionato per inserimento finale
   		er = new EntityRelation();
        er.setSystem(useExitInfo.getSystem());
        er.setSubSystem(useExitInfo.getSubSystem());
   		er.setIdObjectA(idObject);
   		er.setIdObjectB(idObjectRelated);
   		er.setTypeObjectA(typeObject);
   		er.setTypeObjectB(typeObjectRelated);
   		er.setRelation(typeRelation);
   		addObjEntityRelation(er);

		// Programma assente, si stava analizzando un modulo copy
		if (programName.isEmpty()) {
			return;
		}
				  		  	
   		// Numero definizione di origine < 0: origine relazione da non inserire
   		if (numInstrOrDef >= 0) {
   	 		// Inserimento origine relazione nel programma
   	   		ero = new EntityRelationOrigin();
   	        ero.setSystem(useExitInfo.getSystem());
   	        ero.setSubSystem(useExitInfo.getSubSystem());
   	   		ero.setIdObjectRelA(idObject);
   	   		ero.setIdObjectRelB(idObjectRelated);
   	   		ero.setTypeObjectA(typeObject);
   	   		ero.setTypeObjectB(typeObjectRelated);
   	   		ero.setRelation(typeRelation);
   	   		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
   	   		ero.setNumInstrOrigin(numInstrOrDef);
   	   		ero.setRowStart(rowStart);
   	   		ero.setRowEnd(rowEnd);
   	   		if (rowEnd == 0) {
   	   			ero.setRowEnd(rowStart);
   	   		}	   		
   	   		ero.setCopyOrigin(copyOrigin);
   	   		ero.setRowStartInCopy(rowStartInCopy);
   	   		ero.setRowEndInCopy(rowEndInCopy);
   	   		if (rowEndInCopy == 0) {
   	   			ero.setRowEndInCopy(rowStartInCopy);
   	   		}	   		
  	   		ero.setInstrCategory(instrCategory);
  	   		ero.setInstrProgramArea(activeDivisionArea);
  	   		ero.setIdObjectOrigin(programName);
   	   		ero.setTypeObjectOrigin(EnumObject.OBJECT_PGM_COBOL);
   	   		ero.setRelationSource(relationSourceProcess);
   	   		ero.setInstrLang(EnumLanguageItemInstr.ITEM_COBOL);
   	   		ero.setInstrTypePrecompiler(EnumPrecompilerReservedWords.NOT_ASSIGNED);
   	   		addObjEntityRelationOrigin(ero);
		}
   		
  		// Inserimento opzione PGM_WITH_COPY
 		if (typeObject == EnumObject.OBJECT_PGM_COBOL 
 		&& (typeObjectRelated == EnumObject.OBJECT_COPY_COBOL_PROC || typeObjectRelated == EnumObject.OBJECT_COPY_COBOL_DATA)) {
 			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(typeObject);
   			eoo.setOption(EnumObjectOption.PGM_WITH_COPY);
   			addObjEntityOption(eoo);
 		}
   		
  		// Inserimento opzione PGM_WITH_COPY_UNUSED
 		if (typeRelation == EnumRelation.PGM_COPY_UNUSED) {
 			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(typeObject);
   			eoo.setOption(EnumObjectOption.PGM_WITH_COPY_UNUSED);
   			addObjEntityOption(eoo);
 		}

 		// Inserimento opzione PGM_WITH_DYNAMIC_CODE
 		if (typeObject == EnumObject.OBJECT_PGM_COBOL && isDynamic) {
  			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
  			eoo.setIdObject(idObject);
  			eoo.setTypeObject(typeObject);
  			eoo.setOption(EnumObjectOption.PGM_WITH_DYNAMIC_CODE);
  			addObjEntityOption(eoo);
		}
   		
 		// Inserimento opzione PGM_BATCH
 		if (typeObject == EnumObject.OBJECT_PGM_COBOL 
  		&&	typeObjectRelated == EnumObject.OBJECT_INTERNAL_FILE) {
  			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
  			eoo.setIdObject(idObject);
  			eoo.setTypeObject(typeObject);
  			eoo.setOption(EnumObjectOption.PGM_BATCH);
  			addObjEntityOption(eoo);
		}
   		
   		// Inserimento opzione PGM_WITH_IO
 		if (typeObject == EnumObject.OBJECT_PGM_COBOL) {
	   		if (typeObjectRelated == EnumObject.OBJECT_INTERNAL_FILE 
	  		||  typeObjectRelated == EnumObject.OBJECT_EXTERNAL_FILE
	  		||  typeObjectRelated == EnumObject.OBJECT_ENTITY_SQL
	  		    ) {
	   			eoo = new EntityObjectOption();
	 	        eoo.setSystem(useExitInfo.getSystem());
	 	        eoo.setSubSystem(useExitInfo.getSubSystem());
	  			eoo.setIdObject(idObject);
	   			eoo.setTypeObject(typeObject);
	   			eoo.setOption(EnumObjectOption.PGM_WITH_I_O);
	   			addObjEntityOption(eoo);
	   			// Inserimento opzione PGM_UPDATE
	   	  		if (typeRelation == EnumRelation.PGM_INTERNAL_FILE_UPDATE
	  	  		||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_DELETE
	  	  	    ||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_INSERT
	  	  	    ||  typeRelation == EnumRelation.PGM_EXTERNAL_FILE_UPDATE
	  	  	    ||  typeRelation == EnumRelation.PGM_EXTERNAL_FILE_DELETE
	  	  	    ||  typeRelation == EnumRelation.PGM_EXTERNAL_FILE_INSERT
	  	  	    ||  typeRelation == EnumRelation.PGM_ENTITY_UPDATE
	  	  	    ||  typeRelation == EnumRelation.PGM_ENTITY_DELETE
	  	  	    ||  typeRelation == EnumRelation.PGM_ENTITY_INSERT
	  	  	    ||  typeRelation == EnumRelation.PGM_ENTITY_INSERT
	    	  	  		    ) {
	   	  	   			eoo = new EntityObjectOption();
	   	  	   			eoo.setSystem(useExitInfo.getSystem());
	   	  	   			eoo.setSubSystem(useExitInfo.getSubSystem());
	   	  	   			eoo.setIdObject(idObject);
	   	  	   			eoo.setTypeObject(typeObject);
	   	  	   			eoo.setOption(EnumObjectOption.PGM_UPDATE);
	   	  	   			addObjEntityOption(eoo);
	     	  	}
			}
		}
	    
 		
  		// Inserimento opzione oggetto relazionato INTERNAL_FILE_VSAM
  		if (typeRelation == EnumRelation.PGM_INTERNAL_FILE_UPDATE
  		||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_DELETE
 	    ||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_READ
 	    ||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_READNEXT
 	    ||  typeRelation == EnumRelation.PGM_INTERNAL_FILE_READPREV
   	    	  	  		    ) {
   			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(typeObject);
   			eoo.setOption(EnumObjectOption.INTERNAL_FILE_VSAM);
   			addObjEntityOption(eoo);
 		}

   		
   		// Inserimento opzione PGM_WITH_I_O_SEQ
 		if (ioSequential) {
   			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(EnumObject.OBJECT_PGM_COBOL);
   			eoo.setOption(EnumObjectOption.PGM_WITH_I_O_SEQ);
   			addObjEntityOption(eoo);
		}
 		
   		// Inserimento opzione PGM_WITH_IO_VSAM
 		if (ioVsam) {
 			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(typeObject);
   			eoo.setOption(EnumObjectOption.PGM_WITH_I_O_VSAM);
   			addObjEntityOption(eoo);
		}

 		// Inserimento opzione COPY_INCLUDES_COPY e COPY_INCLUDED_IN_COPY
 		if (typeRelation == EnumRelation.COPY_COPY) {
			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObject);
   			eoo.setTypeObject(typeObject);
   			eoo.setOption(EnumObjectOption.COPY_INCLUDES_COPY);
   			addObjEntityOption(eoo);
   			
 			eoo = new EntityObjectOption();
 	        eoo.setSystem(useExitInfo.getSystem());
 	        eoo.setSubSystem(useExitInfo.getSubSystem());
   			eoo.setIdObject(idObjectRelated);
   			eoo.setTypeObject(typeObjectRelated);
   			eoo.setOption(EnumObjectOption.COPY_INCLUDED_IN_COPY);
   			addObjEntityOption(eoo);
 		}
 		
	}

	/**
     * 
     * Inserisce un oggetto. <br>
     * <p>
     * I dati particolari dell'oggetto sono valorizzati dal chiamante.
	 * @param userExitInfoPgm 
     * 
     */
	public EntityObject prepareForDbObject(
						   EnumObject typeObject				// Tipo  oggetto da relazionare con (di solito è il jcl source corrente)
						 , String idObject 						// Id    oggetto da relazionare con
						 , EnumObjectStatus objectStatus	    // Stato oggetto relazionato
						 , UserExitInfo userExitInfoPgm			// Info per sistema/sottosistema 
					  	   ) {

		 
		EntityObject eo = null;
		
		// Oggetto
		eo = new EntityObject();
        completeInfoObject(eo);
        eo.setSystem(userExitInfoPgm.getSystem());
        eo.setSubSystem(userExitInfoPgm.getSubSystem());
		eo.setIdObject(idObject);
		eo.setTypeObject(typeObject);
		eo.setStatus(objectStatus);   
        addObjEntity(eo);
		
		return eo;
 	}
 
	/**
     * 
     * Inserisce una origine di relazione fra due oggetti  <br>
     * <p>
     * I dati particolari dell'origine sono valorizzati dal chiamante.
	 * @param userExitInfoPgm 
     */
	public EntityRelationOrigin prepareForDbObjectRelationOrigin(
			               EnumRelation typeRelation			// Tipo relazione					
						 , EnumObject typeObjectToRelateWith	// Tipo  oggetto da relazionare con  
						 , String idObjectToRelateWith 			// Id    oggetto da relazionare con
						 , EnumObject typeObjectRelated			// Tipo  oggetto relazionato
						 , String idObjectRelated				// Id    oggetto relazionato
						 , Instruction instrOrigin              // Istruzione origine
						 , EnumObject typeObjectOrigin			// Tipo  oggetto origine della relazione (Pgm, Script etc.)
						 , String idObjectOrigin				// Id    oggetto origine
						 , EnumCobolReservedWords programArea   // PROC_DIVISION, DATA_DIVISION..
						 , UserExitInfo userExitInfoPgm			// Info per sistema/sottosistema
						 , Instruction instruction              // Istruzione Sql
					  	   ) {

		 
		EntityRelationOrigin ero = null;
		
   		// Origine relazione nel jcl da inserire solo se numero definizione di origine > 0
	   	ero = new EntityRelationOrigin();
   		ero.setSystem(userExitInfoPgm.getSystem());   						 
   		ero.setSubSystem(userExitInfoPgm.getSubSystem()); 
   		ero.setIdObjectRelA(idObjectToRelateWith);
   		ero.setIdObjectRelB(idObjectRelated);
   		ero.setTypeObjectA(typeObjectToRelateWith);
   		ero.setTypeObjectB(typeObjectRelated);
   		ero.setRelation(typeRelation);
   		ero.setRelationType(EnumRelationType.RELATION_DIRECT);
  		ero.setNumInstrOrigin(instrOrigin.getNumInstr());
  		ero.setRowStart(instruction.getRowStartSource());
  		ero.setRowEnd(instruction.getRowEndSource());
 		ero.setTypeObjectOrigin(typeObjectOrigin);
 		ero.setIdObjectOrigin(idObjectOrigin);
 		ero.setInstrCategory(instrOrigin.getTypePrecompiler());
   		ero.setInstrTypePrecompiler(instrOrigin.getTypeInstrPrecompiler());
   		ero.setInstrProgramArea(programArea);
   		ero.setRelationSource(EnumRelationSourceProcess.SOURCE_STATIC_LOCAL);
   		this.addObjEntityRelationOrigin(ero);
   		
   		return ero;
	}

	
	/**
     * 
     * Inserisce una relazione fra due oggetti <br>
     * <p>
	 * @param userExitInfoPgm 
     * 
     */
	public EntityRelation prepareForDbObjectRelation(
			   			   EnumRelation typeRelation			// Tipo relazione					
						 , EnumObject typeObjectToRelateWith	// Tipo  oggetto da relazionare con  
						 , String idObjectToRelateWith 			// Id    oggetto da relazionare con
						 , EnumObject typeObjectRelated			// Tipo  oggetto relazionato
						 , String idObjectRelated				// Id    oggetto relazionato
						 , UserExitInfo userExitInfoPgm			// Info sistema/sosttosistema
					  	   ) {

		 
		EntityRelation er = null;
		
		
		// Inserimento relazione fra oggetto da relazionare e oggetto relazionato per inserimento finale
   		er = new EntityRelation();
		er.setSystem(userExitInfoPgm.getSystem());   						 
		er.setSubSystem(userExitInfoPgm.getSubSystem()); 
  		er.setIdObjectA(idObjectToRelateWith);
   		er.setIdObjectB(idObjectRelated);
   		er.setTypeObjectA(typeObjectToRelateWith);
   		er.setTypeObjectB(typeObjectRelated);
   		er.setRelation(typeRelation);
   		addObjEntityRelation(er);

    	return er;	
	}

	/**
     * 
     * Inserisce un elemento di un tracciato record di un copy o una colonna di un entity <br>
     * <p>
     * 
     */
	public EntityCopyEntityDefinition prepareForDbCopyEntityDefinition(EnumObject typeObject				// Tipo  oggetto ENTITY o COPY
																	 , String idObject 			            // Id    oggetto  
																	 , int numSeq							// Numero sequenza
																	 , String idFieldColumn                 // Nome campo o colonna
																	 , EnumLanguageItemInstr itemLang       // Linguaggio item (Cobol, Sql, ..) 
																	 , EnumDataItemType itemType            // Tipologia specifica item
																  	   ) {

		 
		EntityCopyEntityDefinition eced = null;
		
		
		// Inserimento colonna o elemento tracciato
		eced = new EntityCopyEntityDefinition();
		eced.setSystem(di.systemInput);   						 
		eced.setSubSystem(di.subSystemInput); 
		eced.setTypeObject(typeObject);
		eced.setIdObject(idObject);
		
		eced.setIdField(idFieldColumn);
		eced.setItemLang(itemLang);
		eced.setItemType(itemType);
		
   		this.addObjEntityCopyDefinition(eced);

    	return eced;	
	}


	/**
     * 
     * Inserisce una colonna di un indice. <br>
     * <p>
     * 
     */
	public EntityIndexItem prepareForDbIndexItem(EnumObject typeObject				// Tipo  oggetto INDEX
											   , String idObject 			        // Id    oggetto  
											   , EnumObject typeObjectOwner		    // Tipo  oggetto proprietario (ENTITY_SQL, ... )
											   , String idObjectOwner 			    // Id    oggetto proprietario
											   , String idColumnName                // Nome colonna indice
											   , EnumIndexOrder order               // Tipo ordinamento colonna indice
											   , int numSeq							// Numero sequenza colonna
										  	    ) {

		 
		EntityIndexItem eii = null;
		
		
		// Inserimento colonna o elemento tracciato
		eii = new EntityIndexItem ();
		eii.setSystem(di.systemInput);   						 
		eii.setSubSystem(di.subSystemInput); 
		eii.setTypeObject(typeObject);
		eii.setIdObject(idObject);
		eii.setTypeObjectOwner(typeObjectOwner);
		eii.setIdObjectOwner(idObjectOwner);
		
		eii.setIdColumnName(idColumnName);
		eii.setOrderType(order);
		
   		this.addObjEntityIndexItem(eii);

    	return eii;	
	}




	/**
     * 
     * Inserisce una opzione di un oggetto. <br>
     * <p>
     * 
     */
	public EntityObjectOption prepareForDbObjectOption(
								      String idObject					// Nome oggetto
								    , EnumObject typeObject				// Tipo oggetto
								    , EnumObjectOption objectOption     // Opzione
					  	            ) {

		 
		EntityObjectOption eoo = null;
		
   		// Allocazione e inserimento opzione
    	eoo = new EntityObjectOption();
 	  	eoo.setSystem(di.systemInput);   						 
 	   	eoo.setSubSystem(di.subSystemInput); 
   		eoo.setIdObject(idObject);
   		eoo.setTypeObject(typeObject);
   		eoo.setOption(objectOption);
   		addObjEntityOption(eoo);
   		
   		return eoo;
  	}


	
	/**
	 * 
	 * Aggiornamenti su db.<br>
	 * <p>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi o i vari processi/funzioni post analisi.<br>
     * <p>
     * Oggetti, relazioni e altro associato al programma  sono gà state eliminati
     *  dal data base con sqlDeleteProgram() e sqlDeleteGeneric()<br>
     * <p>
	 * @param ictx 
     * 
     * @param boolean isProcessAnalysis true se a fronte di analisi preliminare sorgente, false se di processi successivi
     * @param String idObjectUnderAnalysis con nome programma correntemente sotto analisi
     * @param boolean isConnectionToOpen true se necessario acquisire la connessione dal pool disponibile
     * @param boolean isConnectionToRelease true  se connessione da rilasciare a fine aggiornamenti
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean update(boolean isProcessAnalysis
			            , boolean isProcessSystemLevel
			            , String idObjectUnderAnalysis
			            , InnerContextAnalysis ictx
			             ) throws SQLException, ExceptionAmrita  {
		
		EnumObject typeObject = null;
		EnumObjectStatus objectStatus = null;
		long timeMsStartDb = 0;
		long timeMsStart = 0;
		long timeMsEnd = 0;
		String idObject = "";
		String strSql = "";
		boolean objectAlreadyOnDb = false;

		long msDbUpdate = 0;                       			// ms totale di update data base
	    long msUpdObject = 0;                               // ms Object aggiornati
	    long msInsRelation = 0;                             // ms Relation inserite
	    long msInsRelationOrigin = 0;                       // ms RelationOrigin inserite
	    long msInsCopyEntity = 0;                           // ms copyEntityDefinition inserite
	    long msInsWhereUsed = 0;                            // ms wehereUsed inserite
	    long msInsMetric = 0;                               // ms Metric inserite
	    long msInsMetricViolation = 0;                      // ms MetricViolation inserite
	    long msInsDynamicField = 0;                         // ms DynamicField inserite
	    long cntInsObject = 0;                              // Counter Object inseriti
	    long cntUpdObject = 0;                              // Counter Object aggiornati
	    long cntInsWhereUsed = 0;                           // Counter wehereUsed inserite
	    long cntInsRelation = 0;                            // Counter relazioni inserite
	    long cntInsRelationOrigin = 0;                      // Counter relazioni origine inserite
	    long cntInsCopyEntity = 0;                          // Counter copyEntityDefinition inserite
	    long cntInsMetric = 0;                              // Counter Metric inserite
	    long cntInsMetricViolation = 0;                     // Counter MetricViolation inserite
	    long cntInsDynamicField = 0;                        // Counter DynamicField inserite
	    int i = 0;	
		
	    Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO1 = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false,false, ucfg);
		timeMsStartDb = System.currentTimeMillis();
		
		// Inserimento per errori e informazioni di analisi
		updateInfoAndErrors(isProcessAnalysis, ictx);
						
		// Inserimento oggetti nuovi o aggiornamento di esistenti
		// L'oggetto programma è trattato come gli altri oggetti
		// Nel caso di analisi di programma è il primo oggetto programma dell'array list

		timeMsStart = System.currentTimeMillis();
		i=0;
		for (EntityObject entityObject : this.al_DbObject) {
			
			 objectAlreadyOnDb = false;
			 objectStatus = entityObject.getStatus();       // Save status
             objectAlreadyOnDb = eoDAO1.read(entityObject);
             
             // Oggetto sotto analisi  esistente nel sistema/sottosistema corrente: aggiorno status e data ultimo aggiornamento 
             // Tutti gli oggetti/relazioni/opzioni/etc sono stati inseriti al momento dell'analisi del copy stmt e ricorsiva
             if (objectAlreadyOnDb
             &&   isProcessAnalysis) {   
            	 entityObject.setStatus(objectStatus);    					  // Nuovo status
            	 entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
            	 entityObject.setTmLastAnalysis(DateTimeService.getDateFormatted(new Date(), "hhmmss") + "00");
            	 entityObject=this.al_DbObject.get(i);
            	 eoDAO1.update(entityObject);
            	 cntUpdObject++;
			 } else {
				 eoDAO1.create(entityObject);
		         cntInsObject++;
			 }
	         conn.commit();
     		 timeMsEnd = System.currentTimeMillis();
    		 msUpdObject = timeMsEnd - timeMsStart;
       		 
             // Delete definizioni esistenti per il copy se:
             //  - Abilitato l'inserimento generale delle definizioni dei copy
             //  - Abilitato il reinserimento generale delle definizioni dei copy per copy già esistenti
             //  - Richiesta esplicita di forzatura reinserimento per il modulo copy
     		 if (entityObject.getTypeObject()  == EnumObject.OBJECT_COPY_COBOL_PROC ||
     			 entityObject.getTypeObject()  == EnumObject.OBJECT_COPY_COBOL_DATA   ) {
                 if (di.optCopyDefOnDbToInsert
                  || di.optReBuildCopyDefOnDb 
                  || di.al_copyToForceReBuildOndb.contains(idObject)
                    )  {
                    // Tracciato copy da ricostruire: delete definizioni presenti
            		strSql = "DELETE FROM CopyEntityDefinition  WHERE sys = '" + di.systemInput + "'" +
       					     		 	      " AND  subSys = '" + di.subSystemInput + "'" +
       						                  " AND  idObject = '" + idObject + "'" +
       						                  " AND (typeObject = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal()  +  " OR " +
       						                  "      typeObject = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal()  +  
       						                  "     )";
            		eoDAO1.execSqlGeneric(strSql);
            		conn.commit();
            		
           		 } // end-if
                 
			 } // end-if
     		 
     		 i++;
  		} // end-for

		IDAOMapDescriptor eoDA18 = (DAOImplMapDescriptor) AmritaStartup.sqlFactory.getDAOMapDescriptor(conn, false, false, ucfg);
		 
		// Inserimento oggetti mapDescriptor o aggiornamento esistenti
		for (EntityMapDescriptor entityMapDescriptor : this.al_DbMapDescriptor) {	                		 
             objectAlreadyOnDb = eoDA18.create(entityMapDescriptor);
             
            // MapDescriptor già presente su db, aggiorno
           if (!objectAlreadyOnDb) {   
        	   eoDA18.update(entityMapDescriptor);
           } 
		}
	    conn.commit();	 
		 
 		// Inserimento MapItem
		IDAOMapItem eoDA19 = (DAOImplMapItem) AmritaStartup.sqlFactory.getDAOMapItem(conn, false,false, ucfg);
		eoDA19.createBulk(this.al_DbMapItem);
		conn.commit();
		
		// 
		// Esecuzione SQL UPDATE  generico Object 
		//
		for (String sqlUpdate : this.al_sqlUpdate) {
			eoDAO1.execSqlGeneric(sqlUpdate);
		}
		conn.commit();

 		// Inserimento opzioni
		IDAOObjectOption eoDAO5 = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false,false, ucfg);
		eoDAO5.createBulk(this.al_DbObjectOption);
		
		// Inserimento relazioni
		IDAORelation eoDAO6 = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false,false, ucfg);
		timeMsStart = System.currentTimeMillis();
		eoDAO6.createBulk(this.al_DbRelation);
		conn.commit();
		cntInsRelation=this.al_DbRelation.size();
		timeMsEnd = System.currentTimeMillis();
		msInsRelation = timeMsEnd - timeMsStart;
		
		// Scan origine relazioni
		IDAORelationOrigin eoDAO7 = (DAOImplRelationOrigin) AmritaStartup.sqlFactory.getDAORelationOrigin(conn, false,false, ucfg);
		timeMsStart = System.currentTimeMillis();
		eoDAO7.createBulk(this.al_DbRelationOrigin);
		conn.commit();
		cntInsRelationOrigin=this.al_DbRelationOrigin.size();
 		timeMsEnd = System.currentTimeMillis();
		msInsRelationOrigin = timeMsEnd - timeMsStart;
						 		
		// Inserimento definizione campi moduli copy/tabelle.
 		// Nell'ArrayList sono presente tutti i campi di tutti i moduli copy.
		IDAOCopyEntityDefinition eoDAO2 = (DAOImplCopyEntityDefinition) AmritaStartup.sqlFactory.getDAOCopyEntityDefinition(conn, false,false, ucfg);
		List<EntityCopyEntityDefinition> wal_DbCopyEntityDefinition = new ArrayList<EntityCopyEntityDefinition> ();				
		timeMsStart = System.currentTimeMillis();
		for (EntityCopyEntityDefinition entityCopyEntityDefinition : this.al_DbCopyEntityDefinition) {
			idObject = entityCopyEntityDefinition.getIdObject();
			
            // Inserimento definizioni esistenti per il copy se:
            //  - Abilitato l'inserimento generale delle definizioni dei copy
            //  - Abilitato il reinserimento generale delle definizioni dei copy per copy già esistenti
            //  - Richiesta esplicita di forzatura reinserimento per il modulo copy
			if (entityCopyEntityDefinition.getTypeObject() == EnumObject.OBJECT_COPY_COBOL_DATA) {
	           if (di.optCopyDefOnDbToInsert || di.optReBuildCopyDefOnDb ||  di.al_copyToForceReBuildOndb.contains(idObject))  {
	        	   wal_DbCopyEntityDefinition.add(entityCopyEntityDefinition);
   			   }  
               continue; 
			}
			// Può essere solo una colonna sql da inserire
			wal_DbCopyEntityDefinition.add(entityCopyEntityDefinition);
			
  		} // end-for
		eoDAO2.createBulk(wal_DbCopyEntityDefinition);
		conn.commit();
		cntInsCopyEntity=wal_DbCopyEntityDefinition.size();
		timeMsEnd = System.currentTimeMillis();
		msInsRelation = timeMsEnd - timeMsStart;
		
		// Inserimento where used di campi di copy/file/tabelle
		IDAOWhereUsedItem eoDAO3 = (DAOImplWhereUsedItem) AmritaStartup.sqlFactory.getDAOWhereUsedItem(conn, false,false, ucfg);
		List<EntityWhereUsedItem> wal_DbWhereUsed = new ArrayList<EntityWhereUsedItem> ();		
 		for (EntityWhereUsedItem entityWhereUsedItem : this.al_DbWhereUsed) {          
            // Where used di copy item
 	        typeObject = entityWhereUsedItem.getTypeObject();
 	        if (typeObject == EnumObject.OBJECT_COPY_COBOL_DATA			    
 			&&  (di.optWhereUsedCopyItemsOnDbToInsert || di.optWhereUsedEntityColumnsOnDbToInsert)) {
	 			wal_DbWhereUsed.add(entityWhereUsedItem);
	 			cntInsWhereUsed++;
				continue;
			}
 		} // end-for
		timeMsStart = System.currentTimeMillis();		
		eoDAO3.createBulk(wal_DbWhereUsed);
		conn.commit();
		timeMsEnd = System.currentTimeMillis();
		msInsWhereUsed = timeMsEnd - timeMsStart;
		
		// Inserimento definizione colonne indici.
 		// Nell'ArrayList sono presente tutte le colonne di tutti gli indici correnti.
		IDAOIndexItem eoDAO4 = (DAOImplIndexItem) AmritaStartup.sqlFactory.getDAOIndexItem(conn, false,false, ucfg);
		eoDAO4.createBulk(this.al_DbIndexItem);
		conn.commit();
		
 		// Inserimento campi istruzioni dinamiche
		IDAODynamicField eoDAO8 = (DAOImplDynamicField) AmritaStartup.sqlFactory.getDAODynamicField(conn, false,false, ucfg);
		eoDAO8.createBulk(this.al_DbDynamicField);
		conn.commit();
 		
 		// Inserimento sottocampi istruzioni dinamiche
		IDAODynamicFieldSub eoDAO9 = (DAOImplDynamicFieldSub) AmritaStartup.sqlFactory.getDAODynamicFieldSub(conn, false,false, ucfg);
		eoDAO9.createBulk(this.al_DbDynamicFieldSub);
		conn.commit();
		cntInsDynamicField=this.al_DbDynamicFieldSub.size();
					
 		// Inserimento assegnazioni sottocampi istruzioni dinamiche
		IDAODynamicFieldSubSetting eoDA10 = (DAOImplDynamicFieldSubSetting) AmritaStartup.sqlFactory.getDAODynamicFieldSubSetting(conn, false,false, ucfg);
		eoDA10.createBulk(this.al_DbDynamicFieldSubSetting);
		conn.commit();
		cntInsDynamicField=this.al_DbDynamicFieldSubSetting.size();
				
		// Inserimento valori campi dinamici istruzioni 
		IDAODynamicFieldSubValue eoDA11 = (DAOImplDynamicFieldSubValue) AmritaStartup.sqlFactory.getDAODynamicFieldSubValue(conn, false,false, ucfg);
		eoDA11.createBulk(this.al_DbDynamicFieldSubValue);
		conn.commit();
						
		// Inserimento richiesta/certificazione valori esterni associati a istruzioni dinamiche (tabelle, nomi terminale etc.)
		IDAODynamicFieldSubWaitExt eoDA12 = (DAOImplDynamicFieldSubWaitExt) AmritaStartup.sqlFactory.getDAODynamicFieldSubWaitExt(conn, false,false, ucfg);
		eoDA12.createBulk(this.al_DbDynamicFieldSubWaitExt);
		conn.commit();
		
		// Inserimento valori esterni associati a istruzioni dinamiche (tabelle, nomi terminale etc.)
		IDAODynamicValueExt eoDA13 = (DAOImplDynamicValueExt) AmritaStartup.sqlFactory.getDAODynamicValueExt(conn, false,false, ucfg);
		eoDA13.createBulk(this.al_DbDynamicValueExt);
		conn.commit();
		
 		// Inserimento tag applicativi incontrati nel sorgente
		IDAOTagValue eoDA14 = (DAOImplTagValue) AmritaStartup.sqlFactory.getDAOTagValue(conn, false,false, ucfg);
		eoDA14.createBulk(this.al_DbTagValue);
		conn.commit();
		
  		// Inserimento Metriche di programma. 
		IDAOMetricValue eoDA15 = (DAOImplMetricValue) AmritaStartup.sqlFactory.getDAOMetricValue(conn,false,false, ucfg);
		timeMsEnd = System.currentTimeMillis();
		eoDA15.createBulk(this.al_DbMetric);
		conn.commit();
		cntInsMetric=this.al_DbMetric.size();
		msInsMetric = timeMsEnd - timeMsStart;		
 		
  		// Inserimento violazioni alle Metriche di programma. 
		IDAOMetricViolation eoDA16 = (DAOImplMetricViolation) AmritaStartup.sqlFactory.getDAOMetricViolation(conn, false,false, ucfg);
		timeMsStart = System.currentTimeMillis();
		eoDA16.createBulk(this.al_DbMetricViolation);
		conn.commit();
		cntInsMetricViolation=this.al_DbMetricViolation.size();
 		timeMsEnd = System.currentTimeMillis();
		msInsMetricViolation = timeMsEnd - timeMsStart;
 		
 		// Elapsed complessivo aggiornamenti
		msDbUpdate = timeMsEnd - timeMsStartDb;
		
		// Update AnalysisInfo
		IDAOObjectAnalysisInfo eoDA17 = (DAOImplObjectAnalysisInfo) AmritaStartup.sqlFactory.getDAOObjectAnalysisInfo(conn, false,false, ucfg);		
		this.dbObjectAnalysisInfo.setMsDbUpdate(msDbUpdate);
		this.dbObjectAnalysisInfo.setMsUpdObject(msUpdObject);
		this.dbObjectAnalysisInfo.setMsInsRelation(msInsRelation);
		this.dbObjectAnalysisInfo.setMsInsRelationOrigin(msInsRelationOrigin);
		this.dbObjectAnalysisInfo.setMsInsCopyEntity(msInsCopyEntity);
		this.dbObjectAnalysisInfo.setMsInsWhereUsed(msInsWhereUsed);
		this.dbObjectAnalysisInfo.setMsInsMetric(msInsMetric);
		this.dbObjectAnalysisInfo.setMsInsMetricViolation(msInsMetricViolation);
		this.dbObjectAnalysisInfo.setMsInsDynamicField(msInsDynamicField);
		this.dbObjectAnalysisInfo.setCntInsObject(cntInsObject);
		this.dbObjectAnalysisInfo.setCntUpdObject(cntUpdObject);
		this.dbObjectAnalysisInfo.setCntInsRelation(cntInsRelation);
		this.dbObjectAnalysisInfo.setCntInsRelationOrigin(cntInsRelationOrigin);
		this.dbObjectAnalysisInfo.setCntInsCopyEntity(cntInsCopyEntity);
		this.dbObjectAnalysisInfo.setCntInsWhereUsed(cntInsWhereUsed);
		this.dbObjectAnalysisInfo.setCntInsMetric(cntInsMetric);
		this.dbObjectAnalysisInfo.setCntInsMetricViolation(cntInsMetricViolation);
		this.dbObjectAnalysisInfo.setCntInsDynamicField(cntInsDynamicField);

		// Solo se si sta analizzando pgm cobol è valorizzato
		if (this.dbObjectAnalysisInfo.getTypeObject() != EnumObject.NOT_ASSIGNED) {
			if (objectAlreadyOnDb) {
				eoDA17.update(this.dbObjectAnalysisInfo);
			} else {
				eoDA17.create(this.dbObjectAnalysisInfo);
			}
		}
				
        // Commit finale
		conn.commit();
 		DataBaseConnections.releaseConnection(conn);
 		
 		// Clear connessioni
 		eoDAO1.setConn(null);
 		eoDAO2.setConn(null);
 		eoDAO3.setConn(null);
 		eoDAO4.setConn(null);
 		eoDAO5.setConn(null);
 		eoDAO6.setConn(null);
 		eoDAO7.setConn(null);
 		eoDAO8.setConn(null);
 		eoDAO9.setConn(null);
 		eoDA10.setConn(null);
 		eoDA11.setConn(null);
 		eoDA12.setConn(null);
 		eoDA13.setConn(null);
 		eoDA14.setConn(null);
 		eoDA15.setConn(null);
 		eoDA16.setConn(null);
 		eoDA17.setConn(null);
		eoDA18.setConn(null);
 		eoDA19.setConn(null);
 		return false;
	}


	
	/**
	 * 
	 * Restituisce true se è già presente il tracciato del copy specificato<br>
	 * Non viene testato il sottosistema proprietario del tracciato.
	 * <p>
     * <p>
	 * @param EntityCopyEntityDefinition  
     * @param boolean isConnectionToOpen true se necessario acquisire la connessione dal pool disponibile
     * @param boolean isConnectionToRelease true  se connessione da rilasciare a fine aggiornamenti
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public int getCntFieldCopyDefined(  
			                     String sys
			                   , String idCopy
			                   , EnumObject typeCopy 
			                   , boolean isConnectionToOpen
			                   , boolean isConnectionToRelease

			             ) throws SQLException, ExceptionAmrita  {
		
		String strSql="";
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOCopyEntityDefinition eoDAO = (DAOImplCopyEntityDefinition) AmritaStartup.sqlFactory.getDAOCopyEntityDefinition(conn, false,false, ucfg);

 		// Composizione Select di lettura EntityObject
		strSql = "SELECT COUNT(*) FROM CopyEntityDefinition " 
		+	  " WHERE sys = '" + this.di.systemInput + "'"
	    +	  "   AND idObject = '" + idCopy+ "'"
	    +	  "   AND typeObject = " + typeCopy.ordinal();

			
		// Esecuzione query e produzione  ResultSet
		ResultSet rs = eoDAO.execSqlGeneric(strSql);

		// Scan valori trovati
		boolean found = rs.next();
		int cnt = rs.getInt(1);
		rs.close();		

		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return cnt;
	}
	/**
	 * 
	 * Aggiornamenti su db della sole tabella con info di analisi.<br>
	 * <p>
     * Aggiornamento data base a partire dalla struttura predisposta
     * e già valorizzate durante l'analisi o i vari processi/funzioni post analisi.<br>
     * <p>
     * 
     * @param boolean isProcessAnalysis true se a fronte di analisi preliminare sorgente, false se di processi successivi
     * @param String idObjectUnderAnalysis con nome programma correntemente sotto analisi
     * @param boolean isConnectionToOpen true se necessario acquisire la connessione dal pool disponibile
     * @param boolean isConnectionToRelease true  se connessione da rilasciare a fine aggiornamenti
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean updateInfo(boolean isProcessAnalysis) throws SQLException, ExceptionAmrita  {
		
		boolean objectNotAlreadyOnDb = false;
		
		
		// Inserimento/update oggetto con informazioni dimensionali analisi valide 
		// Se restituisce true la riga NON era presente sul db
		// Se restituisce false la riga ERA presente e deve essere aggiornata,
		//    probabilmente questo metodo è stato richiamato a fronte di exception
		Connection conn = DataBaseConnections.getConnection();
		IDAOObjectAnalysisInfo eoDAO = (DAOImplObjectAnalysisInfo) AmritaStartup.sqlFactory.getDAOObjectAnalysisInfo(conn, false,false, ucfg);
		
		objectNotAlreadyOnDb = eoDAO.create(this.dbObjectAnalysisInfo);
		if (!objectNotAlreadyOnDb) {
			eoDAO.update(this.dbObjectAnalysisInfo);
		}
			
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return false;
	}
	

	
	/**
	 * 
	 * Aggiornamenti su db delle sole tabelle con errori e info di analisi.<br>
	 * <p>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi o i vari processi/funzioni post analisi.<br>
     * <p>
     * Oggetti, relazioni e altro associato al programma  sono gà state eliminati
     *  dal data base con sqlDeleteProgram() e sqlDeleteGeneric()<br>
     * <p>
	 * @param ictx 
     * 
     * @param boolean isProcessAnalysis true se a fronte di analisi preliminare sorgente, false se di processi successivi
     * @param String idObjectUnderAnalysis con nome programma correntemente sotto analisi
     * @param boolean isConnectionToOpen true se necessario acquisire la connessione dal pool disponibile
     * @param boolean isConnectionToRelease true  se connessione da rilasciare a fine aggiornamenti
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean updateInfoAndErrors(boolean isProcessAnalysis
						             , InnerContextAnalysis ictx
						              ) throws SQLException, ExceptionAmrita  {
		
		// Inserimento/update oggetto con informazioni dimensionali analisi
		// Al momento valido solo per analizi di programmi cobol
	    if (this.dbObjectAnalysisInfo.getTypeObject() == EnumObject.OBJECT_PGM_COBOL) {
		   updateInfo(isProcessAnalysis);
         }
	  	
	    // nessun errore da loggare
	    if (this.al_DbObjectAnalysisError.size() == 0) {
			return false;
		}
	    
		Connection conn = DataBaseConnections.getConnection();
		IDAOObjectAnalysisError eoDAO = (DAOImplObjectAnalysisError) AmritaStartup.sqlFactory.getDAOObjectAnalysisError(conn, false,false, ucfg);

		// Inserimento oggetti con errori/warning rilevati
		eoDAO.createBulk(this.al_DbObjectAnalysisError);
		
       // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
 		
		return false;
	}
	

	/* ---------------------------------------------------------------------------
     * Restituisce true se il nome oggetto è un mapset
     * ---------------------------------------------------------------------------
     */
	public boolean isMapset(String idObject) throws ExceptionAmrita, SQLException {
		EntityObject eo = null;	
		boolean isFound = false;
		
		eo = new EntityObject();
		eo.setSystem(this.di.systemInput);
		eo.setSubSystem(this.di.subSystemInput);
		eo.setTypeObject(EnumObject.OBJECT_CICS_MAPSET);
		eo.setIdObject(idObject);
		
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
		isFound=eoDAO.read(eo);	
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
        return isFound;
	}
	
	/* ---------------------------------------------------------------------------
     * Restituisce il nome della mappa Cics  associata al mapset Cics
     * ---------------------------------------------------------------------------
     */
	public String getMapName(String idObject) throws ExceptionAmrita, SQLException {
		List<EntityRelation> ls_object = null;
		String mapName = "";

		Connection conn = DataBaseConnections.getConnection();
		IDAORelation erDAO = (DAOImplRelation) AmritaStartup.sqlFactory.getDAORelation(conn, false, false, ucfg);

		ls_object=erDAO.findAllBySubSysIdBRel(di.systemInput, di.subSystemInput, idObject, EnumRelation.CICS_MAP_MAPSET);
		
		if (ls_object.size() > 0) {
			mapName = ls_object.get(0).getIdObjectA();
		}
		DataBaseConnections.releaseConnection(conn);
		erDAO.setConn(null);        
		return mapName;
	}
	

	/* ---------------------------------------------------------------------------
     * Lettura oggetto 
     * ---------------------------------------------------------------------------
     */
	public boolean getObject(EntityObject eo) throws ExceptionAmrita, SQLException {

		boolean isFound = false;
		Connection conn = DataBaseConnections.getConnection();
		IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
		isFound=eoDAO.read(eo);	
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
        return isFound;
	}
	
	
    /*
     * Restituisce l'oggetto  da struttura db/sistema/sottosistema corrente/ oppure quello owner/ o new object
     * Se non presente inserisce l'oggetto nella struttura x updates db
     * 1) Si verifica se già inserito in struttura db (con il suo subSysOwner)
     * 2) Si verifica se definito in un differente sys/subsys e si prende quel subSys come Owner
     * 3) Se NON ancora acquisito si inserisce come da aquisire
     * 4) Negli altri casi si recupera il sottosistema owner via euser exit
     */
      public EntityObject getAddObjectEntitled(String objectName, EnumObject typeObject) throws ExceptionAmrita, SQLException {
  		EntityObject eo = null;
		EntityObject eoOther = null;       // Oggetto definito come owner in diverso sistema/sottosistema
		UserExitInfo userExitInfo = null;
		boolean isEntityDefined = false;  
		
		// 1) Verifico se oggetto già presente in struttura per inserimento finale su db
		eo = getDbObjEntity(typeObject, objectName, this.di.systemInput, this.di.subSystemInput);
		if (eo != null) {
		  return eo;
		}
		
		// 2) Cerca oggetto in sys/subsys corrente
		eo=new EntityObject();
		eo.setSystem(this.di.systemInput);
		eo.setSubSystem(this.di.subSystemInput);
		eo.setIdObject(objectName);
		eo.setTypeObject(typeObject);
		isEntityDefined = this.getObject(eo);
		
		// Oggetto già inserito ins struttura per sys/subSys in elaborazione
		if (isEntityDefined) {
			return eo;
		}
		
		// 3) Verifica se definito in differente sistema/sottosistema rispetto a quello in elaborazione
		eoOther = this.getObjectOwner(typeObject, objectName, this.di.systemInput, this.di.subSystemInput, true, true);
		// Oggetto definito in diverso sys/subSys
		if (eoOther != null) {
			eo=new EntityObject();
			eo.setSystem(this.di.systemInput);
			eo.setSubSystem(this.di.subSystemInput);
			eo.setSystemOwner(this.di.systemInput);                   // Assumo il copy appartenga al sys/subsys corrente
			eo.setSubSystemOwner(eoOther.getSubSystem());				
			eo.setIdObject(objectName);
			eo.setTypeObject(typeObject);
			eo.setStatus(EnumObjectStatus.OBJECT_NOT_TO_BE_ANALYZED);     // Va analizzato solo nel suo sottosistema owner
			eo.setSystemOwner(eoOther.getSystem());
			eo.setSubSystemOwner(eoOther.getSubSystemOwner());
			this.addObjEntity(eo);
			return eo;
		}

		// 4) Oggetto non ancora aquisito e analizzato in nessun sistema/sottosistema
		eo=new EntityObject();
		eo.setIdObject(objectName);
		eo.setSystem(this.di.systemInput);
		eo.setSubSystem(this.di.subSystemInput);
		eo.setSystemOwner(this.di.systemInput);                   // Assumo il copy appartenga al sys/subsys corrente
		eo.setSubSystemOwner(this.di.subSystemInput);				
		eo.setStatus(EnumObjectStatus.OBJECT_TO_BE_ACQUIRED);     // Status di default
		eo.setTypeObject(typeObject);
		eo.setFileSource(objectName);                             // Nome oggetto 
		
		// Recupero sottosistema owner via exit 
		userExitInfo = new UserExitInfo();
		userExitInfo.setRunningProcessFunction(getRunningProcessFunction());
		userExitInfo.setCustomerCode(di.curCustomerCode);
		userExitInfo.setCustomerInfo(di.curCustomerInfo);
		userExitInfo.setNameToEvaluate(objectName);
		userExitInfo.setSubSystemOwner(this.di.subSystemInput);
		userExitInfo.setUserExitOperation(EnumUserExit.GET_SUB_SYSTEM_OWNER);
		userExitGenericActivate(userExitInfo);
		eo.setSubSystem(userExitInfo.getSubSystemOwner());	
		
		this.completeInfoObject(eo);
		
		this.addObjEntity(eo);
		return eo;
	}	
      
		
      
      
      
    /* ------------------------------------------------------------------------------
     * Lettura oggetto con stesso nome/tipo in diverso sottosistema di quello fornito 
     * Si restituisce l'oggetto solo se si trova l'oggetto nel suo sottosistema owner 
     * -------------------------------------------------------------------------------
     * 
     */
	public EntityObject getObjectOwner(EnumObject typeObject, String idObject, String sysCur, String subSysCur, boolean isConnectionToOpen, boolean isConnectionToRelease) throws ExceptionAmrita, SQLException {
		
		List<EntityObject> ar_objEntity = null;
		EntityObject entityObject = null;
		String whereCondition = "";

   	    entityObject = new EntityObject();	
      	whereCondition = "";
      
     	// Composizione Where di lettura oggetto proprietario di diverso sottosistema
      	whereCondition =  "     idObject = '" +idObject 	+ "'"
     	               +  " AND typeObject = " + typeObject.ordinal()
    	               +  " AND sys = '" + sysCur + "'"
    	               +  " AND subSys <> '" + subSysCur + "'"
    	               +  " AND sys    = sysOwner"
                       +  " AND subSys = subSysOwner";
      	
     	// Lettura oggetti di tutti i sistemi/sottosistemi
		Connection conn = DataBaseConnections.getConnection();
     	IDAOObject eoDAO = (DAOImplObject) AmritaStartup.sqlFactory.getDAOObject(conn, false, false, ucfg);
     	ar_objEntity = eoDAO.findAllWhere(whereCondition, "");
     	
     	if (ar_objEntity.size() == 0) {
     		entityObject = null;
		} else {
	    	entityObject = ar_objEntity.get(0);
		}
  
     	DataBaseConnections.releaseConnection(conn);
     	eoDAO.setConn(null);
     	
		return entityObject;
	}

    
  /* ------------------------------------------------------------------------------
   * Restituisce true se presente l'opzione per l'oggetto specificato
   * -------------------------------------------------------------------------------
   */
	public boolean isThereObjectOption(String sysCur, String subSysCur, EnumObject typeObject, String idObject, EnumObjectOption option) throws ExceptionAmrita, SQLException {
		
		List<EntityObjectOption> ar_objObjectOption = null;
		String whereCondition = "";
	
    	whereCondition = "";
    
      	// Composizione Where di lettura oggetto proprietario di diverso sottosistema
    	whereCondition =  "     idObject = '" +idObject 	+ "'"
   	               +  " AND typeObject = " + typeObject.ordinal()
  	               +  " AND sys = '" + sysCur + "'"
  	               +  " AND subSys = '" + subSysCur + "'"
  	               +  " AND optionObject = " + option.ordinal()
  	               ;
    	
	   	// Lettura oggetti di tutti i sistemi/sottosistemi
		Connection conn = DataBaseConnections.getConnection();
	   	IDAOObjectOption eoDAO = (DAOImplObjectOption) AmritaStartup.sqlFactory.getDAOObjectOption(conn, false, false, ucfg);
	   	ar_objObjectOption = eoDAO.findAllWhere(whereCondition, "");
	   	
	   	if (ar_objObjectOption.size() > 0) {
		   	DataBaseConnections.releaseConnection(conn);
		   	eoDAO.setConn(null);
	   		return true;
		}
	
	   	DataBaseConnections.releaseConnection(conn);
	   	eoDAO.setConn(null);
   	
		return false;
	}
	/**
	 * 
	 * Esecuzione istruzioni sql di delete generiche impostate dal chiamante.<br>
	 * <p>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi.
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteGenericLevel() throws SQLException, ExceptionAmrita  {
		
		int cntDelete = 0;
		
		if (this.al_sqlDelete.size() == 0) {
			return false;
		}
		
		
		///////////////////////////////////////////////////////////////////////////////////////
		// Esecuzione statements delete generate nei processi da effettuare prima
		///////////////////////////////////////////////////////////////////////////////////////		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);
		
		for (String sqlDelete : this.al_sqlDelete) {
			eoDAO.execSqlGeneric(sqlDelete);
			cntDelete++;
		}

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		return false;
	}

	/**
	 * 
	 * Esecuzione istruzioni sql di update generiche impostate dal chiamante.<br>
	 * <p>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi. Attualmente NON usato
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlUpdateGenericLevel() throws SQLException, ExceptionAmrita  {
		
		int cntUpdate = 0;
				
		///////////////////////////////////////////////////////////////////////////////////////
		// Esecuzione statements delete generate nei processi da effettuare prima
		///////////////////////////////////////////////////////////////////////////////////////		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);

		for (String sqlUpdate : this.al_sqlUpdate) {
			eoDAO.execSqlGeneric(sqlUpdate);
			cntUpdate++;
		}

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}
	/**
	 * 
	 * Delete preliminare oggetti associati al programma su db.<br>
	 * <p>
	 * Si sta analizzando il sorgente di un programma.<br>
     * Si eliminano tutti i dati che verranno rigenerati a fronte dell'analisi del programma.
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteProgramLevel(String programName) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false,false, ucfg);


		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione oggetti info ed errori associati al programma
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM ObjectAnalysisInfo  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObject = '" + programName + "'" +
        " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);    
		conn.commit();

		strSql = "DELETE FROM ObjectAnalysisError  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObject = '" + programName + "'" +
        " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni del programma (se si sta analizzando il programma)
		///////////////////////////////////////////////////////////////////////////////////////	
		
		// Eliminazione di tutte le origini relazioni del programma
		strSql = "DELETE FROM RelationOrigin  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + programName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte le origini relazioni originate nel programma 
		strSql = "DELETE FROM RelationOrigin  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectOrigin = '" + programName + "'" +
		         " AND typeObjectOrigin = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte le  relazioni del programma 
		strSql = "DELETE FROM Relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + programName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte le opzioni del programma
		strSql = "DELETE FROM ObjectOption  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutti i where-used di copy items e entity columns utilizzati dal programma
		strSql = "DELETE FROM WhereUsed  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectRefer = '" + programName + "'" +
		         " AND typeObjectRefer = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte le richieste waiting/soddisfatte di valori esterni delle istruzioni dinamiche del programma
		strSql = "DELETE FROM DynamicFieldSubWaitExt  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte le impostazioni delle istruzioni dinamiche del programma
		strSql = "DELETE FROM DynamicFieldSubSetting WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte i valori delle istruzioni dinamiche del programma
		strSql = "DELETE FROM DynamicFieldSubValue WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte i sottocampi dinamici del programma
		strSql = "DELETE FROM DynamicFieldSub  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione di tutte i campi dinamici del programma
		strSql = "DELETE FROM DynamicField  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione delle violazione alle metriche di programma
		strSql = "DELETE FROM MetricViolation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione delle metriche di programma
		strSql = "DELETE FROM MetricValue  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		conn.commit();
		
		// Eliminazione metriche e violazioni alle metriche
		sqlDeleteProgramLevelMetrics(programName, false, false);
		sqlDeleteProgramLevelMetricsViolations(programName, false, false);
		
        // Consolidamento e chiusura connessione
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}

	/**
	 * 
	 * Delete preliminare oggetti associati al copy su db.<br>
	 * <p>
	 * Si sta analizzando il sorgente di un copy.<br>
     * Si eliminano tutti i dati che verranno rigenerati a fronte dell'analisi del modulo copy.
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteCopyLevel(String copyName) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione oggetti  errori associati al copy
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM ObjectAnalysisError  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObject = '" + copyName + "'" +
        " AND (typeObject = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal() +
        "         OR typeObject = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal() +
        "   ) ";
		eoDAO.execSqlGeneric(strSql);

		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni del copy
		///////////////////////////////////////////////////////////////////////////////////////	
		
		// Eliminazione di tutte le origini relazioni del copy
		strSql = "DELETE FROM RelationOrigin  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + copyName + "'" +
		         " AND (typeObjectA = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal() +
		         "         OR typeObjectA = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal() +
		         "   ) ";
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le  relazioni del copy 
		strSql = "DELETE FROM Relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + copyName + "'" +
		         " AND (typeObjectA = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal() +
		         "         OR typeObjectA = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal() +
		         "   ) ";
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le opzioni del copy
		strSql = "DELETE FROM ObjectOption  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + copyName + "'" +
		         " AND (typeObject = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal() +
		         "         OR typeObject = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal() +
		         "   ) ";
		eoDAO.execSqlGeneric(strSql);
		
			
        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return false;
	}

	/**
	 * 
	 * Delete preliminare righe copy ObjectAnalysisError su db.<br>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeletePgmCopyLevelErrors(String copyName) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
	
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);


		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione oggetti  errori associati al copy
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM ObjectAnalysisError  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObject = '" + copyName + "'" +
        " AND (typeObject = " + EnumObject.OBJECT_COPY_COBOL_DATA.ordinal() +
        "         OR typeObject = " + EnumObject.OBJECT_COPY_COBOL_PROC.ordinal() +
        "   ) ";
		eoDAO.execSqlGeneric(strSql);
		
		conn.commit();    // Consolido gli ultimi aggiornamenti
        DataBaseConnections.releaseConnection(conn);
        eoDAO.setConn(null);
		return false;
	}
	
	/**
	 * 
	 * Delete preliminare oggetti associati allo sript Sql su db.<br>
	 * <p>
	 * Si sta analizzando il sorgente di uno script Sql.<br>
     * Si eliminano tutti i dati che verranno rigenerati a fronte dell'analisi dello script.
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteScriptSqlLevel(String scriptName) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
				
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);
		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni dello script
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + scriptName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectB = '" + scriptName + "'" +
		         " AND typeObjectB = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le origini relazioni dello script
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + scriptName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectB = '" + scriptName + "'" +
		         " AND typeObjectB = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);

		// Eliminazione di tutte le origini relazioni originate nello script
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectOrigin = '" + scriptName + "'" +
			        " AND typeObjectOrigin = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le opzioni dello script
		strSql = "DELETE FROM object  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObject = '" + scriptName + "'" +
			        " AND typeObject = " + EnumObject.OBJECT_SQL_SCRIPT.ordinal();
		eoDAO.execSqlGeneric(strSql);
		

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}

	/**
	 * 
	 * Delete preliminare oggetti associati al jcl job su db.<br>
	 * <p>
	 * Si sta analizzando un jcl che contiene un job schedulabile.<br>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi.<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteJclJobLevel(String jclName, boolean isConnectionToOpen, boolean isConnectionToRelease) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
	
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni del jcl job  
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectB = '" + jclName + "'" +
			        " AND typeObjectB = " + EnumObject.OBJECT_JCL_JOB.ordinal() +
                    " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le origini relazioni del jcl job
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectB = '" + jclName + "'" +
			        " AND typeObjectB = " + EnumObject.OBJECT_JCL_JOB.ordinal() +
			        " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le origini relazioni originate nel jcl
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectOrigin = '" + jclName + "'" +
			        " AND typeObjectOrigin = " + EnumObject.OBJECT_JCL_JOB.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le opzioni del jcl
		strSql = "DELETE FROM objectOption WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObject = '" + jclName + "'" +
			        " AND typeObject = " + EnumObject.OBJECT_JCL_JOB.ordinal();
		eoDAO.execSqlGeneric(strSql);
		

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}

	/**
	 * 
	 * Delete preliminare oggetti associati al jcl include su db.<br>
	 * <p>
	 * Si sta analizzando un jcl che non è un job schedulabile e non è una proc.<br>
	 * Si tratta di statements jcl inserite con INCLUDE MEMBER=...<br>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi.<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteJclInclude(String includeName, boolean isConnectionToOpen, boolean isConnectionToRelease) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";

		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni del jcl include
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectB = '" + includeName + "'" +
			        " AND typeObjectB = " + EnumObject.OBJECT_JCL_INCLUDE.ordinal() +
                    " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObjectA = '" + includeName + "'" +
        " AND typeObjectA = " + EnumObject.OBJECT_JCL_INCLUDE.ordinal() +
        eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le origini relazioni del jcl job
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObjectB = '" + includeName + "'" +
			        " AND typeObjectB = " + EnumObject.OBJECT_JCL_INCLUDE.ordinal() +
			        " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
        " AND subSys = '" + di.subSystemInput + "'" +
        " AND idObjectA = '" + includeName + "'" +
        " AND typeObjectA = " + EnumObject.OBJECT_JCL_INCLUDE.ordinal() +
        eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le opzioni del jcl include
		strSql = "DELETE FROM objectOption WHERE sys = '" + di.systemInput + "'" +
			        " AND subSys = '" + di.subSystemInput + "'" +
			        " AND idObject = '" + includeName + "'" +
			        " AND typeObject = " + EnumObject.OBJECT_JCL_INCLUDE.ordinal();
		eoDAO.execSqlGeneric(strSql);
		

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}

	/**
	 * 
	 * Delete preliminare oggetti associati al jcl proc su db.<br>
	 * <p>
	 * Si sta analizzando un jcl che contiene una proc/pend.<br>
     * Aggiornamento data base a partire dalle strutture predisposte
     * e già valorizzate durante l'analisi.<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteJclProc(String procName, boolean isConnectionToOpen, boolean isConnectionToRelease) throws SQLException, ExceptionAmrita  {
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);
		
		
		///////////////////////////////////////////////////////////////////////////////////////
		// Eliminazione di tutte le relazioni del jcl include
		///////////////////////////////////////////////////////////////////////////////////////		
 
		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectB = '" + procName + "'" +
		         " AND typeObjectB = " + EnumObject.OBJECT_JCL_PROC.ordinal() +
                 " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);

		strSql = "DELETE FROM relation  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + procName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_JCL_PROC.ordinal() +
		 		eoDAO.execSqlGeneric(strSql);

		// Eliminazione di tutte le origini relazioni del jcl proc
		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectB = '" + procName + "'" +
		         " AND typeObjectB = " + EnumObject.OBJECT_JCL_PROC.ordinal() +
		         " AND typeObjectA <> " + EnumObject.OBJECT_LIBRARY.ordinal();
		eoDAO.execSqlGeneric(strSql);

		strSql = "DELETE FROM relationOrigin  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObjectA = '" + procName + "'" +
		         " AND typeObjectA = " + EnumObject.OBJECT_JCL_PROC.ordinal() +
		 		eoDAO.execSqlGeneric(strSql);

		// Eliminazione di tutte le opzioni del jcl proc
		strSql = "DELETE FROM objectOption WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + procName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_JCL_PROC.ordinal();
		eoDAO.execSqlGeneric(strSql);
		

        // Consolidamento e chiusura connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);


		return false;
	}


	
	/**
	 * 
	 * Delete preliminare oggetti associati al programma su db per le sole istruzioni dinamiche,
	 * sia locali al programma sia spreaded in programmi chiamanti.<br>
	 * <p>
	 * Si sta eseguendo un'elaborazione a livello di programma o di sistema, di soluzione del 
	 * codice dinamico.<br>
     * Questo metodo elimina tutte le origini di relazioni su <strong>Relation</strong> gestite da 
     * {@link EntityRelationOrigin} e originate da istruzioni dinamiche.<br>
     * Nel caso, a fronte dell'eliminazione  delle origini relazione, la relazione non presenti +
     * nemmeno una origine, viene eeliminata anche la relazione su <strong>RELA</strong>
     * gestite da {@link EntityRelation} e già valorizzate durante l'analisi.<br>
     * Per ogni istruzione vengono inoltre eliminati tutti i valori individuati su <strong>DVAL</strong>
     * e gestiti da {@link EntityDynamicValue}.<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	/*
	public boolean sqlDeleteProgramLevelDynamic(String programName
											  , EnumObject programType
											  , boolean isToDeleteOnlyInstructionUnresolved
											  , boolean isToDeleteOnlyInstructionDynamicSpreaded
											  , boolean isConnectionToOpen
											  , boolean isConnectionToRelease
											  ) throws SQLException, ExceptionAmrita  {
		
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);
		
		
		// Eliminazione di tutte le origini relazioni del programma dovute a istruzioni dinamiche
		// locali o spreaded
		strSql = "DELETE FROM RelationOrigin AS A WHERE "
		       +    "     A.sys = '" + di.systemInput    + "'" 
		       +    " AND A.subSys = '" + di.subSystemInput + "'" 
			   +    " AND A.idObjectRelA = '" + programName          + "'" 
			   +    " AND A.typeObjectA =  " + programType.ordinal() 
               +    " AND A.numInstrOrigin IN (SELECT B.numInstr FROM DynamicField AS B WHERE "
	           +                       "        B.sys = A.sys" 
	           +                       "    AND B.subSys = A.subSys"  
	           +                       "    AND B.idObject = A.idObjectRelA"  
	           +                       "    AND B.typeObject = A.typeObjectA" ;
	    if (isToDeleteOnlyInstructionUnresolved) {          
	    	strSql = strSql +          "    AND B.solved = false  ";
		}
	    if (isToDeleteOnlyInstructionDynamicSpreaded) {          
	    	strSql = strSql +          "    AND B.spreaded = true  ";
		}
	    strSql = strSql       +                       ")" ;
		
		eoDAO.execSqlGeneric(strSql);
		
		// Eliminazione di tutte le relazioni dinamiche senza più origine
		strSql = "DELETE FROM Relation R WHERE "  
               +     "     R.sys = '" + di.systemInput + "'"  
               +     " AND R.subSys = '" + di.subSystemInput + "'"  
			   +     " AND R.idObjectA = '" + programName + "'"  
			   +     " AND R.idObjectA = " + programType.ordinal()
		       +     " AND NOT EXISTS (SELECT * FROM RelationOrigin RO WHERE "
		       +                     "        RO.sys = R.sys" 
		       +                     "    AND RO.subSys = R.subSys"  
		       +                     "    AND RO.idObjectA = R.idObjectA"  
		       +                     "    AND RO.typeObjectA = R.typeObjectA" 
               +                     ")" ;
		
		eoDAO.execSqlGeneric(strSql);

		// Eliminazione di tutte le opzioni programma associate alle istruzioni dinamiche
		strSql = "DELETE FROM ObjectOption AS A WHERE "  
               +     "      A.sys = '" + di.systemInput + "'"  
               +     " AND  A.subSys = '" + di.subSystemInput + "'"  
			   +     " AND  A.idObject = '" + programName + "'"  
			   +     " AND  A.typeObject = " + programType.ordinal()
		       +     " AND (A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE.ordinal() 			        + " OR "
		       +     "      A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE_TO_SOLVE.ordinal() 			+ " OR "
		       +     "      A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SOLVED.ordinal() 			+ " OR "
		       +     "      A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SOLVED_FULL.ordinal() 		+ " OR "
		       +     "      A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE_SPREADED.ordinal() 			+ " OR "
		       +     "      A.optionObject = " + EnumObjectOption.PGM_WITH_DYNAMIC_CODE_WAITING_FOR_DATA.ordinal() 
		       +     "     )";
		
		eoDAO.execSqlGeneric(strSql);
		
		
		// Eliminazione di tutti i valori delle istruzioni dinamiche del programma
		// locali o spreaded
		strSql = "DELETE FROM DynamicVal AS A  WHERE "  
			   +    "     A.sys = '" + di.systemInput + "'" 				 
			   +    " AND A.subSys = '" + di.subSystemInput + "'" 				 
			   +    " AND A.idObject = '" + programName + "'"				 		 
			   +    " AND A.typeObject = " + programType.ordinal()                  	 
	    	   +    " AND A.numInstr IN (SELECT B.numInstr FROM DynamicField AS B WHERE " 	 
	           +    "                         B.sys = A.sys"   			 
	           +    "                     AND B.subSys = A.subSys"   			 
	           +    "                     AND B.idObject = A.idObject"   			 
	           +    "                     AND B.typeObject = A.typeObject"   			 
               +    "                     AND B.idField = A.idField" ;

	    if (isToDeleteOnlyInstructionUnresolved) {          
	    	strSql = strSql +          "    AND B.solved = false  ";
		}
	    if (isToDeleteOnlyInstructionDynamicSpreaded) {          
	    	strSql = strSql +          "    AND B.spreaded = true  ";
		}
	    strSql = strSql   +        "    )  ";
	     
		eoDAO.execSqlGeneric(strSql);
		
        // Consolidamento e rilascio connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);


		return false;
	}
	
*/
	
	/**
	 * 
	 * Delete preliminare oggetti associati al programma su db per le sole metriche.<br>
	 * <p>
     * Questo metodo elimina tutte le righe su <strong>METR</strong> relative al programma fornito.<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteProgramLevelMetrics(String programName
											  , boolean isConnectionToOpen
											  , boolean isConnectionToRelease
											  ) throws SQLException, ExceptionAmrita  {
		
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);
		
		// Eliminazione delle metriche di programma
		strSql = "DELETE FROM MetricValue  WHERE sys = '" + di.systemInput + "'" +
		         " AND subSys = '" + di.subSystemInput + "'" +
		         " AND idObject = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		
        // Consolidamento e rilascio connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}
	
	
	/**
	 * 
	 * Delete preliminare oggetti a fronte di esecuzione PROCESS_SYSTEM_LEVEL per aggregazione metriche.<br>
	 * <p>
     * In base alle direttive di esecuzione e ai processi a livello sistema da eseguire<br>
     * vengone eliminate le righe dalle tabelle opportune (METR, METP)
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteSystemLevelMetrics(boolean isConnectionToOpen
											 , boolean isConnectionToRelease
											 ) throws SQLException, ExceptionAmrita  {
		
		
		String strSql = "";

		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		// Eliminazione delle metriche aggregate a qualsiasi livello
		strSql = "DELETE FROM MetricValue  WHERE  " +
		         "      scope = " + EnumMetricsScope.SCOPE_LEVEL_SYSTEM.ordinal() 	+ " OR " +
	             "      scope = " + EnumMetricsScope.SCOPE_LEVEL_SUBSYSTEM.ordinal() + " OR " +
	             "      scope = " + EnumMetricsScope.SCOPE_LEVEL_GLOBAL.ordinal() 	+
                 "      ";
		eoDAO.execSqlGeneric(strSql);
		
        // Consolidamento e rilascio connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);

		return false;
	}
	
	/**
	 * 
	 * Delete tabelle sistema/sottosistema<br>
	 * <p>
     * Viene effettuata una pulizia di tutte le tabelle<br>
     * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlClearDb(  String sys
			                  , String[] subSys
			                  , boolean isConnectionToOpen
							  , boolean isConnectionToRelease
							) throws SQLException, ExceptionAmrita  {
		
		StringBuffer sb = null;
		sb = new StringBuffer();

		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);

		execDeleteString(eoDAO, sb, sys, subSys, "dynamicFieldSubSetting" );		
		execDeleteString(eoDAO, sb, sys, subSys, "dynamicFieldSubValue" );		
		execDeleteString(eoDAO, sb, sys, subSys, "dynamicFieldSubWaitExt" );
		execDeleteString(eoDAO, sb, sys, subSys, "dynamicFieldSub" );		
		execDeleteString(eoDAO, sb, sys, subSys, "dynamicfield" );
		execDeleteString(eoDAO, sb, sys, subSys, "metricValue" );		
		execDeleteString(eoDAO, sb, sys, subSys, "metricViolation" );
		execDeleteString(eoDAO, sb, sys, subSys, "objectAnalysisError" );		
		execDeleteString(eoDAO, sb, sys, subSys, "objectAnalysisInfo" );
		execDeleteString(eoDAO, sb, sys, subSys, "objectOption" );		
		execDeleteString(eoDAO, sb, sys, subSys, "relationOrigin" );
		execDeleteString(eoDAO, sb, sys, subSys, "relation" );	
		execDeleteString(eoDAO, sb, sys, subSys, "whereUsed" );	
		execDeleteString(eoDAO, sb, sys, subSys, "copyEntityDefinition" );
		execDeleteString(eoDAO, sb, sys, subSys, "object" );		

        // Rilascio connessione
		DataBaseConnections.releaseConnection(conn);

		return false;
	}	
	
	/*
	 * Append delete instructions
	 */
	private void execDeleteString(DAOImplSqlGeneric eoDAO, StringBuffer sb, String sys, String[] subSys, String table) throws ExceptionAmrita, SQLException {
		
		String strSql = "";
		sb.setLength(0);
		sb.append("delete FROM ");
		sb.append(table);
		sb.append(" where sys='");	
		sb.append(sys);	
		if (subSys[0].equals("*ALL*")) {
			sb.append("';");			  
		} else {
			sb.append("' and (");		
			for (int i = 0; i < subSys.length; i++) {
				if (i>0) {
					sb.append(" OR ");
				}
				sb.append("subSys='" + subSys[i] + "' " );
			}		
			sb.append(");");			
		}
		
		strSql=sb.toString();	
		
		eoDAO.execSqlGeneric(strSql);

		// Consolido gli  aggiornamenti
		eoDAO.getConn().commit();    
	}


	
	
	/**
	 * 
	 * Delete preliminare oggetti associati al programma su db per le sole violazioni alle metriche,
	 * <p>
     * Questo metodo elimina tutte le righe su <strong>METV</strong> relative al programma fornito.<br>
       * <p>
     * 
     * 
	 * @throws SQLException 
	 * @throws ExceptionAmrita 
	 * 
	 */
	public boolean sqlDeleteProgramLevelMetricsViolations(String programName
														, boolean isConnectionToOpen
														, boolean isConnectionToRelease
														 ) throws SQLException, ExceptionAmrita  {
		
		
		String strSql = "";
		
		Connection conn = DataBaseConnections.getConnection();
		DAOImplSqlGeneric eoDAO = (DAOImplSqlGeneric) AmritaStartup.sqlFactory.getDAOSqlGeneric(conn, false, false, ucfg);
				
		// Eliminazione delle violazione alle metriche di programma
		strSql = "DELETE FROM MetricViolation  WHERE sys = '" + di.systemInput + "'" +
		         " AND sys = '" + di.subSystemInput + "'" +
		         " AND subSys = '" + programName + "'" +
		         " AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal();
		eoDAO.execSqlGeneric(strSql);
		

        // Consolidamento e rilascio connessione
		conn.commit();
		DataBaseConnections.releaseConnection(conn);
		eoDAO.setConn(null);
		
		return false;
	}
	

	
	/**
	 * 
	 * Inserimento statement Sql Delete.<br>
	 * <p>
	 * Gi statement di delete verranno eseguiti prima di effettuare
	 * gli inserimenti.
	 * 
 	 * 
	 */
	public int addSqlDeleteStatement(String stmt) {
		al_sqlDelete.add(stmt);
		return al_sqlDelete.size() - 1;
	}
	
	/**
	 * 
	 * Inserimento statement Sql Update.<br>
	 * <p>
	 * Gi statement di delete verranno eseguiti dopo aver effettuato
	 * gli inserimenti.
	 * 
 	 * 
	 */
	public int addSqUpdateStatement(String stmt) {
		al_sqlUpdate.add(stmt);
		return al_sqlUpdate.size() - 1;
	}
	
	/**
	 * 
	 * Inserimento oggetto entity in struttura oggetti da inserire su db.
	 * 
 	 * 
	 */
	public int addObjEntity(EntityObject entityObject) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbObject.contains(entityObject)) {
			return al_DbObject.indexOf(entityObject);
		}
		al_DbObject.add(entityObject);
		
		return al_DbObject.size() - 1;
	}

	/**
	 * 
	 * Inserimento oggetto MapDescriptor in struttura oggetti da inserire su db.
	 * 
	 */
	public int addObjMapDescriptor(EntityMapDescriptor entityMapDescriptor) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbMapDescriptor.contains(entityMapDescriptor)) {
			return al_DbMapDescriptor.indexOf(entityMapDescriptor);
		}
		al_DbMapDescriptor.add(entityMapDescriptor);
		
		return al_DbMapDescriptor.size() - 1;
	}

	/**
	 * 
	 * Inserimento oggetto MapItem in struttura oggetti da inserire su db.
	 * 
	 */
	public int addObjMapItem(EntityMapItem entityMapItem) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbMapItem.contains(entityMapItem)) {
			return al_DbMapItem.indexOf(entityMapItem);
		}
		al_DbMapItem.add(entityMapItem);
		
		return al_DbMapItem.size() - 1;
	}


	/**
	 * 
	 * Ricerca oggetto entity in struttura oggetti da inserire su db.
	 * @param typeObject 
	 * 
	 */
	public EntityObject getDbObjEntity(EnumObject typeObject, String idObject, String sys, String subSys) {
		
		for (EntityObject eo : al_DbObject) {
			if (eo.getIdObject().equals(idObject)
			&&  eo.getTypeObject() == typeObject
			&&  eo.getSystem().equals(sys)	
			&&  eo.getSubSystem().equals(subSys)	
			) {
			  return eo;
			}
		}
		
		return null;
	}
	
	/**
	 * 
	 * Inserimento oggetto entityOption in struttura opzioni da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityOption(EntityObjectOption entityObjectOption) {
		// Oggetto già presente nella lista: skip
		if (al_DbObjectOption.contains(entityObjectOption)) {
			return;
		}
		al_DbObjectOption.add(entityObjectOption);
	}

	/**
	 * 
	 * Inserimento oggetto entityRelation in struttura opzioni da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityRelation(EntityRelation entityRelation) {

		// Oggetto già presente nella lista: skip
		if (al_DbRelation.contains(entityRelation)) {
			return;
		}
		al_DbRelation.add(entityRelation);
	}

	/**
	 * 
	 * Inserimento oggetto entityRelationOrigin in struttura opzioni da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityRelationOrigin(EntityRelationOrigin entityRelationOrigin) {
				
		// Oggetto già presente nella lista: skip
		if (al_DbRelationOrigin.contains(entityRelationOrigin)) {
			return;
		}
		al_DbRelationOrigin.add(entityRelationOrigin);
	}


	/**
	 * 
	 * Inserimento oggetto addObjEntityWhereUsed in struttura  da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityWhereUsed(EntityWhereUsedItem entityWhereUsedItem) {
				
		// Oggetto già presente nella lista: skip
		if (al_DbWhereUsed.contains(entityWhereUsedItem)) {
			return;
		}
		al_DbWhereUsed.add(entityWhereUsedItem);
	}

	/**
	 * 
	 * Inserimento oggetto entityCopyDefinition in struttura da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityCopyDefinition(EntityCopyEntityDefinition entityCopyDefinition) {
		
		al_DbCopyEntityDefinition.add(entityCopyDefinition);
	}

	/**
	 * 
	 * Inserimento oggetto entityIndexItem in struttura da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityIndexItem(EntityIndexItem entityIndexItem) {
		
		al_DbIndexItem.add(entityIndexItem);
	}

	/**
	 * 
	 * Inserimento oggetto metric in struttura oggetti da inserire su db.
	 * 
 	 * 
	 */
	public void addObjMetric(EntityMetricValue entityMetric) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbMetric.contains(entityMetric)) {
			return;
		}
		al_DbMetric.add(entityMetric);
	}
	
	/**
	 * 
	 * Inserimento oggetto metricViolation in struttura oggetti da inserire su db.
	 * 
	 */
	public void addObjMetricViolation(EntityMetricViolation entityMetricViolation) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbMetricViolation.contains(entityMetricViolation)) {
			return;
		}
		al_DbMetricViolation.add(entityMetricViolation);
	}
	

	/**
	 * 
	 * Inserimento oggetto DynamicFieldSubWaitExt in struttura oggetti da inserire su db.
	 * 
 	 * 
	 */
	public void addObjEntityDynamicFieldSubWaitExt(EntityDynamicFieldSubWaitExt entityDynamicFieldSubWaitExt) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbDynamicFieldSubWaitExt.contains(entityDynamicFieldSubWaitExt)) {
			return;
		}
		al_DbDynamicFieldSubWaitExt.add(entityDynamicFieldSubWaitExt);
	}
	

	/**
	 * 
	 * Inserimento oggetto EntityDynamicField in struttura da inserire su db.
	 * 
	 * 
	 */
	public int addObjEntityDynamicField(EntityDynamicField entityDynamicField) {
		EntityDynamicField entityDynamicFieldX = null;
		int i = 0;
		
		// Oggetto già presente nella lista: skip
		if (al_DbDynamicField.contains(entityDynamicField)) {
			for (i = 0; i < al_DbDynamicField.size(); i++) {
				entityDynamicFieldX = al_DbDynamicField.get(i);
				if (entityDynamicField.getSubSystem().equals(entityDynamicFieldX.getSubSystem()) 
				&&  entityDynamicField.getIdObject().equals(entityDynamicFieldX.getIdObject())
				&&  entityDynamicField.getTypeObject() == entityDynamicFieldX.getTypeObject()
				&&  entityDynamicField.getNumInstr() == entityDynamicFieldX.getNumInstr()
				&&  entityDynamicField.getIdField().equals(entityDynamicFieldX.getIdField() )) {
					return i;
				}
				
			}
			// Non deve succedere, provoca exception nel chiamante
			return -1;
		}
		al_DbDynamicField.add(entityDynamicField);
		return al_DbDynamicField.size() - 1;
	}

	/**
	 * 
	 * Inserimento oggetto EntityDynamicFieldSubValue in struttura da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityDynamicFieldSubValue(EntityDynamicFieldSubValue entityDynamicFieldSubValue) {
		
		al_DbDynamicFieldSubValue.add(entityDynamicFieldSubValue);
	}

	/**
	 * 
	 * Inserimento oggetto EntityDynamicFieldSub in struttura da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityDynamicFieldSub(EntityDynamicFieldSub entityDynamicFieldSub) {
		
		// Oggetto già presente nella lista: skip
		if (al_DbDynamicFieldSub.contains(entityDynamicFieldSub)) {
			return;
		}
		al_DbDynamicFieldSub.add(entityDynamicFieldSub);
	}

	/**
	 * 
	 * Inserimento oggetto EntityDynamicFieldSubSetting in struttura da inserire su db.
	 * 
	 * 
	 */
	public void addObjEntityDynamicFieldSubSetting(EntityDynamicFieldSubSetting entityDynamicInstructionFieldSubSetting) {
		
		al_DbDynamicFieldSubSetting.add(entityDynamicInstructionFieldSubSetting);
	}

	/**
	 * Restituisce l'oggetto con le informazioni di analisi.<br>
	 * <p>
	 * @return the dbObjectAnalysisInfo
	 */
	public EntityObjectAnalysisInfo getObjAnalysisInfo() {
		return dbObjectAnalysisInfo;
	}


	/**
	 * Imposta l'oggetto con le informazioni di analisi.<br>
	 * <p>
	 * @param dbObjectAnalysisInfo the dbObjectAnalysisInfo to set
	 */
	public void setObjAnalysisInfo(EntityObjectAnalysisInfo dbObjectAnalysisInfo) {
		this.dbObjectAnalysisInfo = dbObjectAnalysisInfo;
	}


	/**
	 * @return the al_DbObjectAnalysisError
	 */
	public ArrayList<EntityObjectAnalysisError> getObjsAnalysisError() {
		return al_DbObjectAnalysisError;
	}


	/**
	 * @param alDbObjectAnalysisError the al_DbObjectAnalysisError to set
	 */
	public void setObjsAnalysisError(ArrayList<EntityObjectAnalysisError> al_DbObjectAnalysisError) {
		this.al_DbObjectAnalysisError = al_DbObjectAnalysisError;
	}


	/*
	 * Completamento informazioni oggetto
	 * 
	 * 
	 */
	public void completeInfoObject(EntityObject entityObject) {
		
  		entityObject.setDtFirstAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmFirstAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");
  		entityObject.setDtLastAnalysis(DateTimeService.getDateFormatted(new Date(), "yyyyMMdd"));
		entityObject.setTmLastAnalysis(DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00");
		
	}

	/*
	 * Clone  oggetto
	 */
	public EntityObject cloneObject(EntityObject eo) {
		EntityObject eoCloned = new  EntityObject();

		eoCloned.setSystem(eo.getSystem());   						 
		eoCloned.setSubSystem(eo.getSubSystem());  
		eoCloned.setSubSystemOwner(eo.getSubSystemOwner());   
		eoCloned.setIdObject(eo.getIdObject());
		eoCloned.setIdObjectExtended(eo.getIdObjectExtended());
		eoCloned.setStatus(eo.getStatus());
		eoCloned.setTypeObject(eo.getTypeObject());
		eoCloned.setTypeObjectDescriptor(eo.getTypeObjectDescriptor());
		eoCloned.setLibraryDir(eo.getLibraryDir());
		eoCloned.setLibrarySource(eo.getLibrarySource());
		eoCloned.setLibrarySourceObject(eo.getLibrarySourceObject());
		eoCloned.setPathDocFile(eo.getPathDocFile());
		eoCloned.setSuffixFileSource(eo.getSuffixFileSource());
		eoCloned.setSystemOwner(eo.getSystemOwner());
		eoCloned.setSubSystemOwner(eo.getSubSystemOwner());
		
		return eoCloned;
	}


}
