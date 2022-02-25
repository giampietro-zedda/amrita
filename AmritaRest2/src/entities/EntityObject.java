package entities;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import utilities.DateTimeService;
import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumSourceType;

/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityObject (Object, OBJT) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityObject, ovvero la tabella oggetti OBJT.
	 * Ogni oggetto analizzato viene inserito in questa tabella.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObject 
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

@Entity(name="Object")
public class EntityObject implements Cloneable {

	///////////////////////////////////////////////////////////////////////
    // Data Items Object                                                 //                                                        
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            			// Sistema applicativo
	@Id	
    @Column(name="subSys")
	private String subSystem = "";         			// Sotto sistema applicativo
	@Id
    @Column(name="idObject")
	private String idObject = "";          			// Nome oggetto (es. programma, file fisico,..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       	// Tipologia oggetto (T0001)
	@Id
    @Column(name="typeSource")
	private EnumSourceType typeSource; 				// Tipologia sorgente (T0002)			
	
	// Data
 // @Column(name="idObjectSource")
	private String idObjectSource = "";       	    // Nome oggetto sorgente da cui questo oggetto è derivato (es. OBJECT_MAP)
    @Column(name="idObjectExtended")
	private String idObjectExtended = "";       	// Nome oggetto esteso (Phisical file, User tag,..) 
    @Column(name="librarySourceObject")
	private String librarySourceObject = "";  		// Nome oggetto LIBRARY libreria sorgente 
    @Column(name="libraryDir")
	private String libraryDir = "";  		    	// Directory libreria per oggetti LIBRARY
    @Column(name="librarySource")
	private String librarySource = "";  			// Libreria sorgente di analisi (Per oggetti direttamente dedotti da un file sorgente)
    @Column(name="fileSource")
	private String fileSource = "";     			// Nome file sorgente di analisi 
    @Column(name="suffixFileSource")
	private String suffixFileSource = "";     	    // Suffisso file sorgente di analisi dopo .
    @Column(name="statusObject")
	private EnumObjectStatus status;            	// Stato oggetto (T0003)
    @Column(name="author")
	private String author = "";				    	// Autore oggetto analizzato
    @Column(name="idObjectDescriptor")
	private String idObjectDescriptor = "";     	// Nome oggetto descrittore o descrizione sottosistema per oggetto SUBSYS. Per esempio un copy per i file
    @Column(name="typeObjectDescriptor")
	private EnumObject typeObjectDescriptor = null; // Tipologia oggetto descrittore (T0001)	
    @Column(name="sysOwner")
	private String systemOwner = "";       			// Sistema applicativo proprietario
    @Column(name="subSysOwner")
	private String subSystemOwner = "";    			// Sotto sistema applicativo proprietario
    @Column(name="dateWritten")
	private String dateWritten = "";				// Data scrittura (es. da date-written Cobol)
    @Column(name="pathDocFile")
	private String pathDocFile = "";				// Path completo file documentazione
    @Column(name="dtFirstAnalysis")
	private String dtFirstAnalysis = "";	    	// Data prima analisi AAAAMMGG
    @Column(name="tmFirstAnalysis")
	private String tmFirstAnalysis = "";	    	// Ora  prima analisi HHMMSSCC	
    @Column(name="dtLastAnalysis")
	private String dtLastAnalysis = "";	        	// Data ultima analisi AAAAMMGG
    @Column(name="tmLastAnalysis")
	private String tmLastAnalysis = "";	        	// Ora  ultima analisi HHMMSSCC	
	
	
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityObject() {
		super();
		
		typeObject = EnumObject.NOT_ASSIGNED;
		typeObjectDescriptor = EnumObject.NOT_ASSIGNED;
		typeSource = EnumSourceType.NOT_ASSIGNED;
		status = EnumObjectStatus.OBJECT_TO_BE_ANALYZED;
		
		this.dtFirstAnalysis = DateTimeService.getDateFormatted(new Date(), "yyyyMMdd");
		this.tmFirstAnalysis = DateTimeService.getTimeFormatted(new Date(), "hhmmss") + "00";
		this.dtLastAnalysis = this.dtFirstAnalysis;
		this.tmLastAnalysis = this.tmFirstAnalysis;
		

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
	 * @return the idObjectDescriptor
	 */
	public String getIdObjectDescriptor() {
		return idObjectDescriptor;
	}


	/**
	 * @param idObjectDescriptor the idObjectDescriptor to set
	 */
	public void setIdObjectDescriptor(String idObjectDescriptor) {
		this.idObjectDescriptor = idObjectDescriptor;
	}


	/**
	 * @return the typeObjectDescriptor
	 */
	public EnumObject getTypeObjectDescriptor() {
		return typeObjectDescriptor;
	}


	/**
	 * @param typeObjectDescriptor the typeObjectDescriptor to set
	 */
	public void setTypeObjectDescriptor(EnumObject typeObjectDescriptor) {
		this.typeObjectDescriptor = typeObjectDescriptor;
	}


	/**
	 * @return the status
	 */
	public EnumObjectStatus getStatus() {
		return status;
	}
	/**
	 * @param status the status to set
	 */
	public void setStatus(EnumObjectStatus status) {
		this.status = status;
	}
	/**
	 * @return the author
	 */
	public String getAuthor() {
		return author;
	}
	/**
	 * @param author the author to set
	 */
	public void setAuthor(String author) {
		this.author = author;
	}
	/**
	 * @return the systemOwner
	 */
	public String getSystemOwner() {
		return systemOwner;
	}
	/**
	 * @param systemOwner the systemOwner to set
	 */
	public void setSystemOwner(String systemOwner) {
		this.systemOwner = systemOwner;
	}
	/**
	 * @return the subSystemOwner
	 */
	public String getSubSystemOwner() {
		return subSystemOwner;
	}
	/**
	 * @param subSystemOwner the subSystemOwner to set
	 */
	public void setSubSystemOwner(String subSystemOwner) {
		this.subSystemOwner = subSystemOwner;
	}
	/**
	 * @return the dateWritten
	 */
	public String getDateWritten() {
		return dateWritten;
	}
	/**
	 * @param dateWritten the dateWritten to set
	 */
	public void setDateWritten(String dateWritten) {
		this.dateWritten = dateWritten;
	}
	/**
	 * @return the pathDocFile
	 */
	public String getPathDocFile() {
		return pathDocFile;
	}
	/**
	 * @param pathDocFile the pathDocFile to set
	 */
	public void setPathDocFile(String pathDocFile) {
		this.pathDocFile = pathDocFile;
	}

	
	/**
	 * @return the fileSource
	 */
	public String getFileSource() {
		return fileSource;
	}
	/**
	 * @param fileSource the fileSource to set
	 */
	public void setFileSource(String fileSource) {
		this.fileSource = fileSource;
	}

	
	/**
	 * @return the suffixFileSource
	 */
	public String getSuffixFileSource() {
		return this.suffixFileSource;
	}


	/**
	 * @param suffixFileSource the suffixFileSource to set
	 */
	public void setSuffixFileSource(String suffixFileSource) {
		this.suffixFileSource = suffixFileSource;
	}


	/**
	 * @return the libraryDir
	 */
	public String getLibraryDir() {
		return libraryDir;
	}


	/**
	 * @param libraryDir the libraryDir to set
	 */
	public void setLibraryDir(String libraryDir) {
		this.libraryDir = libraryDir;
	}


	/**
	 * @return the librarySource
	 */
	public String getLibrarySource() {
		return librarySource;
	}


	/**
	 * @param librarySource the librarySource to set
	 */
	public void setLibrarySource(String librarySource) {
		this.librarySource = librarySource;
	}


	/**
	 * @return the idObjectExtended
	 */
	public String getIdObjectExtended() {
		return idObjectExtended;
	}
	/**
	 * @param idObjectExtended the idObjectExtended to set
	 */
	public void setIdObjectExtended(String idObjectExtended) {
		this.idObjectExtended = idObjectExtended;
	}
	
	/**
	 * @return the idObjectSource
	 */
	public String getIdObjectSource() {
		return idObjectSource;
	}


	/**
	 * @param idObjectSource the idObjectSource to set
	 */
	public void setIdObjectSource(String idObjectSource) {
		this.idObjectSource = idObjectSource;
	}


	/**
	 * @return the librarySourceObject
	 */
	public String getLibrarySourceObject() {
		return librarySourceObject;
	}
	/**
	 * @param librarySourceObject the librarySourceObject to set
	 */
	public void setLibrarySourceObject(String librarySourceObject) {
		this.librarySourceObject = librarySourceObject;
	}
	/**
	 * @return the dtFirstAnalysis
	 */
	public String getDtFirstAnalysis() {
		return dtFirstAnalysis;
	}
	/**
	 * @param dtFirstAnalysis the dtFirstAnalysis to set
	 */
	public void setDtFirstAnalysis(String dtFirstAnalysis) {
		this.dtFirstAnalysis = dtFirstAnalysis;
	}
	/**
	 * @return the tmFirstAnalysis
	 */
	public String getTmFirstAnalysis() {
		return tmFirstAnalysis;
	}
	/**
	 * @param tmFirstAnalysis the tmFirstAnalysis to set
	 */
	public void setTmFirstAnalysis(String tmFirstAnalysis) {
		this.tmFirstAnalysis = tmFirstAnalysis;
	}
	/**
	 * @return the dtLastAnalysis
	 */
	public String getDtLastAnalysis() {
		return dtLastAnalysis;
	}
	/**
	 * @param dtLastAnalysis the dtLastAnalysis to set
	 */
	public void setDtLastAnalysis(String dtLastAnalysis) {
		this.dtLastAnalysis = dtLastAnalysis;
	}
	/**
	 * @return the tmLastAnalysis
	 */
	public String getTmLastAnalysis() {
		return tmLastAnalysis;
	}
	/**
	 * @param tmLastAnalysis the tmLastAnalysis to set
	 */
	public void setTmLastAnalysis(String tmLastAnalysis) {
		this.tmLastAnalysis = tmLastAnalysis;
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  typeObject.toString() + " IdObject:" + idObject;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityObject objEntityObject = null;
		
		objEntityObject = (EntityObject) obj;
		 
		return this.system.equals(objEntityObject.system)
		 &&    this.subSystem.equals(objEntityObject.subSystem)
		 &&    this.idObject.equals(objEntityObject.idObject)
		 &&    this.typeObject == objEntityObject.typeObject;
		 
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public EntityObject clone()  {
		EntityObject o = null;
		 try {
			o = (EntityObject) super.clone();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return o;
	}





}
