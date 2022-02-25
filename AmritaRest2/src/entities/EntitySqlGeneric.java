package entities;

import javax.persistence.Column;
import javax.persistence.Entity;

import enums.EnumObject;
import enums.EnumObjectStatus;
import enums.EnumRelation;


/**
	 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntitySqlGeneric  
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity generico, usato con generiche istruzioni SQL di ti po SELECT o altro.
	 * In caso di SELECT vengono utilizzate le colonne definite, senza indicare nessuna tabella di riferimento.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 17/11/2020

*/


@Entity(name="NotUsed")
public class EntitySqlGeneric implements Cloneable {

	///////////////////////////////////////////////////////////////////////
    // Data Items                                                        //                                                        
    ///////////////////////////////////////////////////////////////////////
		
	// Data 
    @Column(name="sys")    	  	private String sys = "";            				 
    @Column(name="subSystem")	  	private String subSystem = ""; 
    @Column(name="subSysOwner")	private String subSysOwner = ""; 
    @Column(name="pgm") 	  	private String pgm = "";  
    @Column(name="idObject") 	private String idObject = "";  
    @Column(name="idObjectDescriptor") private String idObjectDescriptor = "";   			 									
    @Column(name="statusObject") private EnumObjectStatus statusObject = null;   			 									
    @Column(name="entity") 	  	private String entity = "";  
    @Column(name="relation")  	private EnumRelation relation = null;   			 									
    @Column(name="typeObjectA") private EnumObject typeObjectA = null;   			 									
    @Column(name="typeObjectAOrdinal") private int typeObjectAOrdinal = 0;   			 									
    @Column(name="typeObjectB") private EnumObject typeObjectB = null;      
    @Column(name="cnt")         private int cnt = 0;      
		
    /*
	 * Costruttore 
	 */
	public EntitySqlGeneric() {
		super();
		relation = EnumRelation.NOT_ASSIGNED;
		typeObjectA = EnumObject.NOT_ASSIGNED;
		typeObjectB = EnumObject.NOT_ASSIGNED;
		statusObject = EnumObjectStatus.NOT_ASSIGNED;
	}

	/**
	 * @return the sys
	 */
	public String getSys() {
		return sys;
	}

	/**
	 * @param sys the sys to set
	 */
	public void setSys(String sys) {
		this.sys = sys;
	}

	/**
	 * @return the subSystem
	 */
	public String getSubSys() {
		return subSystem;
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
	 * @return the pgm
	 */
	public String getPgm() {
		return pgm;
	}

	/**
	 * @param pgm the pgm to set
	 */
	public void setPgm(String pgm) {
		this.pgm = pgm;
	}

	/**
	 * @return the entity
	 */
	public String getEntity() {
		return entity;
	}

	/**
	 * @param entity the entity to set
	 */
	public void setEntity(String entity) {
		this.entity = entity;
	}

	/**
	 * @return the relation
	 */
	public EnumRelation getRelation() {
		return relation;
	}

	/**
	 * @param relation the relation to set
	 */
	public void setRelation(EnumRelation relation) {
		this.relation = relation;
	}

	/**
	 * @return the typeObjectA
	 */
	public EnumObject getTypeObjectA() {
		return typeObjectA;
	}

	/**
	 * @param typeObjectA the typeObjectA to set
	 */
	public void setTypeObjectA(EnumObject typeObjectA) {
		this.typeObjectA = typeObjectA;
	}

	/**
	 * @return the typeObjectB
	 */
	public EnumObject getTypeObjectB() {
		return typeObjectB;
	}

	/**
	 * @param typeObjectB the typeObjectB to set
	 */
	public void setTypeObjectB(EnumObject typeObjectB) {
		this.typeObjectB = typeObjectB;
	}

	/**
	 * @return the subSysOwner
	 */
	public String getSubSysOwner() {
		return subSysOwner;
	}

	/**
	 * @param subSysOwner the subSysOwner to set
	 */
	public void setSubSysOwner(String subSysOwner) {
		this.subSysOwner = subSysOwner;
	}

	/**
	 * @return the statusObject
	 */
	public EnumObjectStatus getStatusObject() {
		return statusObject;
	}

	/**
	 * @param statusObject the statusObject to set
	 */
	public void setStatusObject(EnumObjectStatus statusObject) {
		this.statusObject = statusObject;
	}

	/**
	 * @return the cnt
	 */
	public int getCnt() {
		return cnt;
	}

	/**
	 * @param cnt the cnt to set
	 */
	public void setCnt(int cnt) {
		this.cnt = cnt;
	}

	/**
	 * @return the typeObjectAOrdinal
	 */
	public int getTypeObjectAOrdinal() {
		return typeObjectA.ordinal();
	}


}
