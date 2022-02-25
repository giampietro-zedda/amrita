
package analyzer;

import java.io.Serializable;
import enums.EnumPrecompilerReservedWords;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlPeriodSpecification
 * </h1>
 * <p>
 * Descrive le informazioni di period-specification presenti nella clausola FROM delle istruzioni sql dove prevista.<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlSelectStatement
 * @see SqlFullSelect
 * @see SqlSubselectSelectInto
 */

public class SqlPeriodSpecification implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private EnumPrecompilerReservedWords typePeriod = null;             // FOR SYSTEM_TIME|BUSINESS_TIME
	private boolean isAsOf = false;                                     // FOR SYSTEM_TIME|BUSINESS_TIME AS OF value
	private boolean isFrom = false;                                     // FOR SYSTEM_TIME|BUSINESS_TIME FROM value1 TO value2
	private boolean isBetween = false;                                  // FOR SYSTEM_TIME|BUSINESS_TIME BETWEEN value1 AND value2
	private String value = "";											// FOR SYSTEM_TIME|BUSINESS_TIME AS OF value
	private String value1 = "";											// FOR SYSTEM_TIME|BUSINESS_TIME FROM value1 TO value2
	private String value2 = "";											// FOR SYSTEM_TIME|BUSINESS_TIME FROM value1 TO value2
	
    
	/**
	 * Costruttore vuoto
	 */
	public SqlPeriodSpecification() {
		super();
		typePeriod = EnumPrecompilerReservedWords.NOT_ASSIGNED;
	}


	/**
	 * Restituisce il tipo di periodo, che può essere <tt>SYSTEM_TIME</tt> o <tt>BUSINESS_TIME</tt><br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * <li>FOR BUSINESS_TIME FROM value1 TO value2</li>
	 * </ul>
	 * 
	 * @return the typePeriod
	 */
	public EnumPrecompilerReservedWords getTypePeriod() {
		return typePeriod;
	}


	/**
	 * Imposta il tipo di periodo, che può essere <tt>SYSTEM_TIME</tt> o <tt>BUSINESS_TIME</tt><br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * <li>FOR BUSINESS_TIME FROM value1 TO value2</li>
	 * </ul>
	 * 
	 * @param typePeriod the typePeriod to set
	 */
	public void setTypePeriod(EnumPrecompilerReservedWords typePeriod) {
		this.typePeriod = typePeriod;
	}


	/**
	 * Restituisce se il periodo è codificato con la clausola <tt>AS OF</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * </ul>
	 * 
	 * @return the isAsOf
	 */
	public boolean isAsOf() {
		return isAsOf;
	}


	/**
	 * Imposta se il periodo è codificato con la clausola <tt>AS OF</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * </ul>
	 * 
	 * @param isAsOf the isAsOf to set
	 */
	public void setAsOf(boolean isAsOf) {
		this.isAsOf = isAsOf;
	}


	/**
	 * Restituisce se il periodo è codificato con la clausola <tt>FROM</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * </ul>
	 * 
	 * @return the isFrom
	 */
	public boolean isFrom() {
		return isFrom;
	}


	/**
	 * Imposta se il periodo è codificato con la clausola <tt>FROM</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * </ul>
	 * 
	 * @param isFrom the isFrom to set
	 */
	public void setFrom(boolean isFrom) {
		this.isFrom = isFrom;
	}


	/**
	 * Restituisce se il periodo è codificato con la clausola <tt>BETWEEN</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * @return the isBetween
	 */
	public boolean isBetween() {
		return isBetween;
	}


	/**
	 * Imposta se il periodo è codificato con la clausola <tt>BETWEEN</tt> <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * 
	 * @param isBetween the isBetween to set
	 */
	public void setBetween(boolean isBetween) {
		this.isBetween = isBetween;
	}


	/**
	 * Restituisce il valore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * </ul>
	 * 
	 * @return the value
	 */
	public String getValue() {
		return value;
	}


	/**
	 * Imposta il valore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME AS OF value </li>
	 * </ul>
	 * 
	 * @param value the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}


	/**
	 * Restituisce il valore inferiore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * 
	 * @return the value1
	 */
	public String getValue1() {
		return value1;
	}


	/**
	 * Imposta il valore inferiore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * 
	 * @param value1 the value1 to set
	 */
	public void setValue1(String value1) {
		this.value1 = value1;
	}


	/**
	 * Restituisce il valore superiore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * 
	 * @return the value2
	 */
	public String getValue2() {
		return value2;
	}


	/**
	 * Imposta il valore superiore del periodo <br>
	 * <p>
	 * <ul>
	 * <li>FOR SYSTEM_TIME FROM value1 TO value2 </li>
	 * <li>FOR SYSTEM_TIME BETWEEN value1 AND value2 </li>
	 * </ul>
	 * 
	 * @param value2 the value2 to set
	 */
	public void setValue2(String value2) {
		this.value2 = value2;
	}


	
}
