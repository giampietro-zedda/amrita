
package analyzer;
import java.io.Serializable;

import enums.EnumDataItemType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlDataType
 * </h1>
 * <p>
 * Viene modellato un data type sql, predefinito o custom.<br>
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 20/lug/2011 
 *
*/

public class SqlDataType implements Serializable {

 	private static final long serialVersionUID = 1L;

 	private EnumDataItemType type = null; 			// Tipologia data type
    private boolean isCustom = true;                // true indica un data type custom personalizzato individuato da un nome
    private String customDataTypeName = "";         // Nome per il data type custom
	private int digit = 0;                          // Numero di digit
	private int scale = 0;                          // Numero di decimali (di cui)
	private int size = 0;                      		// Dimensioni in bytes del data type
	private String sizeUnit = "";                   // C=char, K=Kilo char M=Mega char, G=Giga char
	private int ccsidNum = 0;                       // CCSID integer
	
	// Opzioni presenti nel data type
	private boolean isCcsid = false;                // CCSID
	private boolean isAscii = false;                // ASCII
	private boolean isEbcdic = false;               // EBCDIC
	private boolean isUnicode = false;              // EBCDIC
	private boolean isForSbcsData = false;          // FOR SBCS DATA
	private boolean isForMixedData = false;         // FOR MIXED DATA
	private boolean isForBitData = false;         	// FOR BIT DATA
	private boolean isWithoutTimeZone = false;      // WITHOUT TIME ZONE
	private boolean isWithTimeZone = false;      	// WITH TIME ZONE
	private boolean isAfterSizeCodeunits16 = false; // CHAR VARYING (n CODEUNITS16)
	private boolean isAfterSizeCodeunits32 = false; // CHAR VARYING (n CODEUNITS32)
	private boolean isAfterSizeOctets = false; 		// CHAR VARYING (200 OCTETS)
	
	/**
	 * Costruttore 
	 */
	public SqlDataType() {
		super();
	}

	/**
	 * Restituisce il tipo di dato sql.<br>
	 * <p>
	 * @return the type
	 */
	public EnumDataItemType getType() {
		return type;
	}

	/**
	 * Imposta il tipo di dato sql.<br>
	 * <p>
	 * @param type the type to set
	 */
	public void setType(EnumDataItemType type) {
		this.type = type;
	}

	/**
	 * Restituisce se il tipo di dato sql è custom.<br>
	 * <p>
	 * @return the isCustom
	 */
	public boolean isCustom() {
		return isCustom;
	}

	/**
	 * Imposta se il tipo di dato sql è custom.<br>
	 * <p>
	 * @param isCustom the isCustom to set
	 */
	public void setCustom(boolean isCustom) {
		this.isCustom = isCustom;
	}

	/**
	 * Restituisce il nome assegnato al tipo di dato custom.<br>
	 * <p>
	 * @return the customDataTypeName
	 */
	public String getCustomDataTypeName() {
		return customDataTypeName;
	}

	/**
	 * Imposta il nome assegnato al tipo di dato custom.<br>
	 * <p>
	 * @param customDataTypeName the customDataTypeName to set
	 */
	public void setCustomDataTypeName(String customDataTypeName) {
		this.customDataTypeName = customDataTypeName;
	}

	/**
	 * Restituisce il numero totale di digit di un data type <tt>DECIMAL</tt>.<br>
	 * <p>
	 * @return the digit
	 */
	public int getDigit() {
		return digit;
	}

	/**
	 * Imposta il numero totale di digit di un data type <tt>DECIMAL</tt>.<br>
	 * <p>
	 * @param digit the digit to set
	 */
	public void setDigit(int digit) {
		this.digit = digit;
	}

	/**
	 * Restituisce il numero di digit decimali, un di cui del numero totale di digit, di un data type <tt>DECIMAL</tt>.<br>
	 * <p>
	 * @return the scale
	 */
	public int getScale() {
		return scale;
	}

	/**
	 * Imposta il numero di digit decimali, un di cui del numero totale di digit, di un data type <tt>DECIMAL</tt>.<br>
	 * <p>
	 * @param scale the scale to set
	 */
	public void setScale(int scale) {
		this.scale = scale;
	}

	/**
	 * Restituisce le dimensioni, o le dimensioni massime in bytes del data type.<br>
	 * <p>
	 * @return the size
	 */
	public int getSize() {
		return size;
	}

	/**
	 * Imposta le dimensioni, o le dimensioni massime in bytes del data type.<br>
	 * <p>
	 * @param size the size to set
	 */
	public void setSize(int size) {
		this.size = size;
	}

	/**
	 * Restituisce in quale tipo di unità devono essere considerate le dimensioni del campo.<br>
	 * <p>
	 * <ul>
	 * <li><tt>C</tt> se bytes</li>
	 * <li><tt>K</tt> se Kilo bytes</li>
	 * <li><tt>M</tt> se Mega bytes</li>
	 * <li><tt>G</tt> se Giga bytes</li><
	 * <p>
	 * 
	 * @return the sizeUnit
	 */
	public String getSizeUnit() {
		return sizeUnit;
	}

	/**
	 * Imposta in quale tipo di unità devono essere considerate le dimensioni del campo.<br>
	 * <p>
	 * <ul>
	 * <li><tt>C</tt> se bytes</li>
	 * <li><tt>K</tt> se Kilo bytes</li>
	 * <li><tt>M</tt> se Mega bytes</li>
	 * <li><tt>G</tt> se Giga bytes</li><
	 * <p>
	 * 
	 * @param sizeUnit the sizeUnit to set
	 */
	public void setSizeUnit(String sizeUnit) {
		this.sizeUnit = sizeUnit;
	}

	/**
	 * Restituisce il numero dopo l'opzione CCSID.<br>
	 * <p>
	 * 
	 * @return the ccsidNum
	 */
	public int getCcsidNum() {
		return ccsidNum;
	}

	/**
	 * Imposta il numero dopo l'opzione CCSID.<br>
	 * <p>
	 * 
	 * @param ccsidNum the ccsidNum to set
	 */
	public void setCcsidNum(int ccsidNum) {
		this.ccsidNum = ccsidNum;
	}

	/**
	 * Restituisce se presente l'opzione CCSID.<br>
	 * <p>
	 * 
	 * @return the isCcsid
	 */
	public boolean isCcsid() {
		return isCcsid;
	}

	/**
	 * Imposta se presente l'opzione CCSID.<br>
	 * <p>
	 * @param isCcsid the isCcsid to set
	 */
	public void setCcsid(boolean isCcsid) {
		this.isCcsid = isCcsid;
	}

	/**
	 * Restituisce se presente l'opzione ASCII.<br>
	 * <p>
	 * @return the isAscii
	 */
	public boolean isAscii() {
		return isAscii;
	}

	/**
	 * Imposta se presente l'opzione ASCII.<br>
	 * <p>
	 * @param isAscii the isAscii to set
	 */
	public void setAscii(boolean isAscii) {
		this.isAscii = isAscii;
	}

	/**
	 * Restituisce se presente l'opzione EBCDIC.<br>
	 * <p>
	 * @return the isEbcdic
	 */
	public boolean isEbcdic() {
		return isEbcdic;
	}

	/**
	 * Imposta se presente l'opzione EBCDIC.<br>
	 * <p>
	 * @param isEbcdic the isEbcdic to set
	 */
	public void setEbcdic(boolean isEbcdic) {
		this.isEbcdic = isEbcdic;
	}

	/**
	 * Restituisce se presente l'opzione UNICODE.<br>
	 * <p>
	 * @return the isUnicode
	 */
	public boolean isUnicode() {
		return isUnicode;
	}

	/**
	 * Imposta se presente l'opzione UNICODE.<br>
	 * <p>
	 * @param isUnicode the isUnicode to set
	 */
	public void setUnicode(boolean isUnicode) {
		this.isUnicode = isUnicode;
	}

	/**
	 * Restituisce se presente l'opzione FOR SBCS DATA.<br>
	 * <p>
	 * @return the isForSbcsData
	 */
	public boolean isForSbcsData() {
		return isForSbcsData;
	}

	/**
	 * Imposta se presente l'opzione FOR SBCS DATA.<br>
	 * <p>
	 * @param isForSbcsData the isForSbcsData to set
	 */
	public void setForSbcsData(boolean isForSbcsData) {
		this.isForSbcsData = isForSbcsData;
	}

	/**
	 * Restituisce se presente l'opzione FOR MIXED DATA.<br>
	 * <p>
	 * @return the isForMixedData
	 */
	public boolean isForMixedData() {
		return isForMixedData;
	}

	/**
	 * Imposta se presente l'opzione FOR MIXED DATA.<br>
	 * <p>
	 * @param isForMixedData the isForMixedData to set
	 */
	public void setForMixedData(boolean isForMixedData) {
		this.isForMixedData = isForMixedData;
	}

	/**
	 * Restituisce se presente l'opzione FOR BIT DATA.<br>
	 * <p>
	 * @return the isForBitData
	 */
	public boolean isForBitData() {
		return isForBitData;
	}

	/**
	 * Imposta se presente l'opzione FOR BIT DATA.<br>
	 * <p>
	 * @param isForBitData the isForBitData to set
	 */
	public void setForBitData(boolean isForBitData) {
		this.isForBitData = isForBitData;
	}

	/**
	 * Restituisce se presente l'opzione WITHOUT TIME ZONE<br>
	 * <p>
	 * @return the isWithoutTimeZone
	 */
	public boolean isWithoutTimeZone() {
		return isWithoutTimeZone;
	}

	/**
	 * Imposta se presente l'opzione WITHOUT TIME ZONE<br>
	 * <p>
	 * @param isWithoutTimeZone the isWithoutTimeZone to set
	 */
	public void setWithoutTimeZone(boolean isWithoutTimeZone) {
		this.isWithoutTimeZone = isWithoutTimeZone;
	}

	/**
	 * Restituisce se presente l'opzione WITH TIME ZONE<br>
	 * <p>
	 * @return the isWithTimeZone
	 */
	public boolean isWithTimeZone() {
		return isWithTimeZone;
	}

	/**
	 * Imposta se presente l'opzione WITH TIME ZONE<br>
	 * <p>
	 * @param isWithTimeZone the isWithTimeZone to set
	 */
	public void setWithTimeZone(boolean isWithTimeZone) {
		this.isWithTimeZone = isWithTimeZone;
	}

	/**
	 * Restituisce se presente CODEUNITS16 dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @return the isAfterSizeCodeunits16
	 */
	public boolean isAfterSizeCodeunits16() {
		return isAfterSizeCodeunits16;
	}

	/**
	 * Imposta se presente CODEUNITS16 dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @param isAfterSizeCodeunits16 the isAfterSizeCodeunits16 to set
	 */
	public void setAfterSizeCodeunits16(boolean isAfterSizeCodeunits16) {
		this.isAfterSizeCodeunits16 = isAfterSizeCodeunits16;
	}

	/**
	 * Restituisce se presente CODEUNITS32 dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @return the isAfterSizeCodeunits32
	 */
	public boolean isAfterSizeCodeunits32() {
		return isAfterSizeCodeunits32;
	}

	/**
	 * Imposta se presente CODEUNITS32 dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @param isAfterSizeCodeunits32 the isAfterSizeCodeunits32 to set
	 */
	public void setAfterSizeCodeunits32(boolean isAfterSizeCodeunits32) {
		this.isAfterSizeCodeunits32 = isAfterSizeCodeunits32;
	}

	/**
	 * Restituisce se presente OCTETS dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @return the isAfterSizeOctets
	 */
	public boolean isAfterSizeOctets() {
		return isAfterSizeOctets;
	}

	/**
	 * Imposta se presente OCTETS dopo la lunghezza di var e varchar<br>
	 * <p>
	 * @param isAfterSizeOctets the isAfterSizeOctets to set
	 */
	public void setAfterSizeOctets(boolean isAfterSizeOctets) {
		this.isAfterSizeOctets = isAfterSizeOctets;
	}


	
}
