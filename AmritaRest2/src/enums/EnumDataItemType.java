package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumDataItemType
  * </h1>
  *  <p>
  * Questa enum elenca le possibili tipologie di un item di dati, nei vari ambienti.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * @see EnumDataItemGeneric
  * @see EnumDataItemFigurative
  * @see EnumDataItemType 
  * @see EnumDataItemTypeSql
*/
@DataBaseMappedEnumeration
public enum EnumDataItemType {
	
	NOT_ASSIGNED,           									// 00 Di servizio 
	
	// Cobol
	COBOL_GROUP,           										// 01 Gruppo
	COBOL_DISPLAY,         										// 02 Alfanumerico
	COBOL_PACKED,          										// 03 Packed
	COBOL_ZONED,           										// 04 Zoned
	COBOL_FLOATING,        										// 05 Floating point
	COBOL_BINARY,          										// 06 Binary
	COBOL_POINTER,         										// 07 Pointer
	COBOL_INDEX,          										// 08 Index
	COBOL_CONDITION_NAME,  										// 09 Livello 88
    
	// Sql DB2 data types
	SQL_SMALLINT("SMALLINT"), 									// 10
	SQL_INTEGER("INT|INTEGER",""),								// 11
	SQL_BIGINT("BIGINT",""),						    		// 12
	SQL_DECIMAL("DEC|DECIMAL|NUMERIC"), 			    		// 13
	SQL_FLOAT("FLOAT"),				        					// 14
	SQL_REAL("REAL"),				        					// 15
	SQL_DOUBLE("DOUBLE"),				        				// 16
	SQL_DECFLOAT("DECFLOAT"),				            		// 17
	SQL_CHAR("CHAR|CHARACTER"), 								// 18
	SQL_VARCHAR("VARCHAR|LONG VARCHAR"), 			    		// 19
	SQL_GRAPHIC("GRAPHIC"), 									// 20
	SQL_VARGRAPHIC("VARGRAPHIC|LONG VARGRAPHIC"), 			    // 21
	SQL_BINARY("BINARY"), 				    					// 22
	SQL_VARBINARY("VARBYNARY|BYNARY VARYING"), 				    // 23
	SQL_CLOB("CLOB|CHAR LARGE OBJECT|CHARACTER LARGE OBJECT"),	// 24
	SQL_BLOB("BLOB|BINARY LARGE OBJECT"), 					    // 25
	SQL_DBCLOB("DCLOB"), 				    					// 27
	SQL_XML, 													// 27
	SQL_DATE, 													// 28
	SQL_TIME, 													// 29
	SQL_TIMESTAMP, 												// 30
	SQL_ROWID; 										    		// 31

	
	//////////////////////////////////////////////////////////////////////////////////////
	// Variabili di istanza per ogni entry
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	String valueText1 = "";							// Valore testuale per il parametro
	String valueText2 = "";							// Valore testuale  per il parametro, alternativo
	String valueText3 = "";							// Valore testuale  per il parametro, alternativo
	EnumInstrDataCategory instrCategory = null;     // Categoria istruzione jcl
	
	/*
     * Costruttore vuoto
     */
	EnumDataItemType(){
	}

	
	/*
     * Costruttore con 1 parametro
     */
	EnumDataItemType(String  valueText1){
		this.valueText1 = valueText1;		
	}

	/*
     * Costruttore con 2 parametri
     */
	EnumDataItemType(String  valueText1, String  valueText2){
		this.valueText1 = valueText1;		
		this.valueText2 = valueText2;		
	}

	/*
     * Costruttore con 3 parametri
     */
	EnumDataItemType(String  valueText1, String  valueText2, String  valueText3){
		this.valueText1 = valueText1;		
		this.valueText2 = valueText2;		
		this.valueText3= valueText3;		
	}


	/**
	 * Restituisce la stringa identificativa dell'istruzione jcl.<br>
	 * 
	 * @return the valueText1
	 */
	public String getValueText1() {
		return valueText1;
	}

	/**
	 * Restituisce la stringa identificativa dell'istruzione jcl.<br>
	 * 
	 * @return the valueText1
	 */
	public String getValueText2() {
		return valueText2;
	}

	/**
	 * Restituisce la stringa identificativa dell'istruzione jcl.<br>
	 * 
	 * @return the valueText1
	 */
	public String getValueText3() {
		return valueText3;
	}


}












