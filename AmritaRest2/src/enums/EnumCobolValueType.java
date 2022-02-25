package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumCobolValueType	
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie generali di Value di data item Cobol
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/04/2010
  * @see EnumCobolUsage
   *  
*/
public enum EnumCobolValueType {
	
	NOT_ASSIGNED, 				 	   		// (00) Di servizio   
	
	VALUE_LITERAL_ALPHA,        			// (01) Costante alfanumerica del tipo 'VV' o "VV"
	VALUE_LITERAL_ALPHA_ALL,    			// (02) Sequenza alfanumerica del tipo, per esempio, ALL '0'
	VALUE_LITERAL_ALPHA_NULL_TERM,	        // (03) Sequenza alfanumerica del tipo Z'kk' o Z"JJ"
	VALUE_LITERAL_ALPHA_NATIONAL, 			// (04) Sequenza alfanumerica del tipo N'kk' o N"JJ"
	VALUE_LITERAL_ALPHA_DBCS,        	    // (05) Costante alfanumerica DBCS
	VALUE_LITERAL_HEX,        			    // (06) Costante esadecimale  del tipo X'FFFF' o X"FFFF"
	VALUE_LITERAL_HEX_NATIONAL,        	    // (07) Costante esadecimale  del tipo NX'FFFF' o NX"FFFF"
	VALUE_LITERAL_NUM_INT,             		// (08) Costante numerica intera
	VALUE_LITERAL_NUM_LONG,                 // (09) Costante numerica intera
	VALUE_LITERAL_NUM_WITH_COMMA,           // (10) Costante numerica con virgola
	VALUE_LITERAL_NUM_FLOATING,    			// (11) Costante numerica floating point come -0.5E-10
	VALUE_FIGURATIVE;         				// (12) Costante figurativa come SPACE, ZERO, ..
}