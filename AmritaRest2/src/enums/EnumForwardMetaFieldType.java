package enums;

import forward.ForwardFunction;
import analyzer.DataBaseMappedEnumeration;

/**
  * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumForwardMetaFieldType
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di un metadato forward.<br>
  * Si tratta di definizioni generiche non ancora calate a livello applicativo.<br>
  * <p>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/11/2011
  * @see ForwardFunction
  * @see EnumForwardFunctionModel
  */
@DataBaseMappedEnumeration
public enum EnumForwardMetaFieldType {
	
	NOT_ASSIGNED,           	// 00 Di servizio 
	
	NUMERIC_INTEGER,     		// 01 Informazione numerica intera
	NUMERIC_COMMA,     			// 01 Informazione numerica con virgola
	CHAR_ALPHABETIC,			// 03 Informazione alfanumerica testuale
	CHAR_ALPHANUMERIC,	        // 04 Informazione alfanumerica testuale
	DATE,           			// 05 Data
	TIME,           			// 06 Ora
	IMAGE,           			// 07 Immagine grafica
	SOURCE,           			// 08 Sorgente formato da n righe testuali
	DOCUMENT;           		// 09 Documentazione (HTML, doc, link, ..)
}












