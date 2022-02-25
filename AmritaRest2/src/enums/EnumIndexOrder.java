package enums;

import analyzer.DataBaseMappedEnumeration;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumIndexType
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di ordinamento di un indice.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 22/02/2010
  * @see Analyzer
  * 
*/
@DataBaseMappedEnumeration
public enum EnumIndexOrder {
	
	NOT_ASSIGNED,        		// 00 Di servizio 
	
	ORDER_ASCENDING,   			// 01 Ordinamento ascendente
	ORDER_DESCENDING,   		// 02 Ordinamento decrescente
	ORDER_RANDOM   				// 03 Ordinamento random (indici DB2)
}
