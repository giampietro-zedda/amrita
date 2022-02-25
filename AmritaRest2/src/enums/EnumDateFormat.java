/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  *  Questa enumerazione elenca i possibili formati di una data.
  *  Utilizzata nella gestione generalizzata tabelle.
  * 
 */
package enums;

import analyzer.DataBaseMappedEnumeration;
import entities.EntityTableData;
import entities.EntityTableHeader;
import entities.EntityTableStructure;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumDateFormat
 * </h1>
 *  <p>
 * Questa enum elenca i tipi di assegnazione nelle logiche di trasformazione
 * dei campi e sottocampi elementari.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 14/03/2010
 * @see Analyzer
 * @see EnumTable
 * @see EntityTableData
 * @see EntityTableHeader
 * @see EntityTableStructure
*/
@DataBaseMappedEnumeration
public enum EnumDateFormat {
	
	NOT_ASSIGNED,           	// 00 Di servizio 
	
	DATE_AAAAMMGG,				// 01
	DATE_GGMMAAAA,				// 02
	DATE_AAAABMMBGG,            // 03 AAAA/MM/GG
	DATE_GGBMMBBAAAA;			// 04 GG/MM/AAAA

}
