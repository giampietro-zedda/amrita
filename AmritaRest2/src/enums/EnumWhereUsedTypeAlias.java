package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumWhereUsedTypeAlias 
 * </h1>
 *  <p>
 * Questa enum elenca i possibili livelli di matching dell'utilizzo di un dato elementare.
 * Particolarmente rilevante per i tracciati record che possono essere mappati da 
 * moduli copy o direttamente nei programmi, causando possibili disallineamenti.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 12/03/2010
 * @see Analyzer
 * @see EnumWhereUsedOrigin
 * @see EnumWhereUsedType
 * @see EnumEntityAccessType
*/
@DataBaseMappedEnumeration
public enum EnumWhereUsedTypeAlias{

	NOT_ASSIGNED,			  	  // 00 
	
	ALIAS_FULL_MATCH,             // 01 Pos, Lng, Usage, Sign =
	ALIAS_HIGHER_MATCH,           // 02 Pos = e lng >
	ALIAS_LOWER_MATCH,            // 03 Pos = e lng <
	ALIAS_PARTIAL_MATCH,          // 04 Pos = e usage, segno etc <>
	ALIAS_INSIDE_MATCH,           // 05 ALIAS all'interno di un campo definito
	ALIAS_ACROSS_MATCH,           // 06 ALIAS a cavallo di due campi
	ALIAS_BETWEEN_MATCH,          // 07 ALIAS Fra due campi non definito
	ALIAS_BEYOND_LAST_MATC,       // 08 ALIAS Oltre l'area
	ALIAS_EXCEED_LAST_MATCH,      // 09 ALIAS su ultimo campo oltre la fine
	ALIAS_PRECEDING_FIRST_MATCH,  // 10 ALIAS prima del primo campo
	ALIAS_OVERLAP_MATCH,          // 11 ALIAS da area non definita a campo definito
	ALIAS_SQL_LENGTH_MATCH,       // 12 ALIAS di lunghezza diversa da quella SQL
	ALIAS_SQL_INT_MATCH,          // 13 ALIAS con interi   diversi da quelli SQL
	ALIAS_SQL_DEC_MATCH,          // 14 ALIAS con decimali diversi da quelli SQL
	ALIAS_SQL_SIGN_MATCH,         // 15 ALIAS con segno    diverso da quello SQL
	ALIAS_SQL_NILL_MATCH,         // 16 ALIAS con NULL     diverso da quello SQL
}
