package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumRelationOriginInstr 
 * </h1>
 *  <p>
 * Questa enum elenca le possibili origini fisiche delle relazioni inserite ed
 * elencate dal  {@link EnumRelationSourceProcess}
 * <br>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 22/02/2010
 * @see Analyzer
 * @see EnumRelation
 * @see EnumRelationSourceProcess
*/
@DataBaseMappedEnumeration
public enum EnumRelationOriginInstr {
	
	NOT_ASSIGNED,					// 00 
	
	ORIGIN_CALL,                  	// 01 Call generica in programma
	ORIGIN_CICS_LINK,             	// 02 Exec Cics Link
	ORIGIN_CICS_XCTL,             	// 03 Exec Cics Xctl
	ORIGIN_CICS_EXEC,             	// 04 Exec Cics Generica
	ORIGIN_CICS_READ,             	// 05 READ     di Archivio
	ORIGIN_CICS_READNEXT,         	// 06 READNEXT di Archivio
	ORIGIN_CICS_READPREV,         	// 07 READPREV di Archivio
	ORIGIN_CICS_UPDATE,           	// 08 UPDATE   di Archivio
	ORIGIN_CICS_DELETE,           	// 09 DELETE   di Archivio
	ORIGIN_CICS_INSERT,          	// 10 INSERT   di Archivio
	ORIGIN_CICS_STARTBR,         	// 11 STARTBR  di Archivio
	
	ORIGIN_COPY_COBOL,          	// 12 COBOL_COPY_INSTRUCTION
	ORIGIN_FD_COBOL,             	// 13 FD
	ORIGIN_SD_COBOL,             	// 14 SD
	
	ORIGIN_SQL_INCLUDE,          	// 15 Exec Sql Include
	ORIGIN_SQL_SELECT,           	// 16 Exec Sql Select
	ORIGIN_SQL_FETCH,            	// 17 Exec Sql Fetch
	ORIGIN_SQL_INSERT,           	// 18 Exec Sql Insert
	ORIGIN_SQL_UPDATE,           	// 19 Exec Sql Update
	ORIGIN_SQL_DELETE;           	// 20 Exec Sql Delete

}
