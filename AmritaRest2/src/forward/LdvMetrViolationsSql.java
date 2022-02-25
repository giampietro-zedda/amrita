/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ClassName
 * </h1>
 * <p>
 * Paragraph text
 * Paragraph text <br>
 * List
 * <Ul>
 * <Li> Item list 1
 * <Li> Item list 2
 * </Ul>
 * 
 * <h3>Title </h3>
 * bold <b>PROCEDURE DIVISION USING</b>. 
 * Link {@link ClassName}).
 * 
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/mar/2012 
 * @see ClassName
 *
*/

package forward;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * LdvMetrViolationsSql (METV) 
	 * </h1>
	 *  <p>
	 * Version of LdvMetrViolations with application Sql embedded.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
 *
 */
public class LdvMetrViolationsSql extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvMetrViolationsSql() {
		super();
	}

	@Override
	public void declare() {
		VAR("typeObject", EnumObject.OBJECT_PGM_COBOL.ordinal());
		FOR_SQL(
"SELECT " + 
"   T045.TBDTRDAT AS metricsScopeDesc " + 
",  METV.METVSYST AS system " + 
",  METV.METVSUBS AS subSystem " + 
",  METV.METVIDOB AS idObject " + 
",  METV.METVTYPO AS typeObject " + 
",  METV.METVSECT AS section " + 
",  METV.METVTYPV AS typeViolation " + 
",  METV.METVSVRT AS severityViolation " + 
",  METV.METVORGN AS originViolation " + 
",  METV.METVSROW AS originViolationRows " + 
",  METV.METVSCPY AS originViolationRowsCopy " + 
",  METV.METVCNTV AS cntViolations " + 
",  METV.METVVALU AS value2 " + 
",  METV.METVRCST AS remediationCost " + 
",  METV.METVRUNT AS remediationUnit " + 
",  METV.METVQCHR AS qualityCharacteristic " + 
",  METV.METVT001 AS numTableT001 " + 
",  METV.METVT046 AS numTableT046 " + 
",  METV.METVT047 AS numTableT047 " + 
",  METV.METVT048 AS numTableT048 " + 
",  METV.METVT049 AS numTableT049 " + 
",  T046.TBDTRDAT AS typeViolationDesc " + 
",  T047.TBDTRDAT AS severityViolationDesc " + 
",  T048.TBDTRDAT AS remediationUnitDesc " + 
",  T049.TBDTRDAT AS qualityCharacteristicDesc  " + 

" FROM ((((((METR AS METR  LEFT JOIN TBDT AS T045  ON T045.TBDTKVAL = CSTR(METR.METRSCOP) " +
"                                                 AND METR.METRT045 = T045.TBDTTTAB) " +
"                         INNER JOIN METV AS METV  ON METR.METRSYST = METV.METVSYST " +
"                                                 AND METR.METRSUBS = METV.METVSUBS " +
"                                                 AND METR.METRIDOB = METV.METVIDOB " +
"                                                 AND METR.METRTYPO = METV.METVTYPO " +
"                                                 AND METR.METRSECT = METV.METVSECT) " +
"                          LEFT JOIN TBDT AS T046  ON T046.TBDTKVAL = CSTR(METV.METVTYPV) " +
"                                                 AND METV.METVT046 = T046.TBDTTTAB) " +
"                          LEFT JOIN TBDT AS T047  ON T047.TBDTKVAL = CSTR(METV.METVSVRT) " +
"                                                 AND METV.METVT047 = T047.TBDTTTAB) " +
"                          LEFT JOIN TBDT AS T048  ON T048.TBDTKVAL = CSTR(METV.METVRUNT) " +
"                                                 AND METV.METVT048 = T048.TBDTTTAB) " +
"                          LEFT JOIN TBDT AS T049  ON T049.TBDTKVAL = CSTR(METV.METVQCHR) " +
"                                                 AND METV.METVT049 = T049.TBDTTTAB)" + 

" WHERE METR.METRSYST = '*' " + 
" ANDMETR.METRSUBS = 'AI'  " + 
" ANDMETR.METRSCOP = 1  " + 
" ANDMETR.METRIDOB = 'ARAIB009'  " +  
" ANDMETR.METRTYPO = 1  " + 
" ANDT045.TBDTSYST = '*'  " + 
" ANDT045.TBDTSUBS = '*'  " + 
" ANDT045.TBDTLANG = 2  " + 
" ANDT046.TBDTSYST = '*' " +  
" ANDT046.TBDTSUBS = '*'  " + 
" ANDT046.TBDTLANG = 2  " + 
" ANDT047.TBDTSYST = '*'  " + 
" ANDT047.TBDTSUBS = '*'  " + 
" ANDT047.TBDTLANG = 2  " + 
" ANDT048.TBDTSYST = '*'  " + 
" ANDT048.TBDTSUBS = '*' " +  
" ANDT048.TBDTLANG = 2  " + 
" ANDT049.TBDTSYST = '*' " +  
" ANDT049.TBDTSUBS = '*' " +  
" ANDT049.TBDTLANG = 2 " 
				  
				);
	}

}
