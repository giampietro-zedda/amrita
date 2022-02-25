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

import entities.EntityMetricValue;
import entities.EntityMetricViolation;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * LdvMetrViolations (METV) 
	 * </h1>
	 *  <p>
	 * This logical data view allow you to get all metrics violation data and returns a set of rows.<br>
	 * All rule tables, to get descriptions, are accessed.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
 *
 */
public class LdvMetrViolations extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvMetrViolations() {
		super();
	}

	@Override
	public void declare() {
		VAR("typeObject", EnumObject.OBJECT_PGM_COBOL.ordinal());
		VAR("scope", EnumObject.OBJECT_PGM_COBOL.ordinal());
		FOR_ANY(EntityMetricValue.class, "METR", "", null, new String[]{"scope"}, false);
			FOR_ANY(45, "T045", "", new String[]{"rowData AS metricsScopeDesc"});
			FOR_EACH(EntityMetricViolation.class, "METV", "", new String[]{"value AS value2"}, true) ;
				FOR_ANY(46, "T046", "", new String[]{"rowData AS typeViolationDesc"});
				FOR_ANY(47, "T047", "", new String[]{"rowData AS severityViolationDesc"});
				FOR_ANY(48, "T048", "", new String[]{"rowData AS remediationUnitDesc"});
				FOR_ANY(49, "T049", "", new String[]{"rowData AS qualityCharacteristicDesc"});
		ORDER_BY("typeViolationDesc");
	}
// new String[]{"typeViolation"},
}
