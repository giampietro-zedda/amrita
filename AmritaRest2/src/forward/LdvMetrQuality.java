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
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * LdvMetrQuality (METR) 
	 * </h1>
	 *  <p>
	 * This logical data view allow you to get all metrics data and returns a single row.<br>
	 * Apart the type object, all primary keys values mus be se externally.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
 *
 */
public class LdvMetrQuality extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvMetrQuality() {
		super();
	}

	@Override
	public void declare() {
		VAR("typeObject", EnumObject.OBJECT_PGM_COBOL.ordinal());
		FOR_ANY(EntityMetricValue.class, "QUAL", "", null, null, true);
	}

}
