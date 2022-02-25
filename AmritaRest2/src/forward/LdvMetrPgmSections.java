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
 * @author Amrita
 *
 */
public class LdvMetrPgmSections extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvMetrPgmSections() {
		super();
	}

	@Override
	public void declare() {
		VAR("typeObject", EnumObject.OBJECT_PGM_COBOL.ordinal());
		FOR_EACH(EntityMetricValue.class, "METR", "", null, new String[]{"scope", "section"}, false,  "scope","section");
	}

}
