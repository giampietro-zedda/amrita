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

import entities.EntityObject;
import enums.EnumObject;

/**
 * @author Amrita
 *
 */
public class LdvSystem extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvSystem() {
		super();
	}

	@Override
	public void declare() {
		VAR("typeObject", EnumObject.OBJECT_SYS.ordinal());
		VAR("system", "*");
		VAR("subSystem", "*");
		FOR_EACH(EntityObject.class, "SYST", "", new String[]{"idObject AS systemValue"}, new String[]{"idObject"}, false,  new String[]{"idObject"});
	}

}
