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
public class LdvSubSystem extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvSubSystem() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		VAR("typeObject", EnumObject.OBJECT_SUBSYS.ordinal());
		FOR_EACH(EntityObject.class, "SUBS", "", new String[]{"idObject AS subSystemValue"}, new String[]{"idObject"}, false,  new String[]{"idObject"});
	}
}
