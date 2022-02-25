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

/**
 * @author Amrita
 *
 */
public class LdvObjects extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvObjects() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "");
		VAR("subSystem", "");
		VAR("typeObject", new Integer(0));
		VAR("language", new Integer(1));
		FOR_EACH(EntityObject.class, "OBJT", "", null, new String[]{"idObject"}, false,  new String[]{"idObject"});
			FOR_ANY(1, "T001", "",  new String[]{"rowData AS typeObjectDesc"}, "typeObject");
			FOR_ANY(2, "T002", "",  new String[]{"rowData AS typeSourceDesc"});
			FOR_ANY(3, "T003", "",  new String[]{"rowData AS statusDesc"});
			FOR_ANY(1, "T001B", "", new String[]{"rowData AS typeObjectDescriptorDesc"}, "typeObjectDescriptor");
	}
}
