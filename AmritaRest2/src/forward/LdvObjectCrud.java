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
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * LdvObjectCrud (METR) 
	 * </h1>
	 *  <p>
	 * This logical data view allow you to get all object data and returns a single row.<br>
	 * That's the logical data view to be used as CRUD access.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
 *
 */
public class LdvObjectCrud extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvObjectCrud() {
		super();
	}

	@Override
	public void declare() {
		FOR_ANY(EntityObject.class, "OBJT", "", null, null, true);
			FOR_ANY(1, "T001", "",  new String[]{"rowData AS typeObjectDesc"}, "typeObject");
			FOR_ANY(2, "T002", "",  new String[]{"rowData AS typeSourceDesc"});
			FOR_ANY(3, "T003", "",  new String[]{"rowData AS statusDesc"});
			FOR_ANY(1, "T001A", "", new String[]{"rowData AS typeObjectDescriptorDesc"}, "typeObjectDescriptor");
		/* Crud Allowed */
        INSERT(EntityObject.class, "OBJT_INS");
        UPDATE(EntityObject.class, "OBJT_UPD", true, null);
        DELETE(EntityObject.class, "OBJT_DEL");
	}

}
