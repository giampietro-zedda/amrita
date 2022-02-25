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
import entities.EntityTableHeader;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * LdvRuleTableHeaderCrud (TBHD) 
	 * </h1>
	 *  <p>
	 * This logical data view allow you to get all rule table header and returns a single row.<br>
	 * That's the logical data view to be used as CRUD access.<br>
	 * 
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
 *
 */
public class LdvRuleTableHeaderCrud extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvRuleTableHeaderCrud() {
		super();
	}

	@Override
	public void declare() {
		FOR_ANY(EntityTableHeader.class, "TBHD", "", null, null, true);
			FOR_ANY(1, "T000", "",  new String[]{"rowData AS descTbLocalized"}, "numTable");
		/* Crud Allowed */
        INSERT(EntityTableHeader.class, "TBHD_INS");
        UPDATE(EntityTableHeader.class, "TBHD_UPD", true, null);
        DELETE(EntityTableHeader.class, "TBHD_DEL");
	}

}
