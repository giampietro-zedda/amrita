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
import entities.EntityTableData;
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
public class LdvRuleTableDataCrud extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvRuleTableDataCrud() {
		super();
	}

	@Override
	public void declare() {
		FOR_ANY(EntityTableData.class, "TBDT", "", null, null, true);
		
		/* Crud Allowed */
        INSERT(EntityTableHeader.class, "TBDT_INS");
        UPDATE(EntityTableHeader.class, "TBDT_UPD", true, null);
        DELETE(EntityTableHeader.class, "TBDT_DEL");
	}

}
