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
import entities.EntityTableStructure;

/**
 * @author Amrita
 *
 */
public class LdvReadSetStructureTable extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvReadSetStructureTable() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		VAR("numTable", new Integer(0));
		FOR_EACH(EntityTableStructure.class, "TBST",  "", new String[]{"idItem AS columnName"},  new String[]{"posItem", "idItem"}, true);
			FOR_ANY(30, "T030", "",  new String[]{"rowData AS descLong"});
			FOR_ANY(31, "T031", "",  new String[]{"rowData AS descShort"});
	}
}
