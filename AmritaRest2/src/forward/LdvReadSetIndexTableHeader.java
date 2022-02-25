package forward;
import entities.EntityTableHeader;
/**
 * copyright (c) 2010-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LdvReadSetIndexTableHeader
 * </h1>
 * <p>
 * Gets all tables defined by the Forward generalized table system.<br>
 * Only the language must be set externally, otherwise ENGLISH will be used.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/mar/2012 
 * @see LdvReadItemRuleTable
 * @see LdvReadSetRuleTable
 *
*/

public class LdvReadSetIndexTableHeader extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvReadSetIndexTableHeader() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		FOR_EACH(EntityTableHeader.class, "TBHD", "tableSystem = true", null,  new String[]{"numTable"}, true);
			FOR_ANY(0, "T000", "",  new String[]{"rowData AS tableDesc"});
		ORDER_BY("numTable");
	}

}
