/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LdvReadItemRuleTable
 * </h1>
 * <p>
 * Generic access to a rule table, to get a specific item row.<br>
 * The rule table number, the language and the item key must be set externally. <br>
 * This is a generalized access declaration.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/mar/2012 
 *
*/

package forward;
import enums.EnumLanguage;


/**
 * @author Amrita
 *
 */
public class LdvReadItemRuleTable extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvReadItemRuleTable() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		VAR("language", EnumLanguage.ITALIAN.ordinal());
		FOR_ANY("TXXX", new String[]{"rowData AS descItem"});
	}

}
