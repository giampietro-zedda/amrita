package forward;
import enums.EnumLanguage;


/**
 * Copyright (c) 2010-2012 e-Amrita - ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LdvReadSetRuleTable
 * </h1>
 * <p>
 * Generic access to a rule table, to get a set of rows.<br>
 * The rule table number must be set externally. This is a generalized access declaration.<br>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/mar/2012 
 * @see LdvReadItemRuleTable
 * @see LdvReadSetIndexTable
 *
*/


/**
 * @author Amrita
 *
 */
public class LdvReadSetRuleTable extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvReadSetRuleTable() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		VAR("language", EnumLanguage.ITALIAN.ordinal());
		FOR_EACH("TXXX", "", new String[]{"rowData AS descItem"});
 		ORDER_BY("keySeq");
	}

}
