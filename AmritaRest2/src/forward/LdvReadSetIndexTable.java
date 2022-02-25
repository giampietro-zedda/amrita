package forward;
import enums.EnumLanguage;
import enums.EnumTable;
/**
 * copyright (c) 2010-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LdvReadSetIndexTable
 * </h1>
 * <p>
 * Gets all tables defined by the Forward generalized table system.<br>
 * Only the language must be set externally, otherwise ENGLISH will be used.<br>
 * It will be returned a set of rows with two sinificative columns: <b>codeTable</b> and <b>descTable</b>.<br>
 * <b>codeTable</b> is a string with the table number and <b>descTable</b> the localized table description.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 27/mar/2012 
 * @see LdvReadItemRuleTable
 * @see LdvReadSetRuleTable
 *
*/

public class LdvReadSetIndexTable extends ForwardLogicalDataView {

	/**
	 * Empty constructor
	 */
	public LdvReadSetIndexTable() {
		super();
	}

	@Override
	public void declare() {
		VAR("system", "*");
		VAR("subSystem", "*");
		VAR("language", EnumLanguage.ENGLISH.ordinal());
		VAR("numTable", EnumTable.EnumTable.getNumTable());				// Tabella 0, tabella delle tabelle definite
		FOR_EACH("TXXX", "", new String[]{"keyVal AS codeTable", "rowData AS descTable"});
	}

}
