package forward;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
/**
 * copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardDialogLookupTable
 * </h1>
 * <p>
 * Models a standard forward dialog to select one or more forward table items<br>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 29/dic/2011 
 *
*/

	public class ForwardDialogLookupTable extends ForwardDialog {
		
		private static final long serialVersionUID = 1L;
		private String tableCode = "";
		
		/**
		 *  Constructor for a dialog {@link JOptionPane} standard.<br>
		 */
		public ForwardDialogLookupTable(String tableCode, JFrame frameOwner, String dialogName, boolean modal, String title) {
			super(frameOwner, dialogName, modal, title);
			this.tableCode = tableCode;
		}
	  
  
}
