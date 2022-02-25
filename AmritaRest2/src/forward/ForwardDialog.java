package forward;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
/**
 * copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardDialog
 * </h1>
 * <p>
 * Models a standard forward dialog implemented extending a  {@link JDialog}.<br>
 * A {@link ForwardDialog} it's used to manage customized standard {@link JOptionPane} and application panels<br>
 * Supplementary informations are stored such as a reference to the active forward monitor, the dialog name<br>
 * and the panel (user or JOptionPane) to display with the dialog.<br>
 * <p>
 * No listener is registered in this class. All events are managed by forward monitor, in a centralized way.<br>
 * So it's the forward monitor that intercepts events, entered data and so on, updating informations in the<br>
 * {@link ForwardSystem} area, available to the application code.<br>
 *<p>
 * All dialog to show standard {@link JOptionPane} are automatically modal, so that's impossible do anything in<br>
 * others open windows, until the dialog has been not closed.<br>
 * Normally a dialog can be closed by pressing an option button or the X in the title bar. ForwardDialog makes the<br>
 * closing by X button in title bar not allowed. In any case it's possible intercept this attempt and to execute<br>
 * specific application logic for the event, thru <code>ON_DIALOG_CLOSED_OPTION</code> ON_EVENT() declaring.<br> 
 *<br>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 29/dic/2011 
 *
*/

	public class ForwardDialog extends JDialog {
		
		static final int DIALOG_OPTION_STANDARD = 1;
		static final int DIALOG_USER_PANEL = 2;
		
		private static final long serialVersionUID = 1L;
	    private int dialogType = 0;							// Tipo dialogo
		private JOptionPane optionPanel = null;				// Option panel to display on			
		private JPanel userPanel = null;					// User panel to display on	
		private String iconPath = "";              			// The path icon to be displayed on the dialog frame
		private int posX = 0;              					// The X position where to place the window
		private int posY = 0;              					// The Y position where to place the window
		private boolean isOnCenter  = false;				// If true the initial position will be X, Y
		private boolean isUndecorated = false;              // True to make the dialog undecorated and so fixed on the screen

		/**
		 *  Constructor for a dialog {@link JOptionPane} standard.<br>
		 */
		public ForwardDialog(JFrame frameOwner, String dialogName, boolean modal, String title) {
			super(frameOwner);
			this.userPanel = null;
			this.dialogType = DIALOG_OPTION_STANDARD;
			this.setName(dialogName);
			this.setModal(modal);
			this.setTitle(title);
			this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		}
	  

		/**
		 * Gets the forward dialog type as ForwardDialog.DIALOG_OPTION_STANDARD or ForwardDialog.DIALOG_USER_PANEL <br>
		 * <p>
		 * @return the dialogType
		 */
		public int _getDialogType() {
			return dialogType;
		}


		/**
		 * Gets the JOptionPane that will be displayed by dialog.
		 * <br>
		 * 
		 * @return the optionPanel
		 */
		public JOptionPane _getOptionPanel() {
			return optionPanel;
		}


		/**
		 * Sets the JOptionPane that will be displayed by dialog.
		 * <br>
		 * 
		 * @param optionPanel the optionPanel to set
		 */
		public void _setOptionPanel(JOptionPane optionPanel) {
			this.optionPanel = optionPanel;
		}


		/**
		 * Gets the JPanel that will be displayed by dialog.
		 * <br>
		 * 
		 * @return the userPanel
		 */
		public JPanel _getUserPanel() {
			return userPanel;
		}


		/**
		 * Sets the JPanel that will be displayed by dialog.
		 * <br>
		 * 
		 * @param userPanel the userPanel to set
		 */
		public void _setUserPanel(JPanel userPanel) {
			this.userPanel = userPanel;
		}


		/**
		 * Gets the path of the icon on the title bar.<br>
		 * <p>
		 * @return the iconPath
		 */
		public String getIconPath() {
			return iconPath;
		}


		/**
		 * Sets the path of the icon on the title bar.<br>
		 * <p>
		 * @param iconPath the pathIcon to set
		 */
		public void setIconPath(String iconPath) {
			this.iconPath = iconPath;
		}


		/**
		 * Gets the initial X coordinate where the dialog is placed.<br>
		 * <p>
		 * @return the posX
		 */
		public int _getPosX() {
			return posX;
		}


		/**
		 * Sets the initial X coordinate where the dialog is placed.<br>
		 * <p>
		 * @param posX the posX to set
		 */
		public void setPosX(int posX) {
			this.posX = posX;
		}


		/**
		 * Gets the initial Y coordinate where the dialog is placed.<br>
		 * <p>
		 * @return the posY
		 */
		public int getPosY() {
			return posY;
		}


		/**
		 * Sets the initial Y coordinate where the dialog is placed.<br>
		 * <p>
		 * @param posY the posY to set
		 */
		public void setPosY(int posY) {
			this.posY = posY;
		}


		/**
		 * Gets the undecorate properties of the dialog.<br>
		 * <p>
		 * A true value means no frame, no title bar displayed.<br>
		 * <p>
		 * @return the isUndecorated
		 */
		public boolean _isUndecorated() {
			return isUndecorated;
		}


		/**
		 * Sets the undecorate properties of the dialog.<br>
		 * <p>
		 * A true value means no frame, no title bar displayed.<br>
		 * <p>
		 * @param isUndecorated the isUndecorated to set
		 */
		public void _setUndecorated(boolean isUndecorated) {
			this.isUndecorated = isUndecorated;
		}


		/**
		 * Gets if the dialog will be initially placed on the center of the coordinates system<br>
		 * or at initial X, Y coordinates.
		 * <p>
		 * @return the isOnCenter
		 */
		public boolean isOnCenter() {
			return isOnCenter;
		}


		/**
		 * Sets if the dialog will be initially placed on the center of the coordinates system.<br>
		 * or at initial X, Y coordinates.
		 * <p>
		 * @param isOnCenterContainer the isOnCenterContainer to set
		 */
		public void setOnCenterr(boolean isOnCenter) {
			this.isOnCenter = isOnCenter;
		}
	  
  
  
}
