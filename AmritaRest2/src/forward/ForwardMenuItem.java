package forward;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;

import enums.EnumForwardComponent;
/**
 * copyright (c) 2009-2011 e-Amrita - Ing Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardMenuItem
 * </h1>
 * <p>
 * This class model a generic menu item.<br>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardPanel
 * @see ForwardCommonAreas
 *
*/

public class ForwardMenuItem {
	
	private String controlName = "";					// Nome menuItem, coincide con nome oggetto JMenuItem/JCheckBoxMenuItem/JSeparator.... 
	private String caption = "";						// Descrizione in lingua menu item 
	private String pathIcon = "";						// Path icona utilizzata per arricchire il menu item
	private String groupName = "";						// Nome gruppo per insiemi di JCheckBoxMenuItem e JRadioButtonMenuItem
	private int shortKey = 0;							// Short key di attivazione come KeyEvent.VK_1, KeyEvent.VK_A, ...
	private int keyModifiers = 0;						// Modificatori insieme a shortKey come maschere ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	private EnumForwardComponent componentType = null;	// Tipo componente grafico
	private JComponent graphicObjectMenu = null; 		// Component Swing JMenuItem, JCheckBoxMenuItem, ... per menu standard realizzati con JMenu (sempre istanziato)
	private JComponent graphicObject = null;            // Component Swing JButton con cui la riga di menù item è implementata o completata
	private boolean isSubMenuActivation = false;        // True indica che il menu item e una foglia di attivazione di un submenu
	private String subMenuName = "";					// Nome submenu se il menuItem apre un subMenu (isSubMenuActivation = true), descritto da ForwardMenu
	                                                    // In questo caso controName NON è significativo
	private ForwardMenu subMenu = null;					// Descrittore completo del submenu
    private int index = 0;                         	    // Numero definizione menu item 0-based
	
    
    /** Constructor */
	public ForwardMenuItem(String menuItemName, String caption) {
		this.controlName = menuItemName;
		this.caption = caption;
	}

	/**
	 * Get the name of the menu control name.<br>
	 * <p>
	 * It could be a {@link JMenuItem}, a {@link JCheckBoxMenuItem} etc.<br>
	 * <p>
	 * @return the controlName
	 */
	public String getControlName() {
		return controlName;
	}


	/**
	 * Set the name of the menu item.<br>
	 * <p>
	 * It could be a {@link JMenuItem}, a {@link JCheckBoxMenuItem} etc.<br>
	 * <p>
	 * @param controlName the controlName to set
	 */
	public void setControlName(String controlName) {
		this.controlName = controlName;
	}




	/**
	 * Get the text of the menu item.<br>
	 * <p>
	 * @return the caption
	 */
	public String getCaption() {
		return caption;
	}


	/**
	 * Set the text of the menu item.<br>
	 * <p>
	 * @param caption the caption to set
	 */
	public void setCaption(String caption) {
		this.caption = caption;
	}


	/**
	 * Get the icon path of the menu item.<br>
	 * <p>
	 * @return the pathIcon
	 */
	public String getPathIcon() {
		return pathIcon;
	}


	/**
	 * Set the icon path of the menu item.<br>
	 * <p>
	 * @param pathIcon the pathIcon to set
	 */
	public void setPathIcon(String pathIcon) {
		this.pathIcon = pathIcon;
	}

	
	/**
	 * Get the group owner of the set of {@link JCheckBoxMenuItem} or {@link JRadioButtonMenuItem}.<br>
	 * <p>
	 * @return the groupName
	 */
	public String getGroupName() {
		return groupName;
	}

	/**
	 * Set the group owner of the set of {@link JCheckBoxMenuItem} or {@link JRadioButtonMenuItem}.<br>
	 * <p>
	 * @param groupName the groupName to set
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	/**
	 * Get the short key active with a normal click on the menu item<br>
	 * <p>
	 * The short key must be like KeyEvent.VK_x.<br>
	 * <p>
	 * @return the shortKey
	 */
	public int getShortKey() {
		return shortKey;
	}


	/**
	 * Set the short key active with a normal click on the menu item<br>
	 * <p>
	 * The short key must be like KeyEvent.VK_x.<br>
	 * <p>
	 * @param shortKey the shortKey to set
	 */
	public void setShortKey(int shortKey) {
		this.shortKey = shortKey;
	}


	
	
	/**
	 * Get modifiers pressed together the shortKey as a mask of ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK<br>
	 * <p>
	 * @return the keyModifiers
	 */
	public int getKeyModifiers() {
		return keyModifiers;
	}

	/**
	 * Set modifiers pressed together the shortKey as a mask of ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK<br>
	 * <p>
	 * @param keyModifiers the keyModifiers to set
	 */
	public void setKeyModifiers(int keyModifiers) {
		this.keyModifiers = keyModifiers;
	}

	/**
	 * Get the component type as coded by forward.<br>
	 * <p>
	 * @return the componentType
	 */
	public EnumForwardComponent getComponentType() {
		return componentType;
	}

	/**
	 * Set the component type as coded by forward.<br>
	 * <p>
	 * @param componentType the componentType to set
	 */
	public void setComponentType(EnumForwardComponent componentType) {
		this.componentType = componentType;
	}

	/**
	 * Get the graphic object binded.<br>
	 * <p>
	 * Is the {@link JButton}, {@link JLabel}, {@link JTextField}, {@link JComboBox}, {@link JCheckBox}, {@link JSeparator} object,  declared <br>
	 * for the menu item.<br>
	 * <p>
	 * @return the graphicObject
	 */
	public JComponent getGraphicObject() {
		return graphicObject;
	}


	/**
	 * Set the graphic object binded.<br>
	 * <p>
	 * Is the {@link JButton}, {@link JLabel}, {@link JTextField}, {@link JComboBox}, {@link JCheckBox}, {@link JSeparator} object,  declared <br>
	 * for the menu item.<br>
	 * <p>
	 * @param graphicObject the graphicObject to set
	 */
	public void setGraphicObject(JComponent graphicObject) {
		this.graphicObject = graphicObject;
	}


	/**
	 * Get the graphic object for standard {@link JMenu} implementation<br>
	 * <p>
	 * Is the {@link JMenu}, {@link JMenuItem}, {@link JCheckBoxBoxMenuItem}, , {@link JOptionBoxMenuItem}, {@link JSeparator} object,  in any case <br>
	 * created for the menu, to support standard menuBar reference menu types.<br>
	 * <p>
	 * @return the graphicObjectMenu
	 */
	public JComponent getGraphicObjectMenu() {
		return graphicObjectMenu;
	}


	/**
	 * Set the graphic object for standard {@link JMenu} implementation<br>
	 * <p>
	 * Is the {@link JMenu}, {@link JMenuItem}, {@link JCheckBoxBoxMenuItem}, , {@link JOptionBoxMenuItem}, {@link JSeparator} object,  in any case <br>
	 * created for the menu, to support standard menuBar reference menu types.<br>
	 * <p>
	 * @param graphicObjectMenu the graphicObjectMenu to set
	 */
	public void setGraphicObjectMenu(JComponent graphicObjectMenu) {
		this.graphicObjectMenu = graphicObjectMenu;
	}




	/**
	 * Get if the menu item is a leaf with no submenu attached<br>
	 * <p>
	 * @return the isSubMenuActivation
	 */
	public boolean isSubMenuActivation() {
		return isSubMenuActivation;
	}


	/**
	 * Set if the menu item is a leaf with no submenu attached<br>
	 * <p>
	 * @param isSubMenuActivation the isSubMenuActivation to set
	 */
	public void setSubMenuActivation(boolean isSubMenuActivation) {
		this.isSubMenuActivation = isSubMenuActivation;
	}


	/**
	 * Get the name of the submenu descriptor when the menu item is not a leaf<br>
	 * <p>
	 * 
	 * @return the subMenuName
	 */
	public String getSubMenuName() {
		return subMenuName;
	}


	/**
	 * Set the name of the submenu descriptor when the menu item is not a leaf<br>
	 * <p>
	 * 
	 * @param subMenuName the subMenuName to set
	 */
	public void setSubMenuName(String subMenuName) {
		this.subMenuName = subMenuName;
	}

	/**
	 * Get the submenu descriptor.<br>
	 * <p>
	 * @return the subMenu
	 */
	public ForwardMenu getSubMenu() {
		return subMenu;
	}

	/**
	 * Set the submenu descriptor.<br>
	 * <p>
	 * @param subMenu the subMenu to set
	 */
	public void setSubMenu(ForwardMenu subMenu) {
		this.subMenu = subMenu;
	}

	

	/**
	 * Get the defintion number, 0-based, of the menu item declared for the menu.<br>
	 * <p>
	 * @return the index
	 */
	public int getIndex() {
		return index;
	}

	/**
	 * @param index the index to set
	 */
	public void setIndex(int index) {
		this.index = index;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Name="+controlName+",Caption="+caption;
	}

	
}
