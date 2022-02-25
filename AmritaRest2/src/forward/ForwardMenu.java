package forward;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;

import enums.EnumForwardComponent;
import enums.EnumForwardOption;

import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;


/**
 * copyright (c) 2009-2012 e-Amrita - Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardMenu
 * </h1>
 * <p>
 * This class models a generic menu.<br>
 * <p>
 * The base is the most detailed menu description, a {@link JMenuBar}, including title, icons, key strokes and so on<br>
 * and built with objects like {@link JMenuItem}, {@link JCheckBoxMenuItem}, {@link JRadioButtonMenuItem} and {@link JSeparator}.<br>
 * <p>
 * According to menu rendering, the same menu description can produce a {@link JPopupMenu}, a {@link JToolBar}, a {@link JList} or<br>
 * finally, a {@link JTree}.<br>
 * <p>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardPanel
 *
*/

public class ForwardMenu {
	
	// Reference forward monitor & function
	private ForwardMonitorDesktop forwardMonitor = null;
	private ForwardFunction function = null;
	
	// Identificazione e shortKey di attivazione
	private String panelName = "";										// Nome pannello in cui il menu viene collocato
	private ForwardPanelMenu panel = null;                              // Pannello di menu forward dove il menu viene costruito
	private String menuName = "";										// Nome menu/submenu
	private String menuTitle = "";										// Title, caption usata solo se rendering è menuBar
	private JComponent menuObject = null;            					// Component Swing per menu in menu bar/popUp/toolbar (JMenu o JPopUpMenu o JToolBar)
	private int shortKey = 0;											// Short key di attivazione come KeyEvent.VK_1, KeyEvent.VK_A, ...

	// Menu root.
	// Ogni menu, anche submenu, è gestito separatamente, anche se originato da un menu root di livello o
	private ForwardMenu menuRoot = null;								// Reference a primo livello menu ( o this)
	
	// Descrizione generale menu se isSubMenu = true
	private EnumForwardOption menuRendering;							// Rendering menu come menuBar, toolbar, ...
	private EnumForwardOption menuType;									// Tipologia menu root, PopUpMenu, menu/submenu classico 
	private EnumForwardOption menuDirection;							// Direzione  
	private EnumForwardOption menuStyle;								// Stile
	private EnumForwardOption menuHeigh;								// Spessore items di menu
	
	// Livello menu  
	private int menuLevel = 0;                                          // Nell'ambito della funzione che lo definisce
	private int menuLevelMax = 0;                                       // Livello massimo menu, 0-based, valido se ROOT_MENU
	
	// Working
	boolean isFound = false;
	
	// Singoli menu items.
	// La struttura è ricorsiva.
	// A fronte di items con sottomenu in ForwardMenuItem
	// viene definito un elenco di sottomenu items.
	private ArrayList<ForwardMenuItem> al_menuItem = null;				// Lista items menu/submenu con eventuali separator

	
	/** Constructor empty */
	public ForwardMenu() {
		this.al_menuItem = new ArrayList<ForwardMenuItem> ();
	}

    /** Constructor for generic menu */
	public ForwardMenu(String menuName
			         , String menuTitle								// Titolo menu significativo se menuBar
			         , EnumForwardOption menuType				// Tipologia generale menu (popUp, menu, submenu)
					 , EnumForwardOption menuRendering			// Rendering (menuBar, btoolBar, ...)
					 , EnumForwardOption menuDirection			// Verticale o orizzontale
					 , EnumForwardOption menuStyle				// Stile implementativo o custom
					 , EnumForwardOption menuHeigh				// Spessore menu item
					  ) {
		super();
		this.menuName = menuName;
		this.menuTitle = menuTitle;
		this.menuType = menuType;
		this.menuRendering = menuRendering;
		this.menuDirection = menuDirection;
		this.menuStyle = menuStyle;
		this.menuHeigh = menuHeigh;
		
		this.al_menuItem = new ArrayList<ForwardMenuItem> ();

	}
	


	/**
	 * Get the name assigned to the menu.<br>
	 * <p>
	 * @return the menuName
	 */
	public String getMenuName() {
		return menuName;
	}

	/**
	 * Get the caption assigned to the menu, used with a menuBar rendering.<br>
	 * <p>
	 * @return the menuTitle
	 */
	public String getMenuTitle() {
		return menuTitle;
	}

	/**
	 * Set the caption assigned to the menu, used with a menuBar rendering.<br>
	 * <p>
	 * @param the menuTitle
	 */
	public void setMenuTitle(String menuTitle) {
		this.menuTitle = menuTitle;
	}


	/**
	 * Set the name assigned to the menu.<br>
	 * <p>
	 * @param menuName the menuName to set
	 */
	public void setMenuName(String menuName) {
		this.menuName = menuName;
	}

	
	/**
	 * Get menu type of the menu.<br>
	 * <p>
	 * The mode can be as popUpMenu or as a normal menu/submenu.<br>
	 * <p>
	 * @return the menuMode
	 */
	public EnumForwardOption getMenuType() {
		return menuType;
	}

	/**
	 * Set menu type of the menu.<br>
	 * <p>
	 * The mode can be as popUpMenu or as a normal menu/submenu.<br>
	 * <p>
	 * @param menuMode the menuMode to set
	 */
	public void setMenuType(EnumForwardOption menuType) {
		this.menuType = menuType;
	}

	/**
	 * Get the graphic object menu with wich the menu is implemented.<br>
	 * <p>
	 * For menus on menu bar the object is a {@link JMenu} object.<br>
	 * For menus popup menus, the object is a {@link JPopUpMenu} object.<br>
	 * <p>
	 * @return the menuObject
	 */
	public JComponent getGraphicObject() {
		return menuObject;
	}

	/**
	 * Set the graphic object menu with wich the menu is implemented.<br>
	 * <p>
	 * For menus on menu bar and popup menus, the object is a {@link JMenu} object.<br>
	 * <p>
	 * @param menuObject the menuObject to set
	 */
	public void setGraphicObject(JComponent menuObject) {
		this.menuObject = menuObject;
	}

	/**
	 * Get the short key active for menu activation.<br>
	 * <p>
	 * The short key must be like KeyEvent.VK_x.<br>
	 * <p>
	 * @return the shortKey
	 */
	public int getShortKey() {
		return shortKey;
	}


	/**
	 * Set the short key active  for menu activation.<br>
	 * <p>
	 * The short key must be like KeyEvent.VK_x.<br>
	 * <p>
	 * @param shortKey the shortKey to set
	 */
	public void setShortKey(int shortKey) {
		this.shortKey = shortKey;
	}


	
	
	/**
	 * Get the menu root, the first level menu.<br>
	 * <p>
	 * 
	 * @return the menuRoot
	 */
	public ForwardMenu getMenuRoot() {
		return menuRoot;
	}

	/**
	 * set the menu root, the first level menu.<br>
	 * <p>
	 * 
	 * @param menuRoot the menuRoot to set
	 */
	public void setMenuRoot(ForwardMenu menuRoot) {
		this.menuRoot = menuRoot;
	}

	/**
	 * Get the menu rendering as coded by forward.<br>
	 * <p>
	 * A menu can be realized with a toolbar, buttons, labels or classic menu items
	 * even in a popup menu.<br>
	 * The a menu can be realized with sections hiding by click or submenu appearing
	 * by mouse moving on the leaf menu item.<br>
	 * <p>
	 * @return the menuRendering
	 */
	public EnumForwardOption getMenuRendering() {
		return menuRendering;
	}

	/**
	 * Set the menu rendering as coded by forward.<br>
	 * <p>
	 * A menu can be realized with a toolbar, buttons, labels or classic menu items
	 * even in a popup menu.<br>
	 * The a menu can be realized with sections hiding by click or submenu appearing
	 * by mouse moving on the leaf menu item.<br>
	 * <p>
	 * @param menuType the menuType to set
	 */
	public void setMenuRendering(EnumForwardOption menuRendering) {
		this.menuRendering = menuRendering;
	}

	/**
	 * Get the menu direction as coded by forward.<br>
	 * <p>
	 * A menu can be horizontal or vertical.<br>
	 * <p>
	 * @return the menuDirection
	 */
	public EnumForwardOption getMenuDirection() {
		return menuDirection;
	}

	/**
	 * Set the menu direction as coded by forward.<br>
	 * <p>
	 * A menu can be horizontal or vertical.<br>
	 * <p>
	 * @param menuDirection the menuDirection to set
	 */
	public void setMenuDirection(EnumForwardOption menuDirection) {
		this.menuDirection = menuDirection;
	}

	/**
	 * Get the menu style as coded by forward.<br>
	 * <p>
	 * A menu can be horizontal or vertical.<br>
	 * <p>
	 * @return the menuStyle
	 */
	public EnumForwardOption getMenuStyle() {
		return menuStyle;
	}

	/**
	 * Get the menu style as coded by forward.<br>
	 * <p>
	 * A menu style can be automatic, custom or of a predefined style.<br>
	 * <p>
	 * @param menuStyle the menuStyle to set
	 */
	public void setMenuStyle(EnumForwardOption menuStyle) {
		this.menuStyle = menuStyle;
	}

	
	/**
	 * Get the heigh of menu item.<br>
	 * <p>
	 * @return the menuHeigh
	 */
	public EnumForwardOption getMenuHeigh() {
		return menuHeigh;
	}

	/**
	 * Get the heigh of menu item.<br>
	 * <p>
	 * @param menuHeigh the menuHeigh to set
	 */
	public void setMenuHeigh(EnumForwardOption menuHeigh) {
		this.menuHeigh = menuHeigh;
	}

	
	
	/**
	 * Get the level of the menu, 0-based, related to the function where the menu is declared.<br>
	 * <p>
	 * @return the menuLevel
	 */
	public int getMenuLevel() {
		return menuLevel;
	}

	/**
	 * Set the level of the menu, 0-based, related to the function where the menu is declared.<br>
	 * <p>
	 * @param menuLevel the menuLevel to set
	 */
	public void setMenuLevel(int menuLevel) {
		this.menuLevel = menuLevel;
	}

	
	
	/**
	 * Get the max deep menu level, as a 0-based value.<br>
	 * <p>
	 * This method is valid for ROOT and POP_UP menus.<br>
	 * <p> 
	 * @return the menuLevelMax
	 */
	public int getMenuLevelMax() {
		return menuLevelMax;
	}

	/**
	 * Set the max deep menu level, as a 0-based value.<br>
	 * <p>
	 * This method is valid for ROOT and POP_UP menus.<br>
	 * <p> 
	 * @param menuLevelMax the menuLevelMax to set
	 */
	public void setMenuLevelMax(int menuLevelMax) {
		this.menuLevelMax = menuLevelMax;
	}

	
	
	/**
	 * Get first level menu items.<br>
	 * <p>
	 * A menu item is modeled by a {@link ForwardMenuItem} object.<br>
	 * <p>
	 * @return the al_menuItem
	 */
	public ArrayList<ForwardMenuItem> getMenuItems() {
		return al_menuItem;
	}

	/**
	 * Add a menu item to the current menu.<br>
	 * <p>
	 * A menu item is modeled by a {@link ForwardMenuItem} object.<br>
	 * <p>
	 */
	public void addMenuItem(ForwardMenuItem forwardMenuItem) {
		getMenuItems().add(forwardMenuItem);
		return;
	}

	/**
	 * Get a specific menu item, with the given swing control name.<br>
	 * <p>
	 * A menu item is modeled by a {@link ForwardMenuItem} object.<br>
	 * If non menu item has been defined with the name a null value will be returned.<br>
	 * <p>
	 * @param controlName 
	 * @return the ForwardMenuItem
	 */
	public ForwardMenuItem getMenuItem(String controlName) {
		int index = 0;
		for (ForwardMenuItem forwardMenuItem : this.al_menuItem) {
			if (forwardMenuItem.getControlName().equals(controlName)) {
				forwardMenuItem.setIndex(index);
				return forwardMenuItem;
			}
			index++;
		}
		return null;
	}
    
	
	/**
	 * Set all first level menu items.<br>
	 * <p>
	 * A menu item is modeled by a {@link ForwardMenuItem} object.<br>
	 * <p>
	 * @param al_menuItem the al_menuItem to set
	 */
	public void setMenuItems(ArrayList<ForwardMenuItem> al_menuItem) {
		this.al_menuItem = al_menuItem;
	}

	/**
	 * Get the reference to forward monitor.<br>
	 * <p>
	 * @return the forwardMonitor
	 */
	public ForwardMonitorDesktop getForwardMonitor() {
		return forwardMonitor;
	}

	/**
	 * Set the reference to forward monitor.<br>
	 * <p>
	 * @param forwardMonitor the forwardMonitor to set
	 */
	public void setForwardMonitor(ForwardMonitorDesktop forwardMonitor) {
		this.forwardMonitor = forwardMonitor;
	}

	
	
	
	/**
	 * Get the forward function where the menu has been declared.<br>
	 * <p>
	 * @return the function
	 */
	public ForwardFunction getFunction() {
		return function;
	}

	/**
	 * Set the forward function where the menu has been declared.<br>
	 * <p>
	 * @param function the function to set
	 */
	public void setFunction(ForwardFunction function) {
		this.function = function;
	}

	/**
	 * Get the panel name into wich the menu is placed.<br>
	 * <p>
	 * @return the panelName
	 */
	public String getPanelName() {
		return panelName;
	}

	/**
	 * Set the panel name into wich the menu is placed.<br>
	 * <p>
	 * @param panelName the panelName to set
	 */
	public void setPanelName(String panelName) {
		this.panelName = panelName;
	}
	
	
	/**
	 * Gets the {@link ForwardPanelMenu} containg the menu.<br>
	 * <p>
	 * @return the panel
	 */
	public ForwardPanelMenu getForwardPanelMenu() {
		return panel;
	}

	/**
	 * Sets the {@link ForwardPanelMenu} containg the menu.<br>
	 * <p>
	 * @param panel the panel to set
	 */
	public void setForwardPanelMenu(ForwardPanelMenu panel) {
		this.panel = panel;
	}

	/**
	 * Get a JMenuBar object as menu rendering.<br>
	 * <p>
	 * The {@link JMenuBar} returned can be directly set on the frame<br>
	 * <p<
	 * @return the jmenuBar to set directly on the frame
	 */
	public JMenuBar getJMenuBar() {
		JMenuBar jmenuBar = null;
		JMenu jmenu = null;

		jmenuBar = new JMenuBar();
		jmenu = new JMenu(getMenuTitle());
		buildMenuBarPopUpRecursive(jmenuBar, jmenu, this, 0);
		return jmenuBar;
	}

	/**
	 * Get a JPopupMenu object as menu rendering.<br>
	 * <p>
	 * @return the jpopupMenu to show on mouse events on registered components
	 */
	public JPopupMenu getJPopupMenu() {
		
		JMenuBar jmenuBar = null;					// Di servizio
		JPopupMenu jpopupMenu = null;
		
		jpopupMenu =  new JPopupMenu(getMenuTitle());
		buildMenuBarPopUpRecursive(jmenuBar, jpopupMenu, this, 0);
		return jpopupMenu;
	}

	
	/**
	 * Get a JToolBar object as menu rendering.<br>
	 * <p>
	 * @return the jpopupMenu to show on mouse events on registered components
	 */
	@SuppressWarnings("rawtypes")
	public JToolBar getJToolbarMenu() {
		
		JToolBar jmenuToolBar = null;
		JSeparator jseparator = null;
		JButton jbutton = null;
		JTextField jtextField = null;
		JComboBox jcomboBox = null;
		
        // Riempimento menu
		jmenuToolBar = (JToolBar) this.getGraphicObject();
		
		// Scan menu items da disporre nel pannello del menu in input
		for (ForwardMenuItem menuItem : this.getMenuItems()) {
			
			// Submenu: skip
			if (menuItem.isSubMenuActivation()) {
				continue;
			}
			
			// Separator
			if (menuItem.getComponentType() == EnumForwardComponent.JSeparator) {
				jseparator = (JSeparator) menuItem.getGraphicObject();
				jseparator.setOrientation(JSeparator.VERTICAL);
				jseparator.setPreferredSize(new Dimension(3, jseparator.getWidth()));
				jseparator.setMinimumSize(new Dimension(3, jseparator.getWidth()));
				jseparator.setMaximumSize(new Dimension(3, jseparator.getWidth()));
				jmenuToolBar.add(jseparator);
				continue;
			}
			
			// MenuItem, si utilizza l'oggetto JButton istanziato in dichiarazione.
			// E' il modo standard per definire elementi di toolbar.
			// Il listener è stato impostato dal monito in base alle logiche di evento
			// Si aggiungono pulsanti quadrati e se c'è l'icona, il testo viene eliminato
			if (menuItem.getComponentType() == EnumForwardComponent.JMenuItem) {
				jbutton = (JButton) menuItem.getGraphicObject();
				if (jbutton.getIcon() != null) {
					jbutton.setIcon(jbutton.getIcon());
					jbutton.setText("");
				}
				setLookStyleMenuItem(jbutton);
				setLookHeighMenuItem(jbutton);
				jmenuToolBar.add(jbutton);
				continue;
			}
			
			// Button esplicito.
			// Si mantengono le dimensioni originarie/personalizzate.
			if (menuItem.getComponentType() == EnumForwardComponent.JButton) {
				jbutton = (JButton) menuItem.getGraphicObject();
				jmenuToolBar.add(jbutton);
				continue;
			}
			
			// TextField
			// Si mantengono le dimensioni originarie/personalizzate.
			if (menuItem.getComponentType() == EnumForwardComponent.JTextField) {
				jtextField = (JTextField) menuItem.getGraphicObject();
				jmenuToolBar.add(jtextField);
				continue;
			}
			
			// ComboBox
			// Si mantengono le dimensioni originarie/personalizzate.
			if (menuItem.getComponentType() == EnumForwardComponent.JComboBox) {
				jcomboBox = (JComboBox) menuItem.getGraphicObject();
				jmenuToolBar.add(jcomboBox);
				continue;
			}
		}
		
		return jmenuToolBar;
	}

	/*
	 * Impostazione  colore menu in base allo stile
	 */
	private void setLookStyleMenuItem(JButton btn) {
		
		btn.setHorizontalAlignment(SwingConstants.LEADING);
		
		// Stile
		switch (this.getMenuStyle()) {
				case MENU_STYLE1:
					btn.setBackground(ForwardPanelMenu.GREY);													// Grigio
					btn.setForeground(ForwardPanelMenu.ORANGE);													// Arancione
					break;
				case MENU_STYLE2:
					btn.setBackground(ForwardPanelMenu.ORANGE);													// Arancione
					btn.setForeground(ForwardPanelMenu.GREY);													// Grigio
					break;
				case MENU_STYLE3:
					btn.setBackground(ForwardPanelMenu.GREY);													// Grigio
					btn.setForeground(ForwardPanelMenu.WHITE);													// Bianco
					break;
				case MENU_STYLE4:
					btn.setBackground(ForwardPanelMenu.WHITE);													// Bianco
					btn.setForeground(ForwardPanelMenu.GREY);													// Grigio
					break;
				case MENU_STYLE5:
					btn.setBackground(ForwardPanelMenu.BLUE);													// BLU
					btn.setForeground(ForwardPanelMenu.WHITE);													// Bianco
					break;
				case MENU_STYLE6:
					btn.setBackground(ForwardPanelMenu.WHITE);													// Bianco
					btn.setForeground(ForwardPanelMenu.BLUE);													// Blu
					break;
				case MENU_STYLE_JAVA_DEFAULT:
					break;
				default:
					btn.setBackground(ForwardPanelMenu.GREY);													// Grigio
					btn.setForeground(ForwardPanelMenu.ORANGE);													// Arancione
					break;
			}

	}

	/*
	 * Impostazione spessore e menu in base allo stile
	 */
	private void setLookHeighMenuItem(JButton btn) {
		
		btn.setHorizontalAlignment(SwingConstants.LEADING);
		
		
		// Spessore
		switch (this.getMenuHeigh()) {
				case MENU_HEIGH_LARGE:
					btn.setPreferredSize(new Dimension(128, 128));		// Large
					btn.setMinimumSize(new Dimension(128, 128));		// Large
					btn.setMaximumSize(new Dimension(128, 128));		// Large
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					btn.setBorder(new EmptyBorder(0,0,0,0));
					break;
				case MENU_HEIGH_MEDIUM:
					btn.setPreferredSize(new Dimension(64, 64));		// Medium
					btn.setMinimumSize(new Dimension(64, 64));			// Medium
					btn.setMaximumSize(new Dimension(64, 64));			// Medium
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					btn.setBorder(new EmptyBorder(0,0,0,0));
					break;
				case MENU_HEIGH_THIN:
					btn.setPreferredSize(new Dimension(32, 32));		// Thin
					btn.setMinimumSize(new Dimension(32, 32));			// Thin
					btn.setMaximumSize(new Dimension(32, 32));			// Thin
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					btn.setBorder(new EmptyBorder(0,0,0,0));
					break;
				case MENU_HEIGH_EXTRA_THIN:
					btn.setPreferredSize(new Dimension(16, 16));		// Extra thin
					btn.setMinimumSize(new Dimension(16, 16));			// Medium
					btn.setMaximumSize(new Dimension(16, 16));			// Medium
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					btn.setBorder(new EmptyBorder(0,0,0,0));
					break;
				default:
					btn.setPreferredSize(new Dimension(64, 64));		// Large
					btn.setMinimumSize(new Dimension(64, 64));			// Medium
					btn.setMaximumSize(new Dimension(64, 64));			// Medium
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					btn.setBorder(new EmptyBorder(0,0,0,0));
					break;
			}
	}

	
	/* ------------------------------------------------------------------------------
     * Costruzione menu e accodamento a menuBar.
     * ------------------------------------------------------------------------------
     * 
     * Gli oggetti standard del menu sono già stati creati al momento della dichiarazione della funzione.
     * Sono impostati quindi text, accelerator key, image etc.
     * Viene qui gestito il raggruppamento di JRadioButtonMenuItem o di JCheckBoxMenuItem
     */ 
	private void buildMenuBarPopUpRecursive(JMenuBar jmenuBar, JComponent menu, ForwardMenu menuDescriptor, int lvl) {
        
		ForwardMenu forwardSubMenu = null;
		JMenu menuNew = null;
		JMenu subMenu = null;
        JMenuItem jmenuItem;
        JSeparator jseparator;
        JRadioButtonMenuItem jradioButtonMenuItem;
        JCheckBoxMenuItem jcheckBoxMenuItem;
        ButtonGroup group = null;
        String groupNamePrec = "";
        String subMenuName = "";
        
        // Popolamento menu
        
		// Scan menu items da disporre nel pannello del menu in input
		for (ForwardMenuItem menuItem : menuDescriptor.getMenuItems()) {
			
			// Clear button group
			if (menuItem.getGroupName().equals("")) {
				group = null;
			}
			
			// New button group
			if (!menuItem.getGroupName().equals("") 
			&&  !menuItem.getGroupName().equals(groupNamePrec)) {
				groupNamePrec = menuItem.getGroupName();
				group = new ButtonGroup();
			}
			
			// Separator
			if (menuItem.getComponentType() == EnumForwardComponent.JSeparator) {
				if (lvl == 0 && menuDescriptor.menuType != EnumForwardOption.MENU_TYPE_POPUP) {continue;}
				jseparator = (JSeparator) menuItem.getGraphicObject();
				menu.add(jseparator); 
				continue;
			}
			
			// MenuItem Non di attivazione submenu
			if (menuItem.getComponentType() == EnumForwardComponent.JMenuItem 
			&& !menuItem.isSubMenuActivation()) {
				jmenuItem = (JMenuItem) menuItem.getGraphicObjectMenu();
				// Il menu coincide con l'item, al primo livello
				if (lvl == 0) {
					menuNew = new JMenu();
					menuNew.setName(jmenuItem.getName());
					menuNew.setText(jmenuItem.getText());
					menuNew.setMnemonic(jmenuItem.getMnemonic());
					menuNew.addActionListener(this.getForwardMonitor());								// Il listener è il monitor di forward
					if (menu instanceof JPopupMenu) {
						menu.add(jmenuItem);															// Inserimento in popup menu
						continue;
					}
					
					jmenuBar.add(menuNew);																// Inserimento menu vuoto a menuBar
					continue;
				}
				// Si tratta di un menu item di livello + annidato, da accodare al menu corrente (normale o popup)
				jmenuItem.addActionListener(this.getForwardMonitor());								// Il listener è il monitor di forward
				menu.add(jmenuItem);
				continue;
			}
			
			// CheckBox
			if (menuItem.getComponentType() == EnumForwardComponent.JCheckBoxMenuItem) {
				if (lvl == 0 && menuDescriptor.menuType != EnumForwardOption.MENU_TYPE_POPUP) {continue;}															// Il primo livello della menuBar non può contenere una checkBox
				jcheckBoxMenuItem = (JCheckBoxMenuItem) menuItem.getGraphicObject();
				jcheckBoxMenuItem.addActionListener(this.getForwardMonitor());						// Il listener è il monitor di forward
				menu.add(jcheckBoxMenuItem);
				continue;
			}
			
			// RadioButton
			if (menuItem.getComponentType() == EnumForwardComponent.JRadioButtonMenuItem) {
				if (lvl == 0 && menuDescriptor.menuType != EnumForwardOption.MENU_TYPE_POPUP) {continue;}															// Il primo livello della menuBar non può contenere un RadioButton
				jradioButtonMenuItem = (JRadioButtonMenuItem) menuItem.getGraphicObject();
				jradioButtonMenuItem.addActionListener(this.getForwardMonitor());					// Il listener è il monitor di forward
				menu.add(jradioButtonMenuItem);
				// Gruppo di appartenenza
				if (!menuItem.getGroupName().equals("")) {
					group.add(jradioButtonMenuItem);
				}
				continue;
			}			
			
			// Submenu
			if (menuItem.isSubMenuActivation()) {
				// Recupero dichiarazione subMenu per la funzione
				subMenuName = menuItem.getSubMenuName();
				forwardSubMenu = this.forwardMonitor.mcb.function.getMenu(subMenuName);
				if (forwardSubMenu == null) {continue;}												// SubMenu non dichiarato: skip
				subMenu = (JMenu) forwardSubMenu.getGraphicObject();
				subMenu.setFont(subMenu.getFont().deriveFont(Font.PLAIN));							// Elimina il Bold di default
				subMenu.setMnemonic(menuItem.getShortKey());
				subMenu.addActionListener(this.getForwardMonitor());								// Il listener è il monitor di forward
				
				// Accodamento nuovo subMenu in menuBar solo se a livello 0
				if (lvl == 0) {
					jmenuBar.add(subMenu);																 
				} else {
					menu.add(subMenu);
				}
				
		        // Popolamento ricorsivo submenu
		        buildMenuBarPopUpRecursive(jmenuBar, subMenu, forwardSubMenu, lvl + 1);
		        continue;
			}
		}
  		return;
	}

	
	
	/*
	 * Restituisce il menu/submenu dove il menu item è stato definito<br>
	 * <p>
	 * La ricerca viene effettuata ricorsivamente su tutti i sottomenu presenti.<br>
	 * <p>
	 */
	public ForwardMenu getMenuOwner(String menuItemName){
		ForwardMenu menuOwner = null;
		menuOwner = getMenuOwnerRecursive(this, menuItemName);
		return menuOwner;
	};
	
	/*
	 * Ricerca ricorsiva menu item 
	 */
	private ForwardMenu getMenuOwnerRecursive(ForwardMenu menu, String menuItemName) {
		ForwardMenu menuOwner = null;
		ForwardMenu subMenuRecursive = null;
		String subMenuNameRecursive = "";
		
		
		// Scan menu items
		for (ForwardMenuItem menuItem : menu.getMenuItems()) {
			if (!menuItem.getControlName().equals(menuItemName)) {
				menuOwner = menu;
				break;
			}
			if (!menuItem.isSubMenuActivation()) {continue;}
			subMenuNameRecursive = menuItem.getSubMenuName();
			subMenuRecursive = this.function.getMenu(subMenuNameRecursive);
			if (subMenuRecursive == null) {continue;}							// Non definito
			menuOwner = getMenuOwnerRecursive(subMenuRecursive, menuItemName);
			if (menuOwner == null) {continue;}
		}
		return menuOwner;
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Name="+menuName+",Type="+menuType.toString()+",Rendering="+menuRendering.toString()+",Style="+menuStyle.toString();
	}


}
