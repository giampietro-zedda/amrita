package forward;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;

/**
 * copyright (c) 2010-2011 e-Amrita - Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardPanelMenu
 * </h1>
 * <p>
 * This class implements all forward menu panels in a general way as listed in {@link EnumForwardMenuOptions}<br>
 * <p>
 * As any custom implementation, as this one is just a project rule, forward lets you to tailor, if it needs, all parameters<br>
 * directly on the java object used.<br>
 * So it's possible obtain directly any object {@link JComponent} inside menu and modify directly all native properties.<br>
 * <pre>
 *  MENU_TYPE_PLAIN_BUTTONS 
 *  MENU_TYPE_SECTIONS_HIDE 
 *  MENU_TYPE_TOOLBAR 
 *  MENU_TYPE_MENUBAR
 * </pre>
 * <p>
 * <h4>MENU_TYPE_PLAIN_BUTTONS</h4><br>
 * it's a simple list of buttons that you can put <code>VERTICAL</code> or <code>HORIZONTAL</code>.<br>
 * In the simpler mode it's a one level menu of buttons.<br>
 * In the more complex mode is a multilevel menu of buttons sensitive to mouse moving.<br>
 * Mouse cursor moving causes a specific submenu panel to be showed just below, or beside of the menu item around the cursor.<br>
 * The panel showed is a normal panel, in a specific frame, that will be closed on click on leaf item or  due the mouse moving on another menu item.<br>
 * 
 * <p>
 * 
 * <h4>MENU_TYPE_SECTIONS_HIDE</h4><br>
 * it's a  <code>VERTICAL</code> menu realized with a cascade of couples of panels laid with <code>BOX_LAYOUT</code> layout manager.<br>
 * Each couple of panels is a thin panel, with just the caption of first level, followed by another panel, with a further detail,<br>
 * followed by a second couple and so on.<br>
 * Click on thin panel lets to toggle the panel detail, that will be visible or unvisible.<br>
 * <p>
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardPanelMenu
 *
*/

public class ForwardPanelMenu extends ForwardPanel implements Serializable, MouseListener {
	
	final static Color GREY = 	new Color(55, 55, 55);						// Grigio
	final static Color ORANGE = new Color(255, 170, 8);						// Arancione
	final static Color WHITE = 	new Color(255, 255, 255);					// Bianco
	final static Color BLUE = 	new Color(0, 0, 204);						// Blue

	private static final long serialVersionUID = 1L;
	
	private ForwardMenu menuLevel0 = null; 									// Descrittore menu di primo livello dichiarato a livello di funzione
	private JPanel panelMenu = null;										// Pannello di menu restituito all'applicazione pronto da disporre sul layout
	private int activeMenuLevel = 0;                           	 			// Livello di menu attivo, 0 = radice
	private int maxMenulevel = 0; 											// Livello nesting massimo menù, 0-based
	private ArrayList<JFrame> al_activeJFrame = null;           			// JFrame attivi contenenti i menu dal livello 1 ai successivi
	private JMenuBar jmenuBar = null;  										// Oggetto JMenuBar sempre istanziato e disponibile run time                   
	private JToolBar jtoolBar = null;  										// Oggetto JToolBar istanziato e disponibile run time in caso di panel menu toolbar                  
	
	// Working
	Map<String, JPanel> hs_panelSection = null;								// Key=name button show/hide panel  Data=JPanel
	
	
	/**
	 * Creates a {@link ForwardPanelMenu} object that inherits from a standard {@link ForwardPanel} class.<br>
	 * The descriptor of the menu, as an object {@link ForwardMenu}, will be used to build the menu.<br>
	 * As result, will be available for the application, a panel ready to be layed out that can be get by  <br>
	 * getGraphicObject() for menu implemented by a {@link JPanel}<br>
	 * <p>
	 * For menuBar menus <code>getJMenubar()</code> will return the {@link JMenuBar} object for setting on {@link JFrame}<br>
	 * For toolBar menus <code>getJToolBar()</code> will return the {@link JToolBar} object too<br>
	 * <p>
	 * @param forwardMonitor the {@link ForwardMonitorDesktop} active at runtime
	 * @param panelName the panel name with menu structure to be layed on
	 * @param menuLevel0 as a {@link ForwardMenu} object built at declaring time
	 */
	public ForwardPanelMenu(ForwardMonitorDesktop forwardMonitor, String panelName, ForwardMenu menuLevel0) {
		JToolBar jtoolBar = null;
		
		this.setForwardMonitor(forwardMonitor);
		this.setType(EnumForwardPanelType.MENU);
		this.setName(panelName);
		this.setGraphicObject(new JPanel());
		this.menuLevel0 = menuLevel0;
		this.al_activeJFrame = new ArrayList<JFrame> ();
		this.maxMenulevel = this.menuLevel0.getMenuLevelMax();
		this.hs_panelSection = new  HashMap<String, JPanel> ();

		// Costruzione panel menu di semplici pulsanti di uno o + livelli
		if (menuLevel0.getMenuRendering() == EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS) {
			JPanel jpanelMenu = (JPanel) this.getGraphicObject();
			buildMenuPlainLevelX(jpanelMenu, menuLevel0, true);
			return;
		}
		
		
		// Costruzione panel menu con sezioni a scomparsa.
		if (menuLevel0.getMenuRendering() == EnumForwardOption.MENU_RENDERING_SECTIONS_HIDE) {
			buildMenuSectionsHide(menuLevel0);
			return;
		}
		
		// Costruzione panel menu toolbar
		// L'oggetto JToolBar è già stato istanziato in fase di dichiarazione della funzione e memorizzato nell'oggetto menu
		// Sempre l'oggetto menù restituisce la toolbar pronta e popolata con i pulsanti, separator e altro
		// In questa sede viene confezionato il pannello contenente la toolbar, pronto per essere disposto nella GUI
		if (menuLevel0.getMenuRendering() == EnumForwardOption.MENU_RENDERING_TOOLBAR) {
			jtoolBar = menuLevel0.getJToolbarMenu();
			JPanel jpanelToolBar = (JPanel) this.getGraphicObject();
			jpanelToolBar.setLayout(new BorderLayout());
			jpanelToolBar.add(jtoolBar, BorderLayout.PAGE_START);
			return;
		}
		
	}
    
	

	/**
	 * Get panel menu.<br>
	 * <br>
	 * The panel is ready to be layed out by forward monitor, as declared by function.<br>
	 * <p>
	 * 
	 * @return panel to lay out
	 */
	public JPanel getPanelMenu() {
		return panelMenu;
	}
	
	/**
	 * Get panel menu.<br>
	 * <br>
	 * The panel is ready to be layed out by forward monitor, as declared by function.<br>
	 * <p>
	 * 
	 * @return panel to lay out
	 */
	public void dropActiveSubmenus() {
		for (JFrame activeJFrameMenu : this.al_activeJFrame) {
			activeJFrameMenu.setVisible(false);
		}
		return;
	}
	
	
    /**
     * Get the level max of menu.<br>
     * <p>
	 * @return the maxMenulevel
	 */
	public int getMaxMenulevel() {
		return maxMenulevel;
	}

	/**
     * Set the level max of menu.<br>
     * <p>
	 * @param maxMenulevel the maxMenulevel to set
	 */
	public void setMaxMenulevel(int maxMenulevel) {
		this.maxMenulevel = maxMenulevel;
	}


	/**
	 * Get the object {@link JMenuBar} for the current menu.<br>
	 * <p>
	 * A standard {@link JMenuBar} menu will be always created from any menu declaring,<br>
	 * as a reference for all menus types. Informations to build different types of menus<br>
	 * are the same.<br>
	 * The menu bar will be set directly on {@link JFrame} of the function.
	 * <p>
	 * @return the jmenuBar
	 */
	public JMenuBar getJMenuBar() {
		return jmenuBar;
	}


	/**
	 * Set the object {@link JMenuBar} for the current menu.<br>
	 * <p>
	 * A standard {@link JMenuBar} menu will be always created from any menu declaring,<br>
	 * as a reference for all menus types. Informations to build different types of menus<br>
	 * are the same.<br>
	 * The menu bar will be set directly on {@link JFrame} of the function.
	 * <p>
	 * @param jmenuBar the jmenuBar to set
	 */
	public void setJMenuBar(JMenuBar jmenuBar) {
		this.jmenuBar = jmenuBar;
	}

	
	

	/**
	 * Get the object {@link JToolBar} for the current menu.<br>
	 * <p>
	 * A standard {@link JMenuBar} menu will be always created from any menu declaring,<br>
	 * as a reference for all menus types. Informations to build different types of menus<br>
	 * are the same.<br>
	 * If the panel is describing a toolbar menu, it will be generated a standard swing toolbar.
	 * Normally a toolbar is made of buttons and sometimes of text field or comboBox. So not all <br>
	 * types of menu can be exchanged with a toolbar menu.<br>
	 * Only elementary menu items definitions at the first level will be used and whatever else will be<br>
	 * discarded.<br>
	 * <p>
	 * @return the jtoolBar
	 */
	public JToolBar getJToolBar() {
		return jtoolBar;
	}



	/**
	 * Set the object {@link JToolBar} for the current menu.<br>
	 * <p>
	 * A standard {@link JMenuBar} menu will be always created from any menu declaring,<br>
	 * as a reference for all menus types. Informations to build different types of menus<br>
	 * are the same.<br>
	 * If the panel is describing a toolbar menu, it will be generated a standard swing toolbar.
	 * Normally a toolbar is made of buttons and sometimes of text field or comboBox. So not all <br>
	 * types of menu can be exchanged with a toolbar menu.<br>
	 * Only elementary menu items definitions at the first level will be used and whatever else will be<br>
	 * discarded.<br>
	 * <p>
	 * @param jtoolBar the jtoolBar to set
	 */
	public void setJToolBar(JToolBar jtoolBar) {
		this.jtoolBar = jtoolBar;
	}


	
	

	/**
	 * Get an array list with active JFrame.<br>
	 * <p>
	 * It'used for a correct disposition of submenu with menus implemented by PLAIN_BITTONS.<br>
	 * <p>
	 * @return the al_activeJFrame
	 */
	public ArrayList<JFrame> getActivesJFrame() {
		return al_activeJFrame;
	}



	/* ------------------------------------------------------------
	 * Gestione click su pulsante hide/show di section a scomparsa
	 * ------------------------------------------------------------
	 * 
	 * 
	 */
	public void mouseClicked(MouseEvent e) {

		Object objComponent = null;
        JButton btn = null;
		JPanel jpanelSection = null;
        
        objComponent = e.getSource();
		
		// Non è sicuramente da trattare
		if (!(objComponent instanceof JButton)) {return;}
		btn = (JButton) objComponent;
		jpanelSection = this.hs_panelSection.get(btn.getName());
		if (jpanelSection == null) {return;}
		
		// Show panel section
		if (e.getClickCount() == 1) {
			jpanelSection.setVisible(true);
			return;
		}
		
		// Hide panel section
		if (e.getClickCount() == 2) {
			jpanelSection.setVisible(false);
			return;
		}
		

	}
	
	
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
	
	/**
	 * Gestione uscita del mouse
	 */
	public void mouseExited(MouseEvent e) {
				
	}
	
	
   /**
	* Gestione ingresso mouse per tutti i tipi di menu NON menubar.<br>
	* <p>
	* 
	* Si gestisce l'attivazione di un submenu in un nuovo frame<br>
	* Si gestisce l'oscuramento dei submenu non necessari<br>
	* Si aggiornano le variabili correnti di gestione<br>
	* 
	*/
	public void mouseEntered(MouseEvent e) {
		ForwardMenu subMenuToCall = null;
		ForwardMenu menuOwner = null;
		ForwardMenuItem menuItem = null;
		
		JFrame frameMenuLevelActive = null;
		JMenuBar frameMenuBar = null;
		int heighMenuBar = 0;
		Container parent = null;
		String subMenuName = "";
		String controlName = "";
		int insetAdjustmentX = 0;															// 
		int insetAdjustmentY = 0;
	    int levelMenuItem = 0;
		int i = 0;
		
		// Identificazione control
	    JComponent btnLabel = (JComponent) e.getSource();
	    controlName = btnLabel.getName();
	    
	    // Mouse entered su submenu item
	    frameMenuLevelActive = this.al_activeJFrame.get(this.activeMenuLevel);
	    frameMenuBar = frameMenuLevelActive.getJMenuBar();
	    if (frameMenuBar != null) {
	    	insetAdjustmentY = insetAdjustmentY + frameMenuBar.getHeight();
		}
	    
	    
	    // Calcolo coordinate X e Y del pulsante di mouse entered rispetto al frame principale
	    parent =  btnLabel.getParent();
	    while (parent!= null) {
	    	insetAdjustmentX = insetAdjustmentX + parent.getX();
		    insetAdjustmentY = insetAdjustmentY + parent.getY();
	    	parent = parent.getParent();
		}

	    // Se presente menubar nel conteggio insetAdjustmentY l'altezza menubar è di troppo
    	if (frameMenuLevelActive.getJMenuBar() != null) {
    		heighMenuBar = frameMenuLevelActive.getJMenuBar().getHeight() * -1;
		}

    	// Descrittore menu e descrittore menu item
    	menuOwner = this.getFunction().getMenuOwner(controlName);
    	menuItem = menuOwner.getMenuItem(controlName);
    	
	    // Mouse entered su item di livello superiore a quello attivo.
	    // Hiding di tutti i submenu successivi.
    	if (menuOwner != null) {
    		levelMenuItem = menuOwner.getMenuLevel();
		}
	    if (levelMenuItem < this.activeMenuLevel ) {
	    	while (levelMenuItem < this.al_activeJFrame.size() - 1) {
	    		i = this.al_activeJFrame.size() - 1;
				JFrame jframeToHide = this.al_activeJFrame.get(i);
				jframeToHide.setVisible(false);
	    		this.al_activeJFrame.remove(i);
			}
			this.activeMenuLevel = levelMenuItem;
			return;
	    }

	    // Mouse entered su submenu da attivare in un nuovo frame.
		subMenuName = menuItem.getSubMenuName(); 
		subMenuToCall = this.getFunction().getMenu(subMenuName);
	    if (subMenuToCall != null && subMenuToCall.getMenuType() == EnumForwardOption.MENU_TYPE_SUBMENU) {
	    	JFrame frameMenuLevelx = new JFrame();
	    	JPanel jpanelMenu = new JPanel();
	    	buildMenuPlainLevelX(jpanelMenu, subMenuToCall, false);
	        frameMenuLevelx.add(jpanelMenu);
	        frameMenuLevelx.setUndecorated(true);
	        frameMenuLevelx.setLocationRelativeTo(frameMenuLevelActive);
	        // Menu root disposto verticalmente: submenu sempre a destra del pulsante/label
	        if (this.menuLevel0.getMenuDirection() == EnumForwardOption.MENU_DIRECTION_VERTICAL) {
		        frameMenuLevelx.setLocation(btnLabel.getX()+btnLabel.getWidth()+insetAdjustmentX, btnLabel.getY()+insetAdjustmentY+heighMenuBar); 
			} 
	        // Menu root disposto hrizzontalemnte: primo submenu sempre sotto il pulsante/label, poi sempre a destra del pulsante/label
	        if (this.menuLevel0.getMenuDirection() == EnumForwardOption.MENU_DIRECTION_HORIZONTAL) {  // Livello 1 (0-based): submenu sotto il pulsante/label
	        	// Attivo il livello 0 orizzontale: il sottomenu si dispone sotto il pulsante corrente
	        	if (levelMenuItem == 0) {
					frameMenuLevelx.setLocation(btnLabel.getX()+insetAdjustmentX, btnLabel.getY()+btnLabel.getHeight()+insetAdjustmentY+heighMenuBar);
				} else {
					// Livello > 0: il submenu si dispone alla sua destra 
					frameMenuLevelx.setLocation(btnLabel.getX()+btnLabel.getWidth()+insetAdjustmentX, btnLabel.getY()+insetAdjustmentY+heighMenuBar); 
				}
	        }
            // Si rende visibile il frame con il submenu			 
	        frameMenuLevelx.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	        frameMenuLevelx.pack();
	        frameMenuLevelx.setAlwaysOnTop(true);
	        frameMenuLevelx.setVisible(true);
	        this.al_activeJFrame.add(frameMenuLevelx);
	        this.activeMenuLevel = this.al_activeJFrame.size() - 1;
	        return;
		} 
	    
	} 
	
	   /**
		* Makes unvisibles all deeper menus <br>
		* <p>
		* 
		*/
		public void hideSubmenusOpen() {
			int i = 0;
			
	    	while (0 < this.al_activeJFrame.size() - 1) {
	    		i = this.al_activeJFrame.size() - 1;
				JFrame jframeToHide = this.al_activeJFrame.get(i);
				jframeToHide.setVisible(false);
	    		this.al_activeJFrame.remove(i);
			}
			this.activeMenuLevel = 0;

		}


	/* ------------------------------------------------------------------------------
     * Costruzione singolo livello di menu semplice come sequenza pulsanti/labels.
     * ------------------------------------------------------------------------------
     * 
     * Vengono disposti i pulsanti o le label nel pannello fornito.
     * Metodo richiamato per disporre qualsiasi livello di menu.
     * All'enter del mouse su un item si apre l'eventuale sottomenu con listener in questa classe.
     * Al click su un item foglia si attiva la logica dichiarata con listenere in ForwardMonitor
     * Gestione scomparsa automatica submenu all'enter del mouse
     */
	private void buildMenuPlainLevelX(JPanel jpanelMenu, ForwardMenu menuLevel0, boolean isRoot) {
		
		JButton btn = null;
		JSeparator separator = null;
	    JPanel jpanelBtn = null;
	    JPanel jpanelSeparator = null;
		
		menuLevel0.setForwardPanelMenu(this);
	    
		// Per i plain menu si utilizza sempre BOX_LAYOUT
		this.setLayoutManager(EnumForwardLayout.BOX_LAYOUT);
		
		// Impostazione corretto layout manager
		if (!isRoot) {
			jpanelMenu.setLayout(new BoxLayout(jpanelMenu, BoxLayout.Y_AXIS));
		} else {
			if (menuLevel0.getMenuDirection() == EnumForwardOption.MENU_DIRECTION_VERTICAL) {
				jpanelMenu.setLayout(new BoxLayout(jpanelMenu, BoxLayout.Y_AXIS));
			} else {
				jpanelMenu.setLayout(new BoxLayout(jpanelMenu, BoxLayout.X_AXIS));
			}
		}
		
		// Scan menu items da disporre nel pannello del menu in input
		for (ForwardMenuItem menuItem : menuLevel0.getMenuItems()) {
			
			// Separator
			if (menuItem.getComponentType() == EnumForwardComponent.JSeparator) {
				separator = new JSeparator();
				menuItem.setGraphicObject(separator);
                if (!isRoot || menuLevel0.getMenuDirection() == EnumForwardOption.MENU_DIRECTION_VERTICAL) {
                	separator.setOrientation(SwingConstants.HORIZONTAL);
				} else {
					separator.setOrientation(SwingConstants.VERTICAL);
				}
                jpanelSeparator = new JPanel(new BorderLayout());						// Per forzare automaticamente larghezza max disponibile
                jpanelSeparator.add(separator, BorderLayout.CENTER);					//
				jpanelMenu.add(jpanelSeparator);										//
				continue;
			}
			
			// Menu di pulsanti
			if (menuLevel0.getMenuRendering() == EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS) {
				// Skip se non è una dichiarazione di menu item
				if (menuItem.getComponentType() != EnumForwardComponent.JMenuItem) {
					continue;
				}
				btn = (JButton) menuItem.getGraphicObject();			// Si considera l'oggetto JButton istanzato in dichiarazione con il listener già OK
 				// Se attivazione submenu la caption è nel descrittore del submenu
 				if (menuItem.isSubMenuActivation()) {
					ForwardMenu subMenu = this.getForwardMonitor().mcb.function.getMenu(menuItem.getSubMenuName());
					menuItem.setSubMenu(subMenu);
					btn.setText(subMenu.getMenuTitle());
				}
				btn.setAlignmentX(Component.LEFT_ALIGNMENT);	
				// Se caption non fornita si utilizza il nome del controllo
				if (menuItem.getCaption().equals("")) {
					btn.setText((menuItem.getControlName()));
				}
				// Se dichiarata una icona non viene considerata in questo tipo di menu
				btn.setIcon(null);
				
				// Stile pulsante di menu
				setLookStyleMenuItem(btn, menuLevel0);
			
				// Per forzare automaticamente larghezza max disponibile
	            jpanelBtn = new JPanel(new BorderLayout());							 
	            jpanelBtn.add(btn, BorderLayout.CENTER);							 
	            jpanelBtn.setName("Service");
				
	            // Mouse listeners registrati per il pulsante
				MouseListener[] ar_mouseListener = btn.getListeners(MouseListener.class);
				List<?> li_listener = Arrays.asList(ar_mouseListener);
				
				// Se si tratta di menu item foglia l'ascoltatore è il monitor  e tipicamente verrà intercettato il click per attivare una funzionalità
				// Il listener sul menu item foglia è stato registrato dal monitor sulla base delle logiche dichiarate 
				// Se invece si tratta di un menu item di apertura di sottomenu allora la gestione è tutta interna a questa classe, 
				// che deve aprire il sottomenu ascoltando MouseListener (MouseEntered)
	            if (!li_listener.contains(this)) {
					btn.addMouseListener(this);
				}
				jpanelMenu.add(jpanelBtn);	
				 
				// Se menu verticale allineo a sinistra
				if (menuLevel0.getMenuDirection() == EnumForwardOption.MENU_DIRECTION_VERTICAL) {
					jpanelMenu.add(Box.createHorizontalGlue());
				}
			}
		}
	}
	
	
	/*
	 * Impostazione spessore e colore menu in base allo stile
	 */
	private void setLookStyleMenuItem(JButton btn, ForwardMenu menuLevel0) {
		Dimension newDim = null;
		
		btn.setHorizontalAlignment(SwingConstants.LEADING);
		
		// Stile
		switch (menuLevel0.getMenuStyle()) {
				case MENU_STYLE1:
					btn.setBackground(GREY);													// Grigio
					btn.setForeground(ORANGE);													// Arancione
					break;
				case MENU_STYLE2:
					btn.setBackground(ORANGE);													// Arancione
					btn.setForeground(GREY);													// Grigio
					break;
				case MENU_STYLE3:
					btn.setBackground(GREY);													// Grigio
					btn.setForeground(WHITE);													// Bianco
					break;
				case MENU_STYLE4:
					btn.setBackground(WHITE);													// Bianco
					btn.setForeground(GREY);													// Grigio
					break;
				case MENU_STYLE5:
					btn.setBackground(BLUE);													// BLU
					btn.setForeground(WHITE);													// Bianco
					break;
				case MENU_STYLE6:
					btn.setBackground(WHITE);													// Bianco
					btn.setForeground(BLUE);													// Blu
					break;
				default:
					btn.setBackground(GREY);													// Grigio
					btn.setForeground(ORANGE);													// Arancione
					break;
				}

		
		// Spessore
		switch (menuLevel0.getMenuHeigh()) {
				case MENU_HEIGH_LARGE:
					btn.setPreferredSize(new Dimension(btn.getPreferredSize().width, 128));		// Large
					btn.setMinimumSize(new Dimension(btn.getPreferredSize().width, 128));		// Large
					btn.setMaximumSize(new Dimension(btn.getPreferredSize().width, 128));		// Large
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					break;
				case MENU_HEIGH_MEDIUM:
					btn.setPreferredSize(new Dimension(btn.getPreferredSize().width, 64));		// Medium
					btn.setMinimumSize(new Dimension(btn.getPreferredSize().width, 64));		// Medium
					btn.setMaximumSize(new Dimension(btn.getPreferredSize().width, 64));		// Medium
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
 					break;
				case MENU_HEIGH_THIN:
					btn.setPreferredSize(new Dimension(btn.getPreferredSize().width, 32));		// Thin
					btn.setMinimumSize(new Dimension(btn.getPreferredSize().width, 32));		// Thin
					btn.setMaximumSize(new Dimension(btn.getPreferredSize().width, 32));		// Thin
 					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					break;
				case MENU_HEIGH_EXTRA_THIN:
					btn.setPreferredSize(new Dimension(btn.getPreferredSize().width, 16));		// Extra thin
					btn.setMinimumSize(new Dimension(btn.getPreferredSize().width, 16));		// Extra thin
					btn.setMaximumSize(new Dimension(btn.getPreferredSize().width, 16));		// Extra thin
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					break;
				default:
					btn.setPreferredSize(new Dimension(btn.getPreferredSize().width, 64));		// Large
					btn.setMinimumSize(new Dimension(btn.getPreferredSize().width, 64));		// Medium
					btn.setMaximumSize(new Dimension(btn.getPreferredSize().width, 64));		// Medium
					btn.setFont(btn.getFont().deriveFont(Font.BOLD));
					break;
		}
		
		// Necessario allargare il pulsante a causa della NON visualizzazione del test finale causa BOLD
		// Sembrerebbe una anomalia Java, da verificare
		newDim = new Dimension(btn.getPreferredSize().width+10, btn.getPreferredSize().height);
		btn.setPreferredSize(newDim);
	}

	

	/* ------------------------------------------------------------------------------
     * Costruzione menu con sezioni a scomparsa
     * ------------------------------------------------------------------------------
     * 
     * Si tratta di un menu strutturato su due livelli.
     * Il primo livello di menu contiene tutti menuItem di attivazione submenu.
     * Per ogni submenu si recuperano i suoi item foglia e viene:
     *    Accodato un JButton nel pannello di menu
     *    Creato un pannello di item di secondo livello accodati verticalmente
     *    Per ogni item viene generata una label con l'item foglia e i listener del caso
     *    Accodato il pannello al pannello di menu
     *    A fronte di click sul pulsane viene reso visibile il pannello sottostante
     *    A fronte di doubleClick sul pulsane viene reso invisibile il pannello sottostante
     * Nella forma iniziale il menu si presenta con n pulsanti in sequenza verticale
     */ 
	private void buildMenuSectionsHide(ForwardMenu menuLevel0) {
		
		JPanel jpanelMenu = null;
		JPanel jpaneSectionHide = null;
	    JPanel jpanelBtn = null;
	    JPanel jpanelLbl = null;
		JButton btn = null;
		JLabel lbl = null;
		
		ForwardMenu subMenu = null;
		
		jpanelMenu = (JPanel) this.getGraphicObject();
		
		
		// Impostazione layout manager
		this.setLayoutManager(EnumForwardLayout.BOX_LAYOUT);
		jpanelMenu.setLayout(new BoxLayout(jpanelMenu, BoxLayout.Y_AXIS));
		
		// Scan menu items da disporre nel pannello del menu in input
		for (ForwardMenuItem menuItem : menuLevel0.getMenuItems()) {
			
			// Interessano solo i menuItem di attivazione subMenu
			if (menuItem.getComponentType() != EnumForwardComponent.JMenuItem
			|| !menuItem.isSubMenuActivation()) {
				continue;
			}

			// Jbutton di hide/unhide section
			btn = new JButton();
			btn.setName(menuItem.getControlName());
			btn.setText(menuItem.getCaption());
			btn.setSize(btn.getWidth(), 30);
			btn.addMouseListener(this);												// Gestione click e doppio click per show/Hide section
            jpanelBtn = new JPanel(new BorderLayout());								// Per forzare automaticamente larghezza max disponibile
            jpanelBtn.add(btn, BorderLayout.CENTER);								//
			jpanelMenu.add(jpanelBtn);												//
			
			// JPanel section
			jpaneSectionHide = new JPanel();
			jpaneSectionHide.setLayout(new BoxLayout(jpaneSectionHide, BoxLayout.Y_AXIS));
			this.hs_panelSection.put(menuItem.getControlName(), jpaneSectionHide);
			
			// Scan menuItem subMenu
			subMenu = menuItem.getSubMenu();
			for (ForwardMenuItem subMenuItem : subMenu.getMenuItems()) {
				// Interessano solo i menuItem foglia
				if (subMenuItem.getComponentType() != EnumForwardComponent.JMenuItem
				||  subMenuItem.isSubMenuActivation()) {
					continue;
				}

				// JLabel con menuItem foglia.
				lbl = new JLabel();
				lbl.setName(subMenuItem.getControlName());
				lbl.setText(subMenuItem.getCaption());
				lbl.setBackground(Color.WHITE);
				lbl.setForeground(Color.ORANGE);
				lbl.addMouseListener(this.getForwardMonitor());						// Il listener è il monitor che attiverà le logiche ON_EVENT()
	            jpanelLbl = new JPanel(new BorderLayout());							// Per forzare automaticamente larghezza max disponibile
	            jpanelLbl.add(lbl, BorderLayout.CENTER);							//
	            jpaneSectionHide.add(jpanelLbl);									//
			}
		}
	}



	
	

}
