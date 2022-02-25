package forward;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JApplet;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;



/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardForm
 * </h1>
 * <p>
 * Questa classe descrive un form applicativo in termini di pannelli, della loro tipologia e <br>
 * delle relazioni fra gli stessi, governate dall'opportuno gestore di layout.<br>
 * Per disporre pannelli vengono utilizzati principalmente i gestori di layout <tt>BORDER_LAYOUT</tt><br>
 * <tt>BOX_LAYOUT</tt> e <tt>CARD_LAYOUT</tt>
 * <p>
 * Per i pannelli di dettaglio, che contengono controlli Swing, il gestore di layout scelto serve a disporre i<br>
 * controlli per riga e nell'ambito della riga.<vr>
 * <p>
 * Dal punto di vista java un form è rappresentato da un pannello, istanza di {@link JPanel}, che può essere<br>
 * direttamente disposto nel pannello dei contenuti principale o meno. Tale pannello contiene e descrive tutti i<br>
 * i pannelli disposti al suo interno ricorsivamente per ottenere l'interfaccia Gui desiderata.<br>
 * <br>
 * Non si entra nel merito del contenuto dei vari pannelli ma ci si concentra sulla sola struttura<br>
 * dei contenitori componenti il form, che puà assumere una natura ricorsiva.<br>
 * <br>
 * Viene gestita la memorizzazione degli oggetti java container, tipicamente {@link JPanel}, <br>
 * con tutte le informazioni necessarie per un corretto rendering.<br>
 * La scelta tecnica è quella di descrivere e formalizzare le informazioni essenziali per la<br>
 * gestione dell'interfaccia GUI. Qualsiasi parametro specifico di eccezione o particolare, viene impostato<br>
 * da codice java codificato nell'apposito metodo a livello di descrittore della funzione applicativa.<br>
 * Questo approccio consente il massimo risultato con il minimo sforzo.<br>
 * <p>
 * La disposizione dei campi e degli oggetti grafici all'interno dei un pannello, dipende dalla <br>
 * tipologia applicativa del pannello e viene effettuata con appositi gestori di layout all'interno <br>
 * del pannello stesso.<br>
 * <p>
 * A partire dal pannello principale, ovvero il Main panel direttamente associato al contenitore <br>
 * JFrame o JApplet, vengono disposti ricorsivamente i pannelli contenuti, utilizzando il gestore di <br>
 * layout fornito nel descrittore della funzione<br>
 * <p>
 * I Gestori di layout utilizzati sono:
 * <Ul> 
 * <Li> <b>BORDER_LAYOUT</b> <br>
 * Si utilizza in generale per permettere ai pannelli di prendere tutto lo spazio a loro necessario.<br>
 * In particolare si utilizza per disporre pannelli nella classica condìfigurazione NORD, SUD, EST, OVEST<br>
 * e per permettere l'inserimento della toolbar removibile.<br>
 * </Li>
 * <Li> <b>BOX_LAYOUT</b> <br>
 * Si utilizza per disporre dei pannelli in orizzantale ma più frequentemente verticalmente, <br>
 * uno sotto l'altro, per realizzare tipiche interfacce formate da pannelli indipendenti.<br> </Li>
 * <Li> <b>CARD_LAYOUT</b> <br>
 * Si utilizza per disporre dei pannelli sovrapposti come schede.<br> </Li>
 * </Ul>
 * 
 *  
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/

public class ForwardForm implements Serializable {
	
	// Asse verso cui disporre i componenti per i layout che lo richiedono
	public static final int AXIS_DEFAULT = 0;
	public static final int AXIS_HORIZONTAL = 1;
	public static final int AXIS_VERTICAL = 2;
	
	private static final long serialVersionUID = 1L;
	
	private ForwardFunction function = null; 						// Funzione dove il form è definto
	private EnumForwardOption formType = null; 						// Tipologia form
	private String formName = "";                                   // Nome form
	private String rootPanelName = "";                              // Nome pannello principale
	private String menuBarName = "";                                // Nome menu bar da visualizzare nel frame del form
	private boolean isMenuBar = false;                              // True indica frame con menu bar
	private JPanel jrootPanel = null;                              	// Oggetto JPanel pannello principale, direttamente disposto in JFrame o JApplet o JDialog o JTabbedPane
    private ForwardDialog dialog = null;							// Dialogo se formType=FORM_TYPE_DIALOG
	private Map<String, InnerForwardPanel> hs_panel = null;			// Key=panelName Data=panel info & panels children names
    
	
    /**
     * Costruttore.
     * @param formName
     */
	public ForwardForm(String formName, EnumForwardOption formType, String menuBarName) {
		this.formType = formType;
		this.formName = formName;
		this.menuBarName = menuBarName;
		if (menuBarName != null && !menuBarName.equals("")) {
			this.isMenuBar = true;
		}
		this.hs_panel = new HashMap<String, InnerForwardPanel> ();
	}
	
	/**
	 * Add a panel, with own informations and related children in a form.<br>
	 * <p>
	 * It's necessary the parent panel name, where to lay children panels and the layout manager to use.<br>
	 * Then are necessary essential data to lay the container.<br>
     * <br>
     * At the first method call, the panel name become the main panel, laid directly in the main content panel,<br>
     * {@link JFrame} or {@link JApplet}.<br>
 	 * <p>
	 * @param panelAncestorName
	 * @param panelName
	 * @param panelType 
	 * @param jpanel as a {@link JPanel} object 
	 * @param activePanel 
	 * @param layoutManager as a {@link EnumForwardLayout} object
	 * @param int boxLayout_Axis as ForwardFormLayout.AXIS_DEFAULT or AXIS_HORIZONTAL or AXIS_VERTICAL  
	 * @param ar_panelChild the the string array of panels child names to add
	 */
	public void addPanel(String panelName				    // Panel genitore da inserire
					   , EnumForwardPanelType panelType		// Tipo pannello
					   , JComponent jpanel                  // Oggetto JPanel
					   , ForwardPanel forwardPanel			// Panel genitore come codificato da forward
					   , EnumForwardLayout layoutManager	// Layout manager
					   , int layoutAxis         			// ForwardFormLayout.AXIS_DEFAULT AXIS_HORIZONTAL AXIS_VERTICAL  
			           , String ... ar_panelChild			// Pannelli figli da collocare in panelParent
			            ) {
		
		InnerForwardPanel innerForwardPanel = null;
		String panelNorth = "";
		String panelCenter = "";
		String panelSouth = "";
		String panelEast = "";
		String panelWest = "";
		
		// Prima esecuzione, pannello radice
		if (rootPanelName.equals("")) {
			jrootPanel = (JPanel) jpanel;
			rootPanelName = panelName;
		}
		
		// Inserimento pannello in struttura hash del form
		innerForwardPanel = hs_panel.get(panelName);
		if (innerForwardPanel == null) {
			innerForwardPanel = new InnerForwardPanel();			
			hs_panel.put(panelName, innerForwardPanel);
		}
		
		// Valorizzazioni per Flow, Card e Box layout e generale
		innerForwardPanel.panelName = panelName;
		innerForwardPanel.panel = forwardPanel; 
		innerForwardPanel.panel.setGraphicObject(jpanel);
		innerForwardPanel.layoutManager = layoutManager;
		innerForwardPanel.panelType = panelType;
		innerForwardPanel.layoutAxis = layoutAxis;
		List<String> li_panelChildren = (List<String>) Arrays.asList(ar_panelChild);
		innerForwardPanel.al_panelChildren.addAll(li_panelChildren);
		

		// Non è border layout: fine
		if (layoutManager != EnumForwardLayout.BORDER_LAYOUT ) {
			return;
		}
		
		// Se border layout i pannelli sono nell'ordine north, center, south, east e west 
		innerForwardPanel.al_panelChildren.clear();
		if (ar_panelChild.length >= 1) {
			panelNorth = ar_panelChild[0];
		}
		if (ar_panelChild.length >= 2) {
			panelCenter = ar_panelChild[1];
		}
		if (ar_panelChild.length >= 3) {
			panelSouth = ar_panelChild[2];
		}
		if (ar_panelChild.length >= 4) {
			panelEast = ar_panelChild[3];
		}
		if (ar_panelChild.length >= 5) {
			panelWest = ar_panelChild[4];
		}
		innerForwardPanel.panelChildrenNorth = panelNorth;
		innerForwardPanel.panelChildrenCenter = panelCenter;
		innerForwardPanel.panelChildrenSouth = panelSouth;
		innerForwardPanel.panelChildrenEast = panelEast;
		innerForwardPanel.panelChildrenWest = panelWest;
		
	}
	
	
	/**
	 * Gets the function where the form is defined.<br>
	 * <p>
	 * @return the function
	 */
	public ForwardFunction getFunction() {
		return function;
	}

	/**
	 * Sets the function where the form is defined.<br>
	 * <p>
	 * @param function the function to set
	 */
	public void setFunction(ForwardFunction function) {
		this.function = function;
	}

	/**
	 * Add a panelChildren to a panelParent, as a whole form.<br>
	 * <p>
	 * A forward form, at the end, is a panel with any subpanels.<br>
     * <br>
 	 * @param panel
	 * @param jpanel the JPanel object for panelParent
	 * @param formName the form child of panelParent
	 */
	public void addPanelAsForm(String panel						// Panel genitore da inserire
							 , JComponent jpanel          		// Oggetto JPanel
							 , String formName					// Nome form
					          ) {
		
		InnerForwardPanel innerForwardPanel = null;
		
		// Prima esecuzione, pannello radice
		if (rootPanelName.equals("")) {
			rootPanelName = panel;
		}
		
		// Inserimento pannello in struttura hash del form
		innerForwardPanel = hs_panel.get(panel);
		if (innerForwardPanel == null) {
			innerForwardPanel = new InnerForwardPanel();			
			innerForwardPanel.panelName = panel;
			innerForwardPanel.panel = new ForwardPanel(panel, EnumForwardPanelType.FORM, EnumForwardLayout.PREDEFINED_LAYOUT); // Istanzio il descrittore forward specifico del pannello
			hs_panel.put(panel, innerForwardPanel);
		}
		
		// Il descrittore del form viene valorizzato a runtime
		innerForwardPanel.panel.setGraphicObject(jpanel);
		innerForwardPanel.panelType = EnumForwardPanelType.FORM;
		innerForwardPanel.formName = formName;
	}
	
	/**
	 * Add a panelChildren to a panelParent, as a panel implementing a menu.<br>
	 * <p>
	 * The menu panel will be created thru a {@link ForwardPanelMenu} object that use as input<br>
	 * <code>MNU()</code> and <code>MENU_ITEM()</code> declarations.<br>
     * <br>
 	 * @param panel 
	 * @param jpanel the JPanel object for panelParent
	 * @param formName the form child of panelParent
	 */
	public void addPanelAsMenu(String panel 					// Panel genitore da inserire
							 , JComponent jpanel           		// Oggetto JPanel
							 , String menuName					// Nome form
					          ) {
		
		InnerForwardPanel innerForwardPanel = null;
		
		// Prima esecuzione, pannello radice
		if (rootPanelName.equals("")) {
			rootPanelName = panel;
		}
		
		// Inserimento pannello in struttura hash del form
		innerForwardPanel = hs_panel.get(panel);
		if (innerForwardPanel == null) {
			innerForwardPanel = new InnerForwardPanel();			
			innerForwardPanel.panelName = panel;
			hs_panel.put(panel, innerForwardPanel);
		}
		
		// Il descrittore del form viene valorizzato a runtime
		innerForwardPanel.panel = new ForwardPanel(panel, EnumForwardPanelType.MENU, EnumForwardLayout.PREDEFINED_LAYOUT); // Istanzio il descrittore forward specifico del pannello
		innerForwardPanel.panel.setGraphicObject(jpanel);
		innerForwardPanel.panelType = EnumForwardPanelType.MENU;
		innerForwardPanel.menuName = menuName;
	}
	
	
	

	/**
	 * Gets the form type as coded by forward.<br>
	 * <p>
	 * @return the formType
	 */
	public EnumForwardOption getFormType() {
		return formType;
	}

	/**
	 * Sets the form type as coded by forward.<br>
	 * <p>
	 * @param formType the formType to set
	 */
	public void setFormType(EnumForwardOption formType) {
		this.formType = formType;
	}

	/**
	 * Restituisce il nome del form contenente i pannelli
	 * coordinati da specifici gestori di layout<br>
	 * <p>
	 * @return the formName
	 */
	public String getFormName() {
		return formName;
	}

	/**
	 * Imposta il nome del form contenente i pannelli
	 * coordinati da specifici gestori di layout<br>
	 * <p>
	 * @param formName the formName to set
	 */
	public void setFormName(String formName) {
		this.formName = formName;
	}

	/**
	 * Restituisce il nome del pannello radice.<br>
	 * <p>
	 * Si tratta del pannello direttamente disposto nel pannello<br>
	 * dei contenuti principale, JFrame o JApplet.<br>
	 * A partire dal nome di questo pannello si può conoscere la struttura<br>
	 * completa della deisposizione generale dei pannelli, attraverso il metodo<br>
	 * <code>getChildren()</code>
	 * <p<
	 * @return the panelMain
	 */
	public String getRootPanelName() {
		return rootPanelName;
	}

	
	
	
	/**
	 * Gets the {@link JPanel} object for the form root panel.<br>
	 * <p>
	 * Root panel form is the panel directly laid out the main container JFrame, JApplet, JDialog<br>
	 * that contains all recursively declared application panel.<br>
	 * A root panel form can be laid out in another panel too.<br>
	 * <p>
	 * @return the jrootPanel
	 */
	public JPanel getJrootPanel() {
		return jrootPanel;
	}

	/**
	 * Sets the {@link JPanel} object for the form root panel.<br>
	 * <p>
	 * Root panel form is the panel directly laid out the main container JFrame, JApplet, JDialog<br>
	 * that contains all recursively declared application panel.<br>
	 * A root panel form can be laid out in another panel too.<br>
	 * <p>
	 * @param jrootPanel the jrootPanel to set
	 */
	public void setJrootPanel(JPanel jrootPanel) {
		this.jrootPanel = jrootPanel;
	}

	
	
	/**
	 * Gets the menuBar name to be attached to form frame.<br>
	 * <p>
	 * 
	 * @return the menuBarName
	 */
	public String getMenuBarName() {
		return menuBarName;
	}

	/**
	 * Sets the menuBar name to be attached to form frame.<br>
	 * <p>
	 * @param menuBarName the menuBarName to set
	 */
	public void setMenuBarName(String menuBarName) {
		this.menuBarName = menuBarName;
	}

	/**
	 * Gets if the form is with the menubar on the frame.<br>
	 * <p>
	 * @return a boolean true if menubar is active
	 */
	public boolean isMenubar() {
		return this.isMenuBar;
	}

	/**
	 * Sets if the form is with the menubar on the frame.<br>
	 * <p>
	 * @return a boolean true if menubar is active
	 */
	public void setMenuBar(boolean isMenuBar) {
		this.isMenuBar = isMenuBar;
		return;
	}


	/**
	 * Gets the {@link ForwardDialog} object, that inherits from {@link JDialog}<br> for the form.<br>
	 * <p>
	 * The dialog is allocated by forward monitor on activation dialogs actions.<br>
	 * <p>
	 * @return the dialog
	 */
	public ForwardDialog getDialog() {
		return dialog;
	}

	/**
	 * Sets the {@link ForwardDialog} object, that inherits from {@link JDialog}<br> for the form.<br>
	 * <p>
	 * The dialog is allocated by forward monitor on activation dialogs actions.<br>
	 * <p>
	 * @param dialog the dialog to set
	 */
	public void setDialog(ForwardDialog dialog) {
		this.dialog = dialog;
	}

	/**
	 * Gets the form panel internal structure.<br>
	 * <p>
	 * The internal structure depics children panels of panel parent and the layout manager.<br>
	 * Will be returned a structure with visible properties with no get methods.<br>
	 * <p>
	 * If the input panel has not been defined, a null value will be returned.
	 * <p>
	 * @return the internal structure of the panel
	 */
	public InnerForwardPanel getPanelStructure(String panelName) {
		return hs_panel.get(panelName);
	}

	/**
	 * Returns the name of all panels defined in the form.<br>
	 * <p>
	 * Panels can be container of other panels or detail panels containing java swing controls.<br>
	 * To gain detail structure informations about panel <code>getPanelStructure()</code> method must be used.<br>
	 * <p<
	 * @return the panelMain
	 */
	public ArrayList<String> getPanelNames() {
		ArrayList<String> al_panelName = null;
		al_panelName = new ArrayList<String> ();
		for (String panelName : hs_panel.keySet()) {
			al_panelName.add(panelName);
		}
		return al_panelName;
	}

	/**
	 * Returns the name of parent panel of input panel<br>
	 * <p>
	 * @param panelName 
	 * @return the panelParent name
	 */
	public String getPanelNameParent(String panelName) {
		InnerForwardPanel innerForwardPanel = null;
		String panelParent = "";
		
		for (String panelLoop : hs_panel.keySet()) {
			if (panelLoop.equals(panelName)) {continue;}
			
			innerForwardPanel = hs_panel.get(panelLoop);
			if (innerForwardPanel.al_panelChildren.contains(panelName)) {
				panelParent = innerForwardPanel.panelName;
				return panelParent;
			}
			if (panelName.equals(innerForwardPanel.panelChildrenCenter)
			||  panelName.equals(innerForwardPanel.panelChildrenNorth)
			||  panelName.equals(innerForwardPanel.panelChildrenSouth)
			||  panelName.equals(innerForwardPanel.panelChildrenEast)
			||  panelName.equals(innerForwardPanel.panelChildrenWest)) {
				panelParent = innerForwardPanel.panelName;
				return panelParent;
			}
		}
		return panelParent;
	}

	/**
	 * Returns the forward descriptor for the panel.<br>
	 * <p>
	 * It's a {@link ForwardPanel} object with application dependent data<br>
	 * <p>
	 * @param String panelName
	 * @return ForwardPanel the panelDescriptor
	 */
	public ForwardPanel getPanelDescriptor(String panelName) {
		InnerForwardPanel innerPanelStructure = null;
		innerPanelStructure = this.getPanelStructure(panelName);
		return innerPanelStructure.panel;
	}

	@Override
	public String toString() {
		return "Form:"+formName+",Type:"+formType+ ",RootPanel:"+rootPanelName;
	}



	/*
	 * Info panel
	 */
	 protected class InnerForwardPanel{

		 // Nome pannello e layout da utilizzare
		 String panelName = "";       					 	    // Nome panel genitore (Il primo è il rootPanel)
		 ForwardPanel panel = null;       					 	// Panel codificato da forward, include l'oggetto JPanel
		 EnumForwardLayout layoutManager = null;				// Layout manager da utilizzare per disporre name in main, viene riportato nel descrittore del pannello
		 EnumForwardPanelType panelType = null;                 // Tipo pannello
		 
		 // Nomi pannelli specifici per layout BORDER_LAYOUT
		 String panelChildrenNorth = "";                        // Se diverso da "" e BORDER_LAYOUT va disposto a NORTH
		 String panelChildrenCenter = "";                       // Se diverso da "" e BORDER_LAYOUT va disposto a CENTER
		 String panelChildrenSouth = "";                        // Se diverso da "" e BORDER_LAYOUT va disposto a SOUTH
		 String panelChildrenEast = "";                         // Se diverso da "" e BORDER_LAYOUT va disposto a EAST
		 String panelChildrenWest = "";                         // Se diverso da "" e BORDER_LAYOUT va disposto a WEST
		 
		 // Per layout BOX_LAYOUT e FLOW_LAYOUT, asse su cui disporre i componenti
         int layoutAxis = 0;        							// javax.swing.BoxLayout.X_AXIS, .Y_AXIS, .LINE_AXIS, .PAGE_AXIS
		 ArrayList<String> al_panelChildren = null;          	// Nomi panel da disporre in parent se boxLayout o flowLayout o tabbed panel
          
		 // Form in alternativa ai panel children.
		 // In pratica il form definisce un singolo panel disposto ricorsivamente
		 String formName = "";  								// Nome form  valorizzato in dichiarazione  
		 ForwardForm form = null;								// Descrittore completo form valorizzato in esecuzione
		 
		 // Menu in alternativa ai panel children e a form
		 String menuName = "";  								// Nome menu  valorizzato in dichiarazione  
		 ForwardPanelMenu panelMenu = null;						// Descrittore completo panel menu valorizzato in esecuzione

		 /* Costruttore */
		 private InnerForwardPanel() {
			 al_panelChildren = new ArrayList<String> ();
			 layoutManager = EnumForwardLayout.NOT_ASSIGNED;
			 panelType = EnumForwardPanelType.NOT_ASSIGNED;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			String strText1 = "";
			String strText2 = "";
			String strText3 = "";
			strText1 = "Panel:"+panelName+","+layoutManager.toString();
			strText2 = (layoutManager == EnumForwardLayout.BORDER_LAYOUT) ? ",North:"+panelChildrenNorth  + ",South:"+panelChildrenSouth  + ",Center:"+panelChildrenCenter  + ",East:"+panelChildrenEast  + ",West:"+panelChildrenWest
					: ",children:"+al_panelChildren.toString();
			strText3 = ",menu:"+menuName;
			return strText1 + strText2 + strText3;
		}

		 
	}


}
