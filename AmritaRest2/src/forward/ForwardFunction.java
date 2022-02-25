package forward;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DateEditor;
import javax.swing.JSpinner.NumberEditor;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.DateFormatter;
import javax.swing.text.DefaultFormatter;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.MaskFormatter;
import javax.swing.text.NumberFormatter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import analyzer.AmritaConstants;
import analyzer.ExecutionDirectives;
import utilities.ReflectionManager;
import enums.EnumForwardAction;
import enums.EnumForwardBorderType;
import enums.EnumForwardCaptionCustomization;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumForwardScope;
import enums.EnumLanguage;
import enums.EnumMessageType;

/**
 * copyright (c) 2012 e-Amrita - Ing. Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardFunction
 * </h1>
 * <p>
 * La funzione applicativa rappresenta l'unità elementare funzionale di esecuzione<br>
 * Dal punto di vista operativo lavora dentro un applet o una finestra JFrame<br>
 * ed è composta da uno o più form, normalmente da un solo form in funzioni mediamente complesse.<br>
 * Una funzione può a sua volta richiamare o essere richiamata da altre funzioni.<br>
 * La base di ogni funzione Forward è la GUI Swing Java, che viene generata in modo generalizzato e viene<br>
 * quindi replicata in fase di deploy.<br>
 * <p>
 * Ogni form è composto da un insieme di pannelli relazionati da 1 o più gestori di layout<br>
 * per realizzare una intergaccia GUI Java comunque complessa, come descritto dalla classe<br>
 * {@link ForwardForm}. Gli oggetti della classe <tt>ForwardForm</tt>, in collaborazione con {@link ForwardPanel}<br>
 * e {@link ForwardPanelComponent}, contengono anche tutti i riferimenti agli oggetti GUI java che compongono <br>
 * l'interfaccia.<br>
 * Dal momento che l'intergaccia GUI Swing è l'interfaccia di riferimento, tutte le informazioni<br>
 * grafiche e di rendering sono già naturalmente ed esaustivamente memorizzate negli stessi.<br>
 * Il monitor Forward Web, responsabile del deploy su server, interrogherà gli oggetti descrittori<br>
 * di forward (applicativi e funzionali) e gli oggetti grafici GUI di Swing, per ottenere tutte le <br>
 * informazioni necessarie a realizzare una interfaccia equivalente in codice HTML puro + eventuale<br>
 * Java Script dove necessario.<br>
 * <p>
  * Una delle caratteristiche di progetto più importanti di Forward è la riusabilità, che arriva fino<br>
 * al singolo pannello. In pratica la definizione di uno specifico pannello, la sua strategia di accesso<br>
 * ai dati e ogni specifica logica applicativa, sono condivisibili fra funzioni diverse. <br>
 * <p>
 * L'unità elementare applicativa è invece il pannello descritto dalla classe {@link ForwardPanel} <br>
 * che contiene i campi e gli oggetti grafici per interagire con l'utente.<br>
 * Ogni pannello può fare riferimento a una strategia di accesso ai dati che lo alimenti e possiede <br>
 * uno specifico gestiore di layout standard Java. <br>
 * Il pannello elenca i campi elementari, da db o meno, da gestire, utilizzando oggetti {@link ForwardPanelComponent}.<br>
 * Parallelamente a ogni campo elementare viene associato il corrispondenti componente grafico standard Java <br>
 * da utilizzare per il rendering, da disporre secondo il gestore di layout scelto.<br>
 * In alternativa il pannello può dichiarare di ospitare un'altra funzione completa<br>
 * <p>
 * Tutte le caratteristiche della funzione, dagli oggetti grafici, ai gestori di layout etc.,<br>
 * sono modellati con le features standard Java, con GUI Swing che possono essere rese una<br>
 * applicazione funzionante ultilizzando il monitor di Forward GUI per Desktop e successivamente<br>
 * in ambiente Web con il Monitor GUI per server.<br>
 * <p>
 * Questo descrittore, ovvero una istanza della classe {@link ForwardFunction}, viene utilizzato<br>
 * in tutti gli ambienti di deploy per generare l'applicazione funzionante.<br>
 * Le informazioni necessarie vengono ottenute con semplici metodi get().<br>
 * A tale scopo un oggetto della classe {@link ForwardFunction}, con tutti gli oggetti di tutte le classi implicate,<br>
 * viene serializzato su disco.<br>
 * <br>
 * Lo stesso descrittore della funzione viene utilizzato, essendo un oggetto java, dal monitor<br>
 * Forward per Web, in pratica una Servelet generalizzata che genera HTML e codice annesso<br>
 * a partire sempre dal descrittore della funzione.
 * <P>
 * Una intera funzione applicativa, descritta da una istanza di questa classe può essere visualizzata<br>
 * e gestita dentro un pannello di una funzione qualsiasi, che ne deventa un contenitore, esattamente <br>
 * come il contenitore principale, ovvero JFrame o JApplet.<br>
 * Ciò permette la completa interoperabilità e riusabilità delle varie funzioni. Un esempio è rappresentato<br>
 * dall'IDE di sviluppo Forward che, nel pannello centrale, contiene prprio la funzione in definizione, <br>
 * con tutti i suoi pannelli, completamente indipendente.<br>
 * <p>
 * {@link ForwardFunction} implementa, rende disponibili le variabili di controllo e ne<br>
 * permette la variazione, del modello MVC rappresentato da Forward.<br>
 * <p>
 * Tutte le informazioni applicative della funzione, dei pannelli e delle loro relazioni, <br>
 * oltre che dei campi nei pannelli, sono disponibili al codice applicativo di eccezione.<br>
 * Tale codice viene collocato in una classe che eredita da {@link ForwardFunction} e contiene:
 * <Ul>
 * <Li> La descrizione formale della funzione in forma dichiarativa
 * <Li> L'elenco dei pannelli, delle loro relazioni e le modalità di visualizzazione
 * <Li> Per ogni pannello la strategia di accesso ai dati, i campi e il loro posizionamento
 * <Li> Le logiche standard predefinite da Forward
 * <Li> L'attivazione del codice applicativo di eccezione, se non gestibile in modo standard
 * <Li> Alle le logiche applicative di eccezione viene assegnato un nome univoco
 * </Ul>
 * E' previsto un unico metodo di codice applicativo di eccezione che smisterà, secondo necessità, <br>
 * verso altri metodi applicativi specifici.<br>
 * <br>
 * Tali metodi hanno a disposizione solo 3 parametri:<br> 
 * <br>
 * <Ul>
 * <Li> <b>system</b><br>
 * Si tratta di un oggetto {@link ForwardSystem}.<br>
 * Sono disponibili tutte le informazioni di sistema, non applicative, <br>
 * di sessione, ambiente, defaulte e di installazione.<br>
 * <p>
 * Attraverso l'oggetto system è possibile accedere a tutte le aree di comuni comunicazione previste, <br>
 * gestite dalla classe {@link ForwardCommonAreas}.<br>
 * Ogni area di comunicazione è tipicamente strutturata in parametri/valore.<br>
 * Tuttavia il valore, normalmente una stringa, può essere un oggetto generico e quindi utilizzabile <br>
 * per gli scopi più diversi, come per esempio <t>cashing</t> o passaggio dati strutturati.<br>
 * <br>
 * <Ul>
 * <Li> <b>Global</b> Common Area<br>
 * Contiene informazioni condivise con tutto il server per tutte le applicazioni.<br>
 * <Li> <b>Session</b> Common Area <br>
 * Contiene informazioni condivise nell'ambito di una stessa sessione utente. <br>
 * <Li> <b>Function</b> Common Area <br>
 * Contiene informazioni condivise nell'ambito della stessa funzione.
 * <Li> <b>Function to function</b> Common Area<br>
 * Contiene informazioni condivise fra una funzione chiamante e una chiamata.<br>
 * Viene utilizzata per fornire parametri e strutture a una funzione choiamata e per<br>
 * permettere a qusta di restituire dei risultati.<br>
 * </Ul>

 * <Li> <b>function</b><br>
 * Si tratta di un oggetto  {@link ForwardFunction}.<br>
 * Sono disponibili tutte le informazioni della funzione correntemente in esecuzione,<br>
 * in tutti i suoi aspetti. Attraverso i metodi a disposizione è possibile conoscere le<br>
 * caratteristiche della funzione, i diritti di accesso, i pannelli che la compongono <br>
 * con tutti i componenti. Di ogni componente è possibile conoscere il valore e modificarlo.<br>
 * Sono inoltre disponibili tutte le informazioni della struttura della funzione ed è possibile
 * modificare il comportamento predefinito della stessa.<br>
 * E' possibile conoscere l'evento che ha attivato l'esecuzione della logica e l'dentificativo della<br>
 * logica per effettuare le attività previste.<br>
 * <br>
 * <Li> <b>commonArea</b>
 * Si tratta di un oggetto  {@link ForwardCommonAreas}.<br>
 * Permette di di conoscere e aggiornare informazioni comuni a tutto il server, <br>
 * valide solo per la sessione attiva oppure comuni a tutta la funzione applicativa.<br>
 * Operativamente si aggiorna o si ottiene un valore a fronte di un nome ed è<br>
 * a carico dell'applicativo il casting corretto.<br>
 * In realtà è possibile memorizzare nelle varie common aree interi oggetti, secondo le<br>
 * più diverse esigenze applicative. Per esempio si possono gestire cash applicative.<br>
 * </Ul>
 *
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardPanel
 * @see ForwardCommonAreas
 *
*/

public abstract class ForwardFunction implements AmritaConstants {
 
	// Reference a istanza monitor attivo (desktot o web)
	protected ForwardMonitorDesktop monitor = null;						// Istanza di ForwardMonitorDesktop o ForwardMonitorWeb
 	ForwardSystem s = null;												// Oggetto system corrente allocato dal monitor, visto da classi applicative senza getter
 	private ReflectionManager rm = null;								// Reflection manager object

 	// Locale di internazionalizzazione
 	private Locale locale = null;										// Locale di internazionalizzazione
 	
	// Descrittore generale funzione
	private String functionId = "";                     				// Id funzione utilizzato nel chiamante per individuare il punto di chiamata della funzione
	private String functionName = "";                     				// Nome funzione applicativo, NON è il nome della classe
	private String title = "";              							// Descrizione funzione
	private EnumForwardFunctionModel model = null;          			// Modello di riferimento della funzione
	private EnumLanguage language = null;           					// Linguaggio caption e descrizioni dichiarato
	private EnumLanguage curLanguage = null;           					// Linguaggio caption e descrizioni corrente
	private int counterStarts = 0;                                      // Contatore numero di FUNCTION_START effettuatte dall'applicazione

	// Caratteristiche funzionali
	private ArrayList<EnumForwardOption> al_functionOption = null;  

	// Map descrittori di form, panel, menu, toolbar, singoli componenti e oggetti java generici
	private Map<String, ForwardForm> hm_form = null;					// Key=formName Data=struttura pannelli contenuti
    private Map<String, ForwardPanel> hm_panel = null;					// Pannelli definiti nei vari form
    private Map<String, InnerComponent> hm_component = null;			// Componenti Swing, ..
    private Map<String, ForwardMenu> hm_menu = null;          			// Descrittore generale menu. key=nome menu/submenu
    																	// I singoli componenti vengono caricati in hm_component 
    																	// Se il menu è di tipo toolbar allora contiene l'elenco dei componenti (button o altro)
    private Map<String, InnerComponent> hm_var = null;					// Variabili dichiarate con VAR() o definite implicitamente per table
   
    // Map Proprietà di default per tipo componente inserito con COMPONENT_PROPERTY_DEFAULT()
    private Map<EnumForwardComponent, ArrayList<InnerComponentProperty>> hm_propertyDefault = null;


    // Dichiarazione funzioni richiamabili, parametri richiesti/restituiti alla/dalla funzione se richiamata/richiama da altra funzione
 	private Map<String, InnerFunctionsCallable> hm_callable = null;     // Funzioni richiamabili
 	 
 	// Dichiarazione parametri richiesti/restituiti alla/dalla funzione se richiamata/richiama da altra funzione
	private ArrayList<String> al_parmRequired = null;					// Parametri richiesti dal chiamante (nome variabili)
	private ArrayList<String> al_parmReturned = null;					// Parametri restituiti al chiamante (nome variabili)

	// Opzioni varie
	private boolean isMultiLanguage = false; 							// True indica supporto runtime multilingue            
	private boolean isLookupFunction = false; 							// True indica funzione di lookup (rule table o applicativa) attivata automaticamente         
	private boolean isLookupFunctionAutomaticParms = false; 			// True indica funzione di lookup attivata con valorizzaztione atutomatica parametri richiesti/restituiti
	
	// Supporto multilingua help.
	// Tabella multilingua con key = panelName + ":" + componentName 
	// Se panelName = componentName = "", l'help è a livello di funzione
	// Se componentName = "", l'help è a livello di panel
	// Presente 1 campo con la stringa HTML, in lingua: 
	private int numTableHelpTechnical = 0;    							// Numero tabella multilingua con help Html di tipo tecnico specialistico     
	private int numTableHelpEndUser = 0;              					// Numero tabella multilingua con help Html di tipo funzionale per l'utente finale 
	
	// Supporto multilingua caption con Tabella multilingue.
	// Key = functionName:X:componentName                 dove X è un ordinal di EnumForwardCaptionCustomizations
	// Data = testo in lingua 
	private int numTableCaptions = 0;          							// Numero tabella multilingua per caption pannello, descrizioni campi, dialog etc
		
	// Dichiarazione condizioni di evento e azioni gestite
	private ArrayList<InnerOnEvent> al_onEvent = null;					// Eventi inseriti sequenzialmente come dichiarati
	private Map<String, InnerOnEvent> hm_onEvent = null;                // Eventi in struttura hash
	
	// Gruppi di action codificate
	private Map<String, InnerActionsGroup> hm_groupActions = null;      // Gruppi di actions in struttura hash
	
	
	
	// Working e servizio.
	// Oggetti attivi dalle varie sezioni dichiarate, ad uso interno all'esecuzione delle dichiarazioni
	InnerMonitorControBlock mcb = null;									// Di servizio valorizzato a fronte di FUNCTION_LOAD()
 	ForwardTableModel tableModel = null;					 			// Per gestione tooltip di cella
	ForwardPanel activePanel = null;
	ForwardForm activeForm = null;
	String activeFormName = "";
	String activePanelName = "";
	String activeMenuName = "";								  		  // Menu attivo
	String activeMenuNameRoot = "";							  		  // Menu attivo di primo livello
	int numProgrLabel = 0;											  // Numero progressivo nome label per generazione automatica label
	int numProgrGlueField = 0;										  // Numero progressivo nome campo di glue per generazione automatica nomi
    boolean isActiveDeclarationToolbar = false; 					  // True indica definizione controlli panel in corso	
    boolean isActiveDeclarationPanel = false; 						  // True indica definizione controlli toolbar in corso
    

    
	/**
     * Constructor
     */
	public ForwardFunction() { 
		super();
		this.rm = new ReflectionManager();
	    this.hm_form = new HashMap<String,ForwardForm> ();
	    this.hm_panel = new HashMap<String,ForwardPanel> ();
	    this.hm_propertyDefault = new HashMap<EnumForwardComponent, ArrayList<InnerComponentProperty>> ();
	    this.hm_menu = new HashMap<String, ForwardMenu> ();
	    this.hm_component = new HashMap<String, InnerComponent> ();
	    this.hm_var = new HashMap<String, InnerComponent> ();
	    this.hm_onEvent = new HashMap<String, InnerOnEvent> ();
	    this.hm_groupActions = new HashMap<String, InnerActionsGroup> ();
	    this.hm_callable = new HashMap<String, InnerFunctionsCallable> ();
		this.al_functionOption = new ArrayList<EnumForwardOption> ();  
	    this.al_onEvent = new ArrayList<InnerOnEvent> ();
	    this.al_parmRequired = new  ArrayList<String> ();
	    this.al_parmReturned = new ArrayList<String> ();
	    this.locale = Locale.getDefault();								// Locale della VM attiva
	    
	  
	    // Proprietà di default per tipo componente
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "Size", Dimension.class, new Dimension(200, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "PreferredSize", Dimension.class, new Dimension(200, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "MaximumSize", Dimension.class, new Dimension(200, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "MinimumSize", Dimension.class, new Dimension(200, 20));
  	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "HorizontalTextPosition", Integer.class, JLabel.LEFT);
  	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JLabel, "VerticalTextPosition", Integer.class, JLabel.CENTER);
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JTextField, "Size", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JTextField, "PreferredSize", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JTextField, "MaximumSize", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JTextField, "MinimumSize", Dimension.class, new Dimension(100, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JFormattedTextField, "Size", Dimension.class, new Dimension(100, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JFormattedTextField, "PreferredSize", Dimension.class, new Dimension(100, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JFormattedTextField, "MinimumSize", Dimension.class, new Dimension(100, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JFormattedTextField, "MaximumSize", Dimension.class, new Dimension(100, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JComboBox, "Size", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JComboBox, "PreferredSize", Dimension.class, new Dimension(150, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JComboBox, "MaximumSize", Dimension.class, new Dimension(150, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JComboBox, "MinimumSize", Dimension.class, new Dimension(150, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JCheckBox, "Size", Dimension.class, new Dimension(200, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JCheckBox, "PreferredSize", Dimension.class, new Dimension(200, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JRadioButton, "Size", Dimension.class, new Dimension(200, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JRadioButton, "PreferredSize", Dimension.class, new Dimension(200, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JButton, "Size", Dimension.class, new Dimension(100, 25));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JButton, "PreferredSize", Dimension.class, new Dimension(100, 25));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JButton, "VerticalTextPosition",  Integer.class,AbstractButton.TOP);
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JButton, "HorizontalTextPosition",  int.class,AbstractButton.LEFT);
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JToggleButton, "Size", Dimension.class, new Dimension(100, 25));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JToggleButton, "PreferredSize", Dimension.class, new Dimension(100, 25));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JToggleButton, "VerticalTextPosition",  Integer.class,AbstractButton.TOP);
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JToggleButton, "HorizontalTextPosition", Integer.class, AbstractButton.RIGHT);
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSlider, "Size", Dimension.class, new Dimension(80, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSlider, "PreferredSize", Dimension.class, new Dimension(80, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSlider, "MinimumSize", Dimension.class, new Dimension(80, 20));
	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSpinner, "Size", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSpinner, "PreferredSize", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSpinner, "MaximumSize", Dimension.class, new Dimension(100, 20));
 	   	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JSpinner, "MaximumSize", Dimension.class, new Dimension(100, 20));

	}

	/**
	 * <h3>Run the declare function specification</h3>
	 * <br>
	 * This method is intended for exclusive internal use by forward monitor, for a correct exception management<br>
	 * <p>
	 */
	public  void runDeclare(ExecutionDirectives di) { 
		
		try {
			declare();
		} catch (Exception e) {
			di.excpOccurred = e;
			return;
		}
	}
	

	/**
	 * <h3>Declare function specification</h3>
	 * <br>
	 * <code>define()</code> contains calls declaring function methods like a documentation script.<br>
	 * <br>
	 * Applications function classes must extends {@link ForwardFunction}, an abstract class that define<br>
	 * just the abstract method <code>declare()</code>.<br>
	 * Forward monitor calls <code>declare()</code> in the initialization process to build function structures.<br>
	 * <p>
	 */
	public abstract void declare();
	
	
	
	/**
	 * Menu level update.<br>
	 * <p>
	 * This method is intended for exclusive internal use by forward monitor.<br>
	 * Menus and submenus are declared by function separately and there is not, at declaring time,<br>
	 * the information of deep level menu.<br>
	 * For each root menu declared, there will be updated any submenu called with its own level, 0-based<br>
	 * <p>
	 */
	public void menuLevelUpdate(){
		ForwardMenu menu = null;
		
		// Scan menu definiti
		for (Entry<String, ForwardMenu> entryMenu : this.hm_menu.entrySet()) {
			menu = entryMenu.getValue();
			if (menu.getMenuType() != EnumForwardOption.MENU_TYPE_ROOT) {continue;}
			menuLevelUpdateRecursive(menu, menu, 0);
		}
	};
	
	/*
	 * Update ricorsivo livello menu
	 */
	private void menuLevelUpdateRecursive(ForwardMenu menuRoot, ForwardMenu menu, int menuLevel) {
		ForwardMenu subMenuRecursive = null;
		String subMenuNameRecursive = "";
		
		menu.setMenuLevel(menuLevel);
		menu.setMenuRoot(menuRoot);
		
		// Scan menu items
		for (ForwardMenuItem menuItem : menu.getMenuItems()) {
			
			
			if (!menuItem.isSubMenuActivation()) {continue;}
			subMenuNameRecursive = menuItem.getSubMenuName();
			subMenuRecursive = this.hm_menu.get(subMenuNameRecursive);
			if (subMenuRecursive == null) {continue;}							// Non definito
			menuLevelUpdateRecursive(menuRoot, subMenuRecursive, menuLevel + 1);
		}
		
	}

	/**
	 * <h4>Begin Function Specification And Documentation declaration</h4>
	 * It's a marker to delimit a full featured application function declaration.<br>
	 * Informations declared in a not procedural fashion are enough to get the application running.<br>
	 * <p>
	 * No matter about technical aspects, fully supported by the monitor forward,<br>
	 * simply you don't care.<br>They're masked by software layers that make the work for the 
	 * programmer.<br>
	 * So a <code>FSD</code> appears to be just a documentation script, a list of not<br>
	 * procedural execution rules, only a static descriptor of the function,<br>
	 * structured in own sections.
	 * <p>
	 * But the compliance of section to the application model implemented by Forward, doesn't <br>
	 * make the application with limits. It's always possible to access to any part of the<br>
	 * application when it needs.<br>
	 * 
	 * @param functionName the name of the funzion
	 * @param title the text in the title bar
	 * @param model as a {@link EnumForwardFunctionModel} enumeration
	 * @param language as a {@link EnumLanguage} enumeration
	 * @param options as an optional list of {@link EnumForwardOption} enumerations
	 */
	public void BEGIN_FUNCTION_SPECIFICATION(String functionName, String title, EnumForwardFunctionModel model, EnumLanguage language, EnumForwardOption ... options) {
		this.functionName = functionName;
		this.title = title;
		this.model = model;
		this.language = language;
		
		// Scan options
		for (EnumForwardOption option : options) {
			this.al_functionOption.add(option);
		}
	}
	
	
	/**
	 *  <h3>End Function Specification And Documentation declaration</h3>
	 */
	public void END_FUNCTION_SPECIFICATION() {
	}
	
	/**
	 * <h3>Begin form declaration</h3>
	 * Depict the first step of the function building, the structure of any form<br>
	 * used by function.<br>
	 * Normally a function is built using just one form but for very complex functions may<br>
	 * be better to structure the function with more forms.<br>
	 * <p>
	 * The structure is based on java and layout manager standard of java, used to lay panels,<br>
	 * in the main container <code>JFrame</code> or <code>JApplet</code> or in another panel.<br>
	 * <p>
	 * A form is implemented with a container like {@link JFrame} or {@link JDialog} and so can be with<br>
	 * a menu bar attached too. If the input menuBar is null or an empty string or there is no menu declared<br>
	 * with that name for the function, no action is taken.
	 * <p>
	 * A form is a set of panels, recursively defined, that contains, in the leaf panel,<br>
	 * java Swing controls, graphic objects like <code>JTextField</code> and so on.<br>
	 * Java graphic controls not elementary like <CODE>JTree</code>, <CODE>JList</code>,
	 * and <CODE>JTable</code>, are managed with specific panel types specialized and<br>
	 * already featured<br>
	 * <p>
	 * There is a set of specialized set of panels ready to use to manage menus, tre, tabbed panels,
	 * properties and so on, to cover the most common application needs.<br>
	 * <p>
	 * Each panel can be a container for others panels and use, to lay panels,<br>
	 * java layout managers like <code>BOX_LAYOUT</code>, <code>FLO_LAYOUT</code>, <code>BORDER_LAYOUT</code>.<br>
	 * To lay java swing controls in a leaf panel, named <CODE>DETAIL</code> the <code>GRID_BAG_LAYOUT<code/><br>
	 * is used instead.<br>
	 * <p>
	 * So there is nothing of hidden or different from the normal way to realize java applications.<br>
	 * It should be declared this methos for each level of nesting panels in the form.<br>
	 * 
	 * @param formName
	 * @param formType the EnumForwardOption describing the form
	 * @param menuBarName as defined by <code>MENU()</code> declarations
	 */
	public void BEGIN_FORM(String formName, EnumForwardOption formType, String menuBarName) {
		this.activeMenuName = "";
		activeFormName = formName;
		activeForm = new ForwardForm(formName, formType, menuBarName);
		activeForm.setFunction(this);
		hm_form.put(formName, activeForm);
	}

	/**
	 * <h3>Begin form declaration</h3>
	 * Depict the first step of the function building, the structure of any form<br>
	 * used by function.<br>
	 * Normally a function is built using just one form but for very complex functions may<br>
	 * be better to structure the function with more forms.<br>
	 * <p>
	 * The structure is based on java and layout manager standard of java, used to lay panels,<br>
	 * in the main container <code>JFrame</code> or <code>JApplet</code> or in another panel.<br>
	 * <p>
	 * A form is implemented with a container like {@link JFrame} or {@link JDialog} and so can be with<br>
	 * a menu bar attached too. If the input menuBar is null or an empty string or there is no menu declared<br>
	 * with that name for the function, no action is taken.
	 * <p>
	 * A form is a set of panels, recursively defined, that contains, in the leaf panel,<br>
	 * java Swing controls, graphic objects like <code>JTextField</code> and so on.<br>
	 * Java graphic controls not elementary like <CODE>JTree</code>, <CODE>JList</code>,
	 * and <CODE>JTable</code>, are managed with specific panel types specialized and<br>
	 * already featured<br>
	 * <p>
	 * There is a set of specialized set of panels ready to use to manage menus, tre, tabbed panels,
	 * properties and so on, to cover the most common application needs.<br>
	 * <p>
	 * Each panel can be a container for others panels and use, to lay panels,<br>
	 * java layout managers like <code>BOX_LAYOUT</code>, <code>FLO_LAYOUT</code>, <code>BORDER_LAYOUT</code>.<br>
	 * To lay java swing controls in a leaf panel, named <CODE>DETAIL</code> the <code>GRID_BAG_LAYOUT<code/><br>
	 * is used instead.<br>
	 * <p>
	 * So there is nothing of hidden or different from the normal way to realize java applications.<br>
	 * It should be declared this methos for each level of nesting panels in the form.<br>
	 * 
	 * @param formName
	 * @param formType the EnumForwardOption describing the form
	 */
	public void BEGIN_FORM(String formName, EnumForwardOption formType) {
		this.activeMenuName = "";
		activeFormName = formName;
		activeForm = new ForwardForm(formName, formType, "");
		activeForm.setFunction(this);
		hm_form.put(formName, activeForm);
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor for use with <code>BOX_LAYOUT</code> layout manager, where it needs to specify an axis orientation.
	 * <p>
	 * Child panels specified will be layed out sequentially with the input axis orientation, horizontally or vertically.<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType  as a EnumForwardPanelType.CONTAINER constant
	 * @param panelLayout as a EnumForwardLayout.BOX_LAYOUT constant
	 * @param axis as a ForwardForm.AXIS_HORIZONTAL or ForwardForm.AXIS_VERTICAl constant
	 * @param ar_panelChild as the panels names list to be laid out
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, EnumForwardLayout panelLayout, int axis,  String ... ar_panelChild) {
		InnerComponent innerComponent = null;
        JPanel jpanel = null;
         
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
		jpanel = new JPanel();
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, panelLayout);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jpanel, this.activePanel, panelLayout, axis, ar_panelChild);		
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor specific for tabbed panels without specification of layout manager, orientation<br>
	 * but just only with child panels<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType  as a EnumForwardPanelType.TABBED constant
	 * @param panelChildren
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, String ... ar_panelChild) {
		InnerComponent innerComponent = null;
		JTabbedPane jtabbedPane = null;
		
		// Inserimento componente in struttura interna funzione
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JTabbedPane;
		jtabbedPane = new JTabbedPane();
		innerComponent.component = jtabbedPane;
		this.setComponentPropertiesDefault(JTabbedPane.class, EnumForwardComponent.JTabbedPane, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, EnumForwardLayout.NOT_ASSIGNED);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jtabbedPane, this.activePanel, EnumForwardLayout.NOT_ASSIGNED,  ForwardForm.AXIS_DEFAULT, ar_panelChild);		
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor specific for split panels with defaults values<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType as EnumForwardPanelType.SPLIT
	 * @param panel1 as the panel left or top
	 * @param panel2 as the panel right or bottom
	 * @param axis the orientation as ForwardForm.AXIS_HORIZONTAL or ForwardFormLayout.AXIS_VERTICAL
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, String panel1, String panel2, int axis) {
		InnerComponent innerComponent = null;
		JSplitPane jSplitPane = null;
		
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
		jSplitPane = new JSplitPane();
		if (axis == ForwardForm.AXIS_HORIZONTAL) {
			jSplitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		} else {
			jSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		}
		innerComponent.component = jSplitPane;
		this.setComponentPropertiesDefault(JSplitPane.class, EnumForwardComponent.JSplitPane, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, EnumForwardLayout.NOT_ASSIGNED);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jSplitPane, this.activePanel, EnumForwardLayout.NOT_ASSIGNED, axis, panel1, panel2);		
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor specific for split panels with all values<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType as EnumForwardPanelType.SPLIT
	 * @param panel1 as the panel left or top
	 * @param panel2 as the panel right or bottom
	 * @param axis the orientation as ForwardForm.AXIS_HORIZONTAL or ForwardFormLayout.AXIS_VERTICAL constant
	 * @param oneTouchExpandable true to let expansion with click
	 * @param dividerLocation the position in pixel where the divider is placed
	 * @param dividerSize the size in pixel of divider 
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, String panel1, String panel2, int axis, boolean oneTouchExpandable, int dividerLocation, int dividerSize) {
		InnerComponent innerComponent = null;
		JSplitPane jSplitPane = null;
		
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
		jSplitPane = new JSplitPane();
		if (axis == ForwardForm.AXIS_HORIZONTAL) {
			jSplitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		} else {
			jSplitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		}
		jSplitPane.setOneTouchExpandable(oneTouchExpandable);
		jSplitPane.setDividerLocation(dividerLocation);
		jSplitPane.setDividerSize(dividerSize);
		innerComponent.component = jSplitPane;
		this.setComponentPropertiesDefault(JSplitPane.class, EnumForwardComponent.JSplitPane, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, EnumForwardLayout.NOT_ASSIGNED);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jSplitPane, this.activePanel, EnumForwardLayout.NOT_ASSIGNED, axis, panel1, panel2);		
	}

	
	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor for predefined panels without specification of layout manager and orientation<br>
	 * It's necessary only the panel name and panel type.<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType  as a EnumForwardPanelType.HTML, EnumForwardPanelType.LOGIN, .. constant
	 */	
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType) {
		InnerComponent innerComponent = null;
        JPanel jpanel = null;
         
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
        jpanel = this.getJPanel(panelName);
		if (jpanel == null) {
			jpanel = new JPanel();
		}
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, EnumForwardLayout.NOT_ASSIGNED);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jpanel, this.activePanel, EnumForwardLayout.PREDEFINED_LAYOUT,  ForwardForm.AXIS_DEFAULT, new String[]{});		
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor for menu panel with the specification of menu name<br>
	 * <p>
	 * 
	 * @param panelName 
	 * @param panelType  as a EnumForwardPanelType.MENU constant
	 * @param menuName as declared in MENU() section.
	 */	
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, String menuName) {
		InnerComponent innerComponent = null;
        JPanel jpanel = null;
        
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
        jpanel = this.getJPanel("panelName");
		if (jpanel == null) {
			jpanel = new JPanel();
		}
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		innerComponent.menuName = menuName;
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, EnumForwardLayout.NOT_ASSIGNED);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanelAsMenu(panelName, jpanel, menuName);		
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor for container panels with layout manager without orientation, with or no panels children<br>
	 * When a panel type DETAIL is specified, no child panels should be coded and in any case they're<br>
	 * not considerated.
	 * <p>
	 * @param panelName 
	 * @param panelType as a EnumForwardPanelType.CONTAINER or EnumForwardPanelType.DETAIL constant
	 * @param panelLayout as a EnumForwardLayout.CARD_LAYOUT or EnumForwardLayout.FLOW_LAYOUT
	 * @param ar_panelChild 
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, EnumForwardLayout panelLayout, String ... ar_panelChild) {
		InnerComponent innerComponent = null;
        JPanel jpanel = null;
        
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
        jpanel = (JPanel) this.getJComponent(panelName);
		if (jpanel == null) {
			jpanel = new JPanel();
		}
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, panelLayout);
		this.activePanelName = this.activePanel.getName();

		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jpanel, this.activePanel, panelLayout, ForwardForm.AXIS_DEFAULT, ar_panelChild);		
	}



	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor specific for panels with layout manager BORDER_LAYOUT<br>
	 * <p>
	 * The order of child panels to lay out depends on the <code>BORDER_LAYOUT</code> layout manager. <br>
	 * So sequence order must be <code>NORTH</code>, <code>CENTER</code>, <code>SOUTH</code>, <code>EAST</code> and <code>WEST</code>. <br>
	 * Put a null string in the place holder if the area is to be left empty.<br>
	 * <p>
	 * @param panelName 
	 * @param panelType  as a EnumForwardPanelType.CONTAINER constant
	 * @param panelLayout as a EnumForwardLayout.BORDER_LAYOUT constant
	 * @param panelNorth as the panel name to be layied out or a null string if no panel to lay out
	 * @param panelCenter as the panel name to be layied out or a null string if no panel to lay out
	 * @param panelSouth as the panel name to be layied out or a null string if no panel to lay out
	 * @param panelEast as the panel name to be layied out or a null string if no panel to lay out
	 * @param panelWest as the panel name to be layied out or a null string if no panel to lay out
	 */
	public void PANELS_STRUCTURE(String panelName, EnumForwardPanelType panelType, EnumForwardLayout panelLayout, String panelNorth, String panelCenter, String panelSouth, String panelEast, String panelWest) {
		InnerComponent innerComponent = null;
		JPanel jpanel = null;
		
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelName;
		innerComponent.componentType = EnumForwardComponent.JPanel;
        jpanel = this.getJPanel("panelName");
		if (jpanel == null) {
			jpanel = new JPanel();
		}
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelName);
		this.hm_component.put(panelName, innerComponent);

		// Inerimento pannelli in descrittore panel
		this.activePanel = putPanelDescriptor(panelName, panelType, panelLayout);
		this.activePanelName = this.activePanel.getName();
		
		// Inserimento pannello nel descrittore dei form
		this.activeForm.addPanel(panelName, panelType, jpanel, this.activePanel, panelLayout, ForwardForm.AXIS_DEFAULT, new String[]{panelNorth, panelCenter, panelSouth, panelEast, panelWest});		
	}
    

	/* ------------------------------------------
	 * Inserimento pannelli in struttura hash
	 * ------------------------------------------
	 * 
	 * Se inesistente istanziazione e inserimento
	 * Se già inserito nessuna operazione
	 */
	private ForwardPanel putPanelDescriptor(String panelName, EnumForwardPanelType panelType, EnumForwardLayout panelLayout) {
		Container panel = null;
		ForwardPanel forwardPanel = null;
		
		forwardPanel = this.hm_panel.get(panelName);
		if (forwardPanel != null) {return forwardPanel;}
		forwardPanel = new ForwardPanel(panelName, panelType, panelLayout);
		panel = getJComponent(panelName);
		if (panel != null) {
			forwardPanel.setGraphicObject(panel);
		}
		this.hm_panel.put(panelName, forwardPanel);
		return forwardPanel;
	}

	/**
	 * <h4>Panel form declaring in structure</h4>
	 * Constructor for panels described by a whole form<br>
	 * At the end a form is just a panel arranged with any sub panel.<br>
	 * The form can be declared before or after the current declaration.<br>
	 * At runtime the forward monitor will build a JPanel for the form and will lay out it onto the panel.<br>
	 * <p>
	 * 
	 * @param panelParent 
	 * @param formName
	 */
	public void PANELS_STRUCTURE(String panelParent, String formName) {
		InnerComponent innerComponent = null;
		JPanel jpanel = null;
		
		// Inserimento componente in struttura interna
		innerComponent = new InnerComponent();
		innerComponent.componentName = panelParent;
		innerComponent.componentType = EnumForwardComponent.JPanel;
		jpanel = new JPanel();
		innerComponent.component = jpanel;
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, innerComponent.component);
		JComponent jcomponent = (JComponent) innerComponent.component;
		jcomponent.setName(panelParent);
		this.hm_component.put(panelParent, innerComponent);

		// Inserimento form nel descrittore dei form, in relazione con il pannello
		this.activeForm.addPanelAsForm(panelParent, jpanel, formName);		
	}



	/**
	 * <h4>Begin panel detail contents</h4>
	 * It's a marker to starts controls declarations for the panel specified.<br>
	 * <p>
	 * @param String panelName the internal name of panel
	 */
	public void BEGIN_PANEL_DETAIL_CONTENT(String panelName) {
		this.activeMenuName = "";
		this.activePanelName = panelName;
		this.activePanel = hm_panel.get(panelName);
		 
		// Panel NON dichiarato in struttura con PANEL_STRUCTURE() come di dettaglio
		// Potrebbe trattarsi di un errore nella dichiarazione della funzione oppure
		// di un subpanel dichiarato dentro un altro pannello
		// Viene in questo caso creato un pannello DETAIL e BOX_LAYOUT 
		if (this.activePanel == null) {
			BEGIN_PANEL_DETAIL_CONTENT(panelName, EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "", "", 0, 0, 0, 0);
		}
		
		this.activePanel.setTabText("");
		this.isActiveDeclarationPanel = true;
	}

	/**
	 * <h4>Begin panel detail contents</h4>
	 * It's a marker to starts controls declarations for the panel specified.<br>
	 * This constructor lets to declare a popUp menu name. <br>
	 * The last nullMarker object makes this constructor unique.<br>
	 * <p>
	 * @param panelName the internal name of panel
	 * @param popUpMenuName the name of popUp to make active
	 * @param nullMarker just to make the metjod unique
	 */
	@SuppressWarnings("rawtypes")
	public void BEGIN_PANEL_DETAIL_CONTENT(String panelName, String popUpMenuName, Class nullMarker) {
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		this.activePanelName = panelName;
		this.activePanel = hm_panel.get(panelName);
		this.isActiveDeclarationPanel = true;
		
		// Panel NON dichiarato in struttura con PANEL_STRUCTURE() come di dettaglio
		// Potrebbe trattarsi di un errore nella dichiarazione della funzione oppure
		// di un subpanel dichiarato dentro un altro pannello
		// Viene in questo caso creato un pannello DETAIL e BOX_LAYOUT 
		if (this.activePanel == null) {
			BEGIN_PANEL_DETAIL_CONTENT(panelName, EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "", popUpMenuName, 0, 0, 0, 0);
		}
		
		// Update descrittore pannello e struttura componenti
		this.getPanel(panelName).setPopUpMenuName(popUpMenuName);
		innerComponent = this.getComponentDeclared(panelName);
		if (innerComponent == null) {return;}
		innerComponent.popUpMenuName = popUpMenuName;
	}

	/**
	 * <h4>Begin panel detail contents declaration</h4>
	 * <p>
	 * After this declaration you are allowed to code <code>COMPONENT()</code> declarations to lay out<br>
	 * GUI components on the panel.<br>
	 * <p>
	 * The panel can be previously declared as <code>DETAIL</code> in a <code>PANEL_STRUCTURE()</code> declaration<br>
	 * or coded as a component directly to be layed out as a {@link JPanel} component.<br>
	 * Normally the <code>PANEL_STRUCTURE()</code> declaration causes the function panel internal structures<br>
	 * to be created but, when a panel is directly layed out as a normal component, a <code>BEGIN_PANEL_DETAIL_CONTENT</code><br>
	 * will cause first the same operation too.<br>
	 * So, in the last case, the panel will be inserted in internal structures as a <code>DETAIL</code> panel with a <code>BOX_LAYOUT</code><br>
	 * manager to be used to lay out all components.<br>
	 * <p>
	 * @param panelName the internal name of panel
	 * @param panelNameDetail the text name of panel as displayed, for example, on tab for tabbed panels
	 */
	public void BEGIN_PANEL_DETAIL_CONTENT(String panelName, String panelNameDetail) {
		this.activeMenuName = "";
		this.activePanelName = panelName;
		this.activePanel = hm_panel.get(panelName);
		
		// Panel NON dichiarato in struttura con PANEL_STRUCTURE() come di dettaglio
		// Potrebbe trattarsi di un errore nella dichiarazione della funzione oppure
		// di un subpanel dichiarato dentro un altro pannello
		// Viene in questo caso creato un pannello DETAIL e BOX_LAYOUT 
		if (this.activePanel == null) {
			BEGIN_PANEL_DETAIL_CONTENT(panelName, EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, panelNameDetail, "", 0, 0, 0, 0);
		}
		
		this.activePanel.setTabText(panelNameDetail);
		this.isActiveDeclarationPanel = true;
	}

	/**
	 * <h4>Begin panel detail contents for a tabbed panel</h4>
	 * It's a marker to starts controls declarations for the panel specified.<br>
	 * This constructor lets to declare a popUp menu name. <br>
	 * <p>
	 * @param panelName the internal name of panel
	 * @param panelTabText the name of panel as displayed on tab
	 * @param popUpMenuName the name of popUp to make active
	 */
	public void BEGIN_PANEL_DETAIL_CONTENT(String panelName, String panelTabText, String popUpMenuName) {
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		this.activePanelName = panelName;
		this.activePanel = hm_panel.get(panelName);
		this.activePanel.setTabText(panelTabText);
		this.isActiveDeclarationPanel = true;
		
		// Panel NON dichiarato in struttura con PANEL_STRUCTURE() come di dettaglio
		// Potrebbe trattarsi di un errore nella dichiarazione della funzione oppure
		// di un subpanel dichiarato dentro un altro pannello
		// Viene in questo caso creato un pannello DETAIL e BOX_LAYOUT 
		if (this.activePanel == null) {
			BEGIN_PANEL_DETAIL_CONTENT(panelName, EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, panelTabText, popUpMenuName, 0, 0, 0, 0);
		}
		
		// Update descrittore pannello e struttura componenti
		this.getPanel(panelName).setPopUpMenuName(popUpMenuName);
		innerComponent = this.getComponentDeclared(panelName);
		if (innerComponent == null) {return;}
		innerComponent.popUpMenuName = popUpMenuName;
	}

	
	/**
	 * <h4>Begin panel detail contents</h4>
	 * It's the full featured declaring of a panel declared in another panel, just as a simple component.<br>
	 * The panel layed out as a normal component, has own and independent properties, like the latout manager<br>
	 * to use and any other kind of information depending on.<br>
	 * <p>
	 * @param panelName the internal name of panel
	 * @param panelType as a EnumForwardPanelType constant
	 * @param panelLayout the as a EnumForwardLayout constant
	 * @param panelTitle  
	 * @param hgap the horizontal gap if FLOW_LAYOUT or GRID_LAYOUT
	 * @param gridRows the rows number if GRID_LAYOUT or GRID_BAG_LAYOUT
	 * @param gridColumns columns number if GRID_LAYOUT or GRID_BAG_LAYOUT
	 */
	public void BEGIN_PANEL_DETAIL_CONTENT(String panelName
										, EnumForwardPanelType panelType
										, EnumForwardLayout panelLayout
										, String panelTitle
										, String popUpMenuName
									    , int hgap         				    		// Horizontal gap se FLOW_LAYOUT o GRID_LAYOUT
									    , int vgap        				    		// Vertical gap se FLOW_LAYOUT o GRID_LAYOUT
									    , int gridRows                              // Numero righe se GRID_LAYOUT o GRID_BAG_LAYOUT
									    , int gridColumns                           // Numero colonne se GRID_LAYOUT o GRID_BAG_LAYOUT
							) {
		
		ForwardPanel forwardPanel = null;
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		
		// Inserimento pannello in struttura panelli della funzione
		forwardPanel = putPanelDescriptor(panelName, panelType, panelLayout);
		
		// Update valori specifici panel
		forwardPanel.setPopUpMenuName(popUpMenuName);
		forwardPanel.setHgap(hgap);
		forwardPanel.setVgap(vgap);
		forwardPanel.setGridRows(gridRows);
		forwardPanel.setGridColumns(gridColumns);

		// Info attive per successivi COMPONENT() successivi a questa definizione
		this.activePanelName = panelName;
		this.activePanel = hm_panel.get(panelName);
		this.isActiveDeclarationPanel = true;

		// Recupero descrittore generale componente panel
		innerComponent = this.getComponentDeclared(panelName);
		
		// Il panel contenente questo pannello come suo componente 
		// NON è stato ancora dichiarato.
		// Verrà aggiornato al momento della dichiarazione del componente JPanel
		// Se componente registrato con questo nome NON è un panel nessuna operazione
		if (innerComponent == null) {return;}
		if (innerComponent.componentType != EnumForwardComponent.JPanel) {return;}
		
		// Panel richiamato precedentemente in un altro pannello.
		// Update descrittore pannello e componente
		innerComponent.popUpMenuName = popUpMenuName;
		forwardPanel.setGraphicObject(innerComponent.component);
	}
	
	
	
	
	/**
	 * <h3>End panel contents</h3>
	 * Ends field declarations for the current panel.<br>
	 * <p>
	 */
	public void END_PANEL_DETAIL_CONTENT() {
		this.activePanelName = "";
		this.activePanel = null;
		this.isActiveDeclarationPanel = false;
	}
	

	/**
	 * <h3>Panel JLabel control declaration</h3>
	 * 
	 * A JLabel Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "LAB01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "LAB01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * 
	 * @param jlabel the swing JLabelobject
	 * @param controlName
	 * @param row
	 * @param text of label
	 */
	public void COMPONENT(JLabel jlabel, String controlName, int row, String textLabel) {
		this.activeMenuName = "";
		jlabelCommon(jlabel, controlName, row, 0, 1, 1, textLabel, -1, -1);
	}

	/**
	 * <h3>Panel JLabel control declaration</h3>
	 * 
	 * A JLabel Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * @param jlabel the swing JLabelobject
	 * @param row
	 * @param text of label
	 */
	public void COMPONENT(JLabel jlabel, int row, String textLabel) {
		String controlName = "";
		
		this.activeMenuName = "";
		controlName = "#AUTOLAB" + this.numProgrLabel;
		this.numProgrLabel++;
		jlabelCommon(jlabel, controlName, row, 0, 1, 1, textLabel, -1, -1);
	}

	/**
	 * <h3>Panel JLabel control declaration</h3>
	 * 
	 * A JLabel Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "LAB01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "LAB01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * 
	 * @param jlabel the swing JLabelobject
	 * @param controlName
	 * @param row
	 * @param text of label
	 * @param width
	 */
	public void COMPONENT(JLabel jlabel, String controlName, int row, String textLabel, int width) {
		this.activeMenuName = "";
		jlabelCommon(jlabel, controlName, row, 0, 1, 1, textLabel, width, -1);
	}

	/**
	 * <h3>Panel JLabel control declaration</h3>
	 * 
	 * A JLabel Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "LAB01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "LAB01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * 
	 * @param jlabel the swing JLabelobject
	 * @param controlName
	 * @param row
	 * @param text of label
	 * @param width
	 * @param height
	 */
	public void COMPONENT(JLabel jlabel, String controlName, int row, String textLabel, int width, int height) {
		this.activeMenuName = "";
		jlabelCommon(jlabel, controlName, row, 0, 1, 1, textLabel, width, height);
	}


	/**
	 * <h3>Panel JLabel control declaration</h3>
	 * 
	 * A JLabel Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * @param jlabel the swing JLabelobject
	 * @param row
	 * @param text of label
	 * @param width
	 */
	public void COMPONENT(JLabel jlabel, int row, String textLabel, int width) {
		String controlName = "";

		this.activeMenuName = "";
		controlName = "#AUTOLAB" + this.numProgrLabel;
		this.numProgrLabel++;
		jlabelCommon(jlabel, controlName, row, 0, 1, 1, textLabel, width, -1);
	}

	/*
	 * Codice comune definizione JLabel
	 */
	private void jlabelCommon(JLabel jlabel, String controlName, int row, int col, int cellWidth, int cellHeight, String textLabel, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JLabel.class, EnumForwardComponent.JLabel, jlabel);
		jlabel.setName(controlName);
		jlabel.setText(textLabel);
		jlabel.setPreferredSize(new Dimension((width == -1) ? jlabel.getWidth() : width, (heigh == -1) ? jlabel.getHeight() : heigh));
		panelComponent = controlPut(controlName, jlabel, EnumForwardComponent.JLabel, row, col, cellWidth, cellHeight);
		
		// Il componente potrebbe non essere disposto in un pannello
		if (panelComponent != null) {
			panelComponent.setDefaultValue(textLabel);
		}
		
	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.componentObjectClass = String.class;
	}
	
	/**
	 * <h3>Panel JTextField control declaration</h3>
	 * 
	 * A JTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "TEXTFIELD01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "TEXTFIELD01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * <p>
	 * 
	 * @param jtextField the swing JTextField object
	 * @param controlName
	 * @param row
	 */
	public void COMPONENT(JTextField jtextField, String controlName, int row) {
		this.activeMenuName = "";
		jtextFieldCommon(jtextField, controlName, row, 0, 1, 1, "", -1, -1);
	}
	

	/**
	 * <h3>Panel JTextField control declaration</h3>
	 * 
	 * A JTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "TEXTFIELD01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "TEXTFIELD01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * <p>
	 * 
	 * @param jtextField the swing JTextField object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 * @param width
	 */
	public void COMPONENT(JTextField jtextField, String controlName, int row, String initialValue, int width) {
		this.activeMenuName = "";
		jtextFieldCommon(jtextField, controlName, row, 0, 1, 1, initialValue, width, -1);
	}

	/**
	 * <h3>Panel JTextField control declaration</h3>
	 * 
	 * A JTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "TEXTFIELD01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "TEXTFIELD01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * <p>
	 * 
	 * @param jtextField the swing JTextField object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 */
	public void COMPONENT(JTextField jtextField, String controlName, int row, String initialValue) {
		this.activeMenuName = "";
		jtextFieldCommon(jtextField, controlName, row, 0, 1, 1, initialValue, -1, -1);
	}

	/**
	 * <h3>Panel JTextField control declaration</h3>
	 * 
	 * A JTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * In the section <code>PANEL_STRUCTURE</code> it has been specified the layout manager for the panel.<br>
	 * Normally the detail panels contains rows, and the choosen layout manager is GRID_BAG_LAYOUT.<br>
	 * In this case are specified parameters specific for this type of layout.<br>
	 * <p>
	 * Any further parameter for the control is set in the section <code>BEGIN_TUNING_LAYOUT</code>,<br>
	 * where it is possible set all <b>native</b> parameters directly on the java object control.<br>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJLabel("Panel1", "TEXTFIELD01").setMinimunSize(150, 70)
	 *   getJLabel("Panel1", "TEXTFIELD01").setToolTipText("This is a label")
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * <p>
	 * 
	 * @param jtextField the swing JTextField object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 * @param width
	 */
	public void COMPONENT(JTextField jtextField, String controlName, int row, int width) {
		this.activeMenuName = "";
		jtextFieldCommon(jtextField, controlName, row, 0, 1, 1, "", width, -1);
	}



	/*
	 * Codice comune definizione JTextField
	 */
	private void jtextFieldCommon(JTextField jtextField, String controlName, int row, int col, int cellWidth, int cellHeight, String initialValue, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelcomponent = null;
		
		this.setComponentPropertiesDefault(JTextField.class, EnumForwardComponent.JTextField, jtextField);
		jtextField.setName(controlName);
		jtextField.getDocument().putProperty("name", controlName);				// Per individuazione source in DocumentListener
		jtextField.setText(initialValue);
		jtextField.setPreferredSize(new Dimension((width == -1) ? jtextField.getWidth() : width, (heigh == -1) ? jtextField.getHeight() : heigh));
		
		// inserimento componente in struttura generale e in quella del pannello attivo, se si sta popolando un panel
		panelcomponent = controlPut(controlName, jtextField, EnumForwardComponent.JTextField, row, col, cellWidth, cellHeight);
		panelcomponent.setDefaultValue(initialValue);
		panelcomponent.setBorderObject(((JTextField)panelcomponent.getGraphicObject()).getBorder());
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = String.class;
	}

	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * Constructor without initial value.<br>
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row) {
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, "", 10, -1, -1);

	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.componentObjectClass = String.class;

	}
	
	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 * @param columns as a size of width
	 * @param initialValue
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row, int columns, String initialValue) {
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, initialValue, columns, -1, -1);
	}
	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 * @param columns as a size of width
	 * @param initialValue
	 * @param width
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row, int columns, String initialValue, int width) {
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, initialValue, columns, width, -1);
	}
	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 * @param columns as a size of width
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row, int columns) {
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, "", columns, -1, -1);
	}
	
	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row, String initialValue) {
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, initialValue, 10, -1, -1);
		
	}
	
	/**
	 * <h3>Panel JPasswordField control declaration</h3>
	 * 
	 * A JPasswordField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jpasswordField object
	 * @param controlName
	 * @param row
	 * @param col
	 * @param initialValue
	 * @param width
	 */
	public void COMPONENT(JPasswordField jpasswordField, String controlName, int row, String initialValue, int width) {
		this.activeMenuName = "";
		jpasswordFieldCommon(jpasswordField, controlName, row, 0, 1, 1, initialValue, 10,width, -1);
		
	}
	
	/*
	 * Codice comune definizione JPasswordField
	 */
	private void jpasswordFieldCommon(JPasswordField jpasswordField, String controlName, int row, int col, int cellWidth, int cellHeight, String initialValue, int columns, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelcomponent = null;
		
		this.setComponentPropertiesDefault(JPasswordField.class, EnumForwardComponent.JPasswordField, jpasswordField);
		jpasswordField.setName(controlName);
		jpasswordField.getDocument().putProperty("ownerName", controlName);				// Per individuazione source in DocumentListener
		jpasswordField.setText(initialValue);
		jpasswordField.setColumns(columns);
		jpasswordField.setPreferredSize(new Dimension((width == -1) ? jpasswordField.getWidth() : width, (heigh == -1) ? jpasswordField.getHeight() : heigh));
		
		// inserimento componente in struttura generale e in quella del pannello attivo, se si sta popolando un panel
		panelcomponent = controlPut(controlName, jpasswordField, EnumForwardComponent.JPasswordField, row, col, cellWidth, cellHeight);
		panelcomponent.setDefaultValue(initialValue);
		panelcomponent.setBorderObject(((JPasswordField)panelcomponent.getGraphicObject()).getBorder());
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = String.class;
}
	
	
	/**
	 * <h3>Panel JFormattedTextField control declaration</h3>
	 * 
	 * A JFormattedTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param JFormattedTextField swing object
	 * @param controlName
	 * @param row
	 * @param initialValue as a String object, an Integer object or a Date object.
	 */
	public void COMPONENT(JFormattedTextField jformattedTextField, String controlName, int row, Object initialValue) {
		this.activeMenuName = "";
		jformattedTextFieldCommon(jformattedTextField, controlName, row, 0, 1, 1, initialValue, -1, -1);
	}
	
	/**
	 * <h3>Panel JFormattedTextField control declaration</h3>
	 * 
	 * A JFormattedTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param JFormattedTextField swing object
	 * @param controlName
	 * @param row
	 * @param initialValue as a String object, an Integer object or a Date object.
	 * @param width  
	 */
	public void COMPONENT(JFormattedTextField jformattedTextField, String controlName, int row, Object initialValue, int width) {
		this.activeMenuName = "";
		jformattedTextFieldCommon(jformattedTextField, controlName, row, 0, 1, 1, initialValue, width, -1);
	}
	
	/**
	 * <h3>Panel JFormattedTextField control declaration</h3>
	 * 
	 * A JFormattedTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param JFormattedTextField swing object
	 * @param controlName
	 * @param row
	 */
	public void COMPONENT(JFormattedTextField jformattedTextField, String controlName, int row) {
		this.activeMenuName = "";
		jformattedTextFieldCommon(jformattedTextField, controlName, row, 0, 1, 1, "", -1, -1);
		
	}
	

	
	/**
	 * <h3>Panel JFormattedTextField control declaration</h3>
	 * 
	 * A JFormattedTextField Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param JFormattedTextField swing object
	 * @param controlName
	 * @param row
	 * @param width
	 */
	public void COMPONENT(JFormattedTextField jformattedTextField, String controlName, int row, int width) {
		this.activeMenuName = "";
		jformattedTextFieldCommon(jformattedTextField, controlName, row, 0, 1, 1, "", width, -1);
	}
	

	/*
	 * Codice comune definizione JFormattedTextField
	 */
	private void jformattedTextFieldCommon(JFormattedTextField jformattedTextField, String controlName, int row, int col, int cellWidth, int cellHeight, Object initialValue, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelcomponent = null;
		
		this.setComponentPropertiesDefault(JFormattedTextField.class, EnumForwardComponent.JFormattedTextField, jformattedTextField);
		jformattedTextField.setName(controlName);
		jformattedTextField.setValue(initialValue);
		jformattedTextField.getDocument().putProperty("ownerName", controlName);				// Per individuazione source in DocumentListener
		jformattedTextField.setPreferredSize(new Dimension((width == -1) ? jformattedTextField.getWidth() : width, (heigh == -1) ? jformattedTextField.getHeight() : heigh));
		
		// inserimento componente in struttura generale e in quella del pannello attivo, se si sta popolando un panel
		panelcomponent = controlPut(controlName, jformattedTextField, EnumForwardComponent.JFormattedTextField, row, col, cellWidth, cellHeight);
		panelcomponent.setDefaultValue(initialValue);
		panelcomponent.setBorderObject(((JFormattedTextField)panelcomponent.getGraphicObject()).getBorder());
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = String.class;
 	    if (initialValue instanceof Integer) {
 	    	innerComponent.componentObjectClass = Integer.class;
 	    	return;
		}
	    if (initialValue instanceof Float) {
 	    	innerComponent.componentObjectClass = Float.class;
 	    	return;
		}
	    if (initialValue instanceof Long) {
 	    	innerComponent.componentObjectClass = Long.class;
 	    	return;
		}
	    if (initialValue instanceof Double) {
 	    	innerComponent.componentObjectClass = Double.class;
 	    	return;
		}
 	    if (initialValue instanceof Date) {
 	    	innerComponent.componentObjectClass = Date.class;
 	    	return;
		}
	}
	
	
	/**
	 * <h3>Panel JComboBox control declaration</h3>
	 * 
	 * A JComboBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcomboBox the swing JComboBox object
	 * @param controlName
	 * @param row
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JComboBox jcomboBox, String controlName, int row) {
		this.activeMenuName = "";
		jcomboBoxCommon(jcomboBox, controlName, row, 0, 1, 1, "", -1, -1);
	}
	
	/**
	 * <h3>Panel JComboBox control declaration</h3>
	 * 
	 * A JComboBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcomboBox the swing JComboBox object
	 * @param controlName
	 * @param row
	 * @param width
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JComboBox jcomboBox, String controlName, int row, int width) {
		this.activeMenuName = "";
		jcomboBoxCommon(jcomboBox, controlName, row, 0, 1, 1, "", width, -1);
	}
	
	/**
	 * <h3>Panel JComboBox control declaration</h3>
	 * 
	 * A JComboBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcomboBox the swing JComboBox object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JComboBox jcomboBox, String controlName, int row, String initialValue) {
		this.activeMenuName = "";
		jcomboBoxCommon(jcomboBox, controlName, row, 0, 1, 1, initialValue, -1, -1);
	}
	
	/**
	 * <h3>Panel JComboBox control declaration</h3>
	 * 
	 * A JComboBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcomboBox the swing JComboBox object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 * @param width
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JComboBox jcomboBox, String controlName, int row, String initialValue, int width) {
		this.activeMenuName = "";
		jcomboBoxCommon(jcomboBox, controlName, row, 0, 1, 1, initialValue, width, -1);
	}
	
	/*
	 * Codice comune definizione JComboBox
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void jcomboBoxCommon(JComboBox jcomboBox, String controlName, int row, int col, int cellWidth, int cellHeight, String initialValue, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelcomponent = null;
		
		this.setComponentPropertiesDefault(JComboBox.class, EnumForwardComponent.JComboBox, jcomboBox);
		jcomboBox.setName(controlName);
		jcomboBox.setModel(new ForwardComboBoxModel());
		jcomboBox.setPreferredSize(new Dimension((width == -1) ? jcomboBox.getWidth() : width, (heigh == -1) ? jcomboBox.getHeight() : heigh));

		// inserimento componente in struttura generale e in quella del pannello attivo, se si sta popolando un panel
		panelcomponent = controlPut(controlName, jcomboBox, EnumForwardComponent.JComboBox, row, col, cellWidth, cellHeight);
		
		// Se combo di servizio per JTable NON è un componente disposto direttamente sul panel
		if (this.isActiveDeclarationPanel) {
			panelcomponent.setDefaultValue(initialValue);		
		}
	
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = String.class;

	}
	

	/**
	 * <h3>Panel JTextArea control declaration</h3>
	 * 
	 * A JTextArea Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * The JTextArea object will be layed out inside a {@link JScrollBar} object with vertical and horizontal<br>
	 * bar just only if it needs.<br>
	 * <p>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextArea the swing JTextArea object
	 * @param controlName
	 * @param row
	 * @param textAreaRows
	 * @param textAreaColumns
	 * @param initialValue
	 */
	public void COMPONENT(JTextArea jtextArea, String controlName, int row, int textAreaRows, int textAreaColumns, String initialValue) {
		this.activeMenuName = "";
		jtextAreaCommon(jtextArea, controlName, row, 0, 1, 1, textAreaRows, textAreaColumns, initialValue, -1, -1, false);
	}

	/**
	 * <h3>Panel JTextArea control declaration</h3>
	 * 
	 * A JTextArea Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * The JTextArea object will be layed out inside a {@link JScrollBar} object with vertical and horizontal<br>
	 * bar just only if it needs.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the text area is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * <p>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextArea the swing JTextArea object
	 * @param controlName
	 * @param row
	 * @param textAreaRows
	 * @param textAreaColumns
	 * @param width the width of the JScrollPane used to show as a scrollable object
	 * @param height the height of the JScrollPane used to show as a scrollable object
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param initialValue
	 */
	public void COMPONENT(JTextArea jtextArea, String controlName, int row, int textAreaRows, int textAreaColumns, int width, int height, boolean maximumSizeToSet, String initialValue) {
		this.activeMenuName = "";
		jtextAreaCommon(jtextArea, controlName, row, 0, 1, 1, textAreaRows, textAreaColumns, initialValue, width, height, maximumSizeToSet);
	}

	/**
	 * <h3>Panel JTextArea control declaration</h3>
	 * 
	 * A JTextArea Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * The JTextArea object will be layed out inside a {@link JScrollBar} object with vertical and horizontal<br>
	 * bar just only if it needs.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextArea the swing JTextArea object
	 * @param controlName
	 * @param row
	 * @param textAreaRows
	 * @param textAreaColumns
	 * @param width the width of the JScrollPane used to show as a scrollable object
	 * @param height the height of the JScrollPane used to show as a scrollable object
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 */
	public void COMPONENT(JTextArea jtextArea, String controlName, int row, int textAreaRows, int textAreaColumns, int width, int height, boolean maximumSizeToSet) {
		this.activeMenuName = "";
		jtextAreaCommon(jtextArea, controlName, row, 0, 1, 1, textAreaRows, textAreaColumns, "", width, height, maximumSizeToSet);
	}

	
	/*
	 * Codice comune definizione jtextArea
	 */
	public void jtextAreaCommon(JTextArea jtextArea, String controlName, int row, int col, int cellWidth, int cellHeight, int textAreaRows, int textAreaColumns, String initialValue, int width, int height, boolean maximumSizeToSet) {
		ForwardPanelComponent panelComponent = null;
		JScrollPane jScrollPane = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JTextArea.class, EnumForwardComponent.JTextArea, jtextArea);
		jtextArea.setName(controlName);
		jtextArea.setRows(textAreaRows);
		jtextArea.setColumns(textAreaColumns);
		jtextArea.getDocument().putProperty("ownerName", controlName);				// Per individiuazione source in DocumentListener
		panelComponent = controlPut(controlName, jtextArea, EnumForwardComponent.JTextArea, row, col, cellWidth, cellHeight);	// -> panelComponent
		panelComponent.setDefaultValue(initialValue);
		
		// Generazione e store scroll pane.
		// Il monitor lo utilizzerà per inserire il controllo.
 	  	jScrollPane = new JScrollPane(jtextArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
 	  	jScrollPane.setPreferredSize(new Dimension((width == -1) ? jScrollPane.getWidth() : width, (height == -1) ? jScrollPane.getHeight() : height));
        if (maximumSizeToSet) {
    	  	jScrollPane.setMaximumSize(new Dimension((width == -1) ? jScrollPane.getWidth() : width, (height== -1) ? jScrollPane.getHeight() : height));
		}
 	  	panelComponent.setScrollPane(jScrollPane);
 	  	
 	  	// Update descrittore componenti con oggetto ScrollPane
 	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.jscrollPane = jScrollPane;
 	    innerComponent.componentObjectClass = String.class;
}
	
	/**
	 * <h3>Panel JTextPane control declaration</h3>
	 * 
	 * A JTextPane Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextPane the JTextPane swing object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 */
	public void COMPONENT(JTextPane jtextPane, String controlName, int row, String initialValue) {
		this.activeMenuName = "";
		jtextPaneCommon(jtextPane, controlName, row, 0, 1, 1, initialValue, -1, -1);
	}

	/**
	 * <h3>Panel JTextPane control declaration</h3>
	 * 
	 * A JTextPane Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextPane the JTextPane swing object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 * @param width
	 * @param heigh
	 */
	public void COMPONENT(JTextPane jtextPane, String controlName, int row, String initialValue, int width, int heigh) {
		this.activeMenuName = "";
		jtextPaneCommon(jtextPane, controlName, row, 0, 1, 1, initialValue, width, heigh);
	}

	/**
	 * <h3>Panel JTextPane control declaration</h3>
	 * 
	 * A JTextPane Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtextPane the JTextPane swing object
	 * @param controlName
	 * @param row
	 * @param initialValue
	 */
	public void COMPONENT(JTextPane jtextPane, String controlName, int row) {
		this.activeMenuName = "";
		jtextPaneCommon(jtextPane, controlName, row, 0, 1, 1, "", -1, -1);
	}

	/*
	 * Codice comune definizione JTextPane
	 */
	private void jtextPaneCommon(JTextPane jtextPane, String controlName, int row, int col, int cellWidth, int cellHeight, String initialValue, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		JScrollPane jScrollPane = null;
		
		this.setComponentPropertiesDefault(JTextPane.class, EnumForwardComponent.JTextPane, jtextPane);
		jtextPane.setName(controlName);
		jtextPane.getDocument().putProperty("ownerName", controlName);				// Per individiuazione source in DocumentListener
		jtextPane.setPreferredSize(new Dimension((width == -1) ? jtextPane.getWidth() : width, (heigh == -1) ? jtextPane.getHeight() : heigh));
		panelComponent = controlPut(controlName, jtextPane, EnumForwardComponent.JTextPane, row, col, cellWidth, cellHeight);

		// Generazione store scroll pane.
		// Il monitor lo utilizzerà per inserire il controllo.
 	  	jScrollPane = new JScrollPane(jtextPane, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
 	  	panelComponent.setScrollPane(jScrollPane);
	}
	

	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param cellWidth
	 * @param cellHeight
	 * @param textButton
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, " ", "", "", "", SwingConstants.CENTER, SwingConstants.CENTER, -1, -1);
	}

	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param cellWidth
	 * @param cellHeight
	 * @param textButton
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, -1, -1);
	}

	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param cellWidth
	 * @param cellHeight
	 * @param textButton
	 * @param width
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton, int width) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, width, -1);
	}

	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton, String iconPath) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, iconPath, "", "", SwingConstants.TOP, SwingConstants.RIGHT, -1, -1);
	}


	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 * @param verticalTextPosition as one of the following values:<pre> 
SwingConstants.TOP 
SwingConstants.BOTTOM 
	 * @param horizontalTextPosition as one of the following values:<pre> 
SwingConstants.RIGHT 
SwingConstants.LEFT 
SwingConstants.CENTER 
SwingConstants.LEADING 
SwingConstants.TRAILING (the default) </pre>
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton, String iconPath, int verticalTextPosition, int horizontalTextPosition ) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, iconPath, "", "", verticalTextPosition, horizontalTextPosition, -1, -1);
	}


	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 * @param iconPathRollover
	 * @param iconPathPressed
	 * @param verticalTextPosition as one of the following values:<pre> 
SwingConstants.TOP 
SwingConstants.BOTTOM 
	 * @param horizontalTextPosition as one of the following values:<pre> 
SwingConstants.RIGHT 
SwingConstants.LEFT 
SwingConstants.CENTER 
SwingConstants.LEADING 
SwingConstants.TRAILING (the default) </pre>
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton, String iconPath, String iconPathRollover, String iconPathPressed, int verticalTextPosition, int horizontalTextPosition ) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, iconPath, iconPathRollover, iconPathPressed, verticalTextPosition, horizontalTextPosition, -1, -1);
	}

	/**
	 * <h3>Panel JButton control declaration</h3>
	 * 
	 * A JButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jbutton the swing JButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 * @param iconPathRollover
	 * @param iconPathPressed
	 * @param verticalTextPosition as one of the following values:<pre> 
SwingConstants.TOP 
SwingConstants.BOTTOM 
	 * @param horizontalTextPosition as one of the following values:<pre> 
SwingConstants.RIGHT 
SwingConstants.LEFT 
SwingConstants.CENTER 
SwingConstants.LEADING 
SwingConstants.TRAILING (the default) </pre>
	 * @param width  
	 */
	public void COMPONENT(JButton jbutton, String controlName, int row, String textButton, String iconPath, String iconPathRollover, String iconPathPressed, int verticalTextPosition, int horizontalTextPosition, int width) {
		this.activeMenuName = "";
		jbuttonCommon(jbutton, controlName, row, 0, 1, 1, textButton, iconPath, iconPathRollover, iconPathPressed, verticalTextPosition, horizontalTextPosition, width, -1);
	}


	/*
	 * Codice comune definizione JButton
	 */
	private void jbuttonCommon(JButton jbutton, String controlName, int row, int col, int cellWidth, int cellHeight, String text, String iconPath, String iconPathRollover, String iconPathPressed, int verticalTextPosition, int horizontalTextPosition, int width, int heigh) {
		
		ForwardPanelComponent panelComponent = null;
		
		this.setComponentPropertiesDefault(JButton.class, EnumForwardComponent.JButton, jbutton);
		jbutton.setName(controlName);
		jbutton.setText(text);
		jbutton.setHorizontalTextPosition(horizontalTextPosition);
		jbutton.setVerticalTextPosition(verticalTextPosition);
		jbutton.setPreferredSize(new Dimension((width == -1) ? jbutton.getWidth() : width, (heigh == -1) ? jbutton.getHeight() : heigh));

		panelComponent = controlPut(controlName, jbutton, EnumForwardComponent.JButton, row, col, cellWidth, cellHeight);		// -> panelComponent
		
		// Il monitor creerà l'oggetto Icon e lo imposterà sul componente
		panelComponent = this.getPanelComponent(controlName);
 	  	panelComponent.setIconPath(iconPath);
 	  	panelComponent.setIconPathRollover(iconPathRollover);
 	  	panelComponent.setIconPathPressed(iconPathPressed);
	}
	


	/**
	 * <h3>Panel JToggleButton control declaration</h3>
	 * 
	 * A JToggleButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtoggleButton the swing JToggleButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param selected true to initial toggle
     */
	public void COMPONENT(JToggleButton jtoggleButton, String controlName, int row, String textButton, boolean selected) {
		this.activeMenuName = "";
		jtoggleButtonCommon(jtoggleButton, controlName, row, 0, 1, 1, textButton, "", "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, selected, -1, -1);
	}

	/**
	 * <h3>Panel JToggleButton control declaration</h3>
	 * 
	 * A JToggleButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtoggleButton the swing JToggleButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param width
	 * @param selected true to initial toggle
     */
	public void COMPONENT(JToggleButton jtoggleButton, String controlName, int row, String textButton, boolean selected, int width) {
		this.activeMenuName = "";
		jtoggleButtonCommon(jtoggleButton, controlName, row, 0, 1, 1, textButton, "", "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, selected, width, -1);
	}


	/**
	 * <h3>Panel JToggleButton control declaration</h3>
	 * 
	 * A JToggleButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtoggleButton the swing JToggleButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
     */
	public void COMPONENT(JToggleButton jtoggleButton, String controlName, int row, String textButton, String iconPath) {
		this.activeMenuName = "";
		jtoggleButtonCommon(jtoggleButton, controlName, row, 0, 1, 1, textButton, iconPath, "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, false, -1, -1);
	}

	/**
	 * <h3>Panel JToggleButton control declaration</h3>
	 * 
	 * A JToggleButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtoggleButton the swing JToggleButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 * @param @param selected true to initial toggle
     */
	public void COMPONENT(JToggleButton jtoggleButton, String controlName, int row, String textButton, String iconPath, boolean selected) {
		this.activeMenuName = "";
		jtoggleButtonCommon(jtoggleButton, controlName, row, 0, 1, 1, textButton, iconPath, "", "", "", SwingConstants.TOP, SwingConstants.RIGHT, selected, -1, -1);
	}



	/**
	 * <h3>Panel JToggleButton control declaration</h3>
	 * 
	 * A JToggleButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jtoggleButton the swing JToggleButton object
	 * @param controlName
	 * @param row
	 * @param textButton
	 * @param iconPath
	 * @param iconPathRollover
	 * @param iconPathPressed
	 * @param iconPathRolloverSelected
	 * @param verticalTextPosition as one of the following values:<pre> 
SwingConstants.TOP 
SwingConstants.BOTTOM 
	 * @param horizontalTextPosition as one of the following values:<pre> 
SwingConstants.RIGHT 
SwingConstants.LEFT 
SwingConstants.CENTER 
SwingConstants.LEADING 
SwingConstants.TRAILING (the default) </pre>
     * @param selected true to initial toggle
     */
	public void COMPONENT(JToggleButton jtoggleButton, String controlName, int row, String textButton, String iconPath, String iconPathRollover, String iconPathPressed, String iconPathRolloverSelected, int verticalTextPosition, int horizontalTextPosition, boolean selected) {
		this.activeMenuName = "";  
		jtoggleButtonCommon(jtoggleButton, controlName, row, 0, 1, 1, textButton, iconPath, iconPathRollover, iconPathPressed, iconPathRolloverSelected, verticalTextPosition, horizontalTextPosition, selected, -1, -1);
	}



	
	/*
	 * Codice comune definizione JToggleButton
	 */
	private void jtoggleButtonCommon(JToggleButton jtoggleButton, String controlName, int row, int col, int cellWidth, int cellHeight, String text, String iconPath, String iconPathRollover, String iconPathPressed, String iconPathRolloverSelected, int verticalTextPosition, int horizontalTextPosition, boolean selected, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;

		this.setComponentPropertiesDefault(JToggleButton.class, EnumForwardComponent.JToggleButton, jtoggleButton);
		jtoggleButton.setName(controlName);
		jtoggleButton.setText(text);
		jtoggleButton.setHorizontalTextPosition(horizontalTextPosition);
		jtoggleButton.setVerticalTextPosition(verticalTextPosition);
		jtoggleButton.setSelected(new Boolean(selected));
		jtoggleButton.setPreferredSize(new Dimension((width == -1) ? jtoggleButton.getWidth() : width, (heigh == -1) ? jtoggleButton.getHeight() : heigh));
		panelComponent = controlPut(controlName, jtoggleButton, EnumForwardComponent.JToggleButton, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(new Boolean(selected));
		
		// Il monitor creerà l'oggetto Icon e lo imposterà sul componente
 	  	panelComponent.setIconPath(iconPath);
	  	panelComponent.setIconPathRollover(iconPathRollover);
	  	panelComponent.setIconPathRolloverSelected(iconPathRolloverSelected);
 	  	panelComponent.setIconPathPressed(iconPathPressed);

		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = Boolean.class;
}

	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text, icon and selected state.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param iconPath
	 * @param groupName
	 * @param selected
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text, String iconPath, String groupName, boolean selected ) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, iconPath, selected, "", -1, -1);
	}
	
	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param iconPath
	 * @param selected
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text, String iconPath, boolean selected) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, iconPath, selected, "", -1, -1);
	}
	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param iconPath
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text, String iconPath) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, iconPath, false, "", -1, -1);
	}
	 


	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state set to false.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, "", false, "", -1, -1);
	}
	 
	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state set to false.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param width
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text, int width) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, "", false, "", width, -1);
	}
	 
	/**
	 * <h3>Panel JCheckBox control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state set to false.<br>
	 * A JCheckBox Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jcheckbox the swing JCheckBox object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param width
	 * @param selected
	 */
	public void COMPONENT(JCheckBox jcheckbox, String controlName, int row, String text, int width, boolean selected) {
		this.activeMenuName = "";
		jcheckboxCommon(jcheckbox, controlName, row, 0, 1, 1, text, "", selected, "", width, -1);
	}
	 
	/*
	 * Codice comune definizione JCheckBox
	 */
	private void jcheckboxCommon(JCheckBox jcheckbox, String controlName, int row, int col, int cellWidth, int cellHeight, String text, String iconPath, boolean selected, String groupName, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JCheckBox.class, EnumForwardComponent.JCheckBox, jcheckbox);
		jcheckbox.setName(controlName);
		jcheckbox.setText(text);
		jcheckbox.setSelected(selected);
		jcheckbox.setPreferredSize(new Dimension((width == -1) ? jcheckbox.getWidth() : width, (heigh == -1) ? jcheckbox.getHeight() : heigh));
		panelComponent = controlPut(controlName, jcheckbox, EnumForwardComponent.JCheckBox, row, col, cellWidth, cellHeight);
		panelComponent.setIconPath(iconPath);
		panelComponent.setGroupName(groupName);
		panelComponent.setDefaultValue(new Boolean(selected));
		
 	  	// Update descrittore componenti con nome gruppo
 	  	innerComponent = this.getComponentDeclared(controlName);
	  	innerComponent.componentType = EnumForwardComponent.JCheckBox;
	  	innerComponent.componentObjectClass = Boolean.class;
 	  	innerComponent.groupName = groupName;
 	}
	

	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text, icon and selected state.<br>
	 * A group name to enable toggling can be set too.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param iconPath
	 * @param selected
	 * @param groupName to let toggling
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, String iconPath, boolean selected, String groupName ) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, iconPath, selected, groupName, -1, -1);
	}


	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param selected
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, "", false, "", -1, -1);

}
	 
	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param selected
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, boolean selected) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, "", selected, "", -1, -1);
	}

	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param selected
	 * @param width
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, boolean selected, int width) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, "", selected, "", width, -1);
	}

	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text, icon and selected state.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param iconPath
	 * @param selected
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, String iconPath, boolean selected ) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, iconPath, selected, "", -1, -1);
	}


	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A group name to enable toggling can be set too.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param selected
	 * @param groupName to let toggling
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, boolean selected, String groupName) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, "", selected, groupName, -1, -1);
	}

	/**
	 * <h3>Panel JRadioButton control declaration</h3>
	 * 
	 * This is the constructor with text aligned horizontal center and vertical center by default and selected state.<br>
	 * A group name to enable toggling can be set too.<br>
	 * A JRadioButton Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jradioButton the swing JRadioButton object
	 * @param controlName
	 * @param row
	 * @param text
	 * @param selected
	 * @param groupName to let toggling
	 * @param width
	 */
	public void COMPONENT(JRadioButton jradioButton, String controlName, int row, String text, boolean selected, String groupName, int width) {
		this.activeMenuName = "";
		jradioButtonCommon(jradioButton, controlName, row, 0, 1, 1, text, "", selected, groupName, width, -1);
	}

	/*
	 * Codice comune definizione JRadioButton
	 */
	private void jradioButtonCommon(JRadioButton jradioButton, String controlName, int row, int col, int cellWidth, int cellHeight, String text, String iconPath, boolean selected, String groupName, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;

		this.setComponentPropertiesDefault(JRadioButton.class, EnumForwardComponent.JRadioButton, jradioButton);
		jradioButton.setName(controlName);
		jradioButton.setText(text);
		jradioButton.setSelected(new Boolean(selected));
		jradioButton.setPreferredSize(new Dimension((width == -1) ? jradioButton.getWidth() : width, (heigh == -1) ? jradioButton.getHeight() : heigh));
		panelComponent = controlPut(controlName, jradioButton, EnumForwardComponent.JRadioButton, row, col, cellWidth, cellHeight);
		panelComponent.setIconPath(iconPath);
		panelComponent.setGroupName(groupName);
		panelComponent.setDefaultValue(new Boolean(selected));

	  	// Update descrittore componenti con nome gruppo
 	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.groupName = groupName;
 	    innerComponent.componentObjectClass = Boolean.class;

	}


	/**
	 * <h3>Panel JSlider control declaration</h3>
	 * 
	 * This is the constructor for a slider with all properties available.<br>
	 * <br>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Here is an example of JSlider declaration:<br>
	 * <pre>
	 *   COMPONENT(new(JSlider), "sld_control", 0, 0, 1, 1, 
	 *                                                      new Integer(JSlider.HORIZONTAL),	 	Orientation
	 *                                                      new Integer(10),                   		Minimum
	 *                                                      new Integer(100),                  		Maximum
	 *                                                      new Integer(30),                   		Initial
	 *                                                      new Integer(20),                   		Major tick spacing
	 *                                                      new Integer(5),                   		Minor tick spacing
	 *                                                      true									Paint ticks
	 *                                                      true									Paint Labels
	 *                                                      true									Values range from higher to lower
	 *                                                      true									Values selection not continue but with snap to ticks
	 *                                                      true									Paint label standard with numeric value
	 *                                                      new Integer[]{20, 40, 80},     	 		Labels values (not significative)
	 *                                                      new String[]{"Low", "Medim", "High"}	Labels strings (not significative)
	 *                                                      "lb_model" 								Label name object model for font and color of slider labels (not significative)
	 * </pre>
	 * 
	 * 
	 * @param jslider the JSlider object
	 * @param controlName
	 * @param row
	 * @param orientation as JSlider.HORIZONTAL or JSlider.VERTICAL
	 * @param min
	 * @param max
	 * @param initialValue
	 * @param majorTickSpacing
	 * @param minorTickSpacing
	 * @param paintTicks true to paint ticks for majorTickSpacing and minorTickSpacing
	 * @param paintLabels true to paint numeric or custom labels after ticks
	 * @param invertedRange true to show values range from higher to lower
	 * @param snapToTicks true to make selecting values not continue
	 * @param labelsStandard true for labels as standard numbers and false for custom labels
	 * @param ar_valueLabel	the array with numeric values to which assign a personalized label significative if labelsStandard is false
	 * @param ar_label the string array with personalized labels significative if labelsStandard is false
	 * @param labelNameModel the name of a label component, previously defined, as a model foreground and background color and font significative if labelsStandard is false
	 */
	public void COMPONENT(JSlider jslider, String controlName, int row, int orientation, int min, int max, int initialValue, int majorTickSpacing, int minorTickSpacing,  boolean paintTicks, boolean paintLabels, boolean invertedRange, boolean snapToTicks, boolean labelsStandard, int ar_valueLabel[], String ar_label[], String labelNameModel) {
		
		jsliderCommon(jslider
					, controlName
					, row
					, 0
					, 1
					, 1
					, orientation
					, min
					, max
					, initialValue
					, majorTickSpacing
					, minorTickSpacing
					, paintTicks
					, paintLabels
					, invertedRange
					, snapToTicks
					, labelsStandard
					, ar_valueLabel
					, ar_label
					, labelNameModel
					, -1
					, -1
				);
	}
	
	/**
	 * <h3>Panel JSlider control declaration</h3>
	 * 
	 * This is the minimal constructor for a slider horizontal by default with values min, max and current.<br>
	 * No ticks and labels will be displayed.<br>
	 * Values range is continue.<br>
	 * <p>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Here is an example of JSlider declaration:<br>
	 * <pre>
	 * 	 * <pre>
	 *   COMPONENT(new JSlider(), "sld_control", 0, 0, 1, 1, 
	 *                                                                    new Integer(10),                   Minimum
	 *                                                                    new Integer(100),                  Maximum
	 *                                                                    new Integer(30),                   Initial
	 * </pre>
	 * 
	 * @param jslider the JSlider object
	 * @param controlName
	 * @param row 
	 * @param min
	 * @param max
	 * @param initialValue
	 */
	public void COMPONENT(JSlider jslider, String controlName, int row, int min, int max, int initialValue) {
		this.activeMenuName = "";
		
		jsliderCommon(jslider
					, controlName
					, row
					, 0
					, 1
					, 1
					, JSlider.HORIZONTAL
					, min
					, max
					, initialValue
					, 0
					, 0
					, false						// No paintTicks 
					, false						// No paintLabels 	
					, false                     // No invertedeRange
					, false						// No snapToTicks
					, false						// No labelsStandard
					, null						// No ar_valueLabel
					, null						// No ar_label
					, null						// No labelNameModel		
					, -1
					, -1
					);

	}
	
	/**
	 * <h3>Panel JSlider control declaration</h3>
	 * 
	 * This is the minimal constructor for a slider horizontal by default with values min, max and current.<br>
	 * Orientation can be set.<br>
	 * No ticks and labels will be displayed.<br>
	 * Values range is continue.<br>
	 * <p>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Here is an example of JSlider declaration:<br>
	 * <pre>
	 * 	 * <pre>
	 *   COMPONENT(EnumForwardJComponent.JSlider, "sld_control", 0, 0, 1, 1, 
	 *                                                                    new Integer(JSlider.HORIZONTAL),	 Orientation
	 *                                                                    new Integer(10),                   Minimum
	 *                                                                    new Integer(100),                  Maximum
	 *                                                                    new Integer(30),                   Initial
	 * </pre>
	 * 
	 * @param slider the object
	 * @param controlName
	 * @param row
	 * @param orientation as JSlider.HORIZONTAL or JSlider.VERTICAL
	 * @param min
	 * @param max
	 * @param initialValue
	 */
	public void COMPONENT(JSlider jslider, String controlName, int row, int orientation, int min, int max, int initialValue) {
		this.activeMenuName = "";
		
		jsliderCommon(jslider
					, controlName
					, row
					, 0
					, 1
					, 1
					, orientation
					, min
					, max
					, initialValue
					, 0
					, 0
					, false						// No paintTicks 
					, false						// No paintLabels 	
					, false                     // No invertedeRange
					, false						// No snapToTicks
					, false						// No labelsStandard
					, null						// No ar_valueLabel
					, null						// No ar_label
					, null						// No labelNameModel		
					, -1
					, -1
					);
	}
	
	
	/**
	 * <h3>Panel JSlider control declaration</h3>
	 * 
	 * This is the constructor for a slider with orientation, values min, max and current, major and minor tick spacing.<br>
	 * Orientation can be set.<br>
	 * Ticks will be painted.<br>
	 * Snap to ticks can be set.<br>
	 * Values range from higher to lower or from lower to higher can be set
	 * Labels numeric can be painted.
	 * <br>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Here one an example of JSlider declaration:<br>
	 * <pre>
	 *   COMPONENT(EnumForwardJComponent.JSlider, "sld_control", 0, 0, 1, 1, 
	 *                                                                    new Integer(JSlider.HORIZONTAL),	 Orientation
	 *                                                                    new Integer(10),                   Minimum
	 *                                                                    new Integer(100),                  Maximum
	 *                                                                    new Integer(30),                   Initial
	 *                                                                    new Integer(20),                   Major tick spacing
	 *                                                                    new Integer(5),                    Minor tick spacing
	 *                                                                    true								 Values from higher to lower
	 *                                                                    true								 Values selection not continue
	 *                                                                    true								 Paint labels numeric
	 * </pre>
	 * 
	 * 
	 * @param jslider the JSlider object
	 * @param controlName
	 * @param row
	 * @param orientation 
	 * @param min
	 * @param max
	 * @param initialValue
	 * @param majorTickSpacing
	 * @param minorTickSpacing
	 * @param invertedRange true to show values range from higher to lower
	 * @param snapToTicks true to make selecting values not continue
	 * @param paintLabelsNumeric true to paint numeric labels after ticks
	 */
	public void COMPONENT(JSlider jslider, String controlName, int row, int orientation, int min, int max, int initialValue, int majorTickSpacing, int minorTickSpacing, boolean invertedRange, boolean snapToTicks, boolean paintLabelsNumeric) {
		this.activeMenuName = "";

		jsliderCommon(jslider
					, controlName
					, row
					, 0
					, 1
					, 1
					, orientation
					, min
					, max
					, initialValue
					, majorTickSpacing
					, minorTickSpacing
					, true						// Yes paintTicks 
					, paintLabelsNumeric		// No paintLabels 	
					, invertedRange             // No invertedeRange
					, snapToTicks    			// snapToTicks true or false
					, true						// Paint labelsStandard numeric
					, null						// No ar_valueLabel
					, null						// No ar_label
					, null						// No labelNameModel		
					, -1
					, -1
					);
	}
	
	/**
	 * <h3>Panel JSlider control declaration</h3>
	 * 
	 * This is the constructor for a slider with orientation, values min, max and current, major and minor tick spacing.<br>
	 * Orientation can be set.<br>
	 * Ticks will be painted.<br>
	 * Snap to ticks can be set.<br>
	 * Values range from higher to lower or from lower to higher can be set
	 * Labels custom will be painted.
	 * <br>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Here one an example of JSlider declaration:<br>
	 * <pre>
	 *   COMPONENT(EnumForwardJComponent.JSlider, "sld_control", 0, 0, 1, 1, 
	 *                                                                    new Integer(JSlider.HORIZONTAL),	 Orientation
	 *                                                                    new Integer(10),                   Minimum
	 *                                                                    new Integer(100),                  Maximum
	 *                                                                    new Integer(30),                   Initial
	 *                                                                    new Integer(20),                   Major tick spacing
	 *                                                                    new Integer(5),                    Minor tick spacing
	 *                                                                    true								 Values from higher to lower
	 *                                                                    true								 Values selection not continue
	 *                                                                    true								 Paint labels numeric
	 * </pre>
	 * 
	 * 
	 * @param jslider the JSlider object
	 * @param controlName
	 * @param row
	 * @param orientation 
	 * @param min
	 * @param max
	 * @param initialValue
	 * @param majorTickSpacing
	 * @param minorTickSpacing
	 * @param invertedRange true to show values range from higher to lower
	 * @param snapToTicks true to make selecting values not continue
	 * @param ar_valueLabel	the array with numeric values to which assign a personalized label significative if labelsStandard is false
	 * @param ar_label the string array with personalized labels significative if labelsStandard is false
	 * @param labelNameModel the name of a label component, previously defined, as a model foreground and background color and font significative if labelsStandard is false
	 */
	public void COMPONENT(JSlider jslider, String controlName, int row, int orientation, int min, int max, int initialValue, int majorTickSpacing, int minorTickSpacing, boolean invertedRange, boolean snapToTicks, int ar_valueLabel[], String ar_label[], String labelNameModel) {
		this.activeMenuName = "";

		jsliderCommon(jslider
					, controlName
					, row
					, 0
					, 1
					, 1
					, orientation
					, min
					, max
					, initialValue
					, majorTickSpacing
					, minorTickSpacing
					, true						// Yes paintTicks 
					, true						// Yes paintLabels 	
					, invertedRange             // No invertedeRange
					, snapToTicks    			// snapToTicks true or false
					, false						// Paint labels custom
					, ar_valueLabel				// Yes ar_valueLabel
					, ar_label					// Yes ar_label
					, labelNameModel			// Yes labelNameModel	
					, -1
					, -1
					);
	}

	
	/*
	 * Common code per oggetto JSlider
	 */
	private void jsliderCommon(JSlider jslider, String controlName, int row, int col, int cellWidth, int cellHeight, int orientation, int min, int max, int initialValue, int majorTickSpacing, int minorTickSpacing, boolean paintTicks, boolean paintLabels, boolean inverted, boolean snapToTicks, boolean labelsStandard, int ar_valueLabel[], String ar_label[], String labelModel, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		Dictionary<Integer, JLabel> labelsDictionary = null;
		JLabel jlabel = null;
		JLabel jlabelModel = null;

		this.setComponentPropertiesDefault(JSlider.class, EnumForwardComponent.JSlider, jslider);
		jslider.setName(controlName);
		jslider.setOrientation(orientation);
		jslider.setMinimum(min);
		jslider.setMaximum(max);
		jslider.setValue(initialValue);
		jslider.setPaintTicks(paintTicks);
		jslider.setMajorTickSpacing(majorTickSpacing);
		jslider.setMinorTickSpacing(minorTickSpacing);
		jslider.setInverted(inverted);
		jslider.setSnapToTicks(snapToTicks);
		jslider.setPaintLabels(paintLabels);
		jslider.setPreferredSize(new Dimension((width == -1) ? jslider.getWidth() : width, (heigh == -1) ? jslider.getHeight() : heigh));
		
		// Si visualizzano i numeri dopo i tick o nulla se paintLabels = false
		if (labelsStandard && minorTickSpacing > 0) {
			jslider.createStandardLabels(minorTickSpacing);
			panelComponent = controlPut(controlName, jslider, EnumForwardComponent.JSlider, row, col, cellWidth, cellHeight);
			panelComponent.setDefaultValue(new Integer(initialValue));
			return;
		} 
		
		// Le label sono personalizzate
		
		// Recupero modello label personalizzata
		jlabelModel = getJLabel(labelModel);
		if (jlabelModel == null) {
			jlabelModel = new JLabel();
		}
		
		// Label personalizzate, per ogni valore numerico personalizzato fornito
		if (ar_label != null) {
			labelsDictionary = new Hashtable<Integer, JLabel>();
	 		for (int i = 0; i < ar_valueLabel.length; i++) {
	 			jlabel = new JLabel(ar_label[i]);
	 			if (jlabelModel != null) {
	 				jlabel.setFont(jlabelModel.getFont()); 
	 				jlabel.setForeground(jlabelModel.getForeground());
	 				jlabel.setBackground(jlabelModel.getBackground());
				}
	 			labelsDictionary.put(ar_valueLabel[i], jlabel);
	  		}
	 		jslider.setLabelTable(labelsDictionary);
			jslider.setPaintLabels(true);
		}
		
		panelComponent = controlPut(controlName, jslider, EnumForwardComponent.JSlider, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(new Integer(initialValue));
 	  	
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = Integer.class;

	}



	/**
	 * <h3>Panel JSeparator control declaration</h3>
	 * 
	 * This is the constructor for a separator with orientation horizontal by default.<br>
	 * A JSlider Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jseparator object
	 * @param controlName
	 * @param row
	 */
	public void COMPONENT(JSeparator jseparator, String controlName, int row) {
		this.activeMenuName = "";
		jseparatorCommon(jseparator, controlName, row, 0, 1, 1, SwingConstants.HORIZONTAL, -1, -1);
	}
	
	
	/**
	 * <h3>Panel JSeparator control declaration</h3>
	 * 
	 * This is the constructor for a separator with orientation.<br>
	 * Orientation is coded as SwingConstants.HORIZONTAL or SwingConstants.VERTICAL<br>
	 * A JSeparator Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jseparator object
	 * @param controlName
	 * @param row
	 * @param orientation 
	 */
	public void COMPONENT(JSeparator jseparator, String controlName, int row, int orientation) {
		this.activeMenuName = "";
		jseparatorCommon(jseparator, controlName, row, 0, 1, 1, orientation, -1, -1);
	}

	/**
	 * <h3>Panel JSeparator control declaration</h3>
	 * 
	 * This is the constructor for a separator with orientation.<br>
	 * Orientation is coded as SwingConstants.HORIZONTAL or SwingConstants.VERTICAL<br>
	 * A JSeparator Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jseparator object
	 * @param controlName
	 * @param row
	 * @param orientation 
	 * @param width 
	 * @param heigh 
	 */
	public void COMPONENT(JSeparator jseparator, String controlName, int row, int orientation, int width, int heigh) {
		this.activeMenuName = "";
		jseparatorCommon(jseparator, controlName, row, 0, 1, 1, orientation, -1, -1);
	}

	/*
	 * Common code per oggetto JSeparator
	 */
	public void jseparatorCommon(JSeparator jseparator, String controlName, int row, int col, int cellWidth, int cellHeight, int orientation, int width, int heigh) {
		this.setComponentPropertiesDefault(JSeparator.class, EnumForwardComponent.JSeparator, jseparator);
		jseparator.setName(controlName);
		jseparator.setOrientation(orientation);
		jseparator.setPreferredSize(new Dimension((width == -1) ? jseparator.getWidth() : width, (heigh == -1) ? jseparator.getHeight() : heigh));
		controlPut(controlName, jseparator, EnumForwardComponent.JSeparator, row, col, cellWidth, cellHeight);
	}
	

	/**
	 * <h3>Panel JSpinner control string declaration</h3>
	 * 
	 * This is the constructor for a spinner string list<br>
	 * It's possible specify the name of the label registered for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jspinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param ar_string the spinner string array
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName, String ... ar_string) {
		this.activeMenuName = "";
		jspinnerCommonText(jspinner, controlName, row, 0, 1, 1, -1, -1, labelName, "", ar_string);
	}

	/**
	 * <h3>Panel JSpinner control string declaration</h3>
	 * 
	 * This is the constructor for a spinner string list<br>
	 * It's possible specify the name of the label registered for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jspinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param isFirstStringDefault as a boolean true if the first ar_string element is the initialValue  
	 * @param ar_string the spinner string array
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName,  boolean isFirstStringDefault, String ... ar_string) {
		this.activeMenuName = "";
		String initialValue = "";
		String ar_stringToUse[] = null;
		
		if (isFirstStringDefault) {
			initialValue = ar_string[0];
			ar_stringToUse = new String[ar_string.length - 1];
			for (int i = 1; i < ar_string.length; i++) {
				ar_stringToUse[i - 1] = ar_string[i];
			}
		} else {
			initialValue = "";
			ar_stringToUse = ar_string;
		}
		jspinnerCommonText(jspinner, controlName, row, 0, 1, 1, -1, -1, labelName, initialValue, ar_stringToUse);
	}

	/**
	 * <h3>Panel JSpinner control string declaration</h3>
	 * 
	 * This is the constructor for a spinner string list<br>
	 * It's possible specify the name of the label registered for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param jspinner object
	 * @param controlName
	 * @param row
	 * @param width
	 * @param labelName for spinner
	 * @param isFirstStringDefault as a boolean true if the first ar_string element is the initialValue  
	 * @param ar_string the spinner string array
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, int width, String labelName,  boolean isFirstStringDefault, String ... ar_string) {
		this.activeMenuName = "";
		String initialValue = "";
		String ar_stringToUse[] = null;
		
		if (isFirstStringDefault) {
			initialValue = ar_string[0];
			ar_stringToUse = new String[ar_string.length - 1];
			for (int i = 1; i < ar_string.length; i++) {
				ar_stringToUse[i - 1] = ar_string[i];
			}
		} else {
			initialValue = "";
			ar_stringToUse = ar_string;
		}
		jspinnerCommonText(jspinner, controlName, row, 0, 1, 1, width, -1, labelName, initialValue, ar_stringToUse);
	}

	/*
	 * Common code per oggetto JSpinner text
	 */
	private void jspinnerCommonText(JSpinner jspinner, String controlName, int row, int col, int cellWidth, int cellHeight, int width, int heigh, String labelName, String initialValue, String ... ar_string) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		SpinnerListModel spinnerListModel = null;
		JFormattedTextField jformattedTextField = null;
	    JLabel labelForSpinner = null;
	    JComponent editor = null;
	    
		this.setComponentPropertiesDefault(JSpinner.class, EnumForwardComponent.JSpinner, jspinner);
        
	    // Impostazione modello con le stringhe e la label associata
	    spinnerListModel = new SpinnerListModel(ar_string);
	    jspinner.setModel(spinnerListModel);
	    jspinner.setPreferredSize(new Dimension((width == -1) ? jspinner.getWidth() : width, (heigh == -1) ? jspinner.getHeight() : heigh));
        labelForSpinner = getJLabel(labelName);
        if (labelForSpinner != null) {
            labelForSpinner.setLabelFor(jspinner);
		}
   
        // Recupero editor per impostazione numero colonne spinner
        editor = jspinner.getEditor();
        if (editor instanceof JSpinner.DefaultEditor) {
        	jformattedTextField = ((JSpinner.DefaultEditor)editor).getTextField();
        	jformattedTextField.setColumns(ar_string.length); 				// Numero colonne fornite
        	jformattedTextField.setHorizontalAlignment(JTextField.RIGHT);
	    }
		
        // Valore iniziale
        jspinner.setValue(initialValue);
        
        // Assegnazione nome e inserimento in strutture interne
	    jspinner.setName(controlName);
		panelComponent = controlPut(controlName, jspinner, EnumForwardComponent.JSpinner, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(initialValue);
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = String.class;
	}

		/**
	 * <h3>Panel JSpinner control numeric declaration</h3>
	 * 
	 * This is the constructor for a spinner numeric<br>
	 * To make the number to be formatted without a thousands separator, specify an aditor mask like <code>#</code>.
	 * It's possible specify the name of the label regestred for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param spinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param editor mask
	 * @param initialValue 
	 * @param min
	 * @param max
	 * @param step
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName, String editorMask, int initialValue, int min, int max, int step) {
	    this.activeMenuName = "";
	    jspinnerCommonNumeric(jspinner, controlName, row, 0, 1, 1, labelName, editorMask, initialValue, min, max, step, -1, -1);
	}
	
	/**
	 * <h3>Panel JSpinner control numeric declaration</h3>
	 * 
	 * This is the constructor for a spinner numeric<br>
	 * To make the number to be formatted without a thousands separator, specify an aditor mask like <code>#</code>.
	 * It's possible specify the name of the label regestred for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param spinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param editor mask
	 * @param initialValue 
	 * @param min
	 * @param max
	 * @param step
	 * @param width
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName, String editorMask, int initialValue, int min, int max, int step, int width) {
	    this.activeMenuName = "";
	    jspinnerCommonNumeric(jspinner, controlName, row, 0, 1, 1, labelName, editorMask, initialValue, min, max, step, -1, -1);
	}
	
	/*
	 * Common code per oggetto JSpinner numeric
	 */
	private void jspinnerCommonNumeric(JSpinner jspinner, String controlName, int row, int col, int cellWidth, int cellHeight, String labelName, String editorMask, int initialValue, int min, int max, int step, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		SpinnerNumberModel spinnerNumberModel = null;
		NumberEditor numberEditor = null;
		DefaultFormatter formatter = null;
		JFormattedTextField jformattedTextField = null;
	    JLabel labelForSpinner = null;
	    String w_editorMask = "#";
	    
	    this.setComponentPropertiesDefault(JSpinner.class, EnumForwardComponent.JSpinner, jspinner);
	    
	    // Impostazione modello con le stringhe e la label associata
	    spinnerNumberModel = new SpinnerNumberModel(initialValue, min, max, step);
	    jspinner.setModel(spinnerNumberModel);
	    jspinner.setPreferredSize(new Dimension((width == -1) ? jspinner.getWidth() : width, (heigh == -1) ? jspinner.getHeight() : heigh));
        labelForSpinner = getJLabel(labelName);
        if (labelForSpinner != null) {
            labelForSpinner.setLabelFor(jspinner);
		}
   
        // Impopstazione editor per numeri
        // Se editorMask è null allora viene impostata a '#' per evitare il separatore di migliaia
        if (editorMask != null) {
        	w_editorMask = editorMask;
		}
        numberEditor = new JSpinner.NumberEditor(jspinner, w_editorMask);
        jspinner.setEditor(numberEditor);

        // Valore iniziale
        jspinner.setValue(initialValue);
        
        // Per gestione stateChanged(), in presenza di ON_APPL_SPINNER_TICKED()
        jformattedTextField = numberEditor.getTextField();
        formatter = (DefaultFormatter) jformattedTextField.getFormatter();
        formatter.setCommitsOnValidEdit(true);

        // Assegnazione nome e inserimento in strutture interne
	    jspinner.setName(controlName);
		panelComponent = controlPut(controlName, jspinner, EnumForwardComponent.JSpinner, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(initialValue);
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = Integer.class;

	}
	
	/**
	 * <h3>Panel JSpinner control date declaration</h3>
	 * 
	 * This is the constructor for a spinner of date<br>
	 * To make the date to be formatted and verified, specify an editor mask like, for example,  <code>MM/yyyy</code>.<br>
	 * or <code>dd/MM/yyyy</code><br>
	 * Parameters date can be achieved for example in this way:
	 * <pre>
	 *  Date initDate = calendar.getTime();
     *  calendar.add(Calendar.YEAR, -100);
     *  Date earliestDate = calendar.getTime();
     *  calendar.add(Calendar.YEAR, 200);
     *  Date latestDate = calendar.getTime();
     *</pre>
	 * It's possible specify the name of the label regestred for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param spinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param editor mask
	 * @param initDate
	 * @param earliestDate
	 * @param latestDate
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName, String editorMask, Date initDate, Date earliestDate, Date latestDate) {
	    this.activeMenuName = "";
		jspinnerCommonDate(jspinner, controlName, row, 0, 1, 1, labelName, editorMask, initDate, earliestDate, latestDate, -1, -1);
	}

	/**
	 * <h3>Panel JSpinner control date declaration</h3>
	 * 
	 * This is the constructor for a spinner of date<br>
	 * To make the date to be formatted and verified, specify an editor mask like, for example,  <code>MM/yyyy</code>.<br>
	 * or <code>dd/MM/yyyy</code><br>
	 * Parameters date can be achieved for example in this way:
	 * <pre>
	 *  Date initDate = calendar.getTime();
     *  calendar.add(Calendar.YEAR, -100);
     *  Date earliestDate = calendar.getTime();
     *  calendar.add(Calendar.YEAR, 200);
     *  Date latestDate = calendar.getTime();
     *</pre>
	 * It's possible specify the name of the label regestred for spinner, with no effects if an empty string.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * A JSpinner Swing control will be laid out on the panel starting at row and column specified<br>
	 * for the number of cells in horizontal (cellWidth) and in vertical (cellHeight) specified.<br>
	 * If a GRID_BAG_LAYOUT manager has been used to lay out the control on the panel, a {@link GridBagConstraints}<br>
	 * will be generated and stored at the panel component level.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param spinner object
	 * @param controlName
	 * @param row
	 * @param labelName for spinner
	 * @param editor mask
	 * @param initDate
	 * @param earliestDate
	 * @param latestDate
	 * @param width
	 */
	public void COMPONENT(JSpinner jspinner, String controlName, int row, String labelName, String editorMask, Date initDate, Date earliestDate, Date latestDate, int width) {
	    this.activeMenuName = "";
		jspinnerCommonDate(jspinner, controlName, row, 0, 1, 1, labelName, editorMask, initDate, earliestDate, latestDate, width, -1);
	}

	/*
	 * Common code per oggetto JSpinner date
	 */
	private void jspinnerCommonDate(JSpinner jspinner, String controlName, int row, int col, int cellWidth, int cellHeight, String labelName, String editorMask, Date initDate, Date earliestDate, Date latestDate, int width, int heigh) {
		
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		SpinnerDateModel spinnerDateModel = null;
		DateEditor dateEditor = null;
		JFormattedTextField jformattedTextField = null;
		DefaultFormatter formatter = null;
	    JLabel labelForSpinner = null;
	    String w_editorMask = "MM/yyyy";
	    
	    this.setComponentPropertiesDefault(JSpinner.class, EnumForwardComponent.JSpinner, jspinner);
	    
	    // Impostazione modello con le stringhe e la label associata
	    spinnerDateModel = new SpinnerDateModel(initDate, earliestDate, latestDate, 1);
	    jspinner.setModel(spinnerDateModel); 
	    jspinner.setPreferredSize(new Dimension((width == -1) ? jspinner.getWidth() : width, (heigh == -1) ? jspinner.getHeight() : heigh));
        labelForSpinner = getJLabel(labelName);
        if (labelForSpinner != null) {
            labelForSpinner.setLabelFor(jspinner);
		}
   
        // Impopstazione editor per numeri
        // Se editorMask è null allora viene impostata a '#' per evitare il separatore di migliaia
        if (editorMask != null) {
        	w_editorMask = editorMask;
		}
        dateEditor = new DateEditor(jspinner, w_editorMask);
        jspinner.setEditor(dateEditor);
        
        // Valore iniziale
        jspinner.setValue(initDate);
        
        // Per gestione stateChanged(), in presenza di ON_APPL_SPINNER_TICKED()
        jformattedTextField = dateEditor.getTextField();
        formatter = (DefaultFormatter) jformattedTextField.getFormatter();
        formatter.setCommitsOnValidEdit(true);
        
        // Assegnazione nome e inserimento in strutture interne
	    jspinner.setName(controlName);
		panelComponent = controlPut(controlName, jspinner, EnumForwardComponent.JSpinner, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(initDate);
		
		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = Date.class;

	}



	/**
	 * <h3>Panel JProgressBar control declaration</h3>
	 * 
	 * This is the constructor for a JProgressBar horizontal control with percent text painted and starting from minimum value<br>
	 * <p>
	 * Any further parameter can be changed by esplicit code in the declared function.
	 * <p>
	 * @param jprogressBar object
	 * @param controlName
	 * @param row
	 * @param orientation as SwingConstants.VERTICAL or SwingConstants.HORIZONTAL
	 * @param min the minimumm value 
	 * @param max the maximum value
	 * @param stringPainted true for percent string painted
	 * @param initialValue >= min & <= max
	 */
	public void COMPONENT(JProgressBar jprogressBar, String controlName, int row, int min, int max) {
	   this.activeMenuName = "";
	   jprogressBarCommon(jprogressBar, controlName, row, 0, 1, 1, SwingConstants.HORIZONTAL, min, max, true, min, -1, -1);

	}
	

	/**
	 * <h3>Panel JProgressBar control declaration</h3>
	 * 
	 * This is the constructor for a JProgressBar control<br>
	 * <p>
	 * Any further parameter can be changed by esplicit code in the declared function.
	 * <p>
	 * @param jprogressBar object
	 * @param controlName
	 * @param row
	 * @param orientation as SwingConstants.VERTICAL or SwingConstants.HORIZONTAL
	 * @param min the minimumm value 
	 * @param max the maximum value
	 * @param stringPainted true for percent string painted
	 */
	public void COMPONENT(JProgressBar jprogressBar, String controlName, int row, int orientation, int min, int max, boolean stringPainted) {
	   this.activeMenuName = "";
 	   jprogressBarCommon(jprogressBar, controlName, row, 0, 1, 1, orientation, min, max, stringPainted, min, -1, -1);
	}
	
	/**
	 * <h3>Panel JProgressBar control declaration</h3>
	 * 
	 * This is the constructor for a JProgressBar control<br>
	 * <p>
	 * Any further parameter can be changed by esplicit code in the declared function.
	 * <p>
	 * @param jprogressBar object
	 * @param controlName
	 * @param row
	 * @param orientation as SwingConstants.VERTICAL or SwingConstants.HORIZONTAL
	 * @param min the minimumm value 
	 * @param max the maximum value
	 * @param stringPainted true for percent string painted
	 * @param initialValue >= min & <= max
	 * @param width
	 * @param heigh
	 */
	public void COMPONENT(JProgressBar jprogressBar, String controlName, int row, int orientation, int min, int max, boolean stringPainted, int initialValue, int width, int heigh) {
	   this.activeMenuName = "";
 	   jprogressBarCommon(jprogressBar, controlName, row, 0, 1, 1, orientation, min, max, stringPainted, initialValue, width, heigh);
	}
	
	/*
	 * Common code per oggetto JProgressBar
	 */
	private void jprogressBarCommon(JProgressBar jprogressBar, String controlName, int row, int col, int cellWidth, int cellHeight, int orientation, int min, int max, boolean stringPainted, int initialValue, int width, int heigh) {
		InnerComponent innerComponent = null;
		ForwardPanelComponent panelComponent = null;
		
		this.setComponentPropertiesDefault(JSpinner.class, EnumForwardComponent.JProgressBar, jprogressBar);
	    
	    jprogressBar.setOrientation(orientation);
	    jprogressBar.setMaximum(min);
	    jprogressBar.setMaximum(max);
	    jprogressBar.setValue(initialValue);
		jprogressBar.setStringPainted(stringPainted);
		jprogressBar.setPreferredSize(new Dimension((width == -1) ? jprogressBar.getWidth() : width, (heigh == -1) ? jprogressBar.getHeight() : heigh));
	     
        // Assegnazione nome e inserimento in strutture interne
        jprogressBar.setName(controlName);
		panelComponent = controlPut(controlName, jprogressBar, EnumForwardComponent.JProgressBar, row, col, cellWidth, cellHeight);
		panelComponent.setDefaultValue(initialValue);

		// Classe tipo oggetto di rappresentazione
 	    innerComponent = this.getComponentDeclared(controlName);
 	    innerComponent.componentObjectClass = Integer.class;
}



	/**
	 * <h3>Panel JList control declaration</h3>
	 * 
	 * This is the constructor for a SINGLE_SELECTION list with java default  preferred row count showed<br>
	 * A {@link ForwardListModel} that inherits from standard {@link DefaultListModel}, is generated and set for<br>
	 * the list and it will be used for any operations and to store extra application data object for each item too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * 
	 * @param list object
	 * @param controlName
	 * @param row
	 * @param col
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JList jlist, String controlName, int row, int col) {
		this.activeMenuName = "";
		jlistCommon(jlist, controlName, row, col, 1, 1, ListSelectionModel.SINGLE_SELECTION, 8, -1, -1, false);
	}

	/**
	 * <h3>Panel JList control declaration</h3>
	 * 
	 * This is the constructor for a SINGLE_SELECTION list with java default  preferred row count showed<br>
	 * A {@link ForwardListModel} that inherits from standard {@link DefaultListModel}, is generated and set for<br>
	 * the list and it will be used for any operations and to store extra application data object for each item too.<br>
	 * Then the number of columns lets to assign the dimension of the formatted text field of the spinner.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the tree is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * 
	 * 
	 * @param list object
	 * @param controlName
	 * @param row
	 * @param width
	 * @param heigh
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JList jlist, String controlName, int row, int width, int heigh, boolean maximumSizeToSet) {
		this.activeMenuName = "";
		jlistCommon(jlist, controlName, row, 0, 1, 1, ListSelectionModel.SINGLE_SELECTION, 8, -1, -1, maximumSizeToSet);
	}

	
	/**
	 * <h3>Panel JList control declaration</h3>
	 * 
	 * This is the constructor for a list with selection type and preferred row count showed<br>
	 * Selection type can be ListSelectionModel.SINGLE_SELECTION, ListSelectionModel.SINGLE_INTERVAL_SELECTION <br>
	 * or ListSelectionModel.MULTIPLE_INTERVAL_SELECTION.<br>
	 * A {@link ForwardListModel} that inherits from standard {@link DefaultListModel}, is generated and set for<br>
	 * the list and it will be used for any operations and to store extra application data object for each item too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the tree is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * 
	 * @param jlist object
	 * @param controlName
	 * @param row
	 * @param selectionType as ListSelectionModel.SINGLE_SELECTION or ListSelectionModel.SINGLE_INTERVAL_SELECTION 
	 * @param visibleRowCount
	 * @param width
	 * @param heigh
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(JList jlist, String controlName, int row, int selectionType, int visibleRowCount, int width, int heigh, boolean maximumSizeToSet) {
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		jlistCommon(jlist, controlName, row, 0, 1, 1, selectionType, visibleRowCount, width, heigh, maximumSizeToSet);

	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.componentObjectClass = String.class;

	}

	

	/*
	 * Common code per oggetto JList
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void jlistCommon(JList jlist, String controlName, int row, int col, int cellWidth, int cellHeight, int selectionType, int visibleRowCount, int width, int heigh, boolean maximumSizeToSet) {

		JScrollPane jScrollPane = null;
		ForwardPanelComponent panelComponent = null;
		ForwardListModel listModel = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JList.class, EnumForwardComponent.JList, jlist);
        listModel = new ForwardListModel();
        jlist.setModel(listModel);
        jlist.setSelectionMode(selectionType);
        jlist.setVisibleRowCount(visibleRowCount);
        jlist.setSelectedIndex(0);
       
        // Assegnazione nome e inserimento in strutture interne
        jlist.setName(controlName);
		controlPut(controlName, jlist, EnumForwardComponent.JList, row, col, cellWidth, cellHeight);
		
		// Generazione store scroll pane.
		// Il monitor lo utilizzerà per inserire il controllo.
		panelComponent = this.getPanelComponent(controlName);
 	  	jScrollPane = new JScrollPane(jlist, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	  	jScrollPane.setPreferredSize(new Dimension((width == -1) ? jScrollPane.getWidth() : width, (heigh == -1) ? jScrollPane.getHeight() : heigh));
	  	if (maximumSizeToSet) {
		  	jScrollPane.setMaximumSize(new Dimension((width == -1) ? jScrollPane.getWidth() : width, (heigh == -1) ? jScrollPane.getHeight() : heigh));
		}
  	  	panelComponent.setScrollPane(jScrollPane);
 	  	panelComponent.setListModel(listModel);

 	  	// Update descrittore componenti con oggetto ScrollPane
 	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.jscrollPane = jScrollPane;
	}



	/**
	 * <h3>Panel JTable control declaration</h3>
	 * 
	 * This is the constructor for a table with specification of selection mode, dimension and if rows and columns are allowed<br>
	 * Selection mode must be specified as:
	 * A {@link ForwardTableModel} that inherits from standard {@link AbstractTableModel}, is generated and set for<br>
	 * the table and it will be used for any operations and to store extra application data object for each row too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the table is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value can avoid an undesiderable and unexpected<br>
	 * table expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * @param jtable object
	 * @param controlName
	 * @param row
	 * @param widthViewport of the table viewport in pixel
	 * @param heightViewport of the table viewport in pixel
	 * @param selectionMode as a ListSelectionModel constant <code>MULTIPLE_INTERVAL_SELECTION SINGLE_INTERVAL_SELECTION SINGLE_SELECTION</code>
	 * @param rowSelectionAllowed
	 * @param columnSelectionAllowed
	 * @param fillsViewportHeight
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param autoResizeMode as a JTable constant like:<pre>
	 * AUTO_RESIZE_OFF
	 * AUTO_RESIZE_NEXT_COLUMN
	 * AUTO_RESIZE_SUBSEQUENT_COLUMNS
	 * AUTO_RESIZE_LAST_COLUMN
	 * AUTO_RESIZE_ALL_COLUMNS</pre>
	 */
	public void COMPONENT(JTable jtable, String controlName, int row, int widthViewport, int heightViewport, int selectionMode, boolean rowSelectionAllowed, boolean columnSelectionAllowed, boolean fillsViewportHeight, boolean maximumSizeToSet, int autoResizeMode) {
		this.activeMenuName = "";
		jtableCommon(jtable
					, controlName
					, row
					, 0
					, 1
					, 1
					, selectionMode
					, widthViewport
					, heightViewport
					, rowSelectionAllowed
					, columnSelectionAllowed
					, fillsViewportHeight
					, widthViewport
					, heightViewport
					, maximumSizeToSet
					, autoResizeMode
					);
	}


	/**
	 * <h3>Panel JTable control declaration</h3>
	 * 
	 * This is the constructor for a table with specification of selection mode, dimension and if rows and columns are allowed<br>
	 * The fillsViewportHeight is set to true.<br>
	 * Selection mode must be specified as:
	 * A {@link ForwardTableModel} that inherits from standard {@link AbstractTableModel}, is generated and set for<br>
	 * the table and it will be used for any operations and to store extra application data object for each row too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the table is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value can avoid an undesiderable and unexpected<br>
	 * table expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * @param jtable object
	 * @param controlName
	 * @param row
	 * @param widthViewport of the table viewport in pixel
	 * @param heightViewport of the table viewport in pixel
	 * @param selectionMode as a ListSelectionModel constant <code>MULTIPLE_INTERVAL_SELECTION SINGLE_INTERVAL_SELECTION SINGLE_SELECTION</code>
	 * @param rowSelectionAllowed
	 * @param columnSelectionAllowed
	 * @param fillsViewportHeight
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param autoResizeMode as a JTable constant like:<pre>
	 * AUTO_RESIZE_OFF
	 * AUTO_RESIZE_NEXT_COLUMN
	 * AUTO_RESIZE_SUBSEQUENT_COLUMNS
	 * AUTO_RESIZE_LAST_COLUMN
	 * AUTO_RESIZE_ALL_COLUMNS</pre>
	 */
	public void COMPONENT(JTable jtable, String controlName, int row, int width, int height, int widthViewport, int heightViewport, int selectionMode, boolean rowSelectionAllowed, boolean columnSelectionAllowed, boolean maximumSizeToSet, int autoResizeMode) {
		this.activeMenuName = "";
		jtableCommon(jtable
					, controlName
					, row
					, 0
					, 1
					, 1
					, selectionMode
					, widthViewport
					, heightViewport
					, rowSelectionAllowed
					, columnSelectionAllowed
					, true
					, widthViewport
					, heightViewport
					, maximumSizeToSet
					, autoResizeMode
					);
	}


	/**
	 * <h3>Panel JTable control declaration</h3>
	 * 
	 * This is the constructor for a table with SINGLE_SELECTION, rows selection allowed and columns selection not allowed.<br>
	 * The fillsViewportHeight is set to true.<br>
	 * A {@link ForwardTableModel} that inherits from standard {@link AbstractTableModel}, is generated and set for<br>
	 * the table and it will be used for any operations and to store extra application data object for each item too.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the tree is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * 
	 * @param jtable the swing object
	 * @param controlName the name assigned
	 * @param row the row number in the panel, 0-based
	 * @param widthViewport of the table viewport in pixel
	 * @param heightViewport of the table viewport in pixel
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param autoResizeMode as a JTable constant like:<pre>
	 * AUTO_RESIZE_OFF
	 * AUTO_RESIZE_NEXT_COLUMN
	 * AUTO_RESIZE_SUBSEQUENT_COLUMNS
	 * AUTO_RESIZE_LAST_COLUMN
	 * AUTO_RESIZE_ALL_COLUMNS</pre>
	 *                                           
	 */
	public void COMPONENT(JTable jtable, String controlName, int row, int widthViewport, int heightViewport, boolean maximumSizeToSet, int autoResizeMode) {

		this.activeMenuName = "";
		
		jtableCommon(jtable
				, controlName
				, row
				, 0
				, 1
				, 1
				, ListSelectionModel.SINGLE_SELECTION
				, widthViewport
				, heightViewport
				, true				// rowSelectionAllowed
				, false				// columnSelectionAllowed
				, true				// fillsViewportHeight
				, widthViewport
				, heightViewport
				, maximumSizeToSet
				, autoResizeMode
				);
	}


	/*
	 * Common code per oggetto JTable
	 * width e height sono le dimensioni della tabella assegnate al JScrollPane che la contiene
	 * widthViewport e heightViewport sono le dimensione della vista scrollabile della tabella, che sono considerate uguali alle precedemti
	 */
	private void jtableCommon(JTable jtable
							, String controlName
							, int row
							, int col
							, int cellWidth
							, int cellHeight
							, int selectionMode
							, int width
							, int height
							, boolean rowSelectionAllowed
							, boolean columnSelectionAllowed
							, boolean fillsViewportHeight
							, int widthViewport
							, int heightViewport
							, boolean maximumSizeToSet
							, int autoResizeMode
							) {

		ForwardPanelComponent panelComponent = null;
 	  	JScrollPane jScrollPane = null;
		InnerComponent innerComponent = null;

		// Nuovo oggetto tabella, con metodo in override per gestire tooltip di cella.
        // Il metodo effettivo di gestione tooltip è gestito nel modello forward della tabella
 		jtable = new JTable(tableModel) {private static final long serialVersionUID = 1L;
 		                                 public String getToolTipText(MouseEvent e) {return tableModel.getToolTipText(e); }
 		                                };

		// Modello tabella, valorizzato con declaration DATA_VALUES() e assegnato a JTable in esecuzione, al completamento dei controlli
	  	this.tableModel = new ForwardTableModel(jtable);					

        // Caricamento 
        jtable.setSelectionMode(selectionMode);
        jtable.setPreferredScrollableViewportSize(new Dimension(widthViewport, heightViewport));
        jtable.setFillsViewportHeight(fillsViewportHeight);			// Forza o meno il riempimento di tutta l'area di scroll
    	jtable.setRowSelectionAllowed(rowSelectionAllowed);
    	jtable.setColumnSelectionAllowed(columnSelectionAllowed);
     	jtable.setAutoResizeMode(autoResizeMode);
    	
        // Assegnazione nome e inserimento in strutture interne 
        jtable.setName(controlName);
		controlPut(controlName, jtable, EnumForwardComponent.JTable, row, col, cellWidth, cellHeight);
		
		// Generazione store scroll pane.
		// Il monitor lo utilizzerà per inserire il controllo.
		panelComponent = this.getPanelComponent(controlName);
 	  	jScrollPane = new JScrollPane(jtable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
 	  	jScrollPane.setPreferredSize(new Dimension(width, height));
 	  	if (maximumSizeToSet) {
 	  		jScrollPane.setMaximumSize(new Dimension(width, height));
		}
 	  	panelComponent.setScrollPane(jScrollPane);
 	  	panelComponent.setTableModel(tableModel);
 	  	
 	  	// Update descrittore componenti con oggetto ScrollPane
 	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.jscrollPane = jScrollPane;
 	  	
	}

	
	/**
	 * <h3>Panel JTree control declaration</h3>
	 * 
	 * This is the constructor for a jtree with full parameters allowed.<br>
	 * A {@link ForwardTreeModel} that inherits from standard java {@link DefaultTreeModel}, is generated and set for<br>
	 * the tree and it will be used for any operations and to store extra application data object for each item too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update any swing control parameter.<br>
	 * <p>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the tree is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * <p>
	 * 
	 * @param jtree the swing object
	 * @param controlName the name assigned
	 * @param row the row number in the panel, 0-based
	 * @param width of the tree in pixel
	 * @param height of the tree in pixel
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param rootText  
	 * @param selectionMode as a TreeSelectionModel constant like SINGLE_TREE_SELECTION, CONTIGUOUS_TREE_SELECTION, DISCONTIGUOUS_TREE_SELECTION
	 * @param editable true to make any node text editable
	 * @param showsRootHandles true to show the root in extended mode
	 * @param pathOpenIcon the path of the open node icon
	 * @param pathClosedIcon the path of the closed node icon
	 * @param pathLeafIcon the path of the leaf node icon
	 * @param initialExpandMode as a ForwardTreeModel constants <pre>
		INITIAL_EXPAND_NONE
		INITIAL_EXPAND_ROOT
		INITIAL_EXPAND_ALL<pre>
	 */
	public void COMPONENT(JTree jtree, String controlName, int row, int width, int height, boolean maximumSizeToSet, String rootText, int selectionMode, boolean editable, boolean showsRootHandles, String pathOpenIcon, String pathClosedIcon, String pathLeafIcon, int initialExpandMode) {

		this.activeMenuName = "";
		
		jtreeCommon(jtree
				, controlName
				, row
				, 0
				, 1
				, 1
				, width
				, height
				, maximumSizeToSet
				, selectionMode
				, initialExpandMode
				, editable
				, showsRootHandles		    
				, pathOpenIcon				 
				, pathClosedIcon
				, pathLeafIcon
				, rootText
				);
	}

	/**
	 * <h3>Panel JTree control declaration</h3>
	 * 
	 * This is the constructor for a jtree with following properties:<pre>
	 * SINGLE_TREE_SELECTION
	 * INITIAL_EXPAND_MODE_ROOT 
	 * editable = false
	 * showsRootHandles = true
	 * Standard open/closed/leaf icon</pre>
	 * A {@link ForwardTreeModel} that inherits from standard {@link DefaultTreeModel}, is generated and set for<br>
	 * the tree and it will be used for any operations and to store extra application data object for each item too.<br>
	 * <code>TUNING_LAYOUT</code> section can get and update the specific constraint and any swing control parameter.<br>
	 * <p>
	 * Notice that it needs to set <code>maximumSizeToSet</code> parameter to true, when the tree is layed out on a panel where<br>
	 * a <code>BOX_LAYOUT</code> layout manager is used. In this case a true value avoid an undesiderable and unexpected<br>
	 * tree expansion in width and heigh. For any other layout manager this doesn't happen.<br>
	 * This is important as normally to lay out controls in detail panels is used <code>BOX_LAYOUT</code> layout manager.<br>
	 * 
	 * @param jtree the swing object
	 * @param controlName the name assigned
	 * @param row the row number in the panel, 0-based
	 * @param width of the scrollpane to be used to show the tree in pixel
	 * @param height of the scrollpane to be used to show the tree in pixel
	 * @param maximumSizeToSet true to set <code>maximumSize</code> property to width and height set for <code>preferredSize</code>
	 * @param rootText the string root text
	 */
	public void COMPONENT(JTree jtree, String controlName, int row, int width, int height, boolean maximumSizeToSet, String rootText) {

		this.activeMenuName = "";
		
		jtreeCommon(jtree
				, controlName
				, row
				, 0
				, 1
				, 1
				, width
				, height
				, maximumSizeToSet
				, TreeSelectionModel.SINGLE_TREE_SELECTION
				, ForwardTreeModel.INITIAL_EXPAND_ROOT
				, false
				, true		    
				, ""				 
				, ""
				, ""
				, rootText
				);
	}



	/*
	 * Common code per oggetto JTree
	 */
	private void jtreeCommon( JTree jtree
							, String controlName
							, int row
							, int col
							, int cellWidth
							, int cellHeight
							, int width
							, int height
							, boolean maximumSizeToSet
							, int selectionMode
							, int initialExpandMode
							, boolean editable
							, boolean showsRootHandles
							, String pathOpenIcon
							, String pathClosedIcon
							, String pathLeafIcon
							, String rootText
							) {
		
		ForwardPanelComponent panelComponent = null;
 	  	JScrollPane jScrollPane = null;
		ForwardTreeModel treeModel = null;
		ForwardTreeUserObject rootNodeObject = null;
		DefaultMutableTreeNode rootNodeTree = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JTable.class, EnumForwardComponent.JTree, jtree);
		
		// Modello tabella, valorizzato con declaration DATA_VALUES()
		rootNodeObject = new ForwardTreeUserObject(rootText);
		rootNodeTree = new DefaultMutableTreeNode(rootNodeObject);
		rootNodeObject.setNodeJTree(rootNodeTree);
        treeModel = new ForwardTreeModel(jtree, rootNodeObject);
        treeModel._setInitialExpandMode(initialExpandMode);
        jtree.setModel(treeModel);
        
        // Caricamento proprietà tree, assegnazione nome e inserimento in strutture interne 
        jtree.getSelectionModel().setSelectionMode(selectionMode);
        jtree.setEditable(editable);
        jtree.setShowsRootHandles(showsRootHandles);
        jtree.setName(controlName);
		controlPut(controlName, jtree, EnumForwardComponent.JTree, row, col, cellWidth, cellHeight);
		
		// Generazione store scroll pane.
		// Il monitor lo utilizzerà per inserire il controllo.
		panelComponent = this.getPanelComponent(controlName);
 	  	jScrollPane = new JScrollPane(jtree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
 	  	jScrollPane.setPreferredSize(new Dimension(width, height));
        if (maximumSizeToSet) {
     	  	jScrollPane.setMaximumSize(new Dimension(width, height));
		}
  	  	panelComponent.setScrollPane(jScrollPane);
 	  	
		// Modello Tree, valorizzato con declaration DATA_VALUES() 
  	  	panelComponent.setTreeModel(treeModel);
 	  	
 	  	// Update descrittore componenti con oggetto ScrollPane
 	  	innerComponent = this.getComponentDeclared(controlName);
 	  	innerComponent.jscrollPane = jScrollPane;
	}

	/**
	 * <h3>JPanel control declaration</h3>
	 * 
	 * A JPanel Swing control will be laid out on the panel starting at row specified.<br>
	 * <p>
	 * This is a convenient method to declare a detail panel as a short cut declaration, directly inside another detail panel,<br>
	 * without defining previously it by PANELS_STRUCTURE() declaratives.<br>
	 * <p>
     * It there must be a BEGIN_PANEL_DETAIL_CONTENT() for the panel to lay out as a component and there.<br>
     * it must be specified the layout manager to use to lay out controls on the panel.<br>
     * <p>
     * The panel will be automatically resized by java depending on swing controls layed out on.<br>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * This is the method for minimal parameters specification.<br>
	 * <br>
	 * 
	 * @param jPanel the swing JPanel object
	 * @param panelName
	 * @param row
	 */
	public void COMPONENT(JPanel jPanel, String panelName, int row) {
		this.activeMenuName = "";
		jpanelCommon(jPanel, panelName, row, 0, 1, 1, -1, -1, false);
	}

	/**
	 * <h3>JPanel control declaration</h3>
	 * 
	 * A JPanel Swing control will be laid out on the panel starting at row and column specified.<br>
	 * <p>
	 * This is a convenient method to declare a detail panel as a short cut declaration, directly inside another detail panel,<br>
	 * without defining previously it by PANELS_STRUCTURE() declaratives.<br>
	 * <br>
	 * <p>
     * It there must be a BEGIN_PANEL_DETAIL_CONTENT() for the panel to lay out as a component and there.<br>
     * it must be specified the layout manager to use to lay out controls on the panel.<br>
     * <p>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * This is the method for full parameters specification.<br>
	 * <p>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * <p>
	 * This method allows you to set preferred width and height size and if the maximum size should be to the same value too.<br>
	 * <p>
	 * 
	 * @param jPanel the swing JPanel object
	 * @param panelName
	 * @param width
	 * @param height
	 * @param maximumSizeToSet
	 */
	public void COMPONENT(JPanel jPanel, String panelName, int row, int width, int height, boolean maximumSizeToSet) {

		this.activeMenuName = "";
		jpanelCommon(jPanel, panelName, row, 0, 1, 1, width, height, maximumSizeToSet);
	}

 

	/*
	 * Common code per oggetto JPanel
	 */
	private void jpanelCommon(JPanel jPanel
							, String panelName
							, int row
							, int col
							, int cellWidth
							, int cellHeight
							, int width
							, int height
							, boolean maximumSizeToSet
							 ) {
		
		ForwardPanel forwardPanel = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JPanel.class, EnumForwardComponent.JPanel, jPanel);
		
        // Assegnazione nome e inserimento in struttura interna generale dei componenti e in quella specifica dei panels
		jPanel.setName(panelName);
//		jPanel.setPreferredSize(new Dimension((width == -1) ? jPanel.getWidth() : width, (height == -1) ? jPanel.getHeight() : height));
        if (maximumSizeToSet) {
        	jPanel.setMaximumSize(new Dimension((width == -1) ? jPanel.getWidth() : width, (height== -1) ? jPanel.getHeight() : height));
		}
		controlPut(panelName, jPanel, EnumForwardComponent.JPanel, row, col, cellWidth, cellHeight);

		// Update valori specifici in descrittore panel
		// Descrittore panel esistente significa panel dichiarato precedentemente (inconsueto)
		innerComponent = getComponentDeclared(panelName);
		forwardPanel = getPanelDescriptor(panelName);
		if (forwardPanel != null) {
			forwardPanel.setGraphicObject(innerComponent.component);
			forwardPanel.setPopUpMenuName(innerComponent.popUpMenuName);
		} 
	}
	 
	/**
	 * <h3>Space separator control declaration</h3>
	 * <p>
	 * This constructor lets to declare a java transparent separator between controls and between rows when controls <br>
	 * are layed out using the BOX_LAYOUT layout manager.<br>
	 * The separator is a Box.createXyyy() object and can be a rigid area, as a rectangole, a structure vertical, horizontal<br>
	 * or, at the end, an object that expands itself both horizontal and vertical (glue).<br>
	 * For any detailed explaination about glue, struct and rigid area  as separators, please refers to standard Java documentation <br>
	 * about {@link BoxLayout}<br>
	 * <p>
	 * At declaring time, if the separator type is not correct, no action will be taken.<br>
	 * At execution time, if the panel is not managed by a BOX_LAYOUT layout manager, no action will be taken.<br>
	 * <p>
	 * To leave spaces between components it's better to use JBoxRigidArea separator to be sure to leave unmodified the thickness of others row controls.<br>
	 * <p>
	 * The input <code>glue</code> component must be an object obtained at the method call like, for example, valid gllue declaration are:<pre>
	 * COMPONENT(Box.class, Box.createHorizontalGlue(), "glue01", EnumForwardComponent.JBoxGlueHorizontal, 7, 0, 0); 		 
	 * COMPONENT(Box.class, Box.createHorizontalStrut(10), "glue01", EnumForwardComponent.JBoxStrutHorizontal, 7, 10, 0); 		 
	 * COMPONENT(Box.class, Box.createGlue(), "glue01", EnumForwardComponent.JBoxGlue, 7, 3, 0, 0); 								 
	 * COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), "glue01", EnumForwardComponent.JBoxRigidArea, 7, 10, 20);  
	 * COMPONENT(Box.class, Box.createVerticalGlue(), "glue02", EnumForwardComponent.JBoxGlueVertical, 8, 0, 0);       			 
	 * COMPONENT(Box.class, Box.createVerticalStrut(10), "glue02", EnumForwardComponent.JBoxStrutVertical, 8, 0, 10);       		 
	 * COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), "glue02", EnumForwardComponent.JBoxRigidArea, 8, 10, 20); 
	 * <pre>
	 * <code>Note. When JBoxStrutHorizontal, JBoxStrutVertical or JBoxRigidArea are used, widht and heigh will be those in input
	 * method and not those of the object Box.<br>
	 * </code>
	 * @param boxClass the method marker as an object class that must be Box.class
	 * @param Component the Box.CreateX... component
	 * @param controlName the name assigned
	 * @param glueType must be EnumForwardComponent element among JBoxGlue | JBoxGlueHorizontal | JBoxGlueVertical
	 * @param row the row number in the panel, 0-based
	 * @param width the BoxRigidArea width or the size of the BoxStrutHorizontal or 0 if not significative
	 * @param heigh the BoxRigidArea heigh or the size of the BoxStrutVertical or 0 if not significative
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(Class boxClass, Component glue, String controlName, EnumForwardComponent glueType, int row, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		this.activeMenuName = ""; 
		panelComponent = controlPut(controlName, glue, glueType, row, 0, 1, 1);  // Assegnazione nome e inserimento in strutture interne
		panelComponent.setFillerHorizontal(width);
		panelComponent.setFillerVertical(heigh);
	}

	/**
	 * <h3>Space separator control declaration</h3>
	 * <p>
	 * This constructor lets to declare a java transparent separator between controls and between rows when controls <br>
	 * are layed out using the BOX_LAYOUT layout manager.<br>
	 * The separator is a Box.createXyyy() object and can be a rigid area, as a rectangole, a structure vertical, horizontal<br>
	 * or, at the end, an object that expands itself both horizontal and vertical (glue).<br>
	 * For any detailed explaination about glue, struct and rigid area  as separators, please refers to standard Java documentation <br>
	 * about {@link BoxLayout}<br>
	 * <p>
	 * At declaring time, if the separator type is not correct, no action will be taken.<br>
	 * At execution time, if the panel is not managed by a BOX_LAYOUT layout manager, no action will be taken.<br>
	 * <p>
	 * To leave spaces between components it's better to use JBoxRigidArea separator to be sure to leave unmodified the thickness of others row controls.<br>
	 * <p>
	 * The input <code>glue</code> component must be an object obtained at the method call like, for example, valid gllue declaration are:<pre>
	 * COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 7, 0, 0); 		 
	 * COMPONENT(Box.class, Box.createHorizontalStrut(10), EnumForwardComponent.JBoxStrutHorizontal, 7, 10, 0); 		 
	 * COMPONENT(Box.class, Box.createGlue(), EnumForwardComponent.JBoxGlue, 7, 3, 0, 0); 								 
	 * COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), EnumForwardComponent.JBoxRigidArea, 7, 10, 20);  
	 * COMPONENT(Box.class, Box.createVerticalGlue(),  EnumForwardComponent.JBoxGlueVertical, 8, 0, 0);       			 
	 * COMPONENT(Box.class, Box.createVerticalStrut(10), EnumForwardComponent.JBoxStrutVertical, 8, 0, 10);       		 
	 * COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), EnumForwardComponent.JBoxRigidArea, 8, 10, 20); 
	 * <pre>
	 * <code>Note. When JBoxStrutHorizontal, JBoxStrutVertical or JBoxRigidArea are used, widht and heigh will be those in input
	 * method and not those of the object Box.<br>
	 * </code>
	 * @param boxClass the method marker as an object class that must be Box.class
	 * @param Component the Box.CreateX... component
	 * @param glueType must be EnumForwardComponent element among JBoxGlue | JBoxGlueHorizontal | JBoxGlueVertical
	 * @param row the row number in the panel, 0-based
	 * @param width the BoxRigidArea width or the size of the BoxStrutHorizontal or 0 if not significative
	 * @param heigh the BoxRigidArea heigh or the size of the BoxStrutVertical or 0 if not significative
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT(Class boxClass, Component glue, EnumForwardComponent glueType, int row, int width, int heigh) {
		ForwardPanelComponent panelComponent = null;
		String controlName = "";
		
		this.activeMenuName = ""; 
		controlName = "#AUTOGLUE" + this.numProgrGlueField;
		this.numProgrGlueField++;
		panelComponent = controlPut(controlName, glue, glueType, row, 0, 1, 1);  // Assegnazione nome e inserimento in strutture interne
		panelComponent.setFillerHorizontal(width);
		panelComponent.setFillerVertical(heigh);
	}

	

	
	/**
	 * <h3>Timer component declaration</h3>
	 * 
	 * This is the constructor for a Swing timer component definition, a semplified type of timer.<br>
	 * It's possible specify just few parameters like, delay time, initial delay time and repeatible <br>
	 * <p>
	 * 
	 * @param timer the Timer object
	 * @param componentName the name assigned to object timer
	 * @param delay the delay time in ms
	 * @param initialDelay the initial delay in ms
	 * @param repeats true if timer starts again at the end
	 */
	public void COMPONENT(Timer timer, String componentName, int delay, int initialDelay, boolean repeats) {

		this.setComponentPropertiesDefault(Timer.class, EnumForwardComponent.TimerSwing, timer);
		
        // Caricamento valori
        timer.setInitialDelay(initialDelay);
        timer.setDelay(delay);
        timer.setRepeats(repeats);
        
        // Assegnazione nome e inserimento in strutture interne 
		putComponentOnStructure(componentName, EnumForwardComponent.TimerSwing, timer);
	}


	
	/* --------------------------------------------------
	 * Inserimento componente Swing in strutture funzione
	 * --------------------------------------------------
	 * 
	 */
	private ForwardPanelComponent controlPut(String controlName, Component jcomponent, EnumForwardComponent componentType, int row, int col, int cellWidth, int cellHeight) {
		ForwardPanelComponent panelComponent = null;
		
		// Si sta dichiarando un pannello di dettaglio
		if (this.isActiveDeclarationPanel) {
			
			// Componente in panel corrente
			panelComponent = new ForwardPanelComponent(controlName);
			this.activePanel.addPanelField(panelComponent);
			panelComponent.setGraphicObject(jcomponent);
			panelComponent.setType(componentType);
			
			// Logic row and column
			panelComponent.setRow(row);
			panelComponent.setCol(col);
			
			// Sets the constraint default
			panelComponent.getConstraints().gridx = col;
			panelComponent.getConstraints().gridy = row;
			panelComponent.getConstraints().gridwidth = cellWidth;
			panelComponent.getConstraints().gridheight = cellHeight;
			panelComponent.getConstraints().anchor = GridBagConstraints.WEST;
			panelComponent.getConstraints().fill = GridBagConstraints.NONE;
			panelComponent.getConstraints().ipadx = 0;
			panelComponent.getConstraints().ipady = 0;
			panelComponent.getConstraints().insets = new Insets(0,0,0,0);
		}

		// Componente java in Map, per tutti i componenti
		putComponentOnStructure(controlName, componentType, jcomponent);

		return panelComponent; 
	}

	/**
	 * <h3>Border declaration with full options</h3>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param EnumForwardBorderType borderType
	 * @param borderTitle
	 * @param titleJustification as TitledBorder.constant
	 * @param titlePosition as TitledBorder.constant
	 * @param titleColor as a Color object for the line title
	 * @param titleFont as Font object for line title
	 * @param lineColor as a Color object for the border line
	 * @param lineThickness as the thickness of border line
	 * @param rounded as true to have rounded corners
	 * @param insetTop an integer specifying the width of the top, in pixels
	 * @param insetLeft an integer specifying the width of the left, in pixels
	 * @param insetBottom an integer specifying the width of the bottom, in pixels
	 * @param insetRight an integer specifying the width of the right, in pixels
	 */
	public void BORDER(String componentlName
					, EnumForwardBorderType borderType
					, String borderTitle
					, int titleJustification
					, int titlePosition
					, Color titleColor
					, Font titleFont
					, Color lineColor
					, int lineThickness
					, boolean rounded
					, int insetTop
					, int insetLeft
					, int insetBotton
					, int insetRight)      {
 
		borderCommon(componentlName
	                , borderType
	                , borderTitle
	                , titleJustification
	                , titlePosition
	                , titleColor                 // titleColor
	                , titleFont                  // titleFont
	                , lineColor                  // lineColor					// Per bordi line
	                , null                       // highlightColor				// Per bordi etched e lowered
	                , null                       // shadowColor					// Per bordi etched e lowered
	                , lineThickness 			 // Spessore livea bordo
	                , rounded					 // Angoli arrotondati
	                , insetTop						
	                , insetLeft
	                , insetBotton
	                , insetRight);
	}
    
	
	/**
	 * <h3>Border declaration</h3>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param EnumForwardBorderType borderType
	 * @param borderTitle
	 * @param titleJustification as TitledBorder.constant
	 * @param titlePosition as TitledBorder.constant
	 * @param thickness as the thickness of border line
	 * @param rounded as true to have rounded corners
	 * @param insetTop an integer specifying the width of the top, in pixels
	 * @param insetLeft an integer specifying the width of the left, in pixels
	 * @param insetBottom an integer specifying the width of the bottom, in pixels
	 * @param insetRight an integer specifying the width of the right, in pixels
	 */
	public void BORDER(String componentlName, EnumForwardBorderType borderType, String borderTitle, int titleJustification, int titlePosition, int thickness, boolean rounded, int insetTop, int insetLeft, int insetBotton, int insetRight)      {
 
		borderCommon(componentlName
	                , borderType
	                , borderTitle
	                , titleJustification
	                , titlePosition
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null                       // highlightColor				// Per bordi etched e lowered
	                , null                       // shadowColor					// Per bordi etched e lowered
	                , thickness 			 	// Spessore livea bordo
	                , rounded					 // Angoli arrotondati
	                , insetTop						
	                , insetLeft
	                , insetBotton
	                , insetRight);
	}
    
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for an empty border.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param insetTop an integer specifying the width of the top, in pixels
	 * @param insetLeft an integer specifying the width of the left, in pixels
	 * @param insetBottom an integer specifying the width of the bottom, in pixels
	 * @param insetRight an integer specifying the width of the right, in pixels
	 */
	public void BORDER(String componentlName, int insetTop, int insetLeft, int insetBotton, int insetRight)      {
 
		borderCommon(componentlName
	                , EnumForwardBorderType.EMPTY
	                , ""
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null                       // highlightColor				// Per bordi etched e lowered
	                , null                       // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , false					     // Angoli arrotondati
	                , insetTop						
	                , insetLeft
	                , insetBotton
	                , insetRight);
	}
    
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a line border.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param lineColor 
	 * @param thicknessLine an integer specifying the line tickness, in pixels
	 * @param rounded a boolean specifying corners rounded or not
	 */
	public void BORDER(String componentlName, Color lineColor, int thicknessLine, boolean rounded)      {
 
		borderCommon(componentlName
	                , EnumForwardBorderType.LINE
	                , ""
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , lineColor                  // lineColor					// Per bordi line
	                , null                       // highlightColor				// Per bordi etched e lowered
	                , null                       // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , false					     // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a lowered etched or raised etched border.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param borderType must be  EnumForwardBorderType.LOWERED_ETCHED or EnumForwardBorderType.RAISED_ETCHED
	 * @param highlightColor the hilight color or null for default
	 * @param shadowColor the shadow color or null for default
	 */
	public void BORDER(String componentlName, EnumForwardBorderType borderType, Color highlightColor, Color shadowColor)      {
 
		borderCommon(componentlName
	                , EnumForwardBorderType.LINE
	                , ""
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , highlightColor             // highlightColor				// Per bordi etched e lowered
	                , shadowColor                // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , false					     // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a lowered bevel, raised bevel, raised lowered or compound border.<br>
	 * A compound border is made by two bevel border working together, a lowered and a raised.
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param borderType must be  EnumForwardBorderType.LOWERED_BEVEL|RAISED_BEVEL|COMPOUND|RAISED_LOWERED
	 * @param highlightColor the hilight color or null for default
	 * @param shadowColor the shadow color or null for default
	 */
	public void BORDER(String componentlName, EnumForwardBorderType borderType)      {
 
		borderCommon(componentlName
	                , borderType
	                , ""
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , false					     // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
 
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a titled border with all defaults.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentlName
	 * @param borderTitle 
	 */
	public void BORDER(String componentlName, String borderTitle)      {
 
		borderCommon(componentlName
	                , EnumForwardBorderType.TITLED_DEFAULT
	                , borderTitle
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , false					     // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    
	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a titled line border with full options.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentName
	 * @param borderTitle 
	 * @param titleJustification as a TitleBorder constant LEFT, RIGHT, CENTER, DEFAULT_JUSTIFICATION
	 * @param titlePosition as a TitleBorder constant ABOVE_TOP, TOP, BELOW_TOP, ABOVE_BOTTOM, NELOW_BOTTOM, DEFAULT_POSITION
	 * @param titleColor 
	 * @param titleFont 
	 * @param lineColor 
	 * @param thicknessLine in pixel 
	 * @param rounded as true or false
	 */
	public void BORDER(String componentName, String borderTitle, int titleJustification, int titlePosition, Color titleColor, Font titleFont, Color lineColor, int thicknessLine, boolean rounded)      {
 
		borderCommon(componentName
	                , EnumForwardBorderType.TITLED_LINE
	                , borderTitle
	                , titleJustification
	                , titlePosition
	                , titleColor                 // titleColor
	                , titleFont                  // titleFont
	                , lineColor                  // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , thicknessLine				 // Spessore linea bordo
	                , rounded					 // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    

	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a titled line border with black line 2 pixel thickness with rounded corners<br>
	 * default positioned and justified<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentName
	 * @param borderTitle 
	 * @param rounded as true for roounded corners or false
	 */
	public void BORDER(String componentName, String borderTitle, boolean rounded)      {
 
		borderCommon(componentName
	                , EnumForwardBorderType.TITLED_LINE
	                , borderTitle
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                       // titleColor
	                , null                       // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , 2				 			 // Spessore linea bordo
	                , rounded					 // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    

	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a titled lowered border etched or lowered with full options.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentName
	 * @param borderType must be  EnumForwardBorderType.TITLED_LOWERED_ETCHED|TITLED_LOWERED_BEVEL
	 * @param borderTitle 
	 * @param titleJustification as a TitleBorder constant LEFT, RIGHT, CENTER, DEFAULT_JUSTIFICATION
	 * @param titlePosition as a TitleBorder constant ABOVE_TOP, TOP, BELOW_TOP, ABOVE_BOTTOM, NELOW_BOTTOM, DEFAULT_POSITION
	 * @param titleColor 
	 * @param titleFont 
	 */
	public void BORDER(String componentName, EnumForwardBorderType borderType, String borderTitle, int titleJustification, int titlePosition, Color titleColor, Font titleFont)      {
 
		borderCommon(componentName
	                , borderType
	                , borderTitle
	                , titleJustification
	                , titlePosition
	                , titleColor                 // titleColor
	                , titleFont                  // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , 0				 			 // Spessore linea bordo
	                , true					 	 // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    

	/**
	 * <h3>Border declaration</h3>
	 * <br>
	 * Constructor for a titled lowered border etched or lowered with default values.<br>
	 * <p>
	 * Declare a specific border for a component with the name specified<br>
	 * The component can be a panel or any control inside a panel.<br>
	 * <p>
	 * @param componentName
	 * @param borderType must be  EnumForwardBorderType.TITLED_LOWERED_ETCHED|TITLED_LOWERED_BEVEL
	 * @param borderTitle 
	 * @param titleJustification as a TitleBorder constant LEFT, RIGHT, CENTER, DEFAULT_JUSTIFICATION
	 * @param titlePosition as a TitleBorder constant ABOVE_TOP, TOP, BELOW_TOP, ABOVE_BOTTOM, NELOW_BOTTOM, DEFAULT_POSITION
	 * @param titleColor 
	 * @param titleFont 
	 */
	public void BORDER(String componentName, EnumForwardBorderType borderType, String borderTitle)      {
 
		borderCommon(componentName
	                , borderType
	                , borderTitle
	                , TitledBorder.DEFAULT_JUSTIFICATION
	                , TitledBorder.DEFAULT_POSITION
	                , null                  	 // titleColor
	                , null                  	 // titleFont
	                , null                       // lineColor					// Per bordi line
	                , null             			 // highlightColor				// Per bordi etched e lowered
	                , null	               		 // shadowColor					// Per bordi etched e lowered
	                , 0				 			 // Spessore linea bordo
	                , true					 	 // Angoli arrotondati
	                , 0						
	                , 0
	                , 0
	                , 0);
	}
    

	/*
	 * Metodo common per BORDER()
	 */
	private void borderCommon(String componentName
			                , EnumForwardBorderType borderType
			                , String borderTitle
			                , int titleJustification
			                , int titlePosition
			                , Color titleColor
			                , Font titleFont
			                , Color lineColor					// Per bordi line
			                , Color highlightColor				// Per bordi etched e lowered
			                , Color shadowColor					// Per bordi etched e lowered
			                , int thicknessLine					// Spessore livea bordo
			                , boolean rounded					// Angoli arrotondati
			                , int insetTop						
			                , int insetLeft
			                , int insetBotton
			                , int insetRight)      {
		 
		
		ForwardPanelComponent panelComponent = null;
		JComponent component = null;
		Border border = null;
		ForwardPanel panel = null;
		InnerComponent innerComponent = null;
        Border empty, titleLine, blackline, raisedetched, loweredetched, raisedbevel, loweredbevel;
        TitledBorder titledBorder;;
        
        this.activeMenuName = "";
        
        // Component dichiarata
        innerComponent = this.getComponentDeclared(componentName);
        if (innerComponent == null) {return;}
        
        // Impostazione JComponent, tipo bordo e titolo
        if (innerComponent.component instanceof JPanel) {
     		panel = hm_panel.get(componentName);					// Gets panel 
    		if (panel == null) {return;}
			panel.setBorderType(borderType);
			panel.setBorderTitle(borderTitle);
			component = (JPanel) innerComponent.component;
		} else if (innerComponent.component instanceof JComponent) {
	        panelComponent = this.getPanelComponent(componentName);
			panelComponent.setBorderType(borderType);
			panelComponent.setBorderTitle(borderTitle);
			component = (JComponent) innerComponent.component;
		}
        
		// Assign border to component object
		switch (borderType) {
				case EMPTY:
					empty = BorderFactory.createEmptyBorder(insetTop, insetLeft, insetBotton, insetRight);
					component.setBorder(empty);
					break;
				case LINE:
//		        	blackline = BorderFactory.createLineBorder((lineColor == null) ? Color.black : lineColor, thicknessLine, rounded); 
		        	blackline = BorderFactory.createLineBorder((lineColor == null) ? Color.black : lineColor, thicknessLine); 
					component.setBorder(blackline);
					break;
				case LOWERED_ETCHED:
					loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
					if (shadowColor != null && highlightColor != null) {
						loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED, highlightColor, shadowColor);
					}
					component.setBorder(loweredetched);
					break;
				case RAISED_ETCHED:
					raisedetched = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
					if (shadowColor != null && highlightColor != null) {
						raisedetched = BorderFactory.createEtchedBorder(EtchedBorder.RAISED, highlightColor, shadowColor);
					}
					component.setBorder(raisedetched);  
					break;
				case LOWERED_BEVEL:
					loweredbevel = BorderFactory.createLoweredBevelBorder();
					component.setBorder(loweredbevel);
					break;
				case RAISED_BEVEL:
					raisedbevel = BorderFactory.createRaisedBevelBorder(); 
					component.setBorder(raisedbevel);
					break;
				case RAISED_LOWERED:
					loweredbevel = BorderFactory.createLoweredBevelBorder();
					component.setBorder(loweredbevel);
					break;
				case MATTE:
					break;
				case COMPOUND:
					loweredbevel = BorderFactory.createLoweredBevelBorder();
					raisedbevel = BorderFactory.createRaisedBevelBorder();
					border = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel);
					component.setBorder(border);
					break;
				case TITLED_DEFAULT:
					titledBorder = BorderFactory.createTitledBorder(title);
					titledBorder.setTitleJustification(TitledBorder.DEFAULT_JUSTIFICATION); 
					titledBorder.setTitlePosition(TitledBorder.DEFAULT_POSITION);
					component.setBorder(titledBorder); 
					break;
				case TITLED_LINE:
//					titleLine = BorderFactory.createLineBorder((lineColor == null) ? Color.black : lineColor, thicknessLine, rounded);
					titleLine = BorderFactory.createLineBorder((lineColor == null) ? Color.black : lineColor, thicknessLine);
					titledBorder = BorderFactory.createTitledBorder(titleLine, borderTitle);
					if (titleColor != null) {titledBorder.setTitleColor(titleColor);}
					if (titleFont  != null) {titledBorder.setTitleFont(titleFont);}
					titledBorder.setTitleJustification(titleJustification);
					titledBorder.setTitlePosition(titlePosition);
					component.setBorder(titledBorder);
					break;
				case TITLED_LOWERED_ETCHED:
					loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED); 
					titledBorder = BorderFactory.createTitledBorder(loweredetched, borderTitle);
					if (titleColor != null) {titledBorder.setTitleColor(titleColor);}
					if (titleFont  != null) {titledBorder.setTitleFont(titleFont);}
					titledBorder.setTitleJustification(titleJustification);
					titledBorder.setTitlePosition(titlePosition);		
					component.setBorder(titledBorder);
					break;
				case TITLED_LOWERED_BEVEL:
					loweredbevel = BorderFactory.createLoweredBevelBorder();
					titledBorder = BorderFactory.createTitledBorder(loweredbevel, borderTitle);
					if (titleColor != null) {titledBorder.setTitleColor(titleColor);}
					if (titleFont  != null) {titledBorder.setTitleFont(titleFont);}
					titledBorder.setTitleJustification(titleJustification);
					titledBorder.setTitlePosition(titlePosition);		
					component.setBorder(titledBorder);
					break;
				default:
					break;
		}
		
	}

	
	/**
	 * <h4>Declare the source of data for a component</h3>
	 * Data source is not specified directly as a database table but as a forward logical data view. <br>
	 * A logical data view, often named <code>ldv</code> is a simple way to declare complex database access,<br>
	 * without Sql use.<br>
	 * <p>
	 * A logical data view returns a set of rows with and every column has the own name, object and entity table data base definition.<br>
	 * So the same java column name is assigned by default to every column of a logical data view.<br>
	 * Automatically the value got by the logical data view is assigned by forward monitor to all controls<br>
	 * with the same name, in every panel into wich they are declared.
	 * <p>
	 * A section <code>DATA_SOURCE</code> is declared for objects like:
	 * <br>
	 * <ul>
	 *  <li>PANEL</li>
	 *  The panel can be specialized to manage a {@link JTable} object, a {@link JList} or to be a plain detail panel.<br>
	 *  In any case, specifyng an ldv for the panel, the forward monitor is forced to exec it populating fields with<br>
	 *  the same name.
	 *  <li>COMBO</li>
	 *  ComboBox objects are like a normal field and so, through the logical data view of the panel, a value is initially set.<br>
	 *  But when the user requires to select a new value by a click, then it's necessary specify a logical data view to show<br>
	 *  the desired column and to get all necessary columns values.<br>
	 *  Returning back from the comboBox row selection, the forward monitor will store selected columns values in the monitor<br>
	 *  internal areas and will update controls in panels if it needs.<br>
	 *  <li>LIST</li>
	 *  Jlist objects are a set of rows  to be populated through the logical data view data.<br>
	 * </ul>
	 * <p>
	 * Notice that the columns specified are couples of names. The first is the column name as returned by logical data view<br>
	 * and the second is the name of a control declared, to let the automatic bound and setting by forward monitor.<br>
	 * If the second name is left to an empty string, is intended that the forward monitor will do the bound using the<br>
	 * same column name of the logical data view.<br>
	 * <p>
	 * Any section <code>DATA_SOURCE</code> can appear anywhere in the function declaring, even before the component declaration.<br>
	 * <p>
	 * @param componentName
	 * @param ldvName
	 * @param ar_columnBound as list of strings containing couples of JavaName and ldv columnBound
	 */
	public void DATA_SOURCE(String componentName, String ldvName, String ... ar_columnBound) {
		InnerComponent innerComponent = null;
		
		this.activeMenuName = "";
		
		// Lettura o creazione entry
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {
			innerComponent = new InnerComponent();
			innerComponent.componentName = componentName;
			this.hm_component.put(componentName, innerComponent);
		}
		
		// Inserimento ldv e append colonne
		innerComponent.ldvName = ldvName;
		innerComponent.al_columnBound.addAll(Arrays.asList(ar_columnBound));
		
	}
	
	/**
	 * <h3>List values declaration</h3>
	 * Are declared values that will be displayed by the list, tipically in the phase of Gui tuning,<br>
	 * as an array of objects depending on the control type.<br>
	 * <p>
	 * The <code>DATA_VALUES</code> declaration must be coded after the <code>CONTROL</code> declaration and hence it's necessary<br>
	 * specify only the the name assigned to the list<br>
	 * <p>
	 * If panel or panel field specified are not defined, no action will be executed.<br>
	 * <p>
	 * @param panelName
	 * @param controlName
	 * @param Object ... value
	 */
	@SuppressWarnings("rawtypes")
	public void DATA_VALUES(JList jlist, String listName, String[] ar_value) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardListModel listModel = null;
		String listItem = "";
		
		this.activeMenuName = "";
		
		panelComponent = this.getPanelComponent(listName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JList) {return;}

		// Popolamento modello jlist, tipicamente per prototipazione.
		listModel = (ForwardListModel) getJList(listName).getModel();
		for (Object obj_listItem : ar_value) {
			listItem = (String) obj_listItem;
			listModel._addElement(listItem);
		}
		return;
		
	}

	/**
	 * <h3>ComboBOx items value declaration</h3>
	 * 
	 * Are declared values that will be displayed by the ComboBox, tipically in the phase of GUI tuning.<br>
	 * <p>
	 * The <code>DATA_VALUES</code> declaration must be coded after the <code>COMPONENT()</code> declaration<br>
	 * <p>
	 * If the comboBox is not defined, no action will be executed.<br>
	 * If the first element of items list is specified as the initial value and it doesn't match any following item,<br>
	 * the first item will be displayed at the first show.<br>
	 * For any further panel default executed by <code>DO_PANEL_DEFAULT_CONTROLS()</code> no default value will be set.<br
	 * <p>
	 * @param jcomboBox to mark the constructor
	 * @param comboBoxName 
	 * @param isFirstElementDefault as a boolean true if the first element of ar_comboBoxItemValue is the default value
	 * @param ar_comboBoxItemValue a string array with comboBox items value
	 */
	@SuppressWarnings("rawtypes")
	public void DATA_VALUES(JComboBox jcomboBox, String comboBoxName, boolean isFirstElementDefault, String[] ar_comboBoxItemValue) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardComboBoxModel comboBoxModel = null;
		InnerComponent innerComponent = null;
		JComboBox jcomboBoxDeclared = null;
		String initialValue = "";
		
		this.activeMenuName = "";
		
		innerComponent = getComponentDeclared(comboBoxName);
		if (innerComponent == null) {return;}
		if (innerComponent.componentType != EnumForwardComponent.JComboBox) {return;}
		
		// Popolamento modello jComboBox, tipicamente per prototipazione.
		if (ar_comboBoxItemValue != null) {
			comboBoxModel = (ForwardComboBoxModel) getJComboBox(comboBoxName).getModel();
			for (Object comboBoxItemValue : ar_comboBoxItemValue) {
				if (isFirstElementDefault) {
					isFirstElementDefault = false;
					initialValue = (String)comboBoxItemValue;
					continue;
				}
				comboBoxModel._addElement(comboBoxItemValue, null);
			} 
		}
		
		// Recupero componente pannello
		panelComponent = this.getPanelComponent(comboBoxName);
		if (panelComponent == null) {return;}
		
		// Impostazione default
		panelComponent.setDefaultValue(initialValue);
		jcomboBoxDeclared = (JComboBox) panelComponent.getGraphicObject();
		jcomboBoxDeclared.setSelectedItem(initialValue);
		return;
	}

	/**
	 * <h3>ComboBOx items value declaration</h3>
	 * 
	 * Are declared values that will be displayed by the ComboBox, tipically in the phase of GUI tuning.<br>
	 * <p>
	 * The <code>DATA_VALUES</code> declaration must be coded after the <code>COMPONENT()</code> declaration<br>
	 * <p>
	 * If the comboBox is not defined, no action will be executed.<br>
	 * <p>
	 * @param jcomboBox to mark the constructor
	 * @param comboBoxName 
	 * @param ar_comboBoxItemValue a string array with comboBox items value
	 */
	@SuppressWarnings("rawtypes")
	public void DATA_VALUES(JComboBox jcomboBox, String comboBoxName, String[] ar_comboBoxItemValue) {
		
		ForwardComboBoxModel comboBoxModel = null;
		InnerComponent innerComponent = null;
		this.activeMenuName = "";
		
		innerComponent = getComponentDeclared(comboBoxName);
		if (innerComponent == null) {return;}
		if (innerComponent.componentType != EnumForwardComponent.JComboBox) {return;}
		
		// Popolamento modello jlist, tipicamente per prototipazione.
		comboBoxModel = (ForwardComboBoxModel) getJComboBox(comboBoxName).getModel();
		for (Object comboBoxItemValue : ar_comboBoxItemValue) {
			comboBoxModel._addElement(comboBoxItemValue, null);
		}
		return;
	}

	/**
	 * <h3>Table values declaration</h3>
	 * <br>
	 * This is the minimal constructor for a table editable, with columns header, cell data, no toolTip, specific renderer and editors.<br>
	 * The table will have resizable columns with no width preferred too.<br>
	 * <p>
	 * All types of data related to table are managed by own table model, initialized at the table declaring.<br>
	 * Here are required main data that populate the table model as column names, types, editable informations<br>
	 * column tooltip and finally data cell for rows and columns.<br>
	 * <p>
	 * The <code>DATA_VALUES()</code> declaration must be coded after the table <code>COMPONENT()</code> declaration.<br>
	 * <p>
	 * If the table specified is not defined, no action will be executed.<br>
	 * <p>
	 * @param jtable just to mark the method, not used
	 * @param tableName
	 * @param ar_columnName
	 * @param ar_columnHeader a text array with header
	 * @param ar_columnClass
	 * @param data an array [][] with cella data for row and column
	 */
	public void DATA_VALUES(JTable jtable 						// Di servizio per rendere identificabile il costruttore
						   ,String tableName 					// Nome tabella
					       ,String[] ar_columnName			    // Un elemento per colonna con il nome applicativo della colonna
					       ,String[] ar_columnHeader			// Un elemento per colonna con il l'intestazione della colonna
					       ,Class<?>[] ar_columnClass			// Un elemento per colonna con la classe indicante il tipo colonna
				           ,Object[][] data						// N elementi di riga, ognuno con n elementi di colonna
						   ) {
		
		TableCellRenderer[] ar_cellRenderer = null;
		TableCellEditor[] ar_cellEditor = null;
		int[] ar_columnWidth = null;			         
		boolean[] ar_columnResizible = null;			 
	    boolean[] ar_columnEditable = null;
	    String[] ar_columnToolTip = null;
		
		// Allocazione arrays null values
		ar_cellRenderer = new TableCellRenderer[ar_columnHeader.length];
		ar_cellEditor = new TableCellEditor[ar_columnHeader.length];
		ar_columnWidth = new int[ar_columnHeader.length];
		ar_columnResizible = new boolean[ar_columnHeader.length];
		ar_columnEditable = new boolean[ar_columnHeader.length];
		ar_columnToolTip = new String[ar_columnHeader.length];
		
		// Tutte le colonne sono resizable senza limiti di larghezza
		for (int i = 0; i < ar_columnResizible.length; i++) {
			ar_columnWidth[i] = -1;
			ar_columnResizible[i] = true;
			ar_columnEditable[i] = true;
			ar_columnToolTip[i] = "";
		}
		
		// Metodo comune a tutti i costruttori DATA_VALUES()
		dataValuesJTableCommon(tableName, ar_columnName, ar_columnHeader, ar_columnClass, ar_columnEditable, ar_columnToolTip, data, ar_columnWidth, ar_columnResizible, ar_cellRenderer, ar_cellEditor);
		return;
		
	}


	/**
	 * <h3>Table values declaration</h3>
	 * 
	 * All types of data related to table are managed by own table model, initialized at the table declaring.<br>
	 * Here are required main data that populate the table model as column names, types, editable informations<br>
	 * column tooltip and finally data cell for rows and columns.<br>
	 * <p>
	 * The <code>DATA_VALUES()</code> declaration must be coded after the table <code>COMPONENT()</code> declaration.<br>
	 * <p>
	 * If the table specified is not defined, no action will be executed.<br>
	 * <p>
	 * @param jtable just to mark the method, not used
	 * @param tableName
	 * @param ar_columnName
	 * @param ar_columnHeader a text array with header
	 * @param ar_columnClass
	 * @param ar_columnToolTip
	 * @param ar_columnEditable
	 * @param data an array [][] with cella data for row and column
	 */
	public void DATA_VALUES(JTable jtable 						// Di servizio per rendere identificabile il costruttore
						   ,String tableName 					// Nome tabella
					       ,String[] ar_columnName			    // Un elemento per colonna con il nome applicativo della colonna
					       ,String[] ar_columnHeader			// Un elemento per colonna con il l'intestazione della colonna
					       ,Class<?>[] ar_columnClass			// Un elemento per colonna con la classe indicante il tipo colonna
					       ,String[] ar_columnToolTip			// Un elemento per colonna con il toolTip di colonna
					       ,boolean[] ar_columnEditable			// Un elemento per colonna con true se editabile e false se non editabile
				           ,Object[][] data						// N elementi di riga, ognuno con n elementi di colonna
						   ) {
		
		TableCellRenderer[] ar_cellRenderer = null;
		TableCellEditor[] ar_cellEditor = null;
		int[] ar_columnWidth = null;			         
		boolean[] ar_columnResizible = null;			 
		
		// Allocazione arrays null values
		ar_cellRenderer = new TableCellRenderer[ar_columnHeader.length];
		ar_cellEditor = new TableCellEditor[ar_columnHeader.length];
		ar_columnWidth = new int[ar_columnHeader.length];
		ar_columnResizible = new boolean[ar_columnHeader.length];
		
		// Tutte le colonne sono resizable senza limiti di larghezza
		for (int i = 0; i < ar_columnResizible.length; i++) {
			ar_columnWidth[i] = -1;
			ar_columnResizible[i] = true;
		}
		
		// Metodo comune a tutti i costruttori DATA_VALUES()
		dataValuesJTableCommon(tableName, ar_columnName, ar_columnHeader, ar_columnClass, ar_columnEditable, ar_columnToolTip, data, ar_columnWidth, ar_columnResizible, ar_cellRenderer, ar_cellEditor);
		return;
		
	}

	/**
	 * <h3>Table values declaration</h3>
	 * 
	 * All types of data related to table are managed by own table model, initialized at the table declaring.<br>
	 * Here are required main data that populate the table model as column names, types, editable informations<br>
	 * column tooltip and finally data cell for rows and columns.<br>
	 * <p>
	 * The <code>DATA_VALUES()</code> declaration must be coded after the table <code>COMPONENT()</code> declaration.<br>
	 * <p>
	 * If the table specified is not defined, no action will be executed.<br>
	 * <p>
	 * @param jtable just to mark the method, not used
	 * @param tableName
	 * @param ar_columnName
	 * @param ar_columnHeader a text array with header
	 * @param ar_columnClass
	 * @param ar_columnToolTip
	 * @param ar_columnEditable
	 * @param ar_columnWidth an array with Width column , -1 value if no limit
	 * @param data an array [][] with cella data for row and column
	 */
	public void DATA_VALUES(JTable jtable 						// Di servizio per rendere identificabile il costruttore
						   ,String tableName 					// Nome tabella
					       ,String[] ar_columnName			    // Un elemento per colonna con il nome applicativo della colonna
					       ,String[] ar_columnHeader			// Un elemento per colonna con il l'intestazione della colonna
					       ,Class<?>[] ar_columnClass			// Un elemento per colonna con la classe indicante il tipo colonna
					       ,String[] ar_columnToolTip			// Un elemento per colonna con il toolTip di colonna
					       ,boolean[] ar_columnEditable			// Un elemento per colonna con true se editabile e false se non editabile
						   ,int[] ar_columnWidth				// Un elemento per colonna le dimensioni preferred in pixel
				           ,Object[][] data						// N elementi di riga, ognuno con n elementi di colonna
						   ) {
		
		TableCellRenderer[] ar_cellRenderer = null;
		TableCellEditor[] ar_cellEditor = null;
		boolean[] ar_columnResizible = null;			 
		
		// Allocazione arrays null values
		ar_cellRenderer = new TableCellRenderer[ar_columnHeader.length];
		ar_cellEditor = new TableCellEditor[ar_columnHeader.length];
		ar_columnWidth = new int[ar_columnHeader.length];
		ar_columnResizible = new boolean[ar_columnHeader.length];
		
		// Tutte le colonne sono resizable senza limiti di larghezza
		for (int i = 0; i < ar_columnResizible.length; i++) {
			ar_columnResizible[i] = true;
		}
		
		// Metodo comune a tutti i costruttori DATA_VALUES()
		dataValuesJTableCommon(tableName, ar_columnName, ar_columnHeader, ar_columnClass, ar_columnEditable, ar_columnToolTip, data, ar_columnWidth, ar_columnResizible, ar_cellRenderer, ar_cellEditor);
		return;
		
	}

	/**
	 * <h3>Table values declaration</h3>
	 * <br>
	 * This is the constructor for a table with all options available<br>
	 * <br>
	 * All types of data related to table are managed by own table model, initialized at the table declaring.<br>
	 * Here are required main data that populate the table model as column names, types, editable informations<br>
	 * column tooltip and finally data cell for rows and columns.<br>
	 * <p>
	 * The <code>DATA_VALUES()</code> declaration must be coded after the table <code>COMPONENT()</code> declaration.<br>
	 * <p>
	 * If the table specified is not defined, no action will be executed.<br>
	 * <p>
	 * @param jtable just to mark the method, not used
	 * @param tableName
	 * @param ar_columnName
	 * @param ar_columnHeader a text array with header
	 * @param ar_columnClass
	 * @param ar_columnToolTip
	 * @param al_columnEditable
	 * @param ar_columnResizible an array of boolean where true is for resizable column
	 * @param ar_columnWidth an array with Width column , -1 value if no limit
	 * @param ar_cellRenderer an array with column cell renderer to use or null
	 * @param ar_cellEditor an array with column cell editor to use or null
	 * @param data an array [][] with cella data for row and column
	 */
	public void DATA_VALUES(JTable jtable 						// Di servizio per rendere identificabile il costruttore
						   ,String tableName 					// Nome tabella
					       ,String[] ar_columnName			    // Un elemento per colonna con il nome applicativo della colonna
					       ,String[] ar_columnHeader			// Un elemento per colonna con il l'intestazione della colonna
					       ,Class<?>[] ar_columnClass			// Un elemento per colonna con la classe indicante il tipo colonna
					       ,String[] ar_columnToolTip			// Un elemento per colonna con il toolTip di colonna
					       ,boolean[] ar_columnEditable			// Un elemento per colonna con true se editabile e false se non editabile
						   ,boolean[] ar_columnResizible		// Un elemento per colonna con true se colonna resizable o false se fissa
						   ,int[] ar_columnWidth				// Un elemento per colonna le dimensioni preferred in pixel
						   ,TableCellRenderer[] ar_cellRenderer	// Un elemento per colonna con cell renderer da usare o null
						   ,TableCellEditor[] ar_cellEditor		// Un elemento per colonna con cell editor da usare o null
				           ,Object[][] data						// N elementi di riga, ognuno con n elementi di colonna
							) {
		
		
		// Metodo comune a tutti i costruttori DATA_VALUES()
		dataValuesJTableCommon(tableName, ar_columnName, ar_columnHeader, ar_columnClass, ar_columnEditable, ar_columnToolTip, data, ar_columnWidth, ar_columnResizible, ar_cellRenderer, ar_cellEditor);
		return;
		
	}
	
	/*
	 * Metodo comune per DATA_VALUES() JTable
	 */
	private void dataValuesJTableCommon(
								 String tableName 					// Nome tabella
								,String[] ar_columnName			    // Un elemento per colonna con il nome assegnato interno all'applicazione
								,String[] ar_columnHeader			// Un elemento per colonna con il l'intestazione della colonna
								,Class<?>[] ar_columnClass			// Un elemento per colonna con la classe indicante il tipo colonna
								,boolean[] ar_columnEditable		// Un elemento per colonna con true se editabile e false se non editabile
								,String[] ar_columnToolTip			// Un elemento per colonna con il toolTip di colonna
								,Object[][] data					// N elementi di riga, ognuno con n elementi di colonna
								,int[] ar_columnWidth				// Un elemento per colonna le dimensioni preferred in pixel
								,boolean[] ar_columnResizible		// Un elemento per colonna con true se colonna resizable o false se fissa
								,TableCellRenderer[] ar_cellRenderer// Un elemento per colonna con cell renderer da usare o null se default renderer
								,TableCellEditor[] ar_cellEditor	// Un elemento per colonna con cell editor da usare o null se default editor
								) {

		ForwardPanelComponent panelComponent = null;
		ForwardTableModel tableModel = null;
		ArrayList<ForwardTableModelColumn> al_column = null;
		ForwardTableModelColumn column = null;
		Object ar_dataCol[] = null;
        Object varObjectInitial = null;
		Class<?> columnClass = null;
        
		this.activeMenuName = "";

		panelComponent = this.getPanelComponent(tableName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JTable) {return;}

		// Modello tabella
		tableModel = panelComponent.getTableModel();

		al_column = tableModel._getColumns();
		
		// Scan colonne da inserire
		for (int i = 0; i < ar_columnHeader.length; i++) {
			column = new ForwardTableModelColumn(tableModel._getJtable(), i);
			al_column.add(column);
			column.setName(ar_columnName[i]);
			column.setHeader(ar_columnHeader[i]);
			column.setClassType(ar_columnClass[i]);
			column.setEditable(ar_columnEditable[i]);
			column.setResizable(ar_columnResizible[i]);
			column.setToolTipText(ar_columnToolTip[i]);
			column.setWidth(ar_columnWidth[i]);
			if (ar_columnWidth[i] < 0) {
 				column.setWidthOriginal(20);							// default minimo di 5 pixel
			} else {
				column.setWidthOriginal(ar_columnWidth[i]);
			}
			column.setCellRenderer(ar_cellRenderer[i]);
			column.setCellEditor(ar_cellEditor[i]); 
			
			// Variabile di colonna
			columnClass = ar_columnClass[i];
			if (columnClass == String.class) {
				varObjectInitial = "";
			}  else if (columnClass == Boolean.class) {
				varObjectInitial = new Boolean(false);
			}  else if (columnClass == Integer.class) {
				varObjectInitial = new Integer(0);
			}  else if (columnClass == Float.class) {
				varObjectInitial = new Float(0.0);
			}  else if (columnClass == Double.class) {
				varObjectInitial = new Double(0.0);
			}  else if (columnClass == Date.class) {
				varObjectInitial = new Date();
			}   else if (columnClass == Color.class) {
				varObjectInitial = new Color(0, 0, 0);
			}
			
			// Creazione variabile per valore corrente colonna con scope a livello di funzione
			VAR(EnumForwardScope.SCOPE_FUNCTION, ar_columnName[i], varObjectInitial);
		}
	
		// Scan righe
		for (int i = 0; i < data.length; i++) {
			ar_dataCol = data[i];
			ArrayList<Object> al_dataCol = new ArrayList<Object>();
			// Popolamento celle riga
			for (int j = 0; j < ar_dataCol.length; j++) {
				al_dataCol.add(ar_dataCol[j]);
			}
			tableModel._appendRow(al_dataCol);
		}
		
 		return;

	}

	/**
	 * <h3>Tree values declaration</h3>
	 * <br>
	 * This is the constructor for a tree data with all options available<br>
	 * <br>
	 * All types of data related to tree are managed by own tree model, initialized at the tree declaring.<br>
	 * Here are required main data that populate the tree model as node text names, optional id key and editable informations.<br>
	 * <p>
	 * This method is intend for a compact, exausive declaration of tree nodes, mainly for prototyping purpose and<br>
	 * as default values too. it's possible to declare the complete tree nodes hierarchy.<br>
	 * <p>
	 * For any node declared you can assign a string key identificator to be used by application to immediately<br>
	 * identify a node. The same key id can be use by DATA_BOUND() section to assign specific user application
	 * objects to nodes, using that direct key id, instead expensive operations to identify the node.<br>
	 * Of course application can then clear and populate the tree with database data or something else.<br>
	 * Any node user data object data must be stored as {@link ForwardTreeUserObject} object as forward keep necessary <br>
	 * data during the node life cycle.<br>
	 * For prototyping purpose it's enaugh to manage text only and thereby will be automatically created all necessary<br>
	 * {@link ForwardTreeUserObject} objects.<br>
	 * <code>DATA_BOUND()</code> section, optionally, lets to assign to each node declared a specific applicationuser object<br>
	 * that overrides the standard {@link ForwardTreeUserObject}, string based, automatically generated and assigned to the node.<br>
	 * <p>
	 * The <code>DATA_VALUES()</code> declaration must be coded after the tree <code>COMPONENT()</code> declaration.<br>
	 * <p>
	 * Here an example of recursive data tree hierarchy declaration:<pre>
	 * DATA_VALUES(new JTree(), "tr_tree01", "Root"
	 *                                        new Object[]{new Object[]{"Node1", "0-0"}                       // Node1 leaf 
	 *                                                    ,new Object[]{"Node2", "0-1",                       // Node2 folder 
	 *                                                                           new Object[]{"Node21", "0-2-0"		// .. 
	 *                                                                                       ,"Node22", "0-2-1"		// ..
	 *                                                                                       ,"Node23", "0-2-2"		// ..
	 *                                                                                       ,"Node24", "0-2-3"		//	Nodes child of Node2  
	 *                                                                                       ,"Node25", "0-2-4"		// ..
	 *                                                                                       ,"Node26", "0-2-5"		// ..
	 *                                                                                       ,"Node27", "0-2-6"		// ..
	 *                                                                                       }
	 *                                                     									  
	 *                                                                 }
	 *                                                    ,new Object[]{"Node3", "0-2", true}                 // Node3 folder with first service child automatically inserted
	 *                                                    ,new Object[]{"Node4", "0-2",                       // Node4 folder  
	 *                                                                           new Object[]{"Node41", "0-4-0"	   		// ..
	 *                                                                                       ,"Node42", "0-4-1"	   		// ..
	 *                                                                                       ,"Node43", "0-4-2"	   		// Nodes child of Node4 
	 *                                                                                       ,"Node44", "0-4-3"	   		// .
	 *                                                                                       ,"Node45", "0-4-4"	   		// ..
	 *                                                                                       }
	 *                                                                 }
	 *                                                    }
	 * 
	 * 
	 * If the tree specified is not defined, no action will be executed.<br>
	 * <p>
	 * @param jtree just to mark the method, not used
	 * @param treeName the internal name assigned to tree
	 * @param rootObject as a string object rapresenting the root node
	 * @param ar_NodeRecursive as a recursive objects array describing the tree hierarchy as showed in the example
	 */
	public void DATA_VALUES(JTree jtree 						// Di servizio per rendere identificabile il costruttore
						   ,String treeName 					// Nome tabella
						   ,Object rootObject 					// Oggetto applicativo root
					       ,Object ar_nodeRecursive[]			// Conterrà ricorsivamente strutture parent/childs come da esempio
						) {
		
		
		// Metodo comune a tutti i costruttori DATA_VALUES() 
		dataValuesJtreeCommon(treeName, rootObject, ar_nodeRecursive);
		return;
		
	}
	
	/*
	 * Metodo comune per DATA_VALUES() JTree
	 */
	private void dataValuesJtreeCommon(String treeName, Object rootObject, Object[] ar_nodeRecursive) {

		ForwardPanelComponent panelComponent = null;
		ForwardTreeModel treeModel = null;
		ForwardTreeUserObject nodeRootObject = null;
		
		this.activeMenuName = "";

		// Controlli di sicurezza
		panelComponent = this.getPanelComponent(treeName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JTree) {return;}
		
		// Descrittori modello e nodo di partenza
		treeModel = (ForwardTreeModel) getJTree(treeName).getModel();				// Modello Tree da utilizzare
		nodeRootObject = treeModel._getRootNode();
		
		// Popolamento ricorsivo tree
		dataValuesJtreeAddChildrenRecursive(treeModel, nodeRootObject, ar_nodeRecursive);
		
		// Gestione espansione iniziale tree, come dichiarato per la funzione
		if (treeModel._getInitialExpandMode() == ForwardTreeModel.INITIAL_EXPAND_ROOT) {
			treeModel._expandRoot();
			return;
		}
		if (treeModel._getInitialExpandMode() == ForwardTreeModel.INITIAL_EXPAND_ALL) {
			treeModel._expandAll();
			return;
		}
 		return;
	}


    /*
     * Analisi e inserimento nodi child al nodo parent corrente
     * Attivazione ricorsiva in caso di nodi folder con childrenn
     * Inserimento nodo child di servizio in caso di nodo folder con children a inserimento successivo on expand
     */
	private void dataValuesJtreeAddChildrenRecursive(ForwardTreeModel treeModel, ForwardTreeUserObject nodeParent, Object[] ar_nodeRecursive) {
		
		ForwardTreeUserObject nodeChildObject = null;
		DefaultMutableTreeNode nodeChildTree = null;
		Object ar_obj_item[] = null;
		Object obj_Children[] = null;
		Boolean isNodeWithAnyChild = null;
		String nodeIdKey = "";
		String nodeText = "";
		
		// Scan livello corrente oggetti da inserire
		for (Object obj_descriptor : ar_nodeRecursive) {
			ar_obj_item = (Object[]) obj_descriptor;
			
			// Descrittore di un nodo leaf
			if (ar_obj_item.length == 2) {
				nodeText = (String) ar_obj_item[0];
				nodeIdKey = (String) ar_obj_item[1];
				// Inserimento child
				nodeChildObject = new ForwardTreeUserObject(nodeText);
				nodeChildTree = new DefaultMutableTreeNode(nodeChildObject);
				nodeChildObject.setNodeJTree(nodeChildTree);
				treeModel._addNodeChild(nodeParent, nodeChildObject, false, false);
				// Update map nodi 
				if (nodeIdKey.equals("")) {continue;}
				treeModel._getMapNodes().put(nodeIdKey, nodeChildObject);
				continue;
			}
			
			// Descrittore di un nodo folder con children inseriti dall'applicazione ON_APPL_TREE_NODE_CHILDREN_TO_ADD
			if (ar_obj_item.length == 3 && !(ar_obj_item[2] instanceof Object[])) {
				nodeText = (String) ar_obj_item[0];
				nodeIdKey = (String) ar_obj_item[1];
				isNodeWithAnyChild = (Boolean) ar_obj_item[2];
				// Inserimento child
				nodeChildObject = new ForwardTreeUserObject(nodeText);
				nodeChildTree = new DefaultMutableTreeNode(nodeChildObject);
				nodeChildObject.setNodeJTree(nodeChildTree);
				treeModel._addNodeChild(nodeParent, nodeChildObject, isNodeWithAnyChild.booleanValue(), false);
				// Update map nodi 
				if (nodeIdKey.equals("")) {continue;}
				treeModel._getMapNodes().put(nodeIdKey, nodeChildObject);
				continue;
			}
			
			// Descrittore di un nodo folder e di tutti i suoi children
			if (ar_obj_item.length == 3 && ar_obj_item[2] instanceof Object[]) {
				nodeText = (String) ar_obj_item[0];
				nodeIdKey = (String) ar_obj_item[1];
				obj_Children =  (Object[]) ar_obj_item[2];
				// Inserimento child (nodo folder)
				nodeChildObject = new ForwardTreeUserObject(nodeText);
				nodeChildTree = new DefaultMutableTreeNode(nodeChildObject);
				nodeChildObject.setNodeJTree(nodeChildTree);
				treeModel._addNodeChild(nodeParent, nodeChildObject, false, false);
				// Update map nodi 
				if (!nodeIdKey.equals("")) {treeModel._getMapNodes().put(nodeIdKey, nodeChildObject);}
				// Attivazione ricorsiva inserimento child 
				if (obj_Children instanceof Object[]) {
					dataValuesJtreeAddChildrenRecursive(treeModel,nodeChildObject, obj_Children);
				}
			}
			
			// Caso non contemplato: nessuna operazione
		}
	}

	/**
	 * <h4>Declare application data bound for each node, to be bound to the tree</h3>
	 * <p>
	 * Thi is an optional declaration of specific user objects nodes to be bound to tree nodes.<br>
	 * By default, in the <code>DATA_VALUES()</code> section, a {@link ForwardTreeUserObject} object have been created<br>
	 * for each node, with the only text property. In this way it's possible to bind any user application objects,<br>
	 * with the constraint that they must inherits form {@link ForwardTreeUserObject}.<br>
	 * <p>
	 * The size of input arrays must be the same but no exception occurs if not.<br>
	 * <p>
	 * @param jtree the JTree object to qualify the constructor
	 * @param treeName name
	 * @param ar_idKey as a String array of id node keys as declared in <code>DATA_VALUES()</code> section.<br>
	 * @param ar_objDataBound as a {@link ForwardTreeUserObject} objects array bound to nodes identified by <code>ar_idKey</code>.<br>
	 */
	public void DATA_VALUES_BOUND(JTree jtree, String treeName, String ar_idKey[], ForwardTreeUserObject[] ar_objDataBound) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardTreeModel treeModel = null;
		ForwardTreeUserObject nodeDataBoundUserObject = null;
		DefaultMutableTreeNode nodeDataBoundTree = null;
		ForwardTreeUserObject mapNodeObjDataBound = null;
		String idKeyNode = "";
		
		this.activeMenuName = "";
		
		panelComponent = this.getPanelComponent(treeName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JTree) {return;}

		treeModel = (ForwardTreeModel) getJTree(treeName).getModel();

		// Scan data bound
		for (int i = 0; i < ar_idKey.length; i++) {
			if (i >= ar_objDataBound.length) {return;}
			
			// Key node, object bound emap key node element
			idKeyNode = ar_idKey[i];
			nodeDataBoundUserObject = ar_objDataBound[i];
			mapNodeObjDataBound = treeModel._getMapNodes().get(idKeyNode);
			
			// Key node non dichiarata in DATA_VALUES(): skip
			if (mapNodeObjDataBound == null) {
				continue;
			}							
			
			// Update node tree con user Object fornito
			nodeDataBoundTree = mapNodeObjDataBound.getNodeJTree();
			nodeDataBoundTree.setUserObject(nodeDataBoundUserObject);
			nodeDataBoundUserObject.setNodeJTree(nodeDataBoundTree);
			mapNodeObjDataBound = treeModel._getMapNodes().put(idKeyNode, nodeDataBoundUserObject);
		}
	}
	

	/**
	 * <h4>Declare application data bound for each item in the list</h3>
	 * <p>
	 * @param jlist the JList object to qualify the constructor
	 * @param list name
	 * @param array of objects to bind to items, one to one.
	 */
	@SuppressWarnings("rawtypes")
	public void DATA_VALUES_BOUND(JList jlist, String listName, Object[] ar_objDataBound) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardListModel listModel = null;
		Object objDataBound = null;
		
		this.activeMenuName = "";
		
		panelComponent = this.getPanelComponent(listName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JList) {return;}

		listModel = (ForwardListModel) getJList(listName).getModel();

		// Scan data bound
		for (int i = 0; i < ar_objDataBound.length; i++) {
			objDataBound = ar_objDataBound[i];
			if (i >= listModel.getSize()) {return;}
			// Update oggetto bound di riga, precaricato all'inserimento dati a null
			listModel._getAllDataBound().set(i, objDataBound);
		}
	}
	
	/**
	 * <h4>Declare application data bound for each column in the table</h3>
	 * <p>
	 * The name of a variable, used runtime by monitor, to store the current row object bound, is necessary too.<br>
	 * <p>
	 * @param jtable the JTable object to qualify the constructor
	 * @param tableName the table name
	 * @param varNameObjectBound the name of the variable always updated with the currente row object bound
	 * @param ar_objDataBound as an array of objects to bind to rows, one to one.<br>
	 * A null value means no prototupe bound object.
	 */
	public void DATA_VALUES_BOUND(JTable jtable, String tableName, String varNameObjectBound, Object[] ar_objDataBound) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardTableModel tableModel = null;
		Object objDataBound = null;
		
		this.activeMenuName = "";
		
		panelComponent = this.getPanelComponent(tableName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JTable) {return;}
		
		tableModel = panelComponent.getTableModel();
		tableModel._setVarNameDataBound(varNameObjectBound);
		
		// Creazione variabile per valore corrente oggetto bound di riga con scope a livello di funzione
		VAR(EnumForwardScope.SCOPE_FUNCTION, tableModel._getVarNameDataBound(), new Object());

		// Nessun oggetto bound prototipale
		if (ar_objDataBound == null) {
			return;
		}
		
		// Scan data bound
		for (int i = 0; i < ar_objDataBound.length; i++) {
			objDataBound = ar_objDataBound[i];
			if (i >= tableModel._getDataBound().size()) {return;}
			// Update oggetto bound di riga, precaricato a null
			tableModel._getDataBound().set(i, objDataBound);
		}
	}
	
	/**
	 * <h4>Declare application data bound for each item in the list</h3>
	 * <p>
	 * The number of input bound objects must be the same of items.<br>
	 * A greater value will be discarded.<br>
	 * <p>
	 * @param jcomboBox the JComboBox object to qualify the constructor
	 * @param comboBoxName the comboBox of which to bind application data
	 * @param ar_objDataBound data to bind to comboBox items
	 */
	@SuppressWarnings("rawtypes")
	public void DATA_VALUES_BOUND(JComboBox jcomboBox, String comboBoxName, Object[] ar_objDataBound) {
		
		ForwardPanelComponent panelComponent = null;
		ForwardComboBoxModel comboBoxModel = null;
		Object objDataBound = null;
		
		this.activeMenuName = "";
		
		panelComponent = this.getPanelComponent(comboBoxName);
		if (panelComponent == null) {return;}
		if (panelComponent.getType() != EnumForwardComponent.JComboBox) {return;}

		comboBoxModel = (ForwardComboBoxModel) getJComboBox(comboBoxName).getModel();

		// Scan data bound
		for (int i = 0; i < ar_objDataBound.length; i++) {
			objDataBound = ar_objDataBound[i];
			if (i >= comboBoxModel.getSize()) {return;}
			// Update oggetto bound di riga, precaricato a null
			comboBoxModel._getAllDataBound().set(i, objDataBound);
		}
	}
	

	/**
	 * <h4>End form declaration</h3>
	 * It's simply a marker to finish the current form declaration.
	 */
	public void END_FORM() {
		this.activeFormName = "";
		this.activeForm = null;
	}
	

	/**
	 * <h4>Menu declaration for a root menu</h3>
	 * This is the constructor for the root menu definition, for any type of menu.<br>
	 * All submenus inherit direction and style from root menu.<br>
	 * <br>
	 * @param menuName
	 * @param menuTitle as the caption when the menu type is MENU_TYPE_MENUBAR
	 * @param menuType as <code>EnumForwardPanelOption.MENU_TYPE_ROOT .. MENU_TYPE_POPUP .. MENU_TYPE_SUBMENU</code>
	 * @param menuRendering as <code>EnumForwardPanelOption.MENU_RENDERING_MENUBAR .. MENU_RENDERING_TOOLBAR .. MENU_RENDERING_PLAIN_BUTTONS .. MENU_TYPE_SECTIONS_HIDE</code>
	 * @param menuDirection as <code>EnumForwardPanelOption.MENU_DIRECTION_VERTICAL .. MENU_DIRECTION_HORIZONTAL</code>
	 * @param menuStyle as <code>EnumForwardPanelOption.MENU_STYLE_AUTOMATIC .. MENU_STYLE_CUSTOM .. MENU_STYLE_THIN .. MENU_STYLE_MEDIUM .. MENU_STYLE_LARGE</code>
	 */
	public void MENU(String menuName, String menuTitle, EnumForwardOption menuType, EnumForwardOption menuRendering, EnumForwardOption menuDirection, EnumForwardOption menuStyle, EnumForwardOption menuHeigh) {
		ForwardMenu forwardMenu = null;
		forwardMenu = this.addMenu(menuName, menuTitle, menuType, menuRendering, menuDirection, menuStyle, menuHeigh, null);
		forwardMenu.setFunction(this);
		this.activeMenuName = menuName;
		this.activeMenuNameRoot = menuName;
	}


	/**
	* <h4>Menu item declaration for a {@link JMenuItem}</h3>
	 * <br>
	 * Here is declared a menu item with minimal properties, no icon, no short key.<br>
	 * The menu item is declared always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JMenuItem jmenuItem
			            , String controlName
						, String menuItemCaption
						  ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JButton jbuttonMenuItem = null;
		
		// Accodamento item a menu corrente
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, menuItemCaption);
		
		// Button con cui eventualmente sarà implementato il menu item di una toolbar o di una menu di pulsanti
		// In fase di costruzione della GUI verrà assegnato il listener del monitor di forward
		jbuttonMenuItem = new JButton();
		
		// Informazioni di default item
 		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setSubMenuActivation(false);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setGraphicObject(jbuttonMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		
		// Oggetto grafico standard JMenuItem
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jmenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		
		// Oggetto grafico JButton
		jbuttonMenuItem.setText(menuItemCaption);
		jbuttonMenuItem.setName(controlName);
		

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JMenuItem, jmenuItem);
		innerComponent.menuName = this.activeMenuName;
		innerComponent.panelName = "";
	}


	
	/**
	 * <h4>Menu item declaration for a {@link JMenuItem}</h3>
	 * <br>
	 * Here is declared a menu item with icon.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param iconPath
	 */
	public void MENU_ITEM(JMenuItem jmenuItem
			            , String controlName
						, String menuItemCaption
						, String iconPath
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JButton jbuttonMenuItem = null;
		
		// Accodamento item a menu corrente
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, menuItemCaption);
		
		// Button con cui eventualmente sarà implementato il menu item di una toolbar o di una menu di pulsanti
		// In fase di costruzione della GUI verrà assegnato il listener del monitor di forward
		jbuttonMenuItem = new JButton();
		
		// Informazioni di default item
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setGraphicObject(jbuttonMenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);

		// Oggetto grafico standard JMenuItem
 		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jmenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jmenuItem.setIcon(createImageIcon(iconPath));
		
		// Oggetto grafico JButton
		jbuttonMenuItem.setText(menuItemCaption);
		jbuttonMenuItem.setName(controlName);
		jbuttonMenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jbuttonMenuItem.setIcon(createImageIcon(iconPath));
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JMenuItem, jmenuItem);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}


	
	
	/**
	 * <h4>Menu item declaration for a {@link JMenuItem}</h3>
	 * <br>
	 * Here is declared a menu item with all properties.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param iconPath
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 */
	public void MENU_ITEM(JMenuItem jmenuItem
			            , String controlName
						, String menuItemCaption
						, String iconPath
						, int shortKey
						, int keyModifiers
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JButton jbuttonMenuItem = null;
		
		// Accodamento item a menu corrente
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, menuItemCaption);
		
		// Button con cui eventualmente sarà implementato il menu item di una toolbar o di una menu di pulsanti
		// In fase di costruzione della GUI verrà assegnato il listener del monitor di forward
		jbuttonMenuItem = new JButton();
		
		// Informazioni di default item
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setGraphicObject(jbuttonMenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
		   jmenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jmenuItem.setMnemonic(shortKey);
		}
		
		// Oggetto grafico standard JMenuItem
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jmenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jmenuItem.setIcon(createImageIcon(iconPath));

		// Oggetto grafico JButton
		jbuttonMenuItem.setText(menuItemCaption);
		jbuttonMenuItem.setName(controlName);
		jbuttonMenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jbuttonMenuItem.setIcon(createImageIcon(iconPath));
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JMenuItem, jmenuItem);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}

	/**
	 * <h4>Menu item declaration for a {@link JMenuItem}</h3>
	 * <br>
	 * Here is declared a menu item starting a submenu specification.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenu the swing component object
	 * @param controlName the name assigned to component
	 * @param subMenuName the submenu name to be declared as a specific menu.
	 */
	public void MENU_ITEM_SUBMENU(String controlName, String subMenuName) {
		
		JMenuItem jmenuItem = null;
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JButton jbuttonMenuItem = null;
		
		// Accodamento item a menu corrente
		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenu.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, "*");
	
		// Button con cui eventualmente sarà implementato il menu item di una toolbar o di una menu di pulsanti
		// In fase di costruzione della GUI verrà assegnato il listener del monitor di forward
		jbuttonMenuItem = new JButton();
		
		// Informazioni di default item
		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(subMenuName);
 		forwardMenuItem.setCaption("*");
		forwardMenuItem.setSubMenuActivation(true);
		forwardMenuItem.setSubMenu(null);
  	 	forwardMenuItem.setSubMenuName(subMenuName);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setGraphicObject(jbuttonMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		
		// Oggetto grafico standard JMenuItem
		jmenuItem.setText("*");
		jmenuItem.setName(controlName);
		jmenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Oggetto grafico JButton
		jbuttonMenuItem.setName(controlName);
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JMenuItem, jmenuItem);
		innerComponent.panelName = "";	
		innerComponent.menuName = this.activeMenuName;


	}
	
	/**
	 * <h4>Menu item declaration for a {@link JMenuItem}</h3>
	 * <br>
	 * Here is declared a menu item starting a submenu specification with accelerator keys.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenu the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param subMenuName the submenu name to be declared as a specific menu.
	 */
	public void MENU_ITEM_SUBMENU(String controlName
								, String subMenuName
								, int shortKey
								, int keyModifiers
							     ) {
		
		JMenuItem jmenuItem = null;
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JButton jbuttonMenuItem = null;
		
		// Accodamento item a menu corrente
		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenu.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, "*");

		// Button con cui eventualmente sarà implementato il menu item di una menu di pulsanti
		// In fase di costruzione della GUI verrà assegnato il listener del monitor di forward
		jbuttonMenuItem = new JButton();

		// Informazioni di default item
		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption("*");
		forwardMenuItem.setSubMenuActivation(true);
		forwardMenuItem.setSubMenu(null);
  	 	forwardMenuItem.setSubMenuName(subMenuName);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setGraphicObject(jbuttonMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jmenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jmenuItem.setMnemonic(shortKey);
		}

		// Oggetto grafico standard JMenuItem
		jmenuItem.setText("*");
		jmenuItem.setName(controlName);
		jmenuItem.setFont(jmenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
	
		// Oggetto grafico JButton
		jbuttonMenuItem.setName(controlName);
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JMenuItem, jmenuItem);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	


	/**
	 * <h4>Menu item declaration for a {@link JCheckBoxMenuItem}</h3>
	 * <br>
	 * Here is declared aJCheckBoxMenuItem  menu item with minimal properties set to false.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JCheckBoxMenuItem jcheckBoxMenuItem
			            , String controlName
						, String menuItemCaption
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JCheckBoxMenuItem.class, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JCheckBoxMenuItem, menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JCheckBoxMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jcheckBoxMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jcheckBoxMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		// Oggetto grafico
		jcheckBoxMenuItem.setText(menuItemCaption);
		jcheckBoxMenuItem.setName(controlName);
		jcheckBoxMenuItem.setSelected(false);
		jcheckBoxMenuItem.setFont(jcheckBoxMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;
	}
	


	/**
	 * <h4>Menu item declaration for a {@link JCheckBoxMenuItem}</h3>
	 * <br>
	 * Here is declared a JCheckBoxMenuItem  menu item with initial set and icon.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param initialSet true for control initially checked
	 * @param iconPath 
	 */
	public void MENU_ITEM(JCheckBoxMenuItem jcheckBoxMenuItem
			            , String controlName
						, String menuItemCaption
						, boolean initialSet
						, String iconPath
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JCheckBoxMenuItem.class, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jcheckBoxMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jcheckBoxMenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		// Oggetto grafico
 		jcheckBoxMenuItem.setText(menuItemCaption);
		jcheckBoxMenuItem.setName(controlName);
		jcheckBoxMenuItem.setSelected(initialSet);
		jcheckBoxMenuItem.setFont(jcheckBoxMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jcheckBoxMenuItem.setIcon(createImageIcon(iconPath));
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;
		innerComponent.groupName = "";
	}
	


	/**
	 * <h4>Menu item declaration for a {@link JCheckBoxMenuItem}</h3>
	 * <br>
	 * Here is declared a JCheckBoxMenuItem  menu item with all properties.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param initialSet true for control initially checked
	 * @param iconPath 
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 */
	public void MENU_ITEM(JCheckBoxMenuItem jcheckBoxMenuItem
			            , String controlName
						, String menuItemCaption
						, boolean initialSet
						, String iconPath
						, int shortKey
						, int keyModifiers
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JCheckBoxMenuItem.class, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName, EnumForwardComponent.JMenuItem, menuItemCaption);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jcheckBoxMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jcheckBoxMenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		forwardMenuItem.setGroupName("");
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jcheckBoxMenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jcheckBoxMenuItem.setMnemonic(shortKey);
		}
		jcheckBoxMenuItem.setText(menuItemCaption);
		jcheckBoxMenuItem.setName(controlName);
		jcheckBoxMenuItem.setSelected(initialSet);
		jcheckBoxMenuItem.setFont(jcheckBoxMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jcheckBoxMenuItem.setIcon(createImageIcon(iconPath));

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JCheckBoxMenuItem, jcheckBoxMenuItem);
		innerComponent.panelName = "";
		innerComponent.groupName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	
	/**
	 * <h4>Menu item declaration for a {@link JRadioButtonMenuItem}</h3>
	 * <br>
	 * Here is declared aJCheckBoxMenuItem  menu item with minimal properties.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JRadioButtonMenuItem jradioButtonMenuItem
			            , String controlName
						, String menuItemCaption
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JRadioButtonMenuItem.class, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JRadioButtonMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jradioButtonMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jradioButtonMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		// Oggetto grafico
		jradioButtonMenuItem.setText(menuItemCaption);
		jradioButtonMenuItem.setName(controlName);
		jradioButtonMenuItem.setSelected(false);
		jradioButtonMenuItem.setFont(jradioButtonMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		innerComponent.panelName = "";
		innerComponent.groupName = "";
		innerComponent.menuName = this.activeMenuName;


	}
	


	/**
	 * <h4>Menu item declaration for a {@link JRadioButtonMenuItem}</h3>
	 * <br>
	 * Here is declared a JRadioButtonMenuItem  menu item with initial set and group name.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jradioButtonMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param initialSet true for control initially checked
	 * @param groupName the group owner to toggle values
	 */
	public void MENU_ITEM(JRadioButtonMenuItem jradioButtonMenuItem
			            , String controlName
						, String menuItemCaption
						, boolean initialSet
						, String groupName
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JRadioButtonMenuItem.class, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JRadioButtonMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jradioButtonMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jradioButtonMenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName(groupName);
		
		// Oggetto grafico
		jradioButtonMenuItem.setText(menuItemCaption);
		jradioButtonMenuItem.setName(controlName);
		jradioButtonMenuItem.setSelected(initialSet);
		jradioButtonMenuItem.setFont(jradioButtonMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		innerComponent.panelName = "";
		innerComponent.groupName = groupName;
		innerComponent.menuName = this.activeMenuName;

	}
	
	/**
	 * <h4>Menu item declaration for a {@link JRadioButtonMenuItem}</h3>
	 * <br>
	 * Here is declared a JRadioButtonMenuItem  menu item with all properties.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jradioButtonMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param initialSet true for control initially checked
	 * @param groupName the group owner to toggle values
	 * @param iconPath 
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 */
	public void MENU_ITEM(JRadioButtonMenuItem jradioButtonMenuItem
			            , String controlName
						, String menuItemCaption
						, boolean initialSet
						, String groupName
						, String iconPath
						, int shortKey
						, int keyModifiers
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JRadioButtonMenuItem.class, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JRadioButtonMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jradioButtonMenuItem);
		forwardMenuItem.setGraphicObjectMenu(jradioButtonMenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		forwardMenuItem.setGroupName(groupName);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jradioButtonMenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jradioButtonMenuItem.setMnemonic(shortKey);
		}
		jradioButtonMenuItem.setText(menuItemCaption);
		jradioButtonMenuItem.setName(controlName);
		jradioButtonMenuItem.setSelected(initialSet);
		jradioButtonMenuItem.setFont(jradioButtonMenuItem.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jradioButtonMenuItem.setIcon(createImageIcon(iconPath));
		
		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JRadioButtonMenuItem, jradioButtonMenuItem);
		innerComponent.panelName = "";
		innerComponent.groupName = groupName;
		innerComponent.menuName = this.activeMenuName;

	}
	


	/**
	 * <h4>Menu item declaration for a {@link JButton}</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a button in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jbutton the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 * @param iconPath 
	 */
	public void MENU_ITEM(JButton jbutton
			            , String controlName
						, String menuItemCaption
						, int shortKey
						, int keyModifiers
						, String iconPath
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;
		
		this.setComponentPropertiesDefault(JButton.class, EnumForwardComponent.JButton, jbutton);

		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jbutton);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setPathIcon(iconPath);
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jmenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jmenuItem.setMnemonic(shortKey);
		}
		
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jbutton.setText(menuItemCaption);
		jbutton.setName(controlName);
		jbutton.setFont(jbutton.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default
		jbutton.setIcon(createImageIcon(iconPath));

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JButton, jbutton);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	

	/**
	 * <h4>Menu item declaration for a {@link JButton} on a menu type toolbar</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a button in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JButton jbutton
			            , String controlName
						, String menuItemCaption
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;
		
		this.setComponentPropertiesDefault(JButton.class, EnumForwardComponent.JButton, jbutton);

		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JButton);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jbutton);
		forwardMenuItem.setGraphicObjectMenu(jbutton);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jbutton.setText(menuItemCaption);
		jbutton.setName(controlName);
		jbutton.setFont(jbutton.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JButton, jbutton);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	


	/**
	 * <h4>Menu item declaration for a {@link JTextField}</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a textField in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jtextField the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 */
	public void MENU_ITEM(JTextField jtextField
			            , String controlName
						, String menuItemCaption
						, int shortKey
						, int keyModifiers
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;
		
		this.setComponentPropertiesDefault(JTextField.class, EnumForwardComponent.JTextField, jtextField);

		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JMenuItem);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jtextField);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jmenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jmenuItem.setMnemonic(shortKey);
		}
		
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jtextField.setText(menuItemCaption);
		jtextField.setName(controlName);
		jtextField.setFont(jtextField.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JTextField, jtextField);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	

	/**
	 * <h4>Menu item declaration for a {@link JTextField}</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a textField in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jcheckBoxMenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JTextField jtextField
			            , String controlName
						, String menuItemCaption
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;
		
		this.setComponentPropertiesDefault(JTextField.class, EnumForwardComponent.JTextField, jtextField);

		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JTextField);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jtextField);
		forwardMenuItem.setGraphicObjectMenu(jtextField);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		jmenuItem.setText(menuItemCaption);
		jmenuItem.setName(controlName);
		jtextField.setText(menuItemCaption);
		jtextField.setName(controlName);
		jtextField.setFont(jtextField.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JTextField, jtextField);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	



	/**
	 * <h4>Menu item declaration for a {@link JComboBox}</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a comboBox in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jtextField the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 * @param shortKey as KeyEvent.VK_x
	 * @param keyModifiers as ActionEvent.ALT_MASK | ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK
	 */
	@SuppressWarnings("rawtypes")
	public void MENU_ITEM(JComboBox jcomboBox
			            , String controlName
						, String menuItemCaption
						, int shortKey
						, int keyModifiers
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;
		
		this.setComponentPropertiesDefault(JComboBox.class, EnumForwardComponent.JComboBox, jcomboBox);
		
		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JComboBox);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jcomboBox);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(shortKey);
		forwardMenuItem.setKeyModifiers(keyModifiers);
		
 		// Presenti shortKey e keyModifiers, attivazione accelerator con visualizzazione, per esempio Alt-1
		if (shortKey > 0 && keyModifiers > 0) {
			jmenuItem.setAccelerator(KeyStroke.getKeyStroke(shortKey, keyModifiers));
		}
		// Presente solo shortKey, attivazione solo mnemonic per shortKey NON visualizzato
		if (shortKey > 0 && keyModifiers == 0) {
			jmenuItem.setMnemonic(shortKey);
		}
		
		jmenuItem.setName(controlName);
		jmenuItem.setText(menuItemCaption);
		jcomboBox.setName(controlName);
		jcomboBox.setFont(jcomboBox.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JComboBox, jcomboBox);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	

	/**
	 * <h4>Menu item declaration for a {@link JComboBox}</h4>
	 * <br>
	 * This method is intended for declaring a menu item as a comboBox in a menu implemented by a swing {@link JToolBar} object.<br>
	 * The menu item is declared as always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * A JMenuItem object will be created too.<br>
	 * <br>
	 * @param jcomboBox the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	@SuppressWarnings("rawtypes")
	public void MENU_ITEM(JComboBox jcomboBox
			            , String controlName
						, String menuItemCaption
					     ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		JMenuItem jmenuItem = null;

		this.setComponentPropertiesDefault(JComboBox.class, EnumForwardComponent.JComboBox, jcomboBox);
		
		jmenuItem = new JMenuItem();
		this.setComponentPropertiesDefault(JMenuItem.class, EnumForwardComponent.JMenuItem, jmenuItem);
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JComboBox);
		forwardMenuItem.setControlName(controlName);
 		forwardMenuItem.setCaption(menuItemCaption);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jcomboBox);
		forwardMenuItem.setGraphicObjectMenu(jmenuItem);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		forwardMenuItem.setGroupName("");
		
		jmenuItem.setName(controlName);
		jmenuItem.setText(menuItemCaption);
		jcomboBox.setName(controlName);
		jcomboBox.setFont(jcomboBox.getFont().deriveFont(Font.PLAIN));				// Elimina il Bold di default

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JButton, jcomboBox);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}
	


	
	/**
	 * <h4>Menu item declaration for a {@link JSeparator}</h3>
	 * <br>
	 * Here is declared a menu item with minimal properties, no icon, no short key.<br>
	 * The menu item is declared always as a menu bar item, the most general declaration.<br>
	 * The scope is to change the aspect and the behaviour of a menu just changing its type.
	 * <br>
	 * @param jmenuItem the swing component object
	 * @param controlName the name assigned to component
	 * @param menuItemCaption
	 */
	public void MENU_ITEM(JSeparator jseparator
			            , String controlName
						  ) {
		
		ForwardMenuItem forwardMenuItem = null;
		InnerComponent innerComponent = null;
		
		this.setComponentPropertiesDefault(JSeparator.class, EnumForwardComponent.JSeparator, jseparator);
		
		forwardMenuItem = this.addMenuItem(this.activeMenuName, controlName);
 		forwardMenuItem.setComponentType(EnumForwardComponent.JSeparator);
		forwardMenuItem.setSubMenuActivation(false);
 	 	forwardMenuItem.setSubMenuName("");
		forwardMenuItem.setGraphicObject(jseparator);
		forwardMenuItem.setGraphicObjectMenu(jseparator);
		forwardMenuItem.setPathIcon("");
		forwardMenuItem.setShortKey(0);
		forwardMenuItem.setKeyModifiers(0);
		jseparator.setName(controlName);

		// Inserimento in struttura generale componenti funzione
		innerComponent = putComponentOnStructure(controlName, EnumForwardComponent.JSeparator, jseparator);
		innerComponent.panelName = "";
		innerComponent.menuName = this.activeMenuName;

	}


	
	
	/**
	 * <h3>Begin function tuning</h3>
	 * <p>
	 * This section allows you to code declarative statements to customize some functional aspects,<br>
	 * related to the overall function or the specific component layed out on a panel.<br>
	 * Actually it's possible to specify all declaratives below:<pre>
	 * TABLE_HELP()
	 * TABLE_CAPTION()
	 * FUNCTION_CALLABLE()
	 * COMPONENT_LDV_TO_EXEC()
	 * COMPONENT_LOOKUP_FUNCTION()
	 * COMPONENT_LOOKUP_RULE_TABLE()
	 * COMPONENT_OPTION()<7pre>
	 * Please refer to related declarative documentation for any detail.<br>
	 * 
	 */
	public void BEGIN_FUNCTION_TUNING() {
		this.activeMenuName = "";
	}

	/**
	 * <h3>End function tuning </h3>
	 * It's just a marker for the on of section<br>
	 * <p>
	 */
	public void END_FUNCTION_TUNING() {
	}
	
	/**
	 * <h3>Enables a function to be automatically callable.</h3>
	 * <p>
	 * The forward monitor allows you to call an enabled function by means of<br>
	 * a specific function key, that starts a system function with all allowed functions.<br>
	 * Here your'e allowed to declare a callabl functions, with parameters required and retirned.<br>
	 * <p>
	 * No control will be done at declaration time.<br>
	 * <p>
	 * @param functionClassNames as a string array of function class names
	 * @param parmNamesRequired as a string array with two token for each element, the first token as the variable or GUI component name<br>
	 * in the caller function with the item key, the second with the parameter name rquired by called lookup function. 
	 * @param parmNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the parameter name returned by called lookup function. 
	 */
	public void FUNCTION_CALLABLE(String functionClassName,  String parmNamesRequired[], String parmNamesReturned[] ) {
		InnerFunctionsCallable innerFunctionCallable = null;
		
		innerFunctionCallable = new InnerFunctionsCallable();
		this.hm_callable.put(functionClassName, innerFunctionCallable);
		 
		// Parametri richiesti
		for (String parmNameRequired : parmNamesRequired) {
			innerFunctionCallable.al_parmRequired.add(parmNameRequired);
		}
		
		// Parametri restituiti
		for (String parmNameReturned : parmNamesReturned) {
			innerFunctionCallable.al_parmReturned.add(parmNameReturned);		}
	}

	/**
	 * <h3>Sets the help rule table for function, panel and component, HTML help</h3>
	 * <p>
	 * The forward monitor allows you to show a localized help at function level, panel level<br>
	 * and single component layed out the panel.<br> 
	 * Any help can have a technical target or a functionally target.<br>
	 * <p>
	 * The table item key is panelName + ":" + componentName 
	 * If panelName = componentName = "", that's a function level help
	 * If componentName = "", that's a panel level help
	 * Otherwise is panel component level help.<br>
	 * <p>
	 * @param numTableHelpTechnical as the number of forward table with technical helps for the function
	 * @param numTableHelpEndUser as the number of forward table with end user helps for the function
	 */
	public void TABLE_HELP(int numTableHelpTechnical, int numTableHelpEndUser) {
		this.numTableHelpTechnical = numTableHelpTechnical;
		this.numTableHelpEndUser = numTableHelpEndUser;
	}

	/**
	 * <h3>Sets the forward table for all localized function captions.</h3>
	 * <p>
	 * The forward monitor, at the initialization time, sets all captions to the values coded at declaration time. <br>
	 * If a different localization is required, the input table number will be used to get all function captions in<br>
	 * the new language, if any.
	 * <p>
	 * Furthermore, afterward if customizations by user/function/component/language are effective, customized values will
	 * be used instead.>br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:X:componentName</b><br>
	 * Where <b>X</b> is an ordinal of {@link EnumForwardCaptionCustomization} enumeration.<br>
	 * When <b>componentName</b> is a null string, the caption is at the panel level, according to <b>X</b> value.<br>
 	 * <p>
	 * @param numTableCaptions as the number of forward table with captions for the function
	 */
	public void TABLE_CAPTIONS(int numTableCaptions) {
		this.numTableCaptions = numTableCaptions;
	}

	/**
	 * <h3>Sets the modality of the lookup rule table selection function activation with minimal specifications. </h3>
	 * <p>
 	 * The lookup function that realizes the selection of a rule table item needs to code parameters<br>
 	 * required and returned by means of PARM_REQUIRED() and PARM_RETURNED() declarations.<br>
 	 * Actually it needs the numTable and language and returns the item key and the description.<br>
 	 * <p>
 	 * Required NumTable and language are automatically supplied to the function by forward monitor.<br>
 	 * Informations for returned parameters are described by the input parameter,  an array of strings <br>
 	 * where each element contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter name required by called application, defined in the called function by means of a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function<br>
	 * and, after the item selection, all parameters returned by called function, in the caller function
	 * <p>
	 * The lookup function used will be the standard forward function {@link FunctionLookUpRuleTable}.<br>
	 * <p>
	 * If the component has been not defined in any panel, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * in the caller function with the item key, the second with the parameter name rquired by called lookup function. 
	 * @param parmNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the parameter name returned by called lookup function. 
	 */
	public void COMPONENT_LOOKUP_RULE_TABLE(String componentName, int numRuleTable, String parmNamesReturned[] ) {
		componentLookupRuleTableCommon(componentName, FUNCTION_LOOKUP_RULE_TABLE, numRuleTable, new String[]{}, parmNamesReturned);
	}
    
	/**
	 * <h3>Sets the modality of the lookup rule table selection function activation. </h3>
	 * <p>
 	 * The lookup function that realizes the selection of a rule table item needs to code parameters<br>
 	 * required and returned by means of PARM_REQUIRED() and PARM_RETURNED() declarations.<br>
 	 * Actually it needs the numTable and language and returns the item key and the description.<br>
 	 * <p>
 	 * Required NumTable and language are automatically supplied to the function by forward monitor.<br>
 	 * Informations for returned parameters are described by the input parameter,  an array of strings <br>
 	 * where each element contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter name required by called application, defined in the called function by means of a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function<br>
	 * and, after the item selection, all parameters returned by called function, in the caller function
	 * <p>
	 * The lookup function used will be the standard forward function {@link FunctionLookUpRuleTable}.<br>
	 * <p>
	 * If the component has been not defined in any panel, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * @param parmNamesRequired as a string array with two token for each element, the first token as the variable or GUI component name<br>
	 * in the caller function with the item key, the second with the parameter name rquired by called lookup function. 
	 * @param parmNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the parameter name returned by called lookup function. 
	 */
	public void COMPONENT_LOOKUP_RULE_TABLE(String componentName, int numRuleTable, String parmNamesRequired[], String parmNamesReturned[] ) {
		componentLookupRuleTableCommon(componentName, FUNCTION_LOOKUP_RULE_TABLE, numRuleTable, parmNamesRequired, parmNamesReturned);
	}
    
	
	/**
	 * <h3>Sets the modality of the lookup rule table selection function activation. </h3>
	 * <p>
 	 * The lookup function that realizes the selection of a rule table item needs to code parameters<br>
 	 * required and returned by means of PARM_REQUIRED() and PARM_RETURNED() declarations.<br>
 	 * Actually it needs the numTable and language and returns the item key and the description.<br>
 	 * <p>
 	 * Required NumTable and language are automatically supplied to the function by forward monitor.<br>
 	 * Informations for returned parameters are described by the input parameter,  an array of strings <br>
 	 * where each element contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter name required by called application, defined in the called function by means of a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function<br>
	 * and, after the item selection, all parameters returned by called function, in the caller function
	 * <p>
	 * The lookup function used will be an application user supplied function.<br>
	 * <p>
	 * If the component has been not defined in any panel, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * @param parmNamesRequired as a string array with two token for each element, the first token as the variable or GUI component name<br>
	 * in the caller function with the item key, the second with the parameter name rquired by called lookup function. 
	 * @param parmNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the parameter name returned by called lookup function. 
	 * @param lookUpFunction the user suplied rule table lookup function
	 */
	public void COMPONENT_LOOKUP_RULE_TABLE(String componentName, String lookUpFunction, int numRuleTable, String parmNamesRequired[], String parmNamesReturned[] ) {
		componentLookupRuleTableCommon(componentName, lookUpFunction, numRuleTable, parmNamesRequired, parmNamesReturned);
	}
	

	/**
	 * <h3>Sets the modality of a generic lookup function activation. </h3>
	 * <p>
  	 * The lookup function needs to code parameters required and returned by means of PARM_REQUIRED() <br>
  	 * and PARM_RETURNED() declarations.<br>
  	 * <p>
 	 * Required parameters are automatically supplied to the function by forward monitor.<br>
	 * Returned parameters are automatically set by forward monitor in the caller function.<br>
 	 * Informations for required and returned parameters are described by input parameter, two array of strings <br>
 	 * where each element contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter name required or returned by called application, defined in the called function 
	 * by means of a <code>PARM_REQUIRED()</code> or a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function<br>
	 * and, after returning to caller function, all parameters returned by called function.
	 * <p>
	 * If the component has been not defined in any panel, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * @param parmNamesRequired as a string array with two token for each element, the first token as the variable or GUI component name<br>
	 * in the caller function with the item key, the second with the parameter name rquired by called lookup function. 
	 * @param parmNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the parameter name returned by called lookup function. 
	 */
	public void COMPONENT_LOOKUP_FUNCTION(String componentName, String lookupFunction, String parmNamesRequired[], String parmNamesReturned[] ) {
		componentLookupRuleTableCommon(componentName, lookupFunction, -1, parmNamesRequired, parmNamesReturned);
	}

	/*
	 * Parte comune accesso per lookup a rule table e a function
	 */
	private void componentLookupRuleTableCommon(String componentName, String lookUpFunction, int numRuleTable, String parmNamesRequired[], String parmNamesReturned[] ) {
		
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		innerComponent = getComponentDeclared(componentName);
		if (innerComponent == null) {return;}						// Nessun controllo GUI con questo nome
		
		// Descrittore componente nel pannello
		panelComponent = getPanelComponent(componentName);
		panelComponent.setLookupTableNum(numRuleTable);
		panelComponent.setLookupFunction(lookUpFunction);
		
		// Parametri richiesti
		for (String parmNameRequired : parmNamesRequired) {
			panelComponent.getLookupFunctionParmsRequired().add(parmNameRequired);
		}
		
		// Parametri restituiti
		for (String parmNameReturned : parmNamesReturned) {
			panelComponent.getLookupFunctionParmsReturned().add(parmNameReturned);
		}
	}

	
	/**
	 * <h3>Sets the logical data view name linked to the component to be executed. </h3>
	 * <p>
	 * When for a component has been set COMPONENT_CTRL_EXISTS_LDV_TODO or COMPONENT_CTRL_UNEXISTS_LDV_TODO<br>
	 * options, the PANEL_CONTROLS or the PANEL_CONTROLS_EXISTENCE_ENTITY action executes the logical data view<br>
	 * indicated by this declaration.<br>
	 * <p>
	 * The purpose is to control the existence or not existence in a legacy table and, optionally, to set<br>
	 * function application variables or GUI controls, with column values obtained by the logical data view.<br>
	 * <p>
  	 * To run properly, the logical data view needs key colum values to be set with the content of function<br>
  	 * variables or function GUI controls, as described by <code>columnNamesKeyRequired</code> parameter.<br>
  	 * <p>
 	 * So, required parameters are automatically supplied to the logical data view by forward monitor.<br>
	 * Returned logical data view columns are automatically used by forward monitor to set  function<br>
  	 * variables or function GUI controls.<br>
  	 * <br>
 	 * Informations for required parameters and returned columns are described by input parameter, two array of strings <br>
 	 * where each element contains two substrings:<br>
	 * The first is the component name in the function, as a variable or GUI control name.<br>
	 * The second is the column name required or returned by the logical data view.<br>
	 * <p>
	 * The forward monitor, automatically will set all key columns required by the logical data view with data of caller function<br>
	 * and, after the logical data view execution, all function data with columns values of the logical data view.
	 * <p>
	 * If the compment name specified is not a valid variable name or a GUI control name, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * @param ldv the logical data view name to be executed
	 * @param columnNamesKeyRequired as a string array with two token for each element, the first token as the variable or GUI component name<br>
	 * in the function, the second with the key column name required by the logical data view to be properly executed. 
	 * @param columnNamesReturned as a string array with two token for each element, the first token as the variable or GUI component name
	 * in the caller function, the second with the column name to be used to set the function component. 
	 */
	public void COMPONENT_LDV_TO_EXEC(String componentName, String ldv, String columnNamesKeyRequired[], String columnNamesReturned[] ) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		innerComponent = getComponentDeclared(componentName);
		if (innerComponent == null) {return;}						// Nessun controllo GUI con questo nome
		
		// Descrittore componente nel pannello
		panelComponent = getPanelComponent(innerComponent.panelName);
		panelComponent.setLdv(ldv);
		
		// Colonne chiave
		for (String columnKeyRequired : columnNamesKeyRequired) {
			panelComponent.getLdvColumnsKey().add(columnKeyRequired);
		}
		
		// Colonne restituite
		for (String columnNameReturned : columnNamesReturned) {
			panelComponent.getLdvColumnsReturned().add(columnNameReturned);
		}
	}
  
	/**
	 * <h3>Sets a component option status </h3>
	 * <p>
	 * All components controls are piloted by options defined by {@link EnumForwardOption} enumeration,
	 * with the name starting by <code>COMPONENT</code> as, for example:<pre>
	COMPONENT_GOOD_WITH_ERRORS 
	COMPONENT_HIDDEN 
	COMPONENT_DISABLED 
	COMPONENT_AUTOSKIP 
	COMPONENT_HILIGHT 
	COMPONENT_HELP_TECH_ACTIVE 
	COMPONENT_HELP_USER_ACTIVE </pre>
	 * <p>
	 * If the component has been not defined in any panel, no action will be taken.
	 * <br>
	 * @param componentName as the GUI component name to enable for the lookup rule table selection function activation
	 * @param option as a EnumForwardOption enumeration
	 * @param isActive as a boolean value, true to make the option active
	 */
	public void COMPONENT_OPTION(String componentName, EnumForwardOption option, boolean isActive) {
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		innerComponent = getComponentDeclared(componentName);
		if (innerComponent == null) {return;}						// Nessun controllo GUI con questo nome
		
		// Descrittore componente nel pannello
		panelComponent = getPanelComponent(innerComponent.panelName);
		panelComponent.getOptionsMap().put(option, isActive);
	}
  
	/**
	 * <h3>Sets a control to be executed on a panel component</h3>
	 * <p>
	 * This declaration causes the forward monitor to execute the input control type,<br>
	 * during controls at the panel level.<br>
	 * <br>
	 * The type of objects depends on the type of control to be executed and on the type of GUI component.<br>
	 * <p>
	 * Following control types are without obects control parameters specification:<pre>
	COMPONENT_CTRL_EXISTS_LDV 
	COMPONENT_CTRL_UNEXISTS_LDV 
	COMPONENT_CTRL_MANDATORY 
	COMPONENT_CTRL_TEXT_UPPERCASE 
	COMPONENT_CTRL_TEXT_LOWERCASE 
	COMPONENT_CTRL_TEXT_ALPHABETIC 
	COMPONENT_CTRL_TEXT_NUMERIC
	COMPONENT_CTRL_DATE</pre>
	 * <p>
	 * When COMPONENT_CTRL_DATE has been specified it must be coded a COMPONENT_CTRL_MASK declarative too, <br>
	 * with a valid date mask as yyyMMdd, dd-MM-yyy etc.<br>
	 * <p>
	 * Following control types must be coded with a single numeric object parameter:<pre>
	COMPONENT_CTRL_EXISTS_TABLE 
	COMPONENT_CTRL_UNEXISTS_TABLE 
	COMPONENT_CTRL_TEXT_SIZE_MAX			 
	COMPONENT_CTRL_TEXT_SIZE_MIN			 
	COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX	 
	COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN	 
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX	 
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN	 
	COMPONENT_CTRL_NUMERIC_VALUE_MAX		 
	COMPONENT_CTRL_NUMERIC_VALUE_MIN</pre>
	 * <p>
	 * Following control types must be coded with a single text object parameter:<pre>
	COMPONENT_CTRL_MASK 
	COMPONENT_CTRL_TEXT_VALUE_MAX 
	COMPONENT_CTRL_TEXT_VALUE_MIN  
	COMPONENT_CTRL_DATE_MAX 
	COMPONENT_CTRL_DATE_MIN </pre>
	 * <p>
	 * When COMPONENT_CTRL_DATE_MAX or COMPONENT_CTRL_DATE_MIN has been specified it must be coded a COMPONENT_CTRL_MASK declarative too, <br>
	 * Valid  characters masks for Date are supported by standard java {@link DateFormat} and are, for example:<pre>
yyyy
MM
MMM
dd
	 * 
	 * So, for a complete could be yyyMMdd, dd-MM-yyy etc.</pre>
	 * <p>
	 * Valid mask characters formatted field not date are all values supported by standard java {@link MaskFormatter}:<pre>
# 	Any valid number, uses Character.isDigit .
' 	Escape character, used to escape any of the special formatting characters.
U 	Any character ( Character.isLetter ). All lowercase letters are mapped to upper case.
L 	Any character ( Character.isLetter ). All upper case letters are mapped to lower case.
A 	Any character or number ( Character.isLetter or Character.isDigit )
? 	Any character ( Character.isLetter ).
* 	Anything.
H 	Any hex character (0-9, a-f or A-F).

So, for a complex phone number entry field, you could use a mask like: "(###) ###-####"
and for a field with decimal could be ##.0 </pre>
	 * 
	 * Following control types must be coded with two text object parameter:<pre>
	COMPONENT_CTRL_TEXT_VALUE_RANGE  
	COMPONENT_CTRL_DATE_RANGE</pre>
	 * <p>
	 * When COMPONENT_CTRL_DATE_RANGE has been specified it must be coded a COMPONENT_CTRL_MASK declarative too, <br>
	 * with a valid date mask as yyyMMdd, dd-MM-yyy etc.<br>
	 * <p>
	 * Following control types must be coded with two numeric integer object parameter:<pre>
	COMPONENT_CTRL_TEXT_SIZE_RANGE
	COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE 
	COMPONENT_CTRL_NUMERIC_VALUE_DEC_RANGE </pre>
	<p>
	 * Following control types must be coded with two numeric object parameter:<pre>
	COMPONENT_CTRL_NUMERIC_VALUE_RANGE</pre>
	<p>
	 * Following control types must be coded with an unlimited number of object parameter:<pre>
	COMPONENT_CTRL_VALUES</pre>
	<p>
	 * @param componentName as the GUI component name to be controlled
	 * @param ctrlTypeToDo as an {@link EnumForwardOption} enumeration starting with <code>COMPONENT_CTRL</code>
	 * @param ctrlParms as an Object array with the content depending on ctrlTypeToDo
	 */
	public void COMPONENT_CTRL(String componentName , EnumForwardOption ctrlTypeToDo, Object ... ctrlParms) {
		
		ForwardPanelComponent panelComponent = null;
		InnerComponent innerComponent = null;
		innerComponent = getComponentDeclared(componentName);
		Object ctrlParm1 = null;
		Object ctrlParm2 = null;
		if (innerComponent == null) {return;}						// Nessun componente con questo nome
		
		// Descrittore componente nel pannello
		panelComponent = getPanelComponent(componentName);
		if (panelComponent == null) {
			return;
		}
		// Nessun parametro necessario
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_EXISTS_ENTITY	 			
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_UNEXISTS_ENTITY		 					
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_MANDATORY		 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_UPPERCASE		 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_LOWERCASE		 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_ALPHABETIC		 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_NUMERIC
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE) {
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}
		
		// Permesso un numero illimitato di parametri
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_VALUES) {
			for (Object value : ctrlParms) {
				panelComponent.getCtrlValues().add(value);
			}
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}
		
		// Permesso un solo parametro text
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_MASK
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MAX		
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MIN
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE_MAX
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE_MIN) {
			if (ctrlParms == null || ctrlParms.length == 0) {return;}
			ctrlParm1 = ctrlParms[0];
			if (ctrlParm1.getClass() != String.class) {return;}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_MASK) 					{panelComponent.setCtrlMask((String) ctrlParm1);
			                                                                             componentCtrlMask(panelComponent, innerComponent, (String) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MAX) 		{panelComponent.setCtrlValueMax((String) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_MIN) 		{panelComponent.setCtrlValueMin((String) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE_MAX) 				{panelComponent.setCtrlValueMax((String) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE_MIN) 				{panelComponent.setCtrlValueMin((String) ctrlParm1);}
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}
		
		// Permesso due parametri text
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_VALUE_RANGE
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_DATE_RANGE) {
			if (ctrlParms == null || ctrlParms.length < 2) {return;}
			ctrlParm1 = ctrlParms[0];
			ctrlParm2 = ctrlParms[1];
			if (ctrlParm1.getClass() != String.class || ctrlParm2.getClass() != String.class) {return;}
			panelComponent.setCtrlValueMin((String) ctrlParm1);
			panelComponent.setCtrlValueMax((String) ctrlParm2);
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}

		// Permesso un solo parametro numerico
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE	
		||	ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MAX
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MIN		
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN 
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) {
			if (ctrlParms == null || ctrlParms.length == 0) {return;}
			ctrlParm1 = ctrlParms[0];
			if (ctrlParm1.getClass() != Integer.class && ctrlParm1.getClass() != int.class && ctrlParm1.getClass() != double.class) {return;}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE) 	{panelComponent.setCtrlTableNum   ((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_UNEXISTS_RULE_TABLE) 	{panelComponent.setCtrlTableNum   ((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MAX) 		{panelComponent.setCtrlTextSizeMax((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_MIN) 		{panelComponent.setCtrlTextSizeMin((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MAX) {panelComponent.setCtrlValueIntMax((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_MIN) {panelComponent.setCtrlValueIntMin((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MAX) {panelComponent.setCtrlValueDecMax((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_MIN) {panelComponent.setCtrlValueDecMin((Integer) ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MAX) 	{panelComponent.setCtrlValueMax(ctrlParm1);}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_MIN) 	{panelComponent.setCtrlValueMin(ctrlParm1);}
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}
		
		// Permesso due parametri numerici integer
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_RANGE
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE
		||  ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_RANGE) {
			if (ctrlParms == null || ctrlParms.length < 2) {return;}
			ctrlParm1 = ctrlParms[0];
			ctrlParm2 = ctrlParms[1];
			if (ctrlParm1.getClass() != int.class || ctrlParm2.getClass() != int.class) {return;}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_TEXT_SIZE_RANGE) {
				panelComponent.setCtrlTextSizeMin((Integer) ctrlParm1);
				panelComponent.setCtrlTextSizeMax((Integer) ctrlParm2);
				panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
				return;
			}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_INT_RANGE) {
				panelComponent.setCtrlValueIntMin((Integer) ctrlParm1);
				panelComponent.setCtrlValueIntMax((Integer) ctrlParm2);
				panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
				return;
			}
			if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_DEC_RANGE) {
				panelComponent.setCtrlValueDecMin((Integer) ctrlParm1);
				panelComponent.setCtrlValueDecMax((Integer) ctrlParm2);
				panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
				return;
			}
			return;
		}
		
		// Permesso due parametri numerici integer, double, float
		if (ctrlTypeToDo == EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_RANGE) {
			if (ctrlParms == null || ctrlParms.length < 2) {return;}
			ctrlParm1 = ctrlParms[0];
			ctrlParm2 = ctrlParms[1];
			if ((ctrlParm1.getClass() != int.class && ctrlParm1.getClass() != Integer.class && ctrlParm1.getClass() != double.class && ctrlParm1.getClass() != Double.class)
			||  (ctrlParm1.getClass() != int.class && ctrlParm1.getClass() != Integer.class && ctrlParm1.getClass() != double.class && ctrlParm1.getClass() != Double.class)) {
				return;
			}
			panelComponent.setCtrlValueMin(ctrlParm1);
			panelComponent.setCtrlValueMax(ctrlParm2);
			panelComponent.getOptionsMap().put(ctrlTypeToDo, true);
			return;
		}
	}
  
	/*
	 * Impostazione mask su JFormattedTextField
	 */
	private void componentCtrlMask(ForwardPanelComponent panelComponent, InnerComponent innerComponent, String ctrlParm1) {
	    MaskFormatter mask = null;
	    DateFormat dateFormat = null;
	    NumberFormat numberFormat = null;
	    DecimalFormat decimalFormat = null;
	    DateFormatter df = null;
	    NumberFormatter numberFormatter = null;
	 
		if (innerComponent.componentType != EnumForwardComponent.JFormattedTextField) {return;}
		
		// Formattazione editing per stringhe
		if (innerComponent.componentObjectClass == String.class) {
            try {
				mask = new MaskFormatter(panelComponent.getCtrlMask());
		        mask.setPlaceholderCharacter('_');
//		        mask.setValidCharacters("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
		        DefaultFormatterFactory factory = new DefaultFormatterFactory(mask);
		        ((JFormattedTextField) panelComponent.getGraphicObject()).setFormatterFactory(factory);
			} catch (ParseException e) {
				// Wrong mask, no operations
			}
            return;
 		}
		
		// Formattazione editing per date
		if (innerComponent.componentObjectClass == String.class) {
			dateFormat = new SimpleDateFormat(panelComponent.getCtrlMask());
			df = new DateFormatter(dateFormat);
			DefaultFormatterFactory factory = new DefaultFormatterFactory(df);
			((JFormattedTextField) panelComponent.getGraphicObject()).setFormatterFactory(factory);
			return;
 		}
		
		// Formattazione editing per valori numerici interi con eliminazione automatica di . e cifre dopo la virgola
		if (innerComponent.componentObjectClass == Integer.class
		||  innerComponent.componentObjectClass == Long.class) {
			numberFormat = NumberFormat.getIntegerInstance();
			numberFormatter = new NumberFormatter(numberFormat);
			DefaultFormatterFactory factory = new DefaultFormatterFactory(numberFormatter);
			((JFormattedTextField) panelComponent.getGraphicObject()).setFormatterFactory(factory);
		}
		
		// Formattazione editing per valori numerici con virgola
		if (innerComponent.componentObjectClass == Double.class
		||  innerComponent.componentObjectClass == Float.class) {
			decimalFormat = new DecimalFormat(panelComponent.getCtrlMask());
			numberFormatter = new NumberFormatter(decimalFormat);
			DefaultFormatterFactory factory = new DefaultFormatterFactory(numberFormatter);
			((JFormattedTextField) panelComponent.getGraphicObject()).setFormatterFactory(factory);
		}
		
		
	}

	/**
	 * <h4>Begin layout tuning</h4>
	 * There are two types of layout tuning:
	 * <ul>
	 * <li> On own specific forward data, function depending
	 * <li> Directly on bnative java graphic objects parameters
	 * </ul>
	 * All panel declared and all <code>CONTROL</code>  in the section <code>PANEL_CONTENT</code> let to make running a<br>
	 * minimal graphic interface declaring just a few parameters and using default values for any else.<br>
	 * Any further parameter for the control can be set in the section <code>CONTROLS_ADJUSTMENT</code>.<br>
	 * <p>
	 * In this section it is possible set all <b>native</b> parameters directly on the java object object.<br>
	 * The java object it's a {@link JComponent} object {@link JPanel}, {@link JLabel} and so on.<br>
	 * It's available a generic method to get any component <code>getJComponent()</code> but the caller must cast it.<br>
	 * And then it's possible to use specialized methods to get panel and controls.<br>
	 * <p>
	 * Forward monitor will execute this section to complete the GUI definition.<br>
	 * Here is an example of how code a different minimunSize and toolTip for a {@link JLabel} control:
	 * <pre>
	 *   getJPanel("Panel1").setSize(300, 500)						// Java method
	 *   getJLabel("Panel1", "LAB01").setMinimunSize(150, 70)				// Java method
	 *   getJLabel("Panel1", "LAB01").setToolTipText("This is a label")			// Java method
	 * </pre>
	 * No operations are required for about getting instance of object control, casting operations and so on.<br>
	 * It's supported a version without the panel specification, because all names in forward are uniques.<br>
	 * <pre>
	 *   getJLabel("LAB01").setToolTipText("This is a label")
	 * </pre>
	 * <br>
	 * This way to realize the GUI interface let to take advantage of all features of java with the minimun<br>
	 * coding effort.<br>
	 * And then the reference even for the monitor of forward running on the server, is the java graphic object,<br>
	 * the natural and exaustive store of all informations.<br> 
	 * <br>
	 * <p>
	 * Forward populates automatically the panel through the execution of the
	 * strategy data access and setting all values of panel controls.<br>
	 * <p>
	 * The populating process sets the correct value for choice field, like<br>
	 * check box o option goup like group button.<br>
	 * <p>
	 * Only controls with the same name are set. The name must be the same name <br>
	 * defined in the centralyzed dictionary as a <code>metaField</code>.
	 * The name is the same for the bean class used to manage a single database<br>
	 * entity.<br>
	 * <p>
	 */
	public void BEGIN_LAYOUT_TUNING() {
		this.activeMenuName = "";
	}

	/**
	 * <h3>End layout tuning</h3>
	 * It's just a marker for the on of section<br>
	 * <p>
	 */
	public void END_LAYOUT_TUNING() {
	}
	
	

	/**
	 * <h4>Begin events behaviour declaration</h4>
	 * Starts the section declaring all behaviours of the application to events<br>
	 * and all events are declared using the <code>ON_EVENT</code> section<br>
	 * <p>
	 * It's necessary to declare a <code>ON_EVENT</code> section for each event.<br>
	 * Each event declared describes a own action to be executed when the event occurred.<br>
	 * This kind of actions are available without coding by Forward and are enaugh in the most<br>
	 * common cases. They are standard actions in forward applications<br>
	 * <p>
	 * However it's possible to gain the full control of events by coding a own event management<br>
	 * in the application method declared in the <code>ON_EVENT_OTHER</code> section.<br>
	 * Such a method is a generic reusable application method able to know and modify every bit of <br>
	 * the application currently running.<br>
	 * The method declared by <code>ON_EVENT_OTHER</code> is executed by the forward monitor when no<br>
	 * event occurred has been trapped and honorated by any ON_EVENT declared.<br>
	 * <p>
	 * The events are all standard events defined for java Swing application like those related to <br>
	 * <code>ActionListener</code> as <code>ActionPerformed</code> or <code>FocusListener</code> and so on.<br>
	 * 
	 */
	public void BEGIN_EVENTS() {
		this.activeMenuName = "";
		
	}


	/**
	 * <h4>Begin groups of actions declaration</h4>
	 * Starts the section declaring all groups of actions identified by a group name.<br>
	 * <p>
	 * A group of actions is conceptually the same thing of a method or a routine.<br>
	 * The difference is that actions declared in the group are not programming instructions<br>
	 * but model declaring statement.<br>
	 */
	public void BEGIN_ACTIONS_GROUPS() {
		this.activeMenuName = "";
	}


	/**
	 * <h4>End event behaviour declaration</h4>
	 * It's simply a marker to make clear the declaration.
	 */
	public void END_EVENTS() {}


	/**
	 * <h4>Ends groups of actions declaration</h4>
	 * It's only a marker with no effects.<br>
	 */
	public void END_ACTIONS_GROUPS() {}

	/**
	 * <h4>Declare a group containing one or more action declarations</h4>
	 * <br>
	 * <p>
	 * @param event the type event to detect as coded by forward
	 * @param componentName origin of the event for wichh the action must be executed
	 * @param actionCoded the action to take parameters to execute when event occurs
	 */
	public void ACTIONS_GROUP(String groupName, ForwardDoParms ... actionsCoded) {
		
		InnerActionsGroup innerActionsGroup = null;
		
		innerActionsGroup = new InnerActionsGroup();
		innerActionsGroup.groupName = groupName;
		
		// Accodamento azioni da effettuare all'evento
		for (ForwardDoParms actionCoded : actionsCoded) {
			innerActionsGroup.al_actionCoded.add(actionCoded);
		}
		
		// Gruppo di action in struttura hash
		this.hm_groupActions.put(groupName, innerActionsGroup);

	}
 


	/**
	 * <h4>Panel options</h4>
	 * This section declares options specific for the a declared panel.<br>
	 * <p>
	 * They are applicative and not technichal or interface related options.<br>
	 * An option for a <code>PANEL_GRID</code> panel could be for example <code>PANEL_ALLOWED_TYPING</code><br>
	 * and so on.<br>
	 * <p>
	 * @param String panelName
	 * @param String panelTitle
	 * @param boolean borderDefault
	 * @param EnumForwardPanelOption[] panelOption
	 */
	public void PANEL_OPTIONS(String panelName, EnumForwardOption ... ar_panelOption) {
		ForwardPanel panel = null;
		
		panel = this.hm_panel.get(panelName);
		
		// Improperly section declared
		if (panel == null) {return;}
		if (this.activePanel == null) {return;}
		
		// Scan options input
		for (EnumForwardOption panelOption : ar_panelOption) {
			panel.getOptions().add(panelOption);
		}
	}

	/**
	 * <h4>Declare a column of a PANEL_GRID</h4>
	 * A panel grid owns and manages a table implemented by a {@link JTable} Swing component.<br>
	 * A table is always defined through a specialized panel that manages only a table, just a <code>PANEL_GRID</code>,<br>
	 * declaring the logical data view used to automatically populate the panel with database data too.<br>
	 * To list table columns and to bind them with the column names declared by logical data view, its necessary, for each<br>
	 * column in the table, to detail this information using a <code>PANEL_GRID_COLUMN</code> declaration.<br>
	 */
	public void PANEL_GRID_COLUMN(String panelName, String columnName, String columnHeader, String columnNameLdv, String columnToolTipText, Class<?> columnsRenderType) {
		ForwardPanel panel = null;
		panel = this.hm_panel.get(panelName);
		// Improperly section declared
		if (panel == null) {return;}
		if (this.activePanel == null) {return;}
		
		// Append column declaration
		panel.addGridColumn(columnName, columnHeader, columnNameLdv, columnToolTipText, columnsRenderType);
	}


	/**
	 * <h4>Event declaration</h4>
	 * <br>
	 * Furthermore an event can be declared for a not swing object like a {@link Timer}.<br>
	 * In this case the event declared will be registered but will be updated the internal structure with<br>
	 * java object too, assigning the application name to the object.<br>
	 * <p>
	 * @param event the type event to detect as coded by forward
	 * @param componentName origin of the event for wichh the action must be executed
	 * @param actionCoded the action to take parameters to execute when event occurs
	 */
	public void ON_EVENT(EnumForwardEvent event, ForwardDoParms actionCoded) {
		
		InnerOnEvent innerOnEvent = null;
		innerOnEvent = new InnerOnEvent();
		
		innerOnEvent.event = event;
		innerOnEvent.componentName = "";
		ForwardDoParms parmsAction = actionCoded;
		innerOnEvent.action = parmsAction.action;
		innerOnEvent.al_actionCoded.add(actionCoded); 		// Accodamento azioni da effettuare all'evento
		
		// Evento in struttura seqiuenziale e hash
		this.al_onEvent.add(innerOnEvent);  				// Accodamento evento
		this.hm_onEvent.put(innerOnEvent.event.ordinal() + ":" + innerOnEvent.componentName, innerOnEvent);
	}


	/**
	 * <h4>Event declaration</h4>
	 * <br>
	 * The action to take when an event occurs is just declared in asyncronous mode.<br>
	 * In this case the event declared will be registered but will be updated the internal structure with<br>
	 * java object too, assigning the application name to the object.<br>
	 * <p>
	 * @param event the type event to detect as coded by forward
	 * @param componentName origin of the event for wichh the action must be executed
	 * @param actionCoded the action to take parameters to execute when event occurs
	 */
	public void ON_EVENT(EnumForwardEvent event, String componentName, ForwardDoParms ... actionsCoded) {
		
		InnerOnEvent innerOnEvent = null;
		
		innerOnEvent = new InnerOnEvent();
		innerOnEvent.event = event;
		innerOnEvent.componentName = componentName;
		
		// Accodamento azioni da effettuare all'evento
		for (ForwardDoParms actionCoded : actionsCoded) {
			innerOnEvent.al_actionCoded.add(actionCoded);
			innerOnEvent.action = actionCoded.action;
		}
		
		// Evento in struttura seqiuenziale e hash
		this.al_onEvent.add(innerOnEvent);  				// Accodamento evento
		this.hm_onEvent.put(innerOnEvent.event.ordinal() + ":" + innerOnEvent.componentName, innerOnEvent);

	}
 
	
	/**
	 * <h4>Component property default declaration</h4>
	 * Declare a default property to be applied to all Swing {@link JComponent} of specific type.<br>
	 * <p>
	 * A component could be a {@link JPanel} implemented by the <code>PANEL_STRUCTURE</code> section or a<br>
	 * {@link JButton}, implemented by the <code>CONTROL</code> section and so on.<br>
	 * Forward model function declaring uses all java swing properties at the definition time,<br>
	 * with no wrapper, no classes specialized and so on. It use just native java Swing controls,<br>
	 * to carry on all properties using native java structure.<br>
	 * All forward classes let just to describe the application structure and additional applicative data.<br>  
	 * <br>
	 * So, to make the function light and generalized, and to avoid to redefines standard java swing properties<br>
	 * inside forward, all not default properties can be declared just specifying the name of component name, property <br>
	 * and value.<br>
	 * The property name must be specified beginning with capital letter.<br>
	 * <br>
	 * The forward monitor will use java reflection to set properties declared dynamically letting a chance<br>
	 * to runtime tuning and personalizations.<br>
	 * <p>
 	 * 
	 * 
	 * @param componentType the EnumForwardComponent constant
	 * @param propertyName
	 * @param propertyValueClass 
	 * @param propertyValue 
	 */
	@SuppressWarnings("rawtypes")
	public void COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent componentType, String propertyName, Class propertyValueClass, Object propertyValue) {
		ArrayList<InnerComponentProperty> al_propertyDefault = null;
		InnerComponentProperty innerComponentProperty = null;
		
		// Lettura o creazione entry
		al_propertyDefault = this.hm_propertyDefault.get(componentType);
		if (al_propertyDefault == null) {
			al_propertyDefault = new ArrayList<InnerComponentProperty> ();
			this.hm_propertyDefault.put(componentType, al_propertyDefault);
		}
		
		// Append proprietà di default
		innerComponentProperty = new InnerComponentProperty ();
		innerComponentProperty.componentType = componentType;
		innerComponentProperty.propertyName = propertyName;
		innerComponentProperty.propertyValue = propertyValue;
		innerComponentProperty.propertyValueClass = propertyValueClass;
		al_propertyDefault.add(innerComponentProperty);
	}

	
	
	/**
	 * <h4>Component property declaration</h4>
	 * Declare a property of a Swing {@link JComponent} declared by the function.<br>
	 * <p>
	 * A component could be a {@link JPanel} implemented by the <code>PANEL_STRUCTURE</code> section or a<br>
	 * {@link JButton}, implemented by the <code>CONTROL</code> section and so on.<br>
	 * Forward model function declaring uses all java swing properties at the definition time,<br>
	 * with no wrapper, no classes specialized and so on. It use just native java Swing controls,<br>
	 * to carry on all properties using native java structure.<br>
	 * All forward classes let just to describe the application structure and additional applicative data.<br>  
	 * <br>
	 * So, to make the function light and generalized, and to avoid to redefines standard java swing properties<br>
	 * inside forward, all not default properties can be declared just specifying the name of component name, property <br>
	 * and value.<br>
	 * The property name must be specified beginning with capital letter.<br>
	 * <br>
	 * The forward monitor will use java reflection to set properties declared dynamically letting a chance<br>
	 * to runtime tuning and personalizations.<br>
	 * <p>
 	 * 
	 * 
	 * @param componentName
	 * @param propertyName
	 * @param propertyValue 
	 */
	public void COMPONENT_PROPERTY(String componentName, String propertyName, Object propertyValue) {
		InnerComponent innerComponent = null;
		InnerComponentProperty innerComponentProperty = null;
		
		// Lettura o creazione entry
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {
			innerComponent = new InnerComponent();
			innerComponent.componentName = componentName;
			this.hm_component.put(componentName, innerComponent);
		}
		
		// Append proprietà
		innerComponentProperty = new InnerComponentProperty ();
		innerComponentProperty.componentName = componentName;
		innerComponentProperty.propertyName = propertyName;
		innerComponentProperty.propertyValue = propertyValue;
		innerComponent.al_property.add(innerComponentProperty);
	}

	
	
	
	/**
	 * <h4>Function parameter required to function declaration</h4>
	 * Declare a parameter name required for the correct running of the function<br>
	 * when the function is started by another function.<br>
	 * <p>
	 * A parameter name is the name assigned to a fuction variable declared with the same name.<br> 
	 * The variable will be automatically created as it was explicitely created by VAR() declaration.<br>
	 * <br>
	 * A parameter could be, for example a customer code, to be assigned to a GUI control as default,<br>
	 * or to access data using a logical data view.<br>
	 * <p>
	 * @param parmNameRequired 
	 * @param parmInitial the parameter object with initial value
	 */
	public void PARM_REQUIRED(String parmNameRequired, Object parmInitial) {
		VAR(EnumForwardScope.SCOPE_FUNCTION, parmNameRequired, parmInitial);
		this.al_parmRequired.add(parmNameRequired);
		return;
	}
	 
	/**
	 * <h4>Function parameter returned by function declaration</h4>
	 * Declare a single parameter returned by the current to the caller function.<br>
	 * <p>
	 * When a function is started by another function, often the called function returns a value<br>
	 * or a set of values.<br> 
	 * For example a called function called to select a customer from a list, could return back<br>
	 * to caller a customer code and other select data<br>
	 * <br>
	 * But a called function could be return back to caller a structured set o data too. Thus it can<br>
	 * return back just an object of an applicative class, to have the maximum in terms of flexibility.<br>
	 * <p>
	 * @param parmNameReturned 
	 * @param parmInitial the parameter object with initial value
	 */
	public void PARM_RETURNED(String parmNameReturned, Object parmInitial) {
		VAR(EnumForwardScope.SCOPE_FUNCTION, parmNameReturned, parmInitial);
		this.al_parmReturned.add(parmNameReturned);
		return;
	}

	/**
	 * <h4>Variable declaration</h4>
	 * Declare a variable for the current function with the scope function.<br>
	 * The variable will be allocated at execution time using the initial value<br>
	 * <p>
	 * The type of variable, through its class, is set by input initial value, mandatory.<br>
	 * <p>
	 * Notice that variables for each table columns are automatically created<br>
	 * at table definition time.<br>
	 * <p>
	 * @param varName as the name of variable
	 * @param varInitial as an Object value the initial value of variable
	 */
	public void VAR(String varName, Object varInitial) {
		VAR(EnumForwardScope.SCOPE_FUNCTION, varName,  varInitial);
	}

	/**
	 * <h4>Variable declaration with scope</h4>
	 * Declare a variable for the current function.<br>
	 * The variable will be allocated at execution time using the initial value<br>
	 * <p>
	 * The type of variable, through its class, is set by input initial value, mandatory.<br>
	 * <p>
	 * Notice that variables for each table columns are automatically created<br>
	 * at table definition time.<br>
	 * <p>
	 * @param varScope as the EnumForwardScope scope of the variable
	 * @param varName as the name of variable
	 * @param varInitial as an Object value the initial value of variable
	 */
	public void VAR(EnumForwardScope varScope, String varName, Object varInitial) {
		InnerComponent innerComponent = null;
		innerComponent = putVarOnStructure(varName, EnumForwardComponent.DeclaredVariable, varInitial);
		innerComponent.varName = varName;
		innerComponent.varType = varInitial.getClass();
		innerComponent.varScope = varScope;
		innerComponent.varInitial = varInitial;
		
		if (varInitial instanceof Boolean) {
			innerComponent.varObject = rm.newInstance(varInitial.getClass(), new Class[]{boolean.class}, new Object[]{((Boolean)varInitial).booleanValue()});
			return;
		}
		if (varInitial instanceof Integer) {
			innerComponent.varObject = rm.newInstance(varInitial.getClass(), new Class[]{int.class}, new Object[]{((Integer)varInitial).intValue()});
			return;
		}
		if (varInitial instanceof Double) {
			innerComponent.varObject = rm.newInstance(varInitial.getClass(), new Class[]{double.class}, new Object[]{((Double)varInitial).doubleValue()});
			return;
		}
		if (varInitial instanceof Float) {
			innerComponent.varObject = rm.newInstance(varInitial.getClass(), new Class[]{float.class}, new Object[]{((Float)varInitial).floatValue()});
			return;
		}
		if (varInitial instanceof Color) {
			innerComponent.varObject = rm.newInstance(varInitial.getClass(), new Class[]{int.class}, new Object[]{( (Color) varInitial).getRGB()});
			return;
		}
		// La variabile ospita un oggetto strutturata, non elementare
		innerComponent.varObject = rm.newInstance(varInitial.getClass(), null, null);
		return;
	}

	 
	
	/**
	 * Gets all properties declared for a component type in the function.<br>
	 * <p>
	 * A component, a graphic swing object, inherits from {@link JComponent} class.<br>
	 * Component properties have been set by the <code>COMPONENT_PROPERTY</code> section in <br>
	 * the function declaration.
	 * <p>
	 * Runtime, after the new instance of a component with the input name, will be apply all properties specified.<br>
	 * If there are not properties for the component specified, an empty array will be returned.<br>
	 * <p>
	 * @param componentName as the component name
	 * @return an array list of properties
	 */
	public ArrayList<InnerComponentProperty> getProperties(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {
			return new ArrayList<InnerComponentProperty> ();
		}
		return innerComponent.al_property;
	}

	/**
	 * Gets the function name declared.<br>
	 * <p>
	 * @return the functionName
	 */
	public String getFunctionName() {
		return functionName;
	}

	/**
	 * Sets the function name.<br>
	 * <p>
	 * @param functionName the functionName to set
	 */
	public void setFunctionName(String functionName) {
		this.functionName = functionName;
	}

	/**
	 * Gets the function identification code.<br>
	 * <p>
	 * When a caller function declare a <code>START_FUNCTION()</code> action,<br>
	 * an identification code is also coded.<br>
	 * When the forward monitor starts a new function due a <code>FUNCTION_START()</code><br>
	 * must be executed, sets the function identifier.<br>
	 * <p>
	 * This function code is used by forward monitor to properly detect the logic to activate<br>
	 * on return from the called function.<br>
	 * <p>
	 * Normally it should be enaugh the function name called for identification purposes but<br>
	 * may be that the same function is started as result of different conditions and so it's
	 * necessary to identify exactly wich.
	 * <p>
	 * @return the functionId
	 */
	public String getFunctionId() {
		return functionId;
	}

	/**
	 * Sets the function identification code.<br>
	 * <p>
	 * When a caller function declare a <code>START_FUNCTION()<(/code> action,<br>
	 * an identification code is also coded.<br>
	 * When the forward monitor starts a new function due a <code>FUNCTION_START()</code><br>
	 * must be executed, sets the function identifier.<br>
	 * <p>
	 * This function code is used by forward monitor to properly detect the logic to activate<br>
	 * on return from the called function.<br>
	 * <p>
	 * Normally it should be enaugh the function name called for identification purposes but<br>
	 * may be that the same function is started as result of different conditions and so it's
	 * necessary to identify exactly wich.
	 * <p>
	 * @param functionId the functionId to set
	 */
	public void setFunctionId(String functionId) {
		this.functionId = functionId;
	}

	/**
	 * Gets the model function.<br>
	 * <p>
	 * @return the model
	 */
	public EnumForwardFunctionModel getFunctionModel() {
		return model;
	}

	/**
	 * Sets the model function.<br>
	 * <p>
	 * @param model the model to set
	 */
	public void setFunctionModel(EnumForwardFunctionModel model) {
		this.model = model;
	}


	
	
	/**
	 * Gets the monitor reference that will execute the function.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @return the monitor
	 */
	public ForwardMonitorDesktop getMonitor() {
		return monitor;
	}

	/**
	 * Sets the monitor reference that will execute the function.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * @param monitor the monitor to set
	 */
	public void setMonitor(ForwardMonitorDesktop monitor) {
		this.monitor = monitor;
	}

	
	/**
	 * Gets the system object with evevent and runtime informations.<br>
	 * <p>
	 * @return the system object
	 */
	public ForwardSystem getSystem() {
		return s;
	}

	/**
	 * Sets the system object with evevent and runtime informations.<br>
	 * <p>
	 * @param s the s to set
	 */
	public void setSystem(ForwardSystem s) {
		this.s = s;
	}


	/**
	 * Gets function names that are enabled to be started from the current function<br>
	 * <p>
	 * @return the ArrayList of callable function names 
	 */
	public String[] getCallableFunctionNames() {
		return this.hm_callable.keySet().toArray(new String[0]);
	}


	/**
	 * Gets all parameters required to the function potentially callable by function.<br>
	 * <p>
	 * It will be returned a string array.<br>
	 * Each element contains two token.<br>
	 * The first token is the variable or GUI control name in the caller function and the second<br>
	 * token is the parameter name to be set in the called function, declared by a PARM_REQUIRED() declaration.<br>
	 * <p>
	 * If the function has been not declared as callable, no action will be taken.<br>
	 * <p>
	 * @return the string  array of required parameters
	 */
	public String[] getCallableFunctionParmsRequired(String functionName) {
		InnerFunctionsCallable innerFunctionsCallable = null;
		innerFunctionsCallable = this.hm_callable.get(functionName);
		if (innerFunctionsCallable == null) {return new String[0];}
		return innerFunctionsCallable.al_parmRequired.toArray(new String[0]);
	}

	/**
	 * Gets all parameters returned by the function.<br>
	 * <p>
	 * It will be returned a string array.<br>
	 * Each element contains two token.<br>
	 * The first token is the variable or GUI control name in the caller function to be set and the second<br>
	 * token is the parameter name  declared by a PARM_RETURNED() declaration in the called function<br>
	 * to be used as input to set in the caller function the variable or GUI control.<br>
	 * <p>
	 * If the function has been not declared as callable, no action will be taken.<br>
	 * <p> 
	 * @return the string  array of returned parameters
	 */
	public String[] getCallableFunctionParmsReturned(String functionName) {
		InnerFunctionsCallable innerFunctionsCallable = null;
		innerFunctionsCallable = this.hm_callable.get(functionName);
		if (innerFunctionsCallable == null) {return new String[0];}
		return innerFunctionsCallable.al_parmReturned.toArray(new String[0]);
	}

	/**
	 * Gets the message coded<br>
	 * <p>
	 * 
	 * Returns the localized message and, if not found, the message code between "??".<br>
	 * <p>
	 * The message key is the message type as a prefix MW, MI, DG, DB, EI, ET, EF, TC followed by a numeric string value of 5 digits size.<br>
	 * There is a forward table for each message type.<br>
	 * Depending on the message type, it will be read a specific messages table.<br>
	 * Every table contains both system messages and application function messsages.<br>
	 * Application function messages start from the code <code>10000</code><br>
	 * <p>
	 * @param messageCode the numeric string, five digits to identify the message
	 * @param messageType a {@link EnumMessageType} enumeration with the type of message
	 * @param messageLanguage a {@link EnumLanguage} enumeration for the message language
	 * 
	 */
	public String getMessage(String messageCode, EnumMessageType messageType, EnumLanguage messageLanguage) {
     	return this.monitor.getMessage(messageCode, messageType, messageLanguage);
	}


	/**
	 * Gets the message coded<br>
	 * <p>
	 * 
	 * Returns the localized message and, if not found, the message code between "??".<br>
	 * <p>
	 * The message key is the message type as a prefix MW, MI, DG, DB, EI, ET, EF, TC followed by a numeric string value of 5 digits size.<br>
	 * There is a forward table for each message type.<br>
	 * Depending on the message type, it will be read a specific messages table.<br>
	 * Every table contains both system messages and application function messsages.<br>
	 * Application function messages start from the code <code>10000</code><br>
	 * <p>
	 * The language is that declared by function.<br>
	 * <p>
	 * @param messageCode the numeric string, five digits to identify the message
	 * @param messageType a {@link EnumMessageType} enumeration with the type of message
	 * 
	 */
	public String getMessage(String messageCode, EnumMessageType messageType) {
     	return this.monitor.getMessage(messageCode, messageType, getLanguage());
	}


	/**
	 * Gets if the function manages multilanguage captions.<br>
	 * <p>
	 * @return a boolean true if multilanguage active
	 */
	public boolean isMultilanguage() {
		return this.isMultiLanguage;
	}

	/**
	 * Sets if the function manages multilanguage captions.<br>
	 * <p>
	 * @return a boolean true if multilanguage active
	 */
	public void setMultilanguage(boolean isMultiLanguage) {
		this.isMultiLanguage = isMultiLanguage;
		return;
	}


	/**
	 * Gets if the function is a lookup function.<br>
	 * <p>
	 * A lookup function is a function automatically started by forward monitor on a GUI control dblClick.<br>
	 * The GUI component must declare a <code>COMPONENT_LOOKUP_FUNCTION</code> or a <code>COMPONENT_LOOKUP_RULE_TABLE</code> statement.<br>
	 * When a lookup function is declared to be activated, the forward monitor will automatically manage all issues related to the<br>
	 * parameters required and returned setting.<br>
	 * <p>
	 * 
	 * @return the isLookupFunction
	 */
	public boolean isFunctionLookup() {
		return isLookupFunction;
	}

	/**
	 * Sets if the function is a lookup function.<br>
	 * <p>
	 * A lookup function is a function automatically started by forward monitor on a GUI control dblClick.<br>
	 * The GUI component must declare a <code>COMPONENT_LOOKUP_FUNCTION</code> or a <code>COMPONENT_LOOKUP_RULE_TABLE</code> statement.<br>
	 * When a lookup function is declared to be activated, the forward monitor will automatically manage all issues related to the<br>
	 * parameters required and returned setting.<br>
	 * <p>
	 * @param isLookupFunction the isLookupFunction to set
	 */
	public void setFunctionLookup(boolean isLookupFunction) {
		this.isLookupFunction = isLookupFunction;
	}

	/**
	 * Get if the function, a lookup function, has been started with the automatic<br>
	 * setting of required and returned parameters.<br>
	 * In This case a GUI component has been declared as COMPONENT_LOOKUP_FUNCTION() or COMPONENT_LOOKUP_RULE_TABLE()<br>
	 * and the function has been started by a double click on the GUI component.<br>
	 * <p>
	 * @return the isLookupFunctionAutomaticParms
	 */
	public boolean isLookupFunctionAutomaticParms() {
		return isLookupFunctionAutomaticParms;
	}

	/**
	 * Get if the function, a lookup function, has been started with the automatic<br>
	 * setting of required and returned parameters.<br>
	 * In This case a GUI component has been declared as COMPONENT_LOOKUP_FUNCTION() or COMPONENT_LOOKUP_RULE_TABLE()<br>
	 * and the function has been started by a double click on the GUI component.<br>
	 * <p>
	 * @param isLookupFunctionAutomaticParms the isLookupFunctionAutomaticParms to set
	 */
	public void setLookupFunctionAutomaticParms(boolean isLookupFunctionAutomaticParms) {
		this.isLookupFunctionAutomaticParms = isLookupFunctionAutomaticParms;
	}

	/**
	 * Sets the forward table number to be used to get HTML technical help.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:componentName</b> and when the help is referred<br>
	 * to the panel, componentName is a null string.<br>
	 * <p>
	 * This method is intended for internal use only.<br>
	 * <p>
	 * The data field contains the complete HTML string to show.<br>
	 * <p>
	 * @return the numTableHelpTechnical
	 */
	public int getNumTableHelpTechnical() {
		return numTableHelpTechnical;
	}

	/**
	 * Sets the forward table number to be used to get multilanguage HTML technical help.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:componentName</b> and when the help is referred<br>
	 * to the panel, componentName is a null string.<br>
	 * <p>
	 * The data field contains the complete HTML string to show.<br>
	 * <p>
	 * @param numTableHelpTechnical the numTableHelpTechnical to set
	 */
	public void setNumTableHelpTechnical(int numTableHelpTechnical) {
		this.numTableHelpTechnical = numTableHelpTechnical;
	}

	/**
	 * Gets the forward table number to be used to get multilanguage HTML end user help.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:componentName</b> and when the help is referred<br>
	 * to the panel, componentName is a null string.<br>
	 * <p>
	 * The data field contains the complete HTML string to show.<br>
	 * <p>
	 * @return the numTableHelpEndUser
	 */
	public int getNumTableHelpEndUser() {
		return numTableHelpEndUser;
	}

	/**
	 * Sets the forward table number to be used to get multilanguage HTML end user help.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:componentName</b> and when the help is referred<br>
	 * to the panel, componentName is a null string.<br>
	 * <p>
	 * The data field contains the complete HTML string to show.<br>
	 * <p>
	 * @param numTableHelpEndUser the numTableHelpEndUser to set
	 */
	public void setNumTableHelpEndUser(int numTableHelpEndUser) {
		this.numTableHelpEndUser = numTableHelpEndUser;
	}

	/**
	 * Gets the forward table number to be used to get multilanguage captions.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:X:componentName</b><br>
	 * Where <b>X</b> is an ordinal of {@link EnumForwardCaptionCustomization} enumeration.<br>
	 * When <b>componentName</b> is a null string, the caption is at the panel level, according to <b>X</b> value.<br>
	 * <p>
	 * The data field contains the string caption to be applied on GUI interface.<br>
	 * <p>
	 * @return the numTableCaptions
	 */
	public int getNumTableCaptions() {
		return numTableCaptions;
	}
 
	/**
	 * Sets the forward table number to be used to get multilanguage captions.<br>
	 * <p>
	 * It should be coded a rule table for each function.<br>
	 * The item key must be <b>panelName:X:componentName</b><br>
	 * Where <b>X</b> is an ordinal of {@link EnumForwardCaptionCustomization} enumeration.<br>
	 * When <b>componentName</b> is a null string, the caption is at the panel level, according to <b>X</b> value.<br>
	 * <p>
	 * The data field contains the string caption to be applied on GUI interface.<br>
	 * <p>
	 * @param numTableCaptions the numTableCaptions to set
	 */
	public void setNumTableCaptions(int numTableCaptions) {
		this.numTableCaptions = numTableCaptions;
	}

 
	/**
	 * Gets the title declared for the function.<br>
	 * <p>
	 * This caption appears on th title of the main window of the application.<br>
	 * <p>
	 * @return the title
	 */
	public String getTitle() {
		return this.title;
	}

	/**
	 * Sets the title declared for the function.<br>
	 * <p>
	 * This title appears on th title of the main window of the application.<br>
	 * <p>
	 * @param String title
	 */
	public void setTitle(String title) {
		this.title = title;
		return;
	}

	/**
	 * Gets the number of application function activations done<br>
	 * by means of FUNCTION_START action.<br>
	 * <p>
	 * @return the counterStarts
	 */
	public int getCounterStarts() {
		return counterStarts;
	}

	/**
	 * Sets the number of application function activations done<br>
	 * by means of FUNCTION_START action.<br>
	 * <p>
	 * @param counterStarts the counterStarts to set
	 */
	public void setCounterStarts(int counterStarts) {
		this.counterStarts = counterStarts;
	}

	
	/**
	 * Gets all active options for the function.<br>
	 * <p>
	 * @return the al_functionOption
	 */
	public ArrayList<EnumForwardOption> getOptions() {
		return al_functionOption;
	}


	/**
	 * Gets the language predefined for captions in the function.<br>
	 * <p>
	 * @return the language
	 */
	public EnumLanguage getLanguage() {
		return this.language;
	}

	/**
	 * Sets the language predefined for captions in the function.<br>
	 * <p>
	 * @param a EnumLanguage language
	 */
	public void setLanguage(EnumLanguage language) {
		this.language = language;
		return;
	}


	
	/**
	 * Gets the locale set for the function.<br>
	 * <p>
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/**
	 * Sets the locale set for the function.<br>
	 * <p>
	 * @param locale the locale to set
	 */
	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	/**
	 * Gets the name of startup form for the function.<br>
	 * If no form has been declared as <code>FORM_START</br> an empty string will be returned.<br>
	 * The complete description can be achieved by <code>getForm(formName)</code>
	 * <p>
	 * @return the formStartName
	 */
	public String getFormStartName() {
		Collection<ForwardForm> li_formDefined = null;
		li_formDefined =  this.getForms();
		for (ForwardForm forwardFormLayout : li_formDefined) {
			if (forwardFormLayout.getFormType() == EnumForwardOption.FORM_TYPE_START) {
				return forwardFormLayout.getFormName();
			}
		}
		return "";
	}


	/**
	 * Gets all forms defined for the function.<br>
	 * <p>
	 * A form is an independent set of panels bound to realize a specific sub functionality.<br>
	 * Normally it's enaugh just a form for one function.<br>
	 * <p>
	 * @return the set of forms definitions
	 */
	public Collection<ForwardForm> getForms() {
		Collection<ForwardForm> cl_form = this.hm_form.values();
		return  cl_form;
	}

	
	/**
	 * Gets a specific form declared for the function.<br>
	 * <p>
	 * A form is an independent set of panels bound to realize a specific sub functionality.<br>
	 * If the form is not declarede returns null.<br>
	 * <p>
	 * @return the language
	 */
	public ForwardForm getForm(String formName) {
		ForwardForm forwardForm = null;
		
		// Scan form declared
		for (Entry<String, ForwardForm> entryForm : this.hm_form.entrySet()) {
			if (!entryForm.getKey().equals(formName)) {continue;}
			forwardForm = entryForm.getValue();
			break;
		}
		return forwardForm;
	}

	

	/**
	 * Return all variable names declared  in the function by <code>VAR()</code> declaration.<br>
	 * <br>
	 * @return the al_var
	 */
	public ArrayList<String> getVarNames() {
		ArrayList<String> al_varName = null;
		al_varName = new ArrayList<String> ();
		// Scan map componenti
		for (Entry<String, InnerComponent> entryComponent : this.hm_var.entrySet()) {
			if (entryComponent.getValue().componentType != EnumForwardComponent.DeclaredVariable) {continue;}
			al_varName.add(entryComponent.getValue().varName);
		}
		return al_varName;
	}

	

	/**
	 * Returns true if the variable is declared for the scope specified.<br>
	 * <p>
	 * If the variable is not declared or is not declared for the scope specified
	 * returns false.<br>
	 * <p>
	 * @param String varName
	 * @param EnumForwardScope scope
	 * @return true if the var is declared with the input scope
	 */
	public boolean isVarDeclared(String varName, EnumForwardScope scope) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_var.get(varName);
		if (innerComponent == null) {return false;}
		if (innerComponent.componentType != EnumForwardComponent.DeclaredVariable) {return false;}
		if (innerComponent.varScope != scope) {return false;}
		return true;
	}

	/**
	 * Returns true if the name is of a variable declared by a VAR() declaration or automatically defined.<br>
	 * <p>
	 * If the name is not defined as a variable returns false.<br>
	 * <p>
	 * @param String varName
	 * @return true if the var is declared
	 */
	public boolean isVarDeclared(String varName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_var.get(varName);
		if (innerComponent == null) {return false;}
		return true;
	}


	/**
	 * Returns the type of the variable declared by function.<br>.
	 * <br>
	 * the type returned is an object class for String, Integer, Boolean, Float and >Double.<br>
	 * <p>
	 * If the variable has been not declared, a null value will be returned.
	 * <p>
	 * @param String varName
	 * @return the Class object java type
	 */
	public Class<?>  getVarType(String varName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_var.get(varName);
		if (innerComponent == null) {return null;}
		if (innerComponent.componentType != EnumForwardComponent.DeclaredVariable) {return null;}
		return innerComponent.varType;
	}

	/**
	 * Returns the initial value of the variable declared by function.<br>.
	 * <br>
	 * If the variable has been not declared, a null value will be returned.
	 * <p>
	 * @param String varName
	 * @return an object of the same type of the variable, as initial value
	 */
	public Object getVarInitial(String varName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_var.get(varName);
		if (innerComponent == null) {return null;}
		if (innerComponent.componentType != EnumForwardComponent.DeclaredVariable) {return null;}
		return innerComponent.varInitial;
	}

	/**
	 * Return a map with all variables declared or automatically created for tables management.<br>
	 * The key is the component name an data is {@link InnerComponent} object describing the variables.<br>
	 * <p>
	 * @return the map with java components
	 */
	public Map<String, InnerComponent>  getVarsMap() {
		return this.hm_var;
	}

	/**
	 * Return a {@link InnerComponent} object describing the variable declared by VAR() or automatically created.<br>
	 * <p>
	 * If the variable is undefined will be returned null.<br>
	 * <p>
	 * @param varName the name of the variable
	 * @return the variable descriptor
	 */
	public InnerComponent getVarDeclared(String varName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_var.get(varName);
		return innerComponent;
	}


	/**
	 * Returns the current value of the variable in the runtime forward monitor, at the function level scope.
	 * <br>
	 * This method returns a generic object containing the value and so it needs to be casted invoking it.<br>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the desired visibility.<br>
	 * To avoid undesiderable exceptions and the casting operation, specific variable<br>
	 * type dependent methods are available, like getValueString() getValueInt and so on.<br>
	 * <p>
	 * @param varName
	 * @return the object var value
	 */
	public Object  getVarValue(String varName) {
		return this.monitor.getVarValue(this.mcb, varName, EnumForwardScope.SCOPE_FUNCTION);
	}

	/**
	 * Returns the current value of the variable in the runtime forward monitor.
	 * <br>
	 * This method returns a generic object containing the value and so it needs to be casted invoking it.<br>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the desired visibility.<br>
	 * To avoid undesiderable exceptions and the casting operation, specific variable<br>
	 * type dependent methods are available, like getValueString() getValueInt and so on.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the object var value
	 */
	public Object  getVarValue(String varName, EnumForwardScope scope) {
		return this.monitor.getVarValue(this.mcb, varName, scope);
	}

	/**
	 * Sets the current value of the variable, at the function level scope, in the runtime forward monitor.
	 * <br>
	 * It's a way to set complex and structured parameters, organized by means of an application object, <br>
	 * with the visibility wished.<br>
	 * <p>
	 * @param varName
	 * @param varValue the object variable value 
	 */
	public void  setVarValue(String varName, Object varValue) {
		this.monitor.setVarValue(this.mcb, varName, varValue);
		return;
	}

	/**
	 * Returns the current String value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing string object, will be returned the string value.<br>	 
	 * <p>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param varName
	 * @return the String value
	 */
	public String getValueString(String varName) {
		return (String) getValueCommon(varName, true, true);
	}
    
	/**
	 * Returns the current String value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing string object, will be returned the string value.<br>	 
	 * <p>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns an empty string.<br>
	 * <p>
	 * @param varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the String value
	 */
	public String getValueString(String varName, EnumForwardScope scope) {
		InnerComponent innerComponent = null;
		innerComponent = this.getComponentDeclared(varName);
		return getValueStringCommon(innerComponent, varName, scope, true, true);
	}

	/**
	 * Returns the current Object value of the variable name at function scope level or of the GUI control name.
	 * <br>
	 * The name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the name is of a swing object, will be returned an object with the value compatible with the GUI object,
	 * as an object String, Integer, Double, Float, Boolean, Color and Date.<br>	 
	 * <p>
	 * If the name is of a variable not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param name the name of a GUI control or of a variable declared by VAR() or automatically created
	 * @return the value object or null if no GUI control and no variable has been defined with the same name
	 */
	public Object getValue(String name) {
		return getValueCommon(name, true, true);
	}
  
	/**
	 * Returns the current Object value of the GUI control name.
	 * <br>
	 * The name must be the name of a control declared in any panel.<br>
	 * It will be returned an object with the value compatible with the GUI component,
	 * as an object String, Integer, Double, Float, Boolean, Color and Date.<br>	 
	 * <p>
	 * If the name is not of a component a null value will be returned<br>
	 * <p>
	 * @param componentName the name of a GUI control or of a variable declared by VAR() or automatically created
	 * @return the value object or null if no GUI control and no variable has been defined with the same name
	 */
	public Object getValueControlGUI(String componentName ) {
		return getValueCommon(componentName, false, true);
	}

	/**
	 * Sets the GUI control value or the variable value.
	 * <br>
	 * The name must be the name of a control declared in any panel or of a variable declared by VAR().<br>
	 * First, it will be verified if the input name is of a variable declared name and, if not, of a GUI control.<br>
	 * The object value type must be compatible with the GUI component or the variable name,
	 * as an object Boolean, String, Integer, Double, Float and so on.<br>	 
	 * <p>
	 * If the name is not of a component and neither of a variable no action will be taken<br>
	 * If the object value type is wrong no action will be taken<br>
	 * <p>
	 * @param componentOrVar the name of a GUI control component or of a variable
	 * @param valueObject the value to be set
	 */
	public void setValue(String componentOrVar, Object valueObject ) {

		// Componente GUI
		if (this.getComponentDeclared(componentOrVar) != null) {
			setValueComponentGui(componentOrVar, valueObject, null);
		}
		
		// Variabile dichiarata con VAR()
		if (this.getVarDeclared(componentOrVar) != null) {
			setVarValue(componentOrVar, valueObject);
		}
		
		return;
	}

	/**
	 * Sets the GUI control value or the variable value.
	 * <br>
	 * The name must be the name of a control declared in any panel or of a variable declared by VAR().<br>
	 * First, it will be verified if the input name is of a variable declared name and, if not, of a GUI control.<br>
	 * The object value type must be compatible with the GUI component or the variable name,
	 * as an object Boolean, String, Integer, Double, Float and so on.<br>	 
	 * <p>
	 * If the name is not of a component and neither of a variable no action will be taken<br>
	 * If the object value type is wrong no action will be taken<br>
	 * <p>
	 * @param componentOrVar the name of a GUI control component or of a variable
	 * @param varName the variable name to be used to update
	 */
	public void setValueFromVar(String componentOrVar, String varName) {
		Object valueObject = null;
		
		// Componente GUI
		if (this.getComponentDeclared(componentOrVar) != null) {
			setValueComponentGui(componentOrVar, null, varName);
		}
		
		// Variabile dichiarata con VAR()
		if (this.getVarDeclared(componentOrVar) != null) {
			valueObject = this.getValueVar(varName);
			setVarValue(componentOrVar, valueObject);
		}
		
		return;
	}

	/**
	 * Sets the variable value with the content of another variable.
	 * <br>
	 * The name must be the name of a variable declared by VAR().<br>
	 * First, it will be verified if the input name is of a variable declared name.<br>
	 * The object value type must be compatible the variable name,
	 * as an object Boolean, String, Integer, Double, Float and so on.<br>	 
	 * <p>
	 * If the variable to set is not of a variable no action will be taken<br>
	 * If the variable with the value to set is type wrong no action will be taken<br>
	 * <p>
	 * @param varName the variable name to set
	 * @param varNameWithValue the variable with the value to set
	 */
	public void setValueVarFromVar(String varName, String varNameWithValue) {
		Object valueObject = null;
		
		// Variabile dichiarata con VAR()
		if (this.getVarDeclared(varName) != null) {
			// Variabile dichiarata con VAR()
			if (this.getVarDeclared(varNameWithValue) != null) {
				valueObject = this.getValueVar(varName);
				setVarValue(varName, valueObject);
			}
		}
		return;
	}


	/**
	 * Sets the GUI control value.
	 * <br>
	 * The name must be the name of a control declared in any panel.<br>
	 * The object value type must be compatible with the GUI component,
	 * as an object Boolean, String, Integer, Double, Float and so on.<br>	 
	 * <p>
	 * If the name is not of a component no action will be taken<br>
	 * If the object value type is wrong no action will be taken<br>
	 * <p>
	 * @param componentName the name of a GUI control
	 * @return valueObject the value to be set
	 */
	public void setValueControlGui(String componentName, Object valueObject ) {
		setValueComponentGui(componentName, valueObject, null);
		return;
	}

	/**
	 * Sets the GUI control value with the content of a variable.
	 * <br>
	 * The name must be the name of a control declared in any panel.<br>
	 * The variable name must be declared by function as a VAR() declaration and<br>
	 * the variabletype must be compatible with the GUI component,
	 * as an object Boolean, String, Integer, Double, Float and so on.<br>	 
	 * <p>
	 * If the name is not of a component no action will be taken<br>
	 * If the variable type is wrong no action will be taken<br>
	 * <p>
	 * @param componentName the name of a GUI control
	 * @return varName the variable name as input to update the GUI control
	 */
	public void setValueControlGuiFromVar(String componentName, String varName) {
		setValueComponentGui(componentName, null, varName);
		return;
	}


	/**
	 * Returns the current variable value of the variable name at function scope level.
	 * <br>
	 * The name can be the name of a declared variable with VAR() or an implicit automatically created<br>
	 * column variable for a table.<br>
	 * <p>
	 * If the name is not of a defined variable a null value will be returned.<br>
	 * <p>
	 */
	public Object getValueVar(String componentName ) {
		return getValueCommon(componentName, true, false);
	}

  
	/*
	 * Returns the current Object value of the GUI control name or variable.
	 */
	private Object getValueCommon(String name, boolean searchAsVar, boolean searchAsComponentGui) {
		InnerComponent innerComponent = null;
		Class<?> classComponentOrVar = null;
		
		// Recupero descrittore componente o variabile
		innerComponent = this.getComponentDeclared(name);
		if (innerComponent == null) {
			innerComponent = this.getVarDeclared(name);
			if (innerComponent != null) {
				classComponentOrVar = innerComponent.varType;
			}
		} else {
			classComponentOrVar = innerComponent.componentObjectClass;
		}
		
		// Nessun componente Swing o variabile con lo stesso nome
		if (innerComponent == null) {return null;}
		
		// Se Componente Swing, in base al tipo di classe si attivano i metodi specifici
		// per il recupero del valore dagli oggetti GUI rappresentabili con tali classi.
		// Se una variabile recupera il valore corrente in esecuzione attraverso il monitor.
		if (classComponentOrVar == String.class) {
			return getValueStringCommon(innerComponent, name, EnumForwardScope.SCOPE_FUNCTION, searchAsVar, searchAsComponentGui);
		}
		if (classComponentOrVar == Integer.class) {
			return getValueIntCommon(innerComponent, name, EnumForwardScope.SCOPE_FUNCTION, searchAsVar, searchAsComponentGui);
		}
		if (classComponentOrVar == Double.class) {
			return getValueDouble(name, EnumForwardScope.SCOPE_FUNCTION);
		}
		if (classComponentOrVar == Float.class) {
			return getValueFloat(name, EnumForwardScope.SCOPE_FUNCTION);
		}
		if (classComponentOrVar == Boolean.class) {
			return getValueBooleanCommon(innerComponent, name, EnumForwardScope.SCOPE_FUNCTION, searchAsVar, searchAsComponentGui);
		}
		if (classComponentOrVar == Date.class) {
			return getValueDateCommon(innerComponent, name, EnumForwardScope.SCOPE_FUNCTION, searchAsVar, searchAsComponentGui);
		}

		// Se una variabile, deve essere una variabile applicativa strutturata
		
		return this.monitor.getVarValue(this.mcb, name, EnumForwardScope.SCOPE_FUNCTION);
	}

	/*
	 * Imposta il GUI control con il value object in input o il contenuto di una variabile
	 * Il valore della variabile è memorizzato nella map del monitor
	 */
	private void setValueComponentGui(String nameControl, Object value, String varName) {
		
		InnerComponent innerComponentSwing = null;
		InnerComponent innerComponentVar = null;
		Object valueToUpdate = null;
		
		// Recupero descrittore componente gui
		innerComponentSwing = this.getComponentDeclared(nameControl);
		
		// Nessun componente GUI con il nome in input
		if (innerComponentSwing == null) {
			return;
		}
		
		// Valore da variabile in input
		if (varName != null) {
			innerComponentVar = this.getVarDeclared(varName);
			if (innerComponentVar == null) {
				return;
			}
            // Tipo valori incompatibili
			if (innerComponentSwing.componentObjectClass != innerComponentVar.varType) {
				return;
			}
			valueToUpdate = getVarValue(varName);
		}
		
		// Valore da oggetto in input
		if (value != null) {
  			if (innerComponentSwing.componentObjectClass != value.getClass()) {
				return;
			}
  			valueToUpdate = value;
		}
		
		// Chiamata errata
		if (valueToUpdate == null) {
			return;
		}
		
		// In base al tipo componente Swing, si imposta il valore
		if (innerComponentSwing.componentObjectClass == String.class) {
			setValueGuiComponentString(innerComponentSwing, (String)valueToUpdate);
			return;
		}
		if (innerComponentSwing.componentObjectClass == Integer.class) {
			setValueGuiComponentInt(innerComponentSwing, (Integer)valueToUpdate);
			return;
		}
		if (innerComponentSwing.componentObjectClass == Double.class) {
			// Nessun controllo GUI con valori Double
			return;
		}
		if (innerComponentSwing.componentObjectClass == Float.class) {
			// Nessun controllo GUI con valori Float
			return;
		}
		if (innerComponentSwing.componentObjectClass == Boolean.class) {
			setValueGuiComponentBoolean(innerComponentSwing, (Boolean)valueToUpdate);
			return;
		}
		if (innerComponentSwing.componentObjectClass == Date.class) {
			setValueGuiComponentDate(innerComponentSwing, (Date)valueToUpdate);
			return;
		}
		return;
	}

	/*
	 * Gestione comune update controlli GUI per variabili string
	 */
	private void setValueGuiComponentString(InnerComponent innerComponent, String valueString) {
		
		// Selezione componente swing
		switch (innerComponent.componentType) {
				case JTextField:
					getJTextField(innerComponent.componentName).setText(valueString);
					return;
				case JPasswordField:
					getJPasswordField(innerComponent.componentName).setText(valueString);
					return;
				case JTextArea:
					getJTextArea(innerComponent.componentName).setText(valueString);
					return;
				case JFormattedTextField:
					if (getJFormattedTextField(innerComponent.componentName).getValue() instanceof String) {
						getJFormattedTextField(innerComponent.componentName).setValue(valueString);
						return;
					}
					return;
				case JComboBox:
					getJComboBox(innerComponent.componentName).setSelectedItem(valueString);
					return;
				case JLabel:
					getJLabel(innerComponent.componentName).setText(valueString);
					return;
		}
		return;
	}

	/*
	 * Gestione comune update controlli GUI per variabili integer
	 */
	private void setValueGuiComponentInt(InnerComponent innerComponent, Integer intValue) {
		
		
		// Selezione componente swing
		switch (innerComponent.componentType) {
				case JProgressBar:
					getJProgressBar(innerComponent.componentName).setValue(intValue.intValue());
					return;
				case JSlider:
					getJSlider(innerComponent.componentName).setValue(intValue.intValue());
					return;
				case JSpinner:
					// TODO
					return;
				case JFormattedTextField:
					if (getJFormattedTextField(innerComponent.componentName).getValue() instanceof Integer) {
						getJFormattedTextField(innerComponent.componentName).setValue(intValue);
						return;
					}
					return;
		}
		return;
	}

	/*
	 *  Gestione comune update controlli GUI per variabili Boolean
	 */
	private void setValueGuiComponentBoolean(InnerComponent innerComponent, boolean booleanValue) {
		
		// Selezione componente swing
		switch (innerComponent.componentType) {
				case JCheckBox:
					getJCheckBox(innerComponent.componentName).setSelected(booleanValue);
					return;
				case JCheckBoxMenuItem:
					getJCheckBoxMenuItem(innerComponent.componentName).setSelected(booleanValue);
					return;
				case JRadioButton:
					getJRadioButton(innerComponent.componentName).setSelected(booleanValue);
					return;
				case JRadioButtonMenuItem:
					getJRadioButtonMenuItem(innerComponent.componentName).setSelected(booleanValue);
					return;
				case JToggleButton:
					getJToggleButton(innerComponent.componentName).setSelected(booleanValue);
					return;
		}
		return;
	}

	/*
	 * Gestione comune update controlli GUI per variabili Date
	 */
	private void setValueGuiComponentDate(InnerComponent innerComponent, Date dateValue) {
				
		// Selezione componente swing
		switch (innerComponent.componentType) {
				case JFormattedTextField:
					if (getJFormattedTextField(innerComponent.componentName).getValue() instanceof Date) {
						getJFormattedTextField(innerComponent.componentName).setValue(dateValue);
						return;
					}
					break;
				case JTextField:				// Potrebbe contenerere una data: NON gestito
					// TODO
					break;
					
		}
		return;
	}

	/*
	 * Gestione comune per variabili string
	 */
	private String getValueStringCommon(InnerComponent innerComponent, String varName, EnumForwardScope scope, boolean searchAsVar, boolean searchAsComponentGui) {
		
		// E' una variabile: il valore è gestito dal monitor
		if (searchAsVar && innerComponent.componentType == EnumForwardComponent.DeclaredVariable) {
			return this.monitor.getValueString(varName, scope);
		}
		
		// Non è richiesta la ricerva su valore di componenete swing
		if (!searchAsComponentGui) {return "";}
		
		// E' il nome di un componente swing
		switch (innerComponent.componentType) {
				case JTextField:
					return getJTextField(varName).getText();
				case JPasswordField:
					char[] ar_char = getJPasswordField(varName).getPassword();
					String pwd = "";
					for (char c : ar_char) {
						pwd = pwd + c;
					}
					return pwd;
				case JTextArea:
					return getJTextArea(varName).getText();
				case JFormattedTextField:
					if (getJFormattedTextField(varName).getValue() instanceof String) {
						return (String) getJFormattedTextField(varName).getValue();
					}
					if (getJFormattedTextField(varName).getValue() instanceof Integer) {
						return ((Integer) getJFormattedTextField(varName).getValue()).toString();
					}
					return "";
				case JComboBox:
					return (String) getJComboBox(varName).getSelectedItem();
		}
		return "";
	}


	/**
	 * Returns the current int value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current int value, will be returned the int value.<br>	 
	 * <p>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @return the Integer value
	 */
	public Integer getValueInt(String varName) {
		Object value = null;
		value = getValueCommon(varName, true, true);
		if (value == null) {return null;}
		if (!(value instanceof Integer)) {return null;}
		return (Integer) getValueCommon(varName, true, true);

	}

	/**
	 * Returns the current int value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current int value, will be returned the int value.<br>	 
	 * <p>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the Integer value
	 */
	public Integer getValueInt(String varName, EnumForwardScope scope) {
		InnerComponent innerComponent = null;
		innerComponent = this.getComponentDeclared(varName);
		return getValueIntCommon(innerComponent, varName, scope, true, true);
	}

	/*
	 * Gestione comune per variabili Integer
	 */
	private int getValueIntCommon(InnerComponent innerComponent, String varName, EnumForwardScope scope, boolean searchAsVar, boolean searchAsComponentGui) {
		
		// E' una variabile: il valore è gestito dal monitor
		if (searchAsVar && innerComponent.componentType == EnumForwardComponent.DeclaredVariable) {
			return this.monitor.getValueInt(varName, scope);
		}
		
		if (!searchAsComponentGui) {return 0;}
		
		// E' il nome di un componente swing
		switch (innerComponent.componentType) {
				case JProgressBar:
					return getJProgressBar(varName).getValue();
				case JSlider:
					return getJSlider(varName).getValue();
				case JSpinner:
					// TODO
					return 0;
				case JFormattedTextField:
					if (getJFormattedTextField(varName).getValue() instanceof Integer) {
						return (Integer) getJFormattedTextField(varName).getValue();
					}
					return 0;
		}
		return 0;
	}


	/**
	 * Returns the current boolean value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current boolean value, will be returned the boolean value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @return the Boolean value
	 */
	public Boolean getValueBoolean(String varName) {
		Object value = null;
		value = getValueCommon(varName, true, true);
		if (value == null) {return null;}
		if (!(value instanceof Boolean)) {return null;}
		return (Boolean) getValueCommon(varName, true, true);

	}

	/**
	 * Returns the current boolean value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current boolean value, will be returned the boolean value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the Boolean value
	 */
	public boolean getValueBoolean(String varName, EnumForwardScope scope) {
		InnerComponent innerComponent = null;
		innerComponent = this.getComponentDeclared(varName);
		return getValueBooleanCommon(innerComponent, varName, scope, true, true);
	}

	/*
	 * Gestione comune per variabili Boolean
	 */
	private boolean getValueBooleanCommon(InnerComponent innerComponent, String varName, EnumForwardScope scope, boolean searchAsVar, boolean searchAsComponentGui) {
		
		// E' una variabile: il valore è gestito dal monitor
		if (searchAsVar && innerComponent.componentType == EnumForwardComponent.DeclaredVariable) {
			return this.monitor.getValueBoolean(varName, scope);
		}
		
		if (!searchAsComponentGui) {return false;}
		
		// E' il nome di un componente swing
		switch (innerComponent.componentType) {
				case JCheckBox:
					return getJCheckBox(varName).isSelected();
				case JCheckBoxMenuItem:
					return getJCheckBoxMenuItem(varName).isSelected();
				case JRadioButton:
					return getJRadioButton(varName).isSelected();
				case JRadioButtonMenuItem:
					return getJRadioButtonMenuItem(varName).isSelected();
				case JToggleButton:
					return getJToggleButton(varName).isSelected();
		}
		return false;
	}

	/**
	 * Returns the current date value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current date value, will be returned the date value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @return the Date value
	 */
	public Date getValueDate(String varName) {
		Object value = null;
		value = getValueCommon(varName, true, true);
		if (value == null) {return null;}
		if (!(value instanceof Date)) {return null;}
		return (Date) getValueCommon(varName, true, true);
	}
 
	/**
	 * Returns the current date value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current date value, will be returned the date value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the Date value
	 */
	public Date getValueDate(String varName, EnumForwardScope scope) {
		InnerComponent innerComponent = null;
		innerComponent = this.getComponentDeclared(varName);
		return getValueDateCommon(innerComponent, varName, scope, true, true);
	}

	/*
	 * Gestione comune per variabili Date
	 */
	private Date getValueDateCommon(InnerComponent innerComponent, String varName, EnumForwardScope scope, boolean searchAsVar, boolean searchAsComponentGui) {
		
		// E' una variabile: il valore è gestito dal monitor
		if (searchAsVar && innerComponent.componentType == EnumForwardComponent.DeclaredVariable) {
			return this.monitor.getValueDate(varName, scope);
		}
		
		if (!searchAsComponentGui) {return null;}
		
		// E' il nome di un componente swing
		switch (innerComponent.componentType) {
				case JFormattedTextField:
					if (getJFormattedTextField(varName).getValue() instanceof Date) {
						return (Date) getJFormattedTextField(varName).getValue();
					}
					break;
				case JTextField:				// Potrebbe contenerere una data: NON gestito
					// TODO
					break;
					
		}
		return null;
	}



	/**
	 * Returns the current float value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current float value, will be returned the float value.<br>	 
	 * <p>
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @return the Float value
	 */
	public Float getValueFloat(String varName) {
		return this.monitor.getValueFloat(varName);
	}

	/**
	 * Returns the current float value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current float value, will be returned the float value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the Float value
	 */
	public Float getValueFloat(String varName, EnumForwardScope scope) {
		return this.monitor.getValueFloat(varName, scope);
	}

	/**
	 * Returns the current double value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current double value, will be returned the double value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @return the Double value
	 */
	public Double getValueDouble(String varName) {
		return this.monitor.getValueDouble(varName);
	}

	/**
	 * Returns the current double value of the variable in the runtime forward monitor, at function scope level.
	 * <br>
	 * <br>
	 * The variable name can be the name of a declared variable with VAR(), an implicit automatically created<br>
	 * column variable for a table or, at the end, the name of a control declared in any panel.<br>
	 * If the variable name is of a swing object with a current double value, will be returned the double value.<br>	 
	 * If the variable is not defined or if is defined but not visible for the scope or,<br>
	 * at the end, it's defined with of a different type, returns null.<br>
	 * <p>
	 * @param String varName
	 * @param scope as a {@link EnumForwardScope} object
	 * @return the Double value
	 */
	public Double getValueDouble(String varName, EnumForwardScope scope) {
		return this.monitor.getValueDouble(varName, scope);
	}

		

	/**
	 * Gets all parameters names declared as required to the function when it's called by another function.
	 * <br>
	 * @return the parameters required
	 */
	public ArrayList<String> getParmNamesRequired() {
		return this.al_parmRequired;
	}

	/**
	 * Gets all parameters names declared as returned to the caller function when this is a called function.
	 * <br>
	 * @return the parameters returned
	 */
	public ArrayList<String> getParmNamesReturned() {
		return this.al_parmReturned;
	}


	/**
	 * Gets the current language.<br>
	 * <p>
	 * All texts, captions and tooltip are localized by the current language.<br>
	 * <p>
	 * @return the curLanguage
	 */
	public EnumLanguage getCurLanguage() {
		return curLanguage;
	}

	/**
	 * Sets the current language.<br>
	 * <p>
	 * All texts, captions and tooltip are localized by the current language.<br>
	 * <p>
	 * @param curLanguage the curLanguage to set
	 */
	public void setCurLanguage(EnumLanguage curLanguage) {
		this.curLanguage = curLanguage;
	}

	/**
	 * Returns the type of the parameter declared as required by function.<br>.
	 * <br>
	 * the type returned is an object class for String, Integer, Boolean, Float and Double or any application object.<br>
	 * If the parameter is not declared returns null.<br>
	 * <p>
	 * @param parmName
	 * @return the Class object java type
	 */
	public Class<?> getParmTypeRequired(String parmName) {
		InnerComponent innerComponent = null;
		if (!this.al_parmRequired.contains(parmName)) {return null;}
		innerComponent = getVarDeclared(parmName);
	return innerComponent.varType;
	}

	/**
	 * Returns the type of the parameter declared as returned by function.<br>.
	 * <br>
	 * the type returned is an object class for String, Integer, Boolean, Float and Double or any application object.<br>
	 * If the parameter is not declared returns null.<br>
	 * <p>
	 * @param parmName
	 * @return the Class object java type
	 */
	public Class<?> getParmTypeReturned(String parmName) {
		InnerComponent innerComponent = null;
		if (!this.al_parmReturned.contains(parmName)) {return null;}
		innerComponent = getVarDeclared(parmName);
	return innerComponent.varType;
	}

	/**
	 * Returns the current value of the parameter returned by function.
	 * <br>
	 * The returned parameter value is stored into the function object and is set directly by application code<br>
	 * This method returns a generic object containing the value and so it needs to be casted invoking it.<br>
	 * <p>
	 * If the parameter is not declared returns null.<br>
     * <p>
	 * @param parmName
	 * @return the object parm
	 */
	public Object getParmValueReturned(String parmName) {
		if (!this.al_parmReturned.contains(parmName)) {return null;}
		return this.getValueVar(parmName);
	}

	/**
	 * Sets the current value of the parameter to return to caller function.
	 * <br>
	 * The returned parameter value is stored into the function object as a normal variable and it is set directly by application code<br>
	 * <p>
	 * If the parameter is not declared returns does nothing.<br>
     * <p>
	 * @param parmName
	 * @param parmValue the object parm value
	 */
	public void setParmValueReturned(String parmName, Object parmValue) {
		if (!this.al_parmReturned.contains(parmName)) {return;}
		this.setVarValue(parmName, parmValue);
		return;
	}

	/**
	 * Returns the current value of the parameter required by function.
	 * <br>
	 * The required parameter value is stored into the function object and is set by forward  monitor,
	 * before the function activation starting.<br>
	 * Forward monitor knows parameters names and types bjiust asking this kind of informations to the<br>
	 * function to call. 
	 * This method returns a generic object containing the value and so it needs to be casted invoking it.<br>
	 * <p>
	 * If the parameter is not declared returns null.<br>
     * <p>
	 * @param parmName
	 * @return the object parm
	 */
	public Object getParmValueRequired(String parmName) {
		if (!this.al_parmRequired.contains(parmName)) {return null;}
		return this.getValueVar(parmName);
	}

	
	/**
	 * Sets the current value of the parameter required by function.
	 * <br>
	 * The required parameter value is stored into the function object and is set by forward  monitor,
	 * before the function activation starting.<br>
	 * Forward monitor knows parameters names and types bjiust asking this kind of informations to the<br>
	 * function to call. 
	 * This method returns a generic object containing the value and so it needs to be casted invoking it.<br>
	 * <p>
	 * If the parameter is not declared does nothing.<br>
     * <p>
	 * @param parmName
	 * @param parmValue the object parm value
	 */
	public void setParmValueRequired(String parmName, Object parmValue) {
		if (!this.al_parmRequired.contains(parmName)) {return;}
		this.setVarValue(parmName, parmValue);
		return;
	}

	
	

	/**
	 * Gets the menu declared as an object {@link ForwardMenu}.<br>
	 * <p>
	 * If the menu is undefined returns null.<br>
	 * <p>
	 * @param String menuName
	 */
	public ForwardMenu getMenu(String menuName) {
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		return forwardMenu;
	}
	
	/**
	 * Gets the type declared for the menu.<br>
	 * <p>
	 * If the menu is undefined returns null.<br>
	 * <p>
	 * @param String menuName
	 */
	public EnumForwardOption getMenuType(String menuName) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}
		return forwardMenu.getMenuType();
	}
	
	/**
	 * Sets the type declared for the menu.<br>
	 * <p>
	 * If the menu is undefined does nothing.<br>
	 * <p>
	 * @param String menuName
	 * @param EnumForwardMenuOptions with the type of menu
	 */
	public void setMenuType(String menuName, EnumForwardOption menuType) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return;}
		forwardMenu.setMenuType(menuType);
		return;
	}
	
	/**
	 * Gets the direction declared for the menu.<br>
	 * <p>
	 * If the menu is undefined returns null.<br>
	 * <p>
	 * @param String menuName
	 */
	public EnumForwardOption getMenuDirection(String menuName) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}
		return forwardMenu.getMenuDirection();
	}
	
	/**
	 * Sets the direction declared for the menu.<br>
	 * <p>
	 * If the menu is undefined does nothing.<br>
	 * <p>
	 * @param String menuName
	 * @param EnumForwardMenuOptions with the direction of menu
	 */
	public void setMenuDirection(String menuName, EnumForwardOption menuDirection) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return;}
		forwardMenu.setMenuDirection(menuDirection);
		return;
	}
	
	/**
	 * Gets the style declared for the menu.<br>
	 * <p>
	 * If the menu is undefined returns null.<br>
	 * <p>
	 * @param String menuName
	 */
	public EnumForwardOption getMenuStyle(String menuName) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}
		return forwardMenu.getMenuStyle();
	}
	
	/**
	 * Sets the style declared for the menu.<br>
	 * <p>
	 * If the menu is undefined does nothing.<br>
	 * <p>
	 * @param String menuName
	 * @param EnumForwardMenuOptions with the style of menu
	 */
	public void setMenuStyle(String menuName, EnumForwardOption menuStyle) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return;}
		forwardMenu.setMenuStyle(menuStyle);
		return;
	}
	
	/**
	 * Remove all menu items for the menu.<br>
	 * <p>
	 * If the menu is undefined does nothing.<br>
	 * <p>
	 * @param String menuName
	 */
	public void removeMenuItems(String menuName) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return;}
		while (forwardMenu.getMenuItems().size() > 0) {
			forwardMenu.getMenuItems().remove(0);
		}
		return;
	}
	
	/**
	 * Add a menu to the function.<br>
	 * <p>
	 * Only main properties are set.<br>
	 * <p>
	 * @param String menuName
	 * @param String menuTitle used by menuBar rendering
	 * @param EnumForwardMenuOptions menuType
	 * @param EnumForwardMenuOptions menuMode
	 * @param EnumForwardMenuOptions menuDirection
	 * @param EnumForwardMenuOptions menuStyle
	 * @param menuHeigh 
	 */
	public ForwardMenu addMenu(String menuName
					         , String menuTitle
							 , EnumForwardOption menuType
							 , EnumForwardOption menuRendering
							 , EnumForwardOption menuDirection
							 , EnumForwardOption menuStyle
							 , EnumForwardOption menuHeigh
							 , ForwardMenu menuRoot
					) {
		
		JComponent jmenu = null;
		JToolBar jtoolBar = null;
		
		ForwardMenu forwardMenu = null;
		forwardMenu = new ForwardMenu();
		forwardMenu.setMenuName(menuName);
		forwardMenu.setMenuTitle(menuTitle);
		forwardMenu.setMenuType(menuType);
		forwardMenu.setMenuRendering(menuRendering);
		forwardMenu.setMenuDirection(menuDirection);
		forwardMenu.setMenuStyle(menuStyle);
		forwardMenu.setMenuHeigh(menuHeigh);
		forwardMenu.setPanelName("");
		
		if (menuType != EnumForwardOption.MENU_TYPE_ROOT) {
			forwardMenu.setMenuRoot(menuRoot);
		} else {
			forwardMenu.setMenuRoot(forwardMenu);
		}
		
		// Oggetto JMenu se menuBar
		if (menuRendering == EnumForwardOption.MENU_RENDERING_MENUBAR) {
			if (menuType != EnumForwardOption.MENU_TYPE_POPUP) {
				jmenu = new JMenu(menuTitle);
			} else {
				jmenu = new JPopupMenu();
			}
			jmenu.setName(menuName);
			forwardMenu.setGraphicObject(jmenu);
		    putComponentOnStructure(menuName, EnumForwardComponent.JMenu, jmenu);  // Inserimento in map di struttura hm_component
		}
		
		// Oggetto JToolBar se toolBar e root level
		if (menuRendering == EnumForwardOption.MENU_RENDERING_TOOLBAR) {
			jtoolBar = new JToolBar();
			jtoolBar.setName(menuName);
			jtoolBar.setOrientation(JToolBar.HORIZONTAL);
		    jtoolBar.setFloatable(false);
		    jtoolBar.setRollover(true);
			forwardMenu.setGraphicObject(jtoolBar);
		    putComponentOnStructure(menuName, EnumForwardComponent.JToolBar, jtoolBar);  // Inserimento in map di struttura hm_component
		}
		
	    this.hm_menu.put(menuName, forwardMenu);

		return forwardMenu;
	}

	/**
	 * Add a menu item to the menu.<br>
	 * <p>
	 * A {@link ForwardMenuItem} object will be returned and so it's possible update all properies.<br>
	 * <p>
	 * No operations if the menù specified doesn't exists.<br>
	 * <p>
	 * @param menuName
	 * @param controlName
	 * @param componentType
	 * @param caption
	 */
	public ForwardMenuItem addMenuItem(String menuName, String controlName, EnumForwardComponent componentType, String caption) {
		ForwardMenu forwardMenu = null;
		ForwardMenuItem forwardMenuItem = null;
		
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}

		// Creazione e accodamento menu item o separatore in oggetti specifici per menu
		forwardMenuItem = new ForwardMenuItem(controlName, caption);
		forwardMenuItem.setComponentType(componentType);
		forwardMenu.getMenuItems().add(forwardMenuItem);
		return forwardMenuItem;
	}
	

	/**
	 * Add a menu item to the menu, minimal constructor.<br>
	 * <p>
	 * A {@link ForwardMenuItem} object will be returned and so it's possible update all properies.<br>
	 * <p>
	 * No operations if the menù specified doesn't exists.<br>
	 * <p>
	 * @param menuName
	 * @param controlName
	 */
	public ForwardMenuItem addMenuItem(String menuName, String controlName) {
		ForwardMenu forwardMenu = null;
		ForwardMenuItem forwardMenuItem = null;
		
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}

		// Creazione e accodamento menu item o separatore in oggetti specifici per menu
		forwardMenuItem = new ForwardMenuItem(controlName, "");
		forwardMenu.getMenuItems().add(forwardMenuItem);
		return forwardMenuItem;
	}
	

	/**
	 * Remove the menu definition.<br>
	 * <p>
	 * If the menu is undefined does nothing.<br>
	 * <p>
	 * @param menuName
	 */
	public void removeMenu(String menuName) {
		
		ForwardMenu innerMenu = null;
		innerMenu = this.hm_menu.get(menuName);
		if (innerMenu == null) {return;}
		this.hm_menu.remove(menuName);
		return;
	}
	
	 
	/**
	 * Gets the descriptor of the menu item as an internal object {@link ForwardMenuItem}.<br>
	 * <p>
	 * If the menu or the menu item is not defined returns null.<br>
	 * The menù name required must be that at the correct level and so it could be<p>
	 * a submenu too.<br>
	 * <p>
	 * It's so possible to get or modify caption, shortKey, separator, object and so on.<br>
	 * <p>
	 * @param menuName
	 * @param menuItemName
	 */
	public ForwardMenuItem getMenuItem(String menuName, String menuItemName) {
		ForwardMenu forwardMenu = null;
			
		// Gets menu
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return null;}
		return forwardMenu.getMenuItem(menuItemName);
	}
	 
   
	/**
	 * Gets the descriptor of the menu where the menu item has been declared, as an object {@link ForwardMenu}.<br>
	 * <p>
	 * If the menu item has not been defined in any menu, a null value will returned.<br>
	 * <p>
	 * @param menuItemName
	 */
	public ForwardMenu getMenuOwner(String menuItemName) {
		ForwardMenu forwardMenu = null;
		
		// Scan menus declared
		for (Entry<String, ForwardMenu> entryMenu : this.hm_menu.entrySet()) {
			forwardMenu = entryMenu.getValue();
			// Scan menu items
			for (ForwardMenuItem menuItem : forwardMenu.getMenuItems()) {
				if (menuItem.getControlName().equals(menuItemName)) {
					return forwardMenu;
				}
			}
		}
		return null;
	}
	 
   
	
	
	/**
	 * Gets the JMenu or JPopUpMenu component declared for the menu.<br>
	 * <p>
	 * If the menu is undefined returns a new JMenu object with no effetcts on the function.<br>
	 * <p>
	 * @param menuName
	 */
	public JMenu getJMenu(String menuName) {
		
		ForwardMenu forwardMenu = null;
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return new JMenu();}
		return (JMenu) forwardMenu.getGraphicObject();
	}

	/**
	 * Gets the swing {@link JMenuItem} object of the menu item.<br>
	 * <p>
	 * If the menu/submenu or the menu item is not defined returns a new JMenu object with no effetcts on the function.<br>
	 * <p>
	 * It's so possible to get or modify caption, shortKey, separator, object and so on.<br>
	 * <p>
	 * @param menuName
	 * @param controlName
	 */
	public JMenuItem getJMenuItem(String menuName, String controlName) {
		ForwardMenu forwardMenu = null;
			
		// Gets menu
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return new JMenuItem();}

		// Search menu item
		for (ForwardMenuItem forwardMenuItem : forwardMenu.getMenuItems()) {
			if (!forwardMenuItem.getControlName().equals(controlName)) {continue;}
			if (!(forwardMenuItem.getGraphicObjectMenu() instanceof JMenuItem)) {continue;}
			return (JMenuItem) forwardMenuItem.getGraphicObjectMenu();
		}
		return new JMenuItem();
	}
	 
	/**
	 * Gets the swing {@link JCheckBoxMenuItem} object of the menu item.<br>
	 * <p>
	 * If the menu/submenu or the menu item is not defined returns null.<br>
	 * <p>
	 * It's so possible to get or modify caption, shortKey, separator, object and so on.<br>
	 * <p>
	 * @param menuName
	 * @param controlName
	 */
	public JCheckBoxMenuItem getJMenuCheckBox(String menuName, String controlName) {
		ForwardMenu forwardMenu = null;
			
		// Gets menu
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return new JCheckBoxMenuItem();}

		// Search menu item
		for (ForwardMenuItem forwardMenuItem : forwardMenu.getMenuItems()) {
			if (!forwardMenuItem.getControlName().equals(controlName)) {continue;}
			if (!(forwardMenuItem.getGraphicObject() instanceof JCheckBoxMenuItem)) {continue;}
			return (JCheckBoxMenuItem) forwardMenuItem.getGraphicObject();
		}
		return new JCheckBoxMenuItem();
	}
	 
	/**
	 * Gets the swing {@link JRadioButtonMenuItem} object of the menu item.<br>
	 * <p>
	 * If the menu/submenu or the menu item is not defined returns null.<br>
	 * <p>
	 * It's so possible to get or modify caption, shortKey, separator, object and so on.<br>
	 * <p>
	 * @param menuName
	 * @param controlName
	 */
	public JRadioButtonMenuItem getJMenuRadioButton(String menuName, String controlName) {
		ForwardMenu forwardMenu = null;
			
		// Gets menu
		forwardMenu = this.hm_menu.get(menuName);
		if (forwardMenu == null) {return new JRadioButtonMenuItem();}

		// Search menu item
		for (ForwardMenuItem forwardMenuItem : forwardMenu.getMenuItems()) {
			if (!forwardMenuItem.getControlName().equals(controlName)) {continue;}
			if (!(forwardMenuItem.getGraphicObject() instanceof JRadioButtonMenuItem)) {continue;}
			return (JRadioButtonMenuItem) forwardMenuItem.getGraphicObject();
		}
		return new JRadioButtonMenuItem();
	}
	 

	
	/**
	 * Gets the Swing graphic component declared for the component name as an extended  {@link JComponent} object.<br>
	 * <p>
	 * The caller must do the cast to JPanel, JLabel, JText and so on.<br>
	 * If the component is undefined returns null.<br>
	 * <p>
	 * @param componentName
	 */
	public JComponent getJComponent(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {return null;}
		return (JComponent) innerComponent.component;
	}

	
	/**
	 * Gets the JLabel declared for the name specified.<br>
	 * <p>
	 * If the the labelName is undefined or the name is not of a label, returns null.<br>
	 * <p>
	 * @param labelName
	 */
	public JLabel getJLabel(String labelName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(labelName);
		if (innerComponent == null) {return new JLabel();}
		if (innerComponent.componentType != EnumForwardComponent.JLabel) {return  new JLabel();}
		return (JLabel) innerComponent.component;
	}

	/**
	 * Gets the JButton declared for the name specified.<br>
	 * <p>
	 * If the the button name is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param buttonName
	 */
	public JButton getJButton(String buttonName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(buttonName);
		if (innerComponent == null) {return new JButton();}
		if (innerComponent.componentType != EnumForwardComponent.JButton) {return  new JButton();}
		return (JButton) innerComponent.component;
	}

	/**
	 * Gets the JToggleButton declared for the name specified.<br>
	 * <p>
	 * If the the button name is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param buttonName
	 */
	public JToggleButton getJToggleButton(String buttonName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(buttonName);
		if (innerComponent == null) {return new JToggleButton();}
		if (innerComponent.componentType != EnumForwardComponent.JToggleButton) {return new JToggleButton();}
		return (JToggleButton) innerComponent.component;
	}

	
	/**
	 * Gets the JTextField declared for the name specified.<br>
	 * <p>
	 * If the the textFieldName  is undefined or the name is not of a textField, returns null.<br>
	 * <p>
	 * @param textFieldName
	 */
	public JTextField getJTextField(String textFieldName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(textFieldName);
		if (innerComponent == null) {return new JTextField();}
		if (innerComponent.componentType != EnumForwardComponent.JTextField) {return new JTextField();}
		return (JTextField) innerComponent.component;
	}

	/**
	 * Gets the JProgressBar declared for the name specified.<br>
	 * <p>
	 * If the the progressBarName  is undefined or the name is not of a progressBar, returns null.<br>
	 * <p>
	 * @param progressBarName
	 */
	public JProgressBar getJProgressBar(String progressBarName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(progressBarName);
		if (innerComponent == null) {return new JProgressBar();}
		if (innerComponent.componentType != EnumForwardComponent.JProgressBar) {return new JProgressBar();}
		return (JProgressBar) innerComponent.component;
	}

	/**
	 * Gets the JPasswordField declared for the name specified.<br>
	 * <p>
	 * If the the passwordFieldName  is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param passwordFieldName
	 */
	public JPasswordField getJPasswordField(String passwordFieldName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(passwordFieldName);
		if (innerComponent == null) {return new JPasswordField();}
		if (innerComponent.componentType != EnumForwardComponent.JPasswordField) {return new JPasswordField();}
		return (JPasswordField) innerComponent.component;
	}

	/**
	 * Gets the JFormattedTextField declared for the name specified.<br>
	 * <p>
	 * If the the formattedTextFieldName is undefined or the name is not of a textField, returns null.<br>
	 * <p>
	 * @param formattedTextFieldName
	 */
	public JFormattedTextField getJFormattedTextField(String formattedTextFieldName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(formattedTextFieldName);
		if (innerComponent == null) {return new JFormattedTextField();}
		if (innerComponent.componentType != EnumForwardComponent.JFormattedTextField) {return new JFormattedTextField();}
		return (JFormattedTextField) innerComponent.component;
	}

	/**
	 * Gets the JComboBox declared for the name specified.<br>
	 * <p>
	 * If the the comboBoxName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param comboBoxName
	 */
	@SuppressWarnings("rawtypes")
	public JComboBox getJComboBox(String comboBoxName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(comboBoxName);
		if (innerComponent == null) {return new JComboBox();}
		if (innerComponent.componentType != EnumForwardComponent.JComboBox) {return new JComboBox();}
		return (JComboBox) innerComponent.component;
	}

	
	/**
	 * Gets the JTextArea declared for the name specified.<br>
	 * <p>
	 * If the textAreaName is undefined or the name is not of a textArea, returns a new object with no effects for the application.<br>
	 * <p>
	 * @param textAreaName
	 */
	public JTextArea getJTextArea(String textAreaName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(textAreaName);
		if (innerComponent == null) {return new JTextArea();}
		if (innerComponent.componentType != EnumForwardComponent.JTextArea) {return new JTextArea();		}
		return (JTextArea) innerComponent.component;
	}

	
	/**
	 * Gets the JTextPane declared for the name specified.<br>
	 * <p>
	 * If the textPaneName is undefined or the name is not of a textPane, returns null.<br>
	 * <p>
	 * @param textPaneName
	 */
	public JTextPane getJTextPane(String textPaneName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(textPaneName);
		if (innerComponent == null) {return new JTextPane();}
		if (innerComponent.componentType != EnumForwardComponent.JTextPane) {return new JTextPane();}
		return (JTextPane) innerComponent.component;
	}

		
	/**
	 * Gets the JEditorPane declared for the name specified.<br>
	 * <p>
	 * If the textPaneName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param editorPaneName
	 */
	public JEditorPane getJEditorPane(String editorPaneName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(editorPaneName);
		if (innerComponent == null) {return new JEditorPane();}
		if (innerComponent.componentType != EnumForwardComponent.JEditorPane) {return new JEditorPane();}
		return (JEditorPane) innerComponent.component;
	}

	/**
	 * Gets the JCheckBox declared for the name specified.<br>
	 * <p>
	 * If the checkBoxName is undefined or the name is not of a checkBox, returns null.<br>
	 * <p>
	 * @param checkBoxName
	 */
	public JCheckBox getJCheckBox(String checkBoxName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(checkBoxName);
		if (innerComponent == null) {return new JCheckBox();}
		if (innerComponent.componentType != EnumForwardComponent.JCheckBox) {return new JCheckBox();}
		return (JCheckBox) innerComponent.component;
	}

	/**
	 * Gets the JRadioButton declared for the name specified.<br>
	 * <p>
	 * If the radioButtonName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param radioButtonName
	 */
	public JRadioButton getJRadioButton(String radioButtonName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(radioButtonName);
		if (innerComponent == null) {return new JRadioButton();}
		if (innerComponent.componentType != EnumForwardComponent.JRadioButton) {return new JRadioButton();}
		return (JRadioButton) innerComponent.component;
	}

	/**
	 * Gets the JList declared for the name specified.<br>
	 * <p>
	 * If the listName is undefined or the name is not of a list, returns null.<br>
	 * <p>
	 * @param listName
	 */
	@SuppressWarnings("rawtypes")
	public JList getJList(String listName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(listName);
		if (innerComponent == null) {return new JList();}
		if (innerComponent.componentType != EnumForwardComponent.JList) {return new JList();}
		return (JList) innerComponent.component;
	}

	/**
	 * Gets the JTree declared for the name specified.<br>
	 * <p>
	 * If the treeName is undefined or the name is not of a tree, returns null.<br>
	 * <p>
	 * @param treeName
	 */
	public JTree getJTree(String treeName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(treeName);
		if (innerComponent == null) {return new JTree();}
		if (innerComponent.componentType != EnumForwardComponent.JTree) {return new JTree();}
		return (JTree) innerComponent.component;
	}

	/**
	 * Gets the JPanel declared for the name specified.<br>
	 * <p>
	 * If the panelName is undefined or the name is not of a panel, returns null.<br>
	 * <p>
	 * @param panelName
	 */
	public JPanel getJPanel(String panelName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(panelName);
		if (innerComponent == null) {return new JPanel();}
		if (innerComponent.componentType != EnumForwardComponent.JPanel) {return new JPanel();}
		return (JPanel) innerComponent.component;
	}
	
	/**
	 * Gets the JDialog declared for the name specified.<br>
	 * <p>
	 * A JDialog is generated automatically when a form of type Dialog is declared.
	 * The name of the form is assigned to the object {@link JDialog}.<br>
	 * The JDoialog content pane will be the root panel of the form.<br>
	 * If the form name is undefined or the name is not of a panel, returns a service {@link JDialog} object.<br>
	 * <p>
	 * @param formName
	 */
	public JDialog getJDialog(String formName) {
		ForwardForm formDialog = null;
		formDialog = getForm(formName);
		if (formDialog == null) {return new JDialog();}
		if (formDialog.getFormType() != EnumForwardOption.FORM_TYPE_DIALOG) {return new JDialog();}
		if (formDialog.getDialog() == null) {return new JDialog();}
		return formDialog.getDialog();
	}

	
	/**
	 * Gets the JTable declared for the name specified.<br>
	 * <p>
	 * If the tableName is undefined or the name is not of a table, returns null.<br>
	 * <p>
	 * @param tableName
	 */
	public JTable getJTable(String tableName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(tableName);
		if (innerComponent == null) {return new JTable();}
		if (innerComponent.componentType != EnumForwardComponent.JTable) {return new JTable();}
		return (JTable) innerComponent.component;
	}

	
	/**
	 * Gets the JSlider declared for the name specified.<br>
	 * <p>
	 * If the sliderName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param sliderName
	 */
	public JSlider getJSlider(String sliderName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(sliderName);
		if (innerComponent == null) {return new JSlider();}
		if (innerComponent.componentType != EnumForwardComponent.JSlider) {return new JSlider();}
		return (JSlider) innerComponent.component;
	}

	/**
	 * Gets the JSpinner declared for the name specified.<br>
	 * <p>
	 * If the spinnerName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param spinnerName
	 */
	public JSpinner getJSpinner(String spinnerName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(spinnerName);
		if (innerComponent == null) {return new JSpinner();}
		if (innerComponent.componentType != EnumForwardComponent.JSpinner) {return new JSpinner();}
		return (JSpinner) innerComponent.component;
	}

	/**
	 * Gets the JSplitPane declared for the name specified.<br>
	 * <p>
	 * If the splitPaneName is undefined returns null.<br>
	 * <p>
	 * @param splitPaneName
	 */
	public JSplitPane getJSplitPane(String splitPaneName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(splitPaneName);
		if (innerComponent == null) {return  new JSplitPane();}
		if (innerComponent.componentType != EnumForwardComponent.JSplitPane) {return new JSplitPane();}
		return (JSplitPane) innerComponent.component;
	}

	/**
	 * Gets the JTabbedPane declared for the name specified.<br>
	 * <p>
	 * If the tabbedPaneName is undefined returns null.<br>
	 * <p>
	 * @param tabbedPaneName
	 */
	public JTabbedPane getJTabbedPane(String tabbedPaneName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(tabbedPaneName);
		if (innerComponent == null) {return new JTabbedPane();}
		if (innerComponent.componentType != EnumForwardComponent.JTabbedPane) {return  new JTabbedPane();}
		return (JTabbedPane) innerComponent.component;
	}

	/**
	 * Gets the JScrollPane declared for the component specified.<br>
	 * <p>
	 * A JScrollPane is automatically defined at declaring time for JTextArea, JList and JTable.<br>
	 * It's addressed by the same name of the viewport object by this method.<br>
	 * If the scrollPane is undefined returns a new JScrollPane object with no effects for the application.<br>
	 * <p>
	 * @param componentName
	 */
	public JScrollPane getJScrollPane(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {return new JScrollPane();}
		if (innerComponent.jscrollPane == null) {return new JScrollPane();}
		return (JScrollPane) innerComponent.jscrollPane;
	}

	/**
	 * Gets the JToolBar declared for the name specified.<br>
	 * <p>
	 * If the toolbarName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param toolbarName
	 */
	public JToolBar getJToolBar(String toolbarName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(toolbarName);
		if (innerComponent == null) {return new JToolBar();}
		if (innerComponent.componentType != EnumForwardComponent.JToolBar) {return new JToolBar();}
		return (JToolBar) innerComponent.component;
	}

	
	/**
	 * Gets the JSeparator declared for the name specified.<br>
	 * <p>
	 * If the separatorName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param separatorName
	 */
	public JSeparator getJSeparator(String separatorName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(separatorName);
		if (innerComponent == null) {return  new JSeparator();}
		if (innerComponent.componentType != EnumForwardComponent.JSeparator) {return new JSeparator();}
		return (JSeparator) innerComponent.component;
	}
	
	/**
	 * Gets the JCheckBoxMenuItem declared for the name specified.<br>
	 * <p>
	 * If the separatorName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param checkBoxMenuItemName
	 */
	public JCheckBoxMenuItem getJCheckBoxMenuItem(String separatorName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(separatorName);
		if (innerComponent == null) {return new JCheckBoxMenuItem();}
		if (innerComponent.componentType != EnumForwardComponent.JCheckBoxMenuItem) {return  new JCheckBoxMenuItem();}
		return (JCheckBoxMenuItem) innerComponent.component;
	}

	/**
	 * Gets the JRadioButtonMenuItem declared for the name specified.<br>
	 * <p>
	 * If the radioButtonMenuItemName is undefined or the name is not of a button, returns null.<br>
	 * <p>
	 * @param String radioButtonMenuItemName
	 */
	public JRadioButtonMenuItem getJRadioButtonMenuItem(String radioButtonMenuItemName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(radioButtonMenuItemName);
		if (innerComponent == null) {return new JRadioButtonMenuItem();}
		if (innerComponent.componentType != EnumForwardComponent.JRadioButtonMenuItem) {return new JRadioButtonMenuItem();}
		return (JRadioButtonMenuItem) innerComponent.component;
	}

	
	/**
	 * Gets the object {@link ForwardTableModel} of the table specified.<br>
	 * <p>
	 * The {@link ForwardTableModel} object extends the standard java {@link AbstractTableModel} adding<br>
	 * specific organized and structured forward informations.<br>
	 * It depics columns header, type, tooltip, values, embedded objects, editor, rendering an so on.<br>
	 * <p>
	 * If the tableName is undefined or the name is not of a table, returns a defaul object with no effects for the application.<br>
	 * <p>
	 * @param tableName
	 */
	public ForwardTableModel getModelJTable(String tableName) {
		ForwardPanelComponent panelComponent = null;
		ForwardTableModel tableModel = null;
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(tableName);
		if (innerComponent == null) {return new ForwardTableModel(new JTable());}
		if (innerComponent.componentType != EnumForwardComponent.JTable) {return new ForwardTableModel(new JTable());}
		
		// Il modello viene recuperata dalla definizione del campo nel suo pannello.
		// Non è stato ancora assegnato all'oggetto JTable e lo è in fase di esecuzione
		panelComponent = this.getPanelComponent(tableName);
		if (panelComponent == null) {return new ForwardTableModel(new JTable());}

		// Modello tabella
		tableModel = panelComponent.getTableModel();
		
		return tableModel;
	}

	/**
	 * Gets the object {@link ForwardListModel} of the list specified.<br>
	 * <p>
	 * The {@link ForwardListModel} object extends the standard java {@link DefaultListModel} adding<br>
	 * specific organized and structured forward informations.<br>
	 * It depics values and embedded objects.<br>
	 * <p>
	 * If the list is undefined or the name is not of a list, returns a defaul object with no effects for the application.<br>
	 * <p>
	 * @param listName 
	 */
	@SuppressWarnings("rawtypes")
	public ForwardListModel getModelJList(String listName) {
		JList jlist = null;
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(listName);
		if (innerComponent == null) {return new ForwardListModel();}
		if (innerComponent.componentType != EnumForwardComponent.JList) {return new ForwardListModel();}
		jlist = (JList) innerComponent.component;
		return (ForwardListModel) jlist.getModel();
	}

	/**
	 * Gets the object {@link ForwardComboBoxModel} of the comboBox specified.<br>
	 * <p>
	 * The {@link ForwardComboBoxModel} object extends the standard java {@link DefaultComboBoxModel} adding<br>
	 * specific organized and structured forward informations.<br>
	 * It depics values and embedded objects.<br>
	 * <p>
	 * If the comboBox is undefined or the name is not of a comboBox, returns a defaul object with no effects for the application.<br>
	 * <p>
	 * @param comboBoxName
	 */
	@SuppressWarnings("rawtypes")
	public ForwardComboBoxModel getModelJComboBox(String comboBoxName) {
		JComboBox jcomboBox = null;
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(comboBoxName);
		if (innerComponent == null) {return new ForwardComboBoxModel();}
		if (innerComponent.componentType != EnumForwardComponent.JComboBox) {return new ForwardComboBoxModel();}
		jcomboBox = (JComboBox) innerComponent.component;
		return (ForwardComboBoxModel) jcomboBox.getModel();
	}


	
	
	/**
	 * Gets the Swing Timer declared for the name specified.<br>
	 * <p>
	 * If the timer is undefined or the name is not of a timer, returns null.<br>
	 * <p>
	 * @param String timerName
	 * @return a Timer object or null
	 */
	public Timer getTimer(String timerName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(timerName);
		if (innerComponent == null) {return new Timer(0, null);}
		if (innerComponent.componentType != EnumForwardComponent.TimerSwing) {return new Timer(0, null);}
		return (Timer) innerComponent.component;
	}


	/**
	 * Gets the forward definition for the panel.<br>
	 * <p>
	 * If the panel is undefined returns null.<br>
	 * <p>
	 * @param String panelName
	 */
	public ForwardPanel getPanel(String panelName) {
		
		ForwardPanel panel = null;
		panel = this.hm_panel.get(panelName);
		return panel;
	}

	/**
	 * Gets the panel name owner of a component.<br>
	 * <p>
	 * If the component is undefined returns an empty string.<br>
	 * <p>
	 * @param String componentName
	 */
	public String getPanelOwner(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {
			return "";
		}
		return innerComponent.panelName;
	}


	/**
	 * Gets constaints, as an {@link GridBagConstraints} object for the control with the name specified declared for the panel.<br>
	 * <p>
	 * If the panel or the control name are undefined returns null.<br>
	 * <p>
	 * @param String panelName
	 * @param String controlName
	 */
	public GridBagConstraints getComponentConstraints(String panelName, String componentName) {
		
		ForwardPanel panel = null;
		ForwardPanelComponent panelField = null;
		
		panel = this.hm_panel.get(panelName);
		if (panel == null) {return null;}
		panelField = panel.getComponent(componentName);
		if (panelField == null) {return null;}
		return panelField.getConstraints();
	}

	
	
	/**
	 * Returns a map with default properties declared for the function.<br>
	 * <p>
	 * @return the hm_propertyDefault
	 */
	public Map<EnumForwardComponent, ArrayList<InnerComponentProperty>> getPropertiesDefault() {
		return hm_propertyDefault;
	}




	/**
	 * Return  all declarations of events to be managed as a list.<br>
	 * <p>
	 * @return the al_onEvent
	 */
	public ArrayList<InnerOnEvent> getOnEventList() {
		return al_onEvent;
	}


	/**
	 * Return  all declarations of events to be managed as a map.<br>
	 * <p>
	 * @return the hm_onEvent
	 */
	public Map<String, InnerOnEvent> getOnEventMap() {
		return hm_onEvent;
	}


	/**
	 * Return  all groups of actions to be managed as a map.<br>
	 * <p>
	 * @return the hm_groupActions
	 */
	public Map<String, InnerActionsGroup> getGroupActionsMap() {
		return hm_groupActions;
	}



	/**
	 * Return a list with all panels declared for the function.<br>
	 * <p>
	 * 
	 * @return the List<ForwardPanel>
	 */
	public List<ForwardPanel> getPanels() {
		return (List<ForwardPanel>) hm_panel.values();
	}

	/**
	 * Return the panel descriptor declared by function.<br>
	 * <p>
	 * @param panelName
	 * @return the ForwardPanel object
	 */
	public ForwardPanel getPanelDescriptor(String panelName) {
		return hm_panel.get(panelName);
	}


	/**
	 * Return a list with only detail panels declared for the function, as coded by forward.<br>
	 * <p>
	 * Detail panels are {@link JComponent} container containing Swing controls.<br>
	 * <p>
	 * @return the List<ForwardPanel>
	 */
	public List<ForwardPanel> getPanelsDetail() {
		List<ForwardPanel> ls_panels = null;
		ls_panels = new ArrayList<ForwardPanel> ();
		
		// Scan panel dichiarati
		for (Entry<String, ForwardPanel> entryPanel : hm_panel.entrySet()) {
			if (entryPanel.getValue().getType() != EnumForwardPanelType.DETAIL) {continue;}
			ls_panels.add(entryPanel.getValue());
		}
		
		return ls_panels;
	}

	/**
	 * Return a {@link InnerComponent} object describing the java JTable declared by function.<br>
	 * <p>
	 * The {@link JTable} object is searched using its {@link ListSelectionModel} object.<br>
	 * If no {@link JTable} found then will be returned null.
	 * <p>
	 * @return the declared component as a {@link InnerComponent} object.
	 */
	public InnerComponent getComponentJTable(ListSelectionModel listSelectioModel) {
		JTable jtable = null;
		InnerComponent innerComponent = null;
		
		// Scan componenti dichiarati
		for (Entry<String, InnerComponent> entryComponent : this.hm_component.entrySet()) {
			innerComponent = entryComponent.getValue();
			
			// Interessa solo la JTable con il listSelectioModel fornito
			if (innerComponent.componentType != EnumForwardComponent.JTable) {continue;}
			jtable = (JTable) innerComponent.component;
			if (jtable.getSelectionModel() != listSelectioModel) {continue;}
			
			// E' la JTable con il ListSelectionModel richiesto
			return innerComponent;
		}
		
		return null;
	}

	/**
	 * Return the name of declared component implemented by a swing object or another java object.<br>
	 * <p>
	 * The declared component could be, for example, a swing JButton object or a swing Timer object and so on.<br>
	 * <p>
	 * If the component object is undefined will be returned an empty string.<br>
	 * <p>
	 * @return the name of the component
	 */
	public String getComponentName(Object component) {
		InnerComponent innerComponent = null;
		String componentName = "";
		
		// Scan componenti dichiarati dalla funzione
		for (Entry<String, InnerComponent> entryComponent : this.hm_component.entrySet()) {
			innerComponent = entryComponent.getValue();
			if (innerComponent.component != component) {continue;}
			// Oggetto individuato
			componentName = entryComponent.getKey();
            break;
		}
		return componentName;
	}

	/**
	 * Return a {@link InnerComponent} object describing the java component declared by function.<br>
	 * <p>
	 * If the componentName is undefined will be returned null.<br>
	 * <p>
	 * @return the panel component
	 */
	public InnerComponent getComponentDeclared(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		return innerComponent;
	}

	/**
	 * Return a map with all components declared.<br>
	 * The key is the component name an data is {@link InnerComponent} object describing the java component.<br>
	 * <p>
	 * @return the map with java components
	 */
	public Map<String, InnerComponent>  getComponentsMap() {
		return this.hm_component;
	}

	/**
	 * Return a {@link ForwardPanelComponent} object describing a swing {@link JComponent}.<br>
	 * <p>
	 * This method is intended for components layed out on a panel.<br>
	 * Components have been laid out on a specific panel but it's not necessary to specify which<br>
	 * to get the component descriptor.<br>
	 * <p>
	 * If the componentName is undefined or it has been not layed out on a panel, will be returned null.<br>
	 * <p>
	 * @return the panel where the panel has been layed out
	 */
	public ForwardPanelComponent getPanelComponent(String componentName) {
		ForwardPanel forwardPanel = null;
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) { return null;}
		if (!innerComponent.menuName.equals("")) {return null;}
		forwardPanel = this.getPanel(innerComponent.panelName);
		if (forwardPanel == null) {return null;}
		return forwardPanel.getComponent(componentName);
	}

	
	/**
	 * Return a {@link EnumForwardComponent} component for a swing object.<br>
	 * <p>
	 * The component can be a 	JFrame,	JApplet, JPanel, JDialogPanel, JTabbedPanel, JSplitPanel,<br>
	 * or any component laid out on a specific panel.<br>
	 * <p>
	 * If the componentName is undefined will be returned null.<br>
	 * <p>
	 * @return the panel component
	 */
	public EnumForwardComponent getComponentType(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) { return null;}
		return innerComponent.componentType;
	}


	
	

	/*
	 * Inserimento componente in struttura map interna.<br>
	 * <p>
	 * Restituisce l'oggetto InnerComponent inserito<br>
	 * <p>
	 * @param String componentName
	 * @param EnumForwardJComponent componentType
	 * @param JComponent jcomponent
	 */
	private InnerComponent putComponentOnStructure(String componentName, EnumForwardComponent componentType, Object component) {
		InnerComponent innerComponent = null;
		innerComponent = new InnerComponent();
		innerComponent.panelName = this.activePanelName;
		innerComponent.componentName = componentName;
		innerComponent.componentType = componentType;
		innerComponent.component = component;
		this.hm_component.put(componentName, innerComponent);
		return innerComponent;
	}

	/*
	 * Inserimento variabile in struttura map interna.<br>
	 * <p>
	 * Restituisce l'oggetto InnerComponent inserito<br>
	 * <p>
	 * @param String componentName
	 * @param EnumForwardJComponent componentType
	 * @param JComponent jcomponent
	 */
	private InnerComponent putVarOnStructure(String componentName, EnumForwardComponent componentType, Object component) {
		InnerComponent innerComponent = null;
		innerComponent = new InnerComponent();
		innerComponent.panelName = this.activePanelName;
		innerComponent.componentName = componentName;
		innerComponent.componentType = componentType;
		innerComponent.component = component;
		this.hm_var.put(componentName, innerComponent);
		return innerComponent;
	}

	/*
	 * Impostazione proprietà di default definite per il tipo componente.
	 * 
	 * Si utilizzano le proprietà di default definite a livello di funzione e vengono eseguite via reflection.
	 * 
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void setComponentPropertiesDefault(Class componentClass, EnumForwardComponent componentType, Object component) {
		
		Class<? extends JComponent> ar_class[] = null;
		Object ar_object[] = null;
		ArrayList<InnerComponentProperty> al_propertyDefault = null;
		
		al_propertyDefault = this.getPropertiesDefault().get(componentType);
		if (al_propertyDefault == null) {
			return;
		}
		
		// Default presenti per il tipo componente
		
		// Scan properties di default 
		for (InnerComponentProperty propertyDefault : al_propertyDefault) {
			ar_class = new Class[1];
			ar_class[0] = propertyDefault.propertyValueClass;
			ar_object = new Object[1];
			ar_object[0] = propertyDefault.propertyValue;
			this.rm.invokeMethod(component, "set"+propertyDefault.propertyName, ar_class, ar_object);
		}
	}


	/**
	 * Gets if the component named has been declared in the function.<br>
	 * <p>
	 * @param String componentName
	 */
	public boolean isComponentDeclared(String componentName) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {return false;}
		return true;
	}

	/**
	 * Gets if the component named has been declared in the function of the specified type.<br>
	 * <p>
	 * @param String componentName
	 */
	public boolean isComponentType(String componentName, EnumForwardComponent componentType) {
		InnerComponent innerComponent = null;
		innerComponent = this.hm_component.get(componentName);
		if (innerComponent == null) {return false;}
		if (innerComponent.componentType != componentType) {return false;}
		return true;
	}

	/**
	 * Returns an instance of the internal class describinig an event.<br>
	 * <p>
	 */
	public InnerOnEvent factoryGetInstanceInnerOnEvent() {
		return new InnerOnEvent();
	}


	
	/////////////////////////////////////////////////////////////////////////////////////////////
	///// Metodi privati  
	////////////////////////////////////////////////////////////////////////////////////////////
	
	
    /* Restituisce un ImageIcon, or null se il path e' errato
     * 
     * Si prende come riferimento il path dove è memorizzata la classe di definizione della funzione.
     * Se il path prevede dei folder, questi sono nella stessa directory della funzione
     *  
    */ 
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = this.getClass().getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
             return null;
        }
    }

    
	/////////////////////////////////////////////////////////////////////////////////////////////
	///// Metodi di gestione logiche applicative
	////////////////////////////////////////////////////////////////////////////////////////////

	/** <h4>Perform unconditionally an action</h4> 
	 * 
	 * Actions are ranked by {@link EnumForwardAction}, implemented by methods starting by <code>DO</code>, and can be automatically activated by declaring an action<br>
	 * on a <code>ON_EVENT()</code> or invoking a <code>ACTION()</code> directive in any application reusable methos.<br>
	 * <p>
	 * In the first case the activation is made automatically by the forward monitor when the event specified occurs.<br>
	 * In the second case the activation is fully indipendent and piloted only by specific user java application code logic.<br>
	 * Furthermore, the user java application code can itself be activated by an <code>ON_EVENT()</code> declaration and<br>
	 * phisically the code (a normal java method) can be coded in the same class declaring the function (a <code>ForwardFunction</code> object)<br>
	 * or in any other class configutated runtime by forward monitor.<br>
	 * <br>
	 * This logic execution model lets to configure runtime whith no coding wich elementary code to execute, making<br>
	 * the application like a big puzzle where any piece can be changed without to modify the shape.<br> 
	 * <p>
	 * As final result forward realize an applicative behaviour that can be completely configuration dependent and <br>
	 * even any bit of code fully reusable.<br>
	 * The application model and any user application code can be deployed and executed in the web context, by the specific<br>
	 * monitor (a specialized servelet), whith no changes.
	 * <p>
	 * For example you could code an <code>ACTION</code<br> that under specific circumstances causes the refresh of<br>
	 * a panel, the execution of a logica data view or discard a row in table under loading.
	 * <p>
	 * Here one example:<br>
	 * <p>
	 * <pre>
	 * ON_EVENT(EnumForwardEvent.value, DO_REFRESH("fieldName")
	 * </pre>
	 * @param action do to
	 * @param object parameters depending on action type
	 */
	public void ACTION(ForwardDoParms ... actionsCoded) {

		for (ForwardDoParms actionCoded : actionsCoded) {
			InnerOnEvent innerOnEvent = null;
			innerOnEvent = new InnerOnEvent();
			innerOnEvent.action = actionCoded.action;
			innerOnEvent.componentName = "";
			innerOnEvent.al_actionCoded.add(actionCoded);
			this.monitor.actionStarter(this.monitor.mcb, actionCoded.action, actionCoded);
		}
	}

   
	/**
	 * Sets panel components to the declared default<br>
	 * <p>
	 * For all elementary components layed out on the panel, will be applied the declared default<br>
	 * When no default has been declared the component value will be set to space or zero depending on the type.<br>
	 * <p>
	 * If the panel has be not declared by function, no action will be taken.<br>
	 * <p>
	 * @param panelName the panel name to which apply defaults
	 */
	public ForwardDoParms DO_PANEL_DEFAULTS_INITIAL(String ... panelNames) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelNames = panelNames;
		p.action = EnumForwardAction.PANEL_DEFAULTS_INITIAL;
		return p;
	}

	/**
	 * Call a user default method for the panel<br>
	 * <p>
	 * Application default logics are coded in a user declared method.<br>
	 * The method will be called on demand by aan ACTION() rirective or by an ON_condition() event.<br>
	 * <p>
	 * If the panel has be not declared by function, no action will be taken.<br>
	 * If the method has be not declared by function, no action will be taken.<br>
	 * <p>
	 * @param panelName the panel name to which apply defaults
	 */
	public ForwardDoParms DO_PANEL_DEFAULTS_USER(String panelName, String methodName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = panelName;
		p.methodName = methodName;
		p.action = EnumForwardAction.PANEL_DEFAULTS_USER;
		return p;
	}

	/**
	 * Populates the panel using the logical data view, if any.<br>
	 * <p>
	 * If no DATA_SOURCE() has been declared at function declaration level for the panel, 
	 * specifying the logical data view to use, no action will be taken.<br>
	 * <p>
	 * The logical data view will be executed and row data column values will be used to update<br>
	 * swing controls declared by panel.<br>
	 * <p>
	 * For about controls like combobox, list, and table, when a logical data view has been<br>
	 * declared, will be populated too.<br>
	 * <p>
	 * @param panelName the panel name to wich apply defaults
	 */
	public ForwardDoParms DO_PANEL_POPULATE_FROM_DB(String panelName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = panelName;
		p.panelLdvClassName = "";
		p.action = EnumForwardAction.PANEL_POPULATE_FROM_DB;
		return p;
	}

	/**
	 * Populates the panel using the input logical data view.<br>
	 * <p>
	 * If the logical data view is not defined no action will be taken. 
	 * <p>
	 * The logical data view will be executed and row data column values will be used to update<br>
	 * swing controls declared by panel.<br>
	 * <p>
	 * For about controls like combobox, list, and table, when a logical data view has been<br>
	 * declared, will be populated too.<br>
	 * <p>
	 * @param panelName the panel name to wich apply defaults
	 * @param ldvClassName the logical data view class name to be use to populate the panel
	 */
	public ForwardDoParms DO_PANEL_POPULATE_FROM_DB(String panelName, String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = panelName;
		p.panelLdvClassName = ldvClassName;
		p.action = EnumForwardAction.PANEL_POPULATE_FROM_DB;
		return p;
	}

	
	/**
	 * Executes all controls in sequence
	 * <p>
	 * It will be executed the sequence of controls below <pre>
	 * CONTROLS_FORMAL
	 * CONTROLS_EXISTENCE_ENTITY
	 * CONTROLS_EXISTENCE_RULE_TABLE
	 * CONTROLS_USER <pre>
	 * <br>
	 * User controls will be executed only if previosly controls ended with no error.<br>
	 * Overall errors detected will be showed toghether by means of the forward monitor.<br>
	 * <p>
	 * @param panelName the panel name to control
	 * @param methodName the name of the application method, not active if an empty string
	 */
	public ForwardDoParms DO_PANEL_CONTROLS(String panelName, String methodName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelName = panelName;
		p.methodName = methodName;
		p.action = EnumForwardAction.PANEL_CONTROLS;
		return p;
	}

	/**
	 * Executes formal controls for components layed out by panel<br>
	 * <p>
	 * They are controls for about:<pre> 
	 * Numeric/not numeric
	 * Max and Min value
	 * Range
	 * Decimal places 
	 * Decimal value<pre>
	 * <br>
	 * All controls, to be effective, must be declared for the component by means of the<br>
	 * <code>COMPONENT_OPTIONS()</code> declarative.
	 * <p>
	 * @param panelName the panel name to control
	 */
	public ForwardDoParms DO_PANEL_CONTROLS_FORMAL(String panelName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelName = panelName;
		p.methodName = "";
		p.action = EnumForwardAction.PANEL_CONTROLS_FORMAL;
		return p;
	}

	/**
	 * Controls the existence and not existence of entities linked to panel controls.<br>
	 * <p>
	 * @param panelName the panel name to wich apply defaults
	 */
	public ForwardDoParms DO_PANEL_CONTROLS_EXISTENCE_ENTITY(String panelName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelName = panelName;
		p.action = EnumForwardAction.PANEL_CONTROLS_EXISTENCE_ENTITY;
		return p;
	}

	/**
	 * Controls the existence or not existence of rule tables with keys linked to panel controls.<br>
	 * <p>
	 * @param panelName the panel name to wich apply defaults
	 */
	public ForwardDoParms DO_PANEL_CONTROLS_EXISTENCE_RULE_TABLE(String panelName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelName = panelName;
		p.action = EnumForwardAction.PANEL_CONTROLS_EXISTENCE_RULE_TABLE;
		return p;
	}

	
	/**
	 * Executes panel application user context controls.<br>
	 * <p>
	 * This action allow you to declare an application method responsible for<br>
	 * controls among controls in the panel or others panels<br>
	 * <p>
	 * If the application method is not declared by function, no action will be taken.<br>
	 * <p>
	 * The input application method can query the state of all function, and<br>
	 * the value of all declared controls.<br>
	 * It must call the system <code>setActivePanelContextControlsGood()</code> with a
	 * value of true or false, to inform the forward monitor of the result of controls.<br>
	 * <p>
	 * @param panelName the panel name to wich apply defaults
	 * @param methodName the application method name to be executed
	 */
	public ForwardDoParms DO_PANEL_CONTROLS_USER(String panelName, String methodName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.panelName = panelName;
		p.methodName = methodName;
		p.action = EnumForwardAction.PANEL_CONTROLS_USER;
		return p;
	}

	
	/**
	 * Starts a dialog to choose files from file system<br>
	 * <p>
	 * There will be displayed both files and directories.<br>
	 * <p>
	 * @param fileChooserName used as a dialog identifier inside application logic
	 * @param fileChooserTitle
	 * @param isFileChooserMultiSelectionEnabled
	 */
	public ForwardDoParms DO_DIALOG_START_FILE_OPEN_CHOOSER(String fileChooserName, String fileChooserTitle, boolean isFileChooserMultiSelectionEnabled) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = fileChooserName;
		p.fileChooserName = fileChooserName;
		p.fileChooserTitle = fileChooserTitle;
		p.isFileChooserMultiSelectionEnabled = isFileChooserMultiSelectionEnabled;
		p.fileChooserFileSelectionMode = JFileChooser.FILES_AND_DIRECTORIES;
		p.action = EnumForwardAction.DIALOG_START_FILE_OPEN_CHOOSER;
		return p;
	}

	/**
	 * Starts a dialog to choose files from file system<br>
	 * <p>
	 * 
	 * @param fileChooserName used as a dialog identifier inside application logic
	 * @param fileChooserTitle
	 * @param isFileChooserMultiSelectionEnabled
	 * @param fileChooserFileSelectionMode as <pre>  
	 *         FileChooser.FILES_AND_DIRECTORIES 
	 *         JFileChooser.FILES_ONLY
	 *         JFileChooser.DIRECTORIES_ONLY
	 * </pre>
	 */
	public ForwardDoParms DO_DIALOG_START_FILE_OPEN_CHOOSER(String fileChooserName, String fileChooserTitle, boolean isFileChooserMultiSelectionEnabled, int fileChooserFileSelectionMode) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = fileChooserName;
		p.fileChooserName = fileChooserName;
		p.fileChooserTitle = fileChooserTitle;
		p.isFileChooserMultiSelectionEnabled = isFileChooserMultiSelectionEnabled;
		p.fileChooserFileSelectionMode = fileChooserFileSelectionMode;
		p.action = EnumForwardAction.DIALOG_START_FILE_OPEN_CHOOSER;
		return p;
	}


	
	/**
	 * Starts a dialog to choose files from file system<br>
	 * <p>
	 * There will be displayed both files and directories.<br>
	 * <p>
	 * @param fileChooserName used as a dialog identifier inside application logic
	 * @param fileChooserTitle
	 * @param fileChooserMultiSelectionEnabled
	 */
	public ForwardDoParms DO_DIALOG_START_FILE_SAVE_CHOOSER(String fileChooserName, String fileChooserTitle, boolean isFileChooserMultiSelectionEnabled) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = fileChooserName;
		p.fileChooserName = fileChooserName;
		p.fileChooserTitle = fileChooserTitle;
		p.isFileChooserMultiSelectionEnabled = isFileChooserMultiSelectionEnabled;
		p.fileChooserFileSelectionMode = JFileChooser.FILES_AND_DIRECTORIES;
		p.action = EnumForwardAction.DIALOG_START_FILE_SAVE_CHOOSER;
		return p;
	}

	/**
	 * Starts a dialog to choose files from file system<br>
	 * <p>
	 * 
	 * @param fileChooserName used as a dialog identifier inside application logic
	 * @param fileChooserTitle
	 * @param fileChooserMultiSelectionEnabled
	 * @param fileChooserFileSelectionMode as <pre>  
	 *         FileChooser.FILES_AND_DIRECTORIES 
	 *         JFileChooser.FILES_ONLY
	 *         JFileChooser.DIRECTORIES_ONLY
	 * </pre>
	 */
	public ForwardDoParms DO_DIALOG_START_FILE_SAVE_CHOOSER(String fileChooserName, String fileChooserTitle, boolean isFileChooserMultiSelectionEnabled, int fileChooserFileSelectionMode) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = fileChooserName;
		p.fileChooserName = fileChooserName;
		p.fileChooserTitle = fileChooserTitle;
		p.isFileChooserMultiSelectionEnabled = isFileChooserMultiSelectionEnabled;
		p.fileChooserFileSelectionMode = fileChooserFileSelectionMode;
		p.action = EnumForwardAction.DIALOG_START_FILE_SAVE_CHOOSER;
		return p;
	}


	/**
	 * Starts a dialog to choose files from file system<br>
	 * <p>
	 * 
	 * @param colorChooserName used as a dialog identifier inside application logic
	 * @param colorChooserTitle
	 * @param colorChooserInitial
	 */
	public ForwardDoParms DO_DIALOG_START_COLOR_CHOOSER(String colorChooserName, String colorChooserTitle, Color colorChooserInitial) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = colorChooserName;
		p.colorChooserName = colorChooserName;
		p.colorChooserTitle = colorChooserTitle;
		p.colorChooserInitial = colorChooserInitial;
		p.action = EnumForwardAction.DIALOG_START_COLOR_CHOOSER;
		return p;
	}


	/**
	 * Starts a standard dialog error for a plain message OK_OPTION with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 */
	public ForwardDoParms DO_DIALOG_START_PLAIN_MESSAGE(String dialogName, String title, String message) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_PLAIN_MESSAGE;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.PLAIN_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.DEFAULT_OPTION;  
		return p;
		
	}


     /**
	 * Starts a standard dialog for a plain message OK_OPTION with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param labelsOptionCustom an string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_PLAIN_MESSAGE(String dialogName, String title, String message, boolean modal, String iconPath, String Icon, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_PLAIN_MESSAGE;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.PLAIN_MESSAGE;						    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_OPTION;  						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected; 
		return p;
	}


		
     /**
	 * Starts a standard dialog error for an information message with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 */
	public ForwardDoParms DO_DIALOG_START_INFORMATION(String dialogName, String title, String message, int typeOptions) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INFORMATION;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.INFORMATION_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;	
		p.dialogTypeOptions = typeOptions;  
		return p;
	}


     /**
	 * Starts a standard dialog OK_OPTION for an information message with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param labelsOptionCustom a string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_INFORMATION(String dialogName, String title, String message, boolean modal, String iconPath, String Icon, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INFORMATION;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.INFORMATION_MESSAGE;						    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_OPTION;  	 						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected;
		return p;
	}

		
	     /**
		 * Starts a standard dialog OK_OPTION for a warning message with minimal parameters<br>
		 * <p>
		 * @param dialogName used as a dialog identifier inside application logic
		 * @param title the title of window message
		 * @param message the message to be displayed
		 * @param typeOptions as a constant <code>JOptionPane YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
		 */
		public ForwardDoParms  DO_DIALOG_START_WARNING(String dialogName, String title, String message, int typeOptions) {
			ForwardDoParms p = null;
			p = new ForwardDoParms();
			p.action = EnumForwardAction.DIALOG_START_WARNING;
			p.dialogName = dialogName;				 
			p.dialogType = JOptionPane.WARNING_MESSAGE;				    
			p.dialogTitle = title;						 
			p.dialogMessage = message;						 
			p.dialogTypeOptions = typeOptions;  
			return p;
		}

	
     /**
	 * Starts a standard dialog OK_OPTION for a warning message with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param labelsOptionCustom a string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_WARNING(String dialogName, String title, String message, boolean modal, String iconPath, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_WARNING;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.WARNING_MESSAGE;				    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.DEFAULT_OPTION;  
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected;  	
		return p;
	}

    /**
	 * Starts a standard dialog OK_OPTION for an error message with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 */
	public ForwardDoParms DO_DIALOG_START_ERROR(String dialogName, String title, String message) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_ERROR;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.ERROR_MESSAGE;								    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.DEFAULT_OPTION;   	
		return p;
	}

    /**
	 * Starts a standard dialog OK_OPTION for an error message with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param typeOptions as a constant <code>JOptionPane YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	 */
	public ForwardDoParms DO_DIALOG_START_ERROR(String dialogName, String title, String message, int typeOptions) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_ERROR;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.ERROR_MESSAGE;								    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = typeOptions;   	
		return p;
	}
 

     /**
	 * Starts a standard dialog OK_OPTION for an error message with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param labelsOptionCustom a string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_ERROR(String dialogName, String title, String message, boolean modal, String iconPath, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_ERROR;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.ERROR_MESSAGE;								    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_OPTION; 				 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected;  
		return p;
	}

    /**
	 * Starts a standard dialog for a question message YES/NO options with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 */
	public ForwardDoParms DO_DIALOG_START_QUESTION(String dialogName, String title, String message) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.QUESTION_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.YES_NO_OPTION;
		p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
		return p;
	}

		
     /**
	 * Starts a standard dialog for a question message with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param typeOptions as a constant <code>JOptionPane YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	 */
	public ForwardDoParms DO_DIALOG_START_QUESTION(String dialogName, String title, String message, int typeOptions) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.QUESTION_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = typeOptions;
		
		switch (typeOptions) {
			case JOptionPane.YES_NO_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			case JOptionPane.YES_NO_CANCEL_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO_CANCEL;
				break;
			case JOptionPane.OK_CANCEL_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_OK_CANCEL;
				break;
			case JOptionPane.DEFAULT_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			default:
				p.dialogTypeOptions = JOptionPane.DEFAULT_OPTION;  		
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			}
		return p;
	}

	
     /**
	 * Starts a standard dialog for an question message with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param typeOptions as a constant <code>JOptionPane YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 */
	public ForwardDoParms DO_DIALOG_START_QUESTION(String dialogName, String title, String message, int typeOptions, boolean modal, String iconPath) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.QUESTION_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath; 
		p.dialogTypeOptions = typeOptions;  
		switch (typeOptions) {
			case JOptionPane.YES_NO_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			case JOptionPane.YES_NO_CANCEL_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO_CANCEL;
				break;
			case JOptionPane.OK_CANCEL_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_OK_CANCEL;
				break;
			case JOptionPane.DEFAULT_OPTION:
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			default:
				p.dialogTypeOptions = JOptionPane.DEFAULT_OPTION;  		
				p.action = EnumForwardAction.DIALOG_START_QUESTION_YES_NO;
				break;
			}
		return p;
	}

	
     /**
	 * Starts a standard dialog error for input text message with minimal parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param initialValue the initial text value
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_TEXT(String dialogName, String title, String message, String initialValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_TEXT;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.PLAIN_MESSAGE;					// Nessuna icona usata					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION;  						 
		p.dialogSelectionValues = null;  						 
		p.dialogInitialSelectionValue = initialValue; 						 
		p.isDialogWantsInput = true; 						 
		return p;
	}


     /**
	 * Starts a standard dialog for input text message without labels specification<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param initialValue the initial text value
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_TEXT(String dialogName, int dialogType, String title, String message, boolean modal, String iconPath, String Icon, String initialValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_TEXT;
		p.dialogName = dialogName;				 
		p.dialogType = dialogType;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION; 						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogSelectionValues = new String[1];  						 
		p.dialogInitialSelectionValue = initialValue;  						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION;  
		p.isDialogWantsInput = true; 						 
		return p;
	}



     /**
	 * Starts a standard dialog for input text message with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param selectionValues a string array of one element with text
	 * @param initialSelectionValue the initial selection value
	 * @param labelsOptionCustom a string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_TEXT(String dialogName, int dialogType, String title, String message, boolean modal, String iconPath, String Icon, String selectionValues[], String initialSelectionValue, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_TEXT;
		p.dialogName = dialogName;				 
		p.dialogType = dialogType;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION; 						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogSelectionValues = selectionValues;  						 
		p.dialogInitialSelectionValue = initialSelectionValue;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected;  
		p.isDialogWantsInput = true; 						 
		return p;
	}


	

    /**
	 * Starts a standard dialog for input text by a comboBox with minimal specification as a plain message<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param selectionValues a string array of one element with text
	 * @param initialSelectionValue the initial selection value
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_COMBO_BOX(String dialogName, String title, String message, boolean modal, String selectionValues[], String initialSelectionValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_COMBO_BOX;
		p.dialogName = dialogName;				 
		p.dialogType = JOptionPane.PLAIN_MESSAGE;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION; 
		p.dialogModal = modal;  						 
		p.dialogIconPath = null;  						 
		p.dialogSelectionValues = selectionValues;  						 
		p.dialogInitialSelectionValue = initialSelectionValue;  						 
		p.isDialogWantsInput = true; 						 
		return p;
	}

    /**
	 * Starts a standard dialog for input text by a comboBox<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param selectionValues a string array of one element with text
	 * @param initialSelectionValue the initial selection value
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_COMBO_BOX(String dialogName, int dialogType, String title, String message, boolean modal, String iconPath, String selectionValues[], String initialSelectionValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_COMBO_BOX;
		p.dialogName = dialogName;				 
		p.dialogType = dialogType;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION; 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogSelectionValues = selectionValues;  						 
		p.dialogInitialSelectionValue = initialSelectionValue;  						 
		p.isDialogWantsInput = true; 						 
		return p;
	}



     /**
	 * Starts a standard dialog for input text by a comboBox  with full parameters<br>
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogType the type of dialog as a constant <code>JOptionPane ERROR_MESSAGE, INFORMATION_MESSAGE, WARNING_MESSAGE, QUESTION_MESSAGE, PLAIN_MESSAGE</code>
	 * @param title the title of window message
	 * @param message the message to be displayed
	 * @param typeOptions as a constant <code>JOptionPane YES_NO_OPTION, YES_NO_CANCEL_OPTION, OK_CANCEL_OPTION, DEFAULT_OPTION
	 * @param modal true for modal dialog
	 * @param iconPath the path of icon
	 * @param selectionValues a string array of one element with text
	 * @param initialSelectionValue the initial selection value
	 * @param labelsOptionCustom a string array of label options to be displayed and selected by a comboBox
	 * @param labelOptionCustomSelected the label option selected
	 */
	public ForwardDoParms DO_DIALOG_START_INPUT_BY_COMBO_BOX(String dialogName, int dialogType, String title, String message, int typeOptions, boolean modal, String iconPath, String selectionValues[], String initialSelectionValue, String labelsOptionCustom[], String labelOptionCustomSelected) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.DIALOG_START_INPUT_BY_TEXT;
		p.dialogName = dialogName;				 
		p.dialogType = dialogType;					    
		p.dialogTitle = title;						 
		p.dialogMessage = message;						 
		p.dialogTypeOptions = typeOptions; 						 
		p.dialogModal = modal;  						 
		p.dialogIconPath = iconPath;  						 
		p.dialogSelectionValues = selectionValues;  						 
		p.dialogInitialSelectionValue = initialSelectionValue;  						 
		p.dialogLabelsOptionCustom = labelsOptionCustom;  						 
		p.dialogLabelOptionCustomSelected = labelOptionCustomSelected;  
		p.dialogTypeOptions = JOptionPane.OK_CANCEL_OPTION;  
		p.isDialogWantsInput = true; 						 
		return p;
	}


	
    /**
	 * Starts a user dialog as declared by function<br>
	 * <p>
	 * It's required a form defined by function. The form can be done by any panels<br>
	 * recursively arranged and, at the end, it's a panel implemented by a {@link JPanel} object.
	 * <br>
	 * The started dialog will be decorated, placed on center and with the standard icon on the frame.<br>
	 * <p>
	 * 
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogForm the form name as declared by function to be displayed on the dialog
	 * @param title the title of window message
	 * @param modal as true for modal dialog or false if modeless
	 */
	public ForwardDoParms DO_DIALOG_START_USER(String dialogName, String dialogForm, String title, boolean modal) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		if (modal) {
			p.action = EnumForwardAction.DIALOG_START_USER_MODAL;
		} else {
			p.action = EnumForwardAction.DIALOG_START_USER_NO_MODAL;
		}
		p.dialogName = dialogName;				 
		p.dialogForm = dialogForm;				 
		p.dialogTitle = title;						 
		p.dialogModal = modal; 
		p.dialogPos = EnumForwardOption.DIALOG_ON_CENTER_PARENT;	
		p.isDialogUndecorated = false;					 
		return p;
	}
	
    /**
	 * Starts a user dialog as declared by function with all available features<br>
	 * <p>
	 * It's required a form defined by function. The form can be done by any panels<br>
	 * recursively arranged and, at the end, it's a panel implemented by a {@link JPanel} object.
	 * <br>
	 * 
	 * @param dialogName used as a dialog identifier inside application logic
	 * @param dialogForm the form name as declared by function to be displayed on the dialog
	 * @param title the title of window message
	 * @param modal as true for modal dialog or false if modeless
	 * @param isDecorated as true to show the dialog with no frame
	 * @param iconPath as the path of the icon on the title row
	 * @param typeCenter as EnumForwardOption.DIALOG_ON_CENTER_SCREEN or EnumForwardOption.DIALOG_ON_CENTER_PARENT or EnumForwardOption.DIALOG_AT_X_Y
	 * @param posX as the initial X coordinate
	 * @param posY as the initial X coordinate
	 */
	public ForwardDoParms DO_DIALOG_START_USER(String dialogName, String dialogForm, String title, boolean modal, boolean isDecorated, String iconPath, EnumForwardOption typeCenter, int posX, int posY) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		if (modal) {
			p.action = EnumForwardAction.DIALOG_START_USER_MODAL;
		} else {
			p.action = EnumForwardAction.DIALOG_START_USER_NO_MODAL;
		}
		p.dialogName = dialogName;				 
		p.dialogForm = dialogForm;				 
		p.dialogTitle = title;						 
		p.dialogModal = modal;  
		p.dialogPos = typeCenter;	
		p.isDialogUndecorated = isDecorated;		
		p.dialogPosX = posX;		
		p.dialogPosY = posY;		
		return p;
	}
	
 		
		
	/**
	 * Close a user dialog <br>
	 * <p>
	 * All resources will be released.<br>
	 * If the input dialog is not defined or if it is not currently active, no action will be taken
	 * <p>
	 * @param dialogName used as a dialog identifier inside application logic
	 */
	public ForwardDoParms DO_DIALOG_CLOSE(String dialogName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = dialogName;
		p.action = EnumForwardAction.DIALOG_CLOSE;
		return p;
	}

	/**
	 * Close all active user dialogs <br>
	 * <p>
	 * All resources will be released.<br>
	 * If any dialog is not defined or if it is not currently active, no action will be taken<br>
	 * <p>
	 */
	public ForwardDoParms DO_DIALOG_CLOSE_ALL() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogName = "";
		p.action = EnumForwardAction.DIALOG_CLOSE_ALL;
		return p;
	}


	/**
	 * Move the dialog to pos X, Y of the coordinates space. <br>
	 * <p>
	 * @param dialogForm the form name as declared by function to be displayed on the dialog
	 */
	public ForwardDoParms DO_DIALOG_MOVE(String dialogForm, int posX, int posY) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.dialogForm = dialogForm;
		p.dialogPosX = posX;
		p.dialogPosY = posY;
		p.action = EnumForwardAction.DIALOG_MOVE;
		return p;
	}

	
	/**
	 * Execute a reusable application method with application logic<br>
	 * <p>
	 * To increment reusability and so to share even little bit of code, it's optionally possible<br>
	 * to declare an unlimited number of parameters for the method.<br>
	 * The method can then get parameters through the variable <code>parms</code> available by the action descriptor<br>
	 * of the {@link ForwardSystem} object.
	 * <p>
	 * To get parameters the method called must perform something so:<pre>
	 *   Object parms[] = s.getActiveDoParms().parms</pre>
	 * 
	 * @param methodName the method name to execute
	 * @param parms as an optional  sequence of objects, normally simple strings, available in the called method
	 */
	public ForwardDoParms DO_EXEC_METHOD(String methodName, Object ... parms) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.methodName = methodName;
		p.parms = parms;
		p.action = EnumForwardAction.EXEC_METHOD;
		return p;
	}

	/**
	 * Execute all actions already declared for a component name and an event.<br>
	 * <p>
	 * To increment reusability at the model level and so to share even little bit of declarations, <br>
	 * you are allowed to execute all actions coded by a specific declared event.<br>
	 * If the event is not declared for the component no action will be taken.<br>
	 * <p>
	 * @param groupActionsName the group name of actions
	 * @param actions as a sequence of specific actions to exec
	 */
	public ForwardDoParms DO_EXEC_ACTIONS_ON_EVENT(EnumForwardEvent actionEvent, String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.actionEvent = actionEvent;
		p.actionEventComponentName = componentName;
		p.action = EnumForwardAction.EXEC_ACTIONS_ON_EVENT;
		return p;
	}
	
	/**
	 * Execute a set of actions identified by a group name.<br>
	 * <p>
	 * To increment reusability at the model level and so to share even little bit of declarations, it's optionally possible<br>
	 * to declare an unlimited number of groups and actions inside the group to be executed runtime.<br>
	 * If the group name is not defined no action will be taken.<br>
	 * <p>
	 * @param groupActionsName the group name of actions
	 * @param actions as a sequence of specific actions to exec
	 */
	public ForwardDoParms DO_EXEC_ACTIONS_GROUP(String groupActionsName, ForwardDoParms ... actions) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.groupActionsName = groupActionsName;
		p.groupActionsList = actions;
		p.action = EnumForwardAction.EXEC_ACTIONS_GROUP;
		return p;
	}
	

	/**
	 * Stops the execution of all actions still to be executed, for the current event.<br>
	 * <p>
	 * May be useful not to exec further declared actions due application conditions.<br>
	 * For example after the run of a logical data view with non record found you are allowed<br>
	 * to stop any further action by coding <code>ON_EVENT(EnumForwardEvent.ON_LDV_GET_NO_ROWS, DO_STOP_EVENT_ACTIONS())</code>
	 * <p>
	 * Because of several events could need to stop execution under specific events, an id can be specified to<br>
	 * make any condition unique.<br>
	 * <p>
	 * @param actionSkipId the id that must be active to make the stop effective.<br>
	 * an empty value means no id test.
	 */
	public ForwardDoParms DO_SKIP_EVENT_ACTIONS(String actionSkipId) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.actionSkipId = actionSkipId;
		p.action = EnumForwardAction.SKIP_EVENT_ACTIONS;
		return p;
	}
		
	/**
	 * Sets the id to be tested to make the <code>DO_STOP_EVENT_ACTIONS</code> action effetctive.<br>
	 * <p>
	 * @param actionStopId the id that must be active to make the stop effective.<br>
	 */
	public ForwardDoParms DO_SKIP_SET_ID(String actionSkipId) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.actionSkipId = actionSkipId;
		p.action = EnumForwardAction.SKIP_SET_ID;
		return p;
	}

	/**
	 * Sets the variable value.<br>
	 * <p>
	 * @param varName the variable name to be set.<br>
	 * @param varValue the value to set.<br>
	 * @param dummy a dummy parameter just to make the method unique<br>
	 */
	public ForwardDoParms DO_VAR_SET_VALUE(String varName, Object varValue, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.varName = varName;
		p.varValue = varValue;
		p.varValueComponentName = "";
		p.action = EnumForwardAction.VAR_SET_VALUE;
		return p;
	}
		
	/**
	 * Sets the variable value.<br>
	 * <p>
	 * @param varName the variable name to be set.<br>
	 * @param varValue the value to set.<br>
	 */
	public ForwardDoParms DO_VAR_SET_VALUE(String varName, String varValueComponentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.varName = varName;
		p.varValue = null;
		p.varValueComponentName = varValueComponentName;
		p.action = EnumForwardAction.VAR_SET_VALUE;
		return p;
	}
		

	/**
	 * Sets a GUI component or a function variable with a localized coded message.<br>
	 * <p>
	 * The message must be defined in the forward generalized tables system in the rule table number<br>
	 * assigned to the function, for messages to be showed.<br>
	 * It will be searched in the oder a GUI component with the input name compatible with a text value<br>
	 * to show, like a JTextField, JLabel and so on and, otherwise a variable name to be set.<br>
	 * The input language overrides the language specified by function declaration or assigned to the<br>
	 * user at login time.
	 * <p>
	 * If the function doesn't declare a GUI component or a variable with the input name, no action will be taken.<br>
	 * <p>
	 * Message types can be:<pre>
	WARNING 
	INFORMATION 
	DEBUG 
	ERROR_INPUT 
	ERROR_FATAL </pre>   
	 * The key of each messsage rule table element is made by the ordinal of {@link EnumMessageType} followed <br>
	 * by the message code.<br>
	 * <p>
	 * @param messageCode the message code
	 * @param messageType the message type as a {@link EnumMessageType} enumeration
	 * @param messageComponentName the name of a GUI component or function variable
	 * @param messageLanguage the message language as a {@link EnumLanguage} enumeration
	 */
	public ForwardDoParms DO_MESSAGE(String messageCode, EnumMessageType messageType, String messageComponentName, EnumLanguage messageLanguage) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.messageCode = messageCode;
		p.messageType = messageType;
		p.messageComponentName = messageComponentName;
		p.messageLanguage = messageLanguage;
		p.action = EnumForwardAction.MESSAGE;
		return p;
	}

	/**
	 * Action dummy.<br>
	 * <p>
	 * No operation will be executed.<br>
	 * It migth be useful to make not operative standard actions.<br>
	 * <p>
	 */
	public ForwardDoParms DO_NOTHING() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.NOTHING;
		return p;
	}
		

	/**
	 * Sets a GUI component or a function variable with a localized coded message.<br>
	 * <p>
	 * The message must be defined in the forward generalized tables system in the rule table number<br>
	 * assigned to the function, for messages to be showed.<br>
	 * It will be searched in the oder a GUI component with the input name compatible with a text value<br>
	 * to show, like a JTextField, JLabel and so on and, otherwise a variable name to be set.<br>
	 * The message language is the language declared by function or assigned by means of user login procedure.<br>
	 * <p>
	 * If the function doesn't declare a GUI component or a variable with the input name, no action will be taken.<br>
	 * <p>
	 * Message types can be:<pre>
	WARNING 
	INFORMATION 
	DEBUG 
	ERROR_INPUT 
	ERROR_FATAL </pre>   
	 * The key of each messsage rule table element is made by the ordinal of {@link EnumMessageType} followed <br>
	 * by the message code.<br>
	 * <p>
	 * @param messageCode the message code
	 * @param messageType the message type as a {@link EnumMessageType} enumeration
	 * @param messageComponentName the name of a GUI component or function variable
	 */
	public ForwardDoParms DO_MESSAGE(String messageCode, EnumMessageType messageType, String messageComponentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.messageCode = messageCode;
		p.messageType = messageType;
		p.messageComponentName = messageComponentName;
		p.messageLanguage = EnumLanguage.NOT_ASSIGNED;
		p.action = EnumForwardAction.MESSAGE;
		return p;
	}
		
  /**
	 * Creates, validate and execute a logical data view<br>
	 * <p>
	 * It will be verified the existence of the input logical data view name <br>
	 * as a class and a new object will be obtained.<br>
	 * <p>
	 * If the logical data view is not declared no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 * @param limitRows the max number of rows to get
	 */
	public ForwardDoParms DO_LDV_RUN(String ldvClassName, int limitRows) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = limitRows;
		p.action = EnumForwardAction.LDV_RUN;
		return p;
	}

		
	   /**
	 * Creates, validate and execute a logical data view<br>
	 * <p>
	 * It will be verified the existence of the input logical data view name <br>
	 * as a class and a new object will be obtained.<br>
	 * <p>
	 * If the logical data view is not declared no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_RUN(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = -1;
		p.action = EnumForwardAction.LDV_RUN;
		return p;
	}

			
    /**
	 * Creates a logical data view<br>
	 * <p>
	 * It will be verified the existence of the input logical data view name <br>
	 * as a class and a new object will be obtained.<br>
	 * <p>
	 * If the logical data view is not declared no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 * @param limitRows the max number of rows to get
	 */
	public ForwardDoParms DO_LDV_CREATE(String ldvClassName, int limitRows) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = limitRows;
		p.action = EnumForwardAction.LDV_CREATE;
		return p;
	}

    /**
	 * Creates a logical data view<br>
	 * <p>
	 * It will be verified the existence of the input logical data view name <br>
	 * as a class and a new object will be obtained.<br>
	 * <p>
	 * There is not limit to the number of rows to get.
	 * If the logical data view is not declared no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_CREATE(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = -1;
		p.action = EnumForwardAction.LDV_CREATE;
		return p;
	}

    /**
	 * Execute a logical data view<br>
	 * <p>
	 * The logical data view is allowed to return a maximum number of rows as specified by input parameters.<br>
	 * The dynamically built sql query will code the standard sql <code>LIMIT</code> parameters to limit the<br>
	 * number of rows tro get.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 * @param limitRows the max number of rows to be read from database with a zero value for no limits
	 */
	public ForwardDoParms DO_LDV_EXECUTE(String ldvClassName, int limitRows) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = limitRows;
		p.action = EnumForwardAction.LDV_EXECUTE;
		return p;
	}

	
    /**
	 * Execute a logical data view<br>
	 * <p>
	 * There is no limit to the number of rows that the logical data view is allowed to get.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_EXECUTE(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = -1;
		p.action = EnumForwardAction.LDV_EXECUTE;
		return p;
	}

	
    /**
	 * Validates a logical data view<br>
	 * <p>
	 * The logical data view will be checked for any errors and the <br>
	 * equivakent sql select statement will be created if no errors<br>
	 * are detected<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 */
	public ForwardDoParms DO_LDV_VALIDATE(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_VALIDATE;
		return p;
	}

    /**
	 * Set the size of a page as the number of rows.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 * @param pageRows the number of rows in a page
	 */
	public ForwardDoParms DO_LDV_SET_PAGE_ROWS(String ldvClassName, int pageRows) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvPageRows = pageRows;
		p.action = EnumForwardAction.LDV_SET_PAGE_ROWS;
		return p;
	}

   /**
	 * Set GUI controls with current logical data view row data<br>
	 * <p>
	 * For each column of the logical data view declared by AS clause,<br>
	 * the current row column value is used to update GUI controls, with the
	 * same name and type.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name  
	 */
	public ForwardDoParms DO_LDV_SET_FUNCTION_GUI_CONTROLS(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_SET_FUNCTION_GUI_CONTROLS;
		return p;
	}

	/**
	 * Set a specific GUI control with the current logical data view row data column value<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name  
	 * @param ldvColumnName the column name to be used to set the GUI component
	 * @param ldvFunctionControlName the function GUI component name to be set with the value of ldvColumnName  
	 */
	public ForwardDoParms DO_LDV_SET_FUNCTION_GUI_CONTROL(String ldvClassName, String ldvColumnName, String ldvFunctionControlName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvColumnName = ldvColumnName;
		p.ldvFunctionControlName = ldvFunctionControlName;
		p.action = EnumForwardAction.LDV_SET_FUNCTION_GUI_CONTROL;
		return p;
	}

	 /**
	 * Set function declared by VAR() with the current value of logical data view columns with the same name<br>
	 * <p>
	 * For each column of the logical data view declared by AS clause, the current row column value is used <br>
	 * to update the function variables declared at the function declaration time, with the same name and type.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 */
	public ForwardDoParms DO_LDV_SET_FUNCTION_VARS(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_SET_FUNCTION_VARS;
		return p;
	}

	/**
	 * Set a specific function variable declared by VAR() with the current value of the logical data view column<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name  
	 * @param ldvColumnName the column name to be used to set the GUI component
	 * @param ldvFunctionVarName the function variable name, declared by VAR(), to be set with the value of ldvColumnName  
	 */
	public ForwardDoParms DO_LDV_SET_FUNCTION_VAR(String ldvClassName, String ldvColumnName, String ldvFunctionVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvColumnName = ldvColumnName;
		p.ldvFunctionVarName = ldvFunctionVarName;
		p.action = EnumForwardAction.LDV_SET_FUNCTION_VAR;
		return p;
	}

		
    /**
	 * Set logical data view root FOR_ANY() from GUI controls values<br>
	 * <p>
	 * This action must be used to set primary key values of the root FOR_ANY()<br>
	 * of a logical data view, that can return one row or a set of rows, when some <br>
	 * FOR_EACH() declarations are following too.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or a FOR_ANY() is not the first declaration, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_ROOT_FROM_GUI(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_ROOT_FROM_GUI;
		return p;
	}

	
    /**
	 * Set logical data view root FOR_ANY() from variable values<br>
	 * <p>
	 * This action must be used to set primary key values of the root FOR_ANY()<br>
	 * of a logical data view, that can return one row or a set of rows, when some <br>
	 * FOR_EACH() declarations are following too.<br>
	 * Key values will be set using only variables with the same name.<br>
	 * Setting using the current value of GUI controls, must be done with the action<br>
	 * <code>LDV_SET_FIELDS_KEY_ROOT_FROM_GUI</code>
	 * <p>
	 * Actions <code>DO_LDV_SET_FIELDS_KEY_ROOT_FROM_GUI<code> and <code>DO_LDV_SET_FIELDS_KEY_ROOT_FROM_VAR</code><br>
	 * allow you make keys setting automatic and dependent of what coded at function declaration time<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or a FOR_ANY() is not the first declaration, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_ROOT_FROM_VAR(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_ROOT_FROM_VAR;
		return p;
	}


	/**
	 * Set logical data view keys from GUI controls values<br>
	 * <p>
	 * This action must be used to set primary key values of all specified entities<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns exposed with the same name of a GUI control will be updated with <br>
	 * the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or a FOR_ANY() is not the first declaration, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_FROM_GUI(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = "";
		p.ldvEntityNameAs = "";
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_FROM_GUI;
		return p;
	}

	  /**
	 * Set Entity logical data view keys from GUI controls values<br>
	 * <p>
	 * This action must be used to set primary key values of the input entity<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns exposed with the same name of a GUI control will be updated with <br>
	 * the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the input entity is not declared, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param entityNameAs the internal AS entitity name
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_FROM_GUI(String ldvClassName, String entityNameAs) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_FROM_GUI;
		return p; 
	}



  /**
	 * Set logical data view keys from variables values<br>
	 * <p>
	 * This action must be used to set primary key values of all specified entities<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns exposed with the same name of an application variable declared by VAR() <br>
	 * or automatically defined, will be updated with the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_FROM_VAR(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = "";
		p.ldvEntityNameAs = "";
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_FROM_VAR;
		return p;
	}

	/**
	 * Set Entity logical data view keys from variables values<br>
	 * <p>
	 * This action must be used to set primary key values of the input entity <br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns exposed with the same name of a GUI control will be updated with <br>
	 * the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the input entity is not declared, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the internal AS entitity name
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_KEY_FROM_VAR(String ldvClassName, String entityNameAs) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.action = EnumForwardAction.LDV_SET_FIELDS_KEY_FROM_VAR;
		return p;
	}


	/**
	 * Set logical data view not keys from GUI controls values<br>
	 * <p>
	 * This action must be used to set not primary key values of all specified entities<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns not primary key exposed with the same name of a GUI control will <br>
	 * be updated with the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_NOT_KEY_FROM_GUI(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = "";
		p.ldvEntityNameAs = "";
		p.action = EnumForwardAction.LDV_SET_FIELDS_NOT_KEY_FROM_GUI;
		return p;
	}

	  /**
	 * Set logical data view not keys from GUI controls values<br>
	 * <p>
	 * This action must be used to set not primary key values of the input entity<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns not primary key exposed with the same name of a GUI control will be updated with <br>
	 * the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the internal AS entitity name
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_NOT_KEY_FROM_GUI(String ldvClassName, String entityNameAs) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.action = EnumForwardAction.LDV_SET_FIELDS_NOT_KEY_FROM_GUI;
		return p;
	}



  /**
	 * Set logical data view not keys from variables values<br>
	 * <p>
	 * This action must be used to set not primary key values of all specified entities<br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns not primary key exposed with the same name of an application variable declared by VAR() <br>
	 * or automatically defined, will be updated with the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_NOT_KEY_FROM_VAR(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = "";
		p.ldvEntityNameAs = "";
		p.action = EnumForwardAction.LDV_SET_FIELDS_NOT_KEY_FROM_VAR;
		return p;
	}

	/**
	 * Set Entity logical data view keys from variables values<br>
	 * <p>
	 * This action must be used to set not primary key values of the input entity <br>
	 * of a logical data view, declared by a FOR_ANY(), FOR_EACH(), INSERT(), DELETE() or UPDATE().<br>
	 * <p>
	 * All entity columns exposed with the same name of a variable will be updated with <br>
	 * the current value.<br> 
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the input entity is not declared, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the internal AS entitity name
	 */
	public ForwardDoParms DO_LDV_SET_FIELDS_NOT_KEY_FROM_VAR(String ldvClassName, String entityNameAs) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.action = EnumForwardAction.LDV_SET_FIELDS_NOT_KEY_FROM_VAR;
		return p;
	}

	/**
	 * Set a logical data view field value.<br>
	 * <p>
	 * A logical data view field is an entity column, a java name as defined in the entity bean class<br>
	 * or an alias java name as coded by AS clause in the FOR_ANY() or FOR_EACH() declarations.<br>
	 * The logical data view manager, for each column, allocates an internal variable, automatically<br>
	 * updated at row retrieving time. 
	 * <p>
	 * This action allow you to update the column variable with an object value. <br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the object value is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param columnName the java name of the column or the java name overrided by the AS newName clause on FOR_ANY() or FOR_EACH()
	 * @param columnValue the value object to be assigned to the columName 
	 * @param serviceParm as an object service parm to make unique the signature. null is a good value
	 */
	public ForwardDoParms DO_LDV_SET_FIELD(String ldvClassName, String columnName, Object columnValue, Object serviceParm) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvColumnName = columnName;
		p.ldvColumnValue = columnValue;
		p.ldvFunctionVarName = "";;
		p.action = EnumForwardAction.LDV_SET_FIELD;
		return p;
	}

	/**
	 * Set a logical data view field value.<br>
	 * <p>
	 * A logical data view field is an entity column, a java name as defined in the entity bean class<br>
	 * or an alias java name as coded by AS clause in the FOR_ANY() or FOR_EACH() declarations.<br>
	 * The logical data view manager, for each column, allocates an internal variable, automatically<br>
	 * updated at row retrieving time. 
	 * <p>
	 * This action allow you to update the column variable with an object value from a function declared variable by VAR()<br>
	 * or from a GUI defined control with the same name and of a compatible type.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the variable is not defined or it's type is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param columnName the java name of the column or the java name overrided by the AS newName clause on FOR_ANY() or FOR_EACH()
	 * @param functionVarName the variable name, in the application runtime system, to use as input to update the columName value or
	 * the name of a GUI control of compatible type.
	 */
	public ForwardDoParms DO_LDV_SET_FIELD(String ldvClassName, String columnName, String functionVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvColumnName = columnName;
		p.ldvFunctionVarName = functionVarName;
		p.ldvColumnValue = null;
		p.action = EnumForwardAction.LDV_SET_FIELD;
		return p;
	}

	/**
	 * Set a logical data view variable value.<br>
	 * <p>
	 * A logical data view variable is a variable declared by a VAR() declaration.<br>
	 * Variables declared in this way are mainly used to hold values, set externally by application,<br>
	 * to be solved runtime at the creation of the sql query time.<br>
	 * A VAR() declared variable is managed like an host variable of Sql language and it's inserted<br>
	 * in the where condition in the same mode: with a : (semicolon) character to start the variable name.<br>
	 * <p>
	 * This action allow you to update this variable with an object value. <br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the object value is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param ldvVarName the java name of the column or the java name overrided by the AS newName clause on FOR_ANY() or FOR_EACH()
	 * @param newVarValue the value object to be assigned to the ldvVarName 
	 * @param serviceParm as an object service parm to make unique the signature. null is a good value
	 */
	public ForwardDoParms DO_LDV_SET_VAR(String ldvClassName, String ldvVarName, Object newVarValue, Object serviceParm) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvVarName = ldvVarName;
		p.ldvVarValue = newVarValue;
		p.ldvFunctionVarName = "";
		p.action = EnumForwardAction.LDV_SET_VAR;
		return p;
	}

	/**
	 * Set a logical data view variable value.<br>
	 * <p>
	 * A logical data view variable is a variable declared by a VAR() declaration.<br>
	 * Variables declared in this way are mainly used to hold values, set externally by application,<br>
	 * to be solved runtime at the creation of the sql query time.<br>
	 * A VAR() declared variable is managed like an host variable of Sql language and it's inserted<br>
	 * in the where condition in the same mode: with a : (semicolon) character to start the variable name.<br>
	 * <p>
	 * This action allow you to update this variable with the contents of a variable declared and managed by function <br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the function variable is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param ldvVarName the java name of the column or the java name overrided by the AS newName clause on FOR_ANY() or FOR_EACH()
	 * @param functionVarName the name of a function variable to be assigned to the ldvVarName 
	 */
	public ForwardDoParms DO_LDV_SET_VAR(String ldvClassName, String ldvVarName, String functionVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvVarName = ldvVarName;
		p.ldvFunctionVarName = functionVarName;
		p.ldvVarValue = null;
		p.action = EnumForwardAction.LDV_SET_VAR;
		return p;
	}
 
	/**
	 * Set the logical data view rule table number<br>
	 * <p>
	 * Valid for ldv accessing only to a specific rule table, to get a set of rows or a single item<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the function variable is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param ruleTableNum the rule table num defined in the generalyzed forward tables system
	 */
	public ForwardDoParms DO_LDV_SET_RULE_TABLE_NUM(String ldvClassName, int ruleTableNum) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvVarName = "";
		p.ldvRuleTableNum = ruleTableNum;
		p.action = EnumForwardAction.LDV_SET_RULE_TABLE_NUM;
		return p;
	}
 
	/**
	 * Set a supplementary sql where condition of a FOR_ANY/EACH declarative in a logical data view<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and <br>
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can declare this action <br>
	 * to dynamically set the condition runtime.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the logical data view has been not previously loaded or the declaration index is wrong, <br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param entityIndex the index 0-based of the declaration in the logical data view
	 * @param whereSql the sql where condition
	 * @param dummy as a dummy parameter only to make this method unique
	 */
	public ForwardDoParms DO_LDV_SET_SQL_WHERE(String ldvClassName, int entityIndex, String whereSql, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvEntityIndex = entityIndex;
		p.ldvVarName = "";
		p.ldvWhereSql = whereSql;
		p.action = EnumForwardAction.LDV_SET_SQL_WHERE;
		return p;
	}
 
	/**
	 * Set a supplementary sql where condition of a FOR_ANY/EACH declarative in a logical data view<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and <br>
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can declare this action <br>
	 * to dynamically set the condition runtime.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the logical data view has been not previously loaded or the declaration index is wrong, <br>
	 * or the variable is not declared by function, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param entityIndex the index 0-based of the declaration in the logical data view
	 * @param varName the string function variable with the sql where condition
	 */
	public ForwardDoParms DO_LDV_SET_SQL_WHERE(String ldvClassName, int entityIndex, String varName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvEntityIndex = entityIndex;
		p.ldvVarName = varName;
		p.ldvWhereSql = "";
		p.action = EnumForwardAction.LDV_SET_SQL_WHERE;
		return p;
	}
 

	/**
	 * Set a supplementary sql where condition of a FOR_ANY/EACH declarative in a logical data view<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and <br>
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can declare this action <br>
	 * to dynamically set the condition runtime.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the logical data view has been not previously loaded or the declaration index is wrong, <br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param entityNameAs the entity identifier in the logical data view
	 * @param whereSql the sql where condition
	 * @param dummy as a dummy parameter only to make this method unique
	 */
	public ForwardDoParms DO_LDV_SET_SQL_WHERE(String ldvClassName, String entityNameAs, String whereSql, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.ldvEntityIndex = -1;
		p.ldvVarName = "";
		p.ldvWhereSql = whereSql;
		p.action = EnumForwardAction.LDV_SET_SQL_WHERE;
		return p;
	}

	/**
	 * Set a supplementary sql where condition of a FOR_ANY/EACH declarative in a logical data view<br>
	 * <p>
	 * The logical data view may contain several access declaratives, and <br>
	 * a complex sql select, with all joins required, will be generated.<br>
	 * Thereby normally nothing more it's necessary.<br>
	 * When the application needs specific where conditions related to the access of an<br>
	 * entity, depending on the currente status execution, can declare this action <br>
	 * to dynamically set the condition runtime.<br>
	 * <p>
	 * The sql where string must be a valid sql where condition with column names specified<br>
	 * as the java bean name and not as the sql column name.<br>
	 * <p>
	 * If the logical data view has been not previously loaded or the declaration index is wrong, <br>
	 * or the variable is not declared by function, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param entityNameAs the entity identifier in the logical data view
	 * @param varName the string function variable with the sql where condition
	 */
	public ForwardDoParms DO_LDV_SET_SQL_WHERE(String ldvClassName, String entityNameAs, String varName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.ldvEntityIndex = -1;
		p.ldvVarName = varName;
		p.ldvWhereSql = "";
		p.action = EnumForwardAction.LDV_SET_SQL_WHERE;
		return p;
	}

	/**
	 * Set the logical data view rule table number<br>
	 * <p>
	 * Valid for ldv accessing only to a specific rule table, to get a set of rows or a single item<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the function variable is not of the correct type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param ldvVarName the function variable name or GUI control, containing the integer value of the rule table  number. 
	 */
	public ForwardDoParms DO_LDV_SET_RULE_TABLE_NUM(String ldvClassName, String ldvVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvVarName = ldvVarName;
		p.ldvRuleTableNum = -1;
		p.action = EnumForwardAction.LDV_SET_RULE_TABLE_NUM;
		return p;
	}
 
	/**
	 * Set the logical data view language for rule tables.<br>
	 * <p>
	 * This action is valid only when the logical data view does access to rule tables too. <br>
	 * In this case the where condition of the automatic sql statement created, will contains<br>
	 * the constaint for the language specified.
	 * <p>
	 * Your'e noticed that when there are not rows in the generalized Forward tables system<br>
	 * for the input rule table specified, no rows at all will be returned to the application.<br>
	 * <br>
	 * If the logical data view is not declared, has been not created or validated no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param ldvLanguage as the language of accessd rule tables
	 */
	public ForwardDoParms DO_LDV_SET_LANGUAGE(String ldvClassName, EnumLanguage ldvLanguage) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLanguage = ldvLanguage;
		p.ldvFunctionVarName = "";
		p.action = EnumForwardAction.LDV_SET_LANGUAGE;
		return p;
	}

	/**
	 * Set the logical data view language for rule tables.<br>
	 * <p>
	 * This action is valid only when the logical data view does access to rule tables too. <br>
	 * In this case the where condition of the automatic sql statement created, will contains<br>
	 * the constaint for the language specified.
	 * <p>
	 * Your'e noticed that when there are not rows in the generalized Forward tables system<br>
	 * for the input rule table specified, no rows at all will be returned to the application.<br>
	 * <br>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * or the object value is not of <code>EnumLanguage</code> type, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name 
	 * @param functionVarName the variable name, in the application runtime system, to use as input to set the language,
	 * as a valid <code>EnumLanguage</code> enumeration object
	 */
	public ForwardDoParms DO_LDV_SET_LANGUAGE(String ldvClassName, String functionVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLanguage = EnumLanguage.NOT_ASSIGNED;
		p.ldvFunctionVarName = functionVarName;
		p.action = EnumForwardAction.LDV_SET_LANGUAGE;
		return p;
	}

	/**
	 * Clear the logical data view<br>
	 * <p>
	 * All rows will be released and the state will be initialized.<br>
	 * To get new data from database it's necessary to execute again the logical data view.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 */
	public ForwardDoParms DO_LDV_CLEAR(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_CLEAR;
		return p;
	}

	 /**
	 * Gets a specific logical data view row<br>
	 * <p>
	 * The row will be read and all current column variables will be updated.<br>
	 * <p>
	 * If the logical data view is not declared, or the row number is out of bound,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param row the row number to get, 0-based
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_READROW(String ldvClassName, int row, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvRow = row;
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_READROW;
		return p;
	}

	/**
	 * Gets the first logical data view row<br>
	 * <p>
	 * The row will be read and all current column variables will be updated.<br>
	 * <p>
	 * If the logical data view is not declared, or the row number is out of bound,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_READFIRST(String ldvClassName, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_READFIRST;
		return p;
	}

	/**
	 * Gets the last logical data view row<br>
	 * <p>
	 * The row will be read and all current column variables will be updated.<br>
	 * <p>
	 * If the logical data view is not declared, or the row number is out of bound,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_READLAST(String ldvClassName, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_READLAST;
		return p;
	}

		
	/**
	 * Gets the next logical data view row<br>
	 * <p>
	 * The row will be read and all current column variables will be updated.<br>
	 * <p>
	 * If the logical data view is not declared, or the row number is out of bound,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_READNEXT(String ldvClassName, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_READNEXT;
		return p;
	}

	/**
	 * Gets the previous logical data view row<br>
	 * <p>
	 * The row will be read and all current column variables will be updated.<br>
	 * <p>
	 * If the logical data view is not declared, or the row number is out of bound,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_READPREV(String ldvClassName, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = "";
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_READPREV;
		return p;
	}


	 /**
	 * Update the database with the current logical data view row data.<br>
	 * <p>
	 * All entities declared by logical data view will be updated in CRUD modality, to say a row<br>
	 * for each entity accessed, by the own primary key.<br>
	 * <p>
	 * Primary keys of all entities currently accessed by the logical data view are holded<br>
	 * as variables by the logical data view.<br>
	 * <p>
	 * If the logical data view is not defined, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the entity name internal reference to update
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 * @param ldvColsToExclude the columns list to exclude from the update  They are java column name defined by the entity bean.<br>
	 * No column can be specified as a null value.
	 */
	public ForwardDoParms DO_LDV_UPDATE(String ldvClassName, String entityNameAs, String idOperation, String ... ldvColsToExclude) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.ldvIdOperation = idOperation;
		if (ldvColsToExclude == null) {
			p.ldvColsToExclude = new String[0];
		} else {
			p.ldvColsToExclude = ldvColsToExclude;
		}
		p.action = EnumForwardAction.LDV_UPDATE;
		return p;
	}


	 /**
	 * Insert the entity in the database with the current logical data view values, as a CRUD operation.<br>
	 * <p>
	 * The logical data view must declare an INSERT() of the input entity.<br>
	 * <p>
	 * The columns value is holded by logical data view and can be updated by means of<br>
	 * the logical data view set methods.<br>
	 * <p>
	 * If the logical data view is not defined, or the entityName is not declared, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the entity name internal reference to be inserted
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 * @param ldvColsToExclude the columns list to exclude from the update  They are java column name defined by the entity bean.<br>
	 * No column can be specified.
	 */
	public ForwardDoParms DO_LDV_INSERT(String ldvClassName, String entityNameAs, String idOperation, String ... ldvColsToExclude) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.ldvColsToExclude = new String[0];
		p.ldvIdOperation = idOperation;
		if (ldvColsToExclude == null) {
			p.ldvColsToExclude = new String[0];
		} else {
			p.ldvColsToExclude = ldvColsToExclude;
		}
		p.action = EnumForwardAction.LDV_INSERT;
		return p;
	}

	 /**
	 * Insert the entity in the database with the current logical data view values, as a CRUD operation.<br>
	 * <p>
	 * The logical data view must declare an DELETE() of the input entity<br>
	 * <p>
	 * The primary key columns value is holded by logical data view and can be updated by means of<br>
	 * the logical data view set methods.<br>
	 * <p>
	 * If the logical data view is not defined, or the entityName is not declared, no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view name to validate
	 * @param entityNameAs the entity name internal reference to be deleted
	 * @param idOperation as an optional string identifier to be used as reference in ON_CONDITION() declarations
	 */
	public ForwardDoParms DO_LDV_DELETE(String ldvClassName, String entityNameAs, String ... idOperation) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvEntityName = ldvClassName;
		p.ldvEntityNameAs = entityNameAs;
		p.ldvIdOperation = "";
		if (idOperation != null && idOperation.length > 0) {
			p.ldvIdOperation = idOperation[0];
		}
		p.action = EnumForwardAction.LDV_DELETE;
		return p;
	}

    /**
	 * Set some parameters to access to  the database.<br>
	 * <p>
	 * The logical data view is allowed to return a maximum number of rows as specified by input parameters.<br>
	 * The dynamically built sql query will code the standard sql <code>LIMIT</code> parameters to limit the<br>
	 * number of rows tro get.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 * @param ldvDbAutoCommit as true for an automatic commit after updates
	 * @param ldvDbAutoConnection as true to let the logical data view manager to acquire and to release the connection automatically
	 */
	public ForwardDoParms DO_LDV_DB_SET_ACCESS_PARMS(String ldvClassName, int limitRows, boolean ldvDbAutoCommit, boolean ldvDbAutoConnection) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.ldvLimitRows = limitRows;
		p.ldvDbAutoCommit = ldvDbAutoCommit;
		p.ldvDbAutoConnection = ldvDbAutoConnection;
		p.action = EnumForwardAction.LDV_DB_SET_ACCESS_PARMS;
		return p;
	}
	
    /**
	 * Gets a database connection.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * The db connection will be acquired and a reference stored in the logical data view object.
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_DB_GET_CONNECTION(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_DB_GET_CONNECTION;
		return p;
	}
	
    /**
	 * Release the currently acquired  database connection.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_DB_RELEASE_CONNECTION(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_DB_RELEASE_CONNECTION;
		return p;
	}
	
    /**
	 * Commits all updates done on the current database connection.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_DB_COMMIT(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_DB_COMMIT;
		return p;
	}
	
    /**
	 * Rollbacks all updates done on the current database connection.<br>
	 * <p>
	 * If the logical data view is not declared, has been not created or validated,<br>
	 * no action will be taken.<br>
	 * <p>
	 * @param ldvClassName the logical data view to execute
	 */
	public ForwardDoParms DO_LDV_DB_ROLLBACK(String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.ldvClassName = ldvClassName;
		p.action = EnumForwardAction.LDV_DB_ROLLBACK;
		return p;
	}
	
    /**
     * Swing component refresh<br>
     * <p>
	  * @param componentName the Swing component name to refresh
	 */
	public ForwardDoParms DO_COMPONENT_REFRESH(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMPONENT_REFRESH;
		return p;
	}

    /**
     * Swing component get value<br>
     * <p>
     * The value of the named Swing component will be read and a function variable will be set.<br>
     * JCheckBox, JRadioButton, JToggleButton needs od Boolean variable type, JTextBox a String variable and so on.<br>
     * <p>
     * If the component name is not defined or the variable is not compatible with the swing component, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing component name to set
	 * @param ValueVarName the variable name that will be set with the value of the swing component
	 */
	public ForwardDoParms DO_COMPONENT_GET_VALUE(String componentName, String valueVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentValueVarName = valueVarName;
		p.action = EnumForwardAction.COMPONENT_GET_VALUE;
		return p;
	}
	
    /**
      * Swing component set value<br>
      * <p>
      * The named Swing component will be set with the input value.<br>
      * If the component name is not defined or the object value is not compatible with the swing component, no action will be taken.<br>
      * <p>
	  * @param componentName the Swing component name to set
	  * @param componentValue the value to set
	  * @param dummy a dummy parameter just to make the method unique
	 */
	public ForwardDoParms DO_COMPONENT_SET_VALUE(String componentName, Object componentValue, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentValue = componentValue;
		p.componentValueVarName = "";
		p.action = EnumForwardAction.COMPONENT_SET_VALUE;
		return p;
	}

    /**
     * Swing component set value<br>
     * <p>
     * The named Swing component will be set with the content of the variable input value.<br>
     * If the component name is not defined or the variable is undefined or the variable type is not compatible with the swing component, no action will be taken.<br>
     * <p>
	  * @param componentName the Swing component name to set
	  * @param componentValueVarName the variable name with the value to set
	 */
	public ForwardDoParms DO_COMPONENT_SET_VALUE(String componentName, String componentValueVarName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentValue = null;
		p.componentValueVarName = componentValueVarName;
		p.action = EnumForwardAction.COMPONENT_SET_VALUE;
		return p;
	}

    /**
     * Swing component set focus<br>
     * <p>
	  * @param componentName the Swing component name to set focus
	 */
	public ForwardDoParms DO_COMPONENT_SET_FOCUS(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMPONENT_SET_FOCUS;
		return p;
	}
	
	
    /**
     * Swing component set focusable<br>
     * <p>
	  * @param componentName the Swing component 
	  * @param componentFocusable true to make the Swing component focusable
	 */
	public ForwardDoParms DO_COMPONENT_SET_FOCUSABLE(String componentName, boolean focusable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentFocusable = focusable;
		p.action = EnumForwardAction.COMPONENT_SET_FOCUSABLE;
		return p;
	}
	
	
	   /**
     * Swing component set error code<br>
     * <p>
	  * @param componentName the Swing component to mark in error
	  * @param errorCode the application error code
	 */
	public ForwardDoParms DO_COMPONENT_SET_ERROR(String componentName, String errorCode) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentError = true;
		p.errorCode = errorCode;
		p.action = EnumForwardAction.COMPONENT_SET_ERROR;
		return p;
	}
 	
	
	   /**
     * Swing component set error<br>
     * <p>
	  * @param componentName the Swing component to mark in error
	  * @param componentError false for componenent with no error
	 */
	public ForwardDoParms DO_COMPONENT_SET_ERROR(String componentName, boolean componentError) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentError = true;
		p.action = EnumForwardAction.COMPONENT_SET_ERROR;
		return p;
	}
 	
	
    /**
     * Swing component set toolTip text<br>
     * <p>
	  * @param componentName the Swing component to set toolTip
	  * @param localeTooltipText the internationalization code
	 */
	public ForwardDoParms DO_COMPONENT_SET_TOOLTIP(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.localeToolTipText = "";
		p.action = EnumForwardAction.COMPONENT_SET_TOOLTIP;
		return p;
	}

	/**
     * Swing component set toolTip text with internationalization code<br>
     * <p>
     * The internationalization code will be used to get the text in any language.<br>
     * <p>
	  * @param componentName the Swing component to set toolTip
	  * @param localeTooltipText the internationalization code
	 */
	public ForwardDoParms DO_COMPONENT_SET_TOOLTIP(String componentName, String localeToolTipText) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.localeToolTipText = localeToolTipText;
		p.action = EnumForwardAction.COMPONENT_SET_TOOLTIP;
		return p;
	}
	
    /**
     * Swing component hilight<br>
     * <p>
     * The component will be hilighted according to standard forward.<br>
     * <p>
	  * @param componentName the Swing component to hilight
	  * @param componentHiglight as true for component highlighted
	 */
	public ForwardDoParms DO_COMPONENT_HILIGHT(String componentName, boolean componentHiglight) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentHiglight = componentHiglight;
		p.action = EnumForwardAction.COMPONENT_HILIGHT;
		return p;
	}


	
    /**
     * Swing component enable<br>
     * <p>
	 * @param componentName the Swing component to enable
	 * @param componentEnabled true for enable
	 */
	public ForwardDoParms DO_COMPONENT_ENABLE(String componentName, boolean componentEnabled) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentEnabled = componentEnabled;
		p.action = EnumForwardAction.COMPONENT_ENABLE;
		return p;
	}
	
	
	   /**
     *  Make a form or a Swing component panel or control visible<br>
     * <p>
	  * @param componentName the Swing component to make visible
	  * @param componentVisible true to make the Swing component visible
	 */
	public ForwardDoParms DO_COMPONENT_VISIBLE(String componentName, boolean componentVisible) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMPONENT_VISIBLE;
		return p;
	}
	
	   /**
      *  Masks one or more events for a component.<br>
      * <p>
      * When an event is masked, no action declared by function will be taken when it occurs.<br>
      * <p>
      * It true for componentEventsToMask is specified, all events will be stored in the forward monitor<br>
      * structures and no action declared will be executed if any event occurs.<br>
      * If false is specified with no event list following, all events currently to be masked for the component <br>
      * will be removed from the masked list events.<br>
      * <p>
      * When a specified event is not defined it will be discarded and no action will be taken.<br>
      * <p>
	  * @param componentName the Swing component to make visible
	  * @param componentEventsToMask as true to fire events to be masked, false for normal behaviour
	  * @param componentMaskedEvents as an array of events to be masked or not
	 */
	public ForwardDoParms DO_COMPONENT_MASK_EVENTS(String componentName, boolean componentEventsToMask, EnumForwardEvent ... componentMaskedEvents) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentEventsToMask = componentEventsToMask;
		p.componentMaskedEvents = componentMaskedEvents;
		p.action = EnumForwardAction.COMPONENT_MASK_EVENTS;
		return p;
	}
	
 	
    /**
     *  Check a swing control {@link JCheckBox}, {@link JCheckBoxMenuItem}, {@link JRadioButton} or {@link JRadioButtonMenuItem}<br>
     * <p>
	  * @param componentName the Swing component to check
	  * @param componentChecked true to make the Swing component checked
	 */
	public ForwardDoParms DO_STATE_SET_CHECKED(String componentName, boolean componentChecked) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.componentChecked = componentChecked;
		p.action = EnumForwardAction.COMPONENT_SET_CHECKED;
		return p;
	}
	
 	
	
	/**
     * Populates the comboBox by the execution of the logical data view<br>
     * <p>
     * The logical data view to be used will be that declared by DATA_SOURCE() at function declaration level.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ldvClassName the logical data view class name
	 * @param comboBoxLdvColSource the column of the logical data view to be used to populate the combo box
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_FROM_DB(String componentName, String ldvClassName, String comboBoxLdvColSource) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxLdvClassName =ldvClassName;
		p.comboBoxLdvColSource = comboBoxLdvColSource;
		p.comboBoxLdvRuleTableNum = -1;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_FROM_DB;
		return p;
	}
	
	/**
     * Populates the comboBox by the execution of the logical data view with data from a rule table, using a specific logical data view.<br>
     * <p>
     * The logical data view to be used must access with a sole FOR_EACH(), using the constructor for rule tables.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ldvClassName the logical data view class name
	 * @param ruleTableNum the number of th rule table to be used as input
	 * @param comboBoxLdvColSource the column of the logical data view to be used to populate the combo box
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_FROM_DB(String componentName, String ldvClassName, int ruleTableNum, String comboBoxLdvColSource) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxLdvClassName =ldvClassName;
		p.comboBoxLdvColSource = comboBoxLdvColSource;
		p.comboBoxLdvRuleTableNum = ruleTableNum;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_FROM_DB;
		return p;
	}
	
	/**
     * Populates the comboBox by the execution of the logical data view with data from a rule table, using the default logical data view.<br>
     * <p>
     * The logical data view used will be the generic default <code>LdvReadSetRuleTable</code> that exposes a generic <code>descItem</code> column.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ruleTableNum the number of th rule table to be used as input
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_FROM_DB(String componentName, int ruleTableNum) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxLdvClassName = LDV_RULE_TABLE_READ_SET_DEFAULT;
		p.comboBoxLdvColSource = LDV_RULE_TABLE_READ_SET_DEFAULT_COL_BOUND;
		p.comboBoxLdvRuleTableNum = ruleTableNum;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_FROM_DB;
		return p;
	}
	
	/**
     * Gets the ordinal 0-based of the combobox item, in the specified input function declared variable.<br>
     * <p>
     * This action can be executed in any time for a populated combobox but it's mainly useful when<br>
     * executed under <code>ON_COMBOBOX_SELECTED_ITEM</code> event to retrieve the ordinal directly
     * in a function variable declared by <code>VAR()</code>.
     * <p>
     * If the component is not declared or variables are not defined or of a wrong type, no action will be taken.<br>
     * if ordinalValue is null it will be considered the variable ordinalVarNameValue to get the item to locate.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ordinalVarNameReturned the function declared variable that will contains the ordinal as an Integer object
	 * @param ordinalVarNameValue the function declared variable that will contains the string value to get the ordinal
	 * @param ordinalValue as a string value of which to get the ordinal. If an empty string it will be used the variable ordinalVarNameValue
	 */
	public ForwardDoParms DO_COMBOBOX_GET_ORDINAL(String componentName, String ordinalVarNameReturned, String ordinalVarNameValue, String ordinalValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxOrdinalVarNameReturned = ordinalVarNameReturned;
		p.comboBoxOrdinalVarNameValue = ordinalVarNameValue;
		p.comboBoxOrdinalValue = ordinalVarNameValue;
		p.action = EnumForwardAction.COMBOBOX_GET_ORDINAL;
		return p;
	}
	 
	/**
     * Gets the ordinal, 0-based of the current selected combobox item, in the specified input function declared variable.<br>
     * <p>
     * This action can be executed in any time for a populated combobox but it's mainly useful when<br>
     * executed under <code>ON_COMBOBOX_SELECTED_ITEM</code> event to retrieve the ordinal directly
     * in a function variable declared by <code>VAR()</code>.
     * <p>
     * If the component is not declared or variables are not defined or of a wrong type, no action will be taken.<br>
       * <p>
	 * @param componentName the Swing comboBox name
	 * @param ordinalVarNameReturned the function declared variable that will contains the ordinal as an Integer object
	 */
	public ForwardDoParms DO_COMBOBOX_GET_ORDINAL(String componentName, String ordinalVarNameReturned) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxOrdinalVarNameReturned = ordinalVarNameReturned;
		p.comboBoxOrdinalVarNameValue = "";
		p.comboBoxOrdinalValue = "";
		p.action = EnumForwardAction.COMBOBOX_GET_ORDINAL;
		return p;
	}
	 


	/**
     * Gets the object bound of the current selected combobox item, in the specified input function declared variable.<br>
     * <p>
     * This action can be executed in any time for a populated combobox but it's mainly useful when<br>
     * executed under <code>ON_COMBOBOX_SELECTED_ITEM</code> event to retrieve the object bound directly
     * in a function variable declared by <code>VAR()</code>.
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param varNameObjectBound the function declared variable that will contains the object bound
	 */
	public ForwardDoParms DO_COMBOBOX_GET_OBJECT_BOUND(String componentName, String varNameObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxVarNameObjectBound = varNameObjectBound;
		p.action = EnumForwardAction.COMBOBOX_GET_OBJECT_BOUND;
		return p;
	}
	 
	/**
     * Marks the current item to be used to populate the combobox as to be discarded<br>
     * <p>
     * This action will be effective only when it's executed under ON_COMBOBOX_BEFORE_ADD_ELEMENT event.<br>
     * The only effect of this action is to execute the <code>setComboBoxItemToDiscard(true)</code> method,
     * in the system object active runtime.
     * <p>
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_DISCARD_ITEM() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.COMBOBOX_POPULATE_DISCARD_ITEM;
		return p;
	}
	
	/**
     * Set a new text value for the item to be inserted in the combobox at populatiopn from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_COMBOBOX_BEFORE_ADD_ELEMENT event.<br>
     * The only effect of this action is to execute the <code>setComboBoxSelectedItem()</code> method, <br>
     * in the system object active runtime, with the ne value.
     * <p>
     * @param itemTextValue
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_MODIFY_ITEM(String itemTextValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.comboBoxItemText = itemTextValue;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_MODIFY_ITEM;
		return p;
	}

	/**
     * Set the object bound for the item to be inserted in the combobox at populatiopn from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_COMBOBOX_BEFORE_ADD_ELEMENT event<br>
     * The only effect of this action is to execute the <code>setComboBoxBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value, causing the new object to be bound to the<br>
     * item to add.
     * <p>
     * @param componentName the swing component name
     * @param itemObjectBound the object to be bound to the item
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND(String componentName, Object itemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemObjectBound = itemObjectBound;
		p.comboBoxLdvClassName = "";
		p.comboBoxLdvColObjectBound = "";
		p.comboBoxObjectBoundToInteger = false;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_SET_OBJECT_BOUND;
		return p;
	}
	
	
	/**
     * Set the object bound for the item to be inserted in the combobox at populatiopn from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_COMBOBOX_BEFORE_ADD_ELEMENT event<br>
     * The only effect of this action is to execute the <code>setComboBoxBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value, causing the new object to be bound to the<br>
     * item to add.<br>
     * The new object bound value will be the content of the specified column of the logical data view<br>
     * currently used to populate the combo box.<br>
     * <p>
     * @param componentName the swing component name
     * @param ldvClassName the logical data view name currently used to populate the combobx
     * @param ldvColObjectBound the column name to use as object bound
	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND(String componentName, String ldvClassName, String ldvColObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemObjectBound = null;
		p.comboBoxLdvClassName = ldvClassName;
		p.comboBoxLdvColObjectBound = ldvColObjectBound;
		p.comboBoxObjectBoundToInteger = false;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_SET_OBJECT_BOUND;
		return p;
	}
	
	/**
     * Set the object bound for the item to be inserted in the combobox at populatiopn time from db.<br>
     * <p>
     * This action will be effective only when it's executed under ON_COMBOBOX_BEFORE_ADD_ELEMENT event<br>
     * The only effect of this action is to execute the <code>setComboBoxBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value, causing the new object to be bound to the item to add.<br>
     * The new object bound value will be the content of the specified column of the logical data view<br>
     * currently used to populate the combo box.<br>
     * <p>
     * When the column bound is string defined and contains only numeric string values, as for all forward rule tables,<br>
     * may be necessary store the bound value directly as an Integer object.<br>
     * To  do so, it needs to code <code>true</code> in the <code>objectBoundToInteger</code> parameter.
     * <p>
     * @param componentName the swing component name
     * @param ldvClassName the logical data view name currently used to populate the combobx
     * @param ldvColObjectBound the column name to use as object bound
     * @param objectBoundToInteger as a boolean value to convert the object bound from String to integer.<br>
 	 */
	public ForwardDoParms DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND(String componentName, String ldvClassName, String ldvColObjectBound, boolean objectBoundToInteger) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemObjectBound = null;
		p.comboBoxLdvClassName = ldvClassName;
		p.comboBoxLdvColObjectBound = ldvColObjectBound;
		p.comboBoxObjectBoundToInteger = objectBoundToInteger;
		p.action = EnumForwardAction.COMBOBOX_POPULATE_SET_OBJECT_BOUND;
		return p;
	}
	
	
    /**
     * Clear the comboBox and any application object bound<br>
     * <p>
	  * @param componentName the Swing comboBox name
	 */
	public ForwardDoParms DO_COMBOBOX_CLEAR(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMBOBOX_CLEAR;
		return p;
	}
	
    /**
     * Select a specific comboBox item<br>
     * <p>
	  * @param componentName the Swing comboBox name
	  * @param comboBoxItemIndex the index of item to select
	 */
	public ForwardDoParms DO_COMBOBOX_SELECT_ITEM(String componentName, int comboBoxItemIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemIndex = comboBoxItemIndex;
		p.action = EnumForwardAction.COMBOBOX_SELECT_ITEM;
		return p;
	}
	
    /**
     * Unselect the currently selected comboBox item<br>
     * <p>
	  * @param componentName the Swing comboBox name
	 */
	public ForwardDoParms DO_COMBOBOX_UNSELECT_ITEM(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMBOBOX_UNSELECT_ITEM;
		return p;
	}
	
    /**
     * Append a comboBox item<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of comboBox.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param comboBoxItemText the text item value
	 * @param comboBoxItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_COMBOBOX_APPEND_ITEM(String componentName, String comboBoxItemText, Object comboBoxItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemText = comboBoxItemText;
		p.comboBoxItemObjectBound = comboBoxItemObjectBound;
		p.action = EnumForwardAction.COMBOBOX_APPEND_ITEM;
		return p;
	}

	/**
     * Insert a comboBox item before the item index currently selected<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of comboBox.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param comboBoxItemText the text item value
	 * @param comboBoxItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_COMBOBOX_INSERT_ITEM(String componentName, String comboBoxItemText, Object comboBoxItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemText = comboBoxItemText;
		p.comboBoxItemObjectBound = comboBoxItemObjectBound;
		p.action = EnumForwardAction.COMBOBOX_INSERT_ITEM;
		return p;
	}

	/**
     * Delete a specific comboBox item<br>
     * <p>
	  * @param componentName the Swing comboBox name
	  * @param comboBoxItemIndex the index of item to select
	 */
	public ForwardDoParms DO_COMBOBOX_DELETE_ITEM(String componentName, int comboBoxItemIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemIndex = comboBoxItemIndex;
		p.action = EnumForwardAction.COMBOBOX_DELETE_ITEM;
		return p;
	}
	
    /**
     * Delete the currently selectedc comboBox item<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 */
	public ForwardDoParms DO_COMBOBOX_DELETE_SELECTED_ITEM(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.COMBOBOX_DELETE_SELECTED_ITEM;
		return p;
	}
	
    /**
     * Update the comboBox item currently selected<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of comboBox.<br>
     * This constructor let the application to update the object bound or to nullify it.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param comboBoxItemText the text item value
	 * @param comboBoxItemIndex the index of item to update
	 * @param comboBoxItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_COMBOBOX_UPDATE_SELECTED_ITEM(String componentName, String comboBoxItemText, Object comboBoxItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemText = comboBoxItemText;
		p.comboBoxItemObjectBound = comboBoxItemObjectBound;
		p.comboBoxObjectBoundToUpdate = true;
		p.action = EnumForwardAction.COMBOBOX_UPDATE_SELECTED_ITEM;
		return p;
	}
	
    /**
     * Update the comboBox item currently selected<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of comboBox.<br>
     * This constructor let the application to update the object bound or to nullify it.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param comboBoxItemText the text item value
	 * @param comboBoxItemIndex the index of item to update
	 * @param comboBoxItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_COMBOBOX_UPDATE_SELECTED_ITEM(String componentName, String comboBoxItemText) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxItemText = comboBoxItemText;
		p.comboBoxItemObjectBound = null;
		p.comboBoxObjectBoundToUpdate = false;
		p.action = EnumForwardAction.COMBOBOX_UPDATE_SELECTED_ITEM;
		return p;
	}
	

	
	/**
     * Populates the list by the execution of the logical data view<br>
     * <p>
     * The logical data view to be used will be that declared by DATA_SOURCE() at function declaration level.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ldvClassName the logical data view class name
	 * @param listLdvColSource the column of the logical data view to be used to populate the list
	 */
	public ForwardDoParms DO_LIST_POPULATE_FROM_DB(String componentName, String ldvClassName, String listLdvColSource) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listLdvClassName =ldvClassName;
		p.listLdvColSource = listLdvColSource;
		p.action = EnumForwardAction.LIST_POPULATE_FROM_DB;
		return p;
	}


	/**
     * Marks the current item to be used to populate the list as to be discarded<br>
     * <p>
     * This action will be effective only when it's executed under ON_LIST_BEFORE_ADD_ELEMENT event.<br>
     * The only effect of this action is to execute the <code>setListItemToDiscard(true)</code> method,
     * in the system object active runtime.
     * <p>
	 */
	public ForwardDoParms DO_LIST_POPULATE_DISCARD_ITEM() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.LIST_POPULATE_DISCARD_ITEM;
		return p;
	}
	
	/**
     * Set a new text value for the item to be inserted in the combobox at populatiopn from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_LIST_BEFORE_ADD_ELEMENT event.<br>
     * The only effect of this action is to execute the <code>setListSelectedItem()</code> method, <br>
     * in the system object active runtime, with the new value.
     * <p>
     * @param itemTextValue
	 */
	public ForwardDoParms DO_LIST_POPULATE_MODIFY_ITEM(String itemTextValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.listItemText = itemTextValue;
		p.action = EnumForwardAction.LIST_POPULATE_MODIFY_ITEM;
		return p;
	}

	/**
     * Set the object bound for the item to be inserted in the combobox at populatiopn from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_LIST_BEFORE_ADD_ELEMENT event.<br>
     * The only effect of this action is to execute the <code>setListBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value.
     * <p>
     * @param itemObjectBound
	 */
	public ForwardDoParms DO_LIST_POPULATE_SET_OBJECT_BOUND(Object itemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.listItemObjectBound = itemObjectBound;
		p.action = EnumForwardAction.LIST_POPULATE_SET_OBJECT_BOUND;
		return p;
	}

	/**
     * Gets the current object bound in the specified input function declared variable.<br>
     * <p>
 	 * @param componentName the Swing list name
	 * @param varNameObjectBound the function declared variable that will contains the object bound
	 */
	public ForwardDoParms DO_LIST_GET_OBJECT_BOUND(String componentName, String varNameObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.comboBoxVarNameObjectBound = varNameObjectBound;
		p.action = EnumForwardAction.LIST_GET_OBJECT_BOUND;
		return p;
	}

    /**
     * Clear the list and any application object bound<br>
     * <p>
	  * @param componentName the Swing list name
	 */
	public ForwardDoParms DO_LIST_CLEAR(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.LIST_CLEAR;
		return p;

	}
	
    /**
     *  Select one or more items from a list<br>
     * <p>
     * Indexes are 0-based list item numbers.<br>
     * For not valid indexes no action will be taken.<br>
     * <p>
     * 
	  * @param componentName the Swing list name
	  * @param listItemIndexes the undexes of items to select
	 */
	public ForwardDoParms DO_LIST_SELECT_ITEMS(String componentName, int ... listItemIndexes) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemIndexes = listItemIndexes;
		p.action = EnumForwardAction.LIST_SELECT_ITEMS;
		return p;

	}
	
    /**
     *  Append a text item to a list<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * In this constructor no bound object it's declared and will be set to null.<br>
     * <p>
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
     * 
	 */
	public ForwardDoParms DO_LIST_APPEND_ITEM(String componentName, String listItemText) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listItemObjectBound = null;
		p.action = EnumForwardAction.LIST_APPEND_ITEM;
		return p;
	}
    /**
     *  Append a text item to a list<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * <p>
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
	 * @param listItemObjectBound the object bound to the item
     * 
	 */
	public ForwardDoParms DO_LIST_APPEND_ITEM(String componentName, String listItemText, Object listItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listItemObjectBound = listItemObjectBound;
		p.action = EnumForwardAction.LIST_APPEND_ITEM;
		return p;
	}
	
    /**
     *  Delete one or more items from a list<br>
     * <p>
     * Indexes are 0-based list item numbers and can be declared in any order.<br>
     * For not valid indexes no action will be taken.<br>
     * For any list item deleted will be removed the bound object too, if any.<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 * @param listItemIndexes the undexes of items to select
	 */
	public ForwardDoParms DO_LIST_DELETE_ITEMS(String componentName, int ... listItemIndexes) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemIndexes = listItemIndexes;
		p.action = EnumForwardAction.LIST_DELETE_ITEMS;
		return p;
	}
	
    /**
     * Delete all currently selected  items from a list<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 */
	public ForwardDoParms DO_LIST_DELETE_SELECTED_ITEMS(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.LIST_DELETE_SELECTED_ITEMS;
		return p;
	}
	
    /**
     * Update the currently selected item from a list or the first if more than 1 is selected<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * In p constructor no bound object it's declared and its value will be not set.<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
	 */
	public ForwardDoParms DO_LIST_UPDATE_SELECTED_ITEM(String componentName, String listItemText) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listObjectBoundToUpdate = false;
		p.action = EnumForwardAction.LIST_UPDATE_SELECTED_ITEM;
		return p;
	}
    /**
     * Update the currently selected item from a list or the first if more than 1 is selected<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * In p constructor the bound object declared will be updated for the selected item<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
	 * @param listItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_LIST_UPDATE_SELECTED_ITEM(String componentName, String listItemText, Object listItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listItemObjectBound = listItemObjectBound;
		p.listObjectBoundToUpdate = true;
		p.action = EnumForwardAction.LIST_UPDATE_SELECTED_ITEM;
		return p;
	}
	
    /**
     * Update the required item from a list<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * In p constructor no bound object it's declared and its value will be not set.<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
	 * @param listItemIndex the index of item to update
	 */
	public ForwardDoParms DO_LIST_UPDATE_ITEM(String componentName, String listItemText, int listItemIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listObjectBoundToUpdate = false;
		p.action = EnumForwardAction.LIST_UPDATE_ITEM;
		return p;
	}
    /**
     * Update the required item from a list<br>
     * <p>
     * Any item can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the item deleting or the clear of list.<br>
     * In p constructor the bound object declared will be updated for the required item<br>
     * <p>
     * 
	 * @param componentName the Swing list name
	 * @param listItemText the text item value
	 * @param listItemIndex the index of item to update
	 * @param listItemObjectBound the object bound to the item
	 */
	public ForwardDoParms DO_LIST_UPDATE_ITEM(String componentName, String listItemText, int listItemIndex, Object listItemObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listItemText = listItemText;
		p.listItemIndex = listItemIndex;
		p.listItemObjectBound = listItemObjectBound;
		p.listObjectBoundToUpdate = true;
		p.action = EnumForwardAction.LIST_UPDATE_ITEM;
		return p;
	}
	
    /**
     * Make a list item visible, when it is not in the visible scrollable area.<br>
     * <p>
     * If the item index is not valid, no action will be taken.<br>
     * <p>
     * <p>
	 * @param componentName the Swing list name
	 * @param listItemIndex the index of item to update
	 */
	public ForwardDoParms DO_LIST_MAKE_VISIBLE_ITEM(String componentName, int listItemIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.listObjectBoundToUpdate = false;
		p.action = EnumForwardAction.LIST_MAKE_VISIBLE_ITEM;
		return p;
	}

	
	/**
     * Populates the table by the execution of the logical data view<br>
     * <p>
     * The logical data view to be used will be that declared by DATA_SOURCE() at function declaration level.<br>
     * The logical data view has been declared to expose a set o columns with the java name coded in the bean entity class,<br>
     * eventually renamed by means of the AS clause at logical data view declaration level.<br>
     * <p>
     * By default, all swing controls or variables declared by VAR(), will be updated with the content of current table row,<br>
     * for all columns with the same name.<br>
     * <p>
     * First, input java fields and columns to bind, are used to populate the row, then only columns name exposed by logical<br>
     * data view are used.<br>
     * When name or column type doesn't match with a function variable or a swing component name, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ldvClassName the logical data view class name
	 * @param tableJavaFieldsBound as a String array with names of swing controls or declared variables
	 * @param tableLdvColumnsBound as a String array with names of column names exposed by logical data view
	 */
	public ForwardDoParms DO_TABLE_POPULATE_FROM_DB(String componentName, String ldvClassName, String[] tableJavaFieldsBound, String[] tableLdvColumnsBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableLdvClassName =ldvClassName;
		p.tableJavaFieldsBound = tableJavaFieldsBound;
		p.tableLdvColumnsBound = tableLdvColumnsBound;
		p.action = EnumForwardAction.TABLE_POPULATE_FROM_DB;
		return p;
	}
	
	/**
     * Populates the table by the execution of the logical data view<br>
     * <p>
     * The logical data view to be used will be that declared by DATA_SOURCE() at function declaration level.<br>
     * The logical data view has been declared to expose a set o columns with the java name coded in the bean entity class,<br>
     * eventually renamed by means of the AS clause at logical data view declaration level.<br>
     * <br>
     * All swing controls or variables declared by VAR(), will be updated with the content of current table row,<br>
     * for all columns with the same name.<br>
     * When name or column type doesn't match with a function variable or a swing component name, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing comboBox name
	 * @param ldvClassName the logical data view class name
	 */
	public ForwardDoParms DO_TABLE_POPULATE_FROM_DB(String componentName, String ldvClassName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableLdvClassName =ldvClassName;
		p.tableJavaFieldsBound = new String[0];
		p.tableLdvColumnsBound = new String[0];
		p.action = EnumForwardAction.TABLE_POPULATE_FROM_DB;
		return p;
	}
	
	/**
     * Marks the current row to be used to populate the table as to be discarded<br>
     * <p>
     * This action will be effective only when it's executed under ON_TABLE_BEFORE_ADD_ROW event.<br>
     * The only effect of this action is to execute the <code>setTableRowToDiscard(true)</code> method,
     * in the system object active runtime.
     * <p>
	 */
	public ForwardDoParms DO_TABLE_POPULATE_DISCARD_ROW() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.TABLE_POPULATE_DISCARD_ROW;
		return p;
	}
	
	/**
     * Set a new text value for the row column to be inserted in the table at population from db time.<br>
     * <p>
     * This action will be effective only when it's executed under ON_TABLE_BEFORE_ADD_ROW event.<br>
     * The only effect of this action is to execute the <code>setComboBoxSelectedItem()</code> method, <br>
     * in the system object active runtime, with the ne value.
     * <p>
     * @param itemTextValue
	 */
	public ForwardDoParms DO_TABLE_POPULATE_MODIFY_COLUMNS(String componentName, String ldvClassName, String ldvColumnName, Object ldvColumnNewValue) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableLdvClassName =ldvClassName;
		p.tableLdvColumnName = ldvColumnName;
		p.tableLdvColumnNewValue = ldvColumnNewValue;
		p.action = EnumForwardAction.TABLE_POPULATE_MODIFY_COLUMN;
		return p;
	}

	/**
     * Set the object bound for the row to be inserted in the table at populatiopn time from db.<br>
     * <p>
     * This action will be effective only when it's executed under ON_TABLE_BEFORE_ADD_ROW event<br>
     * The only effect of this action is to execute the <code>setTableBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value, causing the new object to be bound to the row to add.<br>
     * The new object bound value will be the content of the specified column of the logical data view<br>
     * currently used to populate the table.<br>
     * <p>
     * This action, executed under the table populating process, should be used when the object bound is an <br>
     * elementary variable, identified by a column of the logical data view used to populate the table.<br>
     * <p>
     * When data to bound are more then one logical data view column or they are a complex structure, the application<br>
     * must code a reusable method to store the desired object bound.<br> 
     * So, under ON_TABLE_BEFORE_ADD_ROW event, a  <code>DO_EXEC_METHOD()<code> must be declared.<br>
     * <p>
     * Here is an example to set as objecy bound a String array with three column values:<pre>
    	ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_metricsViolations", DO_EXEC_METHOD("metricsViolationsSetObjectBound"));	
           --
           --
           --
	    public int metricsViolationsSetObjectBound(ForwardSystem s, ForwardFunction f) {
	    	ForwardLogicalDataView ldv = null;
	     	String[] ar_objBound = null;
	    	
	    	ldv = (ForwardLogicalDataView) s.getLdvObject();
	    	ar_objBound = new String[3];
	    	ar_objBound[0] = ldv.getValueString("originViolation");
	    	ar_objBound[1] = ldv.getValueString("originViolationRows");
	    	ar_objBound[2] = ldv.getValueString("originViolationRowsCopy");
	    	s.setTableBoundObject(ar_objBound);
	    	return 0;
	    }
     * </pre>
     * The row object bound can then retreived at the table row selection.
     * Here is an example to get the object bound previously stored to populate, by manual code, another table (tb_metricsViolationsOrigin):<pre>      		
     * ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_metricsViolations",  DO_TABLE_DELETE_ALL_ROWS("tb_metricsViolationsOrigin")
     				                                                            , DO_EXEC_METHOD("metricsViolationsOriginPopulate"))
     				                                                            
	    public int metricsViolationsOriginPopulate(ForwardSystem s, ForwardFunction f) {
	    	
	    	ForwardTableModel tableModelViolationsOrigin = null;
	       	String[] ar_originViolation = null;
	      	String[] ar_originViolationRows = null;
	      	String[] ar_originViolationRowsCopy = null;
	     	String[] ar_objBound = null;
	    	String originViolation = "";           						 
	    	String originViolationRows = "";           					 
	    	String originViolationRowsCopy = "";           			 
	        String instr = "";
	        String row = "";
	        String rowCopy = "";
	    	
	    	ar_objBound = (String[]) s.getTableBoundObject();
	    	originViolation = ar_objBound[0];
	       	originViolationRows = ar_objBound[1];
	       	originViolationRowsCopy = ar_objBound[2];
	    	
	     	ar_originViolation = originViolation.split(" ");
	     	ar_originViolationRows = originViolationRows.split(" ");
	     	ar_originViolationRowsCopy = originViolationRowsCopy.split(" ");
	    	
	     	if (ar_objBound[2].equals("null")) {
	     		ar_originViolationRowsCopy = new String[ar_originViolationRows.length];
	     		for (int i = 0; i < ar_originViolationRowsCopy.length; i++) {
	     			ar_originViolationRowsCopy[i] = "";
				}
			}
	
	     	if (originViolation.equals("")) {
	     		ar_originViolation = new String[ar_originViolationRows.length];
	    		for (int i = 0; i < ar_originViolation.length; i++) {
	    			ar_originViolation[i] = "";
				}
			}
	     	
	     	// Modello tabella
	     	tableModelViolationsOrigin = getPanelComponent("tb_metricsViolationsOrigin").getTableModel();
	
	     	// Scan  
	     	for (int i = 1; i < ar_originViolation.length; i++) {
	     		instr = ar_originViolation[i];
	     		row = ar_originViolationRows[i];
	     		rowCopy = ar_originViolationRowsCopy[i];
	         	al_rowToAppend = new ArrayList<Object> ();
	         	al_rowToAppend.add(instr);
	         	al_rowToAppend.add(row);
	         	if (rowCopy.equals("-1")) {
	         		rowCopy = "";
				}
	         	al_rowToAppend.add(rowCopy);
	     		tableModelViolationsOrigin._appendRow(al_rowToAppend);
			}
	    	return 0;
	    }
      </pre>
     * 
     * @param componentName the swing component table name
     * @param ldvClassName the logical data view name currently used to populate the table
     * @param ldvColObjectBound the column name to use as object bound
  	 */
	public ForwardDoParms DO_TABLE_POPULATE_SET_OBJECT_BOUND(String componentName, String ldvClassName, String ldvColObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowObjectBound = null;
		p.tableLdvClassName = ldvClassName;
		p.tableLdvColObjectBound = ldvColObjectBound;
		p.action = EnumForwardAction.TABLE_POPULATE_SET_OBJECT_BOUND;
		return p;
	}
	
	/**
     * Set the object bound for the row to be inserted in the table at population time from db.<br>
     * <p>
     * This action will be effective only when it's executed under ON_TABLE_BEFORE_ADD_ROW event<br>
     * The only effect of this action is to execute the <code>setTableBoundObject()</code> method, <br>
     * in the system object active runtime, with the new value, causing the new object to be bound to the<br>
     * item to add.
     * <p>
     * @param componentName the swing component table name
     * @param rowObjectBound the object to be bound to the row
	 */
	public ForwardDoParms DO_TABLE_POPULATE_SET_OBJECT_BOUND(String componentName, Object rowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowObjectBound = rowObjectBound;
		p.tableLdvClassName = "";
		p.tableLdvColObjectBound = "";
		p.action = EnumForwardAction.TABLE_POPULATE_SET_OBJECT_BOUND;
		return p;
	}

	
	/**
     * Gets the current object bound in the specified input function declared variable.<br>
     * <p>
 	 * @param componentName the Swing table name
	 * @param varNameObjectBound the function declared variable that will contains the object bound
	 */
	public ForwardDoParms DO_TABLE_GET_OBJECT_BOUND(String componentName, String varNameObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableVarNameObjectBound = varNameObjectBound;
		p.action = EnumForwardAction.TABLE_GET_OBJECT_BOUND;
		return p;
	}



    /**
     * Clear a swing table object and all bound objects, if any.<br>
     * The table definition, as header and columns, will be cleared too.<br>
     * <p>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * <p>
	 * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_CLEAR(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_CLEAR;
		return p;
	}
	
    /**
     * Select one or more rows from a  swing table object.<br>
     * <p>
     * It's posssible specify one row number, to select just that or two row number to<br>
     * make an inclusive selection<br>
     * <p>
     * If a row index is out of range, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableRowIndexes the sequence of rows index to select
	 */
	public ForwardDoParms DO_TABLE_SELECT_ROWS(String componentName, int ...tableRowIndexes) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowIndexes = tableRowIndexes;
		p.action = EnumForwardAction.TABLE_SELECT_ROWS;
		return p;
	}
	
	   /**
     * Select one columns from a  swing table object.<br>
     * <p>
     * If a column index is out of range, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param col the column number to select
	 */
	public ForwardDoParms DO_TABLE_SELECT_COLS(String componentName, int col) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionColFrom = col;
		p.tableSelectionColTo = col;
		p.action = EnumForwardAction.TABLE_SELECT_COLS;
		return p;
	}
	
	   /**
     * Select a columns range from a  swing table object.<br>
     * <p>
     * If a column index is out of range, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param colFrom the column number start inclusive to select
	 * @param colTo the column number end inclusive to select
	 */
	public ForwardDoParms DO_TABLE_SELECT_COLS(String componentName, int colFrom, int colTo) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionColFrom = colFrom;
		p.tableSelectionColTo = colTo;
		p.action = EnumForwardAction.TABLE_SELECT_COLS;
		return p;
	}
	
     /**
     * Enable the swing table object for single row selection only.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_SINGLE(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionSingle = enable;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_SINGLE;
		return p;
	}
	
    /**
     * Enable the swing table object for single rows interval selection only.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_SINGLE_INTERVAL(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionSingleInterval = enable;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_SINGLE_INTERVAL;
		return p;
	}
	
    /**
     * Enable the swing table object for multiple rows intervals selection only.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_MULTIPLE_INTERVAL(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionMultipleInterval = enable;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_MULTIPLE_INTERVAL;
		return p;
	}
	
    /**
     * Enable the swing table object rows selection.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_ROWS(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionRows = enable;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_ROWS;
		return p;
	}
	
    /**
     * Enable the swing table object columns selection.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_COLS(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionCols = enable;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_COLS;
		return p;
	}
	
    /**
     * Enable the swing table object cells selection.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param enable as true for enable or false if not
	 */
	public ForwardDoParms DO_TABLE_ENABLE_SELECTION_CELLS(String componentName, boolean enable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_ENABLE_SELECTION_CELLS;
		return p;
	}
	
    /**
     * Sets the column header toolTip text.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColToolTipText the toolTip text displayed on column header mouse entered
	 * @param tableColIndex the column number, 0-based
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_TOOLTIP(String componentName, String tableColToolTipText, int tableColIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColToolTipText = tableColToolTipText;
		p.tableColIndex = tableColIndex;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_TOOLTIP;
		return p;
	}
	
    /**
     * Sets the column header toolTip text.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColToolTipText the toolTip text displayed on column header mouse entered
	 * @param tableColumnName as declared by function
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_TOOLTIP(String componentName, String tableColToolTipText, String tableColumnName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColToolTipText = tableColToolTipText;
		p.tableColIndex = -1;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_TOOLTIP;
		return p;
	}
	
    /**
     * Sets cell toolTip text.<br>
     * <p>
     * The cell is identified by a row number and a column number.
     * <br>
     * If row or column number are out of range, no action will be taken.
     * <br>
	 * @param componentName the Swing table name
	 * @param tableCellToolTipText the toolTip text displayed on column header mouse entered
	 * @param tableRowIndex the row number, 0-based
	 * @param tableColIndex the column number, 0-based
	 */
	public ForwardDoParms DO_TABLE_SET_CELL_TOOLTIP(String componentName, String tableCellToolTipText, int tableRowIndex, int tableColIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableCellToolTipText = tableCellToolTipText;
		p.tableRowIndex = tableRowIndex;
		p.tableColIndex = tableColIndex;
		p.action = EnumForwardAction.TABLE_SET_CELL_TOOLTIP;
		return p;
	}

	
    /**
     * Sets the resizable property of al currently selected swing table columns or all.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param onlySelected as true to make resizable only selected columns or false all columns
	 * @param resizable as true for resizable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_RESIZABLE(String componentName, boolean onlySelected, boolean resizable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnResizableNumStart = -1;
		p.tableColumnResizableNumEnd = -1;
		p.tableColumnResizableOnlySelectedCols = onlySelected;
		p.tableColumnResizable = resizable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_RESIZABLE;
		return p;
	}
	

    /**
     * Sets the resizable property of a swing table column.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the table
	 * @param resizable as true for resizable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_RESIZABLE(String componentName, String tableColumnName, boolean resizable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColumnResizableNumStart = -1;
		p.tableColumnResizableNumEnd = -1;
		p.tableColumnResizable = resizable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_RESIZABLE;
		return p;
	}
	   /**
     * Sets the resizable property of a swing table column.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColIndex the 0-based table column
	 * @param resizable as true for resizable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_RESIZABLE(String componentName, int tableColIndex, boolean resizable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnResizableNumStart = tableColIndex;
		p.tableColumnResizableNumEnd = -1;
		p.tableColumnResizable = resizable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_RESIZABLE;
		return p;
	}
	
	 /**
     * Sets the resizable property of a swing table columns range.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param colStart the 0-based table column start inclusive
	 * @param colEnd the 0-based table column end inclusive
	 * @param resizable as true for resizable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_RESIZABLE(String componentName, int colStart, int colEnd, boolean resizable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnResizableNumStart = colStart;
		p.tableColumnResizableNumEnd = colEnd;
		p.tableColumnResizable = resizable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_RESIZABLE;
		return p;
	}
	

	 /**
     * Sets the editable property of only selected swing table columns or all.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param onlySelected as true for make editable only selected columns, false for all columns
	 * @param onlySelected as true to make editable only selected columns or false all columns
	 * @param editable as true for editable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_EDITABLE(String componentName, boolean onlySelected, boolean editable) { 
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnEditableNumStart = -1;
		p.tableColumnEditableNumEnd = -1;
		p.tableColumnEditableOnlySelectedCols = onlySelected;
		p.tableColumnEditable = editable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_EDITABLE;
		return p;
	}
	
	 /**
     * Sets the editable property of a table column.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the table
	 * @param editable as true for editable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_EDITABLE(String componentName, String tableColumnName, boolean editable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColumnEditableNumStart = -1;
		p.tableColumnEditableNumEnd = -1;
		p.tableColumnEditable = editable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_EDITABLE;
		return p;
	}
	
    /**
     * Sets the resizable property of a swing table column.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColIndex the 0-based table column
	 * @param editable as true for editable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_EDITABLE(String componentName, int tableColIndex, boolean editable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnEditableNumStart = tableColIndex;
		p.tableColumnEditableNumEnd = -1;
		p.tableColumnEditable = editable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_EDITABLE;
		return p;
	}

	 /**
     * Sets the editable property of a swing table columns range.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param colStart the 0-based table column start inclusive
	 * @param colEnd the 0-based table column end inclusive
	 * @param editable as true for editable column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_EDITABLE(String componentName, int colStart, int colEnd, boolean editable) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnEditableNumStart = colStart;
		p.tableColumnEditableNumEnd = colEnd;
		p.tableColumnEditable = editable;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_EDITABLE;
		return p;
	}

	/**
     * Makes a table column currently selected hidden or visible.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param hidden as true for hidden column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_HIDDEN(String componentName, boolean hidden) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnHiddenNumStart = -1;
		p.tableColumnHiddenNumEnd = -1;
		p.tableColumnHidden = hidden;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_HIDDEN;
		return p;
	}
	
	/**
     * Makes a table column hidden or visible.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the column
	 * @param hidden as true for hidden column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_HIDDEN(String componentName, String tableColumnName, boolean hidden) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColumnHiddenNumStart = -1;
		p.tableColumnHiddenNumEnd = -1;
		p.tableColumnHidden = hidden;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_HIDDEN;
		return p;
	}
	
    /**
     * Makes a table column hidden or not<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColIndex the 0-based table column
	 * @param hidden as true for hidden column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_HIDDEN(String componentName, int tableColIndex, boolean hidden) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnHiddenNumStart = tableColIndex;
		p.tableColumnHiddenNumEnd = -1;
		p.tableColumnHidden = hidden;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_HIDDEN;
		return p;
	}
	
    /**
     * Makes a table columns range hidden or not<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param colStart the 0-based table column starting inclusive
	 * @param colEnd the 0-based table column ending inclusive
	 * @param hidden as true for hidden column
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_HIDDEN(String componentName, int colStart, int colEnd, boolean hidden) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColumnHiddenNumStart = colStart;
		p.tableColumnHiddenNumEnd = colEnd;
		p.tableColumnHidden = hidden;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_HIDDEN;
		return p;
	}
	
	/**
     * Sets the table column width in pixel.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the table
	 * @param width as the column width in pixel
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_WIDTH(String componentName, String tableColumnName, int width) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColIndex = -1;
		p.tableColumnWidth = width;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_WIDTH;
		return p;
	}
	
    /**
     * Sets the table column width in pixel.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColIndex the 0-based table column
	 * @param width as the column width in pixel
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_WIDTH(String componentName, int tableColIndex, int width) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColIndex = tableColIndex;
		p.tableColumnWidth = width;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_WIDTH;
		return p;
	}
	
    /**
     * Sets all table selected column width in pixel.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param width as the column width in pixel
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_WIDTH(String componentName, int width) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColIndex = -1;
		p.tableColumnWidth = width;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_WIDTH;
		return p;
	}
	
	
	/**
     * Sets if the table column width must fit automatically to the column contents.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the table
	 * @param widthToFit as a boolean true to fit column width to content
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_WIDTH_FITTING_CONTENT(String componentName, String tableColumnName, boolean widthToFit) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = tableColumnName;
		p.tableColIndex = -1;
		p.tableColumnWidthToFitContent = widthToFit;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_WIDTH_TO_FIT_CONTENT;
		return p;
	}
	
    /**
     * Sets if the table column width must fit automatically to the column contents.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColIndex the 0-based table column
	 * @param widthToFit as a boolean true to fit column width to content
	 */
	public ForwardDoParms DO_TABLE_COLUMN_WIDTH_FITTING_CONTENT(String componentName, int tableColIndex, boolean widthToFit) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColIndex = tableColIndex;
		p.tableColumnWidthToFitContent = widthToFit;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_WIDTH_TO_FIT_CONTENT;
		return p;
	}
	
	
	
	/**
     * Sets the margin between columns.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param margin as the gap in pixel between columns
	 */
	public ForwardDoParms DO_TABLE_SET_COLUMN_MARGIN(String componentName, int margin) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableColumnName = "";
		p.tableColIndex = -1;
		p.tableColumnMargin = margin;
		p.action = EnumForwardAction.TABLE_SET_COLUMN_MARGIN;
		return p;
	}
	
	
   /**
	 * Sets the amount of empty space between cells in adjacent rows.<br>
	 * <p>
	 * @param componentName the Swing table name
	 * @param margin as the gap in pixel before and after the row
	 */
	public ForwardDoParms DO_TABLE_SET_ROW_MARGIN(String componentName, int margin) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowMargin = margin;
		p.action = EnumForwardAction.TABLE_SET_ROW_MARGIN;
		return p;
	}
		
	   /**
  * Sets the amount of empty space between cells in adjacent rows.<br>
  * <p>
	 * @param componentName the Swing table name
	 * @param margin as the gap in pixel before and after the row as Integer
	 */
	public ForwardDoParms DO_TABLE_SET_ROW_MARGIN(String componentName, Integer margin) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowMargin = margin;
		p.action = EnumForwardAction.TABLE_SET_ROW_MARGIN;
		return p;
	}
		

	/**
     * Sets the height of all or selected table rows.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableColumnName the name assigned to the table
	 * @param height as the height of all rows in pixel
	 * @param onlySelectedRows as a true boolean value if only the height of currently selected rows have to be set, false for all rows.
	 */
	public ForwardDoParms DO_TABLE_SET_ROW_HEIGHT(String componentName, int height, boolean onlySelectedRows) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowHeightNumStart = -1;
		p.tableRowHeightNumEnd = -1;
		p.tableRowHeight = height;
		p.tableRowHeightOnlySelectedRows = onlySelectedRows;
		p.action = EnumForwardAction.TABLE_SET_ROW_HEIGHT;
		return p;
	}
	
	   /**
	    * Sets the height of a specific table row.<br>
	     * <p>
		 * @param componentName the Swing table name
		 * @param height as the height of all rows in pixel
		 * @param row the 0-based table row number
		 */
		public ForwardDoParms DO_TABLE_SET_ROW_HEIGHT(String componentName, int height, int row ) {
			ForwardDoParms p = null;
			p = new ForwardDoParms();
			p.componentName = componentName;
			p.tableRowHeightNumStart = row; 
			p.tableRowHeightNumEnd = -1; 
			p.tableRowHeight = height;
			p.action = EnumForwardAction.TABLE_SET_ROW_HEIGHT;
			return p;
		}
		
	   /**
	    * Sets the height of a specific table rows range.<br>
	     * <p>
		 * @param componentName the Swing table name
		 * @param height as the height of all rows in pixel
		 * @param row the 0-based table row number
		 */
		public ForwardDoParms DO_TABLE_SET_ROW_HEIGHT(String componentName, int height, int rowStart, int rowEnd ) {
			ForwardDoParms p = null;
			p = new ForwardDoParms();
			p.componentName = componentName;
			p.tableRowHeightNumStart = rowStart; 
			p.tableRowHeightNumEnd = rowEnd; 
			p.tableRowHeight = height;
			p.action = EnumForwardAction.TABLE_SET_ROW_HEIGHT;
			return p;
		}
			
	   /**
     * Sets the foreground Color of selected cells<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param row the 0-based table row number
	 * @param color as a Color foregrouund object
	 */
	public ForwardDoParms DO_TABLE_SET_SELECTION_FOREGROUND_COLOR(String componentName, Color color) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionForegroundColor = color;
		p.action = EnumForwardAction.TABLE_SET_SELECTION_FOREGROUND_COLOR;
		return p;
	}
		
	/**
     * Sets the background Color of selected cells<br>
  	 * <p>
	 * @param componentName the Swing table name
	 * @param row the 0-based table row number
	 * @param color as a Color background object
	 */
	public ForwardDoParms DO_TABLE_SET_SELECTION_BACKGROUND_COLOR(String componentName, Color color) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableSelectionBackgroundColor = color;
		p.action = EnumForwardAction.TABLE_SET_SELECTION_BACKGROUND_COLOR;
		return p;
	}
	
	/**
     * Sets the table grid, with vertical and horizontal lines, visible<br>
  	 * <p>
	 * @param componentName the Swing table name
	 * @param visible as true to make the grid visible
	 */
	public ForwardDoParms DO_TABLE_SET_GRID_COLOR(String componentName, Color color) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableGridColor = color;
		p.action = EnumForwardAction.TABLE_SET_GRID_COLOR;
		return p;
	}
	
	/**
     * Sets the table grid, with vertical and horizontal lines, visible<br>
  	 * <p>
	 * @param componentName the Swing table name
	 * @param visible as true to make the grid visible
	 */
	public ForwardDoParms DO_TABLE_SET_GRID_VISIBLE(String componentName, boolean visible) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableGridVisible = visible;
		p.action = EnumForwardAction.TABLE_SET_GRID_VISIBLE;
		return p;
	}
	
	/**
     * Sets if table vertical lines have to be visible<br>
  	 * <p>
	 * @param componentName the Swing table name
	 * @param visible as true to make vertical lines visible
	 */
	public ForwardDoParms DO_TABLE_SET_VERTICAL_LINES_VISIBLE(String componentName, boolean visible) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableVerticalLinesVisible = visible;
		p.action = EnumForwardAction.TABLE_SET_VERTICAL_LINES_VISIBLE;
		return p;
	}
	
	/**
     * Sets if table horizontal lines have to be visible<br>
  	 * <p>
	 * @param componentName the Swing table name
	 * @param visible as true to make horizontal lines visible
	 */
	public ForwardDoParms DO_TABLE_SET_HORIZONTAL_LINES_VISIBLE(String componentName, boolean visible) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableHorizontalLinesVisible = visible;
		p.action = EnumForwardAction.TABLE_SET_HORIZONTAL_LINES_VISIBLE;
		return p;
	}
	
	
	/**
     * Sets the fillsViewPortHeigh table parameter<br>
  	 * <p>
  	 * A true value causes all the table area to be filled.
  	 * <br>
	 * @param componentName the Swing table name
	 * @param fill as true to fill all table area
	 */
	public ForwardDoParms DO_TABLE_SET_FILLS_VIEWPORT_HEIGH(String componentName, boolean fill) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableFillViewPortHeight = fill;
		p.action = EnumForwardAction.TABLE_SET_FILLS_VIEWPORT_HEIGH;
		return p;
	}
	
	/**
     * Clear all rows and columns active selection<br>
  	 * <p>
 	 * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_CLEAR_SELECTION(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_CLEAR_SELECTION;
		return p;
	}
	
	/**
     * Start the editing of a cell<br>
  	 * <p>
  	 * If the cell row or cell number are out of range, no action will be taken.<br>
  	 * <p>
 	 * @param componentName the Swing table name
 	 * @param row the cell num row
 	 * @param col the cell num column
	 */
	public ForwardDoParms DO_TABLE_EDIT_CELL_AT(String componentName, int row, int col) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowIndex = row;
		p.tableColIndex = col;
		p.action = EnumForwardAction.TABLE_EDIT_CELL_AT;
		return p;
	}
	
	/**
     * Print the table<br>
  	 * <p>
 	 * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_PRINT(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_PRINT;
		return p;
	}
	
	/**
     * Update panels with values of the current selected row.<br>
  	 * <p>
  	 * At declaring time have been automatically created application variables<br>
  	 * for each column declared, with the same name of the column and of the same type.<br>
  	 * On table row selection, the forward monitor automatically updates these variables <br>
  	 * with the current values of the selected row.<br>
  	 * This action update all swing controls with the same name declared in any panel.<br>
  	 * A swing control for the object bound will be updated too, if the name is "tableName"+"_boumd".<br>
  	 * The update is made with respect of the variable type and the type of swing control.<br>
  	 * Thereby a boolean variable will cause the update of swing controls like {@link JCheckBox},<br>
  	 * JToggleButton and so on, an integer variable the update of swing controls <br>
  	 * like {@link JFormattedTextField}, {@link JProgressBar}, {@link JSlider} and so on.<br>
  	 * <p>
  	 * In other words will be updated any panel control with the same declared name and with<br>
  	 * a compatible type.<br>
  	 * <p>
  	 * 
 	 * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_REFRESH_PANEL_FROM_VARS(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_REFRESH_PANEL_FROM_VARS;
		return p;
	}
	
	/**
     * Update all table columns variables with values of controls with the same name layed out on panels<br>
  	 * <p>
  	 * At declaring time have been automatically created application variables<br>
  	 * for each column declared, with the same name of the column and of the same type.<br>
  	 * On table row selection, the forward monitor automatically updates these variables <br>
  	 * with the current values of the selected row.<br>
  	 * This action update all variables with the value of the swing control with the same name declared in any panel.<br>
  	 * The variable for the object bound will be updated too, if the name of the swing controlo is "tableName"+"_boumd".<br>
  	 * The update is made with respect of the variable type and the type of swing control.<br>
  	 * <p>
  	 * 
 	 * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_REFRESH_VARS_FROM_PANEL(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.action = EnumForwardAction.TABLE_REFRESH_VARS_FROM_PANEL;
		return p;
	}
	
	
	
	/**
     * Append a row to a swing table object using current column variables<br>
     * <p>
     * The row will be appended with current values of column variables, set by a row selection<br>
     * with a declared <code>TABLE_REFRESH_VARS_FROM_PANEL</code> action oe set by application,<br>
     * by a specific logic method.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * The bound object value will be stored with the variable value, automatically created by forward, at<br>
     * function declaration time,  with the name declared by DATA_VALUES_BOUND() section. Null is a valid value.
     * <p>
	 * @param tableName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_APPEND_ROW(String tableName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowObjectBound = null;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = true;
		p.action = EnumForwardAction.TABLE_APPEND_ROW;
		return p;
	}

	   /**
  * Append a row to a swing table object using current column variables<br>
  * <p>
  * The row will be appended with current values of column variables, set by a row selection<br>
  * with a declared <code>TABLE_REFRESH_VARS_FROM_PANEL</code> action oe set by application,<br>
  * by a specific logic method.<br>
  * <br>
  * Any row can be bound with an application dependent object, like a database key,<br>
  * a text or even a structured object.<br>
  * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
  * The bound object value will be stored with the input value.<br>
   * <p>
	 * @param tableName the Swing table name
	 * @param tableRowObjectBound the bound object. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_APPEND_ROW(String tableName, Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_APPEND_ROW;
		return p;
	}

	/**
     * Append a row to a swing table object.<br>
     * <p>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * <br>
     * If the number of input row objects is less then columns defined or some column object type is not equal 
     * to the column type declared, no action will be taken.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableRowObjects the array with row objects
	 * @param tableRowObjectBound the bound object. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_APPEND_ROW(String componentName, Object tableRowObjects[] , Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowObjects = tableRowObjects;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_APPEND_ROW;
		return p;
	}
	
	 /**
     * Insert a row in a swing table object at the row specified.<br>
     * <p>
     * The row will be inserted with current values of column variables, set by a row selection<br>
     * or set by application, by a specific logic method.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * <p>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * The bound object value will be stored with the variable value, automatically created by forward, at<br>
     * function declaration time,  with the name declared by DATA_VALUES_BOUND() section. Null is a valid value.
     * <p>
	 * If the number of row is out of range no action will be taken.
	 * <p>
     * @param tableName the Swing table name
	 * @param tableRowIndex the row number where to insert the row, 0-based
	 */
	public ForwardDoParms DO_TABLE_INSERT_ROW(String tableName, int tableRowIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = null;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = true;
		p.action = EnumForwardAction.TABLE_INSERT_ROW;
		return p;
	}

	 /**
     * Insert a row in a swing table object at the row specified.<br>
     * <p>
     * The row will be inserted with current values of column variables, set by a row selection<br>
     * or set by application, by a specific logic method.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * <p>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * The bound object value will be stored with the input value.<br>
     * <p>
	 * If the number of row is out of range no action will be taken.
	 * <p>
     * @param tableName the Swing table name
	 * @param tableRowIndex the row number where to insert the row, 0-based
	 * @param tableRowObjectBound the bound object. null is a valid value.
	 */
	public ForwardDoParms DO_TABLE_INSERT_ROW(String tableName, int tableRowIndex, Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_INSERT_ROW;
		return p;
	}

	/**
     * Insert a row in a swing table object at the row specified.<br>
     * <p>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
     * This constructor lets to specify a bound object for the inserted row.<br>
     * The bound object value will be stored with the input value.<br>
     * <p>
	 * If the number of row is out of range no action will be taken.
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowIndex the row number after to wich to insert the row
	 * @param tableRowObjects the array with row objects
	 * @param tableRowObjectBound the object bound to row where to insert the row, 0-based
	 */
	public ForwardDoParms DO_TABLE_INSERT_ROW(String tableName, int tableRowIndex, Object tableRowObjects[], Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = tableRowObjects;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_INSERT_ROW;
		return p;
	}
	
    /**
     * Delete all rows from a swing table object and all bound objects, if any.<br>
     * Table definition will be still active and it will be possible insert rows again.<br>
     * <p>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
     * @param componentName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_DELETE_ALL_ROWS(String componentName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowIndexes = null;
		p.action = EnumForwardAction.TABLE_DELETE_ALL_ROWS;
		return p;
	}

	
    /**
     * Delete rows from a swing table object and all bound objects, if any.<br>
     * <p>
     * If some row number is out of range, no action will be taken.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
     * @param tableName the Swing table name
     * @param tableRowIndexes the rows indexes, 0-based, to delete
	 */
	public ForwardDoParms DO_TABLE_DELETE_ROWS(String tableName, int ... tableRowIndexes) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowIndexes = tableRowIndexes;
		p.action = EnumForwardAction.TABLE_DELETE_ROWS;
		return p;
	}

	
    /**
     * Delete all currently selected rows from a swing table object and all bound objects too, if any.<br>
     * <p>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
     * @param tableName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_DELETE_SELECTED_ROWS(String tableName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.action = EnumForwardAction.TABLE_DELETE_SELECTED_ROWS;
		return p;
	}

 
	 /**
     * Update the row of a swing table object and the bound object too.<br>
     * <p>
     * The specified row will be updated with current column variable values that can be updated<br>
     * both automatically at row selection or by any application resable code.<br>
     * If the row number is out of range no action will be taken.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowIndex the row number after to wich to insert the row
	 * @param tableObjectBoundToUpdate as true to update the row object bound with the current variable
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW(String tableName, int tableRowIndex, boolean tableObjectBoundToUpdate) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = null;
		p.tableObjectBoundToUpdate = tableObjectBoundToUpdate;
		p.tableObjectBoundFromVar = true;
		p.action = EnumForwardAction.TABLE_UPDATE_ROW;
		return p;
	}

	 /**
     * Update the row of a swing table object and the bound objects too.<br>
     * <p>
     * The specified row will be updated with current column variable values that can be updated<br>
     * both automatically at row selection or by any application resable code.<br>
     * If the row number is out of range no action will be taken.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowIndex the row number after to wich to insert the row
	 * @param tableRowObjectBound the object bound. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW(String tableName, int tableRowIndex, Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = null;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_UPDATE_ROW;
		return p;
	}

	 /**
     * Update the row of a swing table object and the bound object too.<br>
     * <p>
     * All row objects must be specified in the same order and of the same type as defined at the function declaring time.<br>
     * If the row number is out of range no action will be taken.<br>
     * <br>
     * Any row can be bound with an application dependent object, like a database key,<br>
     * a text or even a structured object.<br>
     * This bound object, if declared, will be deleted just only due the row deleting or the clear of table.<br>
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowObjects the array with row objects
	 * @param tableRowIndex the row number after to wich to insert the row
	 * @param tableRowObjectBound the object bound. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW(String tableName, int tableRowIndex, Object tableRowObjects[], Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = tableRowObjects;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_UPDATE_ROW;
		return p;
	}

	
    /**
     * Update all selected rows of a swing table object.<br>
     * <p>
     * Selected rows will be updated with current column variable values, that can be updated<br>
     * both automatically at row selection or by any application reusable code.<br>
       * <br>
     * When more then one rows is selected, the same values are updated in all rows.<br> 
     * No update will be done to the row object bound, if any.<br>
	 * <p>
	 * @param tableName the Swing table name
	 */
	public ForwardDoParms DO_TABLE_UPDATE_SELECTED_ROWS(String tableName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjectBound = null;
		p.tableObjectBoundToUpdate = false;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_UPDATE_SELECTED_ROWS;
		return p;
	}

    /**
     * Update all selected rows of a swing table object.<br>
     * <p>
     * Selected rows will be updated with current column variable values, that can be updated<br>
     * both automatically at row selection or by any application resable code.<br>
     * <br>
     * When more then one rows is selected, the same values are updated in all rows.<br> 
     * The object bound to any row updated will be set to the input value.<br>
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowObjectBound the object bound. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_UPDATE_SELECTED_ROWS(String tableName, Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_UPDATE_SELECTED_ROWS;
		return p;
	}


	  /**
     * Update all selected rows of a swing table object.<br>
     * <p>
     * Selected rows will be updated with input values.<br>
     * <br>
     * When more then one rows is selected, the same values are updated in all rows.<br> 
     * The object bound to any row updated will be set to the input value.<br>
	 * <p>
	 * @param tableName the Swing table name
	 * @param tableRowObjects the array with row objects
	 * @param tableRowObjectBound the object bound. null is a valid value
	 */
	public ForwardDoParms DO_TABLE_UPDATE_SELECTED_ROWS(String tableName, Object tableRowObjects[], Object tableRowObjectBound) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableRowObjects = tableRowObjects;
		p.tableRowObjectBound = tableRowObjectBound;
		p.tableObjectBoundToUpdate = true;
		p.tableObjectBoundFromVar = false;
		p.action = EnumForwardAction.TABLE_UPDATE_SELECTED_ROWS;
		return p;
	}


	
    /**
     * Update the cell of a swing table object identified by row and column.<br>
     * <p>
     * The column is specified by its index, 0-based.<br>
     * If the row number or the column number is out of range no action will be taken.<br>
     * <p>
	 * @param tableName the Swing table name
	 * @param tableCellObject the object to update the table cell
	 * @param tableRowIndex the row number 0-based
	 * @param tableColIndex the col number 0-based
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW_COLUMN_CELL(String tableName, Object tableCellObject, int tableRowIndex, int tableColIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableCellObject = tableCellObject;
		p.tableRowIndex = tableRowIndex;
		p.tableColIndex = tableColIndex;
		p.tableColumnName = "";
		p.action = EnumForwardAction.TABLE_UPDATE_ROW_COLUMN_CELL;
		return p;
	}

    /**
     * Update the cell of a swing table object identified by row and column.<br>
     * <p>
     * The column is specified by its own name, as declared by function.
     * If the row number is out of range or the column name has be not defined, no action will be taken.<br>
     * <p>
	 * @param tableName the Swing table name
	 * @param tableCellObject the object to update the table cell
	 * @param tableRowIndex the row number 0-based
	 * @param tableColumnName the column name as declared by function
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW_COLUMN_CELL(String tableName, Object tableCellObject, int tableRowIndex, String tableColumnName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = tableName;
		p.tableCellObject = tableCellObject;
		p.tableRowIndex = tableRowIndex;
		p.tableColIndex = -1;
		p.tableColumnName = tableColumnName;
		p.action = EnumForwardAction.TABLE_UPDATE_ROW_COLUMN_CELL;
		return p;
	}
    /**
     * Update the object bound to a row of a swing table object.<br>
     * <p>
     * If the row number is out of range no action will be taken.<br>
     * <p>
	 * @param componentName the Swing table name
	 * @param tableRowIndex the row number 0-based
	 */
	public ForwardDoParms DO_TABLE_UPDATE_ROW_BOUND_OBJECT(String componentName, Object tableRowObjectBound, int tableRowIndex) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.componentName = componentName;
		p.tableRowIndex = tableRowIndex;
		p.tableRowObjectBound = tableRowObjectBound;
		p.action = EnumForwardAction.TABLE_UPDATE_ROW_BOUND_OBJECT;
		return p;
	}

	/**
	 * Load a new function for next starting.<br>
	 * <p>
	 * The input function will be loaded and the method <code>declare</code> will be executed.<br>
	 * The new function loaded is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded and then its possible set required parameters <br>
	 * and do any required behaviour change, calling all function available methods, before the function starting.<br>
	 * <p>
	 * @param functionNameToLoad the function name to be loaded
	 */
	public ForwardDoParms DO_FUNCTION_LOAD(String functionNameToLoad) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionNameToLoad = functionNameToLoad;
		p.action = EnumForwardAction.FUNCTION_LOAD;
		return p;
	}

	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The caller function will be disabled but will be always visible behind the started function.
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * The caller function will be disabled but still visible.<br>
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * The function will be started as a Dialog in a JDialog frame, center parent, decorated, with caller function visible and disabled.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos = EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = true;
		p.functionOnJFrame = false;
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}

	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The caller function will be disabled but will be always visible behind the started function.
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * The caller function will be disabled but still visible.<br>
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * The function will be started as a Dialog in a JDialog frame or in a JFrame, center parent, decorated, with caller function visible and disabled.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 * @param functionOnJFrame as true to start the function on a JFrame, false on a JDialog
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId, boolean functionOnJFrame, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos = EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = true;
		if (!functionOnJFrame) {p.functionOnJDialog = false;}
		p.functionOnJFrame = functionOnJFrame;
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}


	
	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 * @param functionCurEnabled as true to disable all panels of the current caller function
	 * @param functionCurVisible as true to hidden all panels of the current caller function
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId, boolean functionCurEnabled, boolean functionCurVisible) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = functionCurEnabled;
		p.functionCurVisible = functionCurVisible;
		p.functionPos = EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}

	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 * @param functionCurEnabled as true to disable all panels of the current caller function
	 * @param functionCurVisible as true to hidden all panels of the current caller function
	 * @param functionUndecorated as true for a function in undecorated frame
	 * @param functionPos as EnumForwardOption.DIALOG_ON_CENTER_PARENT or EnumForwardOption.DIALOG_ON_CENTER_SCREEN constants
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId, boolean functionCurEnabled, boolean functionCurVisible, boolean functionUndecorated, EnumForwardOption functionPos) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = functionCurEnabled;
		p.functionCurVisible = functionCurVisible;
		p.functionPos = functionPos;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = functionUndecorated;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}

	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * The current function caller will be disabled but still visible,<br>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 * @param functionUndecorated as true for a function in undecorated frame
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId, boolean functionUndecorated) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos =  EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = functionUndecorated;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}

	/**
	 * Starts a new function.<br>
	 * <p>
	 * The context of the current function will be stacked and the new function will be started.<br>
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * When a standard automatic lookup function is started by forward monitor, by a double click on a panel GUI control,<br>
	 * the caller function identifier is automatically set to the name of the GUI control.<br>
	 * Thereferore any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same identification value.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 * @param functionCurEnabled as true to disable all panels of the current caller function
	 * @param functionCurVisible as true to hidden all panels of the current caller function
	 * @param functionUndecorated as true for a function in undecorated frame
	 * @param functionPosX as the X position, in pixel, relative to the function frame owner
	 */
	public ForwardDoParms DO_FUNCTION_START(String functionNameToStart, String functionId, boolean functionCurEnabled, boolean functionCurVisible, boolean functionUndecorated, int functionPosX, int functionPosY) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = functionCurEnabled;
		p.functionCurVisible = functionCurVisible;
		p.functionPos = EnumForwardOption.DIALOG_AT_X_Y;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START;
		return p;
	}

    /**
	 * Starts the standard lookUp function to select values from a forward table<br>
	 * <p>
	 * There will be started a function and displayed a dialog with table elements and the user can select them<br>
	 * <br>
	 * The current function caller will be disabled but still visible,<br>
	 * <p>
	 * Notice that any function application logic to be executed when the called function gives back the control, <br>
	 * under <code>ON_FUNCTION_RETURN</code> event condition, must specify the same function identification value.<br>
	 * <p>
	 * @param lookupTableNum the table num
	 * @param functionId the function identifier to be used on return from function to properly detect the actions to execute
	 */
	public ForwardDoParms DO_FUNCTION_START_LOOKUP_TABLE(int lookupTableNum, String functionId) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = functionId;
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionLookupTableNum = lookupTableNum;				 
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START_LOOKUP_TABLE;
		return p;
	}

	/**
	 * Transfers the control to a new function unconditionally.<br>
	 * <p>
	 * The context of the current function will be replaced by the new function started.<br>
	 * The new function started is completely independent from the caller, with own descriptors and function variables<br>
	 * but shares both session and application variables in the same scope.<br> 
	 * <p>
	 * The current function caller will be disabled but still visible.<br>
	 * <p>
	 * Any function can declare a set of input parameters, seen by function simply as local variables.<br>
	 * When a function starts another function that declare input parameters, it must before set all<br>
	 * variable parameters to the desired value.<br>
	 * <p>
	 * For this reason the function to start must before be loaded by means of <code>DO_LOAD_FUNCTION()</code> action.<br>
	 * After the <code>DO_LOAD_FUNCTION()</code> action its possible to set all of required parameters and to do <br>
	 * any required behaviour change calling all function available methods.<br>
	 * <p>
	 * @param functionNameToStart the function name previously loaded to be started
	 * @param functionUndecorated as true for a function in undecorated frame
	 */
	public ForwardDoParms DO_FUNCTION_XCTL(String functionNameToStart, boolean functionUndecorated) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionId = "";
		p.functionNameToStart = functionNameToStart;
		p.functionCurEnabled = false;
		p.functionCurVisible = false;
		p.functionPos =  EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = functionUndecorated;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_XCTL;
		return p;
	}


   /**
	 *  Starts the standard forward function that shows informations on last logical data view executed<br>
	 * <p>
	 * The current function caller will be disabled but still visible.<br>
     *
	 */
	public ForwardDoParms DO_FUNCTION_START_SHOW_LDV_ERROR() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos =  EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START_SHOW_LDV_ERROR;
		return p;
	}
	
	/**
	 * Starts the standard forward function that shows informations on occurred exception<br>
	 * <p>
	 * The current function caller will be disabled but still visible.<br>
	 * <p>
	 * 
	 */
	public ForwardDoParms DO_FUNCTION_START_SHOW_EXCEPTION_ERROR() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos =  EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
	p.action = EnumForwardAction.FUNCTION_START_SHOW_EXCEPTION_ERROR;
		return p;
	}

	 /**
	 * the standard forward function that shows messages and/or errors<br>
	 * <p>
	 * The current function caller will be disabled but still visible.<br>
	 */
	public ForwardDoParms DO_FUNCTION_START_SHOW_MESSAGES() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionCurEnabled = false;
		p.functionCurVisible = true;
		p.functionPos =  EnumForwardOption.DIALOG_ON_CENTER_PARENT;
		p.functionPosX = 0;
		p.functionPosY = 0;
		p.functionUndecorated = false;
		p.functionModal = true;
		p.functionOnJDialog = false;
		p.functionOnJFrame = true;		
		p.action = EnumForwardAction.FUNCTION_START_SHOW_MESSAGES;
		return p;
	}
		
	/**
	  * Gets the current function language and sets a function variable declared by a VAR() statement with value.<br>
	  * <p>
	  * It will be set the variable of function with the current {@link EnumLanguage} object.<br>
	  * If the variable name is not defined, no action will be taken.
	  * The language is defined by function at declaring time.<br>
	  * The user login can then set and override a different language related to the user.<br>
	  * <P>
	 * @param functionName the function name previously loaded or an empty string, for the current function
	 * @param varName the variable name to be updated with the language value
	 */
	public ForwardDoParms DO_FUNCTION_GET_LANGUAGE(String varName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionVarName = varName;
		p.action = EnumForwardAction.FUNCTION_GET_LANGUAGE;
		return p;
	}



	/**
	  * Sets a function variable declared by a VAR() statement with an embedded object value.<br>
	  * <p>
	  * It will be set the variable of the input function specified if it's not an empty string<br>
	  * otherwise of the currently active function.<br>
	  * If the function has been not previously loaded, no action will be taken.<br>
	  * If the variable name is not defined no action will be taken.<br>
	  * if the object value is of a wrong type no action will be taken.<br>
	  * <P>
	 * @param functionName the function name previously loaded or an empty string, for the current function
	 * @param varName the variable name to be updated
	 * @param varValue the new value object for the variable
	 * @param dummy to make the method unique
	 */
	public ForwardDoParms DO_FUNCTION_SET_VAR(String functionName, String varName, Object varValue, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = functionName;
		p.functionVarName = varName;
		p.functionVarValue = varValue;
		p.functionGuiControlName = "";
		p.action = EnumForwardAction.FUNCTION_SET_VAR;
		return p;
	}

     /**
     * Sets a function variable declared by a VAR() statement with the value of a GUI control.<br>
     * <p>
 	 * It will be set the variable of the input function specified if it's not an empty string<br>
	 * otherwise of the currently active function.<br>
     * If the GUI control is not defined no action will be taken.<br>
     * if the object value is of a wrong type no action will be taken.<br>
     * <P>
	 * @param functionName the function name previously loaded or an empty string, for the current function
	 * @param varName the variable name to be updated
	 * @param guiControlName the GUI control to use to set the variable
	 */
	public ForwardDoParms DO_FUNCTION_SET_VAR(String functionName, String varName, String guiControlName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = functionName;
		p.functionVarName = varName;
		p.functionVarValue = null;
		p.functionGuiControlName = guiControlName;
		p.action = EnumForwardAction.FUNCTION_SET_VAR;
		return p;
	}
	
    /**
     * Sets, in the function to be started, a function parameter declared by a PARM_REQUIRED() statement with an embedded object value.<br>
     * <p>
     * The function to be started must be already loaded by means of <code>FUNCTION_LOAD</code> directive.<br>
     * <p>
      * if the object value is of a wrong type no action will be taken.<br>
     * <P>
	 * @param functionName the function name previously loaded
	 * @param parmValue the new value object for the parameter in the function loaded to be started
	 * @param functionParmNameInCalled the parameter input name to be updated in the function loaded to be started
	 * @param dummy to make the method unique
	 */
	public ForwardDoParms DO_FUNCTION_SET_PARM_REQUIRED(String functionName, Object parmValue, String functionParmNameInCalled, Object dummy) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = functionName;
		p.functionParmNameInCalled = functionParmNameInCalled;
		p.functionParmValue = parmValue;
		p.functionParmNameInCaller = "";
		p.action = EnumForwardAction.FUNCTION_SET_PARM_REQUIRED;
		return p;
	}

	/**
     * Sets, in the function to be started, a function parameter declared by a PARM_REQUIRED() statement with the value of a GUI control or a variable of the calling function.<br>
     * <p>
     * The function to be started must be already loaded by means of <code>FUNCTION_LOAD</code> directive.<br>
     * <p>
     * If the function has beeen not loaded or the parameter name is not defined no action will be taken.<br>
     * If the GUI control or a variable with the same name is not defined no action will be taken.<br>
     * if the object value is of a wrong type no action will be taken.<br>
     * <P>
	 * @param functionName the function name previously loaded
	 * @param functionParmNameInCaller the name of a GUI control or a variable, to use as input to set the parameter in the function loaded to be started
	 * @param functionParmNameInCalled the parameter input name to be updated in the function loaded to be started
	 */
	public ForwardDoParms DO_FUNCTION_SET_PARM_REQUIRED(String functionName, String functionParmNameInCaller, String functionParmNameInCalled) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = functionName;
		p.functionParmNameInCalled = functionParmNameInCalled;
		p.functionParmValue = null;
		p.functionParmNameInCaller = functionParmNameInCaller;
		p.action = EnumForwardAction.FUNCTION_SET_PARM_REQUIRED;
		return p;
	}

	/**
	  * Sets a function variable declared by a VAR() statement with a variable of a called function.<br>
	  * <p>
	  * This action will be effective on return of a called function event if the the called function has data to be returned.<br>
	  * <p>
	  * It will be set the variable of the current function with the content of the variable returned by the called function<br>
	  * If the function has been not previously loaded, no action will be taken.<br>
	  * If the functionParmNameInCaller is not defined no action will be taken.<br>
	  * if the functionParmNameInCalled is not defined in the called function no action will be taken.<br>
	  * if the functionParmNameInCaller and functionParmNameInCalled are not of the same type, no action will be taken.<br>
	  * <P>
	 * @param functionName the function name previously loaded 
	 * @param functionParmNameInCaller the variable name to be updated in the caller function
	 * @param functionParmNameInCalled the parameter name exposed by called function to be used as input to update functionVarNameInCaller, in the caller function
	 */
	public ForwardDoParms DO_FUNCTION_SET_PARM_RETURNED(String functionName, String functionParmNameInCaller, String functionParmNameInCalled) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = functionName;
		p.functionParmNameInCalled = functionParmNameInCalled;
		p.functionParmNameInCaller = functionParmNameInCaller;
		p.functionParmValue = null;
		p.action = EnumForwardAction.FUNCTION_SET_PARM_RETURNED;
		return p;
	}


	/**
	  * Sets a all function variables declared by a PARMS_RETURNED() statement to the default value.<br>
	  * <p>
	  * It the function doesn't return any parameter, no action will be taken.<br>
	  * <P>
	 */
	public ForwardDoParms DO_FUNCTION_SET_PARMS_RETURNED_INITIAL() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionName = "";
		p.functionParmNameInCalled = "";
		p.functionParmNameInCaller = "";
		p.functionParmValue = null;
		p.action = EnumForwardAction.FUNCTION_SET_PARMS_RETURNED_INITIAL;
		return p;
	}




    /**
     * Sets a GUI control with an embedded object value.<br>
     * <p>
     * If the control name is not defined no action will be taken.<br>
     * if the object value is of a wrong type no action will be taken.<br>
     * <P>
	 * @param guiControlName the GUI control to use to set the variable
	 * @param guiValue the value to be used to update the GUI control
	 * @param dummyParm as a dummy parameter to make the method unique
	 */
	public ForwardDoParms DO_FUNCTION_SET_GUI_CONTROL(String guiControlName, Object guiValue, Object dummyParm) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionVarName = "";
		p.functionGuiValue = guiValue;
		p.functionGuiControlName = guiControlName;
		p.action = EnumForwardAction.FUNCTION_SET_GUI_CONTROL;
		return p;
	}

    /**
     *Sets a GUI control with the value of a function variable eclared by VAR().<br>
     * <p>
     * If the GUI control or the variable is not defined no action will be taken.<br>
     * if the variable is of a wrong type no action will be taken.<br>
     * <P>
	 * @param guiControlName the GUI control to use to set the variable
	 * @param varName the variable name to be updated
	 */
	public ForwardDoParms DO_FUNCTION_SET_GUI_CONTROL(String guiControlName, String varName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionVarName = varName;
		p.functionVarValue = null;
		p.functionGuiControlName = guiControlName;
		p.action = EnumForwardAction.FUNCTION_SET_GUI_CONTROL;
		return p;
	}

	
	
    /**
     * Exit from current function and returns the control to operating system<br>
     * <p>
	 * @param functionReturnCode the RC exit
	 */
	public ForwardDoParms DO_FUNCTION_QUIT(int functionReturnCode) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionReturnCode = functionReturnCode;
		p.action = EnumForwardAction.FUNCTION_QUIT;
		return p;
	}

	
    /**
     * Return from current function to the function caller<br>
     * <p>
     * @param functionReleaseOnReturn as true to release all objects and resources, false if not
	 */
	public ForwardDoParms DO_FUNCTION_RETURN(boolean functionReleaseOnReturn) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.functionReleaseOnReturn = functionReleaseOnReturn;
		p.action = EnumForwardAction.FUNCTION_RETURN;
		return p;
	}

	
    /**
     *System beep sound<br>
     * <p>
	 */
	public ForwardDoParms DO_SYSTEM_BEEP() {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.action = EnumForwardAction.SYSTEM_BEEP;
		return p;
	}

	
    /**
     * Starts a timer<br>
     * The timer mus be declared by function.<br>
     * If the time has be not defined, no action will be taken.<br>
     * 
     * <p>
     * @param systemTimerName the object timer to start
	 */
	public ForwardDoParms DO_SYSTEM_TIMER_START(String systemTimerName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.systemTimerName = systemTimerName;
		p.action = EnumForwardAction.SYSTEM_TIMER_START;
		return p;
	}

		
    /**
     * Starts a timer<br>
     * The timer mus be declared by function.<br>
     * If the time has be not defined, no action will be taken.<br>
     * 
     * <p>
     * @param systemTimerName the object timer to start
	 */
	public ForwardDoParms DO_SYSTEM_TIMER_STOP(String systemTimerName) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.systemTimerName = systemTimerName;
		p.action = EnumForwardAction.SYSTEM_TIMER_STOP;
		return p;
	}

	
    /**
     * Starts a timer<br>
     * The timer mus be declared by function.<br>
     * If the time has be not defined, no action will be taken.<br>
     * 
     * <p>
     * @param systemTimerName the object timer to start
     * @param systemTimerDelayInitial the time in ms to delay before starting
     * @param systemTimerDelay the time in ms to delay 
	 */
	public ForwardDoParms DO_SYSTEM_TIMER_DELAY(String systemTimerName, int systemTimerDelayInitial, int systemTimerDelay) {
		ForwardDoParms p = null;
		p = new ForwardDoParms();
		p.systemTimerName = systemTimerName;
		p.systemTimerDelayInitial = systemTimerDelayInitial;
		p.systemTimerDelay = systemTimerDelay;
		p.action = EnumForwardAction.SYSTEM_TIMER_DELAY;
		return p;
	}


	
	/////////////////////////////////////////////////////////////////////////////////////////////
	///// Classi interne di servizio
	////////////////////////////////////////////////////////////////////////////////////////////
	
	/*
	 * Descrittore singolo evento e relativa azioni da intraprendere.
	 */
	protected class InnerOnEvent{
		
		// In caso di eventi di finestra i nomi pannello e controllo non sono significativi
		String componentName = null;						// Nome componente (campo, pulsante, idOperazione etc.)su cui intercettare l'evento
		EnumForwardEvent event = null;     		 			// Evento da intercettare
		ArrayList<ForwardDoParms> al_actionCoded = null;  	// Azioni codificate da ForwardDoParms e sue estensioni
		
		/*Obsoleto*/ EnumForwardAction action = null;      	// Azione da intraprendere
		
		
		 /* Costruttore */
		 private InnerOnEvent() {
			 event = EnumForwardEvent.NOT_ASSIGNED;	 
			 action = EnumForwardAction.NOT_ASSIGNED;
			 al_actionCoded = new ArrayList<ForwardDoParms> ();
		}
	
	
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return "Event="+event+",ComponentName="+componentName+",Action="+al_actionCoded.get(0).action.toString();
		}
	}

	/*
	 * Descrittore singolo gruppo di actions da intraprendere.
	 */
	protected class InnerActionsGroup{
		
		String groupName = "";									// Identificativo gruppo di actions
		ArrayList<ForwardDoParms> al_actionCoded = null;  		// Azioni codificate da ForwardDoParms e sue estensioni
		
		 /* Costruttore */
		private InnerActionsGroup() {
			 al_actionCoded = new ArrayList<ForwardDoParms> ();
		}
	
	
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return "GroupName="+groupName+",Actions="+al_actionCoded.get(0).action.toString();
		}
	}

	 
	/*
	 * Descrittore tabella implementata da JTable e cablata in un pannello PANEL_GRID.
	*/
	@SuppressWarnings("unused")
	private class InnerTable{
		
		String tableName = "";				// Nome tabella
		String toolTipText = "";			// ToolTip specifico tabella
	 }
		 
	/*
	 * Descrittore colonna tabella implementata da JTable e cablata in un pannello PANEL_GRID.
	*/
	@SuppressWarnings("unused")
	private class InnerTableColumn{
		String columnHeader = "";			// Text header colonna
		String toolTipText = "";			// ToolTip specifico colonna
		String columnName = "";				// Nome colonna, coincide con il nome variabile utilizzato
		String columnNameLdv = "";			// Nome colonna logical data view di accesso dati
		Class<?> renderType = null;			// Classe oggetto di rendering
	 }
	
	/* -----------------------
	 * Descrittore componente.
	 * -----------------------
	 * 
	 * Per ogni componente (oggetto Swing) viene istanziato un oggetto di questa classe.
	 * E' possibile dichiarare delle proprietà specifiche per ogni componente
	 * e pertanto è necessario avere disponibilità dell'oggetto JComponent.
	 * Utilizzato per descrivere ogni informazione associata al componente.
	 *  Oggetto JComponent (JPanel, JButton, ...)
	 *  Data source
	 *   Logical data view
	 *   Colonne ldv da utilizzare a coppie (nome colonna ldv, nome interno da utilizzare o "" se lo stesso)
	 *  Proprietà swing
	 *  
	*/
	protected class InnerComponent{
		EnumForwardComponent componentType = null;						// Tipo componente swing codificato, non swing oppure variabile applicativa
		
		// Info per componente swing
		public Object component = null;                           		// JPanel, JButton, JTextField, JTable, .... Timer
		public Class<?> componentObjectClass = null;                    // String, Integer, Double, Float, Boolean, Date, Color
		public JScrollPane jscrollPane = null;                         	// Scroll pane in cui il component deve essere visualizzato
		public String componentName = "";								// Nome componente
		public String groupName = "";									// Nome gruppo per JMenuItemRadioButton,..
		public String formName = "";                                   	// Nome form in cui il 
		public String panelName = "";                                  	// Nome pannello in cui il componente è disposto
		public String menuName = "";                                   	// Nome menu se il pannello è di tipo menu o se il componente appartiene al menu
		public String popUpMenuName = "";                              	// Nome PopUp menu da attivare per il componente (Un panel o un component swing)
		public String ldvName = "";                        				// Vista logica di accesso ai dati per Panel, ComboBox, List, Table, Tree
		public ArrayList<String> al_columnBound = null;               	// Ogni elemento stringa contiene il nomeJava e il nome della colonna bound della logical data view 
		public ArrayList<InnerComponentProperty> al_property = null;	// Proprietà dichiarate per il componente
		
		// Info dinamiche di esecuzione, in base al tipo di controllo
		public boolean isPaneFirstShowDone = false;                		// True indica panel visibile
		
		// Info per variabile applicativa definita con VAR() o definite implicitamente per ogni colonna di JTable
		public EnumForwardScope varScope = null;						// Scope variabile
		public String varName = "";										// Nome variabile
		public Class<?> varType = null;									// Tipo variabile (Integer, String, Boolean, Double, Float, Date, Color, ..)
		public Object varInitial = null;     							// Valore iniziale variabile, contiene un oggetto di tipo varType
		public Object varObject = null;     							// Oggetto di tipo varType
		
		/* Costruttore */
		private InnerComponent() {
			varScope = EnumForwardScope.SCOPE_FUNCTION;
			al_columnBound = new ArrayList<String> ();
			al_property = new ArrayList<InnerComponentProperty> ();
		}
	
	}
	
	/*
	 * Descrittore proprietà componenti.
	 * Utilizzato per descrivere le proprietà di default di un tipo componente
	 * Utilizzato per descrivere le proprietà di uno specifico componente
	*/
	protected class InnerComponentProperty{
		EnumForwardComponent componentType = null;			// Tipo componente swuing
		String componentName = "";							// Nome componente
		String propertyName = "";							// Nome proprietà (Es. Size o Width)
		Object propertyValue = null;						// Oggetto valore dipendente dal tipo proprità
		@SuppressWarnings("rawtypes")
		Class propertyValueClass = null;					// Classe valore proprietà
	 }

	/*
	 * Descrittore per funzioni chiamate.
	*/
	protected class InnerFunctionsCallable{
		ArrayList<String> al_parmRequired = null;					// Parametri richiesti dal chiamante (nome variabili)
		ArrayList<String> al_parmReturned = null;					// Parametri restituiti al chiamante (nome variabili)
		
		private InnerFunctionsCallable() {
			this.al_parmRequired = new ArrayList<String> ();
			this.al_parmReturned = new ArrayList<String> ();
		}
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Function="+functionName+",Options="+al_functionOption.toString();
	}
	 
	

	
	
}
