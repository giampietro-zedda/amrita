package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultListModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;

import enums.EnumForwardBorderType;
import enums.EnumForwardInputFieldType;
import enums.EnumForwardComponent;
import enums.EnumForwardOption;

/**
 * copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardPanelComponent
 * </h1>
 * <p>
 * Questa classe descrive il singolo oggetto del pannello (dal punto di vista applicativo un campo,<br>
 * indipendentemente dal suo rendering grafico) e le sue caratteristiche di controllo, arricchite da<br>
 * informazioni a livello di layout.<br>
 * L'oggetto può essere relativo a un campo su database o a una label non associata a una sorgente dati.<br>
 * <p>
 * Il tipo di informazioni gestite sono tipicamente applicative e non tecniche o relative agli aspetti <br>
 * di rendering dell'oggetto grafico. Questo tipo di informazioni viene gestito direttamente da e con <br>
 * l'oggetto grafico associato al campo.<br>
 * <p>
 * Per ogni componente sono presenti i metodi specifici significativi di gestione dello stesso, covered di <br>
 * quelli ufficiali, per una gestione applicativa veloce.<br>
 * Si sottolinea che questa classe decrive campi elementari gestiti da pannelli di dettaglio. In questi <br>
 * pannelli non compaiono oggetti grafici ad albero e tabelle. <br>
 * <p>
 * I campi elementari, definiti nel database, sono descritti da oggetti della classe {@link ForwardMetaField},<br>
 * dove viene definito il tipo, la lunghezza, i vincoli, le tabelle di controllo etc e queste <br>
 * informazioni vengono utilizzate a livello del campo nel pannello applicativo.<br>
 * Le informazioni di {@link ForwardMetaField} sono recuperate dal database, dove è presente un completo<br>
 * dizionario applicativo. Tuttavia, nel caso il metacampo non fosse censito nel sistema, ne viene<br>
 * generato uno runtime con le informazioni specifiche della funzione. Se invece il metacampo esiste<br>
 * già nel sistema, si utilizzano le sue informazioni, dopo averle aggiornate con quelle specifiche della <br>
 * funzione applicativa.<br>
 * <br>
 * L'ambiente grafico di riferimento è quello standard Java, ovvero SWING, e pertanto viene assegnato<br>
 * a ogni campo il componente grafico da utilizzare per il rendering.<br>
 * A seconda dell'ambiente di deploy può essere usato il componente SWING o no. Per esempio per applicazioni<br>
 * locali Desktop vengono utilizzati gli oggetti grafici SWING indicati mentre per applicazioni Web su server,<br>
 * la servelet monitor di Forward genererà codice HTML per ottenere le stesse funzionalità.<br>
 * <br>
 * Ogni campo viene popolato attraverso la logical data view del panel che lo contiene, oppure viene imputato<br>
 * dall'utente o può essere il risultato di una trascodifica di un valore da una tabella.<br>
 * Inoltre un campo può essere popolato attraverso l'attivazione di una funzione di selezione.<br>
 * In questo caso sarà il codice applicativo attivato al ritorno dalla funzione a valorizzare il campo.<br>
 * <br>
 * Nel caso siano digitabili dei campi dai quali dipende formalmente la valorizzazione di altri campi,<br>
 * vengono definite le regole di valorizzazione ed è compito del monitor di Forward nei vari ambienti<br>
 * operativi effettuare automaticamete le operazione necessarie alla valorizzazione.<br>
 * <p<
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/

public class ForwardPanelComponent implements Serializable {

	private static final long serialVersionUID = 1L;
	
	// Nome applicativo interno campo e descrittore metacampo se esistente
	private String componentName = "";						// Può non coincidere con il nome del metacampo. E' il nome visto dall'applicazione.
	private ForwardMetaField metaField = null;				// Descrittore completo metacampo se il componente è associato a un campo
    
	// Informazioni grafiche e varie poi riflesso sull'oggetto grafico
	private EnumForwardComponent componentType = null;		// Tipo componente grafico
    private Object graphicObject = null;    				// Oggetto grafico (JComponent) da disporre nel pannello
    private JScrollPane scrollPane = null;					// Oggetto grafico scrollPane effettivo in cui inserire l'oggetto grafico
    private GridBagConstraints constraints = null;    		// Vincoli per inserimento componente con gridBagLayout manager
    private ArrayList<Object> al_objectInfo = null;         // Parametri oggetto non codificati esplicitamente con significati specifici per i vari controlli
    private String iconPath = "";    						// Path icona del tipo images/icon01.gif
    private String iconPathRollover = "";    				// Path icona del tipo images/icon01.gif per button e toggleButton in rollover
    private String iconPathRolloverSelected = "";    		// Path icona del tipo images/icon01.gif per toggleButton in rollover selezionata
    private String iconPathPressed = "";    				// Path icona del tipo images/icon01.gif per button e toggleButton pressed
    private int mnemonicKey = 0;    						// Tasto keyboard mnemonico di attivazione da keyEvent come KeyEvent.VK_ENTER
    private String groupName = "";							// Gruppo di appartenenza se control type = JRadioButton
    private String popUpMenuName = "";						// Nome popUpMenu specifico da attivare sul pressed/released del mouse sul componente
    private EnumForwardBorderType borderType = null;        // Tipo bordo controllo
    private Border borderObject = null;                     // Oggetto border relativo a borderType e valorizzato da BORDER()
    private String borderTitle = "";                    	// Titolo se border titled
    private int fillerHorizontal = 0;						// Per JBoxRigidArea, JBoxHorizontalStrut
    private int fillerVertical = 0;							// Per JBoxRigidArea, JBoxVerticalStrut
    private int row = 0; 									// Posizionamento nel pannello,numero riga 0-based  
    private int col = 0; 									// Posizionamento nel pannello, numero colonna 0-based. E' il numero progressivo di campo nella riga
    
    // Info supplementari al controllo  
	private EnumForwardInputFieldType inputType = null;     // Modalità di imputazione del campo
	private Object defaultValue = null;                     // Valore iniziale di default dichiarato per il (String, Integer, Date in base al tipo di controllo)
	
	// Modelli contenenti i dati
	private ForwardTableModel tableModel = null;   		    // Definisce il modello dati per Swing, usato per inserire e deletare righe nella JTable
	private ForwardTreeModel treeModel = null;   		    // Definisce il modello dati per Swing, usato per gestire JTtree
	private ForwardListModel listModel = null;   		    // Definisce il modello dati per Swing, usato per inserire e deletare righe nella JList

	// INPUT_BY_SPINNER
	// INPUT_BY_LIST
	// INPUT_INDIRECT  
	
    // INPUT_BY_COMBO_BOX
	// Si utilizza una vista logica di accesso dati e si indica la colonna (campo di valorizzazione) di valorizzazione
	private String comboLdv = null;                         // logical Data View da utilizzare per popolare la ComboBox
	private ArrayList<String> al_comboLdvColumn = null;     // Colonne da visualizzare nella ComboBox
	private String comboValueLdvSourceColumn = null;    	// Colonna (nome campo univoco) con cui valorizzare il campo corrente
	
	// INPUT_BY_LOOKUP_TABLE
	// INPUT_BY_LOOKUP_FUNCTION
	// Valorizzazione a fronte di attivazione funzione generica di lookup (accesso rule table o altro)
	// Tale funzione prevede di ricevere e restituire uno o più oggetti (al limite campi, PARM_REQUIRED() e PARM_RETURNED())
	// La valorizzazione dei paeametri in input PARM_REQUIRED() e resituiti PARM_RETURNED() è automatica a cura del monitor
	// Se il PARM_RETURNED() è anche il nome di un controllo GUI, la valorizzazione sarà automatica
	// L'applicazione può dichiarare logiche supplementari su ON_FUNCTION_RETURN eseguite DOPO quelle automatiche 
	private String lookupFunction = ""; 					// Funzione modale di restituzione valori attivata da doubleClick
	private int lookupTableNum = -1; 						// Numero tabella con elenco valori tabella da selezionare
	private ArrayList<String> al_lookupFunctionParmsRequired = null;    // Ogni elemento contiene nome componente in funzione chiamante e nome parametro required in funzione chiamata			
	private ArrayList<String> al_lookupFunctionParmsReturned = null;    // Ogni elemento contiene nome componente in funzione chiamante e nome parametro returned in funzione chiamata			
	

	// Ozioni generali applicative, controlli da effettuare sul componente
	// Le enumerazioni che interessano iniziano per COMPONENT
	private Map<EnumForwardOption, Boolean> hm_componentOptions = null;  
	
	// Informazioni per controlli formali
	private ArrayList<Object> al_ctrlValues = null;        // Valori possibili che il campo pò assumere
	private String ctrlMask = "";                          // Maskera per controllo/formattazione date e numeri
	private int ctrlTableNum = -1;                      	// Numero tabella rule table di controllo
	private String placeHolderCharacter = "";			   // Valore di default nel campo numerico da controllare
	private int ctrlTextSizeMin = 0;                       // Lunghezza minima componente text
	private int ctrlTextSizeMax = 0;                       // Lunghezza massima componente text
	private int ctrlIntSizeMin = 0;                        // Numero minimo cifre intere in componente numerico o text contenente un numero
	private int ctrlIntSizeMax = 0;                        // Numero massimo cifre intere in componente numerico o text contenente un numero
	private int ctrlDecSizeMin = 0;                        // Numero minimo cifre decimali in componente numerico o text contenente un numero
	private int ctrlDecSizeMax = 0;                        // Numero massimo cifre decimali in componente numerico o text contenente un numero
	private int ctrlValueIntMin = 0;                       // Valore minimo parte intera 
	private int ctrlValueIntMax = 0;                       // Valore massimo parte intera
	private int ctrlValueDecMin = 0;                       // Valore minimo parte decimale
	private int ctrlValueDecMax = 0;                       // Valore massimo parte decimale	
	private Object ctrlValueMin = null;                    // Valore minimo  String, Integer, Long, Double (anche per date)
	private Object ctrlValueMax = null;                    // Valore massimo String, Integer, Long, Double (anche per date)
	
	// Informazioni per controllo di esistenza/inesistenza.
	// Logical data view associata al campo.
	// Utilizzata per controllo esistenza in rule table o legacy table, valorizzare altri campi o per rendere disponibili dati all'applicazione.
	// Le chiavi (nome campo/control nella funzione e nome colonna nella ldv) sono specificate da al_execLdvColumnsKey
	// La ldv eseguita restituisce una riga di valori
	// I campi da valorizzare (nome campo/control nella funzione e nome colonna nella ldv) sono specificate da al_execColumnsReturned
	// La valorizzazione delle colonne chiave prima dell'esecuzione e dei campi della funzione dopo l'esecuzione, è automatica
	private String ldv = "";                           	 
	private ArrayList<String> al_ldvColumnsKey = null;  	// Ogni elemento contiene nome componente in funzione chiamante e nome colonna chiave in logical data view			
	private ArrayList<String> al_ldvColumnsReturned = null;	// Ogni elemento contiene nome componente in funzione chiamante e nome colonna in logical data view da usare come input	
	
	
	// Impostato in esecuzione
	private boolean isWithErrors = false;               	// Indica se il campo è  in errore, come impostato dall'applicazione 
	private boolean isWithFormalErrors = false;             // Indica se il campo ha degli errori formali impostati dai controlli automatici
	private boolean isWithExistsRuleTableErrors = false;    // Indica se il campo ha degli errori di esistemza/insesistenza in rule table
	private boolean isWithExistsEntityErrors = false;    	// Indica se il campo ha degli errori di esistemza/insesistenza in entity

	// Descrizioni in lingua correnti
	private String captionLong = ""; 						// Caption label in lingua
    private String captionShort = ""; 					    // Caption colonna table in lingua
    private String toolTip = ""; 						    // Tooltip in lingua
    

    /* Costruttore */
	public ForwardPanelComponent(String componentName) {
		super();
		this.componentName = componentName;
		this.constraints = new GridBagConstraints();
		this.componentType = EnumForwardComponent.NOT_ASSIGNED;
		this.inputType = EnumForwardInputFieldType.NOT_ASSIGNED;
		this.hm_componentOptions = new HashMap<EnumForwardOption, Boolean> ();
		this.al_objectInfo = new ArrayList<Object> ();
		this.al_lookupFunctionParmsRequired = new ArrayList<String> ();	
		this.al_lookupFunctionParmsReturned = new  ArrayList<String> (); 		
		this.al_ldvColumnsKey = new ArrayList<String> ();	
		this.al_ldvColumnsReturned = new  ArrayList<String> (); 	
		this.al_ctrlValues = new ArrayList<Object> ();
	}


	/**
	 * Restituisce il nome del campo.<br>
	 * <p>
	 * Per campo si intende ogni tipo di oggetto disposto nel pannello sia esso JText o JLabel.<br>
	 * <p>
	 * Il nome è da considerarsi interno all'applicazione e può non coincidere con <br>
	 * il nome del metadato associato. In altri termini è il nome con il quale l'oggetto <br>
	 * disposto sul pannello viene referenziato in ogni punto dell'applicazione.<br>
	 * <p>
	 * @return the componentName
	 */
	public String getName() {
		return componentName;
	}


	/**
	 * Imposta il nome del campo.<br>
	 * <p>
	 * Per campo si intende ogni tipo di oggetto disposto nel pannello sia esso JText o JLabel.<br>
	 * <p>
	 * Il nome è da considerarsi interno all'applicazione e può non coincidere con <br>
	 * il nome del metadato associato. In altri termini è il nome con il quale l'oggetto <br>
	 * disposto sul pannello viene referenziato in ogni punto dell'applicazione.<br>
	 * <p>
	 * @param componentName the componentName to set
	 */
	public void setName(String componentName) {
		this.componentName = componentName;
	}

	/**
	 * Sets component preferred width.<br>
	 * <p>
	 * No change will be done to the preferred heigh.<br>
	 * <p>
	 * @param componentName the componentName to set
	 */
	public void setPreferredWidth(int width) {
		JComponent jcomponent = null;
		if (this.graphicObject == null || !(this.graphicObject instanceof JComponent)) {return;}
		
		// Update width senza  modificare hegh
		jcomponent = (JComponent) this.graphicObject ;
		jcomponent.setPreferredSize(new Dimension((width == -1) ? jcomponent.getWidth() : width,  jcomponent.getHeight()));
	}


	/**
	 * Restituisce un oggetto {@link ForwardMetaField} con il metadato associato al campo<br>
	 * <p>
	 * Nel caso di metadato non disponibile, per esempio a fronte di JLabel, restituisce null.<br>
	 * <p>
	 * Il metadato descrive tutte le caratteristiche, indipendenti dalla sopecifica applicazione,<br>
	 * per i controlli formali, di esistenza, l'help etc.<br>
	 * <p>
	 * @return the metaField
	 */
	public ForwardMetaField getMetaField() {
		return metaField;
	}


	/**
	 * Imposta un oggetto {@link ForwardMetaField} con il metadato associato al campo<br>
	 * <p>
	 * Nel caso di metadato non disponibile, per esempio a fronte di JLabel, restituisce null.<br>
	 * <p>
	 * Il metadato descrive tutte le caratteristiche, indipendenti dalla sopecifica applicazione,<br>
	 * per i controlli formali, di esistenza, l'help etc.<br>
	 * <p>
	 * @param metaField the metaField to set
	 */
	public void setMetaField(ForwardMetaField metaField) {
		this.metaField = metaField;
	}

	
	

	/**
	 * Restituisce il tipo di componente grafico {@link JComponent} che il campo<br>
	 * del pannello implementa.<br>
	 * <p>
	 * @return the componentType
	 */
	public EnumForwardComponent getType() {
		return componentType;
	}


	/**
	 * Imposta il tipo di componente grafico {@link JComponent} che il campo<br>
	 * del pannello implementa.<br>
	 * <p>
	 * @param componentType the componentType to set
	 */
	public void setType(EnumForwardComponent componentType) {
		this.componentType = componentType;
	}


	/**
	 * Restituisce l'oggetto grafico che rappresenta il campo sul pannello<br>
	 * <p>
	 * Si può trattare di JLabel, JTextField, JCheckBox etc.<br>
	 * <p>
	 * @param graphicObject the graphicObject to set
	 */
	public Object getGraphicObject() {
		return graphicObject;
	}


	/**
	 * Imposta l'oggetto grafico che rappresenta il campo sul pannello<br>
	 * <p>
	 * Si può trattare di JLabel, JTextField, JCheckBox etc.<br>
	 * <p>
	 * @param graphicObject the graphicObject to set
	 */
	public void setGraphicObject(Object graphicObject) {
		this.graphicObject = graphicObject;
	}


	
	
	/**
	 * Get the {@link JScrollPane} object into which to insert the scrollable java component.<br>
	 * <br>
	 * If the object is not scrollable null is returned.<br>
	 * <p>
	 * 
	 * @return the scrollPane
	 */
	public JScrollPane getScrollPane() {
		return scrollPane;
	}


	/**
	 * Sets the {@link JScrollPane} object into which to insert the scrollable java component.<br>
	 * <br>
	 * If the object is not scrollable null is returned.<br>
	 * <p>
	 * 
	 * @param scrollPane the scrollPane to set
	 */
	public void setScrollPane(JScrollPane scrollPane) {
		this.scrollPane = scrollPane;
	}


	/**
	 * Restituisce l'oggetto {@link GridBagConstraints} con i vincoli utilizzati<br>
	 * per inserire il campo con il layout <code>GRID_BAG_LAYOUT</code><br>
	 * <p>
	 * @return the constraints
	 */
	public GridBagConstraints getConstraints() {
		return constraints;
	}


	/**
	 * Imposta l'oggetto {@link GridBagConstraints} con i vincoli utilizzati<br>
	 * per inserire il campo con il layout <code>GRID_BAG_LAYOUT</code><br>
	 * <p>
	 * @param constraints the constraints to set
	 */
	public void setConstraints(GridBagConstraints constraints) {
		this.constraints = constraints;
	}

	

	/**
	 * Get an array list with all parameter objects specified for the java swing control<br>
	 * by a <code>CONTROL</code>declaration.<br>
	 * <p>
	 * @return the al_objectInfo
	 */
	public ArrayList<Object> getObjectInfo() {
		return al_objectInfo;
	}


	/**
	 * Sets an array list with all parameter objects specified for the java swing control<br>
	 * by a <code>CONTROL</code>declaration.<br>
	 * <p>
	 * @param alObjectInfo the al_objectInfo to set
	 */
	public void setObjectInfo(ArrayList<Object> alObjectInfo) {
		al_objectInfo = alObjectInfo;
	}


	/**
	 * Get the path of the icon for the control.<br>
	 * <p>
	 * It's valid for all controls supporting icons.<br>
	 * <p> 
	 * @return the iconPath
	 */
	public String getIconPath() {
		return iconPath;
	}


	/**
	 * Sets the path of the icon for the control.<br>
	 * <p>
	 * It's valid for all controls supporting icons.<br>
	 * <p> 
	 * @param iconPath the iconPath to set
	 */
	public void setIconPath(String iconPath) {
		this.iconPath = iconPath;
	}

	

	/**
	 * Get the path of the icon at control rollover.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon change at rollover.<br>
	 * <p> 
	 * @return the iconPathRollover
	 */
	public String getIconPathRollover() {
		return iconPathRollover;
	}


	/**
	 * Sets the path of the icon at control rollover.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon change at rollover.<br>
	 * <p> 
	 * @param iconPathRollover the iconPathRollover to set
	 */
	public void setIconPathRollover(String iconPathRollover) {
		this.iconPathRollover = iconPathRollover;
	}


	/**
	 * Get the path of the icon at control rollover selected.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon selected at rollover.<br>
	 * <p> 
	 * @return the iconPathRolloverSelected
	 */
	public String getIconPathRolloverSelected() {
		return iconPathRolloverSelected;
	}


	/**
	 * set the path of the icon at control rollover selected.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon selected at rollover.<br>
	 * <p> 
	 * @param iconPathRolloverSelected the iconPathRolloverSelected to set
	 */
	public void setIconPathRolloverSelected(String iconPathRolloverSelected) {
		this.iconPathRolloverSelected = iconPathRolloverSelected;
	}


	/**
	 * Get the path of the icon at buttom pressed.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon change at pressed.<br>
	 * <p> 
	 * @return the iconPathPressed
	 */
	public String getIconPathPressed() {
		return iconPathPressed;
	}


	/**
	 * Sets the path of the icon at buttom pressed.<br>
	 * <p>
	 * It's valid for all controls supporting icons and icon change at pressed.<br>
	 * <p> 
	 * @param iconPathPressed the iconPathPressed to set
	 */
	public void setIconPathPressed(String iconPathPressed) {
		this.iconPathPressed = iconPathPressed;
	}


	/**
	 * Get the mnemonic keyboard key to use instead of mouse click.<br>
	 * <p>
	 * Available for controls enabled to alternative keyboard use.<br>
	 * <p>
	 * @return the mnemonicKey
	 */
	public int getMnemonicKey() {
		return mnemonicKey;
	}


	/**
	 * Sets the mnemonic keyboard key to use instead of mouse click.<br>
	 * <p>
	 * Available for controls enabled to alternative keyboard use.<br>
	 * <p>
	 * @param mnemonicKey the mnemonicKey to set
	 */
	public void setMnemonicKey(int mnemonicKey) {
		this.mnemonicKey = mnemonicKey;
	}


	/**
	 * Sets the name to group JRadioButton controls.<br>
	 * <p>
	 * @return the groupName
	 */
	public String getGroupName() {
		return groupName;
	}


	/**
	 * Get the name to group JRadioButton controls.<br>
	 * <p>
	 * @param groupName the groupName to set
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	
	/**
	 * Get the popUp menu linked to the panel control.<br>
	 * <p>
	 * @return the popUpMenuName
	 */
	public String getPopUpMenuName() {
		return popUpMenuName;
	}


	/**
	 * Sets the popUp menu linked to the panel control.<br>
	 * <p>
	 * @param popUpMenuName the popUpMenuName to set
	 */
	public void setPopUpMenuName(String popUpMenuName) {
		this.popUpMenuName = popUpMenuName;
	}


	/**
	 * Get the border type as coded by forward.<br>
	 * <p>
	 * @return the borderType
	 */
	public EnumForwardBorderType getBorderType() {
		return borderType;
	}

	/**
	 * Sets the border type as coded by forward.<br>
	 * <p>
	 * @param borderType the borderType to set
	 */
	public void setBorderType(EnumForwardBorderType borderType) {
		this.borderType = borderType;
	}

	
	/**
	 * Gets the java {@link Border} object stored by function BORDER() declaration.<br>
	 * <p> 
	 * @return the borderObject
	 */
	public Border getBorderObject() {
		return borderObject;
	}


	/**
	 * Sets the java {@link Border} object stored by function BORDER() declaration.<br>
	 * <p> 
	 * @param borderObject the borderObject to set
	 */
	public void setBorderObject(Border borderObject) {
		this.borderObject = borderObject;
	}


	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @return the borderTitle
	 */
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Get the border title for titled borders.<br>
	 * <p>
	 * @param borderTitle the borderTitle to set
	 */
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	/**
	 * Get the row number where the control has been laid out on the panel.<br>
	 * <p>
	 * The row number is 0-based.
	 * @return the row
	 */
	public int getRow() {
		return row;
	}


	/**
	 * Sets the row number where the control has been laid out on the panel.<br>
	 * <p>
	 * The row number is 0-based.
	 * @param row the row to set
	 */
	public void setRow(int row) {
		this.row = row;
	}


	/**
	 * Get the size of horizontal filler for BOX layout manager.<br>
	 * <p>
	 * Used by createVerticalStruts(), and createRigidArea()<br>
	 * <p>
	 * @return the fillerHorizontal
	 */
	public int getFillerHorizontal() {
		return fillerHorizontal;
	}


	/**
	 * Sets the size of horizontal filler for BOX layout manager.<br>
	 * <p>
	 * Used by createVerticalStruts(), and createRigidArea()<br>
	 * <p>
	 * @param fillerHorizontal the fillerHorizontal to set
	 */
	public void setFillerHorizontal(int fillerHorizontal) {
		this.fillerHorizontal = fillerHorizontal;
	}


	/**
	 * Get the size of vertical filler for BOX layout manager.<br>
	 * <p>
	 * Used by createVerticalStruts(), and createRigidArea()<br>
	 * <p>
	 * @return the fillerVertical
	 */
	public int getFillerVertical() {
		return fillerVertical;
	}


	/**
	 * Sets the size of vertical filler for BOX layout manager.<br>
	 * <p>
	 * Used by createVerticalStruts(), and createRigidArea()<br>
	 * <p>
	 * @param fillerVertical the fillerVertical to set
	 */
	public void setFillerVertical(int fillerVertical) {
		this.fillerVertical = fillerVertical;
	}


	/**
	 * Get the column number where the control has been laid out on the panel.<br>
	 * <p>
	 * It's the current control number in the row.<br>
	 * The column number is 0-based.
	 * @return the col
	 */
	public int getCol() {
		return col;
	}


	/**
	 * Sets the column number where the control has been laid out on the panel.<br>
	 * <p>
	 * It's the current control number in the row.<br>
	 * The column number is 0-based.
	 * @param col the col to set
	 */
	public void setCol(int col) {
		this.col = col;
	}


	/**
	 * Restituisce in quale modo i dati vengono inputati nel campo<br>
	 * <p>
	 * Le modalità riflettono il tipo di oggetto grafico utilizzato tranne:
	 * <p>
	 * <ul>
	 * <li> INPUT_BY_DB_TABLE<br>
	 * Il campo viene valorizzzato da una tebella del sistema tabelle di forward in modo automatico
	 * </li>
	 * <li> INPUT_BY_DB_LDV<br>
	 * Il campo viene valorizzzato attraverso una logical data view di accesso al db in modo automatico
	 * </li>
	 * <li> INPUT_INDIRECT<br>
	 * Il campo viene valorizzato indirettamente da un altro campo in modo automatico
	 * </li>
	 * <li> INPUT_AUTOMATIC<br>
	 * Il campo viene impostato automaticamente da Forward al valore della variabile con lo stesso nome
	 * </li>
	 * </ul>
	 */
	public EnumForwardInputFieldType getInputType() {
		return inputType;
	}


	/**
	 * Imposta in quale modo i dati vengono inputati nel campo<br>
	 * <p>
	 * Le modalità riflettono il tipo di oggetto grafico utilizzato tranne:
	 * <p>
	 * <ul>
	 * <li> INPUT_BY_DB_TABLE<br>
	 * Il campo viene valorizzzato da una tebella del sistema tabelle di forward in modo automatico
	 * </li>
	 * <li> INPUT_BY_DB_LDV<br>
	 * Il campo viene valorizzzato attraverso una logical data view di accesso al db in modo automatico
	 * </li>
	 * <li> INPUT_INDIRECT<br>
	 * Il campo viene valorizzato indirettamente da un altro campo in modo automatico
	 * </li>
	 * <li> INPUT_AUTOMATIC<br>
	 * Il campo viene impostato automaticamente da Forward al valore della variabile con lo stesso nome
	 * </li>
	 * </ul>
	 * @param input the input to set
	 */
	public void setInputType(EnumForwardInputFieldType inputType) {
		this.inputType = inputType;
	}

	
	
	/**
	 * Get the initial default value as set at function declaring time.<br>
	 * <p>
	 * It will be returned an object of type depending on the control.
	 * <br>
	 * @return the defaultValue
	 */
	public Object getDefaultValue() {
		return defaultValue;
	}


	/**
	 * Sets the initial default value as set at function declaring time.<br>
	 * <p>
	 * It must be set an object of type depending on the control.
	 * <br>
	 * @param defaultValue the defaultValue to set
	 */
	public void setDefaultValue(Object defaultValue) {
		this.defaultValue = defaultValue;
	}


	/**
	 * Get the selection type allowed for the list.<br>
	 * <p>
	 * To code selection type code:
	 * <pre>
	 *  ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
	 *  ListSelectionModel.SINGLE_INTERVAL_SELECTION;
	 *  ListSelectionModel.SINGLE_SELECTION;
     * </pre>
	 * @return the listSelectionModel
	 */
	@SuppressWarnings("rawtypes")
	public int getListSelectionMode() {
		JList jlist = null;
		jlist = (JList) this.graphicObject;
		return jlist.getSelectionModel().getSelectionMode();
	}

	
	/**
	 * Sets the selection type allowed for the list.<br>
	 * <p>
	 * To code selection type use <code>ListSelectionModel</code> constants:
	 * <pre>
	 *  ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
	 *  ListSelectionModel.SINGLE_INTERVAL_SELECTION;
	 *  ListSelectionModel.SINGLE_SELECTION;
     * </pre>
	 * @param listSelectionMode the listSelectionMode to set
	 */
	@SuppressWarnings("rawtypes")
	public void setListSelectionMode(int listSelectionMode) {
		JList jlist = null;
		jlist = (JList) this.graphicObject;
		jlist.getSelectionModel().setSelectionMode(listSelectionMode);
	}


	/**
	 * Get the Orientation for the JList object.<br>
	 * <p>
	 * To code orientation type use {@link JList} constants:
	 * <pre>
	 *  JList.VERTICAL 
	 *  JList.HORIZONTAL
     * </pre>
     * @return the listLayoutOrientation
	 */
	@SuppressWarnings("rawtypes")
	public int getListLayoutOrientation() {
		JList jlist = null;
		jlist = (JList) this.graphicObject;
		return jlist.getLayoutOrientation();
	}


	/**
	 * Sets the Orientation for the JList object.<br>
	 * <p>
	 * To code orientation type use {@link JList} constants:
	 * <pre>
	 *  JList.VERTICAL 
	 *  JList.HORIZONTAL
     * </pre>
	 * @param listLayoutOrientation the listLayoutOrientation to set
	 */
	@SuppressWarnings("rawtypes")
	public void setListLayoutOrientation(int listLayoutOrientation) {
		JList jlist = null;
		jlist = (JList) this.graphicObject;
		jlist.setLayoutOrientation(listLayoutOrientation);
	}


	/**
	 * Get the {@link DefaultListModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of list rows.<br>
	 * <p>
	 * @return the listModel
	 */
	public ForwardListModel getListModel() {
		return listModel;
	}


	/**
	 * Sets the {@link DefaultListModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of list rows.<br>
	 * <p>
	 * @param listModel the listModel to set
	 */
	public void setListModel(ForwardListModel listModel) {
		this.listModel = listModel;
	}

	/**
	 * Get the {@link ForwardTableModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of table rows with additional features.<br>
	 * <p>
	 * @return the tableModel
	 */
	public ForwardTableModel getTableModel() {
		return this.tableModel;
	}


	/**
	 * Sets the {@link ForwardTableModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of table rows with additional features.<br>
	 * <p>
	 * @param tableModel the tableModel to set
	 */
	public void setTableModel(ForwardTableModel tableModel) {
		this.tableModel = tableModel;
	}

	/**
	 * Get the rowHeight table properties.<br>
	 * <p>
	 * That's a cover of <code>getHeight()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param rowHeight in pixel
	 */
	public int getTableRowHeight() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getRowHeight();
	}


	/**
	 * Get the rowHeight table properties of a specific table row.<br>
	 * <p>
	 * That's a cover of <code>getHeight()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param row the row number
	 */
	public int getTableRowHeight(int row) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getRowHeight(row);
	}

	/**
	 * Sets the rowHeight table properties.<br>
	 * <p>
	 * That's a cover of <code>setHeight()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param rowHeight in pixel
	 */
	public void setTableRowHeight(int rowHeight) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setRowHeight(rowHeight);
		return;
	}


	/**
	 * Sets the rowHeight table properties of a specific table row.<br>
	 * <p>
	 * That's a cover of <code>getHeight()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param row the row number
	 */
	public void setTableRowHeight(int row, int rowHeight) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setRowHeight(row, rowHeight);
		return;
	}

	/**
	 * Sets the width of a table column.<br>
	 * <p>
	 * If the column number is out of range, no action will be taken.<br>
     * <p>
     * @param columnNumber 0-based
     * @param columnWidth in pixel
	 */
	public void setTableColumnWidth(int columnNumber, int columnWidth) {
		ForwardTableModel tableModel = null;
		ForwardTableModelColumn tableColumn = null;
		tableModel = this.getTableModel();
		if (columnNumber >= tableModel._getColumns().size()) {return;}
		tableColumn = tableModel._getColumn(columnNumber);
		tableColumn.setWidth(columnWidth);
		return;
	}

	/**
	 * Sets the width of a table column.<br>
	 * <p>
 	 * If the column number is out of range, no action will be taken.<br>
     * <p>
      * @param columnName in pixel
     * @param columnWidth in pixel
	 */
	public void setTableColumnWidth(String columnName, int columnWidth) {
		ForwardTableModel tableModel = null;
		ForwardTableModelColumn tableColumn = null;
		int columnNumber = 0;
		
		tableModel = this.getTableModel();
		columnNumber = tableModel._getColumnIndex(columnName);
		if (columnNumber < 0 ) {return;}
		tableColumn = tableModel._getColumn(columnNumber);
		tableColumn.setWidth(columnWidth);
		return;
	}

	
	/**
	 * Get the rowMargin table properties.<br>
	 * <p>
	 * Gets the amount of empty space, in pixels, between cells. <br>
	 * That's a cover of <code>getRowMargin()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return rowMargin in pixel
	 */
	public int getTableRowMargin() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getRowMargin();
	}


	/**
	 * Sets the rowMargin table properties.<br>
	 * <p>
	 * Sets the amount of empty space, in pixels, between cells. <br>
	 * That's a cover of <code>getRowMargin()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param rowHeight in pixel
	 */
	public void setTableRowMargin(int rowMargin) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setRowMargin(rowMargin);
		return;
	}


	/**
	 * Get the SelectionBackground table properties.<br>
	 * <p>
	 * That's a cover of <code>getSelectionBackground()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return the selection background color
	 */
	public Color getTableSelectionBackground() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getSelectionBackground();
	}


	/**
	 * Sets the SelectionBackground table properties.<br>
	 * <p>
	 * That's a cover of <code>setSelectionBackground()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param selectionBackground the color to set
	 */
	public void setTableSelectionBackground(Color selectionBackground) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setSelectionBackground(selectionBackground);
		return;
	}


	/**
	 * Get the SelectionForeground table properties.<br>
	 * <p>
	 * That's a cover of <code>getSelectionForeground()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return the selection background color
	 */
	public Color getTableSelectionForeground() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getSelectionForeground();
	}


	/**
	 * Sets the SelectionForeground table properties.<br>
	 * <p>
	 * That's a cover of <code>setSelectionForeground()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param selectionForeground the color to set
	 */
	public void setTableSelectionForeground(Color selectionForeground) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setSelectionForeground(selectionForeground);
		return;
	}

	/**
	 * Get the SelectionMode table selection mode properties.<br>
	 * <p>
	 * That's a cover of <code>getSelectionModel().getSelectionMode()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return the table selection mode as a ListSelectionModel constant <code>MULTIPLE_INTERVAL_SELECTION SINGLE_INTERVAL_SELECTION SINGLE_SELECTION</code>	
	 */
	public int getTableSelectionMode() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getSelectionModel().getSelectionMode();
	}


	/**
	 * Sets the SelectionMode table properties.<br>
	 * <p>
	 * That's a cover of <code>setSelectionMode()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param selectionMode as a ListSelectionModel constant <code>MULTIPLE_INTERVAL_SELECTION SINGLE_INTERVAL_SELECTION SINGLE_SELECTION</code>	 
     */
	public void setTableSelectionMode(int selectionMode) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setSelectionMode(selectionMode);
		return;
	}


	/**
	 * Get if the table row selection is allowed.<br>
	 * <p>
	 * That's a cover of <code>getRowSelectionAllowed()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return true if table row selection allowed	
	 */
	public boolean getTableRowSelectionAllowed() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getRowSelectionAllowed();
	}


	/**
	 * Sets if the table row selection is allowed.<br>
	 * <p>
	 * That's a cover of <code>setRowSelectionAllowed()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param rowSelectionAllowed as a boolean value	 
     */
	public void setTableRowSelectionAllowed(boolean rowSelectionAllowed) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setRowSelectionAllowed(rowSelectionAllowed);
		return;
	}


	/**
	 * Sets whether the table draws grid lines around cells. <br>
	 * <p>
	 * That's a cover of <code>setShowGrid()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param showGrid as true to paint lines around cells
     */
	public void setTableShowGrid(boolean showGrid) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setShowGrid(showGrid);
		return;
	}

	/**
	 * Returns whether the table draws horizontal lines between cells. <br>
	 * <p>
	 * That's a cover of <code>getShowHorizontalLines()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return true if horizontal lines have to be painted
     */
	public boolean getTableShowHorizontalLines() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getShowHorizontalLines();
	}

	/**
	 * Sets whether the table draws horizontal lines between cells. <br>
	 * <p>
	 * That's a cover of <code>setShowHorizontalLines()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param showHorizontalLines as true to paint horizontal lines
     */
	public void setTableShowHorizontalLines(boolean showHorizontalLines) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setShowHorizontalLines(showHorizontalLines);
		return;
	}

	/**
	 * Returns whether the table draws vertical lines between cells. <br>
	 * <p>
	 * That's a cover of <code>getShowHorizontalLines()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return true if horizontal lines have to be painted
     */
	public boolean getTableShowVerticalLines() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getShowVerticalLines();
	}

	/**
	 * Sets whether the table draws vertical lines between cells. <br>
	 * <p>
	 * That's a cover of <code>setShowVerticalLines()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param showVerticalLines as true to paint vertical lines
     */
	public void setTableShowVerticalLines(boolean showVerticalLines) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setShowVerticalLines(showVerticalLines);
		return;
	}



	/**
	 * Get if the table column selection is allowed.<br>
	 * <p>
	 * That's a cover of <code>getColumnSelectionAllowed()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @return true if table column selection allowed	
	 */
	public boolean getTableColumnSelectionAllowed() {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		return jtable.getColumnSelectionAllowed();
	}


	/**
	 * Sets if the table column selection is allowed.<br>
	 * <p>
	 * That's a cover of <code>setColumnSelectionAllowed()</code> of {@link JTable}.<br>
	 * Please refer to it for detailed documentation.
     * <p>
     * @param column as a boolean value	 
     */
	public void setTableColumnSelectionAllowed(boolean columnSelectionAllowed) {
		JTable jtable = null;
		jtable = (JTable) this.graphicObject;
		jtable.setColumnSelectionAllowed(columnSelectionAllowed);
		return;
	}


	
	
	/**
	 * Get the {@link ForwardTreeModel} object that inherits from DefaultTreeModel, used to manage data.<br>
	 * <p>
	 * @return the treeModel
	 */
	public ForwardTreeModel getTreeModel() {
		return treeModel;
	}


	/**
	 * Sets the {@link ForwardTreeModel} object that inherits from DefaultTreeModel, used to manage data.<br>
	 * <p>
	 * @param treeModel the treeModel to set
	 */
	public void setTreeModel(ForwardTreeModel treeModel) {
		this.treeModel = treeModel;
	}


	/**
	 * Get the {@link ForwardComboBoxModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of combo box list rows.<br>
	 * <p>
	 * @return the listModel
	 */
	@SuppressWarnings({ "rawtypes" })
	public ForwardComboBoxModel getComboBoxModel() {
		JComboBox jcomboBox = null;
		jcomboBox = (JComboBox) this.graphicObject;
		return (ForwardComboBoxModel) jcomboBox.getModel();
	}


	/**
	 * Sets the {@link ForwardComboBoxModel} used to store data.<br>
	 * <p>
	 * Uset to manage insert amd delete of combo box list rows.<br>
	 * <p>
	 * @param comboBoxModel the comboBoxModel to set
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void setComboBoxModel(ForwardComboBoxModel comboBoxModel) {
		JComboBox jcomboBox = null;
		jcomboBox = (JComboBox) this.graphicObject;
		jcomboBox.setModel(comboBoxModel);
	}




	/**
	 * Restituisce il nome della logical data view di popolamento della combo associata al campo.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @return the comboLdv
	 */
	public String getComboLdv() {
		return comboLdv;
	}


	/**
	 * Imposta il nome della logical data view di popolamento della combo associata al campo.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @param comboLdv the comboLdv to set
	 */
	public void setComboLdv(String comboLdv) {
		this.comboLdv = comboLdv;
	}


	/**
	 * Restituisce i nomi delle colonne della logical data view per popolare la combo associata al campo.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @return the al_comboLdvColumn
	 */
	public ArrayList<String> getComboLdvColumns() {
		return al_comboLdvColumn;
	}


	/**
	 * Imposta i nomi delle colonne della logical data view per popolare la combo associata al campo.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @param al_comboLdvColumn the al_comboLdvColumn to set
	 */
	public void setComboLdvColumns(ArrayList<String> al_comboLdvColumn) {
		this.al_comboLdvColumn = al_comboLdvColumn;
	}


	/**
	 * Restituisce il nome della colonna della logical data view con cui valorizzare il campo della cobo bozx alla selezione.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @return the comboValueLdvSourceColumn
	 */
	public String getComboValueLdvSourceColumn() {
		return comboValueLdvSourceColumn;
	}


	/**
	 * Imposta il nome della colonna della logical data view con cui valorizzare il campo della cobo bozx alla selezione.<br>
	 * <p>
	 * La logical data view rappresenta una query comunque complessa che restituisce una serie di righe/colonne<br>
	 * <p>
	 * @param comboValueLdvSourceColumn the comboValueLdvSourceColumn to set
	 */
	public void setComboValueLdvSourceColumn(String comboValueLdvSourceColumn) {
		this.comboValueLdvSourceColumn = comboValueLdvSourceColumn;
	}

	/**
	 * Restituisce il nome della funzione applicativa modale di Lookup di selezione valori per il campo.<br>
	 * <p>
	 * Tale funzione prevede di restituire uno o più campi di valori.<br>
	 * I campi necessari alla funzione chiamata sono impostati automaticamente dal contesto corrente.<br>
	 * Il monitor di forward, all'uscita dalla funzione chiamata, valorizzerà automaticamente il campo carrente.<br>
	 * <p>
	 * @return the lookupFunction
	 */
	public String getLookupFunction() {
		return lookupFunction;
	}


	/**
	 * Imposta il nome della funzione applicativa modale di Lookup di selezione valori per il campo.<br>
	 * <p>
	 * Tale funzione prevede di restituire uno o più campi di valori.<br>
	 * I campi necessari alla funzione chiamata sono impostati automaticamente dal contesto corrente.<br>
	 * Il monitor di forward, all'uscita dalla funzione chiamata, valorizzerà automaticamente il campo carrente.<br>
	 * <p>
	 * @param lookupFunction the lookupFunction to set
	 */
	public void setLookupFunction(String lookupFunction) {
		this.lookupFunction = lookupFunction;
	}

	/**
	 * Restituisce il numero della tabella applicativa modale di Lookup dalla quale selezionare i valori per il campo.<br>
	 * <p>
	 * Una specifica funzione modale di forward visualizza gli elementi di una tabella del sistema tabelle di forward<br>
	 * Una tabella può contenere più di un campo definito ed esigenze applicative possono portare alla necessità di <br>
	 * selezionare valori da qualsisia colonna di qualsiasi tabella, senza limitazioni.<br>
	 * I campi necessari alla funzione chiamata sono impostati automaticamente dal contesto corrente.<br>
	 * Il monitor di forward, all'uscita dalla funzione chiamata, valorizzerà automaticamente il campo carrente.<br>
	 * <p>
	 * @return the lookupTableNum
	 */
	public int getLookupTableNum() {
		return lookupTableNum;
	}


	/**
	 * Imposta il numero della tabella applicativa modale di Lookup dalla quale selezionare i valori per il campo.<br>
	 * <p>
	 * Una specifica funzione modale di forward visualizza gli elementi di una tabella del sistema tabelle di forward<br>
	 * Una tabella può contenere più di un campo definito ed esigenze applicative possono portare alla necessità di <br>
	 * selezionare valori da qualsisia colonna di qualsiasi tabella, senza limitazioni.<br>
	 * I campi necessari alla funzione chiamata sono impostati automaticamente dal contesto corrente.<br>
	 * Il monitor di forward, all'uscita dalla funzione chiamata, valorizzerà automaticamente il campo carrente.<br>
	 * <p>
	 * @param lookupTableNum the lookupTableNum to set
	 */
	public void setLookupTableNum(int lookupTableNum) {
		this.lookupTableNum = lookupTableNum;
	}


	/**
	 * Gets all parameters required to the called lookup function linked to the panel component.<vr>
	 * <p>
	 * It will be returned an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter required by called application, defined in the called function by means of a <code>PARM_REQUIRED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function.<br>
	 * <p>
	 * @return the al_lookupFunctionParmsRequired
	 */
	public ArrayList<String> getLookupFunctionParmsRequired() {
		return al_lookupFunctionParmsRequired;
	}


	/**
	 * Sets all parameters required to the called lookup function linked to the panel component.<vr>
	 * <p>
	 * It will be set an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter required by called application, defined in the called function by means of a <code>PARM_REQUIRED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function.<br>
	 * <p>
	 * @param al_lookupFunctionParmsRequired the al_lookupFunctionParmsRequired to set
	 */
	public void setLookupFunctionParmsRequired(ArrayList<String> al_lookupFunctionParmsRequired) {
		this.al_lookupFunctionParmsRequired = al_lookupFunctionParmsRequired;
	}


	/**
	 * Gets all parameters returned to the called lookup function linked to the panel component.<vr>
	 * <p>
	 * It will be returned an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter returned by called application, defined in the called function by means of a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function.<br>
	 * <p>
	 * @return the al_lookupFunctionParmsReturned
	 */
	public ArrayList<String> getLookupFunctionParmsReturned() {
		return al_lookupFunctionParmsReturned;
	}


	/**
	 * Sets all parameters returned to the called lookup function linked to the panel component.<vr>
	 * <p>
	 * It will be set an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the application function caller, as a variable or GUI control name.<br>
	 * The second is the parameter returned by called application, defined in the called function by means of a <code>PARM_RETURNED()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all parameters required by called function with data of caller function.<br>
	 * <p>
	 * @param al_lookupFunctionParmsReturned the al_lookupFunctionParmsReturned to set
	 */
	public void setLookupFunctionParmsReturned(
			ArrayList<String> al_lookupFunctionParmsReturned) {
		this.al_lookupFunctionParmsReturned = al_lookupFunctionParmsReturned;
	}


	/**
	 * Gets all columns key for the execution of the logical data view linked to the panel component.<vr>
	 * <p>
	 * It will be returned an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the current application function, as a variable or GUI control name.<br>
	 * The second is the key column name required by logical data view, defined in the ldv by means of a <code>VAR()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all key columns required by the logical data view with data of caller function.<br>
	 * <p>
	 * @return the al_ldvColumnsKey
	 */
	public ArrayList<String> getLdvColumnsKey() {
		return al_ldvColumnsKey;
	}


	/**
	 * Sets all columns key for the execution of the logical data view linked to the panel component.<vr>
	 * <p>
	 * It will be set an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the current application function, as a variable or GUI control name.<br>
	 * The second is the key column name required by logical data view, defined in the ldv by means of a <code>VAR()</code> declaration.<br>
	 * <p>
	 * The forward monitor, automatically will set all key columns required by the logical data view with data of caller function.<br>
	 * <p>
	 * @param al_ldvColumnsKey the al_ldvColumnsKey to set
	 */
	public void setLdvColumnsKey(ArrayList<String> al_ldvColumnsKey) {
		this.al_ldvColumnsKey = al_ldvColumnsKey;
	}


	/**
	 * Gets all columns returned by logical data view linked to the panel component.<vr>
	 * <p>
	 * It will be returned an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the current application function, as a variable or GUI control name.<br>
	 * The second is the logical data view column name, to be used as input to update the variable or GUI control name.<br>
	 * <p>
	 * The forward monitor, automatically will set all function variables or GUI controls, with the logical data view column with the same name.<br>
	 * <p>
	 * @return the al_ldvColumnsReturned
	 */
	public ArrayList<String> getLdvColumnsReturned() {
		return al_ldvColumnsReturned;
	}


	/**
	 * Sets all columns returned by logical data view linked to the panel component.<vr>
	 * <p>
	 * It will be set an array list of strings and each array list elements contains two substrings:<br>
	 * The first is the component name in the current application function, as a variable or GUI control name.<br>
	 * The second is the logical data view column name, to be used as input to update the variable or GUI control name.<br>
	 * <p>
	 * The forward monitor, automatically will set all function variables or GUI controls, with the logical data view column with the same name.<br>
	 * <p>
	 * @param al_ldvColumnsReturned the al_ldvColumnsReturned to set
	 */
	public void setLdvColumnsReturned(ArrayList<String> al_ldvColumnsReturned) {
		this.al_ldvColumnsReturned = al_ldvColumnsReturned;
	}

	
	/**
	 * Gets the map with all options currently set for the component.<br>
	 * <br>
	 * The key is the enumeration {@link EnumForwardOption} for about entries starting with <code>COMPONENT</code> mapped on a Boolean true/lase:<pre>
	COMPONENT_CTRL_FORMAL_TODO 
	COMPONENT_CTRL_EXISTS_TABLE_TODO 
	COMPONENT_CTRL_UNEXISTS_TABLE_TODO 
	COMPONENT_CTRL_EXISTS_LDV_TODO 
	COMPONENT_CTRL_UNEXISTS_LDV_TODO 
	COMPONENT_MANDATORY 
	COMPONENT_WITH_ERRORS 
	COMPONENT_GOOD_WITH_ERRORS 
	COMPONENT_HIDDEN 
	COMPONENT_DISABLED 
	COMPONENT_AUTOSKIP 
	COMPONENT_HILIGHT 
	COMPONENT_HELP_TECH_ACTIVE 
	COMPONENT_HELP_USER_ACTIVE </pre>
	 * @return the hm_componentOptions
	 */
	public Map<EnumForwardOption, Boolean> getOptionsMap() {
		return hm_componentOptions;
	}


	
	/**
	 * Gets if an option is currently set for the component.<br>
	 * <br>
	 * The option is coded by the enumeration {@link EnumForwardOption} for about entries starting with <code>COMPONENT</code> like:<pre>
	COMPONENT_CTRL_FORMAL_TODO 
	COMPONENT_CTRL_EXISTS_TABLE_TODO 
	COMPONENT_CTRL_UNEXISTS_TABLE_TODO 
	COMPONENT_CTRL_EXISTS_LDV_TODO 
	COMPONENT_CTRL_UNEXISTS_LDV_TODO 
	COMPONENT_MANDATORY 
	COMPONENT_WITH_ERRORS 
	COMPONENT_GOOD_WITH_ERRORS 
	COMPONENT_HIDDEN 
	COMPONENT_DISABLED 
	COMPONENT_AUTOSKIP 
	COMPONENT_HILIGHT 
	COMPONENT_HELP_TECH_ACTIVE 
	COMPONENT_HELP_USER_ACTIVE </pre>
	<p>
	 *@param option the {@link EnumForwardOption} 
	 * @return true if the option is active or false if not
	 */
	public boolean isOption(EnumForwardOption option) {
		Boolean isOption = false;
		isOption = hm_componentOptions.get(option);
		if (isOption == null) {
			return false;
		}
		return isOption;
	}

	/**
	 * Sets if an option is currently set for the component.<br>
	 * <br>
	 * The option is coded by the enumeration {@link EnumForwardOption} for about entries starting with <code>COMPONENT</code> like:<pre>
	COMPONENT_CTRL_FORMAL_TODO 
	COMPONENT_CTRL_EXISTS_TABLE_TODO 
	COMPONENT_CTRL_UNEXISTS_TABLE_TODO 
	COMPONENT_CTRL_EXISTS_LDV_TODO 
	COMPONENT_CTRL_UNEXISTS_LDV_TODO 
	COMPONENT_MANDATORY 
	COMPONENT_WITH_ERRORS 
	COMPONENT_GOOD_WITH_ERRORS 
	COMPONENT_HIDDEN 
	COMPONENT_DISABLED 
	COMPONENT_AUTOSKIP 
	COMPONENT_HILIGHT 
	COMPONENT_HELP_TECH_ACTIVE 
	COMPONENT_HELP_USER_ACTIVE </pre>
	<p>
	 * @param option the {@link EnumForwardOption} to set 
	 * @param optionValue the boolean value to set
	 */
	public void setOption(EnumForwardOption option, boolean optionValue) {
		hm_componentOptions.put(option, optionValue);
		return;
	}



	/**
	 * Gets the minimum size allowed for the panel component text.<br>
	 * <p>
	 * @return the ctrlTextSizeMin
	 */
	public int getCtrlTextSizeMin() {
		return ctrlTextSizeMin;
	}


	/**
	 * Sets the minimum size allowed for the panel component text.<br>
	 * <p>
	 * @param ctrlTextSizeMin the ctrlTextSizeMin to set
	 */
	public void setCtrlTextSizeMin(int ctrlTextSizeMin) {
		this.ctrlTextSizeMin = ctrlTextSizeMin;
	}


	/**
	 * Gets the maximum size allowed for the panel component text.<br>
	 * <p>
	 * @return the ctrlTextSizeMax
	 */
	public int getCtrlTextSizeMax() {
		return ctrlTextSizeMax;
	}


	/**
	 * Sets the maximum size allowed for the panel component text.<br>
	 * <p>
	 * @param ctrlTextSizeMax the ctrlTextSizeMax to set
	 */
	public void setCtrlTextSizeMax(int ctrlTextSizeMax) {
		this.ctrlTextSizeMax = ctrlTextSizeMax;
	}


	/**
	 * Gets the minimum number of integer digits allowed.<br>
	 * <p>
	 * @return the ctrlIntSizeMin
	 */
	public int getCtrlIntSizeMin() {
		return ctrlIntSizeMin;
	}


	/**
	 * Sets the minimum number of integer digits allowed.<br>
	 * <p>
	 * @param ctrlIntSizeMin the ctrlIntSizeMin to set
	 */
	public void setCtrlIntSizeMin(int ctrlIntSizeMin) {
		this.ctrlIntSizeMin = ctrlIntSizeMin;
	}


	/**
	 * Gets the maximum number of integer digits allowed.<br>
	 * <p>
	 * @return the ctrlIntSizeMax
	 */
	public int getCtrlIntSizeMax() {
		return ctrlIntSizeMax;
	}


	/**
	 * Sets the maximum number of integer digits allowed.<br>
	 * <p>
	 * @param ctrlIntSizeMax the ctrlIntSizeMax to set
	 */
	public void setCtrlIntSizeMax(int ctrlIntSizeMax) {
		this.ctrlIntSizeMax = ctrlIntSizeMax;
	}


	/**
	 * Gets the minimum number of decimal digits allowed.<br>
	 * <p>
	 * @return the ctrlDecSizeMin
	 */
	public int getCtrlDecSizeMin() {
		return ctrlDecSizeMin;
	}


	/**
	 * Sets the minimum number of decimal digits allowed.<br>
	 * <p>
	 * @param ctrlDecSizeMin the ctrlDecSizeMin to set
	 */
	public void setCtrlDecSizeMin(int ctrlDecSizeMin) {
		this.ctrlDecSizeMin = ctrlDecSizeMin;
	}


	/**
	 * Gets the maximum number of decimal digits allowed.<br>
	 * <p>
	 * @return the ctrlDecSizeMax
	 */
	public int getCtrlDecSizeMax() {
		return ctrlDecSizeMax;
	}


	/**
	 * Sets the maximum number of decimal digits allowed.<br>
	 * <p>
	 * @param ctrlDecSizeMax the ctrlDecSizeMax to set
	 */
	public void setCtrlDecSizeMax(int ctrlDecSizeMax) {
		this.ctrlDecSizeMax = ctrlDecSizeMax;
	}


	/**
	 * Gets the minimum value allowed.<br>
	 * <p>
	 * The object type depends on the type of GUI component type and so<br>
	 * can be String, Integer, Double, Float<br>
	 * <p>
	 * @return the ctrlValueMin
	 */
	public Object getCtrlValueMin() {
		return ctrlValueMin;
	}


	/**
	 * Sets the minimum value allowed.<br>
	 * <p>
	 * The object type depends on the type of GUI component type and so<br>
	 * can be String, Integer, Double, Float<br>
	 * <p>
	 * @param ctrlValueMin the ctrlValueMin to set
	 */
	public void setCtrlValueMin(Object ctrlValueMin) {
		this.ctrlValueMin = ctrlValueMin;
	}


	/**
	 * Gets the maximum value allowed.<br>
	 * <p>
	 * The object type depends on the type of GUI component type and so<br>
	 * can be String, Integer, Double, Float<br>
	 * <p>
	 * @return the ctrlValueMax
	 */
	public Object getCtrlValueMax() {
		return ctrlValueMax;
	}


	/**
	 * Sets the maximum value allowed.<br>
	 * <p>
	 * The object type depends on the type of GUI component type and so<br>
	 * can be String, Integer, Double, Float<br>
	 * <p>
	 * @param ctrlValueMax the ctrlValueMax to set
	 */
	public void setCtrlValueMax(Object ctrlValueMax) {
		this.ctrlValueMax = ctrlValueMax;
	}


	/**
	 * Gets the minimum value of integer digits allowed.<br>
	 * <p>
	 * @return the ctrlValueIntMin
	 */
	public int getCtrlValueIntMin() {
		return ctrlValueIntMin;
	}
 

	/**
	 * Sets the minimum value of integer digits allowed.<br>
	 * <p>
	 * @param ctrlValueIntMin the ctrlValueIntMin to set
	 */
	public void setCtrlValueIntMin(int ctrlValueIntMin) {
		this.ctrlValueIntMin = ctrlValueIntMin;
	}


	/**
	 * Gets the maximum value of integer digits allowed.<br>
	 * <p>
	 * @return the ctrlValueIntMax
	 */
	public int getCtrlValueIntMax() {
		return ctrlValueIntMax;
	}


	/**
	 * Sets the maximum value of integer digits allowed.<br>
	 * <p>
	 * @param ctrlValueIntMax the ctrlValueIntMax to set
	 */
	public void setCtrlValueIntMax(int ctrlValueIntMax) {
		this.ctrlValueIntMax = ctrlValueIntMax;
	}


	/**
	 * Gets the minimum value of decimal digits allowed.<br>
	 * <p>
	 * @return the ctrlValueDecMin
	 */
	public int getCtrlValueDecMin() {
		return ctrlValueDecMin;
	}


	/**
	 * Sets the minimum value of decimal digits allowed.<br>
	 * <p>
	 * @param ctrlValueDecMin the ctrlValueDecMin to set
	 */
	public void setCtrlValueDecMin(int ctrlValueDecMin) {
		this.ctrlValueDecMin = ctrlValueDecMin;
	}


	/**
	 * Gets the maximum value of decimal digits allowed.<br>
	 * <p>
	 * @return the ctrlValueDecMax
	 */
	public int getCtrlValueDecMax() {
		return ctrlValueDecMax;
	}


	/**
	 * Sets the maximum value of decimal digits allowed.<br>
	 * <p>
	 * @param ctrlValueDecMax the ctrlValueDecMax to set
	 */
	public void setCtrlValueDecMax(int ctrlValueDecMax) {
		this.ctrlValueDecMax = ctrlValueDecMax;
	}


	/**
	 * Gets all values allowed.<br>
	 * <p>
	 * It will be returned an ArrayList of objects.<br>
	 * The type of each object element must be congruent with the GUI component<br>
	 * and so can be String, Integer, Double.<br>
	 * <p>
	 * @return the al_ctrlValues
	 */
	public ArrayList<Object> getCtrlValues() {
		return al_ctrlValues;
	}


	/**
	 * Gets the mask to be used to control the component in the java JFormatter form.<br>
	 * <p>
	 * @return the ctrlMask
	 */
	public String getCtrlMask() {
		return ctrlMask;
	}


	/**
	 * Sets the mask to be used to control the component in the java JFormatter form.<br>
	 * <p>
	 * @param ctrlMask the ctrlMask to set
	 */
	public void setCtrlMask(String ctrlMask) {
		this.ctrlMask = ctrlMask;
	}


	/**
	 * Get the rule table number to be used during the CONTROLS action.<br>
	 * <p>
	 * @return the ctrlTableNum
	 */
	public int getCtrlTableNum() {
		return ctrlTableNum;
	}


	/**
	 * Sets the rule table number to be used during the CONTROLS action.<br>
	 * <p>
	 * @param ctrlTableNum the ctrlTableNum to set
	 */
	public void setCtrlTableNum(int ctrlTableNum) {
		this.ctrlTableNum = ctrlTableNum;
	}


	/**
	 * Gets the placeholder to be used by JFormattedTextField<br>
	 * <p>
	 * @return the placeHolderCharacter
	 */
	public String getPlaceHolderCharacter() {
		return placeHolderCharacter;
	}


	/**
	 * Sets the placeholder to be used by JFormattedTextField<br>
	 * <p>
	 * @param placeHolderCharacter the placeHolderCharacter to set
	 */
	public void setPlaceHolderCharacter(String placeHolderCharacter) {
		this.placeHolderCharacter = placeHolderCharacter;
	}


	/**
	 * Restituisce il nome della logical data view da eseguire a fronte di dati immessi nel campo.<br>
	 * <p>
	 * L'attivazione della logical data view avviene automaticamente a fronte di assenza di errori formali.<br>
	 * <p>
	 * L'accesso al sistema tabelle/entity/ldv avviene utilizzando il campo come chiave o parte di chiave<br>
	 * ed è necessario per valorizzare altri campi o per rendere disponibili dati all'applicazione.<br>
	 * Le eventuali parti di chiavi mancanti vengono reperite runtime dallo stesso o da altri pannelli <br>
	 * La logical Data View può accedere a 0 o più entities e a 0 più tabelle del del sistema tabelle di Forward<br>
	 * La ldv eseguita restituisce una riga di valori, ogni colonna corrisponde al nome di un campo.<br>
	 * Ogni ldv restituisce i nomi dei campi chiave e di quelli non chiave<br>
	 * 
	 * @return the logical data view name 
	 */
	public String getLdv() {
		return ldv;
	}


	/**
	 * Imposta il nome della logical data view da eseguire a fronte di dati immessi nel campo.<br>
	 * <p>
	 * L'attivazione della logical data view avviene automaticamente a fronte di assenza di errori formali.<br>
	 * <p>
	 * L'accesso al sistema tabelle/entity/ldv avviene utilizzando il campo come chiave o parte di chiave<br>
	 * ed è necessario per valorizzare altri campi o per rendere disponibili dati all'applicazione.<br>
	 * Le eventuali parti di chiavi mancanti vengono reperite runtime dallo stesso o da altri pannelli <br>
	 * La logical Data View può accedere a 0 o più entities e a 0 più tabelle del del sistema tabelle di Forward<br>
	 * La ldv eseguita restituisce una riga di valori, ogni colonna corrisponde al nome di un campo.<br>
	 * Ogni ldv restituisce i nomi dei campi chiave e di quelli non chiave<br>
	 * 
	 * @param ldv the ldv to set
	 */
	public void setLdv(String ldv) {
		this.ldv = ldv;
	}




	/**
	 * Get if the panel component has been marked as in error by application.<br>
	 * <p>
	 * @return the isWithErrors
	 */
	public boolean isWithErrors() {
		return isWithErrors;
	}


	/**
	 * Sets if the panel component has been marked as in error by application.<br>
	 * <p>
	 * @param isWithErrors the isWithErrors to set
	 */
	public void setWithErrors(boolean isWithErrors) {
		this.isWithErrors = isWithErrors;
	}



	/**
	 * Gets if the panel component has been automatically marked with formal errors <br>
	 * by forward monitor, at panel control level.<br>
	 * <p>
	 * @return the isWithFormalErrors
	 */
	public boolean isWithFormalErrors() {
		return isWithFormalErrors;
	}


	/**
	 * Sets if the panel component has been automatically marked with formal errors <br>
	 * by forward monitor, at panel control level.<br>
	 * <p>
	 * @param isWithFormalErrors the isWithFormalErrors to set
	 */
	public void setWithFormalErrors(boolean isWithFormalErrors) {
		this.isWithFormalErrors = isWithFormalErrors;
	}


	/**
	 * Gets if the panel component has been automatically marked with  errors <br>
	 * of existence/not existence in rule table, by forward monitor, at panel control level.<br>
	 * <p>
	 * @return the isWithExistsRuleTableErrors
	 */
	public boolean isWithExistsRuleTableErrors() {
		return isWithExistsRuleTableErrors;
	}


	/**
	 * Sets if the panel component has been automatically marked with  errors <br>
	 * of existence/not existence in rule table, by forward monitor, at panel control level.<br>
	 * <p>
	 * @param isWithExistsRuleTableErrors the isWithExistsRuleTableErrors to set
	 */
	public void setWithExistsRuleTableErrors(boolean isWithExistsRuleTableErrors) {
		this.isWithExistsRuleTableErrors = isWithExistsRuleTableErrors;
	}


	/**
	 * Gets if the panel component has been automatically marked with  errors <br>
	 * of existence/not existence in entity, by forward monitor, at panel control level.<br>
	 * <p>
	 * @return the isWithExistsEntityErrors
	 */
	public boolean isWithExistsEntityErrors() {
		return isWithExistsEntityErrors;
	}


	/**
	 * Sets if the panel component has been automatically marked with  errors <br>
	 * of existence/not existence in entity, by forward monitor, at panel control level.<br>
	 * <p>
	 * @param isWithExistsEntityErrors the isWithExistsEntityErrors to set
	 */
	public void setWithExistsEntityErrors(boolean isWithExistsEntityErrors) {
		this.isWithExistsEntityErrors = isWithExistsEntityErrors;
	}


	/**
	 * Restituisce la caption lunga del campo in lingua<br>
	 * <p>
	 * Viene utilizzata nei pannelli per descrivere un campo  in oggetti JLabel.<br>
	 * <p>
	 * @return the captionLong
	 */
	public String getCaptionLong() {
		return captionLong;
	}


	/**
	 * Imposta la caption lunga del campo in lingua<br>
	 * <p>
	 * Viene utilizzata nei pannelli per descrivere un campo  in oggetti JLabel.<br>
	 * <p>
	 * @param captionLong the captionLong to set
	 */
	public void setCaptionLong(String captionLong) {
		this.captionLong = captionLong;
	}


	/**
	 * Restituisce la caption breve del campo in lingua<br>
	 * <p>
	 * Viene utilizzata nei pannelli per descrivere un pulsante in oggetti JButton 
	 * e in tutti i casi in cui è necessaria una descrizione in lingua ridotta del campo.<br>
	 * <p>
	 * @return the captionShort
	 */
	public String getCaptionShort() {
		return captionShort;
	}


	/**
	 * Imposta la caption breve del campo in lingua<br>
	 * <p>
	 * Viene utilizzata nei pannelli per descrivere un pulsante in oggetti JButton 
	 * e in tutti i casi in cui è necessaria una descrizione in lingua ridotta del campo.<br>
	 * <p>
	 * @param captionShort the captionShort to set
	 */
	public void setCaptionShort(String captionShort) {
		this.captionShort = captionShort;
	}


	/**
	 * Restituisce la caption del toolTip in lingua<br>
	 * <p>
	 * Si tratta della descrizione che compare al passaggio del mouse sul campo.<br> 
	 * <p>
	 * @return the toolTip
	 */
	public String getToolTip() {
		return toolTip;
	}


	/**
	 * Imposta la caption del toolTip in lingua<br>
	 * <p>
	 * Si tratta della descrizione che compare al passaggio del mouse sul campo.<br> 
	 * <p>
	 * @param toolTip the toolTip to set
	 */
	public void setToolTip(String toolTip) {
		JComponent jcomponent = null;
		this.toolTip = toolTip;
		if (this.graphicObject == null) {return;}
		jcomponent = (JComponent) this.graphicObject;
		jcomponent.setToolTipText(toolTip);
	}


	/**
	 * Get the minimum value for the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderMinimum
	 */
	public int getSliderMinimum() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getMinimum();
	}


	/**
	 * Sets the minimum value for the JSlider Swing control.<br>
	 * <p>
	 * @param sliderMinimum the sliderMinimum to set
	 */
	public void setSliderMinimum(int sliderMinimum) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setMinimum(sliderMinimum);
	}


	/**
	 * Get the maximum value for the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderMaximum
	 */
	public int getSliderMaximum() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getMaximum();
	}


	/**
	 * Sets the maximum value for the JSlider Swing control.<br>
	 * <p>
	 * @param sliderMaximum the sliderMaximum to set
	 */
	public void setSliderMaximum(int sliderMaximum) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setMaximum(sliderMaximum);  
	}


	/**
	 * Get the initial value for the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderInitialValue
	 */
	public int getSliderInit() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getValue();

	}


	/**
	 * Sets the initial value for the JSlider Swing control.<br>
	 * <p>
	 * @param sliderInitialValue the sliderInitialValue to set
	 */
	public void setSliderInit(int sliderInitialValue) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setValue(sliderInitialValue);
	}


	
	/**
	 * Get the value of major tick spacing between values for the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderMajorTickSpacing
	 */
	public int getSliderMajorTickSpacing() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getMajorTickSpacing();
	}


	/**
	 * Sets the value of major tick spacing between values for the JSlider Swing control.<br>
	 * <p>
	 * @param sliderMajorTickSpacing the sliderMajorTickSpacing to set
	 */
	public void setSliderMajorTickSpacing(int sliderMajorTickSpacing) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setMajorTickSpacing(sliderMajorTickSpacing);
	}


	/**
	 * Get the value of minor tick spacing between values for the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderMinorTickSpacing
	 */
	public int getSliderMinorTickSpacing() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getMinorTickSpacing();
	}


	/**
	 * Sets the value of minor tick spacing between values for the JSlider Swing control.<br>
	 * <p>
	 * @param sliderMinorTickSpacing the sliderMinorTickSpacing to set
	 */
	public void setSliderMinorTickSpacing(int sliderMinorTickSpacing) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setMinorTickSpacing(sliderMinorTickSpacing);
	}


	/**
	 * Get the orientation of the {@link JSlider} swing control.<br>
	 * <p>
	 * JSlider.VERTICAL and JSlider.HORIZONTAL swing constants must be used,<br>
	 * <p>
	 * @return the sliderOrientation
	 */
	public int getSliderOrientation() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getOrientation();
	}


	/**
	 * Sets the orientation of the {@link JSlider} swing control.<br>
	 * <p>
	 * JSlider.VERTICAL and JSlider.HORIZONTAL swing constants must be used,<br>
	 * <p>
	 * @param sliderOrientation the sliderOrientation to set
	 */
	public void setSliderOrientation(int sliderOrientation) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setOrientation(sliderOrientation);
	}


	/**
	 * Get if ticks values for the JSlider Swing control must be shown.<br>
	 * <p>
	 * @return the sliderPaintTicks
	 */
	public boolean isSliderPaintTicks() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getPaintTicks();	
	}


	/**
	 * Sets if ticks values for the JSlider Swing control must be shown.<br>
	 * <p>
	 * @param sliderPaintTicks the sliderPaintTicks to set
	 */
	public void setSliderPaintTicks(boolean sliderPaintTicks) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setPaintTicks(sliderPaintTicks);	
	}


	/**
	 * Get if label values for the JSlider Swing control must be shown.<br>
	 * <p>
	 * @return the sliderPaintLabels
	 */
	public boolean isSliderPaintLabels() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getPaintLabels();	
	}


	/**
	 * Sets if label values for the JSlider Swing control must be shown.<br>
	 * <p>
	 * @param sliderPaintLabels the sliderPaintLabels to set
	 */
	public void setSliderPaintLabels(boolean sliderPaintLabels) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setPaintLabels(sliderPaintLabels);	
	}


	/**
	 * Get the table for customized labels for the JSlider Swing control.<br>
	 * <p>
	 * The key is an int value to bind with a label.<br>
	 * The data is the label text.<br>
	 * <p>
	 * @return the sliderLabelTable
	 */
	@SuppressWarnings("unchecked")
	public Dictionary<Integer, JLabel> getSliderLabelTable() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getLabelTable();
	}


	/**
	 * Sets the table for customized labels for the JSlider Swing control.<br>
	 * <p>
	 * The key is an int value to bind with a label.<br>
	 * The data is the label text.<br>
	 * <p>
	 * @param sliderLabelTable the sliderLabelTable to set
	 */
	public void setSliderLabelTable(Dictionary<Integer, JLabel> sliderLabelTable) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setLabelTable(sliderLabelTable);
	}


	/**
	 * Get the font for labels in the JSlider Swing control.<br>
	 * <p>
	 * @return the sliderFont
	 */
	public Font getSliderFont() {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		return jslider.getFont();
	}


	/**
	 * Sets the font for labels in the JSlider Swing control.<br>
	 * <p>
	 * @param sliderFont the sliderFont to set
	 */
	public void setSliderFont(Font sliderFont) {
		JSlider jslider = null;
		jslider = (JSlider) this.graphicObject;
		jslider.setFont(sliderFont);
	}


	/**
	 * Get the the spinner model.<br>
	 * <p>
	 * The type odf spinner is qualified by the model used to rendeter it.<br>
	 * So for spinner of list text fields will be used {@link SpinnerListModel}<br>
	 * for spinner of numbers will be used {@link SpinnerNumberModel}<br>
	 * and for spinner of date will be used {@link SpinnerDateModel}<br>.<br>
	 * <p>
	 * Will be returned the Class object for the used model.<br>
	 * <p>
	 * @return the spinnerModel
	 */
	public SpinnerModel  getSpinnerModel() {
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		return jspinner.getModel();
	}


	/**
	 * Sets the the spinner model.<br>
	 * <p>
	 * The type odf spinner is qualified by the model used to rendeter it.<br>
	 * So for spinner of list text fields will be used {@link SpinnerListModel}<br>
	 * for spinner of numbers will be used {@link SpinnerNumberModel}<br>
	 * and for spinner of date will be used {@link SpinnerDateModel}<br>.<br>
	 * <p>
	 * @param spinnerModel the spinnerModel to set
	 */
	public void setSpinnerModel(SpinnerModel spinnerModel) {
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		jspinner.setModel(spinnerModel);
	}


	/**
	 * Get the spinner text values.<br>
	 * <p>
	 * This method is enabled for {@link SpinnerListModel}<br>
	 * <p>
	 * @return the List ls_spinnerValue
	 */
	@SuppressWarnings("unchecked")
	public List<String> getSpinnerListValues() {
		
		SpinnerListModel spinnerListModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			return (List<String>) spinnerListModel.getList();
		}
		
		return new ArrayList<String>();
	}


	/**
	 * Sets the spinner text values.<br>
	 * <p>
	 * This method is enabled for {@link SpinnerListModel}<br>
	 * <p>
	 * @param spinnerListValues the spinnerListValues to set
	 */
	public void setSpinnerListValues(List<String> ls_spinnerListValues) {
		
		SpinnerListModel spinnerListModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			spinnerListModel.setList(ls_spinnerListValues);
		}
		
	}


	/**
	 * Get the initial spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model type.<br>
	 * For {@link SpinnerListModel} will be returned a <code>String</code>object<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be returned an <code>Date</code>object<br>
	 * <p>
	 * @return the spinnerInitial
	 */
	public Object getSpinnerInitial() {
		
		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerListModel spinnerListModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			return spinnerNumberModel.getValue();
		}
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			return spinnerListModel.getValue();
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			return spinnerDateModel.getStart();
		}
		
		return null;
	}


	/**
	 * Sets the initial spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model type.<br>
	 * For {@link SpinnerListModel} will be set to a <code>String</code>object<br>
	 * For {@link SpinnerNumberModel} will be set to an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be set to a <code>Date</code>object<br>
	 * <p>
	 * @param spinnerInitial the spinnerInitial to set
	 */
	public void setSpinnerInitial(Object spinnerInitial) {
		
		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerListModel spinnerListModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			spinnerNumberModel.setValue(spinnerInitial);
			return;
		}
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			spinnerListModel.setValue(spinnerInitial);
			return;
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			spinnerDateModel.setValue(spinnerInitial);
			return;
		}
	}


	/**
	 * Get the minimum spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerListModel} will be returned a <code>String</code>object<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be returned an <code>Date</code>object<br>
	 * <p>
	 * @return the spinnerMin
	 */
	public Object getSpinnerMin() {
		
		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerListModel spinnerListModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			return spinnerNumberModel.getMinimum();
		}
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			return spinnerListModel.getList().get(0);
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			return spinnerDateModel.getStart();
		}
		
		return null;
	}


	/**
	 * Sets the minimum spinner value type.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerNumberModel} will be set an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be set an <code>Date</code>object<br>
	 * <p>
	 * @param spinnerMin the spinnerMin to set
	 */
	@SuppressWarnings("rawtypes")
	public void setSpinnerMin(Object spinnerMin) {
		
		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			spinnerNumberModel.setMinimum((Comparable) spinnerMin);
			return;
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			spinnerDateModel.setEnd((Comparable) spinnerMin);
			return;
		}
	}


	/**
	 * Get the maximum spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerListModel} will be returned a <code>String</code>object<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be returned an <code>Date</code>object<br>
	 * <p>
	 * @return the spinnerMax
	 */
	public Object getSpinnerMax() {

		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerListModel spinnerListModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			return spinnerNumberModel.getMaximum();
		}
		
		// Spinner per string
		if (jspinner.getModel() instanceof SpinnerListModel) {
			spinnerListModel = (SpinnerListModel) jspinner.getModel();
			int iMax = spinnerListModel.getList().size();
			return spinnerListModel.getList().get(iMax);
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			return spinnerDateModel.getEnd();
		}
		
		return null;
	}


	/**
	 * Sets the maximum spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * For {@link SpinnerDateModel}<br> will be returned an <code>Date</code>object<br>
	 * <p>
	 * @param spinnerMax the spinnerMax to set
	 */
	@SuppressWarnings("rawtypes")
	public void setSpinnerMax(Object spinnerMax) {
		
		SpinnerNumberModel spinnerNumberModel = null;
		SpinnerDateModel spinnerDateModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			spinnerNumberModel.setMinimum((Comparable) spinnerMax);
			return;
		}
		
		// Spinner per date
		if (jspinner.getModel() instanceof SpinnerDateModel) {
			spinnerDateModel = (SpinnerDateModel) jspinner.getModel();
			spinnerDateModel.setEnd((Comparable) spinnerMax);
			return;
		}
	}


	/**
	 * Get the step spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * Otherwise will be returned a new Integer(0) object<br>
	 * <p>
	 * @return the spinnerStep
	 */
	public Object getSpinnerStep() {


		SpinnerNumberModel spinnerNumberModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			return spinnerNumberModel.getStepSize();
		}
		
		return new Integer(0);
	}


	/**
	 * Sets the step spinner value.<br>
	 * <p>
	 * Will be returned an object depending on the spinner model.<br>
	 * For {@link SpinnerNumberModel} will be returned an <code>Integer</code>object<br>
	 * <p>
	 * @param spinnerStep the spinnerStep to set
	 */
	public void setSpinnerStep(Object spinnerStep) {

		SpinnerNumberModel spinnerNumberModel = null;
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		
		// Spinner per numeri
		if (jspinner.getModel() instanceof SpinnerNumberModel) {
			spinnerNumberModel = (SpinnerNumberModel) jspinner.getModel();
			spinnerNumberModel.setStepSize((Number) spinnerStep);
			return;
		}
	}


	/**
	 * Get the step spinner editor.<br>
	 * <p>
	 * It's a string that depics a mask like "MM/yyyy" for {@link SpinnerDateModel}<br>
	 * or any other mask allowed<br>
	 * <p>
	 * @return the spinnerEditor
	 */
	public JComponent getSpinnerEditor() {
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		return jspinner.getEditor();
	}


	/**
	 * Sets the step spinner editor.<br>
	 * <p>
	 * It's a string that depics a mask like "MM/yyyy" for {@link SpinnerDateModel}<br>
	 * or any other mask allowed<br>
	 * <p>
	 * @param spinnerEditor the spinnerEditor to set
	 */
	public void setSpinnerEditor(JComponent spinnerEditor) {
		JSpinner jspinner = null;
		jspinner = (JSpinner) this.graphicObject;
		jspinner.setEditor(spinnerEditor);
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Name="+componentName+",Type="+componentType.toString();
	}

	
}
