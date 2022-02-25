/**
 * 
 */
package forward;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets; 
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;


import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;

/**
 * ForwardIdeGUI
 * -------------
 * 
 * This class manages creation and update of a
 * generalized graphical user interface.
 * 
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @date 20-09-2008
 *
 */
public class ForwardIdeGUI implements ActionListener,
                                      AdjustmentListener,
                                      FocusListener,
                                      ItemListener,
                                      KeyListener,
                                      MouseListener,
                                      MouseMotionListener,
                                      WindowListener
{
    public static final int BUTTON_TOOLBOX_X_DIM = 50;
	public static final int BUTTON_TOOLBOX_Y_DIM = 50;
    public static final int BUTTON_LITTLE_X_DIM = 20;
	public static final int BUTTON_LITTLE_Y_DIM = 20;

	//-------------------------------------------------
	// Container Panels to fill set by caller 
	//-------------------------------------------------
	private JPanel pWest; 
	private JPanel pCenter; 
	private JPanel pEast;
	
	//-------------------------------------------------
	// Specific Panels to put into containers
	//-------------------------------------------------
	private JSplitPane pEastSplit = new JSplitPane();       //  (0) Split Panel Vertical
	private JPanel pEastSplitToolbox = new JPanel();        //      (1) Top (Flow layout)
	private JPanel pEastSplitProperties = new JPanel();     //      (1) Bot (Grid Layout)
	private JPanel pWestPanels = new JPanel();              //  (0)
	private JPanel pWestForms = new JPanel();               //  (0)
	private JPanel pWestActions = new JPanel();             //  (0)
	private JTabbedPane pCenterTabbed = new JTabbedPane();  //  (0)
	private JPanel pCenterDesign = new JPanel();            //     (1)
	private JPanel pCenterTest = new JPanel();              //     (1)
	private JPanel pCenterSource = new JPanel();            //     (1)
	
	//-------------------------------------------------
	// Colours
	//-------------------------------------------------
	private Color fg = new Color(0,0,0);
	private Color bg = new Color(255,0,0);    // Red
	private Color white = new Color(255,255,255);
	
	//-------------------------------------------------
	// Borders
	//-------------------------------------------------
	private Border emptyBorder = BorderFactory.createEmptyBorder(2,2,2,2); 
	private Border lineBorder = BorderFactory.createLineBorder(bg);
	private Border etchedBorder = BorderFactory.createEtchedBorder();
	private Border compoundBorder = BorderFactory.createCompoundBorder(lineBorder,emptyBorder);
	private Border bevelRaisedBorder = BorderFactory.createBevelBorder(BevelBorder.RAISED,fg,bg);
	private Border bevelLoweredBorder = BorderFactory.createBevelBorder(BevelBorder.LOWERED,fg,bg);
	  
    //-------------------------------------------------
	// Title Borders
	//-------------------------------------------------
	private String pMainCenterMainTitle = "Gui main panel";
	private TitledBorder titleBorderMainCenter = BorderFactory.createTitledBorder(lineBorder, pMainCenterMainTitle);
	private String spEastSplitToolboxTitle = "Toolbox Graphics Objects";
	private TitledBorder titleBorderToolboxObjectGraphic = BorderFactory.createTitledBorder(lineBorder, spEastSplitToolboxTitle);
	private String spEastSplitPropertiesTitle = "Properties";
	private TitledBorder titleBorderProperties = BorderFactory.createTitledBorder(lineBorder, spEastSplitPropertiesTitle);
	private String spPanelsTitle = "Panels";
	private TitledBorder titleBorderPanels = BorderFactory.createTitledBorder(lineBorder, spPanelsTitle);
	private String spFormsTitle = "Forms";
	private TitledBorder titleBorderForms = BorderFactory.createTitledBorder(lineBorder, spFormsTitle);
	private String spActionsTitle = "Actions";
	private TitledBorder titleBorderActions = BorderFactory.createTitledBorder(lineBorder, spActionsTitle);
	
	//-------------------------------------------------
	// Toolbox graphic buttons
	//-------------------------------------------------
	private JButton btJButton = new JButton("Btn");                 	// 01
	private JButton btJLabel = new JButton("Lbl");                  	// 02
	private JButton btJTextField = new JButton("TxtF");      			// 03
	private JButton btJTextArea = new JButton("TxtA");      			// 04
	private JButton btJList = new JButton("List");    				    // 05
	private JButton btJPassword = new JButton("Pwd");     			    // 06
	private JButton btJFormattedTextField = new JButton("TxtFF"); 	    // 07  
	private JButton btJComboBox = new JButton("Cmb");       			// 08
	private JButton btJCheckBox = new JButton("CBox");    			    // 09
	private JButton btJRadioButton = new JButton("RBut"); 			    // 10
	private JButton btJButtonGroup = new JButton("BtnG");   			// 11
	private JButton btJProgressBar = new JButton("ProB");   			// 12
	private JButton btJSLider = new JButton("Slid");     				// 13
	private JButton btJSpinner = new JButton("Spinn");   				// 14
	private JButton btJSeparator = new JButton("Sepa");  				// 15
	private JButton btJScrollPane = new JButton("ScrP");   			    // 16
	private JButton btJToolbar = new JButton("TBar");    				// 17
	private JButton btJSplitPane = new JButton("SpliP"); 				// 18
	private JButton btJTable = new JButton("Table");     				// 19
	private JButton btJTree = new JButton("Tree");       				// 20
	private JButton btJTabbedPane = new JButton("TabP"); 				// 21

    //-------------------------------------------------
	// Array of toolbox buttons
	//-------------------------------------------------
	private JButton oa_btnToolBox[] = {
									   btJButton
									  ,btJLabel
									  ,btJTextField
									  ,btJTextArea
									  ,btJList
									  ,btJPassword
									  ,btJFormattedTextField
									  ,btJComboBox
									  ,btJCheckBox
									  ,btJRadioButton
									  ,btJButtonGroup
									  ,btJProgressBar
									  ,btJSLider
									  ,btJSpinner
									  ,btJSeparator
									  ,btJScrollPane
									  ,btJToolbar
									  ,btJSplitPane
									  ,btJTable
									  ,btJTree
									  ,btJTabbedPane
									};

    //-------------------------------------------------
	// Array of toolbox buttons tooltip
	//-------------------------------------------------
	private String os_btnTooltip[] = {
									  "JButton"                			// 01
								      ,"JLabel"           				// 02
									  ,"JTextField"     				// 03
									  ,"JTextArea"      				// 04
									  ,"JList" 							// 05
									  ,"JPassword"   					// 06
									  ,"JFormattedTextField" 			// 07  
									  ,"JComboBox"      				// 08
									  ,"JCheckBox"						// 09
									  ,"JRadioButton"					// 10
									  ,"ButtonGroup"  					// 11
									  ,"JProgressBar"					// 12
									  ,"JSlider"   						// 13
									  ,"JSpinner"						// 14
									  ,"JSeparator"			  			// 15
									  ,"JScrollPane"					// 16
									  ,"JToolbar"  	  					// 17
									  ,"JSplitPane"						// 18
									  ,"JTable"  						// 19
									  ,"JTree"		     				// 20
									  ,"JTabbedPane"					// 21
									};

	//-------------------------------------------------
	// East panels objects
	//-------------------------------------------------
	private ButtonGroup bgSharedAll = new ButtonGroup();
    private JRadioButton rbShared = new JRadioButton("Shared");
    private JRadioButton rbAll = new JRadioButton("All", true);
    private DefaultListModel lsPanelmodel = new DefaultListModel();
    private JList lsPanel = new JList(lsPanelmodel);
    private JTree treForms = new JTree();	

    //-------------------------------------------------
    // GridBagLayout & Constraints for West panels
    //-------------------------------------------------
    private GridBagLayout gblWestpPanels = new GridBagLayout();
    private GridBagConstraints constraintWestpPanels = new GridBagConstraints();
    
	//-------------------------------------------------
	// Service definitions
	//-------------------------------------------------
    private JButton  bCenter = new JButton("Center");
    private JButton  bCenterDesign = new JButton("Design");
    private JButton  bCenterTest = new JButton("Test");
    private JButton  bCenterSource = new JButton("Source");
	
	// Constructor
	public ForwardIdeGUI(JPanel pWest, JPanel pCenter, JPanel pEast) {
		this.pWest = pWest;
		this.pCenter = pCenter;
		this.pEast = pEast;
	}
    /**
     * defineGUI()
     * 
     * This method supplies the own implementation for GUI functionality.
     * Are available panels West, Center and East, supplied from caller
     * by the constructor. These panels are placed and managed from caller
     * and arranged under the main panel and layout manager.  
     * Here West, Center and East panels are filled with specific panels
     * and layout managers arranged for GUI functionality.
     * So West, Center and East panels are not interested to the type of
     * interface, like Applet, Java application, Desktop panel etc.
     */
	public void defineGUI() {

      //-------------------------------------------------
	  // Prepare East / Center / West panels
      //-------------------------------------------------
		
 	  pEast.setLayout(new GridLayout(1,1,0,0));  
	  pEast.setBorder(emptyBorder); 
	  pWest.setLayout(new GridLayout(3,1,0,0)); 
	  pCenter.setLayout(new BorderLayout());
	  pCenter.setBorder(titleBorderMainCenter);      // Set Border for center panel    

		
	  this.EastPanelsDefinition();    // --> pEastSplit
	  this.centerTabbedDefinition();  // --> pCenterTabbed
	  this.westPanelsDefinition();
	  this.westFormsDefinition();
	  this.westActionsDefinition();
      
	  // Add to panel container
      pEast.add(pEastSplit);
	  pCenter.add(pCenterTabbed); 
	  pWest.add(pWestForms);
	  pWest.add(pWestPanels);
	  pWest.add(pWestActions);
  }

	/*
	 * East panels panel Definition 
    */
	private void EastPanelsDefinition() {

      // Toolbox and properties panel definition		
	  pEastSplitToolbox.setLayout(new GridLayout(7,3,0,0));  
	  pEastSplitToolbox.setBorder(titleBorderToolboxObjectGraphic);  
      this.ToolboxObjectGraphicDefinition();
      pEastSplitProperties.setLayout(new FlowLayout());  
      pEastSplitProperties.setBorder(titleBorderProperties);  
      this.propertiesTableDefinition();
      
      
      // Create a top-bottom split pane
      pEastSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, pEastSplitToolbox, pEastSplitProperties);
      pEastSplit.setContinuousLayout(true);
      pEastSplit.setOneTouchExpandable(true);
	}

	
	/*
	 * 
	 */
	private void propertiesTableDefinition() {
		// TODO Auto-generated method stub

		DefaultTableModel model = new DefaultTableModel();
	    JTable table = new JTable(model);
	    
	    // Create a couple of columns
	    model.addColumn("Col1");
	    model.addColumn("Col2");
	    
	    // Append a row
	    model.addRow(new Object[]{"v1", "v2"});
	    // there are now 2 rows with 2 columns
	    
	    // Append a row with fewer values than columns.
	    // The left-most fields in the new row are populated
	    // with the supplied values (left-to-right) and fields
	    // without values are set to null.
	    model.addRow(new Object[]{"v1"});
	    // there are now 3 rows with 2 columns
	    
	    // Append a row with more values than columns.
	    // The extra values are ignored.
	    model.addRow(new Object[]{"v1", "v2", "v3"});
	    // there are now 4 rows with 2 columns

	    pEastSplitProperties.add(table);
	}
	
	
	/*
	 * West Forms panel Definition 
    */
	private void westFormsDefinition() {
	  pWestForms.setBorder(titleBorderForms); 
	  pWestForms.setLayout(new BorderLayout());

	  // JScrollPane for JTree
  	  JScrollPane scpTreForms = new JScrollPane(treForms,
  			  								   JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
  			  								   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	  pWestForms.add(scpTreForms, BorderLayout.CENTER);	  
	}
		
	
	/*
	 * West Panels panel Definition 
    */
	private void westPanelsDefinition() {
	  pWestPanels.setBorder(titleBorderPanels); 
	  pWestPanels.setLayout(gblWestpPanels); 
	  
	  bgSharedAll.add(rbShared);
	  bgSharedAll.add(rbAll);
	  
  	  // Insert RadioButton 1
  	  BuildConstraints(constraintWestpPanels, 0, 0, 1, 1, 50, 2);    
      constraintWestpPanels.fill = GridBagConstraints.NONE;
      constraintWestpPanels.anchor = GridBagConstraints.WEST;
  	  gblWestpPanels.setConstraints(rbShared, constraintWestpPanels);
  	  pWestPanels.add(rbShared);

  	  // Insert RadioButton 2
  	  BuildConstraints(constraintWestpPanels, 1, 0, 1, 1, 50, 0);    
      constraintWestpPanels.fill = GridBagConstraints.NONE;
      constraintWestpPanels.anchor = GridBagConstraints.WEST;
  	  gblWestpPanels.setConstraints(rbAll, constraintWestpPanels);
  	  pWestPanels.add(rbAll);
  	  // JScrollPane for JList
      int pos = lsPanel.getModel().getSize();
  	  lsPanelmodel.add(pos,"Panel"+pos);
      pos = lsPanel.getModel().getSize();
  	  lsPanelmodel.add(pos,"Panel"+pos);
  	  JScrollPane scpLsPanel = new JScrollPane(lsPanel,
  			  								   JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
  			  								   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  	  scpLsPanel.setSize(new Dimension(120,500));
  	  scpLsPanel.setPreferredSize (new Dimension(120,500));
  	  scpLsPanel.setMinimumSize (new Dimension(120,500));
      scpLsPanel.setMaximumSize (new Dimension(120,500));
  	
  	  // Insert JList 
  	  BuildConstraints(constraintWestpPanels, 0, 1, 2, 1, 0, 98);    
      constraintWestpPanels.fill = GridBagConstraints.NONE;
      constraintWestpPanels.anchor = GridBagConstraints.WEST;
  	  gblWestpPanels.setConstraints(scpLsPanel, constraintWestpPanels);
  	  pWestPanels.add(scpLsPanel);
	}
	
	
	/*
	 * West Actions panel Definition 
    */
	private void westActionsDefinition() {
	  // Title and border
	  pWestActions.setBorder(titleBorderActions); 
	  pWestActions.setLayout(new BorderLayout()); 
	  
	  // Define toolbar with buttons of Action Panel
	  JToolBar tbrWestActions = new JToolBar(); 
	  tbrWestActions.setFloatable(false);
	  
	  JButton  btnActionNew = new JButton("New");
	  JButton  btnActionSave = new JButton("Save");
	  JButton  btnActionDel = new JButton("Del");
	  btnActionNew.setToolTipText("New Form/Panel");
	  btnActionSave.setToolTipText("Save current object");
	  btnActionDel.setToolTipText("Delete CurrentObject");
	  tbrWestActions.add(btnActionNew);
	  btnActionNew.setText(null);
	  btnActionNew.setMargin(new Insets(0, 0, 0, 0));
	  tbrWestActions.add(btnActionSave);
	  btnActionSave.setText(null);
	  btnActionSave.setMargin(new Insets(0, 0, 0, 0));
	  tbrWestActions.add(btnActionDel);
	  btnActionDel.setText(null);
	  btnActionDel.setMargin(new Insets(0, 0, 0, 0));

	  setSizeLittleButton(btnActionNew);
	  setSizeLittleButton(btnActionSave);
	  setSizeLittleButton(btnActionDel);
	  btnActionNew.addActionListener(this);
	  btnActionSave.addActionListener(this);
	  btnActionDel.addActionListener(this);
	  
	  // Define body panel of Action Panel
	  JPanel paWestActionsBody = new JPanel();
	  paWestActionsBody.setLayout(new GridLayout(3,2,0,0)); 
	  // Define objects of body panel
	  JComboBox cbFormPanel = new JComboBox();
	  cbFormPanel.setToolTipText("Panel or Form to manage");
	  cbFormPanel.addActionListener(this);
	  cbFormPanel.addItem("Form");
	  cbFormPanel.addItem("Panel");
	  JTextField tfFormPanelName = new JTextField();
	  
	  JLabel lbEmpty = new JLabel(""); 
	// Add a combobox
	//    JComboBox c3 = new JComboBox(new String[]{"A", "B", "C"});
	//    c3.setPrototypeDisplayValue("XXXXXXXX"); // Set a desired width
	//    c3.setMaximumSize(c3.getMinimumSize());
	//    toolbar.add(c3);
	   
	  JLabel lbMaxRows = new JLabel("Max Rows"); 
	  JTextField tfMaxRows = new JTextField();
	  JLabel lbMaxCols = new JLabel("Max Cols"); 
	  JTextField tfMaxCols = new JTextField();
	  tfMaxRows.setToolTipText("Max Form/Panel Rows Number");
	  tfMaxCols.setToolTipText("Max Form/Panel Cols Number");
	  
	  paWestActionsBody.add(cbFormPanel);
	  paWestActionsBody.add(tfFormPanelName);	
	  paWestActionsBody.add(lbMaxRows);	
	  paWestActionsBody.add(tfMaxRows);	
	  paWestActionsBody.add(lbMaxCols);	
	  paWestActionsBody.add(tfMaxCols);	
	  
	  // Put toolbar and body panel into Action panel
	  pWestActions.add(paWestActionsBody, BorderLayout.NORTH);
	  pWestActions.add(tbrWestActions, BorderLayout.SOUTH);
	}
	
	
    /*
     * Set the size for a little button inside the panel
     */
	private void setSizeLittleButton(JButton btn) {

		btn.setSize(new Dimension(BUTTON_LITTLE_X_DIM,BUTTON_LITTLE_Y_DIM)) ;                		    
		btn.setMinimumSize(new Dimension(BUTTON_LITTLE_X_DIM,BUTTON_LITTLE_Y_DIM)) ;               	 
		btn.setMaximumSize(new Dimension(BUTTON_LITTLE_X_DIM,BUTTON_LITTLE_Y_DIM)) ;               	 
		btn.setPreferredSize(new Dimension(BUTTON_LITTLE_X_DIM,BUTTON_LITTLE_Y_DIM)) ;
		
	}
	/*
	 * Center center panel Definition 
    */
	private void centerTabbedDefinition() {

      // Create a child container which is to be associated with a tab
	  pCenterDesign.add(bCenterDesign); 
	  pCenterTest.add(bCenterTest); 

	  pCenterSource.setLayout(new BorderLayout());
      // Create a text area with some initial text
	  JTextArea txaCenterSource = new JTextArea("Initial Text");
	    
	  // Create a text area with some initial text and a default number of rows and columns.
	  // This number of rows and columns controls the preferred width and height of the component;
	  // each row and column is rougly the size of an M in the current font.
	  int rows = 20;
	  int cols = 30;
	  txaCenterSource = new JTextArea("Initial Text", rows, cols);
	  pCenterSource.add(txaCenterSource,BorderLayout.CENTER); 
	  
	  // Specify on which edge the tabs should appear
	  int location = JTabbedPane.BOTTOM; // or BOTTOM, LEFT, RIGHT

      // Set the text & background color for all tabs
	  pCenterTabbed.setForeground(Color.RED);
	  pCenterTabbed.setBackground(Color.WHITE);
	  // Add a tab
	  pCenterTabbed.addTab("Design", pCenterDesign);
	  pCenterTabbed.addTab("Test", pCenterTest);
	  pCenterTabbed.addTab("Source", pCenterSource);
		  
	}
	
	/*
	 * Definition toolbox palette with graphic object button
    */
	private void ToolboxObjectGraphicDefinition() {
		
 	   // Scan Buttons, set size and add to panel
       for (int i = 0; i < oa_btnToolBox.length; i++) {
    	   
    	   // Size
    	   oa_btnToolBox[i].setSize(new Dimension(BUTTON_TOOLBOX_X_DIM,BUTTON_TOOLBOX_Y_DIM)) ;                		    
    	   oa_btnToolBox[i].setMinimumSize(new Dimension(BUTTON_TOOLBOX_X_DIM,BUTTON_TOOLBOX_Y_DIM)) ;               	 
    	   oa_btnToolBox[i].setMaximumSize(new Dimension(BUTTON_TOOLBOX_X_DIM,BUTTON_TOOLBOX_Y_DIM)) ;               	 
    	   oa_btnToolBox[i].setPreferredSize(new Dimension(BUTTON_TOOLBOX_X_DIM,BUTTON_TOOLBOX_Y_DIM)) ;  
           
    	   // Tooltip
    	   oa_btnToolBox[i].setToolTipText(os_btnTooltip[i]);
    	   
		   pEastSplitToolbox.add(oa_btnToolBox[i]);
       }   
	}
	
    /**
     * populateGUI
     * 
     * 
     */
	public void populateGUI() {
	}

	/*
	 *  Set Constraints for Gridbaglayout
	 */
	private void BuildConstraints(GridBagConstraints gbc
								, int ColStart       // Start column
								, int RowStart       // Start Row
								, int ColsCovered    // Cols to cover
								, int RowsCovered    // Rows to cover
								, int ColSpacePerc    // Space % X axis
								, int RowSpacePerc    // Space % Y axis
								) 
	{
		gbc.gridx = ColStart;
		gbc.gridy = RowStart;
		gbc.gridwidth = ColsCovered;
		gbc.gridheight = RowsCovered;
		gbc.weightx = ColSpacePerc;
		gbc.weighty = RowSpacePerc;	
	}
	
	
	//--------------------------------------------------------------------
    // Events manager methods
	//--------------------------------------------------------------------

	public void actionPerformed(ActionEvent e) {
		System.out.println("actionPerformed");
		
	}
	public void adjustmentValueChanged(AdjustmentEvent e) {
		System.out.println("adjustmentValueChanged");
		
	}
	public void focusGained(FocusEvent e) {
		System.out.println("focusGained");
		
	}
	public void focusLost(FocusEvent e) {
		System.out.println("focusLost");
		
	}
	public void itemStateChanged(ItemEvent e) {
		System.out.println("itemStateChanged");
		
	}
	public void keyPressed(KeyEvent e) {
		System.out.println("keyPressed");
		
	}
	public void keyReleased(KeyEvent e) {
		System.out.println("keyReleased");
		
	}
	public void keyTyped(KeyEvent e) {
		System.out.println("keyTyped");
		
	}
	public void mouseClicked(MouseEvent e) {
		System.out.println("mouseClicked");
		
	}
	public void mouseEntered(MouseEvent e) {
		System.out.println("mouseEntered");
		
	}
	public void mouseExited(MouseEvent e) {
		System.out.println("mouseExited");
		
	}
	public void mousePressed(MouseEvent e) {
		System.out.println("mousePressed");
		
	}
	public void mouseReleased(MouseEvent e) {
		System.out.println("mouseReleased");
		
	}
	public void mouseDragged(MouseEvent e) {
		System.out.println("mouseDragged");
		
	}
	public void mouseMoved(MouseEvent e) {
		System.out.println("mouseMoved");
		
	}
	public void windowActivated(WindowEvent e) {
		System.out.println("windowActivated");
		
	}
	public void windowClosed(WindowEvent e) {
		System.out.println("windowClosed");
		
	}
	public void windowClosing(WindowEvent e) {
		System.out.println("windowClosing");
		
	}
	public void windowDeactivated(WindowEvent e) {
		System.out.println("windowDeactivated");
		
	}
	public void windowDeiconified(WindowEvent e) {
		System.out.println("windowDeiconified");
		
	}
	public void windowIconified(WindowEvent e) {
		System.out.println("windowIconified");
		
	}
	public void windowOpened(WindowEvent e) {
		System.out.println("windowOpened");
		
	}
	
}
