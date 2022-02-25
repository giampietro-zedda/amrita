/**
 * 
 */
package forward;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;

/**
 * ForwardIde
 * -----------------
 * 
 * This class implements a full featured IDE for develop
 * multiplatform applications.
 *  
 * This class is the main code to dispatch every specific
 * functionality, managed by an own specific class.
 * 
 * @see ForwardIdeGUI
 * @see ForwardIdeDataStrategy
 * @see ForwardIdeFunction
 * @see ForwardIdeTest
 * @see ForwardIdeDeploy
 * @see ForwardIdeTablesNoLegacy
 * @see ForwardIdeDataBaseUtilities
 * @see ForwardIdeAccesssecurity
 * @see ForwardIdeCustom
 * @see ForwardIdeDictionary
 * @author Giampietro Zedda 
 * @date 20-09-2008
 * 
 */
public class ForwardIde extends JFrame implements 	ActionListener,    // User action like click on button
													AdjustmentListener,
													FocusListener,
													ItemListener,
													KeyListener,
													MouseListener,
													MouseMotionListener,
													WindowListener
{
	
 
  private static final long serialVersionUID = 1L;
  public static final int BUTTON_TOOLBAR_X_DIM = 40;
  public static final int BUTTON_TOOLBAR_Y_DIM = 40;
  
  //------------------------------------------------------
  // Below are defined all panels used with main structure
  //------------------------------------------------------ 	
  private JPanel paMain = new JPanel();	                   //(0) Main (Border layout) 
  private JPanel paMainNorth = new JPanel();                //   (1) Main North   
  private JToolBar paToolBarFunction = new JToolBar();      //       (2) Toolbar (Flow Layout)   
  private JPanel paMainWest = new JPanel();                 //   (1) Main West 
  private JPanel paMainCenter = new JPanel();               //   (1) Main Center
  private JPanel paMainEast = new JPanel();                 //   (1) Main East
  private JPanel paStatusBar = new JPanel();                //   (1) Sud (GridBagLayout)

  //------------------------------------------------------
  // Below are defined ALL panels populated by card layout
  //------------------------------------------------------
  //West
  private JPanel  paWestGui = new JPanel();
  private JPanel  paWestDataStrategy = new JPanel();
  private JPanel  paWestMenu = new JPanel();
  private JPanel  paWestFunction = new JPanel();
  private JPanel  paWestTest = new JPanel();
  private JPanel  paWestDeploy = new JPanel();
  private JPanel  paWestTablesNoLegacy = new JPanel();
  private JPanel  paWestDataBaseUtilities = new JPanel();
  private JPanel  paWestAccessSecurity = new JPanel();
  private JPanel  paWestCustom = new JPanel();
  private JPanel  paWestDictionary = new JPanel();
  private JPanel  paWestImpactAnalysis = new JPanel();
  
  //East
  private JPanel  paEastGui = new JPanel();
  private JPanel  paEastDataStrategy = new JPanel();
  private JPanel  paEastMenu = new JPanel();
  private JPanel  paEastFunction = new JPanel();
  private JPanel  paEastTest = new JPanel();
  private JPanel  paEastDeploy = new JPanel();
  private JPanel  paEastTablesNoLegacy = new JPanel();
  private JPanel  paEastDataBaseUtilities = new JPanel();
  private JPanel  paEastAccessSecurity = new JPanel();
  private JPanel  paEastCustom = new JPanel();
  private JPanel  paEastDictionary = new JPanel();
  private JPanel  paEastImpactAnalysis = new JPanel();

  //Center
  private JPanel  paCenterGui = new JPanel();
  private JPanel  paCenterDataStrategy = new JPanel();
  private JPanel  paCenterMenu = new JPanel();
  private JPanel  paCenterFunction = new JPanel();
  private JPanel  paCenterTest = new JPanel();
  private JPanel  paCenterDeploy = new JPanel();
  private JPanel  paCenterTablesNoLegacy = new JPanel();
  private JPanel  paCenterDataBaseUtilities = new JPanel();
  private JPanel  paCenterAccessSecurity = new JPanel();
  private JPanel  paCenterCustom = new JPanel();
  private JPanel  paCenterDictionary = new JPanel();
  private JPanel  paCenterImpactAnalysis = new JPanel();

  
  //-------------------------------------------------
  // Definition Forward Modeler buttons in main toolbar 
  //-------------------------------------------------
  private JButton  btnModelerGui = new JButton("Gui");
  private JButton  btnModelerDataStrategy = new JButton("Data");
  private JButton  btnModelerMenu = new JButton("Menu");
  private JButton  btnModelerFunction = new JButton("Func");
  private JButton  btnModelerTest = new JButton("Test");
  private JButton  btnModelerDeploy = new JButton("Deploy");
  private JButton  btnModelerTablesNoLegacy = new JButton("Tables");
  private JButton  btnModelerDataBaseUtilities = new JButton("Db Util");
  private JButton  btnModelerAccessSecurity = new JButton("Secu");
  private JButton  btnModelerCustom = new JButton("Custom");
  private JButton  btnModelerDictionary = new JButton("Dict");
  private JButton  btnModelerImpact = new JButton("Impact");

  //-------------------------------------------------
  // Array of JButton Tooltip
  //-------------------------------------------------
  private String[] sa_ButtonTooltip = {
										  "GUI Modeler"
										 ,"Data Access Strategy Modeler"
										 ,"Menu Modeler"
										 ,"Application Function Modeler"
										 ,"Test And Regression Test Modeler"
										 ,"Deploy Application And Components Modeler"
										 ,"Tables no Legacy Modeler"
										 ,"Data Base Utilities"
										 ,"Access And Security Modeler"
										 ,"Custom Modeler"
										 ,"Dictionary Browsing And Modeler"
										 ,"Impact Analysis & Where Used"
  									  };
  
  //-------------------------------------------------
  // Definition of buttons as temporary placeholder
  //-------------------------------------------------
  private JButton  bCenter = new JButton("Center");
  private JButton  bEast = new JButton("East");
  private JButton  bWest = new JButton("West");  
  private JButton  bStatusBar = new JButton("S");  
  
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
  private Border lineBorder = BorderFactory.createLineBorder(Color.red);
  private Border etchedBorder = BorderFactory.createEtchedBorder();
  private Border compoundBorder = BorderFactory.createCompoundBorder(lineBorder,emptyBorder);
  private Border bevelRaisedBorder = BorderFactory.createBevelBorder(BevelBorder.RAISED,fg,bg);
  private Border bevelLoweredBorder = BorderFactory.createBevelBorder(BevelBorder.LOWERED,fg,bg);
  
  
  //-------------------------------------------------
  // Title Borders
  //-------------------------------------------------
//  private String pMainCenterMainTitle = "Pannello di gestione dati";
//  private TitledBorder titleBorderMainCenter = BorderFactory.createTitledBorder(lineBorder, pMainCenterMainTitle);
  
  //-------------------------------------------------
  // Status Bar Labels & TextFields
  //-------------------------------------------------
  private JLabel lbUser = new JLabel();
  private JLabel lbApplCode = new JLabel();  
  private JLabel lbFuncCode = new JLabel();
  private JLabel lbForm = new JLabel(); 
  private JLabel lbPanel = new JLabel(); 
  private JLabel lbIPAddr = new JLabel();
  private JLabel lbDate = new JLabel(); 
  private JLabel lbTime = new JLabel(); 
  private JTextField tfUser = new JTextField();
  private JTextField tfApplCode = new JTextField();  
  private JTextField tfFuncCode = new JTextField();
  private JTextField tfForm = new JTextField(); 
  private JTextField tfPanel = new JTextField();
  private JTextField tfFiller = new JTextField();
  private JTextField tfIPAddr = new JTextField();
  private JTextField tfDate = new JTextField(); 
  private JTextField tfTime = new JTextField(); 

  //-------------------------------------------------
  // GridBagLayout & Constraints for StatusBar
  //-------------------------------------------------
  private GridBagLayout gblStatusBar = new GridBagLayout();
  private GridBagConstraints constraintStatusBar = new GridBagConstraints();
  
  //-------------------------------------------------
  // Array of managed functions (activated by main toolbar)
  //-------------------------------------------------
  private Object[] oa_Function = {
									  "GUI" 
									 ,"DataStrategy" 
									 ,"Menu" 
									 ,"Function" 
									 ,"Test" 
									 ,"Deploy" 
									 ,"TablesNoLegacy" 
									 ,"DataBaseUtilities" 
									 ,"AccessSecurity" 
									 ,"Custom" 
									 ,"Dictionary"
									 ,"ImpactAnalysis"
						  			};

  //-------------------------------------------------
  // Array of Function panels cards for west side 
  //-------------------------------------------------
  private JPanel[] oa_WestPanel = {
									  paWestGui
									 ,paWestDataStrategy 
									 ,paWestMenu 
									 ,paWestFunction 
									 ,paWestTest 
									 ,paWestDeploy 
									 ,paWestTablesNoLegacy 
									 ,paWestDataBaseUtilities 
									 ,paWestAccessSecurity 
									 ,paWestCustom 
									 ,paWestDictionary 
									 ,paWestImpactAnalysis
							};

  //-------------------------------------------------
  // Array of function panels cards for east side 
  //-------------------------------------------------
  private JPanel[] oa_EastPanel = {
									  paEastGui
									 ,paEastDataStrategy 
									 ,paEastMenu 
									 ,paEastFunction 
									 ,paEastTest 
									 ,paEastDeploy 
									 ,paEastTablesNoLegacy 
									 ,paEastDataBaseUtilities 
									 ,paEastAccessSecurity 
									 ,paEastCustom 
									 ,paEastDictionary 
									 ,paEastImpactAnalysis
							};

  //-------------------------------------------------
  // Array of function panels cards for center side 
  //-------------------------------------------------
  private JPanel[] oa_CenterPanel = {
									  paCenterGui
									 ,paCenterDataStrategy 
									 ,paCenterMenu 
									 ,paCenterFunction 
									 ,paCenterTest 
									 ,paCenterDeploy 
									 ,paCenterTablesNoLegacy 
									 ,paCenterDataBaseUtilities 
									 ,paCenterAccessSecurity 
									 ,paCenterCustom 
									 ,paCenterDictionary 
									 ,paCenterImpactAnalysis
							};
  
  //-------------------------------------------------
  // Array of toolbar buttons
  //-------------------------------------------------
  private JButton[] oa_BtnToolbar = {
									   btnModelerGui
									  ,btnModelerDataStrategy
									  ,btnModelerMenu
									  ,btnModelerFunction
									  ,btnModelerTest
									  ,btnModelerDeploy
									  ,btnModelerTablesNoLegacy
									  ,btnModelerDataBaseUtilities 
									  ,btnModelerAccessSecurity
									  ,btnModelerCustom
									  ,btnModelerDictionary 
									  ,btnModelerImpact 
  									};
  
  //-------------------------------------------------
  // Card layout definitions
  //-------------------------------------------------
  private CardLayout clWest;
  private CardLayout clEast;
  private CardLayout clCenter;
  
  	/**
	 * ForwardIde 
	 * 
	 * Constructor. It will be prepared the skeleton of  
	 * forms and panels layout
	 * 
	 * 
	 */
	public ForwardIde() {
		
      // Main window size and title		
      super("Interactive GUI Builder");
	  setSize(1024,768);
	  paMain.setLayout(new BorderLayout());  // Main panel layout
	  
      //-------------------------------------------------
	  // Prepare North of Main panel (ToolBar)
      //-------------------------------------------------
	  paMainNorth.setLayout(new FlowLayout(FlowLayout.LEFT));  
 	  paMainNorth.setBorder(etchedBorder);        
      toolBarFunctionPopulate();
 	  paMainNorth.add(paToolBarFunction);        // Add Toolbar to component

      //-------------------------------------------------
	  // Prepare South of Main panel (StatusBar)
      //-------------------------------------------------
 	  paStatusBar.setLayout(gblStatusBar);  
	  paStatusBar.setBorder(lineBorder);       
	  paStatusBar.setBorder(bevelRaisedBorder); 
	  paStatusBar.setBorder(bevelLoweredBorder);	  
	  paStatusBar.setBorder(etchedBorder);
      statusBarPopulate();	  

      //-------------------------------------------------
	  // Prepare specific function layout
      //-------------------------------------------------
      
      // GUI Functionality
	  ForwardIdeGUI ideGui = new ForwardIdeGUI(paWestGui, paCenterGui, paEastGui);  // GUI Manager
	  ideGui.defineGUI();

	  
	  
      //-------------------------------------------------
	  // Load card layout panels 
      //-------------------------------------------------
      clWest = new CardLayout();
      clEast = new CardLayout();
      clCenter = new CardLayout();
      
      paMainWest.setLayout(clWest);
      paMainEast.setLayout(clEast);
      paMainCenter.setLayout(clCenter);
      
      // Add cards for each function
      for (int i = 0; i < oa_WestPanel.length; i++) {
    	  paMainWest.add(oa_WestPanel[i], oa_Function[i]);
    	  paMainEast.add(oa_EastPanel[i], oa_Function[i]);
    	  paMainCenter.add(oa_CenterPanel[i], oa_Function[i]);
	  }
      
      //-------------------------------------------------
 	  // Add card layout prepared panels to main panel
      //-------------------------------------------------
	  paMain.add("North", paMainNorth);         // N.B. Using a panel as container, toolbar is floating  
 	  paMain.add("West", paMainWest);
   	  paMain.add("Center", paMainCenter);
 	  paMain.add("East", paMainEast);	
	  paMain.add("South", paStatusBar);
      
	  
      //-------------------------------------------------
 	  // Show GUI panels
      //-------------------------------------------------
	  clWest = (CardLayout)(paMainWest.getLayout());
	  clWest.show(paMainWest, "GUI");
	  clEast = (CardLayout)(paMainEast.getLayout());
	  clEast.show(paMainEast, "GUI");
	  clCenter = (CardLayout)(paMainCenter.getLayout());
	  clCenter.show(paMainCenter, "GUI");

	  
	  //-------------------------------------------------
	  // Final operations, show form
      //-------------------------------------------------
	  setContentPane(paMain);
	  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	  setVisible(true); 
	}
    

	/*
     * Add buttons and other to the main toolbar
     */
	private void toolBarFunctionPopulate() {
	  
	  // Scan Buttons, set size and add to toolbar  
	  for (int i = 0; i < oa_BtnToolbar.length; i++) {
		
		// Size
		oa_BtnToolbar[i].setSize(new Dimension(BUTTON_TOOLBAR_X_DIM,BUTTON_TOOLBAR_Y_DIM)) ;                		    
		oa_BtnToolbar[i].setMinimumSize(new Dimension(BUTTON_TOOLBAR_X_DIM,BUTTON_TOOLBAR_Y_DIM)) ;               	 
		oa_BtnToolbar[i].setMaximumSize(new Dimension(BUTTON_TOOLBAR_X_DIM,BUTTON_TOOLBAR_Y_DIM)) ;               	 
		oa_BtnToolbar[i].setPreferredSize(new Dimension(BUTTON_TOOLBAR_X_DIM,BUTTON_TOOLBAR_Y_DIM)) ;
		// Tooltip
		oa_BtnToolbar[i].setToolTipText(sa_ButtonTooltip[i]);
		// Listener
		oa_BtnToolbar[i].addActionListener(this);
		
		paToolBarFunction.add(oa_BtnToolbar[i]);
	  }
	  paToolBarFunction.setFloatable(false);
	}
	
	/*
	 * Populate statusBar with GridBagLayout
	 */
    private void statusBarPopulate() {
    	
  	  // Define Objects to insert
  	  setStatusbarField(lbUser, "User:", tfUser, "MASTER");
  	  setStatusbarField(lbApplCode, "Appl:", tfApplCode, "System");
  	  setStatusbarField(lbFuncCode, "Func:", tfFuncCode, "GUIB");
  	  setStatusbarField(lbForm, "Form:", tfForm, "Test");
  	  setStatusbarField(lbPanel, "Panel:", tfPanel, "panel1");
  	  setStatusbarField(lbIPAddr, "IP Addr.:", tfIPAddr, "localhost");
  	  setStatusbarField(lbDate, "Date:", tfDate, getCurrentDate());
  	  setStatusbarField(lbTime, "Time:", tfTime, getCurrentTime());
  	  
  	  // Insert user in status bar
  	  BuildConstraints(constraintStatusBar, 0, 0, 1, 1, 5, 100);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbUser, constraintStatusBar);
  	  paStatusBar.add(lbUser);
  	  BuildConstraints(constraintStatusBar, 1, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfUser, constraintStatusBar);
  	  paStatusBar.add(tfUser);
        
  	  // Insert applCode in status bar
  	  BuildConstraints(constraintStatusBar, 2, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbApplCode, constraintStatusBar);
  	  paStatusBar.add(lbApplCode);
  	  BuildConstraints(constraintStatusBar, 3, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfApplCode, constraintStatusBar);
  	  paStatusBar.add(tfApplCode);

  	  // Insert funcCode in status bar
  	  BuildConstraints(constraintStatusBar, 4, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbFuncCode, constraintStatusBar);
  	  paStatusBar.add(lbFuncCode);
  	  BuildConstraints(constraintStatusBar, 5, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfFuncCode, constraintStatusBar);
  	  paStatusBar.add(tfFuncCode);

  	  // Insert form in status bar
  	  BuildConstraints(constraintStatusBar, 6, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbForm, constraintStatusBar);
  	  paStatusBar.add(lbForm);
  	  BuildConstraints(constraintStatusBar, 7, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfForm, constraintStatusBar);
  	  paStatusBar.add(tfForm);

  	  // Insert panel in status bar
  	  BuildConstraints(constraintStatusBar, 8, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbPanel, constraintStatusBar);
  	  paStatusBar.add(lbPanel);
  	  BuildConstraints(constraintStatusBar, 9, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfPanel, constraintStatusBar);
  	  paStatusBar.add(tfPanel);

  	  
  	  // Insert central filler in status bar
  	  BuildConstraints(constraintStatusBar, 10, 0, 1, 1, 20, 0);
  	  constraintStatusBar.fill = GridBagConstraints.HORIZONTAL;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfFiller, constraintStatusBar);
  	  paStatusBar.add(tfFiller);
  	  

  	  // Insert IPAddress in status bar
  	  BuildConstraints(constraintStatusBar, 11, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbIPAddr, constraintStatusBar);
  	  paStatusBar.add(lbIPAddr);
  	  BuildConstraints(constraintStatusBar, 12, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfIPAddr, constraintStatusBar);
  	  paStatusBar.add(tfIPAddr);
  	  
  	  // Insert date in status bar
  	  BuildConstraints(constraintStatusBar, 13, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbDate, constraintStatusBar);
  	  paStatusBar.add(lbDate);
  	  BuildConstraints(constraintStatusBar, 14, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfDate, constraintStatusBar);
  	  paStatusBar.add(tfDate);
  	  
  	  // Insert time in status bar
  	  BuildConstraints(constraintStatusBar, 15, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(lbTime, constraintStatusBar);
  	  paStatusBar.add(lbTime);
  	  BuildConstraints(constraintStatusBar, 16, 0, 1, 1, 5, 0);
  	  constraintStatusBar.fill = GridBagConstraints.BOTH;
  	  constraintStatusBar.anchor = GridBagConstraints.WEST;
  	  gblStatusBar.setConstraints(tfTime, constraintStatusBar);
  	  paStatusBar.add(tfTime);
		
	}

	/*
    *  Set label & text for toolbar field
    */
	private void setStatusbarField(JLabel lb
								,String sLbl
								,JTextField tf
								,String sTf) {

		  lb.setBorder(etchedBorder);
		  lb.setText(sLbl);
		  tf.setText(sTf);
		  tf.setEditable(false);

		  
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
	
	
	/*
     * Get current hour in the format HH:MM:SS AM/PM
    */	
	private String getCurrentTime() {
    	Calendar cal = new GregorianCalendar();
        
        // Get the components of the time
        int hour12 = cal.get(Calendar.HOUR);            // 0..11
        int hour24 = cal.get(Calendar.HOUR_OF_DAY);     // 0..23
        int min = cal.get(Calendar.MINUTE);             // 0..59
        int sec = cal.get(Calendar.SECOND);             // 0..59
//      int ms = cal.get(Calendar.MILLISECOND);         // 0..999
        int ampm = cal.get(Calendar.AM_PM);             // 0=AM, 1=PM
		String sHour24 = "0"+hour24;
		String sMin = "0"+min;
		String sSec = "0"+sec; 
		sHour24 = sHour24.substring(sHour24.length()-2);
		sMin = sMin.substring(sMin.length()-2);
		sSec = sSec.substring(sSec.length()-2);
		String sAM_PM = "AM";
		if (ampm == 1) {sAM_PM = "PM";}
        return sHour24+":"+sMin+":"+sSec+" "+sAM_PM;
	}
	
	/*
     * Get current data in the format AAAAMMGG
    */
	private String getCurrentDate() {
		
		Calendar cal = new GregorianCalendar();
	    
	    // Get the components of the date
	    int year = cal.get(Calendar.YEAR);             // 2002
	    int month = cal.get(Calendar.MONTH);           // 0=Jan, 1=Feb, ...
	    int day = cal.get(Calendar.DAY_OF_MONTH);      // 1..
		String sMonth = "0"+month;
		String sDay = "0"+day;
		sMonth = sMonth.substring(sMonth.length()-2);
		sDay = sDay.substring(sDay.length()-2);
	    return year+sMonth+sDay;
	}

	/**
	 * @param args
	 */
	public static void main(final String[] args) {
		JFrame frame = new ForwardIde();
//		frame.pack();
        frame.setVisible(true);
	}

	
	//--------------------------------------------------------------------
    // Events manager methods
	//--------------------------------------------------------------------

 	public void actionPerformed(ActionEvent e) {
		Object src = e.getSource();
		
		// Test if selected function by click on toolbar
		if (src instanceof JButton) {
			// Scan toolbar buttons
			for (int i = 0; i < oa_BtnToolbar.length; i++) {
				// Show cards of selected function
				if (src == oa_BtnToolbar[i]) {
					clWest = (CardLayout)(paMainWest.getLayout());
					clWest.show(paMainWest, (String)oa_Function[i]);
					clEast = (CardLayout)(paMainEast.getLayout());
					clEast.show(paMainEast, (String)oa_Function[i]);
					clCenter = (CardLayout)(paMainCenter.getLayout());
					clCenter.show(paMainCenter, (String)oa_Function[i]);
					return;
				}
			}
		}
		
 	}

 	  
 	
	public void adjustmentValueChanged(AdjustmentEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void focusGained(FocusEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void focusLost(FocusEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void itemStateChanged(ItemEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void keyPressed(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void keyReleased(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseDragged(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void mouseMoved(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowActivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowClosed(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowClosing(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowDeactivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowDeiconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowIconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	public void windowOpened(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	

	
}




