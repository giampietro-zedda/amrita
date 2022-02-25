package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

import javax.swing.Box;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeSelectionModel;

import enums.EnumForwardBorderType;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumForwardScope;
import enums.EnumLanguage;
import enums.EnumMetricsScope;
import enums.EnumObject;
import enums.EnumTable;
import forward.ForwardFunction;
import forward.ForwardSystem; 
//import guiSunExamples.ColorEditor; 
//import guiSunExamples.ColorRenderer;  

public class ForwardTest extends ForwardFunction {
	
	boolean insertInprogress = false;
	
    /* Costruttore vuoto */
	public ForwardTest() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
    @SuppressWarnings("rawtypes")
	public void declare() {
       
    	/** Proprietà di default per tipo componente in deroga a quelle standard */
//  	COMPONENT_PROPERTY_DEFAULT(EnumForwardComponent.JPanel, "Size", Dimension.class, new Dimension(500, 400));
        
  		/* Variabili elementari con scope a livello funzione */
 		VAR(EnumForwardScope.SCOPE_FUNCTION, "testVarNum", new Integer(5));							// Numeric var
 		VAR(EnumForwardScope.SCOPE_FUNCTION, "testVarText", "");									// Text var
 		VAR(EnumForwardScope.SCOPE_FUNCTION, "metricsQualSectionObjBound", new Integer(0));			// Oggetto bound, numero selezione di enumMetricsScope
 		VAR(EnumForwardScope.SCOPE_FUNCTION, "metricsScope", new Integer(0));						// Scope metrica correntemente selezionato
 		VAR(EnumForwardScope.SCOPE_FUNCTION, "metricsTypeObject", new Integer(0));					// Tipo oggetto in key
		VAR(EnumForwardScope.SCOPE_FUNCTION, "typeObject", new Integer(0));						    // Tipo oggetto per gestione CRUD Object
		VAR(EnumForwardScope.SCOPE_FUNCTION, "insertInProgress", new Boolean(false));				// Insert CRUD in corso
		VAR(EnumForwardScope.SCOPE_FUNCTION, "typeSourceX", new String());							// Tipo sorgente come stringa
  		
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("AmritaSuite", "e-Amrita suite", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ITALIAN);
     	
        BEGIN_FORM("FormAssesment", EnumForwardOption.FORM_TYPE_START, "mnMenuBar"); 

	        // Level 0, container principale
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paMnMainNorth", "paMainCenter", "", "paMainEast", "paMainWest");
     	     		
     	        // Level 1 (menu generale suite in alto)
     	        PANELS_STRUCTURE("paMnMainNorth", EnumForwardPanelType.MENU, "mnSuiteAmrita");
  				
  	   	        // Level 1 (menu specifici moduli suite a sinistra) attivati da paMnMainNorth attraverso mnSuiteAmrita
  	     		PANELS_STRUCTURE("paMainWest", EnumForwardPanelType.CONTAINER, EnumForwardLayout.CARD_LAYOUT, "paMnAssesment", "paMnViewer", "paMnInspector", "paMnDoc", "paMnDataLinage", "paMnImpact", "paMnTest");
	      			PANELS_STRUCTURE("paMnAssesment", EnumForwardPanelType.MENU, "mnAssesment");
	     			PANELS_STRUCTURE("paMnViewer", EnumForwardPanelType.MENU, "mnViewer");
	     			PANELS_STRUCTURE("paMnInspector", EnumForwardPanelType.MENU, "mnInspector");
	     			PANELS_STRUCTURE("paMnDoc", EnumForwardPanelType.MENU, "mnDoc");
	     			PANELS_STRUCTURE("paMnDataLinage", EnumForwardPanelType.MENU, "mnDataLinage");
	     			PANELS_STRUCTURE("paMnImpact", EnumForwardPanelType.MENU, "mnImpact");
	     			PANELS_STRUCTURE("paMnTest", EnumForwardPanelType.MENU, "mnTest");
    	        
	 	   	    // Level 1 (pannello centrale con le schede dei moduli di amrita) attivate dai menu di modulo specifici a sinistra
 	     		PANELS_STRUCTURE("paMainCenter", EnumForwardPanelType.CONTAINER, EnumForwardLayout.CARD_LAYOUT, "paCardAssesmentConf", "paCardAssesmentSourcesAcquiring", "paCardAssesmentSourceAcquired", "paCardAssesmentPostAnalysisPgm", "paCardAssesmentPostAnalysisFunction", "paCardAssesmentMetricsDashBoard", "paCardObjectCRUD", "paCardTestTables");
 	     		    PANELS_STRUCTURE("paCardAssesmentConf", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
 	     			PANELS_STRUCTURE("paCardAssesmentSourcesAcquiring", EnumForwardPanelType.TABBED, "paRunProperties", "paTableSample", "paTreeSample", "gridSample", "dialogSample");
		     		    PANELS_STRUCTURE("paRunProperties", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	     			PANELS_STRUCTURE("paTableSample",EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	     			PANELS_STRUCTURE("paTreeSample", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	     			PANELS_STRUCTURE("gridSample", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	     			PANELS_STRUCTURE("dialogSample", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     		PANELS_STRUCTURE("paCardAssesmentMetricsDashBoard", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_VERTICAL, "paMetricsPilot", "paMetricsCard", "paMetricsViolationsService");
	 	     		 	PANELS_STRUCTURE("paMetricsPilot", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     			PANELS_STRUCTURE("paMetricsCard", EnumForwardPanelType.CONTAINER, EnumForwardLayout.CARD_LAYOUT, "paMetricsEmpty", "paMetricsCnt", "paMetricsSize", "paMetricsTime", "paMetricsDef", "paMetricsDoc", "paMetricsDyn", "paMetricsDeadCode");
	 	     				PANELS_STRUCTURE("paMetricsEmpty", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsCnt", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsSize", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsTime", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsDef", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsDoc", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsDyn", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     				PANELS_STRUCTURE("paMetricsDeadCode", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 	     		 	PANELS_STRUCTURE("paMetricsViolationsService", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_HORIZONTAL, "paMetricsViolations", "paMetricsOrigin");
	 			 	     	PANELS_STRUCTURE("paMetricsViolations", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 			 	     	PANELS_STRUCTURE("paMetricsOrigin", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 		 	    PANELS_STRUCTURE("paCardObjectCRUD", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_VERTICAL, "paObjectKeys", "paObjectData", "paObjectActions");
 		 	    		PANELS_STRUCTURE("paObjectKeys", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
 		 	    		PANELS_STRUCTURE("paObjectData", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
 		 	    		PANELS_STRUCTURE("paObjectActions", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
 		 		 	PANELS_STRUCTURE("paCardTestTables", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	 			// Level 1 (pannello di help a livello di modulo)
	     		PANELS_STRUCTURE("paMainEast", EnumForwardPanelType.HELP_HTML);
     	     		
        END_FORM();
 
        /* Form User Dialog */
        BEGIN_FORM("FormDialogUserModal", EnumForwardOption.FORM_TYPE_DIALOG); 
  	    	PANELS_STRUCTURE("paDialogUserModal", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

        /* Form User Dialog */
        BEGIN_FORM("FormDialogUserNoModal", EnumForwardOption.FORM_TYPE_DIALOG); 
  	    	PANELS_STRUCTURE("paDialogUserNoModal", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

         /* Opzioni applicative/caratteristiche pannelli */
       	PANEL_OPTIONS("paMnMainNorth", EnumForwardOption.MENU_HEIGH_MEDIUM);
       	PANEL_OPTIONS("paMnAssesment", EnumForwardOption.MENU_HEIGH_LARGE);
        
       	/* Componenti da NON disporre nei pannelli utilizzati nel panel */
        COMPONENT(new JLabel(), "lb_modelSlider", 0, "        ");  	// Modello per label di JSlider
      	getJLabel("lb_modelSlider").setForeground(Color.RED);
      	getJLabel("lb_modelSlider").setFont(getJLabel("lb_modelSlider").getFont().deriveFont(Font.BOLD));
      	getJLabel("lb_modelSlider").setForeground(Color.BLUE);

       	/* Contenuto pannello dialog user */
       	BEGIN_PANEL_DETAIL_CONTENT("paDialogUserModal", "Dialog User Moadal");
       		COMPONENT(new JCheckBox(), "cx_dialogUser1", 0, "Test", true);
       		COMPONENT(new JButton(), "bt_dialogModalClose", 1, "Close Dialog", 150); 
       	END_PANEL_DETAIL_CONTENT();
       	
       	/* Contenuto pannello dialog user */
       	BEGIN_PANEL_DETAIL_CONTENT("paDialogUserNoModal", "Dialog User No Moadal");
       		COMPONENT(new JCheckBox(), "cx_dialogUser2", 0, "Test", true);
       		COMPONENT(new JButton(), "bt_dialogNoModalClose", 1, "Close Dialog", 150); 
       	END_PANEL_DETAIL_CONTENT();
       	
       	
       	/* Contenuto pannello proprietà di lancio acquisizione sorgenti */
       	BEGIN_PANEL_DETAIL_CONTENT("paCardAssesmentConf", "Configuration");
       	  
	       	/* Row 0 */  COMPONENT(new JCheckBox(), "selectSlider", 0, "Slider selection", true);
	    	/* Row 1 */  COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), "glue04", EnumForwardComponent.JBoxRigidArea, 1, 10, 20);  
	    	/* Row 2 */  COMPONENT(new JSlider(), "sl_slider01", 2, 10, 100, 0);  
	    	/* Row 3 */  COMPONENT(new JSlider(), "sl_slider02", 3, JSlider.VERTICAL, 10, 100, 70);  
					 	 COMPONENT(new JSlider(), "sl_slider03", 3, JSlider.VERTICAL, 10, 100, 50, 30, 10, false, true, false);  
					  	 COMPONENT(new JSlider(), "sl_slider04", 3, JSlider.VERTICAL, 10, 100, 30, 30, 10, true,  true, true);  
					 	 COMPONENT(new JSlider(), "sl_slider05", 3, JSlider.VERTICAL, 10, 100, 10, 30, 10, false, false,  
					  	           new int[] {15, 72, 100},  new String[] {"Low", "Medium", "High"}, "lb_modelSlider"
							      );  
		                 COMPONENT(new JTextField(), "tf_tickValue", 3);
		    /* Row 4 */	 COMPONENT(new JSpinner(), "sp_spinnerText", 4, null, true, "Label3", "Label1", "Label2", "Label3", "Label4");			// Labels
		    /* Row 5 */	 COMPONENT(new JSpinner(), "sp_spinnerNum", 5, null, "#", 176, 0, 10000, 500);								// Numeri senza migliaia
		    
					     /* Java date setting */
					     Calendar calendar = Calendar.getInstance();
				         Date initDate = calendar.getTime();
				         calendar.add(Calendar.YEAR, -5);
				         Date earliestDate = calendar.getTime();
				         calendar.add(Calendar.YEAR, 10);
				         Date latestDate = calendar.getTime();
		    /* Row 6 */	 COMPONENT(new JSpinner(), "sp_spinnerDate", 6, null, "MM/yyyy", initDate, earliestDate, latestDate);	// Date
		    /* Row 7 */	 COMPONENT(new JTextField(), "tf_spinnerValue", 7);													// Valore selezionato

	   		/* Row 9 */  COMPONENT(new JButton(), "bt_default01", 9, "Default"); 

		END_PANEL_DETAIL_CONTENT();
	    	
	       	
       	/* Contenuto pannello proprietà di lancio acquisizione sorgenti */
       	BEGIN_PANEL_DETAIL_CONTENT("paRunProperties", "Run Properties", "mnPopUpAssesment");
       	  
	       	/* Row 0 */  COMPONENT(new JCheckBox(), "detectSourceType", 0,  "Detect Source Type", true);  
	       				 COMPONENT(new JCheckBox(), "detectFullInfoSource", 0, "Detect Full Info Source");
      		             COMPONENT(new JCheckBox(), "searchFileRecursive", 0, " Search File Recursive");
	       	/* Row 1 */  COMPONENT(new JRadioButton(), "rb_choice1", 1, "Choice 1", true, "GRPRB");
			 			 COMPONENT(new JRadioButton(), "rb_choice2", 1, "Choice 2", false, "GRPRB");
			 			 COMPONENT(new JRadioButton(), "rb_choice3", 1, "Choice 3", false, "GRPRB");
//	      		/* Row 2 */  COMPONENT(new JSeparator(), "sp_01", 2, SwingConstants.HORIZONTAL); 
      		/* Row 3 */  COMPONENT(new JLabel(), "lb_maxFilesInput", 3, "Max Files Input"); 		
     		  			 COMPONENT(new JFormattedTextField(), "maxFilesInput", 3, new Integer(200), 30);
     		  			 COMPONENT(new JLabel(), "lb_dateMax", 3, "Max date to process"); 	
     		  			 COMPONENT(new JFormattedTextField(), "maxDateToProcess", 3, new Date());
//	      		/* Row 4 */  COMPONENT(new JSeparator(), "sp_02", 4, SwingConstants.HORIZONTAL);   
       		/* Row 5 */  COMPONENT(new JLabel(), "lb_customerCode", 5, "Customer Code"); 		
       		  			 COMPONENT(new JTextField(), "customerCode", 5, "CD-02");
      		             COMPONENT(new JLabel(), "lb_customerInfo", 5, "Customer info"); 		
       		  			 COMPONENT(new JTextField(), "customerInfo", 5);
       		/* Row 6  */ COMPONENT(new JLabel(), "lb_userExitPath", 6,"User Exit Path");		
       		  			 COMPONENT(new JTextField(), "userExitPath", 6);
       		  			 COMPONENT(new JButton(), "bt_selPath", 6, "Sel Path."); 
       		  			 COMPONENT(new JButton(), "bt_selColor", 6, "Sel. Color");  
       		  			 COMPONENT(new JButton(), "bt_excp", 6, "Excp"); 
       		/* Row 7  */ COMPONENT(new JLabel(), "lb_timer", 7,  "Timer swing 3s");		 			 
			      		 COMPONENT(new JTextField(), "timerInfoStart", 7);
			      		 COMPONENT(new JTextField(), "timerInfoEnd", 7);
			      		 COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), "glue01", EnumForwardComponent.JBoxRigidArea, 7, 10, 20); 
			       		 COMPONENT(new JButton(), "bt_startTimer", 7, "Start Timer");
	       	/* Row 8  */ COMPONENT(new JTextArea(), "ta_excp", 8, 200, 150, 500, 100, true, "Defaul value\nDefault value");	
       		/* Row 9  */ COMPONENT(new JComboBox(), "cb_enumeration", 9);	
						 COMPONENT(new JTextField(), "tf_cbItemNumber", 9);
						 COMPONENT(new JTextField(), "tf_cbItem", 9);
	   					 COMPONENT(new JTextField(), "tf_cbItemBound", 9);
       		/* Row 10 */ COMPONENT(new JList(), "li_enumeration", 10, ListSelectionModel.SINGLE_SELECTION, 10, 150, 70, true);
						 COMPONENT(new JTextField(), "tf_liItemNumber", 10);
						 COMPONENT(new JTextField(), "tf_liItem", 10);
	   					 COMPONENT(new JTextField(), "tf_liItemBound", 	10);
//		   		/* Row 11 */ COMPONENT(new JSeparator(), "sp_03", 				11, SwingConstants.HORIZONTAL); 
	   		/* Row 12 */ COMPONENT(new JProgressBar(), "pb_test", 			12, SwingConstants.HORIZONTAL, 0, 100, true);
	  	    /* Row 13 */ COMPONENT(new JProgressBar(), "pb_test", 			13, SwingConstants.HORIZONTAL, 0, 100, true);
		         		 COMPONENT(new JButton(), "bt_startProgressBar", 	13, "Start Progress", 150); 
		    /* Row 14 */ COMPONENT(new JButton(), "bt_default02", 			14, "Default"); 
		    /* Row 15 */ COMPONENT(new JToggleButton(), "bt_toggle01",	 	15, "Toggle", true); 
        END_PANEL_DETAIL_CONTENT();

       	/* Contenuto pannello proprietà di lancio acquisizione sorgenti */
       	BEGIN_PANEL_DETAIL_CONTENT("gridSample", "Panel Grid Sample", "mnPopUpAssesment");
       	
       		  COMPONENT(new JLabel(), "lb_panelAsComponent1", 	0, "A Flow layout panel "); 		
	  	      COMPONENT(new JPanel(), "panelAsComponent1", 		1);
	  	      COMPONENT(Box.class, Box.createVerticalStrut(5), "glue01_01", EnumForwardComponent.JBoxRigidArea, 2, 10, 5);
      		  COMPONENT(new JLabel(), "lb_panelAsComponent2", 	3,  "A grid layout panel 7 rows 0 columns", 300); 		
	  	      COMPONENT(new JPanel(), "panelAsComponent2", 		4);
	  	      COMPONENT(Box.class, Box.createVerticalStrut(5), "glue01_01", EnumForwardComponent.JBoxRigidArea, 5, 10, 5);
      		  COMPONENT(new JLabel(), "lb_panelAsComponent3", 	6, "A grid layout panel 4 rows 2 columns", 300); 		
	  	      COMPONENT(new JPanel(), "panelAsComponent3", 		7);
	  	      COMPONENT(Box.class, Box.createVerticalStrut(5), "glue01_01", EnumForwardComponent.JBoxRigidArea, 8, 10, 5);
      		  COMPONENT(new JLabel(), "lb_panelAsComponent5", 	9, "Two grid panel components on the same row", 300); 		
	  	      COMPONENT(new JPanel(), "panelAsComponent51", 	10);
	  	      COMPONENT(new JPanel(), "panelAsComponent52", 	10);
	  	      
       	END_PANEL_DETAIL_CONTENT();
       	
       	/* Panel sample for dialogs */
       	BEGIN_PANEL_DETAIL_CONTENT("dialogSample", "Panel Dialog Sample");
            COMPONENT(Box.class, Box.createVerticalStrut(5), "glue66_01", EnumForwardComponent.JBoxRigidArea, 0, 10, 10);
	 	  	COMPONENT(new JButton(), "bt_dialogPlainMessage", 			1, "Start Dialog Plain Message", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogInformation", 			2, "Start Dialog Information", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogWarning", 				3, "Start Dialog Warning", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogError", 					4, "Start Dialog Error", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogQuestionYesNo", 			5, "Start Dialog Question Yes/No", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogQuestionYesNoCancel", 	6, "Start Dialog Question Yes/No/Cancel", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogQuestionOkCancel", 		7, "Start Dialog Question Ok/Cancel", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogInputByText", 			8, "Start Dialog Input By Text", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogInputByComboBox", 		9, "Start Dialog Input By ComboBox", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogFileOpenChooser", 		10,"Start Dialog File Open Chooser", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogFileSaveChooser", 		11,"Start Dialog File Save Chooser", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogColorChooser", 			12,"Start Dialog Color Chooser", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogFontChooser", 			13,"Start Dialog Font Chooser", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogUserNoModal", 			14,"Start Dialog User No Modal", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogUserNoModal2", 			15,"Start Dialog User No Modal2", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogUserUndecorated", 		16,"Start Dialog no frame, line border, at 200,300", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogUpdate", 				16,"Update dialog and panel", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogUserModal", 				17,"Start Dialog User Modal", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogLookupTable", 			18,"Start Dialog LookUp Table", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogCloseAll", 				19,"Close all active dialogs", 300);	
	 	  	COMPONENT(new JButton(), "bt_dialogMove", 					20,"Move dialog To pos X,Y", 300);	
		    COMPONENT(new JFormattedTextField(), "ff_dialogPosX", 		20, new Integer(0), 40);	
		    COMPONENT(new JFormattedTextField(), "ff_dialogPosY", 		20, new Integer(0), 40);
		    COMPONENT(new JTextArea(), "ta_dialogSelection", 			21, 200, 150, 500, 100, true, "");	
       	END_PANEL_DETAIL_CONTENT();
       	 
       	      	 
       	/* Contenuto pannello incluso dentro un'altro pannello */
       	BEGIN_PANEL_DETAIL_CONTENT("panelAsComponent1", EnumForwardPanelType.DETAIL, EnumForwardLayout.FLOW_LAYOUT, "Flow layout", "", 0, 0, 0, 0);
       		  COMPONENT(new JTextField(), "tf_AsComponent011", 0, "00"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent021", 0, "01");		
       		  COMPONENT(new JTextField(), "tf_AsComponent031", 0, "02"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent041", 0, "03");		
       		  COMPONENT(new JTextField(), "tf_AsComponent051", 0, "04"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent061", 0, "05");		
       		  COMPONENT(new JTextField(), "tf_AsComponent071", 0, "06");		
       		  COMPONENT(new JTextField(), "tf_AsComponent081", 0, "07");		
       		  COMPONENT(new JTextField(), "tf_AsComponent091", 0, "08");		
       		  COMPONENT(new JTextField(), "tf_AsComponent101", 0, "09"); 		
      		  COMPONENT(new JTextField(), "tf_AsComponent111", 0, "10");		
      		  COMPONENT(new JTextField(), "tf_AsComponent112", 0, "11");		
       	END_PANEL_DETAIL_CONTENT();
       	
       	/* Contenuto pannello incluso dentro un'altro pannello */
       	BEGIN_PANEL_DETAIL_CONTENT("panelAsComponent2", EnumForwardPanelType.DETAIL, EnumForwardLayout.GRID_LAYOUT, "Grid layout 7 rows 1 columns", "", 0, 0, 7, 1);
       		  COMPONENT(new JTextField(), "tf_AsComponent012", 0, "00"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent022", 0, "01");		
       		  COMPONENT(new JTextField(), "tf_AsComponent032", 0, "02"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent042", 0, "03");		
       		  COMPONENT(new JTextField(), "tf_AsComponent052", 0, "04"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent062", 0, "05");		
       		  COMPONENT(new JTextField(), "tf_AsComponent072", 0, "06");	
       	END_PANEL_DETAIL_CONTENT();
       	
       	/* Contenuto pannello incluso dentro un'altro pannello */
       	BEGIN_PANEL_DETAIL_CONTENT("panelAsComponent3", EnumForwardPanelType.DETAIL, EnumForwardLayout.GRID_LAYOUT, "Grid layout 4 rows 2 columns", "", 0, 0, 4, 2);
       		  COMPONENT(new JTextField(), "tf_AsComponent013", 0, "00"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent023", 0, "01");		
       		  COMPONENT(new JTextField(), "tf_AsComponent033", 0, "02"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent043", 0, "03");		
       		  COMPONENT(new JTextField(), "tf_AsComponent053", 0, "04"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent063", 0, "05");		
       		  COMPONENT(new JTextField(), "tf_AsComponent073", 0, "06");		
       	END_PANEL_DETAIL_CONTENT();
       	
       	/* Contenuto pannello incluso dentro un'altro pannello */
       	BEGIN_PANEL_DETAIL_CONTENT("panelAsComponent51", EnumForwardPanelType.DETAIL, EnumForwardLayout.GRID_LAYOUT, "Grid layout 4 rows 1 columns", "", 0, 0, 4, 1);
       		  COMPONENT(new JTextField(), "tf_AsComponent0151", 0, "00"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent0251", 0, "01");		
       		  COMPONENT(new JTextField(), "tf_AsComponent0351", 0, "02"); 		
       		  COMPONENT(new JTextField(), "tf_AsComponent0451", 0, "03");		
       	END_PANEL_DETAIL_CONTENT();
       	/* Contenuto pannello incluso dentro un'altro pannello */
       	
       	BEGIN_PANEL_DETAIL_CONTENT("panelAsComponent52", EnumForwardPanelType.DETAIL, EnumForwardLayout.GRID_LAYOUT, "Grid layout 4 rows 1 columns", "", 0, 0, 4, 1);
     		  COMPONENT(new JTextField(), "tf_AsComponent0152", 0, "00"); 		
     		  COMPONENT(new JTextField(), "tf_AsComponent0252", 0, "01");		
     		  COMPONENT(new JTextField(), "tf_AsComponent0352", 0, "02"); 		
     		  COMPONENT(new JTextField(), "tf_AsComponent0452", 0, "03");		
 		END_PANEL_DETAIL_CONTENT();
        
 		/* 
 		 * Table sample panel 
 		 */
	    BEGIN_PANEL_DETAIL_CONTENT("paTableSample", "Table Sample");
	    	  COMPONENT(new JTable(), "tb_example", 							0, 600, 100, true, JTable.AUTO_RESIZE_OFF);
			  COMPONENT(new JPanel(), "panelTableSelType", 						0); // subpanel
			  COMPONENT(new JTextArea(), "ta_tableEvents", 						1, 100, 100, 600, 50, false);	
   		  	  COMPONENT(new JPanel(), "panelTableSelAllowed", 					1); // subpanel
			  COMPONENT(new JPanel(), "panelTableShowGrid", 					2); // subpanel
			  COMPONENT(new JPanel(), "panelTableRowSet", 						2); // subpanel
			  COMPONENT(new JPanel(), "panelTableColumnSet", 					2); // subpanel
		 	  COMPONENT(new JButton(), "bt_tableCountRowsSelected", 			4, "Count selected rows", 150);	
		 	  COMPONENT(new JButton(), "bt_tableCountColsSelected", 			4, "Count selected cols", 150);	
		 	  COMPONENT(new JButton(), "bt_tableShowRowsSelected", 				4, "Show selected rows", 150);	
		 	  COMPONENT(new JButton(), "bt_tableShowColsSelected", 				4, "Show selected cols", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetColWidth", 					5, "Set column width", 150);	
	 	      COMPONENT(new JButton(), "bt_tableSetRowHeight", 					5, "Set row height", 150);	
			  COMPONENT(new JButton(), "bt_tableSetRowMargin", 					5, "Set margin rows", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetColMargin", 					5, "Set margin cols", 150);	
		 	  COMPONENT(new JFormattedTextField(), "ff_tableValueToSet", 		5, new Integer(8), 40);	
		 	  COMPONENT(new JToggleButton(), "bt_tableColHide", 				6,"Hide col On/Off", false, 150);	
		 	  COMPONENT(new JToggleButton(), "bt_tableColEditable", 			6,"Editable col On/Off", false, 150);	
		 	  COMPONENT(new JToggleButton(), "bt_tableColResizable", 			6,"Resizable col On/Off", false, 150);	
		 	  COMPONENT(new JToggleButton(), "bt_tableFillViewportHeight", 		6,"Fill viewport height", false, 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetGridColor", 					7, "Set grid color", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetSelectionFc", 				7, "Set sel fg color", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetSelectionBc", 				7, "Set sel bg color", 150);	
		 	  COMPONENT(new JButton(), "bt_tableEditCellAt", 					7, "Edit cell At row/col", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSelRow", 						8, "Select row", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSelRowInterval", 				8, "Select row interval", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSelCol", 						8, "Select col", 150);	
		 	  COMPONENT(new JButton(), "bt_tableClearSelection", 				8,"Clear selection", 150);
		 	  COMPONENT(new JButton(), "bt_tableSetCellTooltip", 				9, "Set cell tooltip", 150);	
		 	  COMPONENT(new JButton(), "bt_tableSetColTooltip", 				9, "Set Column tooltip", 150);	
       		  COMPONENT(new JTextField(), "tf_tableTooltip", 					9, "tooltip text to enter", 300); 		
       		  COMPONENT(new JPanel(), "panelTableRowSelected", 					18); // subpanel
       		  COMPONENT(new JPanel(), "panelTableUpdateFromVar", 				18); // subpanel
       		  COMPONENT(new JPanel(), "panelTableUpdateFromPanel", 				18); // subpanel
   		END_PANEL_DETAIL_CONTENT();
   		
        /* Table selection allowed subpanel */
 		BEGIN_PANEL_DETAIL_CONTENT("panelTableSelType", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Selections allowed", "", 0, 0, 3, 1);
		    COMPONENT(Box.class, Box.createVerticalGlue(), "glue02", EnumForwardComponent.JBoxGlueVertical, 0, 0, 0);       			 
	        COMPONENT(new JCheckBox(), "cb_tableRowSel", 	1, "Rows", true);
	 	    COMPONENT(new JCheckBox(), "cb_tableColSel", 	2, "Columns", false);
	 	    COMPONENT(new JCheckBox(), "cb_tableCellSel", 	3, "Cells", false);
 		    COMPONENT(Box.class, Box.createVerticalGlue(), "glue02", EnumForwardComponent.JBoxGlueVertical, 3, 0, 0);       			 
	  	END_PANEL_DETAIL_CONTENT();
	  	
	    /* Table selection row mode subpanel */
       	BEGIN_PANEL_DETAIL_CONTENT("panelTableSelAllowed", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Selection row mode", "", 0, 0, 3, 1);
       		COMPONENT(Box.class, Box.createVerticalGlue(), "glue02", EnumForwardComponent.JBoxGlueVertical, 0, 0, 0);       			 
       		COMPONENT(new JRadioButton(), "rb_tableSingleSel", 			1, "Single", true, "GRPRB5");
       		COMPONENT(new JRadioButton(), "rb_tableSingleIntervalSel",	2, "Single interval", false, "GRPRB5");
       		COMPONENT(new JRadioButton(), "rb_tableMultipleIntervalSel",3, "Multiple interval ", false, "GRPRB5");
       		COMPONENT(Box.class, Box.createVerticalGlue(), "glue02", EnumForwardComponent.JBoxGlueVertical, 4, 0, 0);       			 
   		END_PANEL_DETAIL_CONTENT();
   		
	    /* Table selection grid mode subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableShowGrid", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Grid rendering", "", 0, 0, 0, 0);
	        COMPONENT(new JCheckBox(), "cb_tableShowGrid", 			0, "Show grid", true);
	 	    COMPONENT(new JCheckBox(), "cb_tableShowVertLines", 	1, "Show vertical lines", false);
	 	    COMPONENT(new JCheckBox(), "cb_tableShowHorizLines", 	2, "Show horizontal lines", false);
	  	END_PANEL_DETAIL_CONTENT();
	  	
	    /* Table column set parameters subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableColumnSet", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Column set parameters", "", 0, 0, 0, 0);
 		  	COMPONENT(new JRadioButton(), "rb_tableColSetCur", 				0, "Current selected", true, "GRPTCS");	 	
 		  	COMPONENT(new JRadioButton(), "rb_tableColSetAll", 				1, "All", false, "GRPTCS");	 	
		  	COMPONENT(new JRadioButton(), "rb_tableColSetByName", 			3, "By name", false, "GRPTCS");	
     		COMPONENT(new JTextField(), "tf_tableColSetName", 				3, 90);		
		  	COMPONENT(new JRadioButton(), "rb_TableColSetByNum", 			4, "By num (0-based)", true, "GRPTCS");	 	
		 	COMPONENT(new JFormattedTextField(), "ff_tableColSetNumStart", 	4, new Integer(0), 40);	
		 	COMPONENT(new JFormattedTextField(), "ff_tableColSetNumEnd", 	4, new Integer(0), 40);	
	  	END_PANEL_DETAIL_CONTENT();
	  	
	  	/* Table row set parameters subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableRowSet", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Row set parameters", "", 0, 0, 0, 0);
 		  	COMPONENT(new JRadioButton(), "rb_tableRowSetCur", 				0, "Current selected", true, "GRPTRS");	 	
 		  	COMPONENT(new JRadioButton(), "rb_tableRowSetAll", 				1, "All", false, "GRPTRS");	 	
		  	COMPONENT(new JRadioButton(), "rb_TableRowSetByNum", 			2, "By num (0-based)", true, "GRPTRS");	 	
		 	COMPONENT(new JFormattedTextField(), "ff_tableRowSetNumStart", 	2, new Integer(0), 40);	
		 	COMPONENT(new JFormattedTextField(), "ff_tableRowSetNumEnd", 	2, new Integer(0), 40);	
	 	END_PANEL_DETAIL_CONTENT();
	 	
	  	/* Table row selected subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableRowSelected", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Row selected panel", "", 0, 0, 0, 0);
     		COMPONENT(new JLabel(), "lb_tableName", 						0, "Name ", 70); 		
     		COMPONENT(new JTextField(), "name", 							0, 70); 
     		COMPONENT(new JLabel(), "lb_tableDate", 						0, "Date ", 70); 		
     		COMPONENT(new JFormattedTextField(), "date", 					0, new Date(), 70);		
     		COMPONENT(new JLabel(), "lb_tableAge", 							1, "Age ", 70); 		
     		COMPONENT(new JTextField(), "age", 								1, 70); 
     		COMPONENT(new JLabel(), "lb_tableFloat", 						1, "Float ", 70); 		
     		COMPONENT(new JFormattedTextField(), "float", 					1, new Float(0), 70);
     		COMPONENT(new JLabel(), "lb_tableNickName", 					2, "Nick Name ", 70); 		
     		COMPONENT(new JTextField(), "nickName", 						2, 70); 
     		COMPONENT(new JLabel(), "lb_tableDouble", 						2, "Double ", 70); 		
     		COMPONENT(new JFormattedTextField(), "double", 					2, new Double(0), 70);
     		COMPONENT(new JLabel(), "lb_tableBound", 						3, "Bound data", 70); 		
     		COMPONENT(new JTextField(), "tb_example_bound", 				3, 70);
     		COMPONENT(new JLabel(), "color", 								3, "Color ", 70); 
     		COMPONENT(new JCheckBox(), "avail", 							3, "Avail", false, 70);	
//	     		COMPONENT(new JFormattedTextField(), "color", 					3, new Color(0, 0, 0), 70);
     		
	 	END_PANEL_DETAIL_CONTENT();
	 	
	  	/* Table update from var subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableUpdateFromVar", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Update from var panel", "", 0, 0, 0, 0);
  	    	COMPONENT(new JButton(), "bt_tableAppendRow", 					0, "Append Row", 200);
	 	  	COMPONENT(new JButton(), "bt_tableUpdateRow", 					0, "Update Row", 200);
	 	  	COMPONENT(new JButton(), "bt_tableInsertRow", 					1, "Insert Row", 200);	
		 	COMPONENT(new JButton(), "bt_tableUpdateSelectedRows", 			1, "Update Selected Rows", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableDeleteAllRows", 				2, "Delete all Rows", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableUpdateRowColumnCell", 		2, "Update Row Column Cell", 200);	
 	  	    COMPONENT(new JButton(), "bt_tableDeleteSelectedRows", 			3, "Delete selected Rows", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableUpdateRowBoundObject", 		3, "Update Row Bound Object", 200);	
		 	COMPONENT(new JButton(), "bt_tableDeleteRows", 					4, "Delete Rows", 200);	
	 	END_PANEL_DETAIL_CONTENT();
	 	
	  	/* Table update from panel subpanel */
	  	BEGIN_PANEL_DETAIL_CONTENT("panelTableUpdateFromPanel", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Update from panel panel", "", 0, 0, 0, 0);
	 	  	COMPONENT(new JButton(), "bt_tableAppendRow2", 					0, "Append Row", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableInsertRow2", 					1, "Insert Row", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableUpdateRow2", 					2, "Update Row", 200);	
	 	  	COMPONENT(new JButton(), "bt_tableUpdateSelectedRows2", 		3, "Update Selected Rows", 200);	
	 	END_PANEL_DETAIL_CONTENT();

		/* 
    	 * Tree sample panel 
 		 */
	  	BEGIN_PANEL_DETAIL_CONTENT("paTreeSample", "Tree Sample");
	 		  COMPONENT(new JTree(), "tr_tree01", 						0, 300, 170, true, "Tree demo");
		      COMPONENT(new JRadioButton(), "rb_treeSingleSel", 		0, "Single selection allowed", true, "GRPRB2");
		 	  COMPONENT(new JRadioButton(), "rb_treeContiguousSel", 	0, "Contiguous selection allowed", false, "GRPRB2");
		 	  COMPONENT(new JRadioButton(), "rb_treeDiscontinueSel", 	0, "Discontinue selection allowed", false, "GRPRB2");
		 	  COMPONENT(new JTextArea(), "ta_treeEvents", 				1, 200, 150, 300, 170, true);	
		 	  COMPONENT(new JButton(), "bt_treeShowSelected", 			2,  "Show selected nodes", 300);	
		 	  COMPONENT(new JButton(), "bt_treeAppendNode", 			3,  "Append node", 300);	
		 	  COMPONENT(new JButton(), "bt_treeRemoveNode", 			4,  "Remove node", 300);	
		 	  COMPONENT(new JButton(), "bt_treeInsNodeAfter", 			5,  "Insert node after selected", 300);	
		 	  COMPONENT(new JButton(), "bt_treeInsNodeBefore", 			6,  "Insert node before selected", 300);	
		 	  COMPONENT(new JButton(), "bt_treeInsNodeAt", 				7,  "Insert node At", 300);	
	   		  COMPONENT(new JFormattedTextField(), "tf_treeNodeNum", 	7, new Integer(0), 20);		
	   		  COMPONENT(new JTextField(), "tf_treeNodeText", 			8, 300);		
   		END_PANEL_DETAIL_CONTENT();
 	
   		
   		///////////////////////////////////////////////////////////////////
   		// Inizio definizione pannelli metriche                          //
   		///////////////////////////////////////////////////////////////////
   		
		/* 
    	 * Metrics 
 		 */
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsPilot", "Metrics");
		  	COMPONENT(new JPanel(), "paMetricsPilot1", 0); // subpanel
		    COMPONENT(new JPanel(), "paMetricsPilot2", 0); // subpanel
  		END_PANEL_DETAIL_CONTENT();
 	
  		BEGIN_PANEL_DETAIL_CONTENT("paMetricsPilot1", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "PilotKeys", "", 0, 0, 0, 0);
	 		COMPONENT(new JLabel(), "lb_metricsSystem", 0, "System / Subsystem / Scope"); 		
			COMPONENT(new JComboBox(), "cb_metricsSystem", 0, 50);  			
			COMPONENT(new JComboBox(), "cb_metricsSubSystem", 0, 50);  
			COMPONENT(new JComboBox(), "cb_metricsScope", 0, 400);  			
	 		COMPONENT(new JLabel(), "lb_pgmName", 2, "Pgm / Sections"); 		
			COMPONENT(new JComboBox(), "cb_metricsPgmName", 2, 100);  			
			COMPONENT(new JComboBox(), "cb_metricsPgmSections", 2, 400);  			
	 		COMPONENT(new JLabel(), "lb_metricsQualSection", 3, "Quality section to show"); 		
			COMPONENT(new JComboBox(), "cb_metricsQualSection", 3, 500);  			
		END_PANEL_DETAIL_CONTENT();
 		
		BEGIN_PANEL_DETAIL_CONTENT("paMetricsPilot2", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "PilotActions", "", 0, 0, 0, 0);
			COMPONENT(Box.class, Box.createVerticalGlue(), "glueK01", EnumForwardComponent.JBoxGlueVertical, 0, 0, 0); 
  			COMPONENT(new JButton(), "bt_metricsViolations", 1, "Violations", 150); 
			COMPONENT(Box.class, Box.createVerticalGlue(), "glueK02", EnumForwardComponent.JBoxGlueVertical, 0, 0, 0); 
		END_PANEL_DETAIL_CONTENT();
 		
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsViolations", "Violations");
	  		COMPONENT(new JTable(), "tb_metricsViolations", 0, 730, 360, true, JTable.AUTO_RESIZE_OFF);
  		END_PANEL_DETAIL_CONTENT();
  		
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsOrigin", "Origin");
  			COMPONENT(new JTable(), "tb_metricsViolationsOrigin", 0, 270, 360, true, JTable.AUTO_RESIZE_OFF);
  		END_PANEL_DETAIL_CONTENT();
 	
 	
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsEmpty", "Empty");
	  		COMPONENT(Box.class, Box.createVerticalGlue(), "glueM01", EnumForwardComponent.JBoxGlueVertical, 0, 0, 0); 
	  		COMPONENT(Box.class, Box.createHorizontalGlue(), "glueM02", EnumForwardComponent.JBoxGlueHorizontal, 1, 0, 0); 
	  		COMPONENT(new JLabel(), "lb_metricsEmpty", 1, "No selection Done"); 
	  		COMPONENT(Box.class, Box.createHorizontalGlue(), "glueM03", EnumForwardComponent.JBoxGlueHorizontal, 1, 0, 0); 
	  		COMPONENT(Box.class, Box.createVerticalGlue(), "glueM04", EnumForwardComponent.JBoxGlueVertical, 2, 0, 0);       			 
		END_PANEL_DETAIL_CONTENT();
	  
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsCnt", "Counter");
	 		COMPONENT(new JLabel(), "lb_cntPgmAnalyzed", 0, "Programs analyzed"); 		
			COMPONENT(new JFormattedTextField(), "cntPgmAnalyzed", 0, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM01", EnumForwardComponent.JBoxRigidArea, 0, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_cntJclJob", 0, "Jcl jobs analyzed"); 		
			COMPONENT(new JFormattedTextField(), "cntJclJob", 0, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_cntPgmExecuted", 1, "Programs executed"); 		
			COMPONENT(new JFormattedTextField(), "cntPgmExecuted", 1, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM02", EnumForwardComponent.JBoxRigidArea, 1, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_cntJclInclude", 1, "Jcl include"); 		
			COMPONENT(new JFormattedTextField(), "cntJclInclude", 1, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_cntSubPgmAnalyzed", 2, "Subprograms analyzed"); 		
			COMPONENT(new JFormattedTextField(), "cntSubPgmAnalyzed", 2, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM03", EnumForwardComponent.JBoxRigidArea, 2, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_cntJclProc", 2, "Jcl procedure"); 		
			COMPONENT(new JFormattedTextField(), "cntJclProc", 2, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_cntSubPgmExecuted", 3, "Subprograms executed"); 		
			COMPONENT(new JFormattedTextField(), "cntSubPgmExecuted", 3, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM04", EnumForwardComponent.JBoxRigidArea, 3, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_cntCopyDefined", 3, "Copy defined"); 		
			COMPONENT(new JFormattedTextField(), "cntCopyDefined", 3, new Integer(0));
  		END_PANEL_DETAIL_CONTENT();
 	  
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsSize", "Size");
	 		COMPONENT(new JLabel(), "lb_sizeInstr", 0, "Instructions number"); 		
			COMPONENT(new JFormattedTextField(), "sizeInstr", 0, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM10", EnumForwardComponent.JBoxRigidArea, 0, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_sizeLinesComment", 0, "Comment lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesComment", 0, new Integer(0));		
	 		COMPONENT(new JLabel(), "lb_sizeLinesCodeLogical", 1, "Logical code lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesCodeLogical", 1, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM11", EnumForwardComponent.JBoxRigidArea, 1, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_sizeLinesCommentProc", 1, "Procedure division comment lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesCommentProc", 1, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_sizeLinesCodePhisical", 2, "Phisical code lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesCodePhisical", 2, new Integer(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueM12", EnumForwardComponent.JBoxRigidArea, 2, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_sizeLinesCommentData", 2, "Data division comment lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesCommentData", 2, new Integer(0));		
	 		COMPONENT(new JLabel(), "lb_sizeLinesBlank", 3, "Blank lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesBlank",3, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_sizeLinesBlankProc", 4, "Procedure division blank lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesBlankProc", 4, new Integer(0));
	 		COMPONENT(new JLabel(), "lb_sizeLinesBlankData", 5, "Data division blank lines number"); 		
			COMPONENT(new JFormattedTextField(), "sizeLinesBlankData", 5, new Integer(0));			
  		END_PANEL_DETAIL_CONTENT();
 
	  	BEGIN_PANEL_DETAIL_CONTENT("paMetricsTime", "Time");
	 		COMPONENT(new JLabel(), "lb_backFiredFunctionPoint", 0, "Estimated function point size based on LOC"); 		
			COMPONENT(new JFormattedTextField(), "backFiredFunctionPoint", 0, new Double(0));
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), "glueZ10", EnumForwardComponent.JBoxRigidArea, 0, 15, 1); 
	 		COMPONENT(new JLabel(), "lb_timeDevelopment", 0, "Development time estimated by day pruductivity"); 		
			COMPONENT(new JFormattedTextField(), "timeDevelopment", 0, new Double(0));		
  		END_PANEL_DETAIL_CONTENT();
 
  		
 		/* -----------------------------------------------------------
 		 *  Inizio definizione pannelli gestione CRUD su EntityObject
 		 * -----------------------------------------------------------
  		 */
  		
  		BEGIN_PANEL_DETAIL_CONTENT("paObjectKeys", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "PilotKeys", "", 0, 0, 0, 0);
	 		COMPONENT(new JLabel(), 0, "System "); 		
			COMPONENT(new JComboBox(), "cb_objectSystem", 0, 50);  	
	 		COMPONENT(new JLabel(), "objectSystemDesc", 0, "*"); 		
	 		COMPONENT(new JLabel(), 1, "Sub System "); 		
			COMPONENT(new JComboBox(), "cb_objectSubSystem", 1, 50);  	
	 		COMPONENT(new JLabel(), "objectSubSystemDesc", 1, "*"); 
	 		COMPONENT(new JLabel(), 2, "Object "); 		
	        COMPONENT(new JTextField(), "idObject", 2);
	        COMPONENT(new JLabel(), "typeObjectDesc", 2, "*");
		END_PANEL_DETAIL_CONTENT();
  		 
  		BEGIN_PANEL_DETAIL_CONTENT("paObjectData", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Data", "", 0, 0, 0, 0);
  			COMPONENT(new JLabel(), 0, "System owner");
			COMPONENT(new JTextField(), "systemOwner", 0);
	        COMPONENT(new JLabel(), "systemOwnerDesc", 0, "*");
 			COMPONENT(new JLabel(), 1, "Sub System owner");
			COMPONENT(new JTextField(), "subSystemOwner", 1);
	        COMPONENT(new JLabel(), "subSystemOwnerDesc", 1, "*" );
	        COMPONENT(new JLabel(), 2, "Source type / status");
			COMPONENT(new JFormattedTextField(), "typeSource", 2, new Integer(0), 30); 
			COMPONENT(new JLabel(), "typeSourceDesc", 2, "*");
			COMPONENT(new JFormattedTextField(), "status", 2, new Integer(0), 30);
			COMPONENT(new JLabel(), "statusDesc", 2, "*", 270);
  			COMPONENT(new JLabel(), 3, "Library source object");
			COMPONENT(new JTextField(), "librarySourceObject", 3, "*");
			COMPONENT(new JTextField(), "libraryDir", 3, "*");
			COMPONENT(new JLabel(), 4,  "Analysis library source, file, suffix");
			COMPONENT(new JTextField(), "librarySource", 4);
			COMPONENT(new JTextField(), "fileSource", 4);
			COMPONENT(new JTextField(), "suffixFileSource", 4);
			COMPONENT(new JLabel(), 5,  "Path doc file");
			COMPONENT(new JTextField(), "pathDocFile", 5, 200);
			COMPONENT(new JLabel(), 6,  "Date/Time first analysis");
			COMPONENT(new JTextField(), "dtFirstAnalysis", 6);
			COMPONENT(new JTextField(), "tmFirstAnalysis", 6);
			COMPONENT(new JLabel(), 7,  "Date/Time last analysis");
			COMPONENT(new JTextField(), "dtLastAnalysis", 7);
			COMPONENT(new JTextField(), "tmLastAnalysis", 7);
		END_PANEL_DETAIL_CONTENT();
		
  		BEGIN_PANEL_DETAIL_CONTENT("paObjectActions", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "Data", "", 0, 0, 0, 0);
			COMPONENT(new JButton(), "bt_objectCreate", 0, "Create", 150); 
 			COMPONENT(new JButton(), "bt_objectRead", 0, "Read", 150); 
			COMPONENT(new JButton(), "bt_objectUpdate", 0, "Update", 150); 
  			COMPONENT(new JButton(), "bt_objectDelete", 0, "Delete", 150); 
			COMPONENT(new JButton(), "bt_objectReset", 0, "Reset", 150); 
		END_PANEL_DETAIL_CONTENT();
  		  
       	/* Pannello test accesso a rule  tables, strutture, personalizzazioni */
       	BEGIN_PANEL_DETAIL_CONTENT("paCardTestTables", "Test tables etc");
       	    COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 20)), "glueZ10", EnumForwardComponent.JBoxRigidArea, 0, 15, 20); 
	   		COMPONENT(new JButton(), "bt_ruleTableDataLookup", 1, "Start FunctionRuleTableDataLookup", 300); 
	   		COMPONENT(new JButton(), "bt_ruleTableIndexLookup", 2, "Start FunctionRuleTableIndexLookup", 300); 
	   		COMPONENT(new JButton(), "bt_ruleTableOperations", 3, "Start FunctionRuleTableOperations", 300); 
	   		COMPONENT(new JButton(), "bt_ruleTableStructure", 4, "Start FunctionRuleTableStructure", 300); 
	   		COMPONENT(new JButton(), "bt_ruleTableData", 5, "Start FunctionRuleTableData", 300); 
	   		COMPONENT(new JButton(), "bt_functionUser", 6, "Start FunctionUser", 300); 
	   		COMPONENT(new JButton(), "bt_functionUserLookup", 7, "Start FunctionUserLookup", 300); 
	   		COMPONENT(new JButton(), "bt_functionCustomization", 8, "Start FunctionCustomization", 300); 
 		END_PANEL_DETAIL_CONTENT();
	    	

        /* Menu bar livello 0*/
    	MENU("mnMenuBar", "Suite", EnumForwardOption.MENU_TYPE_ROOT, EnumForwardOption.MENU_RENDERING_MENUBAR, EnumForwardOption.MENU_DIRECTION_HORIZONTAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_EXTRA_THIN);
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemAssessment", "Assesment");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemViewer", "Viewer");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemInspector","Inspector");
        MENU_ITEM(new JSeparator(), "mnMenuBar_sp001");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemDoc", "Documentation");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemDataLinage", "Data Linage");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnItemImpact","Impact Analysis");
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnTest","Test"); 
        MENU_ITEM(new JSeparator(), "mnMenuBar_sp002");
 	    MENU_ITEM_SUBMENU("mnMenuBar_mnItemsubMenu01", "mnMenuBar_subMenu01", KeyEvent.VK_S, 0);
        MENU_ITEM(new JMenuItem(), "mnMenuBar_mnLast","Last  item"); 
        	/* Submenu Menu bar livello 1 */
 		   	MENU("mnMenuBar_subMenu01", "Submenu", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_MENUBAR, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_EXTRA_THIN);
	        MENU_ITEM(new JCheckBoxMenuItem(), "mnMenuBar01rb001","Check box");
 	        MENU_ITEM(new JCheckBoxMenuItem(), "mnMenuBar01rb002","Check box2");
	        MENU_ITEM(new JSeparator(), "mnMenuBar01sp003");
            MENU_ITEM(new JMenuItem(), "mnMenuBar01mnSubMn01_itm1","Item1"); 
            MENU_ITEM(new JMenuItem(), "mnMenuBar01mnSubMn01_itm2","Item2"); 
            MENU_ITEM_SUBMENU("mnMenuBar01mnSubMn01_itmsubMn0101", "mnMenuBar_subMn0101", KeyEvent.VK_S, 0);
            MENU_ITEM(new JMenuItem(), "mnMenuBar01mnSubMn01_itm3","Item3"); 
	        MENU_ITEM(new JSeparator(), "mnMenuBar01sp004");
	        MENU_ITEM(new JRadioButtonMenuItem(), "mnMenuBar01rb003","Radio Button3", true, "GRP1", "middle.gif", KeyEvent.VK_0, ActionEvent.SHIFT_MASK);
	        MENU_ITEM(new JRadioButtonMenuItem(), "mnMenuBar01rb004","Radio Button4", false, "GRP1", "middle.gif", KeyEvent.VK_C, ActionEvent.SHIFT_MASK+ActionEvent.ALT_MASK);
	        MENU_ITEM(new JRadioButtonMenuItem(), "mnMenuBar01rb005","Radio Button6", false, "GRP1", "middle.gif", 0, 0);
	        MENU_ITEM(new JSeparator(), "mnMenuBar01sp005");
		        /* Submenu Menu bar livello 2 */
	 		   	MENU("mnMenuBar_subMn0101", "CRUD test", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_MENUBAR, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_EXTRA_THIN);
	            MENU_ITEM(new JMenuItem(), "mnMenuBar0101mnSubMn02_itm1","MenuBar Item1"); 
	            MENU_ITEM(new JMenuItem(), "mnMenuBar0101mnSubMn02_itm2","MenuBar Item2"); 
        

        /* Menu e-Amrita suite in alto, su panel paMnMainNorth, livello 0 */
    	MENU("mnSuiteAmrita", "Suite", EnumForwardOption.MENU_TYPE_ROOT, EnumForwardOption.MENU_RENDERING_TOOLBAR, EnumForwardOption.MENU_DIRECTION_HORIZONTAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_THIN);
        MENU_ITEM(new JMenuItem(), "mnItemAssessment", "Assesment", "f01.gif");
        MENU_ITEM(new JMenuItem(), "mnItemViewer", "Viewer", "f02.gif" );
        MENU_ITEM(new JMenuItem(), "mnItemInspector","Inspector", "f03.gif");
        MENU_ITEM(new JSeparator(), "sp001");
        MENU_ITEM(new JMenuItem(), "mnItemDoc", "Documentation", "f04.gif");
        MENU_ITEM(new JMenuItem(), "mnItemDataLinage", "Data Linage", "f05.gif");
        MENU_ITEM(new JMenuItem(), "mnItemImpact","Impact Analysis", "f06.gif");
        MENU_ITEM(new JMenuItem(), "mnTest","Test", "f07.gif"); 
        MENU_ITEM(new JSeparator(), "sp002");
        MENU_ITEM(new JMenuItem(), "mnLast","Last  item", "f08.gif"); 
 	    MENU_ITEM_SUBMENU("mnItem_itmSubMn01", "subMn01", KeyEvent.VK_S, 0);
 	        /* Submenu liv 1 */
 		   	MENU("subMn01", "Submenu", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_THIN);
	        MENU_ITEM(new JCheckBoxMenuItem(), "rb001","Check box");
 	        MENU_ITEM(new JCheckBoxMenuItem(), "rb002","Check box2");
	        MENU_ITEM(new JSeparator(), "sp003");
            MENU_ITEM(new JMenuItem(), "mnSubMn01_itm1","Item1"); 
            MENU_ITEM(new JMenuItem(), "mnSubMn01_itm2","Item2"); 
            MENU_ITEM_SUBMENU("mnSubMn01_itmSubMn0101", "subMn0101", KeyEvent.VK_S, 0);
            MENU_ITEM(new JMenuItem(), "mnSubMn01_itm3","Item3"); 
	        MENU_ITEM(new JSeparator(), "sp004");
	        MENU_ITEM(new JRadioButtonMenuItem(), "rb003","Radio Button3", true, "GRP1", "middle.gif", KeyEvent.VK_0, ActionEvent.SHIFT_MASK);
	        MENU_ITEM(new JRadioButtonMenuItem(), "rb004","Radio Button4", false, "GRP1", "middle.gif", KeyEvent.VK_C, ActionEvent.SHIFT_MASK+ActionEvent.ALT_MASK);
	        MENU_ITEM(new JRadioButtonMenuItem(), "rb005","Radio Button6", false, "GRP1", "middle.gif", 0, 0);
	        MENU_ITEM(new JSeparator(), "sp005");
	            /* Submenu liv 2 */
	 		   	MENU("subMn0101", "Submenu liv2", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE3, EnumForwardOption.MENU_HEIGH_THIN);
	            MENU_ITEM(new JMenuItem(), "mnSubMn02_itm1","Item1"); 
	            MENU_ITEM(new JMenuItem(), "mnSubMn02_itm2","Item2"); 
        
		/* Menu Assesment specifico a richiesta a sinistra, su panel paMnAssesment */
		MENU("mnAssesment", "Assesment", EnumForwardOption.MENU_TYPE_ROOT, EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE1, EnumForwardOption.MENU_HEIGH_MEDIUM);
        MENU_ITEM(new JMenuItem(), "mnItemConfig", "Configuration");
        MENU_ITEM(new JMenuItem(), "mnItemSourcesAcquiring", "Libraries & sources acquiring");
        MENU_ITEM(new JMenuItem(), "mnItemSourcesAcquired", "Sources acquired");
        MENU_ITEM(new JMenuItem(), "mnItemSourcesAnalysis", "Sources analysis");
        MENU_ITEM(new JMenuItem(), "mnItemPostAnalysisPgm", "Post analysis pgm level");
        MENU_ITEM(new JMenuItem(), "mnItemPostAnalysisFunction", "Post analysis function level");
        MENU_ITEM(new JMenuItem(), "mnItemMetricsDashBoard", "Metrics dashboard");
        MENU_ITEM_SUBMENU( "mnItemSubMn03", "subMn03", KeyEvent.VK_S, 0);
        MENU_ITEM(new JMenuItem(), "mnItemTestTables", "Test tables");
            /* Submenu assesment liv 1 */
 		   	MENU("subMn03", "CRUD test", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE1, EnumForwardOption.MENU_HEIGH_MEDIUM);
            MENU_ITEM(new JMenuItem(), "mnItemTestCrudObject","Insert Delete Update object"); 
            MENU_ITEM(new JMenuItem(), "mnSubMn03_itm2","Item2"); 
            MENU_ITEM_SUBMENU("mnSubMn03_itmSubMn04", "subMn04", KeyEvent.VK_S, 0);
            MENU_ITEM(new JMenuItem(), "mnSubMn03_itm2","Item3"); 
                /* Submenu assesment liv 2 */
	 		   	MENU("subMn04", "Submenu liv3", EnumForwardOption.MENU_TYPE_SUBMENU, EnumForwardOption.MENU_RENDERING_PLAIN_BUTTONS, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE1, EnumForwardOption.MENU_HEIGH_MEDIUM);
	            MENU_ITEM(new JMenuItem(), "mnSubMn04_itm1","Item1"); 
	            MENU_ITEM(new JMenuItem(), "mnSubMn04_itm2","Item2"); 
	            MENU_ITEM(new JMenuItem(), "mnSubMn04_itm2","Item3"); 
            
		/* Menu popUp, su panel paMnAssesment */
		MENU("mnPopUpAssesment", "PopUp", EnumForwardOption.MENU_TYPE_POPUP, EnumForwardOption.MENU_RENDERING_MENUBAR, EnumForwardOption.MENU_DIRECTION_VERTICAL, EnumForwardOption.MENU_STYLE1, EnumForwardOption.NOT_ASSIGNED);
        MENU_ITEM(new JMenuItem(), "mnPopUpItm1", "Riga menuuuuuuu");
        MENU_ITEM(new JSeparator(), "sp006");
        MENU_ITEM(new JCheckBoxMenuItem(), "mnPopUpItm2","Check box");
        MENU_ITEM(new JCheckBoxMenuItem(), "mnPopUpItm3","Check box2");
        

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
     	
     		TABLE_HELP(23, 44);
     		TABLE_CAPTIONS(123);
    	    COMPONENT(new Timer(0, null), "timer1", 3000, 1000, false);
     	    COMPONENT(new Timer(0, null), "timer2", 1000, 1000, false);
      	    COMPONENT_LOOKUP_RULE_TABLE("status", EnumTable.EnumObjectStatus.getNumTable(), new String[]{"status keyValNumeric", "statusDesc descItem" });
     	    COMPONENT_CTRL("status", EnumForwardOption.COMPONENT_CTRL_NUMERIC_VALUE_RANGE, 0, 2);
     	    COMPONENT_CTRL("typeSource", EnumForwardOption.COMPONENT_CTRL_EXISTS_RULE_TABLE, EnumTable.EnumSourceType.getNumTable());
      	    
     	END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
            /* Modifica proprietà direttamente su oggetti swing componente del modello */
      		getJPanel("MainContainer").setPreferredSize(new Dimension(1150, 655));		 
    		getJPanel("MainContainer").setMaximumSize(new Dimension(1150, 655));	  
       		getJPanel("paMetricsPilot").setPreferredSize(new Dimension(1000, 90));	 	 
      		getJPanel("paMetricsPilot").setMaximumSize(new Dimension(1000, 90));	 	 
       		getJPanel("paMetricsCard").setPreferredSize(new Dimension(1000, 150));	 	 
      		getJPanel("paMetricsCard").setMaximumSize(new Dimension(1000, 150));	 	 
       		getJPanel("paObjectKeys").setPreferredSize(new Dimension(1000, 90));	 	 
      		getJPanel("paObjectKeys").setMaximumSize(new Dimension(1000, 90));	 	 
       		getJPanel("paObjectData").setPreferredSize(new Dimension(1000, 200));	 	 
      		getJPanel("paObjectData").setMaximumSize(new Dimension(1000, 200));	 	 
       		getJPanel("paObjectActions").setPreferredSize(new Dimension(1000, 50));	 	 
      		getJPanel("paObjectActions").setMaximumSize(new Dimension(1000, 50));	 	 
    		getJPanel("panelAsComponent1").setPreferredSize(new Dimension(400, 60));				 
          	getJPanel("panelAsComponent1").setMaximumSize(new Dimension(400, 60));				 
          	getJPanel("panelAsComponent2").setPreferredSize(new Dimension(400, 120));				 
          	getJPanel("panelAsComponent2").setMaximumSize(new Dimension(400, 120));						 
          	getJPanel("panelAsComponent3").setPreferredSize(new Dimension(400, 60));						 
          	getJPanel("panelAsComponent3").setMaximumSize(new Dimension(400, 60));						 
          	getJPanel("panelAsComponent51").setPreferredSize(new Dimension(200, 60));						 
          	getJPanel("panelAsComponent51").setMaximumSize(new Dimension(200, 60));							 
          	getJPanel("panelAsComponent52").setPreferredSize(new Dimension(200, 60));						 
          	getJPanel("panelAsComponent52").setMaximumSize(new Dimension(200, 60));							 
	        getJLabel("lb_maxFilesToProcess").setForeground(Color.RED);
          	getJCheckBox("detectSourceType").setToolTipText("The type of source to analyze riga 2");
          	getJMenuItem("mnSuiteAmrita", "mnItemAssessment").setToolTipText("Assesment, analysis & metrics");
          	
          	/* Impostazione X pulsanti testi */
          	getJButton("bt_tableUpdateRow").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteAllRows").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableAppendRow").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateSelectedRows").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteSelectedRows").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableInsertRow").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateRowColumnCell").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteRows").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateRowBoundObject").setMaximumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateRow").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteAllRows").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableAppendRow").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateSelectedRows").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteSelectedRows").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableInsertRow").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateRowColumnCell").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableDeleteRows").setMinimumSize(new Dimension(200, 25));
          	getJButton("bt_tableUpdateRowBoundObject").setMinimumSize(new Dimension(200, 25));
          	
          	// Modifica font/colori di componenti
          	getJButton("bt_startTimer").setFont(getJButton("bt_startTimer").getFont().deriveFont(Font.BOLD));
          	getJButton("bt_startTimer").setForeground(Color.BLUE);
          	
          	/* Modifica dimensioni fisse */
          	getJSeparator("sp_01").setPreferredSize(new Dimension(600, 8));
          	getJSeparator("sp_02").setPreferredSize(new Dimension(600, 8));

          	/* Esempio modifica proprietà specifiche componenti via reflection runtime in aggiunta e override a tunung e ai default */;
	    	COMPONENT_PROPERTY("customerInfo", "Foreground", Color.RED);

	    	/* Personalizzazione panel CRUD */
	    	getJLabel("typeSourceDesc").setForeground(Color.BLUE);
	    	getJLabel("typeSourceDesc").setFont(getJButton("typeSourceDesc").getFont().deriveFont(Font.BOLD));
	    	getJLabel("statusDesc").setForeground(Color.BLUE);
	    	getJLabel("statusDesc").setFont(getJButton("statusDesc").getFont().deriveFont(Font.BOLD));
	    	getJLabel("typeObjectDesc").setForeground(Color.BLUE);
	    	getJLabel("typeObjectDesc").setFont(getJButton("typeObjectDesc").getFont().deriveFont(Font.BOLD));
	    		    	
	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paRunProperties", EnumForwardBorderType.TITLED_LINE, "Run Properties", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableSelAllowed", EnumForwardBorderType.TITLED_LINE, "Row selection mode allowed", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableSelType", EnumForwardBorderType.TITLED_LINE, "Selections allowed", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableShowGrid", EnumForwardBorderType.TITLED_LINE, "Grid rendering", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableRowSet", EnumForwardBorderType.TITLED_LINE, "Row set parameters", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableColumnSet", EnumForwardBorderType.TITLED_LINE, "Column set parameters", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableRowSelected", EnumForwardBorderType.TITLED_LINE, "Panel selected fields", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableUpdateFromVar", EnumForwardBorderType.TITLED_LINE, "Update from panel fields", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("panelTableUpdateFromPanel", EnumForwardBorderType.TITLED_LINE, "Update from input action", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsPilot", EnumForwardBorderType.TITLED_LINE, "Pilot metrics viewer", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsEmpty", EnumForwardBorderType.TITLED_LINE, "", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsSize", EnumForwardBorderType.TITLED_LINE, "Size measures", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsCnt", EnumForwardBorderType.TITLED_LINE, "Counters of sources measures", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsTime", EnumForwardBorderType.TITLED_LINE, "Estimated development effort", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsViolations", EnumForwardBorderType.TITLED_LINE, "Metrics violations", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paMetricsOrigin", EnumForwardBorderType.TITLED_LINE, "Origin", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paObjectKeys", EnumForwardBorderType.TITLED_LINE, "Keys", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paObjectData", EnumForwardBorderType.TITLED_LINE, "Data", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paObjectActions", EnumForwardBorderType.TITLED_LINE, "Actions", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, 1, false, 0, 0, 0, 0);
        BORDER("paDialogUserNoModal", Color.RED, 5, true);
        BORDER("pb_test", Color.RED, 5, true);		// Line border
     
        /* Componenti NON direttamente disposti nei pannelli  */
        COMPONENT(new JComboBox(), "cb_table", 0, 0);  			// Renderer & editor in Table per colonna comboBox
        
        
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_SOURCE("paTableSample", "ldvLibraries", "col1Db fld1", "col2Db fld2");
        DATA_VALUES(new JComboBox(), "cb_table", new String[]{"Pippo", "Pluto", "Paperino"});
        DATA_VALUES(new JComboBox(), "cb_enumeration", true, new String[]{"Item4", "Item1", "Item2", "Item3", "Item4", "Item5", "Item6"});
        DATA_VALUES_BOUND(new JComboBox(), "cb_enumeration", new String[]{"bound1", "bound2", "bound3", "bound4", "bound5", "bound6"});  
        
        /* Metrics */
        DATA_VALUES(new JComboBox(), "cb_metricsSystem", true, new String[]{"01", "01", "02"});
        DATA_VALUES(new JComboBox(), "cb_metricsSubSystem", true, new String[]{"A1", "A1", "A2", "A3"});
        DATA_VALUES(new JComboBox(), "cb_metricsPgmSections", true, new String[]{"A1", "A1", "A2", "A3"});
        DATA_VALUES(new JComboBox(), "cb_metricsPgmName", true, new String[]{"A1", "A1", "A2", "A3"});
        DATA_VALUES(new JComboBox(), "cb_metricsQualSection", true, new String[]{"Contatori", "Dead code", "Complessità"});
        DATA_SOURCE("cb_metricsSystem", "LdvSystem");
        DATA_SOURCE("cb_metricsSubSystem", "LdvSubSystem");
        DATA_SOURCE("cb_metricsQualSection", "LdvReadSetRuleTable");
        DATA_SOURCE("cb_metricsPgmName", "LdvMetrPgms");
        DATA_SOURCE("cb_metricsPgmSections", "LdvMetrPgmSections");
        /* Metrics end */
        
        DATA_VALUES(new JList(), "li_enumeration", new String[]{"Row1", "Row2", "Row3", "Row4", "Row5", "Row6", "Row7", "Row8", "Row9", "Row10"});
        DATA_VALUES_BOUND(new JList(), "li_enumeration", new String[]{"bound1", "bound2", "bound3", "bound4", "bound5", "bound6", "bound7", "bound8", "bound8", "bound9"});  
        DATA_VALUES(new JTable(), "tb_example"
		        	    , new String[]{"name", "age", "avail", "color", "nickName", "date", "float", "double"}															// Nome campo colonne
	    	    		, new String[]{"Name", "Age", "Availability", "Color", "Nickname", "Date", "Float", "Double"}													// Header colonne
	                    , new Class[]{String.class, Integer.class, Boolean.class, Color.class, String.class, Date.class, Float.class, Double.class}						// Tipo colonne
					    , new String[]{"Tooltip 1", "Tooltip 2", "Tooltip 3", "Tooltip 4", "Tooltip 5", "Tooltip 6", "Tooltip 7", "Tooltip 8"}							// Tooltip di header colonne
					    , new boolean[]{false, true, true, true, true, true, true, true}																				// Colonne editabili
					    , new boolean[]{true, false, false, true, true, true, true, true}																				// Colonne resizabili
	    				, new int[]{-1, -1, -1, -1, -1, -1, -1, -1}																									    // Width colonne
//8					    , new TableCellRenderer[]{null, null, null, new ColorRenderer(true), null, null, null, null}													// Renderer colonne
//					    , new TableCellEditor[]{null, null, null, new ColorEditor(), new DefaultCellEditor(getJComboBox("cb_table")), null, null, null}					// Editor colonne
					    ,null, null
                        , new Object[][]{
						                  {"Carlo",     new Integer(34), new Boolean(true), Color.RED, "Pippo", new Date(), new Float(Math.PI), new Double(Math.PI)}	// Dati Riga 1
						                , {"Alberto",   new Integer(22), new Boolean(false), Color.BLUE, "Pippo", new Date(),  new Float(8.3), new Double(8.3)}			 // Dati Riga 2
						                , {"Maurizia",  new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia2", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia3", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia4", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia5", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia6", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
						                , {"Maurizia7", new Integer(21), new Boolean(true), Color.MAGENTA, "Paperino", new Date(), new Float(-7.365), new Double(-7.365)}// Dati Riga 3
		                                }
			       );
        DATA_VALUES_BOUND(new JTable(), "tb_example", "tb_example_bound", new String[]{"bound1", "bound2", "bound3","bound4", "bound5", "bound6","bound7", "bound8", "bound9"});  
   	    
        
        DATA_VALUES(new JTable(), "tb_metricsViolations"
        	    , new String[]{"typeViolationDesc", "severityViolationDesc", "cntViolations", "qualityCharacteristicDesc", "remediationCost", "remediationUnitDesc"}// Nome campo colonne
	    		, new String[]{"Violation", "Severity", "Count", "Quality Characteristic", "Cost", "Unit"}															// Header colonne
                , new Class[]{String.class, String.class, Integer.class, String.class, Integer.class, String.class}													// Tipo colonne
			    , new String[]{"Violation", "Severity", "Counter violations", "Quality characteristic", "Remediation cost", "Remediation unit"}						// Tooltip di header colonne
			    , new boolean[]{false, false, false, false, false, false}																							// Colonne editabili
			    , new boolean[]{true, true, true, true, true, true}																									// Colonne resizabili
				, new int[]{300, 70, 40, 130, 35, 45}																									    		// Width colonne
			    , new TableCellRenderer[]{null, null, null, null, null, null}																						// Renderer colonne
			    , new TableCellEditor[]{null, null, null, null,  null, null}																						// Editor colonne
			    , new Object[][]{
				                  {"Vio01", "Fatal", 3, "Maintenability", 10, "Hours"}																				// Dati Riga 1
                                }
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_metricsViolations", "tb_metricsViolations_bound", new String[]{"bound1"});  
        
        DATA_VALUES(new JTable(), "tb_metricsViolationsOrigin"
        	    , new String[]{"numInstrDef", "numRow", "numRowsCopy"}// Nome campo colonne
	    		, new String[]{"Instr", "Row", "Copy rows"}																				// Header colonne
                , new Class[]{Integer.class, Integer.class, String.class}																// Tipo colonne
			    , new String[]{"Instruction number or definition", "Row number 0-based", "Row numebers in copy"}						// Tooltip di header colonne
			    , new boolean[]{false, false, false}																					// Colonne editabili
			    , new boolean[]{true, true, true}																						// Colonne resizabili
				, new int[]{-1, -1, -1}																									// Width colonne
			    , new TableCellRenderer[]{null, null, null}																				// Renderer colonne
			    , new TableCellEditor[]{null, null, null}																				// Editor colonne
			    , new Object[][]{
				                  {134, 1237, "10 21 18"}													// Dati Riga 1
                                }
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_metricsViolationsOrigin", "tb_metricsViolationsOrigin_bound", new String[]{"bound1"});  

        DATA_VALUES(new JTree(), "tr_tree01", "Root",
		                     new Object[]{new Object[]{"Node1", "0-0"}                       // Node1 leaf 
		                                 ,new Object[]{"Node2", "0-1",                       // Node2 folder 
		                                                        new Object[]{new Object[]{"Node21", "0-2-0"}		// .. 
		                                                                    ,new Object[]{"Node22", "0-2-1"}		// ..
		                                                                    ,new Object[]{"Node23", "0-2-2"}		// ..
		                                                                    ,new Object[]{"Node24", "0-2-3"}		//	Nodes child of Node2  
		                                                                    ,new Object[]{"Node25", "0-2-4"}		// ..
		                                                                    ,new Object[]{"Node26", "0-2-5"}		// ..
		                                                                    ,new Object[]{"Node27", "0-2-6"}		// ..
		                                                                    }
		                                  									  
		                                              }
		                                 ,new Object[]{"Node3", "0-3", true}                 // Node3 folder with first service child automatically inserted
		                                 ,new Object[]{"Node4", "0-4",                       // Node4 folder  
		                                                        new Object[]{new Object[]{"Node41", "0-4-0"}	   		// ..
		                                                                    ,new Object[]{"Node42", "0-4-1"}	   		// ..
		                                                                    ,new Object[]{"Node43", "0-4-2"}	   		// Nodes child of Node4 
		                                                                    ,new Object[]{"Node44", "0-4-3"}	   		// .
		                                                                    ,new Object[]{"Node45", "0-4-4"}	   		// ..
		                                                                    }
		                                              }
		                                 }
   	    		 );
        DATA_VALUES_BOUND(new JTree(), "tr_tree01", new String[]{
        														 "0-1"
        													   , "0-2-5"
        													   , "0-4"
        													    }
        										  , new ForwardTreeUserObject[]{
        	                                                     new ForwardTreeUserObject("Node2 User")
        	                                                   , new ForwardTreeUserObject("Node26 User")
        	                                                   , new ForwardTreeUserObject("Node4 User")
        	                                                    }
        		);  
        
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
  
     		/* -------------------------
     		 * Attivazione logiche Menu 
     		 * -------------------------
     	    */
            /* Attivazione moduli e-Amrita */
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemAssessment", 			DO_COMPONENT_VISIBLE("paCardAssesmentSourceAcquiring", true));
     		/* Attivazione funzioni modulo di assesment */
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemConfig", 				DO_COMPONENT_VISIBLE("paCardAssesmentConf", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemSourcesAcquiring",		DO_COMPONENT_VISIBLE("paCardAssesmentSourcesAcquiring", true));
	     	ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemSourcesAcquired", 		DO_COMPONENT_VISIBLE("paCardAssesmentSourcesAcquired", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemPostAnalysisPgm", 		DO_COMPONENT_VISIBLE("paCardAssesmentPostAnalysisPgm", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemPostAnalysisFunction", 	DO_COMPONENT_VISIBLE("paCardAssesmentPostAnalysisFunction", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemMetricsDashBoard", 		DO_COMPONENT_VISIBLE("paCardAssesmentMetricsDashBoard", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemTestCrudObject", 		DO_COMPONENT_VISIBLE("paCardObjectCRUD", true));
     		ON_EVENT(EnumForwardEvent.ON_MENU_ITEM_SELECTED, "mnItemTestTables", 		    DO_COMPONENT_VISIBLE("paCardTestTables", true));

     		/* Attivazione default iniziali di panel e logiche applicative di default */
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_default01",								DO_PANEL_DEFAULTS_INITIAL("paCardAssesmentConf"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_default02",								DO_PANEL_DEFAULTS_INITIAL("paRunProperties"));
     		ON_EVENT(EnumForwardEvent.ON_PANEL_AFTER_DEFAULTS_INITIAL, 						DO_PANEL_DEFAULTS_USER("paRunProperties", "panelDefaultsUser"));
     		
     		/* Attivazione logica applicativa su paRunProperties  */
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_selPath", 						   		DO_DIALOG_START_FILE_OPEN_CHOOSER("Dialog01", "Path user exit", false));
//	     	ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_selPath", 						    	DO_EXEC_REUSABLE_METHOD("startDialog01"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_selColor", 						    DO_DIALOG_START_COLOR_CHOOSER("Dialog02", "Color chooser test", Color.RED));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_excp", 						        DO_EXEC_METHOD("excpOccurred"));
     		ON_EVENT(EnumForwardEvent.ON_FILE_CHOOSER_APPROVE_OPTION, "bt_selPath", 		DO_EXEC_METHOD("onReturnFromDialog"));
     		ON_EVENT(EnumForwardEvent.ON_COLOR_CHOOSED,                        				DO_EXEC_METHOD("onReturnFromDialog"));  // Common a + eventi
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_startTimer", 						    DO_EXEC_METHOD("startTimer"));

     		/* Attivazione logica applicativa eventi di sistema  */
      		ON_EVENT(EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION,  "",					DO_EXEC_METHOD("onFunctionException")
      				                                                                      , DO_FUNCTION_START_SHOW_EXCEPTION_ERROR()
       		 		);
     		ON_EVENT(EnumForwardEvent.ON_SYSTEM_TIMER_EXPIRED, "timer1", 					DO_EXEC_METHOD("onExpirationTimer"));
     		ON_EVENT(EnumForwardEvent.ON_SYSTEM_TIMER_EXPIRED, "timer2", 					DO_EXEC_METHOD("onExpirationTimer2"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_startProgressBar", 					DO_EXEC_METHOD("onExpirationTimer2"));
            
     		/*  Attivazione logica applicativa su comboBox/list */
     		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_enumeration", 			DO_EXEC_METHOD("onSelectionDone"));
     		ON_EVENT(EnumForwardEvent.ON_LIST_ROWS_SELECTED, "li_enumeration", 				DO_EXEC_METHOD("onSelectionDone"));
     		ON_EVENT(EnumForwardEvent.ON_JAVA_VALUE_CHANGED, "tb_example", 			    	DO_EXEC_METHOD("onSelectionDone"));
     		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "cb_enumeration", 					DO_EXEC_METHOD("onDoubleClick"));
     		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "li_enumeration", 					DO_EXEC_METHOD("onDoubleClick"));
     		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "tf_liItemNumber", 					DO_EXEC_METHOD("onDoubleClick"));
     		
     		/* Attivazione logica su selezione con JSlider */
     		ON_EVENT(EnumForwardEvent.ON_SLIDER_TICKED, "sl_slider01",						DO_EXEC_METHOD("onSliderTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SLIDER_TICKED, "sl_slider02",						DO_EXEC_METHOD("onSliderTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SLIDER_TICKED, "sl_slider03",						DO_EXEC_METHOD("onSliderTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SLIDER_TICKED, "sl_slider04",						DO_EXEC_METHOD("onSliderTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SLIDER_TICKED, "sl_slider05",						DO_EXEC_METHOD("onSliderTicked"));
     		
     		/* Attivazione logica su selezione con JSpinner */
     		ON_EVENT(EnumForwardEvent.ON_SPINNER_TICKED, "sp_spinnerText",					DO_EXEC_METHOD("onSpinnerTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SPINNER_TICKED, "sp_spinnerNum",					DO_EXEC_METHOD("onSpinnerTicked"));
     		ON_EVENT(EnumForwardEvent.ON_SPINNER_TICKED, "sp_spinnerDate",					DO_EXEC_METHOD("onSpinnerTicked"));
     		
     		/* -------------------------
     		 * Attivazione logiche jtree 
     		 * -------------------------
     		 */
     		ON_EVENT(EnumForwardEvent.ON_TREE_NODE_SELECTED, "tr_tree01",					DO_EXEC_METHOD("onTreeNodeSelected"));
     		ON_EVENT(EnumForwardEvent.ON_TREE_NODE_CHILDREN_TO_ADD, "tr_tree01",			DO_EXEC_METHOD("onTreeNodeChildrenToAdd"));
     		ON_EVENT(EnumForwardEvent.ON_TREE_NODE_EXPANDED, "tr_tree01",					DO_EXEC_METHOD("onTreeNodeExpandedCollapsed"));
     		ON_EVENT(EnumForwardEvent.ON_TREE_NODE_COLLAPSED, "tr_tree01",					DO_EXEC_METHOD("onTreeNodeExpandedCollapsed"));
     		ON_EVENT(EnumForwardEvent.ON_TREE_NODE_EDITED_TEXT, "tr_tree01",				DO_EXEC_METHOD("onTreeNodeEditedText"));
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_treeSingleSel",			DO_EXEC_METHOD("onTreeChangeSelectionMode"));
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_treeContiguousSel",		DO_EXEC_METHOD("onTreeChangeSelectionMode"));
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_treeDiscontinueSel",		DO_EXEC_METHOD("onTreeChangeSelectionMode"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeShowSelected", 					DO_EXEC_METHOD("onTreeOperations"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeAppendNode", 						DO_EXEC_METHOD("onTreeOperations"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeRemoveNode", 						DO_EXEC_METHOD("onTreeOperations"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeInsNodeAfter", 					DO_EXEC_METHOD("onTreeOperations"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeInsNodeBefore", 					DO_EXEC_METHOD("onTreeOperations"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_treeInsNodeAt", 						DO_EXEC_METHOD("onTreeOperations"));

     		/* ----------------------------
     		 *  Attivazione logiche jtable 
     		 * ----------------------------
     		 */
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_tableSingleSel",			DO_TABLE_ENABLE_SELECTION_SINGLE("tb_example", true));
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_tableSingleIntervalSel",	DO_TABLE_ENABLE_SELECTION_SINGLE_INTERVAL("tb_example", true));
     		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED,"rb_tableMultipleIntervalSel",	DO_TABLE_ENABLE_SELECTION_MULTIPLE_INTERVAL("tb_example", true));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_CHECKED,  "cb_tableRowSel"			, 	DO_TABLE_ENABLE_SELECTION_ROWS("tb_example", true)
					  																	,	DO_EXEC_METHOD("onTableLogOperations", "Row selection allowed set"));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_UNCHECKED,  "cb_tableRowSel", 			DO_TABLE_ENABLE_SELECTION_ROWS("tb_example", false)
					  																	,	DO_EXEC_METHOD("onTableLogOperations", "Row selection NOT allowed set"));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_CHECKED,  "cb_tableColSel", 				DO_TABLE_ENABLE_SELECTION_COLS("tb_example", true)
																						,	DO_EXEC_METHOD("onTableLogOperations", "Column selection allowed set"));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_UNCHECKED, "cb_tableColSel", 				DO_TABLE_ENABLE_SELECTION_COLS("tb_example", false)
																						,	DO_EXEC_METHOD("onTableLogOperations", "Column selection NOT allowed set"));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_CHECKED,  "cb_tableShowGrid", 	 		DO_TABLE_SET_GRID_VISIBLE("tb_example", true));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_UNCHECKED,  "cb_tableShowGrid",  			DO_TABLE_SET_GRID_VISIBLE("tb_example", false));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_CHECKED,  "cb_tableShowVertLines", 	 	DO_TABLE_SET_VERTICAL_LINES_VISIBLE("tb_example", true));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_UNCHECKED,  "cb_tableShowVertLines", 		DO_TABLE_SET_VERTICAL_LINES_VISIBLE("tb_example", false));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_CHECKED,  "cb_tableShowHorizLines", 	 	DO_TABLE_SET_HORIZONTAL_LINES_VISIBLE("tb_example", true));
	     	ON_EVENT(EnumForwardEvent.ON_CHECKBOX_UNCHECKED,  "cb_tableShowHorizLines", 	DO_TABLE_SET_HORIZONTAL_LINES_VISIBLE("tb_example", false));
	     	ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableCountRowsSelected", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableCountColsSelected", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableShowRowsSelected", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableShowColsSelected", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetColWidth", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetRowHeight", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetColMargin", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetRowMargin", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableEditCellAt", 						DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableClearSelection", 					DO_TABLE_CLEAR_SELECTION("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetGridColor", 					DO_DIALOG_START_COLOR_CHOOSER("Dialog90", "Selection background Color chooser", Color.PINK));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetSelectionFc", 					DO_DIALOG_START_COLOR_CHOOSER("Dialog91", "Selection Foreground Color chooser", Color.RED));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetSelectionBc", 					DO_DIALOG_START_COLOR_CHOOSER("Dialog92", "Selection background Color chooser", Color.ORANGE));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSelRow", 							DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSelRowInterval", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSelCol", 							DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetCellTooltip", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_tableSetColTooltip", 					DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_TOGGLED,  "bt_tableColHide", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_TOGGLED,  "bt_tableColEditable", 			DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_TOGGLED,  "bt_tableColResizable", 			DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_TOGGLED,  "bt_tableFillViewportHeight", 	DO_TABLE_SET_FILLS_VIEWPORT_HEIGH("tb_example", true));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_UNTOGGLED,"bt_tableColHide", 				DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_UNTOGGLED,"bt_tableColEditable", 			DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_BUTTON_UNTOGGLED,"bt_tableColResizable", 			DO_EXEC_METHOD("onTableManageRendering"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableFillViewportHeight", 				DO_TABLE_SET_FILLS_VIEWPORT_HEIGH("tb_example", false));
     		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED,"tb_example", 					DO_TABLE_REFRESH_PANEL_FROM_VARS("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableAppendRow", 					    DO_TABLE_REFRESH_VARS_FROM_PANEL("tb_example")
     				                                                                      , DO_TABLE_APPEND_ROW("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableInsertRow", 						DO_TABLE_REFRESH_VARS_FROM_PANEL("tb_example")
     																					  , DO_TABLE_INSERT_ROW("tb_example", 2));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateRow", 						DO_TABLE_REFRESH_VARS_FROM_PANEL("tb_example")					
     																					  ,	DO_TABLE_UPDATE_ROW("tb_example", 0, false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateSelectedRows", 				DO_TABLE_REFRESH_VARS_FROM_PANEL("tb_example")
     																					  , DO_TABLE_UPDATE_SELECTED_ROWS("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateRowColumnCell", 				DO_TABLE_UPDATE_ROW_COLUMN_CELL("tb_example", "Maurizia00", 0, 0)
     																					  , DO_TABLE_UPDATE_ROW_COLUMN_CELL("tb_example", new Integer(99), 2, "age"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateRowBoundObject", 				DO_TABLE_UPDATE_ROW_BOUND_OBJECT("tb_example", "Bound00", 0));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableDeleteAllRows", 					DO_TABLE_DELETE_ALL_ROWS("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableDeleteRows", 						DO_TABLE_DELETE_ROWS("tb_example", 0, 1, 2));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableDeleteSelectedRows", 				DO_TABLE_DELETE_SELECTED_ROWS("tb_example"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableAppendRow2", 					    DO_TABLE_APPEND_ROW("tb_example", new Object[]{"Maurizia99", new Integer(99), new Boolean(true), Color.YELLOW, "Mauri", new Date(), new Float(99.99F), new Double(99.99D)}, "Bound99"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableInsertRow2", 						DO_TABLE_INSERT_ROW("tb_example", 2, new Object[]{"Maurizia22", new Integer(22), new Boolean(true), Color.YELLOW, "Mauri22", new Date(), new Float(22.99F), new Double(22.99D)}, "Bound22"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateRow2", 						DO_TABLE_UPDATE_ROW("tb_example", 0, new Object[]{"Maurizia33", new Integer(33), new Boolean(true), Color.YELLOW, "Mauri33", new Date(), new Float(33.99F), new Double(33.99D)}, "Bound33"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK,"bt_tableUpdateSelectedRows2", 		 		DO_TABLE_UPDATE_SELECTED_ROWS("tb_example", new Object[]{"Maurizia44", new Integer(44), new Boolean(true), Color.RED, "Mauri44", new Date(), new Float(44.99F), new Double(44.99D)}, "Bound44"));
     		

     		/* ----------------------------
     		 *  Attivazione logiche Dialog 
     		 * ----------------------------
     		 */
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogPlainMessage",		DO_DIALOG_START_PLAIN_MESSAGE("dialogPlainMessage", "Dialog Plain Message", "Plain text ... "));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogInformation",			DO_DIALOG_START_INFORMATION("dialogInformation", "Dialog Information Message", "Information Message ... ", JOptionPane.DEFAULT_OPTION));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogWarning",				DO_DIALOG_START_WARNING("dialogWarning", "Dialog Warning Message", "Warning Message ... ", JOptionPane.DEFAULT_OPTION));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogError",				DO_DIALOG_START_ERROR("dialogError", "Dialog Error Message", "Error Message ... ", JOptionPane.DEFAULT_OPTION));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogQuestionYesNo",		DO_DIALOG_START_QUESTION("dialogQuestionYesNo", "Dialog Question Message", "Question Message ... please answer YES or NO"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogQuestionYesNoCancel",	DO_DIALOG_START_QUESTION("dialogQuestionYesNoCancel", "Dialog Question Message", "Question Message ... please answer YES, NO or CANCEL", JOptionPane.YES_NO_CANCEL_OPTION));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogQuestionOkCancel",	DO_DIALOG_START_QUESTION("dialogQuestionOkCancel", "Dialog Question Message", "Question Message ... please answer OK or CANCEL", JOptionPane.OK_CANCEL_OPTION));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogInputByText",			DO_DIALOG_START_INPUT_BY_TEXT("dialogInputByText", "Dialog Input By Text", "Please enter Value", "DefaulValue ..."));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogInputByComboBox",		DO_DIALOG_START_INPUT_BY_COMBO_BOX("dialogInputByComboBox", "Dialog Input By ComboBox", "Please select the item ...", true, new String[]{"", "item1", "item2", "item3"}, ""));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogFileOpenChooser",		DO_DIALOG_START_FILE_OPEN_CHOOSER("dialogFileOpenChooser", "Dialog File Open Chooser", false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogFileSaveChooser",		DO_DIALOG_START_FILE_SAVE_CHOOSER("dialogFileSaveChooser", "Dialog File Save Chooser", false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogColorChooser",		DO_DIALOG_START_COLOR_CHOOSER("dialogColorChooser", "Dialog Color Chooser", Color.RED));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogUserNoModal",		    DO_DIALOG_START_USER("dialogUserNoModal", "FormDialogUserNoModal", "Dialog User No Modal", false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogUserNoModal2",		DO_DIALOG_START_USER("dialogUserNoModal2", "FormDialogUserNoModal", "Dialog User No Modal2", false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogUserModal",		    DO_DIALOG_START_USER("dialogUserModal", "FormDialogUserModal", "Dialog User Modal", true));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogUserUndecorated",		DO_DIALOG_START_USER("dialogUserUndecorated", "FormDialogUserNoModal", "Dialog User Undecorated", false,true,"",EnumForwardOption.DIALOG_AT_X_Y, 200, 300));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogModalClose",		    DO_DIALOG_CLOSE("FormDialogUserModal"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogNoModalClose",	    DO_DIALOG_CLOSE("FormDialogUserNoModal"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogCloseAll",	    	DO_DIALOG_CLOSE_ALL());
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogUpdate",				DO_EXEC_METHOD("dialogUpdate"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogMove",	    		DO_DIALOG_MOVE("FormDialogUserNo,Modal", 50, 50)		// Move dichiarativo
					  														  , DO_EXEC_METHOD("moveDialog"));							// Move via metodo
     		ON_EVENT(EnumForwardEvent.ON_DIALOG_RETURN, 	    				DO_EXEC_METHOD("onReturnFromDialog"));
     			
     		
   		
     		/*
     		 * 	DIALOG_START_FONT_CHOOSER,    				// 85 Attiva un dialogo modale forward di selezione font
				DIALOG_START_LOOKUP_TABLE, 					// 70 Attiva una funzione modale di lookup per ricerca e restituzione valori da tabelle forward

     		 */
     		
     		/* ----------------------------
     		 *  Test logical data view
     		 * ----------------------------
     		 */
     		ON_EVENT(EnumForwardEvent.ON_LDV_ERROR_EXECUTION, "", 									 DO_FUNCTION_START_SHOW_LDV_ERROR());
    		ON_EVENT(EnumForwardEvent.ON_PANEL_BEFORE_FIRST_SHOW, "paCardAssesmentMetricsDashBoard", DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsSystem", "LdvSystem", "systemValue")
    																							   , DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsSubSystem", "LdvSubSystem", "subSystemValue")
       																							   , DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsScope", "LdvReadSetRuleTable", EnumTable.EnumMetricsScope.getNumTable(), "descItem")
       																							   , DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsQualSection", "LdvReadSetRuleTable", EnumTable.EnumMetricsQualitySections.getNumTable(), "descItem")
    				);
    		
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvSystem", 							 DO_LDV_SET_FIELD("LdvSystem", "system",    "*", null)
																								   , DO_LDV_SET_FIELD("LdvSystem", "subSystem", "*", null)
																								   , DO_LDV_SET_FIELD("LdvSystem", "typeObject", EnumObject.OBJECT_SYS.ordinal(), null)
    				);
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvSubSystem", 						 DO_LDV_SET_FIELD("LdvSubSystem", "system",    "*", null)
					   																			   , DO_LDV_SET_FIELD("LdvSubSystem", "subSystem", "*", null)
    				);
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvReadSetRuleTable", 				 DO_LDV_SET_FIELD("LdvReadSetRuleTable", "system",    "*", null)
																								   , DO_LDV_SET_FIELD("LdvReadSetRuleTable", "subSystem", "*", null)
																								   , DO_LDV_SET_FIELD("LdvReadSetRuleTable", "language", EnumLanguage.ITALIAN.ordinal(), null)
					);
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvMetrPgms", 						 DO_LDV_SET_FIELD("LdvMetrPgms", "system",  "cb_metricsSystem")
    																							   , DO_LDV_SET_FIELD("LdvMetrPgms", "subSystem", "cb_metricsSubSystem")
       																							   , DO_LDV_SET_FIELD("LdvMetrPgms", "scope", Integer.valueOf(EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal()), null)
       																							   , DO_LDV_SET_FIELD("LdvMetrPgms", "typeObject", Integer.valueOf(EnumObject.OBJECT_PGM_COBOL.ordinal()), null)
    				);
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvMetrPgmSections",  				 DO_LDV_SET_FIELD("LdvMetrPgmSections", "system", "cb_metricsSystem")
																						           , DO_LDV_SET_FIELD("LdvMetrPgmSections", "subSystem", "cb_metricsSubSystem")
																						       	   , DO_LDV_SET_FIELD("LdvMetrPgmSections", "scope", EnumMetricsScope.SCOPE_LEVEL_SECTION.ordinal(), null) 
																						       	   , DO_LDV_SET_FIELD("LdvMetrPgmSections", "idObject", "cb_metricsPgmName")
    				);
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvMetrQuality", 				 		 DO_LDV_SET_FIELD("LdvMetrQuality", "system",    "cb_metricsSystem")
																								   , DO_LDV_SET_FIELD("LdvMetrQuality", "subSystem", "cb_metricsSubSystem")
																								   , DO_COMBOBOX_GET_OBJECT_BOUND("cb_metricsScope", "metricsScope")
																								   , DO_LDV_SET_FIELD("LdvMetrQuality", "scope", "metricsScope")
																								   , DO_LDV_SET_FIELD("LdvMetrQuality", "idObject", "cb_metricsPgmName")
																								   , DO_EXEC_METHOD("metricsSetVarTypeObject")
     																							   , DO_LDV_SET_FIELD("LdvMetrQuality", "typeObject", "metricsTypeObject")
																								   , DO_LDV_SET_FIELD("LdvMetrQuality", "section", "cb_metricsPgmSections")
    				); 
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvMetrViolations",  				     DO_LDV_SET_FIELD("LdvMetrViolations", "system", "cb_metricsSystem")
																						           , DO_LDV_SET_FIELD("LdvMetrViolations", "subSystem", "cb_metricsSubSystem")
 																						       	   , DO_LDV_SET_FIELD("LdvMetrViolations", "idObject", "cb_metricsPgmName")
 																						       	   , DO_LDV_SET_FIELD("LdvMetrViolations", "section",  "cb_metricsPgmSections")
 					);
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_POPULATE, "cb_metricsPgmName", 			 DO_COMBOBOX_APPEND_ITEM("cb_metricsPgmName", "", null));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_POPULATE, "cb_metricsPgmSections", 		 DO_COMBOBOX_APPEND_ITEM("cb_metricsPgmSections", "", null));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_metricsScope", 		 	 DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_metricsScope", "LdvReadSetRuleTable", "keyVal", true));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_metricsQualSection", 		 DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_metricsQualSection", "LdvReadSetRuleTable", "keyVal"));
       		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_AFTER_CLEAR, "cb_metricsPgmName", 			 	 DO_COMBOBOX_CLEAR("cb_metricsPgmSections"));
    		
       		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsSubSystem", 			 DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsPgmName", "LdvMetrPgms", "idObject"));
     		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsScope", 				 DO_COMBOBOX_GET_OBJECT_BOUND("cb_metricsScope", "metricsScope") 
       																							   , DO_LDV_CREATE("LdvMetrQuality")
     																							   , DO_LDV_EXECUTE("LdvMetrQuality")
     																							   , DO_LDV_READFIRST("LdvMetrQuality")
     																							   , DO_PANEL_DEFAULTS_INITIAL("paMetricsSize", "paMetricsCnt", "paMetricsTime")
      																							   , DO_LDV_SET_FUNCTION_GUI_CONTROLS("LdvMetrQuality")
     				);
     		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsPgmName", 				 DO_COMBOBOX_POPULATE_FROM_DB("cb_metricsPgmSections", "LdvMetrPgmSections", "section"));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsQualSection", 			 DO_COMBOBOX_GET_OBJECT_BOUND("cb_metricsQualSection", "metricsQualSectionObjBound") );
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsPgmSections", 			 DO_LDV_CREATE("LdvMetrQuality")
         																						   , DO_LDV_EXECUTE("LdvMetrQuality")
     																							   , DO_LDV_READFIRST("LdvMetrQuality")
      																							   , DO_PANEL_DEFAULTS_INITIAL("paMetricsSize", "paMetricsCnt", "paMetricsTime")
      																							   , DO_LDV_SET_FUNCTION_GUI_CONTROLS("LdvMetrQuality")
    																							   , DO_TABLE_DELETE_ALL_ROWS("tb_metricsViolations")
    																							   , DO_TABLE_DELETE_ALL_ROWS("tb_metricsViolationsOrigin")
    																							        				);
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_metricsQualSection", 			 DO_EXEC_METHOD("onCbMetricsQualSectionSelected"));
    		
      		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_metricsViolations",	    						 DO_TABLE_DELETE_ALL_ROWS("tb_metricsViolations")
    																							   , DO_TABLE_POPULATE_FROM_DB("tb_metricsViolations", "LdvMetrViolations"));		 
       		
      		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_metricsViolations",	    		 DO_EXEC_METHOD("metricsViolationsSetObjectBound"));	
      		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_metricsViolations", 	   			 DO_TABLE_DELETE_ALL_ROWS("tb_metricsViolationsOrigin")
     				                                                                               , DO_EXEC_METHOD("metricsViolationsOriginPopulate")
     				);
 
    		
     		/* -------------------------------------
     		 *  Test funzione CRUD su EntityObject
     		 * -------------------------------------
     		 */
    		ON_EVENT(EnumForwardEvent.ON_PANEL_BEFORE_FIRST_SHOW, "paCardObjectCRUD"	, DO_COMBOBOX_POPULATE_FROM_DB("cb_objectSystem", "LdvSystem", "systemValue")
					   																	, DO_COMBOBOX_POPULATE_FROM_DB("cb_objectSubSystem", "LdvSubSystem", "subSystemValue")
						   																, DO_COMPONENT_ENABLE("bt_objectUpdate", false)
						   																, DO_COMPONENT_ENABLE("bt_objectDelete", false)

    				);
    		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "typeSource"						, DO_FUNCTION_LOAD("FunctionLookUpRuleTable")
																						, DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable", EnumTable.EnumSourceType.ordinal(), "numTable", null)
																						, DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpRuleTable", EnumLanguage.ENGLISH.ordinal(), "language", null)
																						, DO_FUNCTION_START("FunctionLookUpRuleTable", "#F01", true)
					);
    		ON_EVENT(EnumForwardEvent.ON_FUNCTION_RETURN, "#F01"						, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpRuleTable", "typeSource", "keyValNumeric")
    																					, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpRuleTable", "typeSourceDesc", "descItem")
     				);
    		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "idObject"						, DO_FUNCTION_LOAD("FunctionLookUpObjects")
    																					, DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpObjects", "cb_objectSystem", "system")
    				 																	, DO_FUNCTION_SET_PARM_REQUIRED("FunctionLookUpObjects", "cb_objectSubSystem", "subSystem")
    																					, DO_FUNCTION_START("FunctionLookUpObjects", "#F02")
			        );
    		ON_EVENT(EnumForwardEvent.ON_FUNCTION_RETURN, "#F02"						, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpObjects", "cb_objectSystem", "system")
    		      																		, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpObjects", "cb_objectSubSystem", "subSystem")
																	    		      	, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpObjects", "idObject", "idObject")
																	    		      	, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpObjects", "typeObject", "typeObject")
																	    		      	, DO_FUNCTION_SET_PARM_RETURNED("FunctionLookUpObjects", "typeObjectDesc", "typeObjectDesc")
																	    		      	, DO_LDV_RUN("LdvObjectCrud")
																	    		      	, DO_LDV_READFIRST("LdvObjectCrud")
																	    		      	, DO_LDV_SET_FUNCTION_GUI_CONTROLS("LdvObjectCrud")
																	    		      	, DO_COMPONENT_ENABLE("bt_objectCreate", false)
																	    		      	, DO_COMPONENT_ENABLE("bt_objectUpdate", true)
																	    		      	, DO_COMPONENT_ENABLE("bt_objectDelete", true)
					);
//    		ON_EVENT(EnumForwardEvent.ON_FUNCTION_RETURNED, "#F02"						, DO_FUNCTION_START_SHOW_LDV_ERROR());
    		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvObjectCrud",  		      DO_LDV_SET_FIELD("LdvObjectCrud", "system", "cb_objectSystem")
																			          	, DO_LDV_SET_FIELD("LdvObjectCrud", "subSystem", "cb_objectSubSystem")
																			          	, DO_LDV_SET_FIELD("LdvObjectCrud", "idObject", "idObject")
																			          	, DO_LDV_SET_FIELD("LdvObjectCrud", "typeObject", "typeObject")
																			          	, DO_LDV_SET_LANGUAGE("LdvObjectCrud", EnumLanguage.ITALIAN)
			     	);	
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_objectCreate",  					  DO_EXEC_METHOD("onObjectButtonCreate"));
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_objectRead", 						  DO_LDV_SET_FIELDS_KEY_FROM_GUI("LdvObjectCrud")
																						, DO_LDV_EXECUTE("LdvObjectCrud")
																						, DO_SKIP_SET_ID("#SKP01") 
 																						, DO_LDV_READFIRST("LdvObjectCrud", "#GET01")
																						, DO_LDV_SET_FUNCTION_GUI_CONTROLS("LdvObjectCrud")
																						, DO_COMPONENT_ENABLE("bt_objectUpdate", true)
																						, DO_COMPONENT_ENABLE("bt_objectDelete", true)
    				);
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_objectDelete", 						  DO_LDV_SET_FIELDS_KEY_FROM_GUI("LdvObjectCrud", "OBJT_DEL")
    																					, DO_SKIP_SET_ID("#SKP02") 
    				 																	, DO_LDV_DELETE("LdvObjectCrud", "OBJT_DEL", "#DEL01")
      				 																	, DO_COMPONENT_ENABLE("bt_objectDelete", false)
      				 																	, DO_DIALOG_START_INFORMATION("objDel", "Information message", "Object succesfully deleted", JOptionPane.DEFAULT_OPTION)
    				 																	, DO_PANEL_DEFAULTS_INITIAL("paObjectKeys", "paObjectData")
  				);
       		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_objectUpdate", 						  DO_SKIP_SET_ID("#SKP05")
       				                                                                    , DO_PANEL_CONTROLS("paObjectData", "")
       				                                                                    , DO_LDV_SET_FIELDS_KEY_FROM_GUI("LdvObjectCrud", "OBJT_UPD")
                       																	, DO_LDV_SET_FIELD("LdvObjectCrud", "system", "cb_objectSystem")
       				                                                                    , DO_LDV_SET_FIELD("LdvObjectCrud", "subSystem", "cb_objectSubSystem")
       				                                                                    , DO_LDV_SET_FIELD("LdvObjectCrud", "typeObject", "typeObject")
       																					, DO_LDV_SET_FIELDS_NOT_KEY_FROM_GUI("LdvObjectCrud", "OBJT_UPD")
       																					, DO_SKIP_SET_ID("#SKP03") 
																						, DO_LDV_UPDATE("LdvObjectCrud", "OBJT_UPD", "#UPD01") 
																						, DO_COMPONENT_ENABLE("bt_objectUpdate", false)
																						, DO_COMPONENT_ENABLE("bt_objectDelete", false)
																						, DO_COMPONENT_ENABLE("bt_objectCreate", true)
																						, DO_DIALOG_START_INFORMATION("objUpd", "Information message", "Object succesfully updated", JOptionPane.DEFAULT_OPTION)
																						, DO_PANEL_DEFAULTS_INITIAL("paObjectKeys", "paObjectData")
       				);
       		
       		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_objectReset", 					      DO_PANEL_DEFAULTS_INITIAL("paObjectKeys", "paObjectData")
       																					, DO_COMPONENT_ENABLE("bt_objectCreate", true)
       																					, DO_COMPONENT_ENABLE("bt_objectRead", true)
       																					, DO_COMPONENT_ENABLE("bt_objectUpdate", false)
      																					, DO_COMPONENT_ENABLE("bt_objectDelete", false)
        			);
     		
       		ON_EVENT(EnumForwardEvent.ON_LDV_READ_NOTFOUND,"#GET01",  			          DO_DIALOG_START_INFORMATION("objRead", "Information message", "Object notfound", JOptionPane.DEFAULT_OPTION)
       																					, DO_SKIP_EVENT_ACTIONS("#SKP01")
       				);            
       		ON_EVENT(EnumForwardEvent.ON_LDV_DELETE_NOTFOUND,"#DEL01",  			      DO_DIALOG_START_INFORMATION("objDel", "Information message", "Object notfound", JOptionPane.DEFAULT_OPTION)
					    															    , DO_SKIP_EVENT_ACTIONS("#SKP02")
       				);            
       		ON_EVENT(EnumForwardEvent.ON_LDV_UPDATE_NOTFOUND,"#UPD01",  			      DO_DIALOG_START_INFORMATION("objUpd", "Information message", "Object notfound", JOptionPane.DEFAULT_OPTION)
					    																, DO_SKIP_EVENT_ACTIONS("#SKP03")
       				);            
       		ON_EVENT(EnumForwardEvent.ON_LDV_INSERT_DUPLICATE,"#INS01",  		          DO_DIALOG_START_INFORMATION("objIns", "Information message", "Object duplicated", JOptionPane.DEFAULT_OPTION)
																						, DO_SKIP_EVENT_ACTIONS("#SKP04")
       				);            
       		ON_EVENT(EnumForwardEvent.ON_PANEL_CONTROLS_WITH_ERRORS,"paObjectData",       DO_FUNCTION_START_SHOW_MESSAGES() 	  
         																				, DO_SKIP_EVENT_ACTIONS("#SKP05")
       				);            
    		
     		/* -------------------------------------
     		 *  Altri test
     		 * -------------------------------------
     		 */
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ruleTableDataLookup"   			   , DO_FUNCTION_LOAD("FunctionRuleTableDataLookUp")  
																					   , DO_FUNCTION_SET_PARM_REQUIRED("FunctionRuleTableDataLookUp", EnumTable.EnumRelationType.ordinal(), "numTable", null)
																					   , DO_FUNCTION_SET_PARM_REQUIRED("FunctionRuleTableDataLookUp", EnumLanguage.ENGLISH.ordinal(), "language", null)
																					   , DO_FUNCTION_START("FunctionRuleTableDataLookUp", "#F01", true)   
				);
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ruleTableIndexLookup"  	   		   , DO_FUNCTION_LOAD("FunctionRuleTableIndexLookup")  
																					   , DO_FUNCTION_SET_PARM_REQUIRED("FunctionRuleTableIndexLookup", EnumLanguage.ENGLISH.ordinal(), "language", null)
																					   , DO_FUNCTION_START("FunctionRuleTableIndexLookup", "#F02", true)   
    				);
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ruleTableStructure"  	   		   , DO_FUNCTION_LOAD("FunctionRuleTableStructure")  
     																				   , DO_FUNCTION_START("FunctionRuleTableStructure", "#F04", false));
  
     	END_EVENTS();

		
	END_FUNCTION_SPECIFICATION();
	     	 
    } // end define method
    

    
    /* Gestione inserimento oggetto su db */
    public int onObjectButtonCreate(ForwardSystem s, ForwardFunction f) {
    	
    	// Inserimento NON in corso
    	if (!getValueBoolean("insertInProgress")) {
     		setVarValue("insertInProgress", true);
			ACTION(DO_PANEL_DEFAULTS_INITIAL("paObjectKeys", "paObjectData")
				 , DO_COMPONENT_ENABLE("bt_objectRead", false)
				 , DO_COMPONENT_ENABLE("bt_objectUpdate", false)
				 , DO_COMPONENT_ENABLE("bt_objectDelete", false)
				  );
			return 0;
		}
    	
    	// Inserimento in corso
    	if (getValueBoolean("insertInProgress")) {
    		setVarValue("insertInProgress", new Boolean(false));
			ACTION(DO_LDV_SET_FIELDS_KEY_FROM_GUI("LdvObjectCrud", "OBJT_INS")
				 , DO_LDV_SET_FIELDS_NOT_KEY_FROM_GUI("LdvObjectCrud", "OBJT_INS")
				 , DO_SKIP_SET_ID("#SKP04")
				 , DO_LDV_INSERT("LdvObjectCrud", "OBJT_INS", "#INS01")
				 , DO_DIALOG_START_INFORMATION("objIns", "Information message", "Object succesfully inserted", JOptionPane.DEFAULT_OPTION)
				 , DO_PANEL_DEFAULTS_INITIAL("paObjectKeys", "paObjectData")
				 , DO_COMPONENT_ENABLE("bt_objectCreate", false)
				 , DO_COMPONENT_ENABLE("bt_objectRead", true)
				 , DO_COMPONENT_ENABLE("bt_objectUpdate", false)
				 , DO_COMPONENT_ENABLE("bt_objectDelete", true)
			);
    	}
     	return 0;
    }
    
    /* ----------------------------
     * Metodi applicativi test ldv
     * ----------------------------
     */
  
    /* Impostazione oggetto bound violazioni con informazioni di origine non visualizzate */
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

    /* Popolamento tabella origine */
    public int metricsViolationsOriginPopulate(ForwardSystem s, ForwardFunction f) {
    	
    	ForwardTableModel tableModelViolationsOrigin = null;
    	ArrayList<Object> al_rowToAppend = null;
       	String[] ar_originViolation = null;
      	String[] ar_originViolationRows = null;
      	String[] ar_originViolationRowsCopy = null;
     	String[] ar_objBound = null;
    	String originViolation = "";           							// METVORGN     Elenco di numeri istruzione/definizione
    	String originViolationRows = "";           						// METVSROW     Elenco di numeri riga sorgente
    	String originViolationRowsCopy = "";           					// METVSCPY     Elenco di numeri riga sorgente di copy (-1 non significativo)
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


    /* Impostazione tipo oggetto in base allo scope */
    public int metricsSetVarTypeObject(ForwardSystem s, ForwardFunction f) {
    	ForwardComboBoxModel comboModel = null;
    	String selectedItem = "";
    	int scope = 0;
    	int typeObject = 0;
    	int index = 0;
    	
    	comboModel = f.getModelJComboBox("cb_metricsScope");
    	selectedItem = (String) comboModel.getSelectedItem();
    	index = comboModel.getIndexOf(selectedItem);
    	scope = (Integer)comboModel._getDataBound(index);
    	if (scope == EnumMetricsScope.SCOPE_LEVEL_OBJECT.ordinal() || scope == EnumMetricsScope.SCOPE_LEVEL_SECTION.ordinal()) {
    		typeObject = EnumObject.OBJECT_PGM_COBOL.ordinal();
		} else if (scope == EnumMetricsScope.SCOPE_LEVEL_SUBSYSTEM.ordinal()) {
			typeObject = EnumObject.OBJECT_SUBSYS.ordinal();
		} else if (scope == EnumMetricsScope.SCOPE_LEVEL_SYSTEM.ordinal()) {
    		typeObject = EnumObject.OBJECT_SYS.ordinal();
		} else {
			typeObject = EnumObject.OBJECT_SYS_SUBSYS_GLOBAL.ordinal();
		}
    	setVarValue("metricsTypeObject", typeObject);
    	  
    	return 0;
    }
     /* Su selezione combo sezione qualità */
    public int onCbMetricsQualSectionSelected(ForwardSystem s, ForwardFunction f) {
    	String panelMetricsSection = "";
    	String ar_panelMetricsSection[] = null;
    	int index = 0;
    	ar_panelMetricsSection = new String[]{"paMetricsEmpty", "paMetricsCnt", "paMetricsSize", "paMetricsDef", "paMetricsTime", "paMetricsDoc", "paMetricsDyn", "paMetricsDeadCode"};
    	index = Integer.parseInt((String)s.getComboBoxBoundObject());
    	panelMetricsSection = ar_panelMetricsSection[index];
    	ACTION(DO_COMPONENT_VISIBLE(panelMetricsSection, true));
    	return s.getReturnCode();
    }
   
    /* ----------------------------
     * Metodi applicativi riusabili
     * ----------------------------
     */
    
    /** Reusable application code */
 	public int startTimer(ForwardSystem s, ForwardFunction f) {
		ACTION(DO_SYSTEM_TIMER_START("timer1"));
		getJTextField("timerInfoStart").setText("Started");
		getJTextField("timerInfoEnd").setText("");
		return 0;
	}
 	
    /** Reusable application code */
 	public int onExpirationTimer(ForwardSystem s, ForwardFunction f) {
		getJTextField("timerInfoEnd").setText("Ended");
		getJTextField("timerInfoStart").setText("");
		return 0;
	}
 	
    /** Reusable application code */
 	public int onExpirationTimer2(ForwardSystem s, ForwardFunction f) {
 		if (getJProgressBar("pb_test").getValue() + 10 > getJProgressBar("pb_test").getMaximum()) {
 			getJProgressBar("pb_test").setValue(0);
			return 0;
		}
 		getJProgressBar("pb_test").setValue(getJProgressBar("pb_test").getValue() + 10);
 		getJProgressBar("pb_test").setStringPainted(true);
 		getTimer("timer2").start();
		return 0;
	}
 	
    /** Reusable application code di attivazione dialogo */
 	public int startDialog01(ForwardSystem s, ForwardFunction f) {
 		ACTION(DO_DIALOG_START_FILE_OPEN_CHOOSER("Dialog01", "Path user exit", false));
		return 0;
	}
 	
    /** Reusable application code */
 	public int onReturnFromDialog(ForwardSystem s, ForwardFunction f) {
 		String userExitPath = "";
      
 		// Dialogo di richiesta user exit path in runProperties
 		if (s.getDialogName().equals("Dialog01")) {
 			userExitPath = s.getFileChooserSelectedFile().getPath();
 			f.getJTextField("userExitPath").setText(userExitPath);
  	 		return 0;
		}
 		
 		// Dialogo di richiesta colore
 		if (s.getDialogName().equals("Dialog02")) {
 			Color color = s.getChooserColorSelected();
 			f.getJTextField("userExitPath").setForeground(color);
  	 		return 0;
		}
 		
 		// Dialogo di richiesta colore griglia tabella
 		if (s.getDialogName().equals("Dialog90")) {
 			ACTION(DO_TABLE_SET_GRID_COLOR("tb_example", s.getChooserColorSelected()));
  	 		return 0;
		}
 		
 		// Dialogo di richiesta colore foreground di selezione tabella
 		if (s.getDialogName().equals("Dialog91")) {
			ACTION(DO_TABLE_SET_SELECTION_FOREGROUND_COLOR("tb_example", s.getChooserColorSelected()));
  	 		return 0;
		}
 		
 		// Dialogo di richiesta colore background di selezione tabella
 		if (s.getDialogName().equals("Dialog92")) {
			ACTION(DO_TABLE_SET_SELECTION_BACKGROUND_COLOR("tb_example", s.getChooserColorSelected()));
  	 		return 0;
		}
 		
 		// Dialogo standard
 		if (s.getDialogName().equals("dialogPlainMessage")
		||  s.getDialogName().equals("dialogInformation")
		||  s.getDialogName().equals("dialogWarning")
		||  s.getDialogName().equals("dialogError")
		||  s.getDialogName().equals("dialogQuestionYesNo")
		||  s.getDialogName().equals("dialogQuestionYesNoCancel")
		||  s.getDialogName().equals("dialogQuestionOkCancel")) {
			String dialogResponse = "";
			switch (s.getDialogOptionChoosed()) {
				case JOptionPane.YES_OPTION:
					dialogResponse = "YES or OK";
					break;
				case JOptionPane.NO_OPTION:
					dialogResponse = "NO";
					break;
				case JOptionPane.CANCEL_OPTION:
					dialogResponse = "CANCEL";
					break;
				case JOptionPane.CLOSED_OPTION:
					dialogResponse = "CLOSED_OPTION";
					break;
				default:
					break;
			}
			f.getJTextArea("ta_dialogSelection").append(dialogResponse + " pressed" + "\n");
			f.getJTextArea("ta_dialogSelection").selectAll();
			f.getJTextArea("ta_dialogSelection").setCaretPosition(f.getJTextArea("ta_dialogSelection").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  	 		return 0;
		}
	
 		// Dialogo di richiesta valore testuale o da combo
 		if (s.getDialogName().equals("dialogInputByText")
		||  s.getDialogName().equals("dialogInputByComboBox")) {
			f.getJTextArea("ta_dialogSelection").append(s.getDialogInputValue() + " entered" + "\n");
			f.getJTextArea("ta_dialogSelection").selectAll();
			f.getJTextArea("ta_dialogSelection").setCaretPosition(f.getJTextArea("ta_dialogSelection").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  	 		return 0;
		}
		
		// Dialogo di richiesta file
 		if (s.getDialogName().equals("dialogFileOpenChooser")
		||  s.getDialogName().equals("dialogFileSaveChooser")) {
			String dialogResponse = "";
			switch (s.getFileChooserResponse()) {
				case JFileChooser.APPROVE_OPTION:
					dialogResponse = "APPROVE";
					f.getJTextArea("ta_dialogSelection").append(s.getFileChooserSelectedFile().getPath() + "\n");
					break;
				case JFileChooser.CANCEL_OPTION:
					dialogResponse = "CANCEL";
					break;
				case JFileChooser.ERROR_OPTION:
					dialogResponse = "ERROR";
					break;
				default:
					break;
			}
			f.getJTextArea("ta_dialogSelection").append(dialogResponse + " pressed" + "\n");
			f.getJTextArea("ta_dialogSelection").selectAll();
			f.getJTextArea("ta_dialogSelection").setCaretPosition(f.getJTextArea("ta_dialogSelection").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  	 		return 0;
		}
 		
		// Dialogo di richiesta colore
 		if (s.getDialogName().equals("dialogColorChooser")) {
			String dialogResponse = "";
 			Color color = s.getChooserColorSelected();
 			dialogResponse = " Red:" + color.getRed() + " Green:" + color.getGreen() +  " Blue:" + color.getBlue();
			f.getJTextArea("ta_dialogSelection").append(dialogResponse + "\n");
			f.getJTextArea("ta_dialogSelection").selectAll();
			f.getJTextArea("ta_dialogSelection").setCaretPosition(f.getJTextArea("ta_dialogSelection").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  	 		return 0;
		}
		
 		/*
 		 * 
	     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_dialogColorChooser",		DO_DIALOG_START_COLOR_CHOOSER("dialogColorChooser", "Dialog Color Chooser", Color.RED));
 		 */
 		
 		
 		return 0;
	}
 	
 	/** Move dialog on screen */
 	public int moveDialog(ForwardSystem s, ForwardFunction f) {
 		ACTION(DO_DIALOG_MOVE("FormDialogUserNoModal", getValueInt("ff_dialogPosX"), getValueInt("ff_dialogPosY")));
 		return 0;
 	}
 	
 	/** Gets swing JDialog object and set dialog as decorated (with the frame visible) */
 	public int dialogUpdate(ForwardSystem s, ForwardFunction f) {
 		JDialog jdialog = getJDialog("FormDialogUserNoModal");
		jdialog.setTitle("Title modified runtime ..");
		
		// Modify main panel of dialog
		getForm("FormDialogUserNoModal").getJrootPanel().setBackground(Color.PINK);
 		return 0;
 	}
 	
 		
    /** Exception simulation */
 	public int excpOccurred(ForwardSystem s, ForwardFunction f) {
 		int i = 0;
 		i = i / 0;
 		return 0;
	}
    
    
    /** Exception applicativa */
 	public int onFunctionException(ForwardSystem s, ForwardFunction f) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		s.getReusableMethodException().printStackTrace(pw);
		String stackTraceNormalized = sw.toString();
		f.getJTextArea("ta_excp").append(stackTraceNormalized + "\n");
		f.getJTextArea("ta_excp").selectAll();
		f.getJTextArea("ta_excp").setCaretPosition(f.getJTextArea("ta_excp").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
 		return 0;
	}
    
    /** Reusable application code */
  	public int onSelectionDone(ForwardSystem s, ForwardFunction f) {
  		
 		getJTable("tb_example").getColumnModel().getColumn(4).setCellEditor(new DefaultCellEditor(getJComboBox("cb_table")));
//		getJTable("tb_example").getColumnModel().getColumn(3).setCellRenderer(new ColorRenderer(true));
//		getJTable("tb_example").getColumnModel().getColumn(3).setCellEditor(new ColorEditor());
//		getJTable("tb_example").getColumnModel().getColumn(3).setResizable(false);
  		
  		// Selezione da combo box
  		if (s.getEventComponentName().equals("cb_enumeration")) {
  			getJTextField("tf_cbItemNumber").setText(s.getComboBoxSelectedIndex()+"");
  			getJTextField("tf_cbItem").setText(s.getComboBoxSelectedItem());
  			getJTextField("tf_cbItemBound").setText((String) s.getComboBoxBoundObject());
  			return 0;
 		}
  		
  		// Selezione da list
  		if (s.getEventComponentName().equals("li_enumeration")) {
  			getJTextField("tf_liItemNumber").setText(s.getListSelectedIndex()+"");
  			getJTextField("tf_liItem").setText(s.getListSelectedItem());
  			getJTextField("tf_liItemBound").setText((String) s.getListBoundObject());
  			return 0;
 		}
  		return 0;
 	}
     
    
    /** Reusable application code */
  	public int onDoubleClick(ForwardSystem s, ForwardFunction f) {
  		
  		// Selezione da combo box
  		if (s.getEventComponentName().equals("tf_liItemNumber")) {
  			getJTextField("tf_liItemNumber").setText("DblClik Done");
   			return 0;
 		}
  		
  		// Selezione da combo box
  		if (s.getEventComponentName().equals("cb_enumeration")) {
  			getJTextField("tf_cbItemNumber").setText(s.getComboBoxSelectedIndex()+"");
  			f.getModelJComboBox("cb_enumeration")._addElement("new", "newBound");
  			getJTextField("tf_cbItem").setText("DblClik Done");
  			return 0;
 		}
  		
  		// Selezione da list
  		if (s.getEventComponentName().equals("li_enumeration")) {
  			getJTextField("tf_liItemNumber").setText(s.getListSelectedIndex()+"");
  			getJTextField("tf_liItem").setText("DblClik Done");
  			return 0;
 		}
  		return 0;
 	}
    
    /** Reusable application code */
  	public int onSliderTicked(ForwardSystem s, ForwardFunction f) {
  		
  		// Tick
  		if (s.getEventComponentName().equals("sl_slider01")) {
  			getJTextField("tf_tickValue").setText(getJSlider("sl_slider01").getValue()+"");
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sl_slider02")) {
  			getJTextField("tf_tickValue").setText(getJSlider("sl_slider02").getValue()+"");
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sl_slider03")) {
  			getJTextField("tf_tickValue").setText(getJSlider("sl_slider03").getValue()+"");
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sl_slider04")) {
  			getJTextField("tf_tickValue").setText(getJSlider("sl_slider04").getValue()+"");
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sl_slider05")) {
  			getJTextField("tf_tickValue").setText(getJSlider("sl_slider05").getValue()+"");
  			return 0;
 		}
  		return 0;
 	}
    
    /** Reusable application code */
  	public int onSpinnerTicked(ForwardSystem s, ForwardFunction f) {
  		
  		// Text
  		if (s.getEventComponentName().equals("sp_spinnerText")) {
  			getJTextField("tf_spinnerValue").setText(s.getEventSpinnerChangedValueText());
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sp_spinnerNum")) {
  			getJTextField("tf_spinnerValue").setText(s.getEventSpinnerChangedValueText());
  			return 0;
 		}
  		if (s.getEventComponentName().equals("sp_spinnerDate")) {
  			getJTextField("tf_spinnerValue").setText(s.getEventSpinnerChangedValueText());
  			return 0;
 		}
  		return 0;
 	}
  	
    /** Reusable application code */
  	public int onTreeNodeSelected(ForwardSystem s, ForwardFunction f) {
		f.getJTextArea("ta_treeEvents").append(s.getEventTreeNodeObject() + " selected on click" + "\n");
		f.getJTextArea("ta_treeEvents").selectAll();
		f.getJTextArea("ta_treeEvents").setCaretPosition(f.getJTextArea("ta_treeEvents").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  		return 0;
 	}
   
    /** Reusable application code */
  	public int onTreeNodeChildrenToAdd(ForwardSystem s, ForwardFunction f) {
  		ForwardTreeUserObject nodeExpanded = (ForwardTreeUserObject) s.getEventTreeNodeParentObject();
  		ForwardTreeUserObject nodeChild = null;
  		ForwardTreeUserObject nodeChildFolder = null;
  		
  		// Children to be inserted on expanded node
  		nodeChild = new ForwardTreeUserObject("Node31 Dynamically inserted on expand");
  		s.getEventTreeModel()._addNodeChild(nodeExpanded, nodeChild, false, true);
  		nodeChild = new ForwardTreeUserObject("Node32 Dynamically inserted on expand");
  		s.getEventTreeModel()._addNodeChild(nodeExpanded, nodeChild, false, true);
  		nodeChild = new ForwardTreeUserObject("Node33 Dynamically inserted on expand");
  		nodeChildFolder = nodeChild;
  		s.getEventTreeModel()._addNodeChild(nodeExpanded, nodeChild, true, true);				// Folder children
   		nodeChild = new ForwardTreeUserObject("Node34 Dynamically inserted on expand");
  		s.getEventTreeModel()._addNodeChild(nodeExpanded, nodeChild, false, true);
  		
  		// Children to be inserted on folder children node
  		nodeChild = new ForwardTreeUserObject("Node331 Dynamically inserted on expand");
		s.getEventTreeModel()._addNodeChild(nodeChildFolder, nodeChild, false, true);
  		nodeChild = new ForwardTreeUserObject("Node332 Dynamically inserted on expand");
		s.getEventTreeModel()._addNodeChild(nodeChildFolder, nodeChild, false, true);
  		return 0;
 	}
   
    /** Reusable application code.
     *  Detecting event type.
     */
  	public int onTreeNodeExpandedCollapsed(ForwardSystem s, ForwardFunction f) {
		f.getJTextArea("ta_treeEvents").append(s.getEventTreeNodeObject() + ((s.getActiveEvent() == EnumForwardEvent.ON_TREE_NODE_COLLAPSED) ? " collapsed" : " expanded") + "\n");
		f.getJTextArea("ta_treeEvents").selectAll();
  		f.getJTextArea("ta_treeEvents").setCaretPosition(f.getJTextArea("ta_treeEvents").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
  		return 0;
 	}
   
    /** Reusable application code */
  	public int onTreeNodeEditedText(ForwardSystem s, ForwardFunction f) {
  		return 0;
 	}
  	
    /** Reusable application code to change tree selection mode */
   	public int onTreeChangeSelectionMode(ForwardSystem s, ForwardFunction f) {
   		
  		// Single selection allowed
   		if (s.getEventComponentName().equals("rb_treeSingleSel")) {
			f.getJTree("tr_tree01").getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
			return 0;
  		}
  		// Contiguous selection allowed
   		if (s.getEventComponentName().equals("rb_treeContiguousSel")) {
   			f.getJTree("tr_tree01").getSelectionModel().setSelectionMode(TreeSelectionModel.CONTIGUOUS_TREE_SELECTION);
    		return 0;
  		}
  		// Discontigous selection allowed
   		if (s.getEventComponentName().equals("rb_treeDiscontinueSel")) {
   			f.getJTree("tr_tree01").getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
   			return 0;
  		}
   		return 0;
  	}
    
    /** Reusable application code to do actions on tree  */
   	public int onTreeOperations(ForwardSystem s, ForwardFunction f) {
   		ForwardTreeModel treeModel = null;
   		ForwardTreeUserObject nodeObj = null;
   		String nodeTextSelected = "";
   		int index = 0;
   		
  		// Show selected nodes
   		if (s.getEventComponentName().equals("bt_treeShowSelected")) {
   			for (int i = 0; i < s.getEventTreeNodesSelectedObject().length; i++) {
				nodeObj = (ForwardTreeUserObject) s.getEventTreeNodesSelectedObject()[i];
				nodeTextSelected+=nodeObj.toString()+ " ";
			}
   			f.getJTextArea("ta_treeEvents").append(nodeTextSelected + "\n");
   			f.getJTextArea("ta_treeEvents").selectAll();
   			f.getJTextArea("ta_treeEvents").setCaretPosition(f.getJTextArea("ta_treeEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
   		
  		// Append node to parent children
   		if (s.getEventComponentName().equals("bt_treeAppendNode")) {
   			treeModel = (ForwardTreeModel) f.getJTree("tr_tree01").getModel();
   			treeModel._addNodeChild(s.getEventTreeNodeParentObject(), new ForwardTreeUserObject(f.getJTextField("tf_treeNodeText").getText()), false, true);
    		return 0;
  		}
   		
 		// Remove node selected
   		if (s.getEventComponentName().equals("bt_treeRemoveNode")) {
   			treeModel = (ForwardTreeModel) f.getJTree("tr_tree01").getModel();
   			treeModel._removeNode(s.getEventTreeNodeObject());
    		return 0;
  		}

  		// Empty string
   		if (f.getJTextField("tf_treeNodeText").getText().equals("")) {return 0;}
 		
  		// Insert node after first currently selected
   		if (s.getEventComponentName().equals("bt_treeInsNodeAfter")) {
  			treeModel = (ForwardTreeModel) f.getJTree("tr_tree01").getModel();
  			index = s.getEventTreeNodeIndex() + 1;
    		treeModel._insertNodeChildAt(s.getEventTreeNodeParentObject(), new ForwardTreeUserObject(f.getJTextField("tf_treeNodeText").getText()), index, false, true);
   			return 0;
  		}
  		// Insert node before first currently selected
   		if (s.getEventComponentName().equals("bt_treeInsNodeBefore")) {
 			treeModel = (ForwardTreeModel) f.getJTree("tr_tree01").getModel();
  			index = s.getEventTreeNodeIndex();
    		treeModel._insertNodeChildAt(s.getEventTreeNodeParentObject(), new ForwardTreeUserObject(f.getJTextField("tf_treeNodeText").getText()), index, false, true);
   			return 0;
  		}
		// Insert node at children position
   		if (s.getEventComponentName().equals("bt_treeInsNodeAt")) {
 			treeModel = (ForwardTreeModel) f.getJTree("tr_tree01").getModel();
  			index = (Integer) f.getJFormattedTextField("tf_treeNodeNum").getValue();
    		treeModel._insertNodeChildAt(s.getEventTreeNodeParentObject(), new ForwardTreeUserObject(f.getJTextField("tf_treeNodeText").getText()), index, false, true);
   			return 0;
  		}
   		return 0;
  	}
 
    /** Reusable application code to append text to a textArea  */
   	public int onTableLogOperations(ForwardSystem s, ForwardFunction f) {
   		String strMsg = "";
   		
   		strMsg = (String) s.getActiveDoParms().parms[0];
		f.getJTextArea("ta_tableEvents").append(strMsg + "\n");
		f.getJTextArea("ta_tableEvents").selectAll();
		f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   		return 0;
    }

    /** Reusable application code to table updates properties  */
   	public int onTableManageUpdates(ForwardSystem s, ForwardFunction f) {
   		

   		return 0;
   	}

   	
    /** Reusable application code to change table properties  */
   	public int onTableManageRendering(ForwardSystem s, ForwardFunction f) {
   		ForwardTableModel tableModel = null;
 		ForwardTableModelColumn tableModelColumn = null;
   		String strMsg = "";
  		String toolTipText = "";
  		String columnName = "";
   		boolean isToggled = false;
   		int numRowFrom = 0;
  		int numRowTo = 0;
  		int numColFrom = 0;
  		int numColTo = 0;
  		
   		/*
   		 * tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();  
   		 * f.getJTable("tb_example").setShowGrid(true);
   		 * f.getJTable("tb_example").setShowVerticalLines(false);
   		 * f.getJTable("tb_example").setShowHorizontalLines(true);
   		 */
 		

  		// Count rows selected
   		if (s.getEventComponentName().equals("bt_tableCountRowsSelected")) {
   			f.getJTextArea("ta_tableEvents").append("Count rows selected:" + s.getTableSelectedRowCount() + "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible

   			return 0;
  		}
 		
  		// Count columns selected
   		if (s.getEventComponentName().equals("bt_tableCountColsSelected")) {
   			f.getJTextArea("ta_tableEvents").append("Count columns selected:" + s.getTableSelectedColumnCount() + "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
 		
		// Show selected rows
   		if (s.getEventComponentName().equals("bt_tableShowRowsSelected")) {
   			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
			f.getJTable("tb_example").setShowHorizontalLines(true);
			strMsg = "Selected row numbers:";
			for (int rowNumber : s.getTableSelectedRows()) {
				strMsg+=rowNumber+" ";
			}
   			f.getJTextArea("ta_tableEvents").append(strMsg + "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
 		
		// Show selected columns
   		if (s.getEventComponentName().equals("bt_tableShowColsSelected")) {
   			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
			f.getJTable("tb_example").setShowHorizontalLines(true);
			strMsg = "Selected column numbers:";
			for (int rowNumber : s.getTableSelectedColumns()) {
				strMsg+=rowNumber+" ";
			}
   			f.getJTextArea("ta_tableEvents").append(strMsg + "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
 		
  		// Set column width to select column
   		if (s.getEventComponentName().equals("bt_tableSetColWidth")){
   			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
   			tableModelColumn = tableModel._getColumn(s.getTableSelectedColumn());
   			
   			// Colonne selezionate
   			if (getJRadioButton("rb_tableColSetCur").isSelected()) {
   	   			ACTION(DO_TABLE_SET_COLUMN_WIDTH("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue()));
   				strMsg = "Selected column " + tableModelColumn.getName() + " number " + s.getTableSelectedColumn() + " set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
			} else if (getJRadioButton("rb_tableColSetByName").isSelected()) {
				ACTION(DO_TABLE_SET_COLUMN_WIDTH("tb_example", getJTextField("tf_tableColSetName").getText(), (Integer) getJFormattedTextField("ff_tableValueToSet").getValue()));
				strMsg = "Selected column " + getJTextField("tf_tableColSetName").getText() + " set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
			} else if (getJRadioButton("rb_tableColSetByNum").isSelected()) {
				ACTION(DO_TABLE_SET_COLUMN_WIDTH("tb_example", (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue(), (Integer) getJFormattedTextField("ff_tableValueToSet").getValue()));
				strMsg = "Selected column " +  (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue() + " set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
			} else {
				return 0;
			}
   			
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
      		
  		// Set row height to value
   		if (s.getEventComponentName().equals("bt_tableSetRowHeight")){
  			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
   			tableModelColumn = tableModel._getColumn(s.getTableSelectedColumn());
   			
 			// Righe selezionate
   			if (getJRadioButton("rb_tableRowSetCur").isSelected()) {
   	 			ACTION(DO_TABLE_SET_ROW_HEIGHT("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue(), true));
  				strMsg = "Selected rows ";
   				for (int rowNumber : s.getTableSelectedRows()) {
   					strMsg+=rowNumber+" ";
   				}
  				strMsg += "set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();

   			// Tutte le righe
			} else if (getJRadioButton("rb_tableRowSetAll").isSelected()) {
 	 			ACTION(DO_TABLE_SET_ROW_HEIGHT("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue(), false));
  				strMsg = "Rows ";
   				for (int rowNumber : s.getTableSelectedRows()) {
   					strMsg+=rowNumber+" ";
   				}
				strMsg += "set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
			// Singola riga o range
			} else if (getJRadioButton("rb_TableRowSetByNum").isSelected()) {
				numRowFrom = (Integer) getJFormattedTextField("ff_tableRowSetNumStart").getValue();
				numRowTo = (Integer) getJFormattedTextField("ff_tableRowSetNumEnd").getValue();
				// Range o singola riga
				if (numRowTo > numRowFrom) {
					ACTION(DO_TABLE_SET_ROW_HEIGHT("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue(), numRowFrom, numRowTo));
					strMsg += "Row from " + numRowFrom + " to " + numRowTo + " set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
				} else {
					ACTION(DO_TABLE_SET_ROW_HEIGHT("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue(), numRowFrom, -1));
					strMsg += "Row " + numRowTo + " set to new width " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
				}
			}
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
      	
   		// Set rows margin  
  		if (s.getEventComponentName().equals("bt_tableSetRowMargin")){
  			ACTION(DO_TABLE_SET_ROW_MARGIN("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue()));
			strMsg += "Margin between rows set to " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
  		}
    		  		
   		// Set cols margin 
 		if (s.getEventComponentName().equals("bt_tableSetColMargin")){
  			ACTION(DO_TABLE_SET_COLUMN_MARGIN("tb_example", (Integer) getJFormattedTextField("ff_tableValueToSet").getValue()));
			strMsg += "Margin between columns set to " + (Integer) getJFormattedTextField("ff_tableValueToSet").getValue();
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
  		}
    		
  		// Cell start editing
   		if (s.getEventComponentName().equals("bt_tableEditCellAt")){
   			ACTION(DO_TABLE_EDIT_CELL_AT("tb_example", (Integer)getJFormattedTextField("ff_tableRowSetNumStart").getValue(), (Integer)getJFormattedTextField("ff_tableColSetNumStart").getValue()));
			strMsg = "Selected row numbers:";
			strMsg = " Started cell editint At row " + (Integer) getJFormattedTextField("ff_tableRowSetNumStart").getValue() + " Col " + (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
   			return 0;
  		}
   		
   		// Select rows interval
   		if (s.getEventComponentName().equals("bt_tableSelRowInterval")){
   			numRowFrom = (Integer) getJFormattedTextField("ff_tableRowSetNumStart").getValue();
   			numRowTo = (Integer) getJFormattedTextField("ff_tableRowSetNumEnd").getValue();
 			strMsg = "Selected rows interval from row " + numRowFrom + " to row " + numRowTo;
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
			ACTION(DO_TABLE_SELECT_ROWS("tb_example", numRowFrom, numRowTo));
 			return 0;
  		}
 
   		// Select row 
   		if (s.getEventComponentName().equals("bt_tableSelRow")){
   			numRowFrom = (Integer) getJFormattedTextField("ff_tableRowSetNumStart").getValue();
 			strMsg = "Selected row number " + numRowFrom;
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
			ACTION(DO_TABLE_SELECT_ROWS("tb_example", numRowFrom));
 			return 0;
  		}
   		
   		
   		// Select Col
   		if (s.getEventComponentName().equals("bt_tableSelCol")){
   			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
   			numColTo = numColFrom;
 			strMsg = "Selected col number " + numColFrom;
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
			ACTION(DO_TABLE_SELECT_COLS("tb_example", numColFrom, numColTo));
 			return 0;
  		}

   		// Select Cols interval
   		if (s.getEventComponentName().equals("bt_tableSelCol")){
  			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
  			numColTo = (Integer) getJFormattedTextField("ff_tableColSetNumEnd").getValue();
			strMsg = "Selected col number range from " + numColFrom + " to " + numColTo;
   			f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
			ACTION(DO_TABLE_SELECT_COLS("tb_example", numColFrom, numColTo));
 			return 0;
  		}

  		// Set Cols hidden
   		if (s.getEventComponentName().equals("bt_tableColHide")){
   			isToggled = (Boolean) getJToggleButton("bt_tableColHide").isSelected();
 			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
  			numColTo = (Integer) getJFormattedTextField("ff_tableColSetNumEnd").getValue();
  			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
   			tableModelColumn = tableModel._getColumn(s.getTableSelectedColumn());
   			
   			// Colonne selezionate
   			if (getJRadioButton("rb_tableColSetCur").isSelected()) {
   	   			ACTION(DO_TABLE_SET_COLUMN_HIDDEN("tb_example",isToggled));
   				strMsg = "Selected columns set to hidden" + isToggled;
   			// Colonna con nome
			} else if (getJRadioButton("rb_tableColSetByName").isSelected()) {
				ACTION(DO_TABLE_SET_COLUMN_HIDDEN("tb_example", getJTextField("tf_tableColSetName").getText(), isToggled));
				strMsg = "Column " +  getJTextField("tf_tableColSetName").getText() + " set to hidden" + isToggled;
			// Range di colonne o colonna singola
			} else if (getJRadioButton("rb_TableColSetByNum").isSelected()) {
				if (numColFrom >= numColTo) {
					ACTION(DO_TABLE_SET_COLUMN_HIDDEN("tb_example", numColFrom, isToggled));
					strMsg = "Column " +  numColFrom + " set to hidden" + isToggled;
				} else {
					ACTION(DO_TABLE_SET_COLUMN_HIDDEN("tb_example", numColFrom, numColTo, isToggled));
					strMsg = "Columns from " +  numColFrom + " to " +  numColTo +" set to hidden" + isToggled;
				}
			} else {
				return 0;
			}
    		f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
     	}
  		
  		// Set Cols resizable
   		if (s.getEventComponentName().equals("bt_tableColResizable")){
   			isToggled = (Boolean) getJToggleButton("bt_tableColResizable").isSelected();
 			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
  			numColTo = (Integer) getJFormattedTextField("ff_tableColSetNumEnd").getValue();
  			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
   			tableModelColumn = tableModel._getColumn(s.getTableSelectedColumn());
   			
   			// Colonne selezionate
   			if (getJRadioButton("rb_tableColSetCur").isSelected()) {
   	   			ACTION(DO_TABLE_SET_COLUMN_RESIZABLE("tb_example", true, isToggled));
   				strMsg = "Selected columns set to resizable" + isToggled;
   			// Tutte le colonne
   			} else if (getJRadioButton("rb_tableColSetAll").isSelected()) {
  	   			ACTION(DO_TABLE_SET_COLUMN_RESIZABLE("tb_example", false, isToggled));
   			    strMsg = "All columns set to resizable " + isToggled;   			
   			// Colonna con nome
			} else if (getJRadioButton("rb_tableColSetByName").isSelected()) {
				ACTION(DO_TABLE_SET_COLUMN_RESIZABLE("tb_example", getJTextField("tf_tableColSetName").getText(), isToggled));
				strMsg = "Column " +  getJTextField("tf_tableColSetName").getText() + " set to resizable " + isToggled;
			// Range di colonne o colonna singola
			} else if (getJRadioButton("rb_TableColSetByNum").isSelected()) {
				if (numColFrom > numColTo) {
					ACTION(DO_TABLE_SET_COLUMN_RESIZABLE("tb_example", numColFrom, isToggled));
					strMsg = "Column " +  numColFrom + " set to resizable " + isToggled;
				} else {
					ACTION(DO_TABLE_SET_COLUMN_RESIZABLE("tb_example", numColFrom, numColTo, isToggled));
					strMsg = "Columns from " +  numColFrom + " to " +  numColTo +" set to resizable " + isToggled;
				}
			} else {
				return 0;
			}
    		f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
     	}
  		
  		// Set Cols Editable
   		if (s.getEventComponentName().equals("bt_tableColEditable")){
   			isToggled = (Boolean) getJToggleButton("bt_tableColEditable").isSelected();
 			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
  			numColTo = (Integer) getJFormattedTextField("ff_tableColSetNumEnd").getValue();
  			tableModel = (ForwardTableModel) f.getJTable("tb_example").getModel();
   			tableModelColumn = tableModel._getColumn(s.getTableSelectedColumn());
   			
   			// Colonne selezionate
   			if (getJRadioButton("rb_tableColSetCur").isSelected()) {
  	   			ACTION(DO_TABLE_SET_COLUMN_EDITABLE("tb_example", true, isToggled));
   			    strMsg = "Selected columns set to editable " + isToggled;
   			// Tutte le colonne
   			} else if (getJRadioButton("rb_tableColSetAll").isSelected()) {
  	   			ACTION(DO_TABLE_SET_COLUMN_EDITABLE("tb_example", false, isToggled));
   			    strMsg = "All columns set to resizable " + isToggled;
   			// Colonna con nome
			} else if (getJRadioButton("rb_tableColSetByName").isSelected()) {
				ACTION(DO_TABLE_SET_COLUMN_EDITABLE("tb_example", getJTextField("tf_tableColSetName").getText(), isToggled));
				strMsg = "Column " +  getJTextField("tf_tableColSetName").getText() + " set to editable " + isToggled;
			// Range di colonne o colonna singola
			} else if (getJRadioButton("rb_TableColSetByNum").isSelected()) {
				if (numColFrom > numColTo) {
					ACTION(DO_TABLE_SET_COLUMN_EDITABLE("tb_example", numColFrom, isToggled));
					strMsg = "Column " +  numColFrom + " set to editable " + isToggled;
				} else {
					ACTION(DO_TABLE_SET_COLUMN_EDITABLE("tb_example", numColFrom, numColTo, isToggled));
					strMsg = "Columns from " +  numColFrom + " to " +  numColTo +" set to editable " + isToggled;
				}
			} else {
				return 0;
			}
    		f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
     	}
  		
   		
  		// Set col tooltip
   		if (s.getEventComponentName().equals("bt_tableSetColTooltip")){
 			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
   			toolTipText = getJTextField("tf_tableTooltip").getText();
   			if (getJRadioButton("rb_tableColSetByName").isSelected()) {
   				columnName = getJTextField("tf_tableColSetName").getText();
   				ACTION(DO_TABLE_SET_COLUMN_TOOLTIP("tb_example", toolTipText, columnName));
   				strMsg = "Column " +  columnName + " tooltip " +  toolTipText +" set";
   			} else if (getJRadioButton("rb_TableColSetByNum").isSelected()) {
   				ACTION(DO_TABLE_SET_COLUMN_TOOLTIP("tb_example", toolTipText, numColFrom));
   				strMsg = "Column " +  numColFrom + " tooltip " +  toolTipText +" set";
			}
    		f.getJTextArea("ta_tableEvents").append(strMsg +  "\n");
   			f.getJTextArea("ta_tableEvents").selectAll();
   			f.getJTextArea("ta_tableEvents").setCaretPosition(f.getJTextArea("ta_tableEvents").getDocument().getLength()); // Make sure the new text is visible
 			return 0;
  		}
  		
  		// Set cell tooltip
   		if (s.getEventComponentName().equals("bt_tableSetCellTooltip")){
			numRowFrom = (Integer) getJFormattedTextField("ff_tableRowSetNumStart").getValue();
			numColFrom = (Integer) getJFormattedTextField("ff_tableColSetNumStart").getValue();
   			toolTipText = getJTextField("tf_tableTooltip").getText();
   			ACTION(DO_TABLE_SET_CELL_TOOLTIP("tb_example", toolTipText, numRowFrom, numColFrom));
  			return 0;
  		}
  		
   		return 0;
  	}
    
  
  	
  	
     /** Reusable application code */
  	public int panelDefaultsUser(ForwardSystem s, ForwardFunction f) {
  		getJTextField("customerInfo").setText("Used Default");
   		return 0;
 	}
    
    
} 
