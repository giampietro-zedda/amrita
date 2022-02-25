package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import enums.EnumForwardBorderType;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumLanguage;
import enums.EnumTable;
import forward.ForwardFunction;

public class FunctionLookUpObjects extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionLookUpObjects() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
    @SuppressWarnings("rawtypes")
	public void declare() {
       
    	PARM_REQUIRED("system", new String(""));
    	PARM_REQUIRED("subSystem", new String(""));
    	PARM_REQUIRED("typeObject", new Integer(0));
    	
    	PARM_RETURNED("system", new String(""));
       	PARM_RETURNED("subSystem", new String(""));
      	PARM_RETURNED("typeObject", new Integer(0));
      	PARM_RETURNED("idObject", new String(""));
    	
      	VAR("languageLogin", EnumLanguage.ITALIAN);
      	VAR("typeObjectSelected", new Integer(0));
  
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("ObjectsSelection", "Objects selection", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH, EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER);
     	
        BEGIN_FORM("FormObjects", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paNorth", "paCenter", "paSouth", "", "");
	   	        PANELS_STRUCTURE("paNorth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paSouth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "OBJECT SELECTION", 600); 
	  		COMPONENT(new JPanel(), "paPilot", 1); // subpanel
   		END_PANEL_DETAIL_CONTENT();
   		
   		BEGIN_PANEL_DETAIL_CONTENT("paPilot", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT, "PilotKeys", "", 0, 0, 0, 0);
	 		COMPONENT(new JLabel(), "lb_system", 0, "system", 70); 
			COMPONENT(new JComboBox(), "cb_system", 0, 50);
	 		COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 1)), EnumForwardComponent.JBoxRigidArea, 0, 20, 1); 
	 		COMPONENT(new JLabel(), "lb_subSystem", 0, "Subsystem", 70); 		
			COMPONENT(new JComboBox(), "cb_subSystem", 0, 50);  
	 		COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 1)), EnumForwardComponent.JBoxRigidArea, 0, 20, 1); 
	 		COMPONENT(new JLabel(), "lb_typeObject", 0, "Type object", 70); 		
			COMPONENT(new JComboBox(), "cb_typeObject", 0, 230);  			
   		END_PANEL_DETAIL_CONTENT();
   		
	  	BEGIN_PANEL_DETAIL_CONTENT("paCenter", "Objects list");
  			COMPONENT(new JTable(), "tb_objects", 0, 730, 360, true, JTable.AUTO_RESIZE_OFF);
		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paSouth");
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_refresh", 0, "Refresh", 200);	
			COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), EnumForwardComponent.JBoxRigidArea, 0, 20, 1); 
	 	  	COMPONENT(new JButton(), "bt_return", 0, "Return", 200);
	 	  	COMPONENT(Box.class, Box.createRigidArea(new Dimension(15, 1)), EnumForwardComponent.JBoxRigidArea, 0, 20, 1); 
	 	  	COMPONENT(new JButton(), "bt_cancel", 0, "Cancel", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	END_PANEL_DETAIL_CONTENT();

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
        END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
            /* Modifica proprietà direttamente su oggetti swing componente del modello */
      		getJPanel("MainContainer").setPreferredSize(new Dimension(600, 500));		 
           	
          	// Modifica font/colore label di titol
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 25));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
        	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paPilot", EnumForwardBorderType.TITLED_LINE, "Pilot", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paCenter", EnumForwardBorderType.TITLED_LINE, "Object", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
     
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JComboBox(), "cb_system", true, null);
        DATA_VALUES(new JComboBox(), "cb_subSystem", true, null);
        DATA_VALUES(new JComboBox(), "cb_typeObject", true, null);
        DATA_VALUES(new JTable(), "tb_objects"
        	    , new String[]{"system", "subSystem", "typeObjectDesc", "idObject"}						// Nome campo colonne
	    		, new String[]{"System", "Subsystem", "Type object", "Id Object"}						// Header colonne
                , new Class[]{String.class, String.class, String.class, String.class}					// Tipo colonne
			    , new String[]{"", "", "", ""}															// Tooltip di header colonne
			    , new boolean[]{false, false, false, false}												// Colonne editabili
			    , new boolean[]{false, false, false, false}												// Colonne resizabili
				, new int[]{70, 70, 270, 100}															// Width colonne
			    , new TableCellRenderer[]{null, null, null, null}										// Renderer colonne
			    , new TableCellEditor[]{null, null, null, null}											// Editor colonne
			    , new Object[][]{}																		// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_objects", "tb_objectsBound", null);  
         
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 
        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "", 					  		  DO_FUNCTION_GET_LANGUAGE("languageLogin")
        			                                                                    , DO_LDV_CREATE("LdvReadSetRuleTable")
        			                                                                    , DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "languageLogin")
        			                                                                    , DO_COMBOBOX_POPULATE_FROM_DB("cb_system", "LdvSystem", "systemValue")
																			     	    , DO_COMBOBOX_POPULATE_FROM_DB("cb_subSystem", "LdvSubSystem", "subSystemValue")
																			     	    , DO_COMBOBOX_POPULATE_FROM_DB("cb_typeObject", "LdvReadSetRuleTable", EnumTable.EnumObject.getNumTable(), "descItem")
																			     	    , DO_FUNCTION_SET_GUI_CONTROL("cb_system", "system")
																			     	    , DO_FUNCTION_SET_GUI_CONTROL("cb_subSystem", "subSystem")
																			     	    , DO_FUNCTION_SET_GUI_CONTROL("cb_typeObject", "typeObject")
																			     	    , DO_TABLE_DELETE_ALL_ROWS("tb_objects")
    				);
       		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_typeObject",    DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_typeObject", "LdvReadSetRuleTable", "keyVal", true));
     		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_typeObject",		  DO_COMBOBOX_GET_OBJECT_BOUND("cb_typeObject", "typeObjectSelected") );
     		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvObjects", 		  		  DO_LDV_SET_FIELD("LdvObjects", "system", "cb_system")
																				 		, DO_LDV_SET_FIELD("LdvObjects", "subSystem", "cb_subSystem")
 																				 		, DO_LDV_SET_FIELD("LdvObjects", "typeObject", "typeObjectSelected")
 																				 		, DO_LDV_SET_LANGUAGE("LdvObjects", "languageLogin")
  					);
      		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_objects",    		  DO_TABLE_POPULATE_SET_OBJECT_BOUND("tb_objects", "LdvObjects", "typeObject"));
      		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_objects", 	          DO_TABLE_GET_OBJECT_BOUND("tb_objects", "typeObject"));
       		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "tb_objects", 			 		  DO_FUNCTION_RETURN(false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_refresh", 			 				  DO_TABLE_POPULATE_FROM_DB("tb_objects", "LdvObjects"));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_return", 			 				  DO_FUNCTION_RETURN(false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_cancel", 							  DO_FUNCTION_SET_VAR("FunctionLookUpObjects", "system", "", null)
     																					, DO_FUNCTION_SET_VAR("FunctionLookUpObjects", "subSystem", "", null)	
     																					, DO_FUNCTION_SET_VAR("FunctionLookUpObjects", "typeObject", 0, null)	
    																					, DO_FUNCTION_SET_VAR("FunctionLookUpObjects", "idObject", "", null)	
     				                                                                    , DO_FUNCTION_RETURN(false));
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method
    
  } 
