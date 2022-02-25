package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import utilities.ReflectionManager;
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

public class FunctionRuleTableIndexLookup extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionRuleTableIndexLookup() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
       
       	PARM_REQUIRED("language", new Integer(0));
      	PARM_RETURNED("numTable", new Integer(0));
      	
       	VAR("languageSelectedEnum", EnumLanguage.ENGLISH);
     	VAR("languageSelectedOrdinal", new Integer(0));
      	VAR("functionLanguage", EnumLanguage.ENGLISH);
      	VAR("systemTablesToGet", new Boolean(true));
     	VAR("applicationTablesToGet", new Boolean(true));
     	VAR("sqlWhere", new String());
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("LookUpRuleTableIndex", "Rule Table index LookUp", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH, EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER
																																  , EnumForwardOption.FUNCTION_UNDECORATED
																															  );
     	
        BEGIN_FORM("FormRuleTable", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paNorth", "paCenter", "paSouth", "", "");
	   	        PANELS_STRUCTURE("paNorth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paSouth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "RULE TABLE INDEX LOOKUP", 600, 80); 
  			COMPONENT(new JLabel(), "lb_language", 1, "Language", 170); 
  			COMPONENT(new JComboBox(), "cb_language", 1, 130); 
			COMPONENT(new JRadioButton(), "rb_systemTablesToGet", 1, "System", true, "GRP1", 200);
			COMPONENT(new JRadioButton(), "rb_applicationTablesToGet", 1, "Application", false, "GRP1", 200);
 	  	END_PANEL_DETAIL_CONTENT();
   		
	  	BEGIN_PANEL_DETAIL_CONTENT("paCenter", "Objects list");
  			COMPONENT(new JTable(), "tb_ruleTable", 0, 730, 360, true, JTable.AUTO_RESIZE_OFF);
		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paSouth");
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
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
	 		getJPanel("paNorth").setBackground(Color.WHITE);
	 		getJPanel("paCenter").setBackground(Color.WHITE);
	 		getJPanel("paSouth").setBackground(Color.WHITE);
           	
          	// Modifica font/colore label di titolo e descrizione tabella
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 25));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setAlignmentX(JLabel.CENTER_ALIGNMENT);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
          	
        	getJRadioButton("rb_systemTablesToGet").setOpaque(true);
        	getJRadioButton("rb_applicationTablesToGet").setOpaque(true);
        	getJRadioButton("rb_systemTablesToGet").setBackground(Color.WHITE);
        	getJRadioButton("rb_applicationTablesToGet").setBackground(Color.WHITE);
         	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paCenter", EnumForwardBorderType.TITLED_LINE, "Items", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("MainContainer", Color.BLACK, 1, true);		// Line border
     
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_ruleTable"
        	    , new String[]{"numTable", "tableDesc", "tableStructured"}		// Nome campo colonne
	    		, new String[]{"Id", "Title", "Structured"}						// Header colonne
                , new Class[]{Integer.class, String.class, Boolean.class}		// Tipo colonne
			    , new String[]{"", "", ""}										// Tooltip di header colonne
			    , new boolean[]{false, false, false}							// Colonne editabili
			    , new boolean[]{false, true, false}								// Colonne resizabili
				, new int[]{50, 450, 70}										// Width colonne
			    , new TableCellRenderer[]{null, null, null}						// Renderer colonne
			    , new TableCellEditor[]{null, null, null}						// Editor colonne
			    , new Object[][]{}												// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_ruleTable", "tb_ruleTableBound", null);  
         
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
//		    ON_EVENT(EnumForwardEvent.ON_LDV_ERROR_EXECUTION, "", 						  DO_FUNCTION_START_SHOW_LDV_ERROR());

        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "", 					 		  DO_FUNCTION_GET_LANGUAGE("functionLanguage")
        																				, DO_LDV_CREATE("LdvReadSetRuleTable")
  			      																		, DO_LDV_CREATE("LdvReadSetIndexTableHeader")
  			      																		, DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "functionLanguage")
			      																		, DO_LDV_SET_LANGUAGE("LdvReadSetIndexTableHeader", "functionLanguage")
	       																				, DO_COMBOBOX_POPULATE_FROM_DB("cb_language", "LdvReadSetRuleTable", 40, "descItem")
	       																				, DO_EXEC_METHOD("selectCurrentComboboxLanguage")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
															        			        , DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetIndexTableHeader", EnumTable.EnumTable.getNumTable())
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetIndexTableHeader")
   				   );
 
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_language",      DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_language", "LdvReadSetRuleTable", "keyVal"));
    		
    		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_language",           DO_COMBOBOX_GET_ORDINAL("cb_language", "languageSelectedOrdinal")
   															        			        , DO_EXEC_METHOD("setLanguageSelectedEnum")
															        			        , DO_LDV_SET_LANGUAGE("LdvReadSetIndexTableHeader", "languageSelectedEnum")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetIndexTableHeader")
	    			);
 
      		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_systemTablesToGet", 	  DO_EXEC_METHOD("setSqlWhere")
      																					, DO_LDV_SET_SQL_WHERE("LdvReadSetIndexTableHeader", "TBHD", "sqlWhere")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
															        			        , DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetIndexTableHeader", EnumTable.EnumTable.getNumTable())
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetIndexTableHeader")
     				);
      		ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_applicationTablesToGet",DO_EXEC_ACTIONS_ON_EVENT(EnumForwardEvent.ON_RADIOBUTTON_CHECKED, "rb_systemTablesToGet"));
    		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_ruleTable",    		  DO_TABLE_POPULATE_SET_OBJECT_BOUND("tb_ruleTable", "LdvReadSetRuleTable", "keyVal"));
    		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "tb_ruleTable", 			 		  DO_FUNCTION_RETURN(false));
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_return", 			 				  DO_FUNCTION_RETURN(false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_cancel", 							  DO_FUNCTION_SET_VAR("FunctionLookUpRuleTable", "keyVal", "", null)
      				                                                                    , DO_FUNCTION_RETURN(false)
     				);
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method
    
 	
    /* Seleziona l'item della comboBox al valore corrente */
    public int selectCurrentComboboxLanguage(ForwardSystem s, ForwardFunction f) {
 
      	EnumLanguage enumFunctionLanguage = null;
      	int ordinal = 0;
      	enumFunctionLanguage = (EnumLanguage) getVarValue("functionLanguage"); 
     	ordinal = enumFunctionLanguage.ordinal();
        ACTION( DO_COMBOBOX_SELECT_ITEM("cb_language", ordinal));
    	return 0;
    }

     /* Impostazione variabile enumeration language */
    public int setLanguageSelectedEnum(ForwardSystem s, ForwardFunction f) {
      	EnumLanguage enumLanguageSelected = null;
      	int ordinal = 0;
      	ordinal = (Integer) getVarValue("languageSelectedOrdinal");
      	enumLanguageSelected = (EnumLanguage) ReflectionManager.getEnumeration(EnumLanguage.class, ordinal);
      	setVarValue("languageSelectedEnum", enumLanguageSelected);
    	return 0;
    }
    
    /* Impostazione where sql condition su testata tabelle*/
    public int setSqlWhere(ForwardSystem s, ForwardFunction f) {
    	String sqlWhere = "";
    	
    	// Tabelle applicative o di sistema
    	if (getValueBoolean("rb_systemTablesToGet")) {
    		sqlWhere = "tableSystem = true";
		} else {
    		sqlWhere = "tableSystem = false";
 		}
    	
		setVarValue("sqlWhere", sqlWhere);
    	return 0;
    }

    
  } 
