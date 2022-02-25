package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import utilities.ReflectionManager;
import utilities.StringService;
import enums.EnumForwardBorderType;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardComponent;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumLanguage;
import forward.ForwardFunction;

public class FunctionRuleTableDataLookUp extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionRuleTableDataLookUp() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
       
       	PARM_REQUIRED("language", new Integer(0));
       	PARM_REQUIRED("numTable", new Integer(0));
       	
      	PARM_RETURNED("keyVal", new String(""));
     	PARM_RETURNED("descItem", new String(""));
     	PARM_RETURNED("keyValNumeric", new Integer(0));
      	
       	VAR("languageSelectedEnum", EnumLanguage.ENGLISH);
     	VAR("languageSelectedOrdinal", new Integer(0));
     	VAR("languageSelectedKey", new String());
    	VAR("numTableSelected", new Integer(0));
     	VAR("numTableString", new String(""));
      	VAR("tableNameBound", new String());
     	VAR("tableNumBound", new String());
     	VAR("functionLanguage", EnumLanguage.ENGLISH);
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("LookUpRuleTable", "Rule Table LookUp", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH, EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER
																																  , EnumForwardOption.FUNCTION_UNDECORATED
																															  );
     	
        BEGIN_FORM("FormRuleTable", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paNorth", "paCenter", "paSouth", "", "");
	   	        PANELS_STRUCTURE("paNorth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paSouth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "RULE TABLE DATA LOOKUP", 600, 80); 
  			COMPONENT(new JLabel(), "lb_language", 1, "Language", 170); 
  			COMPONENT(new JComboBox(), "cb_language", 1, 350); 
	 		COMPONENT(new JLabel(), "lb_tableName", 2, "Table name/num", 170); 
 			COMPONENT(new JComboBox(), "cb_tableName", 2, 350); 
 			COMPONENT(new JComboBox(), "cb_tableNum", 2, 50); 
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
          	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paCenter", EnumForwardBorderType.TITLED_LINE, "Items", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("MainContainer", Color.BLACK, 1, true);		// Line border
     
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_ruleTable"
        	    , new String[]{"keyVal", "descItem"}									// Nome campo colonne
	    		, new String[]{"Item", "Value"}											// Header colonne
                , new Class[]{String.class, String.class}								// Tipo colonne
			    , new String[]{"", ""}													// Tooltip di header colonne
			    , new boolean[]{false, false}											// Colonne editabili
			    , new boolean[]{false, false}											// Colonne resizabili
				, new int[]{50, 515}													// Width colonne
			    , new TableCellRenderer[]{null, null}									// Renderer colonne
			    , new TableCellEditor[]{null, null}										// Editor colonne
			    , new Object[][]{}														// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_ruleTable", "tb_ruleTableBound", null);  
         
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 
        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "", 					 		  DO_FUNCTION_GET_LANGUAGE("functionLanguage")
			      																		, DO_LDV_CREATE("LdvReadSetRuleTable")
			      																		, DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "functionLanguage")
	       																				, DO_COMBOBOX_POPULATE_FROM_DB("cb_language", "LdvReadSetRuleTable", 40, "descItem")
															     			      		, DO_COMBOBOX_POPULATE_FROM_DB("cb_tableName", "LdvReadSetRuleTable", 0, "descItem")                                                              
															     			      		, DO_COMBOBOX_POPULATE_FROM_DB("cb_tableNum", "LdvReadSetRuleTable", 0, "keyVal")  
															     			            , DO_EXEC_METHOD("selectCurrentComboboxLanguage")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
															        			        , DO_EXEC_METHOD("setNumTableSelected")
															        			        , DO_COMPONENT_SET_VALUE("cb_tableNum", "numTableString")
															        			        , DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetRuleTable", "numTableSelected")
															        			        , DO_COMPONENT_SET_VALUE("cb_tableNum", "numTableSelected")
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetRuleTable")
   				   );
 
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_language",      DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_language", "LdvReadSetRuleTable", "keyVal"));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_tableName",     DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_tableName", "LdvReadSetRuleTable", "keyVal"));
    		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_tableNum",      DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_tableNum", "LdvReadSetRuleTable", "descItem"));
    		
    		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_language",     	  DO_COMBOBOX_GET_OBJECT_BOUND("cb_language", "languageSelectedKey")
    				                                                                    , DO_COMBOBOX_GET_ORDINAL("cb_language", "languageSelectedOrdinal")
   															        			        , DO_EXEC_METHOD("setLanguageSelectedEnum")
															        			        , DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "languageSelectedEnum")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetRuleTable")
																			     	    , DO_COMBOBOX_POPULATE_FROM_DB("cb_tableNum", "LdvReadSetRuleTable", 0, "keyVal")  
																	                    , DO_COMBOBOX_POPULATE_FROM_DB("cb_tableName", "LdvReadSetRuleTable", 0, "descItem")      
    				);
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_tableName",     	  DO_COMBOBOX_GET_OBJECT_BOUND("cb_tableName", "tableNumBound")
    				                                                                    , DO_COMPONENT_SET_VALUE("cb_tableNum", "tableNumBound")
      				);
     		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_tableNum",     	  DO_COMBOBOX_GET_OBJECT_BOUND("cb_tableNum", "tableNameBound")
     																					, DO_COMPONENT_SET_VALUE("cb_tableName", "tableNameBound")
															        			        , DO_EXEC_METHOD("setNumTableSelected")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
																        			    , DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetRuleTable", "numTableSelected")
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetRuleTable")
     				);
     		
       		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_ruleTable",    		  DO_TABLE_POPULATE_SET_OBJECT_BOUND("tb_ruleTable", "LdvReadSetRuleTable", "keyVal"));
      		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_ruleTable", 	          DO_EXEC_METHOD("setKeyValNumeric")
      				);
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

 	/* Impostazione chiave item restituito come parametro numerico in luogo di alfanumerico*/
    public int setNumTableSelected(ForwardSystem s, ForwardFunction f) {
    	int keyN = 0;
    	keyN = StringService._getNumericInt(getValueString("cb_tableNum"));
    	setValue("numTableSelected", new Integer(keyN));
    	
    	// Valore stringa del numero tabella in parametro di input
    	setVarValue("numTableString", getParmValueRequired("numTable")+"");
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
    

    /* Impostazione chiave item restituito come parametro numerico in luogo di alfanumerico*/
    public int setKeyValNumeric(ForwardSystem s, ForwardFunction f) {
    	int keyN = 0;
    	keyN = StringService._getNumericInt(getValueString("keyVal"));
    	setValue("keyValNumeric", new Integer(keyN));
    	return 0;
    }

 	
  } 
