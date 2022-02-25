package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import utilities.ReflectionManager;
import enums.EnumDataItemGeneric;
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

public class FunctionRuleTableStructure extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionRuleTableStructure() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
       
       	PARM_REQUIRED("language", new Integer(0));
      	PARM_RETURNED("numTable", new Integer(0));
      	
       	VAR("typeItemSelectedEnum", EnumDataItemGeneric.NOT_ASSIGNED);
     	VAR("typeItemSelectedOrdinal", new Integer(0));
      	VAR("functionLanguage", EnumLanguage.ENGLISH);
//     	VAR("sqlWhere", new String());
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("LookUpRuleTableStructure", "Rule Table structure", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH, EnumForwardOption.FUNCTION_CENTER_FRAME_OWNER
//																																              , EnumForwardOption.FUNCTION_UNDECORATED
																															  );
     	
        BEGIN_FORM("FormRuleTableStructure", EnumForwardOption.FORM_TYPE_START); 
        
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paNorth", "paCenter", "paSouth", "", "");
	   	        PANELS_STRUCTURE("paNorth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        
	   	        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_VERTICAL, "paTable", "paStructure");
	     		   PANELS_STRUCTURE("paTable", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_VERTICAL, "paTableBody", "paTableButtons");
	     		      PANELS_STRUCTURE("paTableBody", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		     		  PANELS_STRUCTURE("paTableButtons", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		          
		     	   PANELS_STRUCTURE("paStructure", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_VERTICAL, "paStructureBody", "paStructureButtons");
	     		      PANELS_STRUCTURE("paStructureBody", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BOX_LAYOUT, ForwardForm.AXIS_HORIZONTAL, "paStructureColumns", "paStructureDetail");
		     		     PANELS_STRUCTURE("paStructureColumns", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		     		     PANELS_STRUCTURE("paStructureDetail", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		     		  PANELS_STRUCTURE("paStructureButtons", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		     		  
		     	   PANELS_STRUCTURE("paSouth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "RULE TABLE STRUCTURE", 600, 50); 
 	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paSouth");
	 	  	COMPONENT(new JButton(), "bt_return", 0, "Return", 200);
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paTableBody");
	  		COMPONENT(new JTable(), "tb_ruleTable", 0, 600, 150, true, JTable.AUTO_RESIZE_OFF);
			COMPONENT(new JLabel(), "lb_numTable", 1, "Num table", 70); 
			COMPONENT(new JFormattedTextField(), "numTableDetail", 1, new Integer(0), 50);
			COMPONENT(new JCheckBox(), "cx_tableSystem", 1, "System", 200);
			COMPONENT(new JTextField(), "tableDescDetail", 2, 600);
  		END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paStructureColumns");
	  		COMPONENT(new JTable(), "tb_columsTable", 0, 180, 150, true, JTable.AUTO_RESIZE_OFF);
		END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paStructureDetail");
	 		COMPONENT(new JLabel(), "lb_columnName", 0, "Column name", 100); 
	 		COMPONENT(new JTextField(), "idItem", 0, 200);
	 		COMPONENT(new JLabel(), "lb_typeItem", 1, "Type", 100); 
	 		COMPONENT(new JComboBox(), "cb_typeItem", 1, 300); 
	 		COMPONENT(new JLabel(), "lb_decsShort", 2, "Desc Short", 100); 
	 		COMPONENT(new JTextField(), "descShort", 2, 200);
	 		COMPONENT(new JLabel(), "lb_dedcLong", 3, "Desc Long", 100); 
	 		COMPONENT(new JTextField(), "descLong", 3, 300);
	 		COMPONENT(new JLabel(), "lb_posItem", 4, "Position", 100); 
	 		COMPONENT(new JFormattedTextField(), "posItem", 4, new Integer(0), 30);
	 		COMPONENT(new JLabel(), "lb_lngBytes", 5, "Length", 100); 
	 		COMPONENT(new JFormattedTextField(), "lngBytes", 5, new Integer(0), 30);
	 		COMPONENT(new JLabel(), "lb_numIntDec", 6, "Int/Dec", 100); 
	 		COMPONENT(new JFormattedTextField(), "numInt", 6, new Integer(0), 30);
	 		COMPONENT(new JFormattedTextField(), "numDec", 6, new Integer(0), 30);
  		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paTableButtons", "Table buttons");
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_insertTable", 0, "Insert", 200);
	 	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_deleteTable", 0, "Delete", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_updateTable", 0, "Update", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
		END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paStructureButtons", "Structure buttons");
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_insertStructure", 0, "Insert", 200);
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_deleteStructure", 0, "Delete", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_updateStructure", 0, "Update", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	  	COMPONENT(new JButton(), "bt_appendStructure", 0, "Append", 200);	
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
	 		getJPanel("paTable").setBackground(Color.WHITE);
	 		getJPanel("paTable").setPreferredSize(new Dimension(600, 200));		 
	 		getJPanel("paTable").setMaximumSize(new Dimension(600, 200));		 
          	
          	// Modifica font/colore label di titolo e descrizione tabella
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 25));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setAlignmentX(JLabel.CENTER_ALIGNMENT);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
          	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paTable", EnumForwardBorderType.TITLED_LINE, "Table", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paStructure", EnumForwardBorderType.TITLED_LINE, "Structure", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("MainContainer", Color.BLACK, 1, true);		// Line border
     
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_ruleTable"
        	    , new String[]{"numTable", "tableDesc", "tableSystem"}		// Nome campo colonne
	    		, new String[]{"Id", "Title", "System"}						// Header colonne
                , new Class[]{Integer.class, String.class, Boolean.class}	// Tipo colonne
			    , new String[]{"", "", ""}									// Tooltip di header colonne
			    , new boolean[]{false, false, false}						// Colonne editabili
			    , new boolean[]{false, false, false}						// Colonne resizabili
				, new int[]{30, 480, 50}									// Width colonne
			    , new TableCellRenderer[]{null, null, null}					// Renderer colonne
			    , new TableCellEditor[]{null, null, null}					// Editor colonne
			    , new Object[][]{}											// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_ruleTable", "tb_ruleTableBound", null);  

        DATA_VALUES(new JTable(), "tb_columsTable"
        	    , new String[]{"columnName"}		// Nome campo colonne
	    		, new String[]{"Column"}			// Header colonne
                , new Class[]{String.class}			// Tipo colonne
			    , new String[]{""}					// Tooltip di header colonne
			    , new boolean[]{false}				// Colonne editabili
			    , new boolean[]{false}				// Colonne resizabili
				, new int[]{180}					// Width colonne
			    , new TableCellRenderer[]{null}		// Renderer colonne
			    , new TableCellEditor[]{null}		// Editor colonne
			    , new Object[][]{}					// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_columsTable", "tb_columsTableBound", null);  

        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
		    ON_EVENT(EnumForwardEvent.ON_LDV_ERROR_EXECUTION, "", 						  DO_FUNCTION_START_SHOW_LDV_ERROR());

        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "", 					 		  DO_FUNCTION_GET_LANGUAGE("functionLanguage")
        																				, DO_LDV_CREATE("LdvReadSetRuleTable")
  			      																		, DO_LDV_CREATE("LdvReadSetIndexTableHeader")
  			      																		, DO_LDV_CREATE("LdvReadSetStructureTable")
  			      																		, DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "functionLanguage")
			      																		, DO_LDV_SET_LANGUAGE("LdvReadSetIndexTableHeader", "functionLanguage")
	       																				, DO_COMBOBOX_POPULATE_FROM_DB("cb_typeItem", "LdvReadSetRuleTable", 14, "descItem")
	       																				, DO_EXEC_METHOD("selectCurrentComboboxTypeItem")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
															        			        , DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetIndexTableHeader", EnumTable.EnumTable.getNumTable())
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetIndexTableHeader")
   				   );
 
    		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_ruleTable",    		  DO_TABLE_POPULATE_SET_OBJECT_BOUND("tb_ruleTable", "LdvReadSetIndexTableHeader", "idItem"));
      		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_BEFORE_ADD_ELEMENT, "cb_typeItem",      DO_COMBOBOX_POPULATE_SET_OBJECT_BOUND("cb_typeItem", "LdvReadSetRuleTable", "keyVal"));
 
    		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_ruleTable",             DO_COMPONENT_SET_VALUE("numTableDetail", "numTable")
    																				    , DO_COMPONENT_SET_VALUE("tableDescDetail", "tableDesc")
    																				    , DO_COMPONENT_SET_VALUE("cx_tableSystem", "tableSystem")
															        			        , DO_TABLE_DELETE_ALL_ROWS("tb_columsTable")
															        			        , DO_LDV_SET_FIELD("LdvReadSetStructureTable", "system", "*", null)
															        			        , DO_LDV_SET_FIELD("LdvReadSetStructureTable", "subSystem", "*", null)
															        			        , DO_LDV_SET_FIELD("LdvReadSetStructureTable", "numTable", "numTable")
 																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_columsTable", "LdvReadSetStructureTable")

    				);
    		ON_EVENT(EnumForwardEvent.ON_COMBOBOX_SELECTED_ITEM, "cb_typeItem",           DO_COMBOBOX_GET_ORDINAL("cb_typeItem", "typeItemSelectedOrdinal")
   															        			        , DO_EXEC_METHOD("setTypeItemSelectedEnum")
	    			);
    		
       		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_insertTable", 			 			  DO_LDV_DELETE("LdvRuleTableHeaderCrud", "TBHD", "#DeleteTableStruct")
       																					, DO_LDV_DELETE("LdvRuleTableHeaderCrud", "TBDTD", "#DeleteTableData")
       																					, DO_LDV_INSERT("LdvRuleTableHeaderCrud", "TBHD", "#insertTableStruct")
       																					, DO_LDV_INSERT("LdvRuleTableHeaderCrud", "TBDT", "#insertTableData")
       																					, DO_DIALOG_START_INFORMATION("insTable1", "Information message", "Table succesfully inserted", JOptionPane.DEFAULT_OPTION)
       				);
       		ON_EVENT(EnumForwardEvent.ON_LDV_INSERT_DUPLICATE, "#insertTable", 			  DO_DIALOG_START_INFORMATION("insTable2", "Information message", "Table duplicated", JOptionPane.DEFAULT_OPTION));
    		
    		
    		
       		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_return", 			 				  DO_FUNCTION_RETURN(false));
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method
    
 	
    /* Seleziona l'item della comboBox al valore corrente */
    public int selectCurrentComboboxTypeItem(ForwardSystem s, ForwardFunction f) {
 
    	EnumDataItemGeneric enumTypeItem = null;
      	int ordinal = 0;
      	enumTypeItem = (EnumDataItemGeneric) getVarValue("typeItemSelectedEnum"); 
     	ordinal = enumTypeItem.ordinal();
        ACTION( DO_COMBOBOX_SELECT_ITEM("cb_typeItem", ordinal));
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
