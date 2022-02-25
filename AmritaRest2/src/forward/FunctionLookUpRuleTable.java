package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import utilities.StringService;
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

public class FunctionLookUpRuleTable extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionLookUpRuleTable() {
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
      	
     	VAR("numTableString", new String(""));
      	 
     	
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
	  		COMPONENT(new JLabel(), "lb_title", 0, "RULE TABLE LOOKUP", 600, 60); 
	 		COMPONENT(new JLabel(), "lb_tableCode", 1, "Table Code", 100); 
	 		COMPONENT(new JFormattedTextField(), "ff_numTable", 1, new Integer(0), 20);
	 		COMPONENT(new JLabel(), "descTable", 1, "", 300); 		
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
          	
	    	getJLabel("descTable").setForeground(Color.BLUE);
	    	getJLabel("descTable").setFont(getJButton("descTable").getFont().deriveFont(Font.BOLD));
	    	getJFormattedTextField("ff_numTable").setEnabled(false);
	    	getJFormattedTextField("ff_numTable").setFont(getJButton("ff_numTable").getFont().deriveFont(Font.BOLD));
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
				, new int[]{50, 529}													// Width colonne
			    , new TableCellRenderer[]{null, null}									// Renderer colonne
			    , new TableCellEditor[]{null, null}										// Editor colonne
			    , new Object[][]{}														// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_ruleTable", "tb_ruleTableBound", null);  
         
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 
        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "", 							  DO_FUNCTION_SET_GUI_CONTROL("ff_numTable", "numTable")
        			                                                                    , DO_TABLE_DELETE_ALL_ROWS("tb_ruleTable")
																			     	    , DO_TABLE_POPULATE_FROM_DB("tb_ruleTable", "LdvReadSetRuleTable")
																			     	    , DO_LDV_RUN("LdvReadItemRuleTable")
																			     	    , DO_LDV_READFIRST("LdvReadItemRuleTable")
																			     	    , DO_LDV_SET_FUNCTION_GUI_CONTROL("LdvReadItemRuleTable", "descItem", "descTable")
   				   );
     		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvReadItemRuleTable", 	  DO_LDV_SET_LANGUAGE("LdvReadItemRuleTable", "language")
																						, DO_LDV_SET_FIELD("LdvReadItemRuleTable", "numTable", EnumTable.EnumTable.getNumTable(), null)
																						, DO_EXEC_METHOD("setNumTableString")
																						, DO_LDV_SET_FIELD("LdvReadItemRuleTable", "keyVal", "numTableString")
																						, DO_LDV_SET_RULE_TABLE_NUM("LdvReadItemRuleTable", "numTable")
      			   );
     		ON_EVENT(EnumForwardEvent.ON_LDV_BEFORE_EXECUTE, "LdvReadSetRuleTable", 	  DO_LDV_SET_LANGUAGE("LdvReadSetRuleTable", "language")
																						, DO_LDV_SET_FIELD("LdvReadSetRuleTable", "numTable", "numTable")
																						, DO_LDV_SET_RULE_TABLE_NUM("LdvReadSetRuleTable", "numTable")
     			   );
      		ON_EVENT(EnumForwardEvent.ON_TABLE_BEFORE_ADD_ROW, "tb_ruleTable",    		  DO_TABLE_POPULATE_SET_OBJECT_BOUND("tb_ruleTable", "LdvReadSetRuleTable", "keyVal"));
      		ON_EVENT(EnumForwardEvent.ON_TABLE_ROWS_SELECTED, "tb_ruleTable", 	          DO_TABLE_GET_OBJECT_BOUND("tb_ruleTable", "typeObject")
      				                                                                    , DO_EXEC_METHOD("setKeyValNumeric")
      				);
    		ON_EVENT(EnumForwardEvent.ON_DOUBLE_CLICK, "tb_ruleTable", 			 		  DO_FUNCTION_RETURN(false));
    		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_return", 			 				  DO_FUNCTION_RETURN(false));
     		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_cancel", 							  DO_FUNCTION_SET_VAR("FunctionLookUpRuleTable", "keyVal", "", null)
      				                                                                    , DO_FUNCTION_RETURN(false)
     				);
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method
    
 	
    /* Impostazione chiave item restituito come parametro numerico in luogo di alfanumerico*/
    public int setKeyValNumeric(ForwardSystem s, ForwardFunction f) {
    	int keyN = 0;
    	keyN = StringService._getNumericInt(getValueString("keyVal"));
    	setValue("keyValNumeric", new Integer(keyN));
    	return 0;
    }
    	
    /* Impostazione chiave item restituito come parametro numerico in luogo di alfanumerico*/
    public int setNumTableString(ForwardSystem s, ForwardFunction f) {
     	setValue("numTableString", getValueInt("numTable")+"");
    	return 0;
    }
    	
 	
  } 
