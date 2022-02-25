package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import analyzer.DataBaseItemDescriptor;
import analyzer.DataBaseStatusDetailed;
import utilities.ReflectionManager;
import enums.EnumForwardBorderType;
import enums.EnumForwardComponent;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardLayout;
import enums.EnumForwardLogicalDataViewControl;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumLanguage;
import forward.ForwardFunction;
import forward.ForwardLogicalDataView.InnerForAnyEach;
/**
 * Shows errors and informations messages detected by application function.<br>
 * <p>
 * @author Amrita
 *
 */
public class FunctionShowLdvAccess extends ForwardFunction {
	
	// Variabili di istanza
	ForwardLogicalDataView forwardLogicalDataView = null;

	
	
    /* Costruttore vuoto */
	public FunctionShowLdvAccess() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
       
 	   	PARM_REQUIRED("systemObjectCaller", new ForwardSystem());
 	   	PARM_REQUIRED("ldvObject", new LdvDummy());
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("ShowLdvAccess", "Logical data View Access Info", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH);
     	
        BEGIN_FORM("FormMessages", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "", "paCenter", "", "", "");
		        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paCenter", "Panels container");
	  		COMPONENT(new JLabel(), "lb_title", 0, "LOGICA DATA VIEW ACCESS INFO", 600, 50); 
	  		COMPONENT(new JPanel(), "paAccessInfo", 1);   	// subpanel
	  		COMPONENT(new JPanel(), "paSqlInfo", 2);   		// subpanel
	  		COMPONENT(new JPanel(), "paParameters", 3);   	// subpanel
	  		COMPONENT(new JPanel(), "paColumns", 4);   		// subpanel
	  		COMPONENT(new JPanel(), "paButtons", 6);   		// subpanel
		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paAccessInfo", "Access Info");
	  		COMPONENT(new JLabel(), "lb_ldvName", 0, "Logical Data View Name", 250); 
		  	COMPONENT(new JTextField(), "tf_ldvName", 0, 350); 		
	 		COMPONENT(new JLabel(), "lb_ldvClass", 1, "Logical Data View Class", 250); 
		  	COMPONENT(new JTextField(), "tf_ldvClass", 1, 350); 
			COMPONENT(new JLabel(), "lb_ldvStatus", 3, "Status", 250); 
			COMPONENT(new JTextField(), "tf_ldvStatus", 3, 350); 
			COMPONENT(new JLabel(), "lb_ldvRowsCount", 4, "Rows count", 246); 
			COMPONENT(new JTextField(), "tf_ldvRowsCount", 4, 50); 		
	  	END_PANEL_DETAIL_CONTENT();
	  	
	  	BEGIN_PANEL_DETAIL_CONTENT("paSqlInfo", "Sql Info");
	  		COMPONENT(new JLabel(), "lb_dataBaseOperation", 0, "Data Base Operation", 250); 
		  	COMPONENT(new JTextField(), "tf_dataBaseOperation", 0, 350); 		
	  		COMPONENT(new JLabel(), "lb_dataBaseStatusOperation", 1, "Data Base Status Operation", 250); 
		  	COMPONENT(new JTextField(), "tf_dataBaseStatusOperation", 1, 350); 		
	  		COMPONENT(new JLabel(), "lb_sqlErrorCode", 2, "Sql Error Code", 250); 
		  	COMPONENT(new JTextField(), "tf_sqlErrorCode", 2, 350); 		
	 		COMPONENT(new JLabel(), "lb_sqlStatus", 3, "SqlStatus", 250); 
		  	COMPONENT(new JTextField(), "tf_sqlStatus", 3, 350); 		
			COMPONENT(new JLabel(), "lb_sqlWarning", 4, "SqlWaring", 250); 
		  	COMPONENT(new JTextField(), "tf_sqlWarning", 4, 350); 		
			COMPONENT(new JTextArea(), "ta_sqlString", 6, 10, 500, 600, 100, false);
	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paParameters", "Parameters");
	  		COMPONENT(new JTable(), "tb_parameters", 0, 600, 360, true, JTable.AUTO_RESIZE_OFF);
	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paColumns", "Columns exposed");
  			COMPONENT(new JTable(), "tb_columns", 0, 600, 460, true, JTable.AUTO_RESIZE_OFF);
	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paButtons");
	 	  	COMPONENT(new JButton(), "bt_ok", 0, "Ok", 200);	
	  	    COMPONENT(Box.class, Box.createHorizontalGlue(), EnumForwardComponent.JBoxGlueHorizontal, 0, 0, 0); 
	 	END_PANEL_DETAIL_CONTENT();

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
        END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
            /* Modifica proprietà direttamente su oggetti swing componente del modello */
      		getJPanel("MainContainer").setPreferredSize(new Dimension(600, 600));	
    		getJPanel("paAccessInfo").setPreferredSize(new Dimension(600, 110));	
       		getJPanel("paSqlInfo").setPreferredSize(new Dimension(600, 200));	
      		getJPanel("paParameters").setPreferredSize(new Dimension(600, 100));	
       		getJPanel("paColumns").setPreferredSize(new Dimension(600, 200));	
       		getJPanel("paButtons").setPreferredSize(new Dimension(600, 25));	
        		
          	// Modifica font/colore label di titolo
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 22));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
        	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paAccessInfo", EnumForwardBorderType.TITLED_LINE, "Info access", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paSqlInfo", EnumForwardBorderType.TITLED_LINE, "Info Sql", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paParameters", EnumForwardBorderType.TITLED_LINE, "Required parameters", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paColumns", EnumForwardBorderType.TITLED_LINE, "Columns exposed", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
      
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_parameters"
        	    , new String[]{"parmName", "parmType", "parmValue"}									// Nome campo colonne
	    		, new String[]{"Name", "Type", "Value"}												// Header colonne
                , new Class[]{String.class,String.class, String.class}								// Tipo colonne
			    , new String[]{"", "", "", ""}														// Tooltip di header colonne
			    , new boolean[]{false, false, false}												// Colonne editabili
			    , new boolean[]{false, false, true}													// Colonne resizabili
				, new int[]{150, 200, 300}															// Width colonne
			    , new TableCellRenderer[]{null, null, null, null}									// Renderer colonne
			    , new TableCellEditor[]{null, null, null, null}										// Editor colonne
			    , new Object[][]{}																	// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_parameters", "tb_parametersBound", null);  

        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_columns"
        	    , new String[]{"ldvEntry", "ldvEntity", "ldvRuleTableNum", "ldvCol", "ldvColType", "ldvColValue", "ldvSqlTable", "ldvSqlCol", "ldvSqlType"} 		// Nome campo colonne
	    		, new String[]{"Entry", "Entity", "Rule Tb", "Column name", "Type", "Value", "Sql Table", "Sql Column", "Sql Type"}								// Header colonne
                , new Class[]{String.class, String.class, String.class, String.class, String.class,String.class, String.class,String.class, String.class}			// Tipo colonne
			    , new String[]{"", "", "", "", "", "", "", "", ""}									// Tooltip di header colonne
			    , new boolean[]{false, false, false, false, false, false, false, false, false}		// Colonne editabili
			    , new boolean[]{true, true, false, true, true, true, true, true, true}				// Colonne resizabili
				, new int[]{150, 130, 50, 140, 40, 50, 70, 100, 70}									// Width colonne
			    , new TableCellRenderer[]{null, null, null, null, null, null, null, null, null}		// Renderer colonne
			    , new TableCellEditor[]{null, null, null, null, null, null, null, null, null}		// Editor colonne
			    , new Object[][]{}																	// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_parameters", "tb_parametersBound", null);  
         

        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 
  			ON_EVENT(EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION,  "",	DO_FUNCTION_START_SHOW_EXCEPTION_ERROR());
        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "",  			DO_EXEC_METHOD("functionInitial") 
        													 ,  			DO_EXEC_METHOD("populatePanels") 
   				);
      		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ok", 	    			DO_FUNCTION_RETURN(false));
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method

    /* Gestione popolamento tabella parametri a partire dai dati di system del chiamante */
    public int functionInitial(ForwardSystem s, ForwardFunction f) {
    	Object objLdv = null;
    	objLdv = this.getParmValueRequired("ldvObject");
    	if (objLdv == null) {return 0;}
    	if (!(objLdv instanceof ForwardLogicalDataView)) {return 0;}
     	this.forwardLogicalDataView = (ForwardLogicalDataView) objLdv;
       	return 0;
    }
    	
    /* Popolamento info di accesso */
    public int populatePanels(ForwardSystem s, ForwardFunction f) {
    	ForwardTableModel tableModelParameters = null;
    	ForwardTableModel tableModelColumns = null;
    	DataBaseItemDescriptor dataBaseItemDescriptor = null;
    	ArrayList<Object> al_rowToAppend = null;
    	Object columnValue = null;
    	Object enumConstant = null;
    	String status = "";
      	String sqlColumn = "";
      	String sqlType = "";
    	
     	if (this.forwardLogicalDataView == null) {return 0;}
 
		ACTION(DO_TABLE_DELETE_ALL_ROWS("tb_parameters"), DO_TABLE_DELETE_ALL_ROWS("tb_columns"));
     	
		// Panel paAccessInfo
       	this.setValueControlGui("tf_ldvName", this.forwardLogicalDataView.getName());
    	this.setValueControlGui("tf_ldvClass", this.forwardLogicalDataView.getClass().getName());
    	int rc = this.forwardLogicalDataView.getReturnCode();
    	enumConstant = ReflectionManager.getEnumeration(EnumForwardLogicalDataViewControl.class, rc);
    	if (enumConstant != null) {status = enumConstant.toString();}
      	this.setValueControlGui("tf_ldvStatus", status);
      	this.setValueControlGui("tf_ldvRowsCount", this.forwardLogicalDataView.getCountRows()+"");
  
		// Panel paSqlInfo
     	if (this.forwardLogicalDataView.getDbs() != null) {
           	this.setValueControlGui("tf_dataBaseOperation", this.forwardLogicalDataView.getDbs().getTypeOperation().toString());
           	this.setValueControlGui("tf_dataBaseStatusOperation", this.forwardLogicalDataView.getDbs().getStatusOperation().toString());
           	this.setValueControlGui("tf_sqlErrorCode", new Integer(this.forwardLogicalDataView.getDbs().getSqlErrorCode()).toString());
           	this.setValueControlGui("tf_sqlStatus", this.forwardLogicalDataView.getDbs().getSqlStatus());
          	this.setValueControlGui("tf_sqlWarning", this.forwardLogicalDataView.getDbs().getWarningMessage());
          	this.setValueControlGui("ta_sqlString", this.forwardLogicalDataView.getSql());
		}

      	// Panel paParameters
     	tableModelParameters = getPanelComponent("tb_parameters").getTableModel();
     	
      	// Scan  parametri richiesti
     	for (String parmNameRequired : this.forwardLogicalDataView.getParmNamesRequired()) {
          	al_rowToAppend = new ArrayList<Object> ();
          	al_rowToAppend.add(parmNameRequired);
          	al_rowToAppend.add(this.forwardLogicalDataView.getValue(parmNameRequired).getClass().getSimpleName());
          	al_rowToAppend.add(this.forwardLogicalDataView.getValue(parmNameRequired).toString());
          	tableModelParameters._appendRow(al_rowToAppend);
  		}
		
      	// Panel paColumns
     	tableModelColumns = getPanelComponent("tb_columns").getTableModel();
    	String typeEntry = "";
    	String entity = "";
    	String typeEntryPrec = "";
    	String entityPrec = "";
    	
      	// Scan  forAny/Each
     	for (InnerForAnyEach innerForAnyEach : this.forwardLogicalDataView.getForAnyEachAll()) {
           	// Scan colonne esposte
          	for (String columnNameExposed : this.forwardLogicalDataView.getColumnNamesExposed(innerForAnyEach.entityNameAs)) {
              	al_rowToAppend = new ArrayList<Object> ();
               	typeEntry = "";
               	if (!innerForAnyEach.typeEntry.toString().equals(typeEntryPrec) || innerForAnyEach.typeEntry == EnumForwardLogicalDataViewControl.LDV_FOR_ANY_RULE_TABLE) {
					typeEntryPrec = innerForAnyEach.typeEntry.toString();
					typeEntry = innerForAnyEach.typeEntry.toString().substring(4);
				}
              	entity = "";
                if (!innerForAnyEach.entityName.equals(entityPrec)) {
             		entityPrec = innerForAnyEach.entityName;
             		entity = innerForAnyEach.entityName;
 				}
            	columnValue = this.forwardLogicalDataView.getValue(columnNameExposed);
            	al_rowToAppend.add(typeEntry);
             	al_rowToAppend.add(entity);
             	al_rowToAppend.add(innerForAnyEach.ruleTableNum);
           	    al_rowToAppend.add(columnNameExposed);
               	al_rowToAppend.add(columnValue.getClass().getSimpleName());
             	al_rowToAppend.add(columnValue);
               	dataBaseItemDescriptor = this.forwardLogicalDataView.getColumnDescriptor(columnNameExposed);
               	al_rowToAppend.add(dataBaseItemDescriptor.getDbTable());
              	sqlColumn = "";
              	sqlType = "";
             	if (dataBaseItemDescriptor != null) {
             		sqlColumn = dataBaseItemDescriptor.getDbColumn();
             		sqlType = dataBaseItemDescriptor.getDbColumnType().toString();
				}
            	al_rowToAppend.add(sqlColumn);
            	al_rowToAppend.add(sqlType);
             	tableModelColumns._appendRow(al_rowToAppend);
			}
     	}
     	
     	

     	return 0;
      }
  } 
