package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import enums.EnumForwardBorderType;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumLanguage;
import forward.ForwardFunction;
/**
 * Shows errors and informations messages detected by application function.<br>
 * <p>
 * @author Amrita
 *
 */
public class FunctionShowMessages extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionShowMessages() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
       
    	PARM_REQUIRED("systemObjectCaller", new ForwardSystem());
      	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("MessagesListr", "Panel Controls Messages List", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH);
     	
        BEGIN_FORM("FormMessages", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paNorth", "paCenter", "paSouth", "", "");
	   	        PANELS_STRUCTURE("paNorth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paCenter", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
		        PANELS_STRUCTURE("paSouth", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "ERRORS & INFORMATION MESSAGES", 600, 50); 
   		END_PANEL_DETAIL_CONTENT();
   		
	  	BEGIN_PANEL_DETAIL_CONTENT("paCenter", "Messages list");
  			COMPONENT(new JTable(), "tb_messages", 0, 600, 500, true, JTable.AUTO_RESIZE_OFF);
		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paSouth");
	 	  	COMPONENT(new JButton(), "bt_ok", 0, "Ok", 200);	
	 	END_PANEL_DETAIL_CONTENT();

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
        END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
            /* Modifica proprietà direttamente su oggetti swing componente del modello */
      		getJPanel("MainContainer").setPreferredSize(new Dimension(600, 500));		 
           	
          	// Modifica font/colore label di titol
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 22));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
        	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paCenter", EnumForwardBorderType.TITLED_LINE, "Messages", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
        BORDER("paSouth", Color.RED, 1, true);		// Line border
     
        /* Data source, valori predefiniti e oggetti bound per combo/list/jtable/jtree*/
        DATA_VALUES(new JTable(), "tb_messages"
        	    , new String[]{"dataItem", "code", "shortMessage"}									// Nome campo colonne
	    		, new String[]{"Data Item", "Code", "Short Message"}								// Header colonne
                , new Class[]{String.class,String.class, String.class}								// Tipo colonne
			    , new String[]{"", "", "", ""}														// Tooltip di header colonne
			    , new boolean[]{false, false, false}												// Colonne editabili
			    , new boolean[]{true, false, true}													// Colonne resizabili
				, new int[]{70, 50, 500}															// Width colonne
			    , new TableCellRenderer[]{null, null, null, null}									// Renderer colonne
			    , new TableCellEditor[]{null, null, null, null}										// Editor colonne
			    , new Object[][]{}																	// Nessuna riga di prototipo
                                 
	       );
        DATA_VALUES_BOUND(new JTable(), "tb_messages", "tb_messagesBound", null);  
         
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 
        	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "",  DO_TABLE_DELETE_ALL_ROWS("tb_messages")
															 ,  DO_EXEC_METHOD("populateTableMessages") 
    				);
      		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ok", 	    DO_FUNCTION_RETURN(false));
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method
    
    /* Gestione popolamento tanella a partire dai dati di system del chiamante */
    public int populateTableMessages(ForwardSystem s, ForwardFunction f) {
    	ForwardTableModel tableModelMessages = null;
     	ArrayList<Object> al_rowToAppend = null;
    	
    	// Modello tabella
    	tableModelMessages = getPanelComponent("tb_messages").getTableModel();

     	// Scan  messaggi
    	for (InnerComponentMessage componentMessage : s.getSystemCaller().getMessages()) {
         	al_rowToAppend = new ArrayList<Object> ();
         	al_rowToAppend.add(componentMessage.componentName);
         	al_rowToAppend.add(componentMessage.msgCode);
         	al_rowToAppend.add(componentMessage.msgShortText);
         	tableModelMessages._appendRow(al_rowToAppend);
		}
       	return 0;
    }
    	

  } 
