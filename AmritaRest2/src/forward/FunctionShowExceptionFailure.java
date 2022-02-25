package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.PrintWriter;
import java.io.StringWriter;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import enums.EnumForwardBorderType;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumLanguage;
import forward.ForwardFunction;

/**
 * Shows last exception failure informations<br>
 * <p>
 * @author Amrita
 *
 */
public class FunctionShowExceptionFailure extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionShowExceptionFailure() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
 		
    	PARM_REQUIRED("systemObjectCaller", new ForwardSystem());

   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("ShowExceptionFailure", "Exception Failure Info", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH);
     	
        BEGIN_FORM("FormMessages", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.CONTAINER, EnumForwardLayout.BORDER_LAYOUT,  "paExceptionInfo", "paStack", "paButtons", "", "");
	  	    	PANELS_STRUCTURE("paExceptionInfo", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	    	PANELS_STRUCTURE("paStack", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
	  	    	PANELS_STRUCTURE("paButtons", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

  		BEGIN_PANEL_DETAIL_CONTENT("paExceptionInfo", "Exception Info");
  		    COMPONENT(new JLabel(), "lb_title", 0, "EXCEPTION FAILURE", 600, 50); 
	  		COMPONENT(new JLabel(), "lb_excpName", 1, "Exception Name", 200); 
		  	COMPONENT(new JTextField(), "tf_excpName", 1, 400); 	
	  		COMPONENT(new JLabel(), "lb_excpCause", 2, "Exception Cause", 200); 
		  	COMPONENT(new JTextField(), "tf_excpCause", 2, 400); 	
	  		COMPONENT(new JLabel(), "lb_excpDesc", 3, "Exception Description", 200); 
		  	COMPONENT(new JTextField(), "tf_excpDesc", 3, 400); 	
	  		COMPONENT(new JLabel(), "lb_methodClassName", 4, "Method Class Name", 200); 
		  	COMPONENT(new JTextField(), "tf_methodClassName", 4, 400); 
	  		COMPONENT(new JLabel(), "lb_methodName", 5, "Method Name", 200); 
		  	COMPONENT(new JTextField(), "tf_methodName",5, 400); 		
		END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paStack", "Stack Area");
			COMPONENT(new JTextArea(), "ta_stackArea", 1, 20, 600, 600, 300, false);
	  	END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paButtons", "Buttons");
	 	  	COMPONENT(new JButton(), "bt_ok", 0, "Ok", 200);	
	 	END_PANEL_DETAIL_CONTENT();

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
        END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
            /* Modifica proprietà direttamente su oggetti swing componente del modello */
      		getJPanel("MainContainer").setPreferredSize(new Dimension(600, 500));		 
           	
          	// Modifica font/colore label di titolo
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 22));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
        	
 	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("paStack", EnumForwardBorderType.TITLED_LINE, "Stack trace", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, Color.BLUE, new Font("SERIF", Font.BOLD, 15), Color.RED, 1, true, 0, 0, 0, 0);
     
         /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
         	ON_EVENT(EnumForwardEvent.ON_FUNCTION_INITIAL, "",  DO_EXEC_METHOD("populatePanels"));
      		ON_EVENT(EnumForwardEvent.ON_CLICK, "bt_ok", 	    DO_FUNCTION_RETURN(false));
     	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method

     	
    /* Popolamento info di accesso */
    public int populatePanels(ForwardSystem s, ForwardFunction f) {
 
    	ForwardSystem forwardSystemCaller = null;				//
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
       	String reusableMethodClassName = "";					// Nome classe dove è definito il metodo riusabile
    	String reusableMethodName = "";							// Nome metodo riusabile in esecuzione o eseguito
    	Exception reusableMethodException = null;				// Exception con cui il metodo è terminato in  modo anormale
 
    	// Info da exception
//    	forwardSystemCaller = s.getSystemCaller();											// Prefereibile usare il parametro
    	forwardSystemCaller = (ForwardSystem) getParmValueRequired("systemObjectCaller");
        reusableMethodClassName = forwardSystemCaller.getReusableMethodClassName();
       	reusableMethodName = forwardSystemCaller.getReusableMethodName();
       	reusableMethodClassName = forwardSystemCaller.getReusableMethodClassName();
       	reusableMethodException = forwardSystemCaller.getReusableMethodException();
      
       	// Carico pannello
    	f.setValueControlGui("tf_methodClassName", reusableMethodClassName);
    	f.setValueControlGui("tf_methodName", reusableMethodName);
    	f.setValueControlGui("tf_excpName", reusableMethodException.getClass().getSimpleName());
    	f.setValueControlGui("tf_excpCause", reusableMethodException.getCause().toString());
       	f.setValueControlGui("tf_excpDesc", reusableMethodException.toString());
    	
    	// Accodo stack area
		s.getSystemCaller().getReusableMethodException().printStackTrace(pw);
		String stackTraceNormalized = sw.toString();
		f.getJTextArea("ta_stackArea").append(stackTraceNormalized + "\n");
		f.getJTextArea("ta_stackArea").selectAll();
		f.getJTextArea("ta_stackArea").setCaretPosition(f.getJTextArea("ta_stackArea").getDocument().getLength());  //Make sure the new text is visible, even if there was a selection in the text area.
       	return 0;
      }
  } 
