package forward;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import utilities.DateTimeService;

import analyzer.DataBaseEntityInterface;
import analyzer.DataBaseManager;
import analyzer.DataBaseStatusDetailed;
import analyzer.UserConfiguration;

import entities.EntityUser;
import enums.EnumForwardComponent;
import enums.EnumForwardEvent;
import enums.EnumForwardFunctionModel;
import enums.EnumForwardLayout;
import enums.EnumForwardOption;
import enums.EnumForwardPanelType;
import enums.EnumUserStatus;
import enums.EnumLanguage;
import enums.EnumMessageType;
import exception.ExceptionAmrita;
import exception.ExceptionAmritaAnnotationMissing;
import exception.ExceptionAmritaReflectionError;
import forward.ForwardFunction;

public class FunctionLogin extends ForwardFunction {
	
    /* Costruttore vuoto */
	public FunctionLogin() {
		super();
	}
	
	/**
	 * Function amrita-suite declaration.
	 */
 	public void declare() {
            	
   		/** Dichiarazione funzione */
    	BEGIN_FUNCTION_SPECIFICATION("AmritaLogin", "Amrita Login", EnumForwardFunctionModel.CUSTOM, EnumLanguage.ENGLISH, EnumForwardOption.FUNCTION_UNDECORATED
    			                                                                                                         , EnumForwardOption.FUNCTION_CENTER_SCREEN);
     	
        BEGIN_FORM("FormLogin", EnumForwardOption.FORM_TYPE_START); 
	  	    PANELS_STRUCTURE("MainContainer", EnumForwardPanelType.DETAIL, EnumForwardLayout.BOX_LAYOUT);
        END_FORM();

	  	BEGIN_PANEL_DETAIL_CONTENT("MainContainer", "Pilot");
			COMPONENT(new JPanel(), "paNorth", 0); 
  			COMPONENT(new JPanel(), "paCenter", 1); 
  			COMPONENT(new JPanel(), "paSouth", 2); 
  		END_PANEL_DETAIL_CONTENT();

	  	BEGIN_PANEL_DETAIL_CONTENT("paNorth", "Pilot");
	  		COMPONENT(new JLabel(), "lb_title", 0, "AMRITA LOGIN", 250); 
	  	END_PANEL_DETAIL_CONTENT();
   		
	  	BEGIN_PANEL_DETAIL_CONTENT("paCenter", "Body");
	  		COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), EnumForwardComponent.JBoxRigidArea, 0, 10, 20);  
			COMPONENT(new JLabel(), "lb_user", 1, "User",150); 
  			COMPONENT(new JTextField(), "tf_user", 1, 100); 
			COMPONENT(new JLabel(), "lb_pwd", 3, "Password", 150); 
  			COMPONENT(new JPasswordField(), "pf_pwd", 3, 5); 
	  		COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), EnumForwardComponent.JBoxRigidArea, 2, 10, 20);  
		END_PANEL_DETAIL_CONTENT();
		
	  	BEGIN_PANEL_DETAIL_CONTENT("paSouth");
		    COMPONENT(new JLabel(), "lb_msg", 0, "   ", 250); 
	 	  	COMPONENT(new JButton(), "bt_ok", 1, "OK", 120);
	 	  	COMPONENT(Box.class, Box.createRigidArea(new Dimension(10, 20)), EnumForwardComponent.JBoxRigidArea, 1, 10, 10); 
	 	  	COMPONENT(new JButton(), "bt_cancel", 1, "Cancel", 120);
	 	END_PANEL_DETAIL_CONTENT();

        /* Parametri di aggiustamento a livello di funzione e di completamento info applicative/funzionali*/
     	BEGIN_FUNCTION_TUNING();
        END_FUNCTION_TUNING();
           
        
        /* Parametri di aggiustamento layout con parametri standard java swing, statici o a runtime  */
        BEGIN_LAYOUT_TUNING();
        
          	// Modifica font/colore label di titolo e descrizione tabella
          	getJLabel("lb_title").setFont(getJButton("lb_title").getFont().deriveFont(Font.BOLD, 25));
          	getJLabel("lb_title").setBackground(Color.WHITE);
          	getJLabel("lb_title").setForeground(Color.BLUE);
          	getJLabel("lb_title").setAlignmentX(JLabel.CENTER_ALIGNMENT);
          	getJLabel("lb_title").setHorizontalAlignment(SwingConstants.CENTER);
           	
          	getJLabel("lb_msg").setBackground(Color.WHITE);
          	getJLabel("lb_msg").setOpaque(true);

          	getJLabel("lb_user").setFont(getJLabel("lb_user").getFont().deriveFont(Font.BOLD));
        	getJLabel("lb_pwd").setFont(getJLabel("lb_pwd").getFont().deriveFont(Font.BOLD));

  	    END_LAYOUT_TUNING();
        
        /* Borders */
        BORDER("MainContainer", Color.BLACK, 1, true);		// Line border
     
          
        /* Gestione risposte agli eventi */
        BEGIN_EVENTS();
 		/* Attivazione logica applicativa eventi di sistema  */
  			ON_EVENT(EnumForwardEvent.ON_SYSTEM_EXCEPTION_FUNCTION,  "" , DO_FUNCTION_START_SHOW_EXCEPTION_ERROR());
			ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_cancel", 	DO_FUNCTION_RETURN(true));
 			ON_EVENT(EnumForwardEvent.ON_CLICK,  "bt_ok", 		DO_EXEC_METHOD("startFunction"));
      	END_EVENTS();
 
		
     	END_FUNCTION_SPECIFICATION();
	     	 
    } // end declare method

 	
    /* ---------------------------------------
     * Attivazione funzione associata a user
     * ---------------------------------------
     * 
     * Gli accessi vengono effettuati senza utililizzare le ldv
     * I messaggi vengono letti direttamente dal database
     * L'inizializzazione funzionale NON è ancora stata effettuata
     * 
     * - Controllo esistenza utente
     * - Controllo validità password
     * - Controllo validità funzione da attivare
     * - Segnalazione errori
     * - Attivazione funzione
     */
   	public int startFunction(ForwardSystem s, ForwardFunction f) throws SQLException, ExceptionAmritaAnnotationMissing, ExceptionAmritaReflectionError, ExceptionAmrita {
   		
   		ForwardFunction function = null;
   		EntityUser user = null; 
   		DataBaseEntityInterface dbei = null;
   		DataBaseManager dbm= null;
   		DataBaseStatusDetailed dbs = null;;
  		Connection dbConn = null;
  		UserConfiguration sd = null;
  		String userFunction = "";
  		String msgLocalized = "";
  		String dateAAAAMMDD = "";
  		EnumLanguage enumLanguage = EnumLanguage.ENGLISH;
   		boolean userFound = false;
   		boolean functionUndecorated = false;
   		
   		dbm = s.getDbm();
   		dbs = new DataBaseStatusDetailed();
		dbConn = s.getDbm().getConnection(dbs);
		sd = s.getSystemDefaults();
   		dbei = new DataBaseEntityInterface(sd, dbm, dbConn);

        user = new EntityUser();
   		user.setUserName(getValueString("tf_user"));
   		
   		userFound = dbei.read(user);
   		dbm.releaseConnection(dbConn, dbs);
   		
   		getJTextField("tf_user").setBorder(null);
   		
   		
   		// Utente NON definito, lettura e segnalazione errore
   		if (!userFound) {
   			msgLocalized = getMessage("10000", EnumMessageType.ERROR_INPUT);			// "User non found"
//   			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
   			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1));
			setValue("lb_msg", msgLocalized);
			return 0;
		}
   		
   		// Password errata
   		if (!user.getPwd().equals(getValueString("pf_pwd"))) {
   			msgLocalized = getMessage("10001", EnumMessageType.ERROR_INPUT);			// "Wrong password"
//			getJPasswordField("pf_pwd").setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
			getJPasswordField("pf_pwd").setBorder(BorderFactory.createLineBorder(Color.RED, 1));
			setValue("lb_msg", msgLocalized);
			return 0;
		}
   		
   		// Utente non attivo
   		if (user.getUserStatus() == EnumUserStatus.USER_INACTIVE) {
  			msgLocalized = getMessage("10002", EnumMessageType.ERROR_INPUT);			// "User not active"
// 			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
 			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1));
			setValue("lb_msg", msgLocalized);
			return 0;
		}
   		
   		// Utente sospeso
   		if (user.getUserStatus() == EnumUserStatus.USER_SUSPENDED) {
			msgLocalized = getMessage("10003", EnumMessageType.ERROR_INPUT);			// "User suspended"
// 			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1, false));
 			getJTextField("tf_user").setBorder(BorderFactory.createLineBorder(Color.RED, 1));
			setValue("lb_msg", msgLocalized);
			return 0;
		}
   		
   		// Utente attivo, verfica funzione
 //  		userFunction = user.getFunctionStart();
   		
   		// LOAD funzione per verifica esistenza e opzioni
  		ACTION(DO_FUNCTION_LOAD(userFunction));
   		if (s.getFunctionObject() == null) {
			msgLocalized = getMessage("10004", EnumMessageType.ERROR_INPUT);			// "User StartUp function loading error" 
			setValue("lb_msg", msgLocalized);
			return 0;
		}
  		
   		function = s.getFunctionObject();
   		if (function.getOptions().contains(EnumForwardOption.FUNCTION_UNDECORATED)) {
   			functionUndecorated = true;
		}
   		
   		// Aggiornamento data di primo/ultimo login e contatore login
  		dbm = s.getDbm();
   		dbs = new DataBaseStatusDetailed();
		dbConn = s.getDbm().getConnection(dbs);
		sd = s.getSystemDefaults();
		dbei.setDbConn(dbConn);

		// Counter login e date
//		user.setCntLogin(user.getCntLogin() + 1);
   		dateAAAAMMDD = DateTimeService.getDateFormatted(new Date(), "yyyyMMdd");
   		if (user.getDtFirstLogin().equals("")) {
   			user.setDtFirstLogin(dateAAAAMMDD);
   			user.setDtLastLogin(dateAAAAMMDD);
		} else {
			user.setDtLastLogin(dateAAAAMMDD);
		}
   		
   		// Update e release connection
   		userFound = dbei.update(user);
   		dbm.releaseConnection(dbConn, dbs);  		
 
   		// Ricerca enum corrispondente al linguaggio (locale) user
   		for (EnumLanguage enumLanguageLoop : EnumLanguage.values()) {
			if (enumLanguageLoop.getLocalValue().equals(user.getLanguage())) {
				enumLanguage = enumLanguageLoop;
				break;
			}
		}
   		
   		// Impostazione language in system, verrà propagato alle funzione chiamate
   		s.setActiveLanguage(enumLanguage);
   		
   		// XCTL alla funzione
  		ACTION(DO_FUNCTION_XCTL(userFunction, functionUndecorated));
   		
   		return 0;
   	} 
  } 
