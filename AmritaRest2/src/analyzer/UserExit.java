package analyzer;

import java.sql.Connection;

import enums.EnumUserExit;


/**
	* Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda Turin (ITALY)
    * 
	* <h1>
	* UserExit  
	* </h1>
	*  <p>
	* Vengono definiti in questa classe i metodi che fungono da exit applicativa esterna, in tutti quei casi in cui
	* devono essere reperite informazioni a livello di classificazione sorgenti e oggetti, non recuperabili con
	* le direttive di processo e di sorgente {@link EnumDirectivesSource} e {@link EnumDirectivesProcess}.<br>
	* <br>
	* Tipicamente si tratta di recuperare il nome del sistema e del sottosistema a partire dal nome del sorgente
	* o dell'oggetto ma possono essere definite tipologie di exit differenti.<br>
	* <p>
	* La exit viene attivata dall'analizzatore di script sql {@link AnalyzerSql} <br>
	* a fronte per esempio di Create Table, per determinare il sistema <br>
	* di appartenenza della tabella o di altro oggetto, non determinabile a priori.<br>
	* Infatti in uno script sql possono essere definite + tabelle appartenenti<br>
	* a sistemi/sottosistemi diversi e il sistema/soottosistema di appartenenza può<br>
	* essere dedotto dal nome della tabella, dall'owner, dal tablespace, dal dabname etc.<br>
	* Questo metodo può effettuare qualsiasi operazione java permessa, accedere al file<br>
	* system per recuperare le informazioni necessarie etc.<br>
	* 
	*
	* 
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 07/04/2010
	* @see Analyzer
	* @see EnumDirectivesSource
	* @see EnumDirectivesProcess
	* @see EnumUserExit
	* @see AmritaConstants
*/

@SuppressWarnings("unused")
public class UserExit implements AmritaConstants{
	
	
	/*
	 * 
	 * Costruttore
	 * 
	 */
	public UserExit() {
		super();

	}
	
   /**
     * 
     * Gestisce il tipo di exit richiesto 
     * 
     * @param InnerInfoExit
     */
	public void executeExit(UserExitInfo uei) {

		uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_OK);
		
	    // Valutazione operazione da effettuare		
		switch (uei.getUserExitOperation()) {
		
		        // A fronte di individuazione oggetti da trattare
				case GET_SYSTEM_OWNER:
					// Default
					uei.setSystem("*");
					// Gestione FIAT
					if (uei.getCustomerCode().equals("FIAT")) {
						executeExitFiatSystem(uei);
					} 
					if (uei.getCustomerCode().equals("SANPAOLO")) {
						executeExitSanpaolo(uei);
					} 
					break;
					// A fronte di individuazione oggetti da trattare
				case GET_SUB_SYSTEM_OWNER:
					// Default
					uei.setSubSystem("*");
					if (uei.getCustomerCode().equals("FIAT")) {
						executeExitFiatSubSystem(uei);
					} 					
					if (uei.getCustomerCode().equals("VALBRUNA")) {
						executeExitValbrunaSubSystem(uei);
						uei.setSubSystem(uei.getNameToEvaluate().substring(0, 2));
					} 
					if (uei.getCustomerCode().equals("SANPAOLO")) {
						executeExitSanpaolo(uei);
			     	} 
					break;
						
				// A fronte di individuazione oggetti da trattare
				case GET_SYSTEM_SUB_SYSTEM_OWNER:
					// Default
					uei.setSystem("*");
					uei.setSubSystem("*");
					if (uei.getCustomerCode().equals("FIAT")) {
						executeExitFiatSystem(uei);
						executeExitFiatSubSystem(uei);
					} 
					if (uei.getCustomerCode().equals("SANPAOLO")) {
						executeExitSanpaolo(uei);
					} 
					break;
					
				// A fronte di individuazione oggetti da trattare
				case FILTER_ON_OBJECT_NAME_EVALUATE:
					// Default
					uei.setMatchingFilter(true);
					if (uei.getCustomerCode().equals("FIAT")) {
						executeExitFiatFilter(uei);
					} 
					if (uei.getCustomerCode().equals("SANPAOLO")) {
						executeExitSanpaolo(uei);
					} 
					break;
					
				// Controllo validità nome campo 	     (prefisso, suffisso etc.)
				case CTRL_NAME_FIELD:
					if (uei.getNameToEvaluate().equals("FIELD-NAME-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome label 	     (prefisso, suffisso etc.)
				case CTRL_NAME_LABEL:
					if (uei.getNameToEvaluate().equals("L999-NOT-STD") 
					||  uei.getNameToEvaluate().equals("LABEL-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome paragrafo      (prefisso, suffisso etc.)
				case CTRL_NAME_PARAGRAPH:
					if (uei.getNameToEvaluate().equals("P001-PARAGRAPH-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome paragrafo thru (prefisso, suffisso etc.)
				case CTRL_NAME_PARAGRAPH_THRU:
					if (uei.getActiveSectionName().equals("P004-PARAGRAPH")
					&&  uei.getNameToEvaluate().equals("P004-EX-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome section        (prefisso, suffisso etc.)
				case CTRL_NAME_SECTION:
					if (uei.getNameToEvaluate().equals("S001-SECTION-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome section thru   (prefisso, suffisso etc.)
				case CTRL_NAME_SECTION_THRU:
					if (uei.getActiveSectionName().equals("S007-SECTION")
					&&  uei.getNameToEvaluate().equals("S007-EX-NOT-STD")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				// Controllo validità nome programma      (prefisso, suffisso etc.)
				case CTRL_NAME_PGM:
					if (uei.getNameToEvaluate().equals("COBTST06")) {
						uei.setReturnCode(UserExitInfo.USER_EXIT_RETURN_ERROR);
					}
					break;
					
				case NOT_ASSIGNED:
					break;
		}
		
		
		return;
	}

	/*
	 * Gestione personalizzata per Fiat recupero sistema
	 * Si lascia la classificazione ma si forza a *
	 */
	private void executeExitFiatSystem(UserExitInfo uei) {
		String sqlOwner = "";
		String sqlTableName = "";
		
		sqlOwner = uei.getSqlOwner();
		sqlTableName = uei.getSqlTableName();
		
		///////////////////////////////////////////////
		// Attivazione da analisi script Sql
		///////////////////////////////////////////////
		
		if (uei.getCustomerInfo().equals("SQL-SCRIPT")) {
			// Presente owner o table name: attivazione da analisi singole istruzioni in script Sql ddl
			if (!sqlOwner.equals("") || !sqlTableName.equals("")) {
				// System su owner
				if (!sqlOwner.equals("")) {
					// Caso D RP00 xxx -> KR UK
					if (sqlOwner.length() > 5 && sqlOwner.substring(1, 5).equals("RP00")) {
						uei.setSystem("KR");
						uei.setSystem("*");
						return;
					}
					// Caso normale D ff xxxxx non c'è il systema ma il sottosistema
					return;
				}
				// System su nome tabella
				if (!sqlTableName.equals("")) {
					// Caso T RP00 xxx -> KR UK
					if (sqlTableName.length() > 5 && sqlTableName.substring(1, 5).equals("RP00")) {
						uei.setSystem("KR");
						uei.setSystem("*");
						return;
					}
					// Caso normale TA ff xxxxx non c'è il systema ma il sottosistema
					uei.setSystem("*");
					return;
				}
				// Rimangono i valori di default
				uei.setSystem("*");
				return;
				
			// Owner e table name "": attivazione da individuazione script Sql ddl
			} else {
				uei.setSystem("*");
				return;
			}
		}

		///////////////////////////////////////////////
		// Attivazione da detecting nomi pgm e copy
		///////////////////////////////////////////////

		// Systema su nome copy
		if (uei.getNameToEvaluate().startsWith("C")) {
			uei.setSystem(uei.getNameToEvaluate().substring(1, 3));
			uei.setSystem("*");
			return;
		}
		// Systema su nome copy di DCLGEN
		if (uei.getNameToEvaluate().startsWith("TA")) {
			// Disponibile solo il sottosistema sul nome
			uei.setSystem("*");
			return;
		}
		// Systema su nome pgm planning -> KR UR
		if (uei.getNameToEvaluate().startsWith("RP00") || uei.getNameToEvaluate().startsWith("CRP00")) {
			uei.setSystem("KR");
			uei.setSystem("*");
			return;
		}
		
		// Nome programma di sort con codifica speciale sffnxxxx dove s è il settore/ambito
		// n è numerico xxxx può essere anche tutto numerico
        if (uei.getNameToEvaluate().charAt(3) >= '0' 
        && uei.getNameToEvaluate().charAt(3) <= '9'
        && uei.getLibraryCode().equals("FIAT-SRC-SORT")
        && (uei.getNameToEvaluate().startsWith("G")
        ||  uei.getNameToEvaluate().startsWith("M")	
        ||  uei.getNameToEvaluate().startsWith("R")	
        ||  uei.getNameToEvaluate().startsWith("S")	
        ||  uei.getNameToEvaluate().startsWith("Z")
        || (uei.getNameToEvaluate().startsWith("A") && !(uei.getNameToEvaluate().charAt(1) == 'R')))	// Non era fra quelli forniti
        ) {
        	uei.setSystem(uei.getNameToEvaluate().substring(0, 1));
        	uei.setSystem("*");
        	return;
		}
		
        // Può solo essere questa codifica
		
		// Nome programma aaffxxxx
        if (uei.getNameToEvaluate().length() > 4) {
        	uei.setSystem(uei.getNameToEvaluate().substring(0, 2));
        	uei.setSystem("*");
		}
        return;
					
	}

	/*
	 * Gestione personalizzata per Fiat recupero sottosistema
	 */
	private void executeExitFiatSubSystem(UserExitInfo uei) {
		
		String subSystemException = "";
		String sqlOwner = "";
		String sqlTableName = "";
		
		sqlOwner = uei.getSqlOwner();
		sqlTableName = uei.getSqlTableName();
		
					
		///////////////////////////////////////////////
		// Attivazione da analisi script Sql
		///////////////////////////////////////////////
		
		if (uei.getCustomerInfo().equals("SQL-SCRIPT")) {
			// Presente owner o table name: attivazione da analisi singole istruzioni in script Sql ddl
			if (!sqlOwner.equals("") || !sqlTableName.equals("")) {

				// SubSystem su owner
				if (!sqlOwner.equals("")) {
					// Caso D RP00 xxx -> KR UK
					if (sqlOwner.length() > 5 && sqlOwner.substring(1, 5).equals("RP00")) {
						uei.setSubSystem("UK");
						return;
					}
					// Caso normale D ff xxxxx non c'è il systema ma il sottosistema
					if (sqlOwner.startsWith("D")) {
						uei.setSubSystem(sqlOwner.substring(1, 3));
						return;
					}
					// Rimane il sottosistema di default
					uei.setSubSystem("XY");							// Forzatura sottosistema ALTRI da considerare
					return;
				}
				// SubSystem su nome tabella
				if (!sqlTableName.equals("")) {
					// Caso T RP00 xxx -> KR UK
					if (sqlTableName.length() > 5 && sqlTableName.startsWith("T") && sqlTableName.substring(1, 5).equals("RP00")) {
						uei.setSubSystem("UK");
						return;
					}
					// Caso normale TA ff xxxxx non c'è il systema ma il sottosistema
					if (sqlTableName.startsWith("TA")) {
						uei.setSubSystem(sqlTableName.substring(2, 4));
						return;
					}
					// Rimane il sottosistema di default
					uei.setSubSystem("XY");							// Forzatura sottosistema ALTRi da considerare
					return;
				}
				// Rimangono i valori di default
				uei.setSubSystem("XY");							// Forzatura sottosistema ALTRi da considerare
				return;
				
			// Owner e table name "": attivazione da individuazione script Sql ddl
			} else {
				uei.setSubSystem("*");
				return;
			}
		}

		///////////////////////////////////////////////
		// Attivazione da detecting nomi pgm e copy
		///////////////////////////////////////////////

		// Gestione eccezioni individuazione sottosistema
		subSystemException = getFiatSubSystemException(uei.getNameToEvaluate());
		if (!subSystemException.equals("")) {
			uei.setSubSystem(subSystemException);
			return;
		}
		
		// Sottosistema su nome copy
		if (uei.getNameToEvaluate().startsWith("C")) {
			uei.setSubSystem(uei.getNameToEvaluate().substring(1, 3));
			return;
		}
		// Sottosistema su nome copy di DCLGEN
		if (uei.getNameToEvaluate().startsWith("TA")) {
			uei.setSubSystem(uei.getNameToEvaluate().substring(2, 4));
			return;
		}
		// Sottosistema su nome pgm planning -> KR UK
		if (uei.getNameToEvaluate().startsWith("RP00") || uei.getNameToEvaluate().startsWith("CRP00")) {
			uei.setSubSystem("UK");
			return;
		}
		
		// Nome programma di sort con codifica speciale sffnxxxx dove s è il settore/ambito
		// n è numerico xxxx può essere anche tutto numerico
        if (uei.getNameToEvaluate().charAt(3) >= '0' 
        	&& uei.getNameToEvaluate().charAt(3) <= '9' 
            && uei.getLibraryCode().equals("FIAT-SRC-SORT")
            && (uei.getNameToEvaluate().startsWith("G")
            ||  uei.getNameToEvaluate().startsWith("M")	
            ||  uei.getNameToEvaluate().startsWith("R")	
            ||  uei.getNameToEvaluate().startsWith("S")	
            ||  uei.getNameToEvaluate().startsWith("Z")
            || (uei.getNameToEvaluate().startsWith("A") && !(uei.getNameToEvaluate().charAt(1) == 'R')))	// Non era fra quelli forniti
            ) {
        	uei.setSubSystem(uei.getNameToEvaluate().substring(1, 3));
        	return;
		}
		
        // può solo essere questa codifica
		
		// Nome programma aaffxxxx
        if (uei.getNameToEvaluate().length() > 4) {
        	uei.setSubSystem(uei.getNameToEvaluate().substring(2, 4));
        	// Eccezione
        	if (uei.getSubSystem().equals("BI") 
        	||  uei.getSubSystem().equals("NT") 
        	||  uei.getSubSystem().equals("KZ")) {
        		uei.setSubSystem("XN");
			}
		}
		
        return;
		
		// viene mantenuto il valore di default
	}

	/*
	 * Gestione personalizzata per Fiat recupero sottosistema
	 */
	private void executeExitValbrunaSubSystem(UserExitInfo uei) {
		
		String subSystemException = "";
		String sqlOwner = "";
		String sqlTableName = "";
		
		sqlOwner = uei.getSqlOwner();
		sqlTableName = uei.getSqlTableName();
		
					
		///////////////////////////////////////////////
		// Attivazione da analisi script Sql
		///////////////////////////////////////////////
		
		if (uei.getCustomerInfo().equals("SQL-SCRIPT")) {
			// Presente owner o table name: attivazione da analisi singole istruzioni in script Sql ddl
			if (!sqlOwner.equals("") || !sqlTableName.equals("")) {

				// SubSystem su owner
				if (!sqlOwner.equals("")) {
					// Caso D RP00 xxx -> KR UK
					if (sqlOwner.length() > 5 && sqlOwner.substring(1, 5).equals("RP00")) {
						uei.setSubSystem("UK");
						return;
					}
					// Caso normale D ff xxxxx non c'è il systema ma il sottosistema
					if (sqlOwner.startsWith("D")) {
						uei.setSubSystem(sqlOwner.substring(1, 3));
						return;
					}
					// Rimane il sottosistema di default
					uei.setSubSystem("XY");							// Forzatura sottosistema ALTRI da considerare
					return;
				}
				// SubSystem su nome tabella
				if (!sqlTableName.equals("")) {
					// Caso T RP00 xxx -> KR UK
					if (sqlTableName.length() > 5 && sqlTableName.startsWith("T") && sqlTableName.substring(1, 5).equals("RP00")) {
						uei.setSubSystem("UK");
						return;
					}
					// Caso normale TA ff xxxxx non c'è il systema ma il sottosistema
					if (sqlTableName.startsWith("TA")) {
						uei.setSubSystem(sqlTableName.substring(2, 4));
						return;
					}
					// Rimane il sottosistema di default
					uei.setSubSystem("XY");							// Forzatura sottosistema ALTRi da considerare
					return;
				}
				// Rimangono i valori di default
				uei.setSubSystem("XY");							// Forzatura sottosistema ALTRi da considerare
				return;
				
			// Owner e table name "": attivazione da individuazione script Sql ddl
			} else {
				uei.setSubSystem("*");
				return;
			}
		}

		///////////////////////////////////////////////
		// Attivazione da detecting nomi pgm e copy
		///////////////////////////////////////////////

		// Gestione eccezioni individuazione sottosistema
		subSystemException = getFiatSubSystemException(uei.getNameToEvaluate());
		if (!subSystemException.equals("")) {
			uei.setSubSystem(subSystemException);
			return;
		}
		
		// Sottosistema su nome copy
		if (uei.getNameToEvaluate().startsWith("C")) {
			uei.setSubSystem(uei.getNameToEvaluate().substring(1, 3));
			return;
		}
		// Sottosistema su nome copy di DCLGEN
		if (uei.getNameToEvaluate().startsWith("TA")) {
			uei.setSubSystem(uei.getNameToEvaluate().substring(2, 4));
			return;
		}
		// Sottosistema su nome pgm planning -> KR UK
		if (uei.getNameToEvaluate().startsWith("RP00") || uei.getNameToEvaluate().startsWith("CRP00")) {
			uei.setSubSystem("UK");
			return;
		}
		
		// Nome programma di sort con codifica speciale sffnxxxx dove s è il settore/ambito
		// n è numerico xxxx può essere anche tutto numerico
        if (uei.getNameToEvaluate().charAt(3) >= '0' 
        	&& uei.getNameToEvaluate().charAt(3) <= '9' 
            && uei.getLibraryCode().equals("FIAT-SRC-SORT")
            && (uei.getNameToEvaluate().startsWith("G")
            ||  uei.getNameToEvaluate().startsWith("M")	
            ||  uei.getNameToEvaluate().startsWith("R")	
            ||  uei.getNameToEvaluate().startsWith("S")	
            ||  uei.getNameToEvaluate().startsWith("Z")
            || (uei.getNameToEvaluate().startsWith("A") && !(uei.getNameToEvaluate().charAt(1) == 'R')))	// Non era fra quelli forniti
            ) {
        	uei.setSubSystem(uei.getNameToEvaluate().substring(1, 3));
        	return;
		}
		
        // può solo essere questa codifica
		
		// Nome programma aaffxxxx
        if (uei.getNameToEvaluate().length() > 4) {
        	uei.setSubSystem(uei.getNameToEvaluate().substring(2, 4));
        	// Eccezione
        	if (uei.getSubSystem().equals("BI") 
        	||  uei.getSubSystem().equals("NT") 
        	||  uei.getSubSystem().equals("KZ")) {
        		uei.setSubSystem("XN");
			}
		}
		
        return;
		
		// viene mantenuto il valore di default
	}	
    /*
     * Assegnazione sottosistema per gestione eccezioni
      */
	private String getFiatSubSystemException(String nameToEvaluate) {
		String subsystemException = "";
		
		// Eccezioni programmi da considerare (XY = ALTRI, XD = XSTD)
		if (nameToEvaluate.startsWith("C43XMBAT")) 	{return "XY";}
		if (nameToEvaluate.startsWith("CCRPDPLL")) 	{return "XY";}
		if (nameToEvaluate.startsWith("CCAV")) 	 	{return "XY";}
		if (nameToEvaluate.startsWith("CCRP00")) 	{return "UK";}
		if (nameToEvaluate.startsWith("CHRB"))   	{return "XY";}
		if (nameToEvaluate.startsWith("CHTP"))   	{return "XY";}
		if (nameToEvaluate.startsWith("CICSFINE"))	{return "XY";}
		if (nameToEvaluate.startsWith("CISBB310"))	{return "XY";}
		if (nameToEvaluate.startsWith("COBKU"))	    {return "UK";}
		if (nameToEvaluate.startsWith("COBLR"))	    {return "LR";}
		if (nameToEvaluate.startsWith("COBNL"))	    {return "NL";}
		if (nameToEvaluate.startsWith("COBOP"))	    {return "OP";}
		if (nameToEvaluate.startsWith("COBOTX03"))  {return "OP";}
		if (nameToEvaluate.startsWith("CRP00"))     {return "UK";}
		if (nameToEvaluate.startsWith("CRP0G"))     {return "UK";}
		if (nameToEvaluate.startsWith("CSPSTM"))    {return "XY";}
		if (nameToEvaluate.startsWith("CSPSTM"))    {return "XY";}
		if (nameToEvaluate.startsWith("HR00"))      {return "XY";}
		if (nameToEvaluate.startsWith("GM2EW2"))    {return "XY";}
		if (nameToEvaluate.startsWith("C43XMBAT"))  {return "XD";}
		if (nameToEvaluate.startsWith("W7984729"))  {return "XY";}
		if (nameToEvaluate.startsWith("ILBOABN"))   {return "XY";}
		if (nameToEvaluate.startsWith("MFDECODT"))  {return "XY";}
		if (nameToEvaluate.startsWith("ZUKE"))      {return "UK";}
		if (nameToEvaluate.startsWith("ARPD"))      {return "ZP";}
		if (nameToEvaluate.startsWith("HRPD"))      {return "ZP";}
		if (nameToEvaluate.startsWith("IRPD"))      {return "ZP";}
		if (nameToEvaluate.startsWith("GOPJ"))      {return "OP";}
		if (nameToEvaluate.startsWith("MQPUTFL"))   {return "XY";}
		if (nameToEvaluate.startsWith("OPS"))       {return "OP";}
		if (nameToEvaluate.startsWith("XSTD"))      {return "XD";}
		if (nameToEvaluate.startsWith("AQWC"))      {return "QW";}
		if (nameToEvaluate.startsWith("AQWW"))      {return "XY";}
		if (nameToEvaluate.startsWith("OPX"))       {return "OP";}
		if (nameToEvaluate.startsWith("HRXX"))      {return "XY";}
		if (nameToEvaluate.startsWith("KRXX"))      {return "XY";}
		if (nameToEvaluate.startsWith("RPXX"))      {return "XY";}
		if (nameToEvaluate.startsWith("AZZI"))      {return "ZI";}
		if (nameToEvaluate.startsWith("ACZZZ"))     {return "XY";}
		if (nameToEvaluate.startsWith("KRERU"))     {return "XY";}
		if (nameToEvaluate.startsWith("CHRPB957"))  {return "XY";}
		if (nameToEvaluate.startsWith("PARZA210"))  {return "XY";}
		if (nameToEvaluate.startsWith("AZWW"))  	{return "XY";}
		if (nameToEvaluate.startsWith("CBB095"))  	{return "XY";}
		if (nameToEvaluate.startsWith("SANTELAI"))  {return "XY";}
		if (nameToEvaluate.startsWith("AZYYY100"))  {return "XY";}
		if (nameToEvaluate.startsWith("AZYYY101"))  {return "XY";}
		if (nameToEvaluate.startsWith("c43xmbat"))  {return "XY";}
		if (nameToEvaluate.startsWith("CPOTA020"))  {return "XY";}
		if (nameToEvaluate.startsWith("GNRLD6002")) {return "XY";}
		
		// Eccezioni programmi da NON considerare (XN = Altri da NON considerare)
		if (nameToEvaluate.startsWith("DLETECMV"))  {return "XN";}
		if (nameToEvaluate.startsWith("CHIAMANS"))  {return "XN";}
		if (nameToEvaluate.startsWith("LOAD"))      {return "XN";}
		if (nameToEvaluate.startsWith("QUADFILE"))  {return "XN";}
		if (nameToEvaluate.startsWith("RBAMI"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ARAN"))    	{return "XN";}
		if (nameToEvaluate.startsWith("GIATA"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SGATB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("IRB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SEC"))    	{return "XN";}
		if (nameToEvaluate.startsWith("OMC"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ARCDC"))   	{return "XN";}
		if (nameToEvaluate.startsWith("GMCHK"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ARCLG"))    	{return "XN";}
		if (nameToEvaluate.startsWith("FRCOUT"))    {return "XN";}
		if (nameToEvaluate.startsWith("HRCO"))      {return "XN";}
		if (nameToEvaluate.startsWith("RECO"))    	{return "XN";}
		if (nameToEvaluate.startsWith("MODABEN"))   {return "XN";}
		if (nameToEvaluate.startsWith("ARDPLL"))    {return "XN";}
		if (nameToEvaluate.startsWith("UPDSUB"))    {return "XN";}
		if (nameToEvaluate.startsWith("MODTELA"))   {return "XN";}
		if (nameToEvaluate.startsWith("HREPI"))    	{return "XN";}
		if (nameToEvaluate.startsWith("HRERU"))    	{return "XN";}
		if (nameToEvaluate.startsWith("WUERTH"))    {return "XN";}
		if (nameToEvaluate.startsWith("ATEST"))    	{return "XN";}
		if (nameToEvaluate.startsWith("DLETVMV"))   {return "XN";}
		if (nameToEvaluate.startsWith("IREX"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ASFOLT"))    {return "XN";}
		if (nameToEvaluate.startsWith("MIGR"))    	{return "XN";}
		if (nameToEvaluate.startsWith("UTILE"))    	{return "XN";}
		if (nameToEvaluate.startsWith("UTILI"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ARITB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("AGIV"))    	{return "XN";}
		if (nameToEvaluate.startsWith("FUK00"))    	{return "XN";}
		if (nameToEvaluate.startsWith("HUK00"))    	{return "XN";}
		if (nameToEvaluate.startsWith("RUKE"))    	{return "XN";}
		if (nameToEvaluate.startsWith("BILAFILE"))  {return "XN";}
		if (nameToEvaluate.startsWith("MLB4"))    	{return "XN";}
		if (nameToEvaluate.startsWith("BOLLETTE"))  {return "XN";}
		if (nameToEvaluate.startsWith("MLM0"))    	{return "XN";}
		if (nameToEvaluate.startsWith("GMLPB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("VALPREFA"))  {return "XN";}
		if (nameToEvaluate.startsWith("DUMMY"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ENMPB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ANNAR"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SANADATE"))  {return "XN";}
		if (nameToEvaluate.startsWith("GMNEW"))    	{return "XN";}
		if (nameToEvaluate.startsWith("HRNFI"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SANTELAJ"))  {return "XN";}
		if (nameToEvaluate.startsWith("STOCK"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ASOLL"))    	{return "XN";}
		if (nameToEvaluate.startsWith("JBONUS"))    {return "XN";}
		if (nameToEvaluate.startsWith("PROVB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("JZP00"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SIPEB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("APROVA"))    {return "XN";}
		if (nameToEvaluate.startsWith("KRRT"))    	{return "XN";}
		if (nameToEvaluate.startsWith("SORT"))      {return "XN";}
		if (nameToEvaluate.startsWith("ARSAP"))    	{return "XN";}
		if (nameToEvaluate.startsWith("GRSKY"))    	{return "XN";}
		if (nameToEvaluate.startsWith("FASTB"))    	{return "XN";}
		if (nameToEvaluate.startsWith("GMSTELA"))   {return "XN";}
		if (nameToEvaluate.startsWith("SANTELAI"))  {return "XN";}
		if (nameToEvaluate.startsWith("RESTART"))   {return "XN";}
		if (nameToEvaluate.startsWith("TEST"))    	{return "XN";}
		if (nameToEvaluate.startsWith("ESTE"))    	{return "XN";}
		if (nameToEvaluate.startsWith("BATFORM"))   {return "XN";}
		if (nameToEvaluate.startsWith("GMX0"))    	{return "XN";}
		if (nameToEvaluate.startsWith("LEXP"))    	{return "XN";}

		// Non si tratta di una eccezione riconosciuta

		return "";
	}

	/*
	 * Gestione personalizzata per Fiat filtro su nomi
	 */
	private void executeExitFiatFilter(UserExitInfo uei) {
		
	}

	
	/*
	 * Gestione personalizzata per Sanpaolo
	 */
	private void executeExitSanpaolo(UserExitInfo uei) {
		String sqlOwner = "";
		
		sqlOwner = uei.getSqlOwner();
		if (sqlOwner.length() == 4) {
			uei.setSystem(sqlOwner.substring(1, 2));
		}
	
		
	    // Valutazione operazione da effettuare		
		switch (uei.getUserExitOperation()) {
		
		        // A fronte di individuazione oggetti da trattare
				case GET_SYSTEM_OWNER:
						break;
					
					// A fronte di individuazione oggetti da trattare
				case GET_SUB_SYSTEM_OWNER:
					break;
					
				// A fronte di individuazione oggetti da trattare
				case GET_SYSTEM_SUB_SYSTEM_OWNER:
					break;
					
				// A fronte di individuazione oggetti da trattare
				case FILTER_ON_OBJECT_NAME_EVALUATE:
					break;
					
				case NOT_ASSIGNED:
					break;
		}

	}

	/* -------------------------------------------------------------------
	 * Recupero sistemaa fronte di analisi di script sql
	 * -------------------------------------------------------------------
	 * 
	 * La exit viene attivata dall'analizzatore di script sql AnalyzerSql,
	 * a fronte per esempio di Create Table, per determinare il sistema 
	 * di appartenenza della tabella o di altro oggetto, non determinabile a priori.
	 * Infatti in uno script sql possono essere definite + tabelle appartenenti
	 * a sistemi/sottosistemi diversi e il sistema/soottosistema di appartenenza può
	 * essere dedotto dal nome della tabella, dall'owner, dal tablespace, dal dabname
	 * etc.
	 * Questo metodo può effettuare qualsiasi operazione java permessa, accedere al file
	 * system per recuperare le informazioni necessarie etc.
	 * 
	 */
	private void getSystemSqlScript(UserExitInfo uei) {
		String sqlOwner = "";
		
		uei.setSystem("*");

		// Per Intesa SanPaolo
		if (uei.getCustomerCode().equals("SANPAOLO")) {
			uei.setSystem("Z");
			sqlOwner = uei.getSqlOwner();
			if (sqlOwner.length() == 4) {
				uei.setSystem(sqlOwner.substring(1, 2));
			}
		}
		
	}

	/* -------------------------------------------------------------------
	 * Recupero sottosistemaa fronte di analisi di script sql
	 * -------------------------------------------------------------------
	 * 
	 * La exit viene attivata dall'analizzatore di script sql AnalyzerSql,
	 * a fronte per esempio di Create Table, per determinare il sottosistema 
	 * di appartenenza della tabella o di altro oggetto, non determinabile a priori.
	 * Infatti in uno script sql possono essere definite + tabelle appartenenti
	 * a sistemi/sottosistemi diversi e il sistema/soottosistema di appartenenza può
	 * essere dedotto dal nome della tabella, dall'owner, dal tablespace, dal dabname
	 * etc.
	 * Questo metodo può effettuare qualsiasi operazione java permessa, accedere al file
	 * system per recuperare le informazioni necessarie etc.
	 * 
	 */
	private void getSubSystemSqlScript(UserExitInfo uei) {
		String sqlOwner = "";
		
		uei.setSubSystem("*");

		// Per Intesa SanPaolo
		if (uei.getCustomerCode().equals("SANPAOLO")) {
			uei.setSystem("Z");
			sqlOwner = uei.getSqlOwner();
			if (sqlOwner.length() == 4) {
				uei.setSubSystem(sqlOwner.substring(2));
			}
		}
	}

	/* -------------------------------------------------------------------
	 * Recupero sistema e sottosistema a fronte di analisi di script sql
	 * -------------------------------------------------------------------
	 * 
	 * La exit viene attivata dall'analizzatore di script sql AnalyzerSql,
	 * a fronte per esempio di Create Table, per determinare il sistema e il
	 * sottosistema di appartenenza della tabella o di altro oggetto, non
	 * determinabile a priori.
	 * Infatti in uno script sql possono essere definite + tabelle appartenenti
	 * a sistemi/sottosistemi diversi e il sistema/soottosistema di appartenenza può
	 * essere dedotto dal nome della tabella, dall'owner, dal tablespace, dal dabname
	 * etc.
	 * Questo metodo può effettuare qualsiasi operazione java permessa, accedere al file
	 * system per recuperare le informazioni necessarie etc.
	 * 
	 */
	private void getSystemSubSystemSqlScript(UserExitInfo uei) {
		String sqlOwner = "";
		
		// Per Intesa SanPaolo
		if (uei.getCustomerCode().equals("SANPAOLO")) {
			uei.setSystem("Z");
			uei.setSubSystem("Z");
			sqlOwner = uei.getSqlOwner();
			if (sqlOwner.length() == 4) {
				uei.setSystem(sqlOwner.substring(1, 2));
				uei.setSubSystem(sqlOwner.substring(2));
			}
		}
	}

}
