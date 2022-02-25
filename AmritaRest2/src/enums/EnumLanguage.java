package enums;

import analyzer.DataBaseMappedEnumeration;
import analyzer.LoggerFacade;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda Turin (ITALY)
  * 
  *  Questa enumerazione elenca i possibili linguaggi con la convenzione della classe Locale.
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 12/01/2010
  * @see LoggerFacade
  * 
 */

@DataBaseMappedEnumeration
public enum EnumLanguage {
	
	NOT_ASSIGNED,        		// 00 Di servizio 
	
    ITALIAN("it"), 				// 01	 
    ENGLISH("en"), 			 	// 02
    DEUTCH("de"), 			    // 03
    FRENCH("fr"), 			    // 04
    SPANISH("es");    			// 04
     
    
	private String idLang = null;

	EnumLanguage() {
		idLang="";
		}
	
	EnumLanguage(String idLang) {
	  setLocalValue(idLang);
	}
	
	public void setLocalValue(String idLang){
		this.idLang = idLang;
		return;
	}
	
	public String getLocalValue(){
		return this.idLang;
	}
	
	public String toString() {
		return this.idLang;
	}   
    
}