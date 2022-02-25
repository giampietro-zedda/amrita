package analyzer;

import java.io.Serializable;
import java.util.Map;

import enums.EnumCobolReservedWords;

/**
 * 
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda    Turin (ITALY)
 * 
 * <h1>
 * InstructionCobolEnvironment
 * </h1>
 *  <p>
 * Questa classe modella uno specifico statement sorgente del linguaggio Cobol di environment division. <br>
 * Vengono gestite eventuali informazioni aggiuntive per sorgenti Cobol.
 *  
 *  
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/02/2010
 * @see Instruction
 * 
 * 
*/
public class InstructionCobolEnvironment extends InstructionCobol implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza  specifiche                                          						  //                                                        //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	/**  
	 * Costruttore senza parametri
	 *  
	 */
	public InstructionCobolEnvironment()  {
		super();
	}
	
	/**  
	 * Costruttore utilizzato per istanziare un oggetto Instruction
	 * con tutti i parametri richiesti
	 *  
	 *  @param numInstr 				Numero sequenza istruzione 0-based
	 *  @param RowStartSource 			Numero riga sorgente di analisi di inizio
	 *  @param RowEndSource 			Numero riga sorgente di analisi di fine
	 *  @param PosStart 		    	Posizione inizio istruzione in riga sorgente
	 *  @param PosEnd    		    	Posizione fine istruzione in riga sorgente	
	 *  @param ar_RowsSource          	Array righe sorgente con l'istruzione
	 *  @param ar_RowsSourceComments  	Array righe sorgente con i commenti precedenti l'istruzione
	 *  
	 */
	public InstructionCobolEnvironment(int numInstr
							   ,int rowStartSource
							   ,int rowEndSource
							   ,int posStartInstr
							   ,int posEndInstr
						 	   ,String ar_RowsSource[]
					           ,String ar_CommentsBeforeInstr[]
					           ,String ar_CommentsLeftInstr[]                        
					           ,String ar_CommentsRightInstr[]                        
					           ,String name
					           ,String sourceInstr 
				               ) {
		
		super(numInstr
		 	 ,rowStartSource
			 ,rowEndSource
			 ,posStartInstr
			 ,posEndInstr
			 ,ar_RowsSource
			 ,ar_CommentsBeforeInstr
			 ,ar_CommentsLeftInstr
			 ,ar_CommentsRightInstr
			 ,name
			 ,sourceInstr
			 );	
		
	}

	/**
	 * Imposta il nome del file interno di uno statement Select. <br>
	 * <p>
	 * per esempio Select file1 assign to extfile<br>
	 * <p>
	 * @param String fileNameInternal
	 */
	public void  selectSetFileNameInternal(String fileNameInternal) {
		this.addMapDescriptorObject("$FILE$", fileNameInternal);
		return;
	}
	
	/**
	 * Restituisce il nome del file interno di uno statement Select. <br>
	 * <p>
	 * per esempio Select file1 assign to extfile<br>
	 * <p>
	 * @return String fileNameInternal
	 */
	public String  selectGetFileNameInternal() {
		return (String) getMapDescriptorObject("$FILE$");
	}

	/**
	 * Imposta il nome del primo campo di file status di uno statement Select. <br>
	 * <p>
	 * @param String fileStatus1
	 */
	public void  selectSetFileStatus1(String fileStatus1) {
		this.addMapDescriptorObject("$FILE-STATUS1$", fileStatus1);
		return;
	}
	
	/**
	 * Restituisce il nome del primo campo di file status di uno statement Select. <br>
	 * <p>
	 * @return String fileStatus1
	 */
	public String  selectGetFileStatus1() {
		return (String) getMapDescriptorObject("$FILE-STATUS1$");
	}

	/**
	 * Imposta il nome del secondo campo di file status di uno statement Select. <br>
	 * <p>
	 * @param String fileStatus2
	 */
	public void  selectSetFileStatus2(String fileStatus2) {
		this.addMapDescriptorObject("$FILE-STATUS2$", fileStatus2);
		return;
	}
	
	/**
	 * Restituisce il nome del secondo campo di file status di uno statement Select. <br>
	 * <p>
	 * @return String fileStatus2
	 */
	public String  selectGetFileStatus2() {
		return (String) getMapDescriptorObject("$FILE-STATUS2$");
	}

	/**
	 * Imposta i file esterni di uno statement Select. <br>
	 * <p>
	 * Si tratta dei nomi delle dd esterne<br>
	 * <p>
	 * @param String[] ar_fileNameExternal
	 */
	public void selectSetFileNamesExternal(String[] ar_fileNameExternal) {
		this.addMapDescriptorObject("$DDNAMES$", ar_fileNameExternal);
		
	}
	
	/**
	 * Restituisce i file esterni di uno statement Select. <br>
	 * <p>
	 * Si tratta dei nomi delle dd esterne<br>
	 * <p>
	 * @return String[] ar_fileNameExternal
	 */
	public String[]  selectGetFileNamesExternal(String[] ar_fileNameExternal) {
		return (String[]) getMapDescriptorObject("$DDNAMES$");
	}

	/**
	 * Imposta la condizione di organizzazione indexed per la select. <br>
	 * <p>
	 *  @param boolean isOrganizationIndexed
	 */
	public void selectSetOrganizationIndexed(boolean isOrganizationIndexed) {
		this.addMapDescriptorObject("$OPT$" + "ORG-INDEXED", isOrganizationIndexed);
	}
	
	/**
	 * Restituisce la condizione di organizzazione indexed per la select. <br>
	 * <p>
	 * @return boolean isOrganizationIndexed
	 */
	public boolean selectIsOrganizationIndexed() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ORG-INDEXED");
	}
	
	/**
	 * Imposta la condizione di organizzazione sequential per la select. <br>
	 * <p>
	 *  @param boolean isOrganizationIndexed
	 */
	public void selectSetOrganizationSequential(boolean isOrganizationSequential) {
		this.addMapDescriptorObject("$OPT$" + "ORG-SEQUENTIAL", isOrganizationSequential);
	}
	
	/**
	 * Restituisce la condizione di organizzazione sequential per la select. <br>
	 * <p>
	 * @return boolean isOrganizationSequential
	 */
	public boolean selectIsOrganizationSequential() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ORG-SEQUENTIAL");
	}
	
	/**
	 * Imposta la condizione di organizzazione sequential per la select. <br>
	 * <p>
	 *  @param boolean isOrganizationLineSequential
	 */
	public void selectSetOrganizationLineSequential(boolean isOrganizationLineSequential) {
		this.addMapDescriptorObject("$OPT$" + "ORG-LINE-SEQUENTIAL", isOrganizationLineSequential);
	}
	
	/**
	 * Restituisce la condizione di organizzazione sequential per la select. <br>
	 * <p>
	 * @return boolean isOrganizationLineSequential
	 */
	public boolean selectIsOrganizationLineSequential() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ORG-LINE-SEQUENTIAL");
	}

	/**
	 * Imposta la condizione di organizzazione relative per la select. <br>
	 * <p>
	 *  @param boolean isOrganizationLineSequential
	 */
	public void selectSetOrganizationRelative(boolean isOrganizationRelative) {
		this.addMapDescriptorObject("$OPT$" + "ORG-RELATIVE", isOrganizationRelative);
	}
	
	/**
	 * Restituisce la condizione di organizzazione relative per la select. <br>
	 * <p>
	 * @return boolean isOrganizationRelative
	 */
	public boolean selectIsOrganizationRelative() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ORG-RELATIVE");
	}

	/**
	 * Imposta la condizione di access sequential per la select. <br>
	 * <p>
	 *  @param boolean isAccessSequential
	 */
	public void selectSetAccessSequential(boolean isAccessSequential) {
		this.addMapDescriptorObject("$OPT$" + "ACC-SEQUENTIAL", isAccessSequential);
	}
	
	/**
	 * Restituisce la condizione di access sequential per la select. <br>
	 * <p>
	 * @return boolean isAccessSequential
	 */
	public boolean selectIsAccessSequential() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ACC-SEQUENTIAL");
	}

	/**
	 * Imposta la condizione di access dynamic per la select. <br>
	 * <p>
	 *  @param boolean isAccessDynamic
	 */
	public void selectSetAccessDynamic(boolean isAccessDynamic) {
		this.addMapDescriptorObject("$OPT$" + "ACC-DYNAMIC", isAccessDynamic);
	}
	
	/**
	 * Restituisce la condizione di access dynamic per la select. <br>
	 * <p>
	 * @return boolean isAccessDynamic
	 */
	public boolean selectIsAccessDynamic() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ACC-DYNAMIC");
	}
	/**
	 * Imposta la condizione di access dynamic per la select. <br>
	 * <p>
	 *  @param boolean isAccessRandom
	 */
	public void selectSetAccessRandom(boolean isAccessDynamic) {
		this.addMapDescriptorObject("$OPT$" + "ACC-RANDOM", isAccessDynamic);
	}
	
	/**
	 * Restituisce la condizione di access dynamic per la select. <br>
	 * <p>
	 * @return boolean isAccessRandom
	 */
	public boolean selectIsAccessRandom() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "ACC-RANDOM");
	}

	/**
	 * Imposta se il punto decimale è una virgola o un punto. <br>
	 * <p>
	 * @param boolean isDecimalPointComma
	 */
	public void specialNamesSetDecimalPointComma(boolean isDecimalPointComma) {
		this.addMapDescriptorObject("$OPT$" + "DEC-POINT", isDecimalPointComma);
	}

	/**
	 * Restitisce se il punto decimale è una virgola. <br>
	 * <p>
	 * @return boolean isDecimalPointComma
	 */
	public boolean specialNamesIsDecimalPointComma() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "DEC-POINT");
	}

	/**
	 * Imposta se l'alfabeto corrente è EBCDIC. <br>
	 * <p>
	 * @param boolean isAlphabetEbcdic
	 */
	public void specialNamesSetAlphabetEbcdic(boolean isAlphabetEbcdic) {
		this.addMapDescriptorObject("$OPT$" + "EBCDIC", isAlphabetEbcdic);
	}

	/**
	 * Restitisce se l'alfabeto corrente è EBCDIC. <br>
	 * <p>
	 * @return boolean isAlphabetEbcdic
	 */
	public boolean specialNamesIsAlphabetEbcdic() {
		return (Boolean) this.getMapDescriptorObject("$OPT$" + "EBCDIC");
	}
    /**
     * Imposta una map con i nomi di ambiente, mappati su un nome mnemonico interno al programma.<br>
     * <p>
     * Per esempi C01 IS NEW-PAGE indica di usare il salto fisico a canale 1 con il nome NEW-PAGE.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=environment name, data=mnemonic name.<br>
     * L'environment name è espresso dalla enumerazione {@link EnumCobolReservedWords}<br>
     * <p>
     * @param Map<EnumCobolReservedWords, String> mapEnvironmentName
     */
	public void specialNamesSetEnvironment(Map<EnumCobolReservedWords, String> map_EnvironmentName) {
		this.addMapDescriptorObject("$ENVIRONMENT$", map_EnvironmentName);
	}

    /**
     * Restituisce una map con i nomi di ambiente, mappati su un nome mnemonico interno al programma.<br>
     * <p>
     * Per esempi C01 IS NEW-PAGE indica di usare il salto fisico a canale 1 con il nome NEW-PAGE.<br>
     * <p>
     * Restituisce una map con tutte le coppie Key=environment name, data=mnemonic name.<br>
     * L'environment name è espresso dalla enumerazione {@link EnumCobolReservedWords}<br>
     * <p>
     * @return Map<EnumCobolReservedWords, String> mapEnvironmentName
     */
	@SuppressWarnings("unchecked")
	public Map<EnumCobolReservedWords, String> specialNamesGetEnvironment() {
		return (Map<EnumCobolReservedWords, String>) this.getMapDescriptorObject("$ENVIRONMENT$");
	}
	
	/**
     * Imposta una map con i nomi interni degli alfabeti, mappati sul tipo di alfabeto.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=alphabet name, data=alphabet type.<br>
     * 
     * Key=alphabet name, data=STANDARD-1, STANDARD-2, NATIVE, EBCDIC<br>
     * @param mapAlphabetName
     */
	public void specialNamesSetAlphabet(Map<String, String> map_AlphabetName) {
		this.addMapDescriptorObject("$ALPHABET$", map_AlphabetName);
	}
	
    /**
     * Restituisce una map con i nomi interni degli alfabeti, mappati sul tipo di alfabeto.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=alphabet name, data=alphabet type.<br>
     * 
     * Key=alphabet name, data=STANDARD-1, STANDARD-2, NATIVE, EBCDIC<br>
     * @param mapAlphabetName
     */
	@SuppressWarnings("unchecked")
	public Map<String, String> specialNamesGetAlphabet() {
		return (Map<String, String>) this.getMapDescriptorObject("$ALPHABET$");
	}

	/**
     * Imposta una map con i nomi interni degli alfabeti, mappati sul tipo di alfabeto.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=alphabet name, data=alphabet type.<br>
     * 
     * Key=alphabet name, data=STANDARD-1, STANDARD-2, NATIVE, EBCDIC<br>
     * @param map_SymbolicChar
     */
	public void specialNamesSetSymbolicChar(Map<String, String> map_SymbolicChar) {
		this.addMapDescriptorObject("$SYMB-CHAR$", map_SymbolicChar);
	}
	
    /**
     * Restituisce una map con i nomi simbolici dei caratteri, mappati sul tipo di alfabeto.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=symbolic char, data=alphabet name.<br>
     * 
     * Key=symbolic char, data=alphabet name<br>
     * @param mapSymbolicChar
     */
	@SuppressWarnings("unchecked")
	public Map<String, String> specialNamesGetSymbolicChar() {
		return (Map<String, String>) this.getMapDescriptorObject("$SYMB-CHAR$");
	}

	/**
     * Imposta una map con i nomi delle classi, mappati sul un range di caratteri
     * rappresentati da due stringhe separate da uno spazio.<br>
     * <p>
     * Imposta una map con tutte le coppie Key=class name, data=literalFrom space literalTo.<br>
     * 
     * key=class name, data=literalFrom space literalTo<br>
     * 
     * @param Map<String, String>map_Class
     */
	public void specialNamesSetClass(Map<String, String> map_Class) {
		this.addMapDescriptorObject("$CLASS$", map_Class);
	}
	
    /**
     * Restituisce una map con i nomi delle classi, mappati sul un range di caratteri
     * rappresentati da due stringhe separate da uno spazio.<br>
     * <p>
     * 
     * key=class name, data=literalFrom space literalTo<br>
     * 
     * @return Map<String, String> map_Class
     */
	@SuppressWarnings("unchecked")
	public Map<String, String> specialNamesGetClass() {
		return (Map<String, String>) this.getMapDescriptorObject("$CLASS$");
	}

	/**
     * Imposta una map con i nomi delle currency sign, come
     * Currency sign come ’$’, ’EUR’, ’CHF’, ’JPY’, ’HK$’, ’HKD’,etc, 
     * mappati sul carattere che deve essere messo nella picture,
     * per ottenere tale valore.<br>
     * <p>
     * 
     * Key=currency sign, data=picture symbol<br>
     * 
     * @param Map<String, String>map_Class
     */
	public void specialNamesSetCurrency(Map<String, String> map_Class) {
		this.addMapDescriptorObject("$CURRENCY$", map_Class);
	}
	
    /**
     * Restituisce una map con i nomi delle currency sign, come
     * Currency sign come ’$’, ’EUR’, ’CHF’, ’JPY’, ’HK$’, ’HKD’,etc, 
     * mappati sul carattere che deve essere messo nella picture,
     * per ottenere tale valore.<br>
     * <p>
     * 
     * Key=currency sign, data=picture symbol<br>
     * 
     * @return Map<String, String> map_Class
     */
	@SuppressWarnings("unchecked")
	public Map<String, String> specialNamesGetCurrency() {
		return (Map<String, String>) this.getMapDescriptorObject("$CURRENCY$");
	}
	
	
}
