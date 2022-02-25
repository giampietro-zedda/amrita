package forward;

import java.io.Serializable;
import java.util.ArrayList;

import enums.EnumForwardMetaFieldType;

/**
 * copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ForwardMetaField
 * </h1>
 * <p>
 * Questa classe descrive le caratteristiche generali di una unità elementare di informazione<br>
 * <p>
 * Si tratta di informazioni memorizzate in campi di tabelle legacy del sistema.<br>
 * Vengono memorizzati tutti i vincoli di descrizione, controllo, visualizzazione che vengono<br>
 * replicati fino al singolo pannello applicativo.<br>
 * <p>
 * Si tratta di informazioni recuperate dal dizionario centralizzato del sistema.<br>
 * <p>
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/nov/2011 
 * @see ForwardFunction
 *
*/

public class ForwardMetaField implements Serializable {

	private static final long serialVersionUID = 1L;
	
	// Identificazione
    private String metaFieldName = "";						// Nome metacampo, può coincidere con il nome della colonna db o del campo di entity
	private EnumForwardMetaFieldType type = null;  			// Tipologia e formato metacampo
    
	// Dimensioni metacampo se CHAR o NUMERIC
	private int sizeChar = 0; 								// Numero caratteri complessivi
	private int sizeDigitInt = 0; 							// Numero digit interi
	private int sizeDigitDec = 0; 							// Numero digit decimali
	
	// File esterni su file system con supporto multilingua.
	// Il supporto multilingue viene realizzato anteponendo al nome del file xx_, dove xx vale IT, DE etc., secondo localizzazione.
	private String fileExternal = "";                 		// Nome file esterno come picture.gif o file1.txt se IMAGE, SOURCE o DOCUMENT
	
	// Help con supporto multilingua.
	// Tabella multilingua con key = codtab + metaFieldName
	// Presente 1 campo con la stringa HTML, in lingua: 
	private String tableCodeHelpTechnical = "";    			// Codice tabella sistema tabelle forward Html con help di tipo tecnico specialistico     
	private String tableCodelHelpEndUser = "";              // Codice tabella sistema tabelle forward Html con help di tipo funzionale per l'utente finale 
	
	// Descrizioni con supporto multilingue.
	// Tabella multilingua con key = codtab + metaFieldName
	// Presenti 3 campi per gestire, in lingua: 
	//  . captionShortCode Descrizione breve 
	//  . captionLongCode lunga  
	//  . toolTipCode tooltip  
	private String multiLangTableCodeCaption = "";          // Codice tabella multilingua metacampo
    
    // Valori di range o ammessi
    private String maxValue = "";                           // Valore massimo testuale in base a tipo (es."A" o 0)
    private String minValue = "";                           // Valore minimo  testuale in base a tipo (es."Z" 0 9999)
	private int minTextSize = 0;                            // Numero minimo  caratteri se meta campo di testo
	private int minDigitIntSize = 0;                        // Numero minimo  di interi se meta campo numerico
	private int minDigitDecSize = 0;                        // Numero minimo  di decimali se meta campo numerico
	private ArrayList<String> al_valuesAllowed = null;      // Unici valori specifici ammessi. Valore elementi in base al tipo di meta dato.
	
	// Controlli di validità
	private boolean isCtrlMaxValueActive = false;           // Il metadato non deve superare il valore
	private boolean isCtrlMinValueActive = false;           // Il metadato non deve essere inferiore al valore
	private boolean isCtrlMaxTextSizeActive = false;        // Il numero di caratteri per il metadato non deve superare il valore
	private boolean isCtrlMinTextSizeActive = false;        // Il numero di caratteri per il metadato non deve essere inferiore al valore
	private boolean isCtrlMaxDigitIntSizeActive = false;    // Il numero di cifre intere del metadato non deve superare il valore
	private boolean isCtrlMinDigitIntSizeActive = false;    // Il numero di cifre intere del metadato non deve essere inferiore al valore
	private boolean isCtrlMaxDigitDecSizeActive = false;    // Il numero di cifre decimali del metadato non deve superare il valore
	private boolean isCtrlMinDigitDecSizeActive = false;    // Il numero di cifre decimali del metadato non deve essere inferiore al valore
	
	// Controllo validità in sistema tabelle forward
	private String ctrlTableCode = "";                      // Codice testuale tabella di controllo (es. T001)
	private int ctrlTableNum = 0;                           // Numero tabella di controllo (es. T001)
	private ArrayList<String> al_ctrlTableKey = null;       // Sequenza di campi chiave necessari per il controllo da risolvere runtime
	
	// Informazioni database, dove il metadato è memorizzato fisicamente
	private String dbEntityOwner = "";                      // Tabella db dove il metadato è memorizzato.
	private String dbEntityColumn = "";                     // Colonna un tabella db dove il metadato è memorizzato.
	private String dbEntityJavaName = "";                   // Nome java con cui il metadato è codificato nella classe di gestione dell'entity
	
	// Informazioni accesso a database, con quale logical data view recuperare il valore del metadato
	private String sourceLdv = "";                          // Nome logical data view da utilizzare per leggere il valore del metadato
	private String sourceLdvColumn = "";                    // Nome campo (colonna db) restituito contenenete il valore del campo
	
	/*
	 * Costruttore
	 */
	private ForwardMetaField(String metaFieldName, EnumForwardMetaFieldType metaFieldType) {
		super();
		this.metaFieldName = metaFieldName;
		this.type = metaFieldType;
	}

	/**
	 * Restituisce il nome del metacampo.<br>
	 * <p>
	 * @return the metaFieldName
	 */
	public String getMetaFieldName() {
		return metaFieldName;
	}

	/**
	 * Imposta il nome del metacampo.<br>
	 * <p>
	 * @param metaFieldName the metaFieldName to set
	 */
	public void setMetaFieldName(String metaFieldName) {
		this.metaFieldName = metaFieldName;
	}

	/**
	 * Restituisce il tipo del metacampo.<br>
	 * <p>
	 * @return the metaFieldType
	 */
	public EnumForwardMetaFieldType getMetaFieldType() {
		return type;
	}

	/**
	 * Restituisce le dimensioni del metacampo in caratteri.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @return the sizeChar
	 */
	public int getSizeChar() {
		return sizeChar;
	}

	/**
	 * Imposta le dimensioni del metacampo in caratteri.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @param sizeChar the sizeChar to set
	 */
	public void setSizeChar(int sizeChar) {
		this.sizeChar = sizeChar;
	}

	/**
	 * Restituisce il numero di digit interi se metacampo numerico.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @return the sizeDigitInt
	 */
	public int getSizeDigitInt() {
		return sizeDigitInt;
	}

	/**
	 * Imposta il numero di digit interi se metacampo numerico.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @param sizeDigitInt the sizeDigitInt to set
	 */
	public void setSizeDigitInt(int sizeDigitInt) {
		this.sizeDigitInt = sizeDigitInt;
	}

	
	/**
	 * Restituisce il nome del file esterno contenente l'immagine il documento o il file sorgente.<br>
	 * <p>
	 * A fronte di metadato associato a IMAGE DOC o SOURCE.<br>
	 * <p>
	 * @return the fileExternal
	 */
	public String getFileExternal() {
		return fileExternal;
	}

	/**
	 * Restituisce il nome del file esterno contenente l'immagine il documento o il file sorgente.<br>
	 * <p>
	 * A fronte di metadato associato a IMAGE DOC o SOURCE.<br>
	 * <p>
	 * @param fileExternal the fileExternal to set
	 */
	public void setFileExternal(String fileExternal) {
		this.fileExternal = fileExternal;
	}

	
	/**
	 * Restituisce il codice della tabella con Html di help a livello tecnico 
	 * specialistico per il metadato.<br>
	 * <p>
	 * @return the tableCodeHelpTechnical
	 */
	public String getTableCodeHelpTechnical() {
		return tableCodeHelpTechnical;
	}

	/**
	 * Imposta il codice della tabella con Html di help a livello tecnico 
	 * specialistico per il metadato.<br>
	 * <p>
	 * @param tableCodeHelpTechnical the tableCodeHelpTechnical to set
	 */
	public void setTableCodeHelpTechnical(String tableCodeHelpTechnical) {
		this.tableCodeHelpTechnical = tableCodeHelpTechnical;
	}

	/**
	 * Restituisce il codice della tabella con Html di help a livello funzionale/applicativo  
	 * valido per l'utente finale.<br>
	 * <p>
	 * @return the tableCodelHelpEndUser
	 */
	public String getTableCodelHelpEndUser() {
		return this.tableCodelHelpEndUser;
	}

	/**
	 * Imposta il codice della tabella con Html di help a livello funzionale/applicativo  
	 * valido per l'utente finale.<br>
	 * <p>
	 * @param tableCodelHelpEndUser the tableCodelHelpEndUser to set
	 */
	public void setTableCodelHelpEndUser(String tableCodelHelpEndUser) {
		this.tableCodelHelpEndUser = tableCodelHelpEndUser;
	}

	/**
	 * Restituisce il numero di digit decimali se metacampo numerico.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @return the sizeDigitDec
	 */
	public int getSizeDigitDec() {
		return sizeDigitDec;
	}

	/**
	 * Imposta il numero di digit decimali se metacampo numerico.<br>
	 * <p>
	 * Le applicazioni che utilizzano il metadato devono rispettare queste dimensioni.<br>
	 * <p>
	 * @param sizeDigitDec the sizeDigitDec to set
	 */
	public void setSizeDigitDec(int sizeDigitDec) {
		this.sizeDigitDec = sizeDigitDec;
	}

	/**
	 * Imposta il tipo del metacampo.<br>
	 * <p>
	 * @param type the type to set
	 */
	public void setMetaFieldType(EnumForwardMetaFieldType type) {
		this.type = type;
	}
	
	
	/**
	 * Restituisce il codice della tabella multilingua associata al metacampo per le caption.<br>
	 * <p>
	 * <p>
	 * Tale tabella ha 3 colonne per gestire, in lingua:<br>
	 * <ul>
	 * <li>Descrizione breve </li><br>
	 * <li>Descrizione lunga </li><br>
	 * <li>Descrizione tooltip </li><br>
	 * </ul>
	 * <br>
	 * 
	 * @return the multiLangTableCode
	 */
	public String getMultiLangTableCodeCaption() {
		return multiLangTableCodeCaption;
	}

	/**
	 * Imposta il codice della tabella multilingua associata al metacampo per le caption.<br>
	 * <p>
	 * <p>
	 * Tale tabella ha 3 colonne per gestire, in lingua:<br>
	 * <ul>
	 * <li>Descrizione breve </li><br>
	 * <li>Descrizione lunga </li><br>
	 * <li>Descrizione tooltip </li><br>
	 * </ul>
	 * <br>
	 * @param multiLangTableCodeCaption the multiLangTableCodeCaption to set
	 */
	public void setMultiLangTableCodeCaption(String multiLangTableCodeCaption) {
		this.multiLangTableCodeCaption = multiLangTableCodeCaption;
	}

		/**
	 * Restituisce il valore massimo assumibile dal metacampo.<br>
	 * <p>
	 * Viene restituita una string il cui contenuto dipende dal tipo di metacampo.<br>
	 * <p>
	 * @return the maxValue
	 */
	public String getMaxValue() {
		return maxValue;
	}

	/**
	 * Imposta il valore massimo assumibile dal metacampo.<br>
	 * <p>
	 * Viene restituita una string il cui contenuto dipende dal tipo di metacampo.<br>
	 * <p>
	 * @param maxValue the maxValue to set
	 */
	public void setMaxValue(String maxValue) {
		this.maxValue = maxValue;
	}

	/**
	 * Restituisce il valore minimo assumibile dal metacampo.<br>
	 * <p>
	 * Viene restituita una string il cui contenuto dipende dal tipo di metacampo.<br>
	 * <p>
	 * @return the minValue
	 */
	public String getMinValue() {
		return minValue;
	}

	/**
	 * Imposta il valore minimo assumibile dal metacampo.<br>
	 * <p>
	 * Viene restituita una string il cui contenuto dipende dal tipo di metacampo.<br>
	 * <p>
	 * @param minValue the minValue to set
	 */
	public void setMinValue(String minValue) {
		this.minValue = minValue;
	}


	/**
	 * Restituisce il numero minimo di caratteri assumibile dal metacampo.<br>
	 * <p>
	 * @return the minTextSize
	 */
	public int getMinTextSize() {
		return minTextSize;
	}

	/**
	 * Imposta il numero minimo di caratteri assumibile dal metacampo.<br>
	 * <p>
	 * @param minTextSize the minTextSize to set
	 */
	public void setMinTextSize(int minTextSize) {
		this.minTextSize = minTextSize;
	}


	/**
	 * Restituisce il numero minimo di digit interi assumibile dal metacampo.<br>
	 * <p>
	 * @return the minDigitIntSize
	 */
	public int getMinDigitIntSize() {
		return minDigitIntSize;
	}

	/**
	 * Imposta il numero minimo di digit interi assumibile dal metacampo.<br>
	 * <p>
	 * @param minDigitIntSize the minDigitIntSize to set
	 */
	public void setMinDigitIntSize(int minDigitIntSize) {
		this.minDigitIntSize = minDigitIntSize;
	}


	/**
	 * Restituisce il numero minimo di digit decimali assumibile dal metacampo.<br>
	 * <p>
	 * @return the minDigitDecSize
	 */
	public int getMinDigitDecSize() {
		return minDigitDecSize;
	}

	/**
	 * Imposta il numero minimo di digit decimali assumibile dal metacampo.<br>
	 * <p>
	 * @param minDigitDecSize the minDigitDecSize to set
	 */
	public void setMinDigitDecSize(int minDigitDecSize) {
		this.minDigitDecSize = minDigitDecSize;
	}

	/**
	 * Restituisce i possibili valori assumibili dal metacampo.<br>
	 * <p>
	 * @return the al_valuesAllowed
	 */
	public ArrayList<String> getValuesAllowed() {
		return al_valuesAllowed;
	}

	/**
	 * Imposta i possibili valori assumibili dal metacampo.<br>
	 * <p>
	 * @param alValuesAllowed the al_valuesAllowed to set
	 */
	public void setValuesAllowed(ArrayList<String> alValuesAllowed) {
		al_valuesAllowed = alValuesAllowed;
	}

	/**
	 * Restituisce se il controllo sul valore massimo è attivo.<br>
	 * <p>
	 * @return the isCtrlMaxValueActive
	 */
	public boolean isCtrlMaxValueActive() {
		return isCtrlMaxValueActive;
	}

	/**
	 * Imposta se il controllo sul valore massimo è attivo.<br>
	 * <p>
	 * @param isCtrlMaxValueActive the isCtrlMaxValueActive to set
	 */
	public void setCtrlMaxValueActive(boolean isCtrlMaxValueActive) {
		this.isCtrlMaxValueActive = isCtrlMaxValueActive;
	}

	/**
	 * Restituisce se il controllo sul valore minimo è attivo.<br>
	 * <p>
	 * @return the isCtrlMinValueActive
	 */
	public boolean isCtrlMinValueActive() {
		return isCtrlMinValueActive;
	}

	/**
	 * Imposta se il controllo sul valore minimo è attivo.<br>
	 * <p>
	 * @param isCtrlMinValueActive the isCtrlMinValueActive to set
	 */
	public void setCtrlMinValueActive(boolean isCtrlMinValueActive) {
		this.isCtrlMinValueActive = isCtrlMinValueActive;
	}

	/**
	 * Restituisce se il controllo sul numero massimo di caratteri è attivo.<br>
	 * <p>
	 * @return the isCtrlMaxTextSizeActive
	 */
	public boolean isCtrlMaxTextSizeActive() {
		return isCtrlMaxTextSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero massimo di caratteri è attivo.<br>
	 * <p>
	 * @param isCtrlMaxTextSizeActive the isCtrlMaxTextSizeActive to set
	 */
	public void setCtrlMaxTextSizeActive(boolean isCtrlMaxTextSizeActive) {
		this.isCtrlMaxTextSizeActive = isCtrlMaxTextSizeActive;
	}

	/**
	 * Restituisce se il controllo sul numero minimo di caratteri è attivo.<br>
	 * <p>
	 * @return the isCtrlMinTextSizeActive
	 */
	public boolean isCtrlMinTextSizeActive() {
		return isCtrlMinTextSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero minimo di caratteri è attivo.<br>
	 * <p>
	 * @param isCtrlMinTextSizeActive the isCtrlMinTextSizeActive to set
	 */
	public void setCtrlMinTextSizeActive(boolean isCtrlMinTextSizeActive) {
		this.isCtrlMinTextSizeActive = isCtrlMinTextSizeActive;
	}

	/**
	 * Restituisce se il controllo sul numero massimo di digit interi è attivo.<br>
	 * <p>
	 * @return the isCtrlMaxDigitIntSizeActive
	 */
	public boolean isCtrlMaxDigitIntSizeActive() {
		return isCtrlMaxDigitIntSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero massimo di digit interi è attivo.<br>
	 * <p>
	 * @param isCtrlMaxDigitIntSizeActive the isCtrlMaxDigitIntSizeActive to set
	 */
	public void setCtrlMaxDigitIntSizeActive(boolean isCtrlMaxDigitIntSizeActive) {
		this.isCtrlMaxDigitIntSizeActive = isCtrlMaxDigitIntSizeActive;
	}

	/**
	 * Restituisce se il controllo sul numero minimo di digit interi è attivo.<br>
	 * <p>
	 * @return the isCtrlMinDigitIntSizeActive
	 */
	public boolean isCtrlMinDigitIntSizeActive() {
		return isCtrlMinDigitIntSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero minimo di digit interi è attivo.<br>
	 * <p>
	 * @param isCtrlMinDigitIntSizeActive the isCtrlMinDigitIntSizeActive to set
	 */
	public void setCtrlMinDigitIntSizeActive(boolean isCtrlMinDigitIntSizeActive) {
		this.isCtrlMinDigitIntSizeActive = isCtrlMinDigitIntSizeActive;
	}

	/**
	 * Restituisce se il controllo sul numero massimo di digit decimali è attivo.<br>
	 * <p>
	 * @return the isCtrlMaxDigitDecSizeActive
	 */
	public boolean isCtrlMaxDigitDecSizeActive() {
		return isCtrlMaxDigitDecSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero massimo di digit decimali è attivo.<br>
	 * <p>
	 * @param isCtrlMaxDigitDecSizeActive the isCtrlMaxDigitDecSizeActive to set
	 */
	public void setCtrlMaxDigitDecSizeActive(boolean isCtrlMaxDigitDecSizeActive) {
		this.isCtrlMaxDigitDecSizeActive = isCtrlMaxDigitDecSizeActive;
	}

	/**
	 * Restituisce se il controllo sul numero minimo di digit decimali è attivo.<br>
	 * <p>
	 * @return the isCtrlMinDigitDecSizeActive
	 */
	public boolean isCtrlMinDigitDecSizeActive() {
		return isCtrlMinDigitDecSizeActive;
	}

	/**
	 * Imposta se il controllo sul numero minimo di digit decimali è attivo.<br>
	 * <p>
	 * @param isCtrlMinDigitDecSizeActive the isCtrlMinDigitDecSizeActive to set
	 */
	public void setCtrlMinDigitDecSizeActive(boolean isCtrlMinDigitDecSizeActive) {
		this.isCtrlMinDigitDecSizeActive = isCtrlMinDigitDecSizeActive;
	}

	/**
	 * Restituisce il codice della tabella di controllo del metacampo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * <p>
	 * @return the ctrlTableCode
	 */
	public String getCtrlTableCode() {
		return ctrlTableCode;
	}

	/**
	 * Imposta il codice della tabella di controllo del metacampo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * <p>
	 * @param ctrlTableCode the ctrlTableCode to set
	 */
	public void setCtrlTableCode(String ctrlTableCode) {
		this.ctrlTableCode = ctrlTableCode;
	}

	/**
	 * Restituisce il numero della tabella di controllo del metacampo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * <p>
	 * @return the ctrlTablenum
	 */
	public int getCtrlTableNum() {
		return ctrlTableNum;
	}

	/**
	 * Imposta il numero della tabella di controllo del metacampo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * <p>
	 * @param ctrlTablenum the ctrlTablenum to set
	 */
	public void setCtrlTableNum(int ctrlTablenum) {
		this.ctrlTableNum = ctrlTablenum;
	}

	/**
	 * Restituisce la sequenza di nomi di campi chiave della tabella di controllo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * Quando sono necessari più campi per comporre la chiave di accesso al sistema tabelle<br>
	 * è necessario conoscere i nomi di tutti i campi (metacampo) per comporre la key della tabella.<br>
	 * Questa operazione viene effettuata runtime con i campi a disposizione nei form e nei pannelli.<br>
	 * <p>
	 * @return the al_ctrlTableKey
	 */
	public ArrayList<String> getCtrlTableKeys() {
		return al_ctrlTableKey;
	}

	/**
	 * Imposta la sequenza di nomi di campi chiave della tabella di controllo<br>
	 * <p>
	 * La tabella è gestita dal sistema tabelle di forward.<br>
	 * Quando sono necessari più campi per comporre la chiave di accesso al sistema tabelle<br>
	 * è necessario conoscere i nomi di tutti i campi (metacampo) per comporre la key della tabella.<br>
	 * Questa operazione viene effettuata runtime con i campi a disposizione nei form e nei pannelli.<br>
	 * <p>
	 * @param al_ctrlTableKey the al_ctrlTableKey to set
	 */
	public void setCtrlTableKeys(ArrayList<String> al_ctrlTableKey) {
		this.al_ctrlTableKey = al_ctrlTableKey;
	}

	/**
	 * Restitisce la tabella db, se esiste, proprietaria del metacampo.<br>
	 * <p>
	 * @return the dbEntityOwner
	 */
	public String getDbEntityOwner() {
		return dbEntityOwner;
	}

	/**
	 * Imposta la tabella db, se esiste, proprietaria del metacampo.<br>
	 * <p>
	 * @param dbEntityOwner the dbEntityOwner to set
	 */
	public void setDbEntityOwner(String dbEntityOwner) {
		this.dbEntityOwner = dbEntityOwner;
	}

	/**
	 * Restitisce il nome della tabella db, se esiste, proprietaria del metacampo.<br>
	 * <p>
	 * @return the dbEntityColumn
	 */
	public String getDbEntityColumn() {
		return dbEntityColumn;
	}

	/**
	 * Imposta il nome della tabella db, se esiste, proprietaria del metacampo.<br>
	 * <p>
	 * @param dbEntityColumn the dbEntityColumn to set
	 */
	public void setDbEntityColumn(String dbEntityColumn) {
		this.dbEntityColumn = dbEntityColumn;
	}

	/**
	 * Restituisce il nome java con cui il metadato è codificato nella classe di gestione dell'entity owner, se esiste<br>
	 * <p>
	 * @return the dbEntityJavaName
	 */
	public String getDbEntityJavaName() {
		return dbEntityJavaName;
	}

	/**
	 * Imposta il nome java con cui il metadato è codificato nella classe di gestione dell'entity owner, se esiste<br>
	 * <p>
	 * @param dbEntityJavaName the dbEntityJavaName to set
	 */
	public void setDbEntityJavaName(String dbEntityJavaName) {
		this.dbEntityJavaName = dbEntityJavaName;
	}

	/**
	 * Restituisce il nome della logical data view da utilizzare per ottenere 
	 * il valore o i valori del metacampo.<br>
	 * <p>
	 * @return the sourceLdv
	 */
	public String getSourceLdv() {
		return sourceLdv;
	}

	/**
	 * Imposta il nome della logical data view da utilizzare per ottenere 
	 * il valore o i valori del metacampo.<br>
	 * <p>
	 * @param sourceLdv the sourceLdv to set
	 */
	public void setSourceLdv(String sourceLdv) {
		this.sourceLdv = sourceLdv;
	}

	/**
	 * Restituisce il nome della colonna della logical data view da utilizzare per ottenere 
	 * il valore o i valori del metacampo.<br>
	 * <p>
	 * Normalmente coincide con il nome del campo utilizzato internamente dalle applicazioni.<br>
	 * <p>
	 * @return the sourceLdvColumn
	 */
	public String getSourceLdvColumn() {
		return sourceLdvColumn;
	}

	/**
	 * Imposta il nome della colonna della logical data view da utilizzare per ottenere 
	 * il valore o i valori del metacampo.<br>
	 * <p>
	 * Normalmente coincide con il nome del campo utilizzato internamente dalle applicazioni.<br>
	 * <p>
	 * @param sourceLdvColumn the sourceLdvColumn to set
	 */
	public void setSourceLdvColumn(String sourceLdvColumn) {
		this.sourceLdvColumn = sourceLdvColumn;
	}
	
	
	
}
