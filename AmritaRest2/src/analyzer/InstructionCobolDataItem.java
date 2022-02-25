package analyzer;
import java.io.Serializable;
import java.util.ArrayList;

import utilities.StringService;

import enums.EnumCobolFigurativeConstants;
import enums.EnumCobolUsage;
import enums.EnumCobolValueType;
import enums.EnumDataItemLevel;
import enums.EnumDataItemType;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
  * 
  * <h1>
  * InstructionCobolDataItem 
  * </h1>
  *  <p>
  * Questa classe  qualifica un attributo, campo dati, elementare, per descrivere le caratteristiche 
  * del linguaggio Cobol.<br>
  * Nella forma più generica un data item rappresenta una informazione numerica o testuale 
  * e viene qualificata da un nome e un valore iniziale.
  * Vengono descritte le specifiche caratteristiche di un data item Cobol, quali il numero di livello, 
  * gli attributi, la picture e altro.<br>
  * Questa classe descrive il data item Cobol indipendentemente dalla sua locazione, Working piuttosto che
  * Data Division oppure all'interno di un modulo copy.<br>
  * Questo genere di informazione viene modellata daslle classi che descrivono le strutture dei dati interne
  * al programma e le istruzioni di programma stesso.
  *  
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 15/02/2010
  * @see DataItem
  * @see Analyzer
  *   
*/

public class InstructionCobolDataItem extends DataItem implements AmritaConstants {

	private static final long serialVersionUID = 1L;

	/////////////////////////////////////////////////////////////////////////////
	// Opzioni e valori data item Cobol 
	/////////////////////////////////////////////////////////////////////////////
    
	// Tipologia specifica 
    private EnumDataItemType en_ItemType = null;  	    						// Tipologia specifica item  (T0008)

	// Campo di gruppo o di definizione
    private boolean groupField = false;		        							// True indica campo di gruppo senza Picture, usage, ...
 
	// Identificazione campo
	private int levelNumber = 0;												// Numero livello cobol
	private EnumDataItemLevel en_Level = null;									// Classe livello: 77, 88, 66, altro ...
    
 	// Condition name  (livello 88)
    private boolean conditionClause = false;									// True indica Condition name
    private ArrayList<InnerConditionValueEntry> al_ConditionValueEntry = null;	// Descrittore coppie condition-literal
    
	// Campo elemento di occurs 
    private boolean underOccurs = false;		        		    			// True indica campo definito sotto una occurs
	private String dataNameOccursOwner = "";        							// Nome campo occurs di gruppo sotto il quale la definizione corrente è collocata
	private int dataNameOccursOwnerOrdinal = 0;      							// Numero ordinale nella struttura generale di definizione
	
    // Occurs
    private boolean occursClause = false;										// True indica occurs Fixed-Length o Variable-Length Tables

    // Occurs Fixed-Length Tables
    private int occursNumber = 0;	                      						// Numero occorrenze fisse gruppo
    
    // Occurs Variable-Length Tables
    private int occursNumberFrom = 0;	            	  						// Numero occorrenze variabili gruppo start
    private int occursNumberTo = 0;	                	  						// Numero occorrenze variabili gruppo to times
    private ArrayList<InnerOccursKeyEntry> al_OccursKeyEntry = null;			// Ascending key dataName1 2 .. Descending key dataName5 ... 
    private String occursDependingOnDataName = "";		  						// Campo indicante le occorrenze effettive della tabella 

    // Occurs Fixed-Length & Variable-Length Tables
    private ArrayList<String> al_OccursIndexName = null;	    	  		    // Indexed By Index-name1 Index-name2 .....

    // Redefines & renames
    private boolean redefinesClause = false;									// True indica redefines
    private boolean renamesClause = false;		    							// True indica renames valido solo per il livello 66
    private String redefinesDataName = "";	       				 				// Campo ridefinito 
    private String renamesDataName = "";	        							// Campo rinominato di inizio (o il solo) 
    private String renamesDataNameThru = "";	    							// Campo rinominato di fine  

    // Value
    private boolean valueClause = false;										// True indica value
	private EnumCobolValueType valueType = null;            					// Tipologia value
	private String valueString = "";                         	 				// Se valore stringa
	private int valueNumeric = 0;                         	 					// Se valore numerico
	private EnumCobolFigurativeConstants en_ValueFigurative = null;				// Se costante figurativa

    // Picture
	private boolean pictureClause = false;										// True indica Picture
    private String picture = ""; 												// Picture così come codificata nel sorgente            
    
    // Usage
    private boolean usageClause = false;										// True indica Usage
    private EnumCobolUsage en_Usage = null;	  					    			// Tipologia campo 
 
    // Like
    private boolean likeClause = false;											// True indica Like
    private String likeDataName = "";                     						// Like campo
    private int likeInteger =0;                           						// Like campo(integer)

    // Sign
    private boolean signClause = false;											// True indica Sign
    private int signType = COBOL_SIGN_LEADING;      	  						// Leading/Trailing 
    private boolean signSeparateCharacter = false;		  						// Separate Character

    // Justified
    private boolean justifiedClause = false;									// True indica Justified
    private int justifiedType = COBOL_JUSTIFIED_LEFT;							//
    
    // Syncronized
    private boolean sincronizedClause = false;									// True indica Sincronized
    private int sincronizedType = COBOL_SYNCRONIZED_LEFT; 						// Left/Right 
  
    // Opzioni semplici senza valori
    private boolean blankWhenZeroClause = false;					 
    private boolean globalClause = false;							 
    private boolean externalClause = false;							 
   
   
    
	/**
	 * Costruttore vuoto
	 */
	public InstructionCobolDataItem() {
		super("");
		en_ItemType =  EnumDataItemType .NOT_ASSIGNED;
		en_Level = EnumDataItemLevel.NOT_ASSIGNED;
	    en_Usage = EnumCobolUsage.USAGE_DISPLAY;	 
		en_ValueFigurative = EnumCobolFigurativeConstants.NOT_ASSIGNED;
		al_ConditionValueEntry = new ArrayList<InnerConditionValueEntry>  ();
		al_OccursKeyEntry = new ArrayList<InnerOccursKeyEntry> ();
		al_OccursIndexName = new ArrayList<String>  ();
		al_ConditionValueEntry.trimToSize();
		al_OccursKeyEntry.trimToSize();
		al_OccursIndexName.trimToSize();
	}


    
	/*  
	 * Costruttore  
	 */
	public InstructionCobolDataItem(String dataName) {
		super(dataName);
		en_ItemType =  EnumDataItemType .NOT_ASSIGNED;
		en_Level = EnumDataItemLevel.NOT_ASSIGNED;
	    en_Usage = EnumCobolUsage.NOT_ASSIGNED;	 
		en_ValueFigurative = EnumCobolFigurativeConstants.NOT_ASSIGNED;
		al_ConditionValueEntry.trimToSize();
		al_OccursKeyEntry.trimToSize();
		al_OccursIndexName.trimToSize();
	}

	
	////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////                    Getter e Setter                    /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

	
	/**
	 * 
	 * Restituisce la la tipologia generale dell'item
	 * 
	 * @return the en_ItemType
	 */
	public EnumDataItemType getItemType() {
		return en_ItemType;
	}



	/**
	 * 
	 * Imposta la tipologia generale dell'item
	 * @param instruction 
	 * 
	 * @param en_ItemType the en_ItemType to set
	 */
	public void setItemType(EnumDataItemType en_ItemType) {
		this.en_ItemType = en_ItemType;
	}


	
	/**
	 * Restituisce il numero di livello del campo.<br>
	 * <p>
	 * @return the levelNumber
	 */
	public int getLevelNumber() {
		return levelNumber;
	}


	/**
	 * Imposta il numero di livello del campo.<br>
	 * <p>
	 * 
	 * @param levelNumber the levelNumber to set
	 */
	public void setLevelNumber(int levelNumber) {
		this.levelNumber = levelNumber;
	}


	/**
	 * Restituisce la tipologia del livello Cobol, ovvero
	 * se numero 77, 66 o 88.<br>
	 * <p>
	 * @return the en_Level
	 */
	public EnumDataItemLevel getLevelType() {
		return en_Level;
	}


	/**
	 * Imposta la tipologia del livello Cobol, ovvero
	 * se numero 77, 66 o 88.<br>
	 * <p>
	 * 
	 * @param EnumDataItemLevel en_Level the en_Level to set
	 */
	public void setLevelType(EnumDataItemLevel en_Level) {
		this.en_Level = en_Level;
	}


	/**
	 * @return the conditionClause
	 */
	public boolean isConditionClause() {
		return conditionClause;
	}


	/**
	 * @param conditionClause the conditionClause to set
	 */
	public void setConditionClause(boolean conditionClause) {
		this.conditionClause = conditionClause;
	}


	/**
	 * @return the al_ConditionValue
	 */
	public ArrayList<InnerConditionValueEntry> getConditionValues() {
		return al_ConditionValueEntry;
	}

	
	/**
	 * @param al_ConditionValue the ar_ConditionValue to set
	 */
	public void setConditionValues(ArrayList<InnerConditionValueEntry> al_ConditionValueEntry) {
		this.al_ConditionValueEntry = al_ConditionValueEntry;
	}



	/**
	 * @return the underOccurs
	 */
	public boolean isUnderOccurs() {
		return underOccurs;
	}


	/**
	 * @param underOccurs the underOccurs to set
	 */
	public void setUnderOccurs(boolean underOccurs) {
		this.underOccurs = underOccurs;
	}


	/**
	 * @return the dataNameOccursOwner
	 */
	public String getDataNameOccursOwner() {
		return dataNameOccursOwner;
	}


	/**
	 * @param dataNameOccursOwner the dataNameOccursOwner to set
	 */
	public void setDataNameOccursOwner(String dataNameOccursOwner) {
		this.dataNameOccursOwner = dataNameOccursOwner;
	}


	/**
	 * @return the dataNameOccursOwnerOrdinal
	 */
	public int getDataNameOccursOwnerOrdinal() {
		return dataNameOccursOwnerOrdinal;
	}


	/**
	 * @param dataNameOccursOwnerOrdinal the dataNameOccursOwnerOrdinal to set
	 */
	public void setDataNameOccursOwnerOrdinal(int dataNameOccursOwnerOrdinal) {
		this.dataNameOccursOwnerOrdinal = dataNameOccursOwnerOrdinal;
	}


	/**
	 * @return the groupField
	 */
	public boolean isGroupField() {
		return groupField;
	}


	/**
	 * @param groupField the groupField to set
	 */
	public void setGroupField(boolean groupField) {
		this.groupField = groupField;
	}



	/**
	 * @return the occursClause
	 */
	public boolean isOccursClause() {
		return occursClause;
	}


	/**
	 * @param occursClause the occursClause to set
	 */
	public void setOccursClause(boolean occursClause) {
		this.occursClause = occursClause;
	}


	/**
	 * @return the occursNumber
	 */
	public int getOccursNumber() {
		return occursNumber;
	}


	/**
	 * @param occursNumber the occursNumber to set
	 */
	public void setOccursNumber(int occursNumber) {
		this.occursNumber = occursNumber;
	}


	/**
	 * @return the alccursKey
	 */
	public ArrayList<InnerOccursKeyEntry> getOccursKeys() {
		return al_OccursKeyEntry;
	}

	/**
	 * @param arOccursKey the alOccursKey to set
	 */
	public void setOccursKeys(ArrayList<InnerOccursKeyEntry> al_OccursKey) {
		this.al_OccursKeyEntry = al_OccursKey;
	}



	/**
	 * @return the occursNumberFrom
	 */
	public int getOccursNumberFrom() {
		return occursNumberFrom;
	}


	/**
	 * @param occursNumberFrom the occursNumberFrom to set
	 */
	public void setOccursNumberFrom(int occursNumberFrom) {
		this.occursNumberFrom = occursNumberFrom;
	}


	/**
	 * @return the occursNumberTo
	 */
	public int getOccursNumberTo() {
		return occursNumberTo;
	}


	/**
	 * @param occursNumberTo the occursNumberTo to set
	 */
	public void setOccursNumberTo(int occursNumberTo) {
		this.occursNumberTo = occursNumberTo;
	}


	/**
	 * @return the occursDependingOnDataName
	 */
	public String getOccursDependingOnDataName() {
		return occursDependingOnDataName;
	}


	/**
	 * @param occursDependingOnDataName the occursDependingOnDataName to set
	 */
	public void setOccursDependingOnDataName(String occursDependingOnDataName) {
		this.occursDependingOnDataName = occursDependingOnDataName;
	}


	/**
	 * @return the alOccursIndexeName
	 */
	public ArrayList<String> getOccursIndexNames() {
		return al_OccursIndexName;
	}


	/**
	 * @param arOccursIndexeName the arOccursIndexeName to set
	 */
	public void setOccursIndexNames(ArrayList<String> al_OccursIndexName) {
		this.al_OccursIndexName = al_OccursIndexName;
	}


	/**
	 * @return the redefinesClause
	 */
	public boolean isRedefinesClause() {
		return redefinesClause;
	}


	/**
	 * @param redefinesClause the redefinesClause to set
	 */
	public void setRedefinesClause(boolean redefinesClause) {
		this.redefinesClause = redefinesClause;
	}


	/**
	 * @return the renamesClause
	 */
	public boolean isRenamesClause() {
		return renamesClause;
	}


	/**
	 * @param renamesClause the renamesClause to set
	 */
	public void setRenamesClause(boolean renamesClause) {
		this.renamesClause = renamesClause;
	}


	/**
	 * @return the redefinesDataName
	 */
	public String getRedefinesDataName() {
		return redefinesDataName;
	}


	/**
	 * @param redefinesDataName the redefinesDataName to set
	 */
	public void setRedefinesDataName(String redefinesDataName) {
		this.redefinesDataName = redefinesDataName;
	}


	/**
	 * @return the renamesDataName
	 */
	public String getRenamesDataName() {
		return renamesDataName;
	}


	/**
	 * @param renamesDataName the renamesDataName to set
	 */
	public void setRenamesDataName(String renamesDataName) {
		this.renamesDataName = renamesDataName;
	}


	/**
	 * @return the renamesDataNameThru
	 */
	public String getRenamesDataNameThru() {
		return renamesDataNameThru;
	}


	/**
	 * @param renamesDataNameThru the renamesDataNameThru to set
	 */
	public void setRenamesDataNameThru(String renamesDataNameThru) {
		this.renamesDataNameThru = renamesDataNameThru;
	}


	/**
	 * @return the valueClause
	 */
	public boolean isValueClause() {
		return valueClause;
	}


	/**
	 * @param valueClause the valueClause to set
	 */
	public void setValueClause(boolean valueClause) {
		this.valueClause = valueClause;
	}


	/**
	 * @return the valueType
	 */
	public EnumCobolValueType getValueType() {
		return valueType;
	}


	/**
	 * @param valueType the valueType to set
	 */
	public void setValueType(EnumCobolValueType valueType) {
		this.valueType = valueType;
	}


	/**
	 * @return the valueString
	 */
	public String getValueString() {
		return valueString;
	}


	/**
	 * 
	 * Restituisce la stringa di Value senza l'apice iniziale e finale,
	 * della stessa lunghezza della stringa stessa.
	 * 
	 * @return the valueString
	 */
	public String getValueStringFormatted() {
		String valueFormatted = "";
		
		if (this.getValueType() == EnumCobolValueType.VALUE_FIGURATIVE) {
			if (this.getValueFigurative() == EnumCobolFigurativeConstants.SPACE
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.SPACES) {
		        valueFormatted = StringService._pad("", ' ', this.getSizeBytes(), StringService.PAD_RIGHT);
		        return valueFormatted;
			}
			if (this.getValueFigurative() == EnumCobolFigurativeConstants.ZERO
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.ZEROS
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.ZEROES) {
		        valueFormatted = StringService._pad("", '0', this.getSizeBytes(), StringService.PAD_RIGHT);
		        return valueFormatted;
			}
		}
		
		if (this.getValueType() != EnumCobolValueType.VALUE_LITERAL_ALPHA) {
			return "";
		}
		
		// Literal alfanumerica: eliminazione apici
		
		valueFormatted = valueString;
		if (valueFormatted.equals("")) {
			valueFormatted = StringService._pad(valueFormatted, ' ', this.getSizeBytes(), StringService.PAD_RIGHT);
			return valueFormatted;
		}

		valueFormatted = valueFormatted.substring(1);
		valueFormatted = valueFormatted.substring(0, valueFormatted.length() - 1);
		return valueFormatted;
	}

	/**
	 * 
	 * Restituisce la stringa di Value senza l'apice iniziale e finale,
	 * della lunghezza definita per il campo, eventualmente fillata con spazi.
	 * 
	 * @return the valueString formattata alla lunghezza di definizione
	 */
	public String getValueStringFormattedToSize() {
		String valueFormatted = "";

		if (this.getValueType() == EnumCobolValueType.VALUE_FIGURATIVE) {
			if (this.getValueFigurative() == EnumCobolFigurativeConstants.SPACE
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.SPACES) {
		        valueFormatted = StringService._pad("", ' ', this.getSizeBytes(), StringService.PAD_RIGHT);
		        return valueFormatted;
			}
			if (this.getValueFigurative() == EnumCobolFigurativeConstants.ZERO
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.ZEROS
			||  this.getValueFigurative() == EnumCobolFigurativeConstants.ZEROES) {
		        valueFormatted = StringService._pad("", '0', this.getSizeBytes(), StringService.PAD_RIGHT);
		        return valueFormatted;
			}
		}
		
		if (this.getValueType() != EnumCobolValueType.VALUE_LITERAL_ALPHA) {
			return "";
		}
		
		// Literal alfanumerica: eliminazione apici
		
		valueFormatted = valueString;
		if (valueFormatted.equals("")) {
			valueFormatted = StringService._pad(valueFormatted, ' ', this.getSizeBytes(), StringService.PAD_RIGHT);
			return valueFormatted;
		}
		
		valueFormatted = valueFormatted.substring(1);
		valueFormatted = valueFormatted.substring(0, valueFormatted.length() - 1);
		
		valueFormatted = StringService._pad(valueFormatted, ' ', this.getSizeBytes(), StringService.PAD_RIGHT);
		return valueFormatted;
	}


	/**
	 * @param valueString the valueString to set
	 */
	public void setValueString(String valueString) {
		this.valueString = valueString;
	}


	/**
	 * @return the valueNumeric
	 */
	public int getValueNumeric() {
		return valueNumeric;
	}


	/**
	 * @param valueNumeric the valueNumeric to set
	 */
	public void setValueNumeric(int valueNumeric) {
		this.valueNumeric = valueNumeric;
	}


	/**
	 * @return the en_ValueFigurative
	 */
	public EnumCobolFigurativeConstants getValueFigurative() {
		return en_ValueFigurative;
	}


	/**
	 * @param en_ValueFigurative the en_ValueFigurative to set
	 */
	public void setValueFigurative(
			EnumCobolFigurativeConstants en_ValueFigurative) {
		this.en_ValueFigurative = en_ValueFigurative;
	}


	/**
	 * @return the pictureClause
	 */
	public boolean isPictureClause() {
		return pictureClause;
	}

	/**
	 * Restituisce true se il campo è in formato display.<br>
	 * <p>
	 * Si tratta di campi sefiniti senza clausola usage oppure
	 * con clausola usage Display esplicita.<br>
	 * 
	 * 
	 * @return if the field is not bynary
	 */
	public boolean isDataItemDisplay() {
        
		// Usage Display specificato
		if (this.getUsage() == EnumCobolUsage.USAGE_DISPLAY
	    ||  this.getUsage() == EnumCobolUsage.USAGE_DISPLAY_1) {
			return true;
		}
		
	    // Si tratta di un campo non visualizzabile direttamente in EBCDIC/ASCII

		// USAGE_DISPLAY,                     // (01) Alfanumerico X
		// USAGE_DISPLAY_1,                   // (02) Alfanumerico X
		// USAGE_INDEX(4),                    // (03) Numerico indice 4 bytes length
		// USAGE_POINTER(4),                  // (04) Numerico Puntatore 4 bytes length
		// USAGE_FUNCTION_POINTER(4),		  // (05) Numerico puntatore 4 bytes length
		// USAGE_PROCEDURE_POINTER(8),		  // (06) Numerico puntatore 8 bytes length
		// USAGE_BINARY,                      // (07) Numerico binario 2/4/8 bytes length se numero digits <= 4/9/18
		// USAGE_COMPUTATIONAL,               // (08) Equivalente a bynary
		// USAGE_COMP,                        // (09) Equivalente a bynary
		// USAGE_COMPUTATIONAL_1(4),          // (10) Numerico floating point singola precisione 4 bytes long
		// USAGE_COMP_1(4),                   // (11) Numerico floating point singola precisione 4 bytes long
		// USAGE_COMPUTATIONAL_2(8),          // (12) Numerico floating point doppia precisione 8 bytes long
		// USAGE_COMP_2(8),                   // (13) Numerico floating point doppia precisione 8 bytes long
		// USAGE_COMPUTATIONAL_3,             // (14) Numerico Packed decimal
		// USAGE_COMP_3,                      // (15) Numerico packed decimal
		// USAGE_COMPUTATIONAL_4,             // (16) Equivalente a bynary
		// USAGE_COMP_4,                      // (17) Equivalente a bynary
		// USAGE_COMPUTATIONAL_5,             // (18) Numerico Floating
		// USAGE_COMP_5,                      // (19) Numerico Floating
		// USAGE_PACKED_DECIMAL;              // (20) Numerico packed decimal
		
		return false;
	}



	/**
	 * @param pictureClause the pictureClause to set
	 */
	public void setPictureClause(boolean pictureClause) {
		this.pictureClause = pictureClause;
	}


	/**
	 * @return the picture
	 */
	public String getPicture() {
		return picture;
	}


	/**
	 * @param picture the picture to set
	 */
	public void setPicture(String picture) {
		this.picture = picture;
	}


	/**
	 * @return the usageClause
	 */
	public boolean isUsageClause() {
		return usageClause;
	}


	/**
	 * @param usageClause the usageClause to set
	 */
	public void setUsageClause(boolean usageClause) {
		this.usageClause = usageClause;
	}


	/**
	 * @return the en_Usage
	 */
	public EnumCobolUsage getUsage() {
		return en_Usage;
	}


	/**
	 * @param en_Usage the en_Usage to set
	 */
	public void setUsage(EnumCobolUsage en_Usage) {
		this.en_Usage = en_Usage;
	}


	/**
	 * @return the likeClause
	 */
	public boolean isLikeClause() {
		return likeClause;
	}


	/**
	 * @param likeClause the likeClause to set
	 */
	public void setLikeClause(boolean likeClause) {
		this.likeClause = likeClause;
	}


	/**
	 * @return the likeDataName
	 */
	public String getLikeDataName() {
		return likeDataName;
	}


	/**
	 * @param likeDataName the likeDataName to set
	 */
	public void setLikeDataName(String likeDataName) {
		this.likeDataName = likeDataName;
	}


	/**
	 * @return the likeInteger
	 */
	public int getLikeInteger() {
		return likeInteger;
	}


	/**
	 * @param likeInteger the likeInteger to set
	 */
	public void setLikeInteger(int likeInteger) {
		this.likeInteger = likeInteger;
	}


	/**
	 * @return the signClause
	 */
	public boolean isSignClause() {
		return signClause;
	}


	/**
	 * @param signClause the signClause to set
	 */
	public void setSignClause(boolean signClause) {
		this.signClause = signClause;
	}


	/**
	 * @return the signType
	 */
	public int getSignType() {
		return signType;
	}


	/**
	 * @param signType the signType to set
	 */
	public void setSignType(int signType) {
		this.signType = signType;
	}


	/**
	 * @return the signSeparateCharacter
	 */
	public boolean isSignSeparateCharacter() {
		return signSeparateCharacter;
	}


	/**
	 * @param signSeparateCharacter the signSeparateCharacter to set
	 */
	public void setSignSeparateCharacter(boolean signSeparateCharacter) {
		this.signSeparateCharacter = signSeparateCharacter;
	}


	/**
	 * @return the justifiedClause
	 */
	public boolean isJustifiedClause() {
		return justifiedClause;
	}


	/**
	 * @param justifiedClause the justifiedClause to set
	 */
	public void setJustifiedClause(boolean justifiedClause) {
		this.justifiedClause = justifiedClause;
	}


	/**
	 * @return the justifiedType
	 */
	public int getJustifiedType() {
		return justifiedType;
	}


	/**
	 * @param justifiedType the justifiedType to set
	 */
	public void setJustifiedType(int justifiedType) {
		this.justifiedType = justifiedType;
	}


	/**
	 * @return the sincronizedClause
	 */
	public boolean isSincronizedClause() {
		return sincronizedClause;
	}


	/**
	 * @param sincronizedClause the sincronizedClause to set
	 */
	public void setSincronizedClause(boolean sincronizedClause) {
		this.sincronizedClause = sincronizedClause;
	}


	/**
	 * @return the sincronizedType
	 */
	public int getSincronizedType() {
		return sincronizedType;
	}


	/**
	 * @param sincronizedType the sincronizedType to set
	 */
	public void setSincronizedType(int sincronizedType) {
		this.sincronizedType = sincronizedType;
	}


	/**
	 * @return the blankWhenZeroClause
	 */
	public boolean isBlankWhenZeroClause() {
		return blankWhenZeroClause;
	}


	/**
	 * @param blankWhenZeroClause the blankWhenZeroClause to set
	 */
	public void setBlankWhenZeroClause(boolean blankWhenZeroClause) {
		this.blankWhenZeroClause = blankWhenZeroClause;
	}


	/**
	 * @return the globalClause
	 */
	public boolean isGlobalClause() {
		return globalClause;
	}


	/**
	 * @param globalClause the globalClause to set
	 */
	public void setGlobalClause(boolean globalClause) {
		this.globalClause = globalClause;
	}


	/**
	 * @return the externalClause
	 */
	public boolean isExternalClause() {
		return externalClause;
	}


	/**
	 * @param externalClause the externalClause to set
	 */
	public void setExternalClause(boolean externalClause) {
		this.externalClause = externalClause;
	}






	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Classi interne di servizio usate come strutture dati  /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////
	




	/*
	 *   Classe contenitore di servizio con la descrizione di una condizione a livello 88.
	 *   La condizione può essere un singolo valore, e in questo caso il nome condizione a 
	 *   livello 88 indica un solo valore di condizione.
	 *   Oppure la condizione può essere una coppia di valori e in questo caso indica un range
	 *   che deve essere verificato fra i due valori (literal 1 THRU literal 2).
	 *   I valori sono espressi da literal, numeriche o alfanumeriche, in base al tipo del
	 *   campo al quale il livello 88 è associato.
	 *   Literal-thru può quindi essere a "" o zero.
	 *   
	 */
	public class InnerConditionValueEntry implements Cloneable, Serializable {
		
		private static final long serialVersionUID = 1L;
		
		// Tipologie literal
		EnumCobolValueType literalFromType = null;        				// Tipologia literal from
		EnumCobolValueType literalThruType = null;        				// Tipologia literal thru
		
		// Se valori stringa
		String literalFromString = "";                         		 	// 88 cond-name value literal-1 
		String literalThruString = "";                          		// 88 cond-name value literal-1 Thru literal-2

		// Se valori numerici from
		int literalFromNumericInt = 0;                         		 	// 88 cond-name value From literal int
		long literalFromNumericLong = 0;                         		// 88 cond-name value From literal long
		double literalFromNumericDouble = 0;                         	// 88 cond-name value From literal double
		float literalFromNumericFloat = 0;                         		// 88 cond-name value From literal float

		// Se valori numerici thru
		int literalThruNumericInt = 0;                         		 	// 88 cond-name value thru literal int
		long literalThruNumericLong = 0;                         		// 88 cond-name value thru literal long
		double literalThruNumericDouble = 0;                         	// 88 cond-name value thru literal double
		float literalThruNumericFloat = 0;                         		// 88 cond-name value thru literal float
		
		// Se costanti figurative
		EnumCobolFigurativeConstants en_literalFromFigurative = null;   // 88 cond-name value literal-1 
		EnumCobolFigurativeConstants en_literalThruFigurative = null;   // 88 cond-name value literal-1 Thru literal-2
		
		// Costruttore vuoto
		public InnerConditionValueEntry() {
			super();
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#clone()
		 */
		@Override
		protected Object clone() {
			try {
				return super.clone();
			} catch (Exception e) {
				return null;
			}
		}

	
	}

	/*
	 *   Classe contenitore di servizio con la descrizione di un item di tipo Occurs
	 */
	public class InnerOccursKeyEntry implements Cloneable, Serializable {

		private static final long serialVersionUID = 1L;
		
		int orderType = COBOL_OCCURS_ASCENDING_KEY;    					// Da AmritasConstants
		ArrayList<String> al_dataNameKey = null;                		// Ascending/Descending key Is data-name1...data-namen
		
		// Costruttore
		public InnerOccursKeyEntry() {
			al_dataNameKey = new ArrayList<String> ();
		}
		
		/* (non-Javadoc)
		 * @see java.lang.Object#clone()
		 */
		@Override
		protected Object clone() {
			try {
				return super.clone();
			} catch (Exception e) {
				return null;
			}
		}

	}
	

}
