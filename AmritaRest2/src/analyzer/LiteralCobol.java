package analyzer;
import java.io.Serializable;

import utilities.StringService;
import enums.EnumCobolValueType;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  * 
  * <h1>
  * LiteralCobol 
  * </h1>
  *  <p>
  *
  * Modella una generica literal Cobol con i relativi metodi di servizio per validare e cercare
  * literal valide in una stringa.<br>
  * <p>
  * Nel Cobol le literal possono essere codificate in Data Division oppure in Procedure
  * Division, negli statement Move, When, Display String etc.<br>
  * <p>
  * Viene validata una stringa per verificare se contiene una literal Cobol, che
  * può essere:<br>
  * 
  * <ul>
  * <li>
  * <b>Alfanumerica</b> <br>
  * Si tratta di literal tipo <tt>' JJ'</tt> oppure <tt>"KKK"</tt>
  * </li>
  * <li>
  * <b>Alfanumerica NATIONAL</b><br>
  * Si tratta di literal tipo <tt>N' JJ'</tt> oppure <tt>N"KKK"</tt>
  * </li>
  * <li>
  * <b>Alfanumerica NULL terminated</b><br>
  * Si tratta di literal tipo <tt>Z' JJ'</tt> oppure <tt>Z"KKK"</tt>
  * </li>
  * <li>
  * <b>Esadecimale</b> <br>
  * </li>
  * Si tratta di literal tipo <tt>X'0000'</tt> oppure <tt>X"FFFF"</tt>
  * <li>
  * <b>Esadecimale NATIONAL</b><br>
  * </li>
  * Si tratta di literal tipo <tt>NX'0000'</tt> oppure <tt>NX"FFFF"</tt>
  * <li>
  * <b>Numerica intera</b> <br>
  * </li>
  * Si tratta di literal tipo <tt>+5</tt> oppure <tt>65</tt>  oppure <tt>-3</tt>
  * <li>
  * <b>Numerica floating point</b> <br>
  * </li>
  * Si tratta di literal inella notazione mantissa + caratteristica del tipo:<br>
  * tipo <tt>-0.72E-3</tt> oppure <tt>+0,5E-3</tt>  oppure <tt>0.5E-3</tt>
  * </ul>
  * <p>
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 18/02/2011
  * @see InstructionCobolDataItem
  * @see Analyzer
  *   
 */

public class LiteralCobol  implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;
	
	private EnumCobolValueType literalType = null;      // Tipologia literal
	private String literalInput = "";              	    // Stringa literal integrale con apici delimitatori
	private boolean isLiteralGood = false;              // True indica literal (di ogni tipo) formalmente corretta
	private boolean isLiteralNum = false;               // True indica literal numerica
	private boolean isLiteralNumInt = false;            // True indica literal numerica rappresentabile come int      (input intero)
	private boolean isLiteralNumLong = false;           // True indica literal numerica rappresentabile come Long     (input intero)
	private boolean isLiteralNumDouble = false;         // True indica literal numerica rappresentabile come Double   (input con la virgola)
	private boolean isLiteralNumFloat = false;          // True indica literal numerica rappresentabile come Floating (input in notazione floating point)
	private boolean isLiteralString = false;            // True indica literal alfanumerica o esadecimale che inizia con ', ", N' etc.
	private String valueString = "";              	    // Valore literal stringa con apici delimitatori
	private String valueHex = "";              	        // Valore literal Esadecimale con apici delimitatori
	private int valueInt = 0;              	            // Valore literal numerica intera
	private long valueLong = 0;              	        // Valore literal numerica intera long integer
	private double valueDouble = 0;           	        // Valore literal numerica con virgola
	private float valueFloat = 0;           	        // Valore literal numerica floating point
	
	
	/**
	 * 
	 * Costruttore vuoto
	 * 
	 */
	public LiteralCobol( ) {
		super();
		literalType = EnumCobolValueType.NOT_ASSIGNED;
	}

	/**
	  * Validazione literal alfanumerica e numerica<br>
	  * <p>
	  * 
	  * <ul>
	  * <li>
	  * <b>Alfanumerica</b> <br>
	  * Si tratta di literal tipo <tt>' JJ'</tt> oppure <tt>"KKK"</tt>
	  * </li>
	  * <li>
	  * <b>Alfanumerica NATIONAL</b><br>
	  * Si tratta di literal tipo <tt>N' JJ'</tt> oppure <tt>N"KKK"</tt>
	  * </li>
	  * <li>
	  * <b>Alfanumerica NULL terminated</b><br>
	  * Si tratta di literal tipo <tt>Z' JJ'</tt> oppure <tt>Z"KKK"</tt>
	  * </li>
	  * <li>
	  * <b>Esadecimale</b> <br>
	  * </li>
	  * Si tratta di literal tipo <tt>X'0000'</tt> oppure <tt>X"FFFF"</tt>
	  * <li>
	  * <b>Esadecimale NATIONAL</b><br>
	  * </li>
	  * Si tratta di literal tipo <tt>NX'0000'</tt> oppure <tt>NX"FFFF"</tt>
	  * <li>
	  * <b>Numerica intera</b> <br>
	  * </li>
	  * Si tratta di literal tipo <tt>+5</tt> oppure <tt>65</tt>  oppure <tt>-3</tt>
	  * <li>
	  * <b>Numerica floating point</b> <br>
	  * </li>
	  * Si tratta di literal inella notazione mantissa + caratteristica del tipo:<br>
	  * tipo <tt>-0.72E-3</tt> oppure <tt>+0,5E-3</tt>  oppure <tt>0.5E-3</tt>
	  * </ul>
	  * <p>
	  * La verifica della literal viene effettuata a partire dalla posizione di inizio
	  * fornita.<br>
	  * Dopo la literal, numerica o alfanumerica, può essere presente qualsiasi combinazione di caratteri.<br>
	  * Viene restituita la posizione dell'ultimo carattere della literal, nella stringa
	  * e viene aggiornato il tipo di literal nell'oggetto.<br>
	  * <p>
	  * Viene restituito <tt>-1</tt> se a partire dalla posizione fornita non viene individuata
	  * una literal valida, alfanumerica o numerica.<br>
	  * Viene restituito <tt>-1</tt> se a partire dalla posizione fornita viene individuata
	  * una literal alfanumerica valida ma non terminata correttamente.<br>
	  * In questo caso è disponibile il metodo isLiteralGood() per ottenere questa informazione
	  * <p>
	  * @param String inputString
	  * @param int fromOffset
	  * @return int offset end literal
	 */
	public int parseLiteral(String inputString, int fromOffset) {
		
		String token = "";
		Character chrDoubleApice = '"';
		boolean isFloatNotation = false;        // Indica numero in notazione floating point
		int iEndLiteral = -2;					// Offset di fine literal
		int iStartLiteral = 0;
		
		// Initial
		this.isLiteralString = false;
		this.isLiteralNum = false;
		this.isLiteralNumInt = false;          
		this.isLiteralNumLong = false;           
		this.isLiteralNumDouble = false;       
		this.isLiteralNumFloat = false;          
		this.valueString = "";              	    
		this.valueHex = "";              	         
		this.valueInt = 0;              	            
		this.valueLong = 0;              	       
		this.valueDouble = 0;           	        
		this.valueFloat = 0;           	        
		
		// Solo spazi dalla posizione indicata 
		iStartLiteral = StringService._firstNoSpace(inputString, fromOffset);
		if (iStartLiteral < 0) {
			return -1;
		}
		
		// Literal alfanumerica
		if (inputString.startsWith("'", iStartLiteral) || inputString.charAt(iStartLiteral) == '"') {
			iEndLiteral = posEndLiteral(iStartLiteral, inputString);
			this.literalType = EnumCobolValueType.VALUE_LITERAL_ALPHA;
			this.literalInput = inputString.substring(iStartLiteral, iEndLiteral + 1);
			this.isLiteralString = true;
			if (iEndLiteral > 0) {
				this.isLiteralGood = true;
				this.valueString = inputString.substring(iStartLiteral, iEndLiteral + 1);
			}
			return iEndLiteral;
		
		// Literal alfanumerica NATIONAL
		} else if (inputString.startsWith("N'", iStartLiteral) || inputString.startsWith("N"+chrDoubleApice.toString(), iStartLiteral)) {
			this.literalType = EnumCobolValueType.VALUE_LITERAL_ALPHA_NATIONAL;
			iStartLiteral++;  
			iEndLiteral = posEndLiteral(iStartLiteral, inputString);
			this.literalInput = inputString.substring(iStartLiteral - 1, iEndLiteral + 1);
			this.isLiteralString = true;
			if (iEndLiteral > 0) {
				this.isLiteralGood = true;
				this.valueString = inputString.substring(iStartLiteral, iEndLiteral + 1);
			}
			return iEndLiteral;
		
		// Literal alfanumerica NULL TERMINATED
		} else if (inputString.startsWith("Z'", iStartLiteral) || inputString.startsWith("Z"+chrDoubleApice.toString(), iStartLiteral)) {
			this.literalType = EnumCobolValueType.VALUE_LITERAL_ALPHA_NULL_TERM;
			iStartLiteral++;
			iEndLiteral = posEndLiteral(iStartLiteral, inputString);
			this.literalInput = inputString.substring(iStartLiteral - 1, iEndLiteral + 1);
			this.isLiteralString = true;
			if (iEndLiteral > 0) {
				this.isLiteralGood = true;
				this.valueString = inputString.substring(iStartLiteral, iEndLiteral + 1);
			}
			return iEndLiteral;
		
		// Literal esadecimale
		} else if (inputString.startsWith("X'", iStartLiteral) || inputString.startsWith("X"+chrDoubleApice.toString(), iStartLiteral)) {
			this.literalType = EnumCobolValueType.VALUE_LITERAL_HEX;
			iStartLiteral++;
			iEndLiteral = posEndLiteral(iStartLiteral, inputString);
			this.literalInput = inputString.substring(iStartLiteral - 1, iEndLiteral + 1);
			this.isLiteralString = true;
			if (iEndLiteral > 0) {
				this.isLiteralGood = true;
				this.valueHex = inputString.substring(iStartLiteral, iEndLiteral + 1);
			}
			return iEndLiteral;
		
		// Literal esadecimale NATIONAL
		} else if (inputString.startsWith("NX'", iStartLiteral) || inputString.startsWith("NX"+chrDoubleApice.toString(), iStartLiteral )) {
			this.literalType = EnumCobolValueType.VALUE_LITERAL_HEX_NATIONAL;
			iStartLiteral = iStartLiteral + 2;
			iEndLiteral = posEndLiteral(iStartLiteral, inputString);
			this.literalInput = inputString.substring(iStartLiteral - 2, iEndLiteral + 1);
			this.isLiteralString = true;
			if (iEndLiteral > 0) {
				this.isLiteralGood = true;
				this.literalType = EnumCobolValueType.VALUE_LITERAL_ALPHA;
				this.valueHex = inputString.substring(iStartLiteral, iEndLiteral + 1);
			}
			return iEndLiteral;
		}
		
		// Non può essere l'inizio di una literal numerica
		if (!((inputString.charAt(iStartLiteral) >= '0' && inputString.charAt(iStartLiteral) <= '9') ||
			  (inputString.charAt(iStartLiteral) == '+')                                             ||
			  (inputString.charAt(iStartLiteral) == '-')
			  )
		    ) {
			return -1;
		}
		
		
		// Estrazione token con literal numerica
		iEndLiteral = iStartLiteral + 1;     // Initial
		isFloatNotation = false;
		
		// Scan posizioni numero
		for (; iEndLiteral < inputString.length(); iEndLiteral++) {
			
			// Notazione floating point
			if (inputString.charAt(iEndLiteral) == 'E' ) {
				isFloatNotation = true;
			}
            
			// Sicuramente fine numero, anche floating point
			if (inputString.charAt(iEndLiteral) == ' ' 
				||  inputString.charAt(iEndLiteral) == '*'
				||  inputString.charAt(iEndLiteral) == '/'	
				||  inputString.charAt(iEndLiteral) == '+') {
				break;
			}

			// Ulteriore possibile operatore di terminazione numero se NON floating point
			if (!isFloatNotation) {
			   if (inputString.charAt(iEndLiteral) == '-') {
				  break;
			   }
			}
		}
		
		  
		// Solo spazi dalla posizione indicata 
		if (iEndLiteral >= inputString.length()) {   
			token = inputString.substring(iStartLiteral);
			iEndLiteral = inputString.length() - 1;
		} else {
			token = inputString.substring(iStartLiteral, iEndLiteral);
			iEndLiteral--;
		}
		       
		// Numerico intero 
		if (StringService._isNumericInt(token)) {
			this.isLiteralGood = true;
			this.isLiteralNum = true;
			this.isLiteralNumInt = true;
			this.literalType = EnumCobolValueType.VALUE_LITERAL_NUM_INT;
			this.valueInt = StringService._getNumericInt(token);
			return iEndLiteral;
		}

		// Numerico intero long
		if (StringService._isNumericLong(token)) {
			this.isLiteralGood = true;
			this.isLiteralNum = true;
			this.isLiteralNumLong = true;
			this.literalType = EnumCobolValueType.VALUE_LITERAL_NUM_LONG;
			if (StringService._getNumericLong(token) == null) {
				this.valueLong = 0;
			} else {
				this.valueLong = StringService._getNumericLong(token);
			}
			return iEndLiteral;
		}

		// Numerico double, con virgola 
		if (StringService._isNumericDouble(token)) {
			this.isLiteralGood = true;
			this.isLiteralNum = true;
			this.isLiteralNumDouble = true;
			this.literalType = EnumCobolValueType.VALUE_LITERAL_NUM_WITH_COMMA;
			this.valueDouble = StringService._getNumericDouble(token);
			return iEndLiteral;
		}
		
		// Numerico floating point
		if (StringService._isNumericFloating(token)) {
			this.isLiteralGood = true;
			this.isLiteralNum = true;
			this.isLiteralNumFloat = true;
			this.literalType = EnumCobolValueType.VALUE_LITERAL_NUM_FLOATING;
			this.valueFloat = StringService._getNumericFloating(token);
			return iEndLiteral;
		}
		
		// La stringa non inizia con una literal alfanumerica o numerica di alcun tipo
		
		return -1;
	}

	/**
	 * Restituisce il tipo di literal cobol.<br>
	 * <p>
	 * 
	 * @return the literalType
	 */
	public EnumCobolValueType getLiteralType() {
		return literalType;
	}

	/**
	 * Restituisce la stringa integrale parsata come literal Cobol.<br>
	 * <p>
	 * 
	 * @return the literalInput
	 */
	public String getLiteralInput() {
		return literalInput;
	}

	/**
	 * Restituisce il valore stringa in caso di literal
	 * Cobol alfanumerica.<br>
	 * <p>
	 * @return the valueString
	 */
	public String getValueString() {
		return valueString;
	}

	/**
	 * Restituisce il valore stringa esadecimale in caso di literal
	 * Cobol alfanumerica.<br>
	 * <p>
	 * 
	 * @return the valueHex
	 */
	public String getValueHex() {
		return valueHex;
	}

	/**
	 * Restituisce il valore intero in caso di literal
	 * Cobol numerica intera.<br>
	 * <p>
	 * 
	 * @return the valueInt
	 */
	public int getValueInt() {
		return valueInt;
	}

	/**
	 * Restituisce il valore intero in caso di literal
	 * Cobol numerica intera da memorizzare in un long integer.<br>
	 * <p>
	 * 
	 * @return the valueLong
	 */
	public long getValueLong() {
		return valueLong;
	}

	/**
	 * Restituisce il valore double in caso di literal
	 * Cobol numerica con virgola.<br>
	 * <p>
	 * 
	 * @return the valueDouble
	 */
	public double getValueDouble() {
		return valueDouble;
	}

	/**
	 * Restituisce il valore float in caso di literal
	 * Cobol numerica floating point.<br>
	 * <p>
	 * 
	 * @return the valueFloat
	 */
	public float getValueFloat() {
		return valueFloat;
	}

	
	/**
	 * Restituisce true se la literal è stata parsata
	 * come valida literal Cobol.<br>
	 * 
	 * @return the isLiteralGood
	 */
	public boolean isLiteralGood() {
		return isLiteralGood;
	}
	
	/**
	 * Restituisce true se la literal alfanumerica inizia
	 * correttamente ma non è terminata correttamente<br>
	 * 
	 * @return the isLiteralStringStart
	 */
	public boolean isLiteralString() {
		return isLiteralString;
	}

	/**
	 * Restituisce true se la literal parsata è
	 * numerica int, long, double o floating point <br>
	 * 
	 * @return the isLiteralNum
	 */
	public boolean isLiteralNum() {
		return isLiteralNum;
	}

	/**
	 * Restituisce true se la literal parsata è  numerica int<br>
	 * <br>
	 * Il numero intero è rappresentabile in un campo int.<br>
	 * <p>
	 * @return the isLiteralNumInt
	 */
	public boolean isLiteralNumInt() {
		return isLiteralNumInt;
	}

	/**
	 * Restituisce true se la literal parsata è  numerica long integer<br>
	 * <br>
	 * Il numero intero è rappresentabile in un campo long.<br>
	 * <p>
	 * @return the isLiteralNumLong
	 */
	public boolean isLiteralNumLong() {
		return isLiteralNumLong;
	}

	/**
	 * Restituisce true se la literal parsata è numerica con virgola decimale double<br>
	 * <br>
	 * Il numero con virgola è rappresentabile in un campo double.<br>
	 * <p>
	 * @return the isLiteralNumDouble
	 */
	public boolean isLiteralNumDouble() {
		return isLiteralNumDouble;
	}

	/**
	 * Restituisce true se la literal parsata è numerica in formato esponenziale
	 * con mantissa e caratteristica<br>
	 * <br>
	 * Il numero esponenziale è rappresentabile in un campo float.<br>
	 * <p>
	 * @return the isLiteralNumFloating
	 */
	public boolean isLiteralNumFloat() {
		return isLiteralNumFloat;
	}

	
	
	/*
	 * Individuazione posizione fine literal.
	 * Se gli apici non sono bilanciati, ovvero non c'è quello
	 * di chiusura, restituisce - 1
	 * 
	 * Restituisce la posizione dell'apice di chiusura della literal.
	 */
	private int posEndLiteral(int iStartLiteral, String stringWithLiteral) {

		int iLastApice = 0;
		int cntApice = 0;
		String chrCur = "";  
		String chrNext = "";					
		char singleApice = 39;					// Apice singolo '
		char doubleApice = 34;					// Doppio apice ""
		String strCurApice = "";
		
		// Caso particolare '''' o """"
		if (stringWithLiteral.trim().equals("''''")
		||  stringWithLiteral.trim().equals(doubleApice+doubleApice+doubleApice+doubleApice)) {
			return 4;
		}
		
		// Individuazione tipo apice di delimitazione literal
		for (int i = iStartLiteral; i < stringWithLiteral.length(); i++) {
			if (stringWithLiteral.charAt(i) == singleApice) {
				strCurApice = "'";
				break;
			}
			if (stringWithLiteral.charAt(i) == doubleApice) {
				strCurApice = Character.toString(doubleApice);
				break;
			}
		}
		
		
		// Scan da inizio stringa (prima dell'apice di inizio)
		for (int i = iStartLiteral; i < stringWithLiteral.length(); i++) {
			
			chrCur = stringWithLiteral.substring(i, i + 1);
			chrNext = "";
			
			if (i <= stringWithLiteral.length() - 2) {
				chrNext = stringWithLiteral.substring(i + 1, i + 2);
			}
			
			// Doppio apice singolo da non considerare 
			// all'interno di stringa delimitata da doppi apici " o apici singoli '
			if (i > iStartLiteral
			&&  chrCur.equals("'") 
			&& chrNext.equals("'")) {
				i = i + 1;
				continue;
			}
			   
			// Apice di apertura o chiusura
			if (chrCur.equals(strCurApice)) {
				iLastApice = i;
				cntApice++;
				
				// Literal terminata
				if (cntApice == 2) {   
					return i;
				}
			}

		} // end-for
		
		return iLastApice;
	}


}
