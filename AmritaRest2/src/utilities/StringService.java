package utilities;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Scanner;
import java.util.StringTokenizer;
/**
	* <h1>
	* StringService  
	* </h1>
	*  <p>
	* Servizi supplementari per oggetti stringa <br>
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 2/03/2010
*/

public class StringService {
  
	private String str = null;
	private ArrayList<String> alWord = null;
	String arWord[] = null;
	int arWordPos[] = null;
    
	// Costanti 
	public static final int PAD_LEFT = 1;
	public static final int PAD_RIGHT = 2;
	
	/**
	 * Costruttore
	 * 
	 * @param str
	 */
	public StringService(String str) {
		super();
		this.str = str;
	}
	
	/**
	 * Restituise le sottotringhe (words) 
	 * 
	 */
	public String[] _words() {
		alWord = new ArrayList<String>();
		StringTokenizer st  = new StringTokenizer(str);
	
		while (st.hasMoreTokens()) {
			String word = st.nextToken();
			alWord.add(word);
		}
		arWord = new String[alWord.size()];
		arWord = alWord.toArray(arWord);
		arWordPos = new int[alWord.size()];
		return arWord;
	}

	/**
	 * Calcola la posizione delle parole estratte da _words
	 * 
	 */
	public void _wordsPos() {
		int posCur = 0;
		int posWord = 0;
		
		for (int i = 0; i < arWord.length; i++) {
			posWord = str.indexOf(arWord[i], posCur);
			arWordPos[i] = posWord;
			posCur = posWord + arWord[i].length();
		}
			arWord = alWord.toArray(arWord);
	}

	
	/**
	 * Restituise la word 1-based 
	 *  
	 * @param str
	 */
	public String _word(int numWord) {
		
		// Richiamo senza aver precdentemete richiesto_Words
		if (arWord == null) {
			this._words();
		}

		// Controlli		
		if (arWord.length == 0) {
			return "";
		}
		if (numWord > arWord.length) {
			return "";
		}
		return arWord[numWord - 1];
	}

	/**
	 * Restituise la posizione della word 1-based 
	 *  
	 * @param str
	 */
	public int _wordPos(int numWord) {
		
		// Richiamo senza aver precdentemete richiesto_Words
		if (arWord == null) {
			this._words();
			this._wordsPos();
		}

		// Controlli		
		if (arWord.length == 0) {
			return -1;
		}
		if (numWord > arWord.length) {
			return -1;
		}
		return arWordPos[numWord - 1];
	}

	
	/**
	 * Restituise il numero di parole presenti
	 * 
	 * @param str
	 */
	public int _wordsCount() {
		// Richiamo senza aver precdentemete richiesto_Words
		if (arWord == null) {
			this._words();
		}
		return arWord.length;
	}

	

	/**
	 * Restituise true se la parola 1-based contiene un numero
	 *  
	 * @param int numWord
	 */
	public boolean _isWordNumeric(int numWord) {
		boolean bNumeric = true;
		
		// Richiamo senza aver precdentemete richiesto_Words
		if (arWord == null) {
			this._words();
		}
		if (arWord.length == 0) {
			return false;
		}
		if (numWord > arWord.length) {
			return false;
		}
		
		// Conversione in numero
		try {
			Integer.parseInt(arWord[numWord - 1]);
		} catch (Exception e) {
			bNumeric = false;
		}
		return bNumeric;
	}
	
	/**
	 * Gets true if all characters are lower-case
	 *  
	 * @param string the text to evaluate
	 */
	public static boolean _isLowerCase(String string) {
		boolean isLowerCase = true;
		for (char c : string.toCharArray()) {
		    if (Character.isUpperCase(c)) {
		    	isLowerCase = false;
		        break;
		    }
		}
		return isLowerCase;
	}

	/**
	 * Gets true if all characters are upper-case
	 *  
	 * @param string the text to evaluate
	 */
	public static boolean _isUpperCase(String string) {
		boolean isUpperCase = true;
		for (char c : string.toCharArray()) {
		    if (Character.isLowerCase(c)) {
		    	isUpperCase = false;
		        break;
		    }
		}
		return isUpperCase;
	}


	/**
	 * Gets true if all characters are alphabetic a|A thru z|Z
	 *  
	 * @param string the text to evaluate
	 */
	public static boolean _isAlphabetic(String string) {
		boolean isAlphabetic = true;
		for (char c : string.toUpperCase().toCharArray()) {
		    if (c < 'A' || c > 'Z') {
		    	isAlphabetic = false;
		        break;
		    }
		}
		return isAlphabetic;
	}

	/**
	 * Gets true if the string contains a valid boolean value true or false
	 *  
	 * @param string the text to evaluate
	 */
	public static boolean _isBoolean(String string) {
		boolean isBoolean = true;
		isBoolean = Boolean.parseBoolean(string);
		return isBoolean;
	}

	/**
	 * Gets true if all characters are numbers from 0 to 9
	 *  
	 * @param string the text to evaluate
	 */
	public static boolean _isNumericString(String string) {
		boolean isNumericString = true;
		for (char c : string.toUpperCase().toCharArray()) {
		    if (c < 'A' || c > 'Z') {
		    	isNumericString = false;
		        break;
		    }
		}
		return isNumericString;
	}



	/**
	 * Restituise true se l'indice fornito, nella stringa fornita,
	 * indica di essere all'interno di una literal.<br>
	 * <p>
	 * Non vengono considerati i doppi apici all'interno della stringa.
	 *  
	 * @param String strInput
	 * @param int index
	 * @param boolean betweenDouble
	 */
	static public boolean _isInsideLiteral(String strInput, int index, boolean betweenDouble) {
		
		String str = "";						//
		char singleApice = 39;					// Apice singolo '
		char doubleApice = 34;					// Doppio apice ""
		char curApice = 0;					    // Apice singolo o doppio da considerare

		boolean bInsideLeft = false;
		int indexNew = 0;
		int i = 0;
		
		str = " " + strInput + " ";				// Di servizio
		indexNew = index + 1;
		
		// Impostazione carattere di delimitazine literal, singolo odoppio apice
		if (betweenDouble) {
			curApice = doubleApice;
		} else {
			curApice = singleApice;
		}
		
		
		// Out of range
		if (index < 0 || index >= str.length() ) {
			return false;
		}
		
		// Scan left
		for (i = indexNew - 1; i > 0; i--) {
			
			// Apice di inizio stringa 
			if (str.charAt(i) == curApice 
			&&  i > 0
			&& !(str.charAt(i  - 1) == curApice)) {
				bInsideLeft = true;
				break;
			}
			
			// Doppi Apici da bypassare
			if (str.charAt(i) == curApice
			&&  str.charAt(i - 1) == curApice) {
				i--;
				continue;
			}
			
			// bypass
		}
		
		// Sicuramente non fra apici
		if (!bInsideLeft) {
			return false;
		}

		// Scan right
		for (i = indexNew + 1; i < str.length(); i++) {
			
			// Apice di fine stringa 
			if (str.charAt(i) == curApice
			&&  i < str.length() - 1
			&& !(str.charAt(i + 1) == curApice)) {
				return true;
			}
			
			// Doppi Apici da bypassare
			if (str.charAt(i) == curApice
			&&  str.charAt(i + 1) == curApice) {
				i++;
				continue;
			}
			
			// bypass
		}
		
		return false;
	}

	
	/**
	 * Restituise true se la stringa è una literal iniziata e terminata
	 * da apice o doppio apice,
	 * <p>
	 * Non vengono considerati i doppi apici all'interno della stringa.
	 * Prima del test la stringa viene trimmata.
	 *  <p>
	 * @param String strInput
	 * @param int index
	 * @param boolean betweenDouble
	 */
	static public boolean _isLiteral(String strInput) {
		
		String str = "";						//
		char singleApice = 39;					// Apice singolo '
		char doubleApice = 34;					// Doppio apice ""

		str = strInput.trim();				// Di servizio

		// Literal fra apici singoli
		if (str.charAt(0) == singleApice && str.charAt(str.length() - 1) == singleApice) {
			return true;
		}
		
		// Literal fra apici doppi
		if (str.charAt(0) == doubleApice && str.charAt(str.length() - 1) == doubleApice) {
			return true;
		}
		
		return false;
	}

	/**
	 * Restituise true se la stringa è una literal satartata da apice o doppio apice,
	 * <p>
	 * Prima del test la stringa viene trimmata.
	 *  <p>
	 * @param String strInput
	 * @param int index
	 * @param boolean betweenDouble
	 */
	static public boolean _isLiteralStarting(String strInput) {
		
		String str = "";						//
		char singleApice = 39;					// Apice singolo '
		char doubleApice = 34;					// Doppio apice ""

		str = strInput.trim();				// Di servizio

		// Literal fra apici singoli
		if (str.charAt(0) == singleApice || str.charAt(0) == doubleApice) {
			return true;
		}
		
		return false;
	}

	/**
	 * Restituise il numero di caratteri del tipo in input, presenti nella stringa.
	 *  <p>
	 * @param String strInput
	 * @param char chrToCount
	 * @param int count of char
	 */
	static public int _countCrt(String strInput, char chrToCount ) {
		
		int cntChr = 0;
		
		// Scan caratteri stringa
		for (int i = 0; i < strInput.length(); i++) {
			if (strInput.charAt(i) == chrToCount) {
				cntChr++;
			}
		}
		
		return cntChr;
	}


	/**
	 * Restituise true se la stringa contiene un numero.<br>
	 * <p>
	 * Il numero può essere con o senza segno, con la virgola
	 * o il punto decimale oppure in notazione esponenziale.<br>
	 * <p>
	 * Esempi di formati gestiti sono:<br>
	 * <p>
	 * 459999999<br>
	 * +105<br>
	 * -5105<br>
	 * 3,8<br>
	 * 4.2<br>
	 * +3,8<br>
	 * -0.2<br>
	 * 0.5E3<br>
	 * +0.5E3<br>
	 * -0.5E3<br>
	 * +2E-4<br>
	 * <p>
	 * @param String str
	 * @return boolean true se numeric
	 */
	static public boolean _isNumeric(String str) {
		boolean bNumeric = false;

		// Numero intero
		bNumeric = _isNumericInt(str);
        if (bNumeric) {
			return true;   
		}
		
		// Numero intero con + cifre intere
		bNumeric = _isNumericLong(str);
        if (bNumeric) {
			return true;
		}
		
		// Numero con la virgola
		bNumeric = _isNumericDouble(str);
        if (bNumeric) {
			return true;
		}
		
		// Nnumero floating point
		bNumeric = _isNumericFloating(str);
		
		return bNumeric;
	}

	/**
	 * Restituise true se la stringa contiene un numero intero senza virgola
	 * che può stare in un campo int.<br>
	 * <p>
	 * Il numero può essere con o senza segno.<br>
	 * <p>
	 * Esempi di formati gestiti sono:<br>
	 * <p>
	 * 459999999<br>
	 * +105<br>
	 * -5105<br>
	 * <p>
	 * @param String str
	 * @return boolean true se numeric
	 */
	static public boolean _isNumericInt(String str) {
		boolean bNumeric = true;
		String strNum = "";
 		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Non è un Numero intero
		if (strNum.indexOf(',') > 0 || strNum.indexOf('.')> 0) {
		   return false;
		}
		
		// Conversione in numero
		try {
			Integer.parseInt(strNum);
		} catch (Exception e) {
			bNumeric = false;
		}
 
		return bNumeric;
	}

	/**
	 * Restituise true se la stringa contiene un numero intero senza virgola.<br>
	 * <p>
	 * Il numero può essere con o senza segno.<br>
	 * <p>
	 * Esempi di formati gestiti sono:<br>
	 * <p>
	 * 459999999<br>
	 * +105<br>
	 * -5105<br>
	 * <p>
	 * @param String str
	 * @return boolean true se numeric
	 */
	static public boolean _isNumericLong(String str) {
		boolean bNumeric = true;
		String strNum = "";
 		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Numero intero
		if (strNum.indexOf(',') >= 0 || strNum.indexOf('.') >= 0) {
			return false;
		}
		// Parse long
		bNumeric = true;
		try {
			Long.parseLong(strNum);
		} catch (Exception e) {
			bNumeric = false;
			// Se campo composto da tutte cifre si considera valido
			for (int i = 0; i < strNum.length(); i++) {
				if (strNum.charAt(i) < 0 || strNum.charAt(i) > '9') {
					return bNumeric;
				}
			}
			bNumeric = true;
		}
		
		return bNumeric;
	}

	/**
	 * Restituise true se la stringa contiene un numero con la virgola.<br>
	 * <p>
	 * Il numero può essere con o senza segno.<br>
	 * <p>
	 * Esempi di formati gestiti sono:<br>
	 * <p>
	 * 3,8<br>
	 * 4.2<br>
	 * +3,8<br>
	 * -0.2<br>
	 * <p>
	 * @param String str
	 * @return boolean true se numeric
	 */
	static public boolean _isNumericDouble(String str) {
		boolean bNumeric = true;
		int iComma = 0;
		String strNum = "";
 		
		// Non è un numero con la virgola
		if (str.indexOf(",") == - 1 && str.indexOf(".") == - 1) {
			return false;
		}
		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Numero con la virgola, forzo notazione americana
		strNum = strNum.replace(',', '.');
		iComma = strNum.indexOf('.');

		// Dopo la virgola ci voglion delle cifre
		if (iComma >= 0 && strNum.substring(iComma).trim().length() == 1) {
			return false;
		}
		
		// Conversione in numero
		try {
			Double.parseDouble(strNum);
		} catch (Exception e) {
			bNumeric = false;
		}
		
		// Numero intero o con la virgola
		if (bNumeric) {
			return true;
		}
		return bNumeric;
	}

	/**
	 * Restituise true se la stringa contiene un numero floating point .<br>
	 * <p>
	 * No
	 * Il numero può essere con o senza segno, con la virgola
	 * o il punto decimale e deve essere in notazione esponenziale.<br>
	 * <p>
	 * Esempi di formati gestiti sono:<br>
	 * <p>
	 * 0.5E3<br>
	 * +0.5E3<br>
	 * -0.5E3<br>
	 * +2E-4<br>
	 * <p>
	 * @param String str
	 * @return boolean true se numeric
	 */
	static public boolean _isNumericFloating(String str) {
		boolean bNumeric = true;
		String strNum = "";
 		
		// Non è un numero floating point
		if (str.indexOf("E") == - 1) {
			return false;
		}
		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Numero con la virgola nella mantissa, forzo notazione americana
		strNum = strNum.replace(',', '.');

		
		// Conversione in numero
		try {
			Float.parseFloat(strNum);
		} catch (Exception e) {
			bNumeric = false;
		}
		return bNumeric;
	}
	
	
	/**
	 * Restituise true se la stringa contiene un numero con virgola.
	 *  
	 * @param String str
	 */
	static public boolean _isNumericWithComma(String str) {
		
		// Numerico senza virgola o punto decimale
		if (str.indexOf(",") == -1 && str.indexOf(".") == -1) {
			return false;
		}

		// Non è numerico
		if (!_isNumericDouble(str)) {
			return false;
		}
		
		// Numerico con virgola o punto decimale a inizio stringa (,888)
		if (str.indexOf(",") == 0 || str.indexOf(".") == 0) {
			return false;
		}
		return true;
	}

	
	/**
	 * Restituise il valore numerico intero di una stringa
	 *  
	 * @param String str
	 */
	static public int _parseInt(String str) {
		int numParsed = 0;
		String strNum = "";
		
		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Conversione in numero
		try {
			numParsed = Integer.parseInt(strNum);
		} catch (Exception e) {
			numParsed = -1;
		}
		return numParsed;
	}

	/**
	 * Restituise il valore numerico double di una stringa
	 *  
	 * @param String str
	 */
	static public double _parseDouble(String str) {
		double numParsed = 0;
		String strNum = "";
		
		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Conversione in numero
		try {
			numParsed = Double.parseDouble(strNum);
		} catch (Exception e) {
			numParsed = -10000000;
		}
		return numParsed;
	}

	
	/**
	 * Restituise un integer con il valore numerico se la stringa contiene un numero
	 *  
	 * @param String str
	 */
	static public Integer _getNumericInt(String str) {
		int intNum = 0;
		String strWrk = "";
		
		strWrk = str;
		
		// Se inizia con + lo elimino
		if (str.startsWith("+")) {
			strWrk = str.substring(1);
		}
				
		// Conversione in numero
		try {
			 intNum = Integer.parseInt(strWrk);
		} catch (Exception e) {
			return null;
		}
		return new Integer(intNum);
	}
	
	/**
	 * Restituise un Long integer con il valore numerico se la stringa contiene un numero
	 *  
	 * @param String str
	 */
	static public Long _getNumericLong(String str) {
		long longNum = 0;
		String strWrk = "";
		
		strWrk = str;
		
		// Se inizia con + lo elimino
		if (str.startsWith("+")) {
			strWrk = str.substring(1);
		}
				
		// Conversione in numero
		try {
			longNum = Long.parseLong(strWrk);
		} catch (Exception e) {
			return null;
		}
		return new Long(longNum);
	}


	/**
	 * Restituise un Float con il valore numerico se la stringa contiene un numero
	 *  
	 * @param String str
	 */
	static public Float _getNumericFloating(String str) {
		String strNum = "";
		float floatNum = 0;
		
		// Non è un numero floating point
		if (str.indexOf("E") == - 1) {
			return new Float(0);
		}
		
		if (str.startsWith("+")) {
			strNum = str.substring(1);
		} else {
			strNum = str;
		}
		
		// Numero con la virgola nella mantissa, forzo notazione americana
		strNum = strNum.replace(',', '.');

		
		// Conversione in numero
		try {
			floatNum = Float.parseFloat(strNum);
		} catch (Exception e) {
		}
		return new Float(floatNum);
	}


	
	/**
	 * Restituise un double con il valore numerico se la stringa contiene un numero con la virgola<br>
	 * Se non numerico restituisce null<br>
	 * Se numerico intero lo converte in double.<br>
	 * Se Numero con la virgola restituisce il valore in double.<br>
	 *  
	 * @param String str
	 */
	static public Double _getNumericDouble(String str) {
		double doubleNum = 0;
		
		// Numerico double
		if (_isNumericWithComma(str)) {
			doubleNum = Double.parseDouble(str.replace(',', '.'));   // Se non è notazione americana lo trasformo
			return new Double(doubleNum);
		}
		
		// Integer
		if (_isNumeric(str)) {
			doubleNum = _parseInt(str);
			return new Double(doubleNum);
		}
		return null;
	}


	/**
	 * Effettua il padding a sinistra o destra per la lunghezza specificata
	 * 
	 * @param char chrPad with the char to pad
	 * @param int lengthTotal of string result
	 */
	public String _pad(char chrPad, int lengthTotal, int PAD_LEFT_RIGHT) {
		StringBuffer sb = null;
		String strOut = "";
		int numCrtToPad = 0;
		
		sb = new StringBuffer("");
		numCrtToPad = lengthTotal - this.str.length();
		
		// Genero stringa da accodare a inizio o fine 
        for (int i = 0; i < numCrtToPad; i++) {
			sb.append(chrPad);
		}		
		
        // Compongo la stringa di pad
        if (PAD_LEFT_RIGHT == PAD_RIGHT) {
        	strOut = this.str + sb.toString();
		} 
        if (PAD_LEFT_RIGHT == PAD_LEFT) {
        	strOut = sb.toString() + this.str;
		} 
		return strOut;
	}

	/**
	 * Effettua il padding a sinistra o destra per la lunghezza specificata
	 * 
	 * @param String strToPad
	 * @param char chrPad with the char to pad
	 * @param int lengthTotal of string result
	 */
	static public String _pad(String strToPad, char chrPad, int lengthTotal, int PAD_LEFT_RIGHT) {
		StringBuffer sb = null;
		String strOut = "";
		int numCrtToPad = 0;
		
		sb = new StringBuffer("");
		numCrtToPad = lengthTotal - strToPad.length();
		
		// Genero stringa da accodare a inizio o fine 
        for (int i = 0; i < numCrtToPad; i++) {
			sb.append(chrPad);
		}		
		
        // Compongo la stringa di pad
        if (PAD_LEFT_RIGHT == PAD_RIGHT) {
        	strOut = strToPad+ sb.toString();
		} 
        if (PAD_LEFT_RIGHT == PAD_LEFT) {
        	strOut = sb.toString() + strToPad;
		} 
        
        // Dimensioni totali > lunghezza: troncamento
        if (strOut.length() > lengthTotal) {
        	strOut = strOut.substring(0, lengthTotal);
		}
		return strOut;
	}

	
	/**
	 * Restituisce il pointer del primo carattere diverso da space nella stringa specificata, a partire
	 * dalla posizione 0-based fornita.<br>
	 * <p>
	 * Se inesistente restituisce -1<br
	 * <p>
	 * 
	 * @param String string to analyze
	 * @param int pos start 0-based
	 * @return int pos first crt no space
	 */
	static public int _firstNoSpace(String str, int iStart) {
		int iFirstNoSpace = 0;
		
		// Scan stringa a partire dalla posizione fornita
		for (iFirstNoSpace = iStart; iFirstNoSpace < str.length(); iFirstNoSpace++) {
			if (!str.substring(iFirstNoSpace, iFirstNoSpace + 1).equals(" ")) {
				break;
			}
		}
		
		// Diverso da Space non trovato
		if (iFirstNoSpace >= str.length()) {
			iFirstNoSpace = -1;
		}
		return iFirstNoSpace;
	}
	/**
	 * Restituisce il pointer al primo carattere space nella stringa specificata, a partire
	 * dalla posizione 0-based fornita.<br>
	 * <p>
	 * Se inesistente restituisce -1<br
	 * <p>
	 * Metodo ridondante in quanto già presente in String, qui solo per chiarezza.
	 * 
	 * @param String string to analyze
	 * @param int pos start 0-based
	 * @return int pos first crt no space
	 */
	static public int _firstSpace(String str, int iStart) {
		int iFirstSpace = 0;
		
		iFirstSpace = str.indexOf(" ", iStart);
		
		return iFirstSpace;
	}

	/**
	 * Sostituisce tutte le occorrenze di input in quelle di output
	 * Restituise la stringas con le sostituzioni
	 * 
	 * @param String strToFind
	 * @param String strToReplace
	 * @return String con la stringa completa rimpiazzata
	 */
	static public String _replace(String strInput, String strToFind, String strToReplace) {
        int i = 0;
        int iPrec = 0;
        
        StringBuffer sb = null;
        
        sb = new StringBuffer("");
        i = strInput.indexOf(strToFind);
        iPrec = 0;
        
        // Loop sostituzioni
        while (i >= 0) {
        	if (i == 0) {i++;};
        	
        	sb.append(strInput.substring(iPrec, i) + strToReplace);
         	iPrec = i + strToFind.length();
         	
         	// Next str to find
        	i = strInput.indexOf(strToFind, iPrec);
 		}
        
		// Ultimo step
        sb.append(strInput.substring(iPrec));
		
		return sb.toString();
	}

	/**
	 * Restituisce una stringa trimmata e senza la prima parola, se c'è match con una di quelle fornite
	 * in input.
	 * 
	 * @param String strInput
	 * @param String[] strToMatch
	 * @return String con la stringa completa rimpiazzata
	 */
	static public String _delFirstWord(String strInput, String ... ar_strToMatch) {
        Scanner scn;
        String token = "";
        String tokenToMatch = "";

        scn = new Scanner(strInput);
    	token = scn.next();
    	
    	for (int j = 0; j < ar_strToMatch.length; j++) {
    		tokenToMatch = ar_strToMatch[j];
			if (token.equals(tokenToMatch)) {
				return strInput.substring(tokenToMatch.length()).trim();
			}
		}
 	return strInput;
	}


	/**
	 * Restituisce una stringa trimmata a destra
	 * 
	 * @param String[] strToMatch
	 * @return String con la stringa completa rimpiazzata
	 */
	static public String _rtrim(String strInput) {
        String strTrimmed = "";
        int j = 0;
        
    	for (j = strInput.length() - 1; j >= 0; j--) {
 			if (strInput.charAt(j) != ' ') {
				break;
			}
		}
    	
    	if (j == strInput.length() - 1) {
			return strInput;
		}
    	
    	strTrimmed = strInput.substring(0, j + 1);
    	
    	return strTrimmed;
	}

	/**
	 * Restituisce una stringa trimmata a sinistra
	 * 
	 * @param String[] strToMatch
	 * @return String con la stringa completa rimpiazzata
	 */
	static public String _ltrim(String strInput) {
        String strTrimmed = "";
        int j = 0;
        
    	for (j = 0; j < strInput.length(); j++) {
 			if (strInput.charAt(j) != ' ') {
				break;
			}
		}
    	
    	if (j >= strInput.length()) {
			return strInput;
		}
    	
    	strTrimmed = strInput.substring(j);
    	
    	return strTrimmed;
	}

	/**
	 * @param index
	 * @return
	 * @see java.lang.String#codePointAt(int)
	 */
	public int codePointAt(int index) {
		return str.codePointAt(index);
	}

	/**
	 * @param index
	 * @return
	 * @see java.lang.String#codePointBefore(int)
	 */
	public int codePointBefore(int index) {
		return str.codePointBefore(index);
	}

	/**
	 * @param beginIndex
	 * @param endIndex
	 * @return
	 * @see java.lang.String#codePointCount(int, int)
	 */
	public int codePointCount(int beginIndex, int endIndex) {
		return str.codePointCount(beginIndex, endIndex);
	}

	/**
	 * @param anotherString
	 * @return
	 * @see java.lang.String#compareTo(java.lang.String)
	 */
	public int compareTo(String anotherString) {
		return str.compareTo(anotherString);
	}

	/**
	 * @param str
	 * @return
	 * @see java.lang.String#compareToIgnoreCase(java.lang.String)
	 */
	public int compareToIgnoreCase(String str) {
		return str.compareToIgnoreCase(str);
	}

	/**
	 * @param str
	 * @return
	 * @see java.lang.String#concat(java.lang.String)
	 */
	public String concat(String str) {
		return str.concat(str);
	}

	/**
	 * @param s
	 * @return
	 * @see java.lang.String#contains(java.lang.CharSequence)
	 */
	public boolean contains(CharSequence s) {
		return str.contains(s);
	}

	/**
	 * @param cs
	 * @return
	 * @see java.lang.String#contentEquals(java.lang.CharSequence)
	 */
	public boolean contentEquals(CharSequence cs) {
		return str.contentEquals(cs);
	}

	/**
	 * @param sb
	 * @return
	 * @see java.lang.String#contentEquals(java.lang.StringBuffer)
	 */
	public boolean contentEquals(StringBuffer sb) {
		return str.contentEquals(sb);
	}

	/**
	 * @param suffix
	 * @return
	 * @see java.lang.String#endsWith(java.lang.String)
	 */
	public boolean endsWith(String suffix) {
		return str.endsWith(suffix);
	}

	/**
	 * @param anObject
	 * @return
	 * @see java.lang.String#equals(java.lang.Object)
	 */
	public boolean equals(Object anObject) {
		return str.equals(anObject);
	}

	/**
	 * @param anotherString
	 * @return
	 * @see java.lang.String#equalsIgnoreCase(java.lang.String)
	 */
	public boolean equalsIgnoreCase(String anotherString) {
		return str.equalsIgnoreCase(anotherString);
	}

	/**
	 * @return
	 * @see java.lang.String#getBytes()
	 */
	public byte[] getBytes() {
		return str.getBytes();
	}

	/**
	 * @param charset
	 * @return
	 * @see java.lang.String#getBytes(java.nio.charset.Charset)
	 */
	public byte[] getBytes(Charset charset) {
		return str.getBytes(charset);
	}

	/**
	 * @param srcBegin
	 * @param srcEnd
	 * @param dst
	 * @param dstBegin
	 * @deprecated
	 * @see java.lang.String#getBytes(int, int, byte[], int)
	 */
	public void getBytes(int srcBegin, int srcEnd, byte[] dst, int dstBegin) {
		str.getBytes(srcBegin, srcEnd, dst, dstBegin);
	}

	/**
	 * @param charsetName
	 * @return
	 * @throws UnsupportedEncodingException
	 * @see java.lang.String#getBytes(java.lang.String)
	 */
	public byte[] getBytes(String charsetName)
			throws UnsupportedEncodingException {
		return str.getBytes(charsetName);
	}

	/**
	 * @param srcBegin
	 * @param srcEnd
	 * @param dst
	 * @param dstBegin
	 * @see java.lang.String#getChars(int, int, char[], int)
	 */
	public void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin) {
		str.getChars(srcBegin, srcEnd, dst, dstBegin);
	}

	/**
	 * @return
	 * @see java.lang.String#hashCode()
	 */
	public int hashCode() {
		return str.hashCode();
	}

	/**
	 * @param ch
	 * @param fromIndex
	 * @return
	 * @see java.lang.String#indexOf(int, int)
	 */
	public int indexOf(int ch, int fromIndex) {
		return str.indexOf(ch, fromIndex);
	}

	/**
	 * @param ch
	 * @return
	 * @see java.lang.String#indexOf(int)
	 */
	public int indexOf(int ch) {
		return str.indexOf(ch);
	}

	/**
	 * @param str
	 * @param fromIndex
	 * @return
	 * @see java.lang.String#indexOf(java.lang.String, int)
	 */
	public int indexOf(String str, int fromIndex) {
		return str.indexOf(str, fromIndex);
	}

	/**
	 * @param str
	 * @return
	 * @see java.lang.String#indexOf(java.lang.String)
	 */
	public int indexOf(String str) {
		return str.indexOf(str);
	}

	/**
	 * @return
	 * @see java.lang.String#intern()
	 */
	public String intern() {
		return str.intern();
	}

	/**
	 * @return
	 * @see java.lang.String#isEmpty()
	 */
	public boolean isEmpty() {
		return str.isEmpty();
	}

	/**
	 * @param ch
	 * @param fromIndex
	 * @return
	 * @see java.lang.String#lastIndexOf(int, int)
	 */
	public int lastIndexOf(int ch, int fromIndex) {
		return str.lastIndexOf(ch, fromIndex);
	}

	/**
	 * @param ch
	 * @return
	 * @see java.lang.String#lastIndexOf(int)
	 */
	public int lastIndexOf(int ch) {
		return str.lastIndexOf(ch);
	}

	/**
	 * @param str
	 * @param fromIndex
	 * @return
	 * @see java.lang.String#lastIndexOf(java.lang.String, int)
	 */
	public int lastIndexOf(String str, int fromIndex) {
		return str.lastIndexOf(str, fromIndex);
	}

	/**
	 * @param str
	 * @return
	 * @see java.lang.String#lastIndexOf(java.lang.String)
	 */
	public int lastIndexOf(String str) {
		return str.lastIndexOf(str);
	}

	/**
	 * @return
	 * @see java.lang.String#length()
	 */
	public int length() {
		return str.length();
	}

	/**
	 * @param regex
	 * @return
	 * @see java.lang.String#matches(java.lang.String)
	 */
	public boolean matches(String regex) {
		return str.matches(regex);
	}

	/**
	 * @param index
	 * @param codePointOffset
	 * @return
	 * @see java.lang.String#offsetByCodePoints(int, int)
	 */
	public int offsetByCodePoints(int index, int codePointOffset) {
		return str.offsetByCodePoints(index, codePointOffset);
	}

	/**
	 * @param ignoreCase
	 * @param toffset
	 * @param other
	 * @param ooffset
	 * @param len
	 * @return
	 * @see java.lang.String#regionMatches(boolean, int, java.lang.String, int, int)
	 */
	public boolean regionMatches(boolean ignoreCase, int toffset, String other,
			int ooffset, int len) {
		return str.regionMatches(ignoreCase, toffset, other, ooffset, len);
	}

	/**
	 * @param toffset
	 * @param other
	 * @param ooffset
	 * @param len
	 * @return
	 * @see java.lang.String#regionMatches(int, java.lang.String, int, int)
	 */
	public boolean regionMatches(int toffset, String other, int ooffset, int len) {
		return str.regionMatches(toffset, other, ooffset, len);
	}

	/**
	 * @param oldChar
	 * @param newChar
	 * @return
	 * @see java.lang.String#replace(char, char)
	 */
	public String replace(char oldChar, char newChar) {
		return str.replace(oldChar, newChar);
	}

	/**
	 * @param target
	 * @param replacement
	 * @return
	 * @see java.lang.String#replace(java.lang.CharSequence, java.lang.CharSequence)
	 */
	public String replace(CharSequence target, CharSequence replacement) {
		return str.replace(target, replacement);
	}

	/**
	 * @param regex
	 * @param replacement
	 * @return
	 * @see java.lang.String#replaceAll(java.lang.String, java.lang.String)
	 */
	public String replaceAll(String regex, String replacement) {
		return str.replaceAll(regex, replacement);
	}

	/**
	 * @param regex
	 * @param replacement
	 * @return
	 * @see java.lang.String#replaceFirst(java.lang.String, java.lang.String)
	 */
	public String replaceFirst(String regex, String replacement) {
		return str.replaceFirst(regex, replacement);
	}

	/**
	 * @param regex
	 * @param limit
	 * @return
	 * @see java.lang.String#split(java.lang.String, int)
	 */
	public String[] split(String regex, int limit) {
		return str.split(regex, limit);
	}

	/**
	 * @param regex
	 * @return
	 * @see java.lang.String#split(java.lang.String)
	 */
	public String[] split(String regex) {
		return str.split(regex);
	}

	/**
	 * @param prefix
	 * @param toffset
	 * @return
	 * @see java.lang.String#startsWith(java.lang.String, int)
	 */
	public boolean startsWith(String prefix, int toffset) {
		return str.startsWith(prefix, toffset);
	}

	/**
	 * @param prefix
	 * @return
	 * @see java.lang.String#startsWith(java.lang.String)
	 */
	public boolean startsWith(String prefix) {
		return str.startsWith(prefix);
	}

	/**
	 * @param beginIndex
	 * @param endIndex
	 * @return
	 * @see java.lang.String#subSequence(int, int)
	 */
	public CharSequence subSequence(int beginIndex, int endIndex) {
		return str.subSequence(beginIndex, endIndex);
	}

	/**
	 * @param beginIndex
	 * @param endIndex
	 * @return
	 * @see java.lang.String#substring(int, int)
	 */
	public String substring(int beginIndex, int endIndex) {
		return str.substring(beginIndex, endIndex);
	}

	/**
	 * @param beginIndex
	 * @return
	 * @see java.lang.String#substring(int)
	 */
	public String substring(int beginIndex) {
		return str.substring(beginIndex);
	}

	/**
	 * @return
	 * @see java.lang.String#toCharArray()
	 */
	public char[] toCharArray() {
		return str.toCharArray();
	}

	/**
	 * @return
	 * @see java.lang.String#toLowerCase()
	 */
	public String toLowerCase() {
		return str.toLowerCase();
	}

	/**
	 * @param locale
	 * @return
	 * @see java.lang.String#toLowerCase(java.util.Locale)
	 */
	public String toLowerCase(Locale locale) {
		return str.toLowerCase(locale);
	}

	/**
	 * @return
	 * @see java.lang.String#toString()
	 */
	public String toString() {
		return str.toString();
	}

	/**
	 * @return
	 * @see java.lang.String#toUpperCase()
	 */
	public String toUpperCase() {
		return str.toUpperCase();
	}

	/**
	 * @param locale
	 * @return
	 * @see java.lang.String#toUpperCase(java.util.Locale)
	 */
	public String toUpperCase(Locale locale) {
		return str.toUpperCase(locale);
	}

	/**
	 * @return
	 * @see java.lang.String#trim()
	 */
	public String trim() {
		return str.trim();
	}

	
}
