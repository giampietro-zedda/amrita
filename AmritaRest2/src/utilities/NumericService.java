package utilities;


/**
	* <h1>
	* NumericService  
	* </h1>
	*  <p>
	* Servizi generali riguardanti il trattamento si numeri.  <br>
	* <p>
	* Sono disponibili metodi per conoscere il numero di interi, decimali,
	* se un numero è pari o dispari etc. <br>
	* In generale vengono offerti servizi non presenti nelle classi standard java o
	* che semplificano applicativamente il loro utilizzo.
	* 
	* @author Giampietro Zedda
	* @version 1.0.0
	* @since 5/02/2011
*/

 public  class NumericService   {
  

	/**
	 * Costruttore vuoto
	 */
	public  NumericService() {
	}
	
	/**
	 * Restituisce true se il numero fornito è pari.<br>
	 * <p>
	 * @param int number
	 * @return boolean isEven
	 */
	public static boolean isEven(int number) {
		boolean isEven = false;
		if (number%2 == 0) {
			isEven = true;
		}
		return isEven;
	}

	/**
	 * Restituisce true se il numero fornito è dispari.<br>
	 * <p>
	 * @param int number
	 * @return boolean isOdd
	 */
	public static boolean isOdd(int number) {
		boolean isOdd = true;
		if (number%2 == 0) {
			isOdd = false;
		}
		return isOdd;
	}

	/**
	 * Gets the value of the integer digits of a Double number.<br>
	 * <p>
	 * @param dblNum the 
	 * @return the number of integer digit value
	 */
	public static int getDoubleIntValue(double dblNum) {
		return (int)dblNum;
	}

	/**
	 * Gets the value string of the integer digits of a Double number.<br>
	 * <p>
	 * @param dblNum the 
	 * @return the value string of integer digit value
	 */
	public static String getDoubleIntString(double dblNum) {
		return new Integer((int)dblNum).toString();
	}

	/**
	 * Gets the value of the integer digits of a Double number.<br>
	 * <p>
	 * @param dblNum the 
	 * @return the number of integer digit value
	 */
	public static int getDoubleDecValue(double dblNum) {
		String valueDoubleX = "";
		String digitsDecX = "";
		int digitsValueDec = 0;
		int i = 0;
		int e = 0;
		valueDoubleX = ""+dblNum; 
		i = valueDoubleX.indexOf(".");
		if (i < 0 ) {
			i = valueDoubleX.indexOf(",");
		}
		e = valueDoubleX.indexOf("E", i++);
		if (e < 0) {
			digitsDecX = valueDoubleX.substring(i + 1);
		} else {
			digitsDecX = valueDoubleX.substring(i + 1, e);
		}
		digitsValueDec  = Integer.parseInt(digitsDecX);

		return digitsValueDec;
	}


	/**
	 * Gets the string value of the integer digits of a Double number.<br>
	 * <p>
	 * @param dblNum the 
	 * @return the string integer digit value
	 */
	public static String getDoubleDecString(double dblNum) {
		String valueDoubleX = "";
		String digitsDecX = "";
		int i = 0;
		int e = 0;
		valueDoubleX = ""+dblNum; 
		i = valueDoubleX.indexOf(".");
		if (i < 0 ) {
			i = valueDoubleX.indexOf(",");
		}
		e = valueDoubleX.indexOf("E", i++);
		if (e < 0) {
			digitsDecX = valueDoubleX.substring(i + 1);
		} else {
			digitsDecX = valueDoubleX.substring(i + 1, e);
		}
		return digitsDecX;
	}

}
