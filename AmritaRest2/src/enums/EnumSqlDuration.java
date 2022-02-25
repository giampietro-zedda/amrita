package enums;

/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumSqlDuration	
  * </h1>
  *  <p>
  * Questa enum elenca le tipologie di durata esprimibili in una expression Sql.<br>
  * Nell'espressione un elemento di durata è preceduto da un elemento del tipo:<br>
  * <ul>
  * <li> function-invocation</li>
  * <li> (expression)</li>
  * <li> constant</li>
  * <li> column-name</li>
  * <li> variabile</li>
  * </ul>
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 19/07/2010
  * @see EnumSqlExpressionElementType
  *  
*/
public enum EnumSqlDuration {
	
	NOT_ASSIGNED, 						   		// (00) Di servizio             

	YEAR(),                                   	// (01) 
	YEARS(),                                  	// (02) 
	MONTH(),                                  	// (03) 
	MONTHS(),                                  	// (04) 
	DAY(),                                  	// (05) 
	DAYS(),                                  	// (06) 
	HOUR(),                                  	// (07) 
	HOURS(),                                  	// (08) 
	MINUTE(),                                  	// (09) 
	MINUTES(),                                  // (10) 
	SECOND(),                                  	// (11) 
	SECONDS(),                                  // (12) 
	MICROSECOND(),                              // (13) 
	MICROSECONDS();                             // (14) 

	/*
	 * Costruttore vuoto
	 */
	private EnumSqlDuration() {
	}
	
	
}