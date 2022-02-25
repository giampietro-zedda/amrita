
package analyzer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import enums.EnumPrecompilerReservedWords;
import enums.EnumSqlDuration;
import enums.EnumSqlExpressionElementType;

/**
 * Copyright (c) 2009-2011 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * SqlExpressionElement
 * </h1>
 * <p>
 * Descrive uno specifico elemento di una espressione Sql<br> 
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 16/lug/2011 
 * @see SqlExpression
*/

public class SqlExpressionElement implements Serializable{

	private static final long serialVersionUID = 1L;
	
	//////////////////////////////////////////////////////////////////////
	// Variabili di istanza
	//////////////////////////////////////////////////////////////////////
	
	private EnumSqlExpressionElementType typeElement = null;				    // Tipologia elemento
	private EnumPrecompilerReservedWords typeLabeledDuration = null;            // Se type element function-invocation, (expression), constant, column-name, variabile
	private boolean isLabeledDuration = false;                                  // True se typeLabeledDuration valorizzato
	private SqlExpression expression = null;                          			// Se type element expression, time zone, case, row change expression
	private SqlFullSelect scalarFullSelect = null;                          	// Se type scalar full select
	private String columnName = "";                                             // Se type column name
	private String columnQualifier = "";                                        // Se type column name qualifier.columnName  qualifier=table|view|Alias|Synonym|corr-name
	private String constant = "";                                               // Se type constant
	private String hostVar = "";                                                // Se type host var
	private String variabileName = "";                                          // Se type variabile
	private String functionName = "";                                           // Se type function invocation
	private EnumPrecompilerReservedWords functionNameCoded = null;              // Se type function invocation
	private List<String> al_functionParm = null;                                // Se type function invocation
	private EnumPrecompilerReservedWords specialRegister = null;				// Se type special register
	private EnumSqlDuration duration = null;									// Se type labeled duration
	private String caseExpression = "";                                         // Se type case expression CASE case-expression END
	private String castSpecification = "";                                      // Se type case specification CAST( cast-specification )
	private boolean isRowChangeTimestamp = false;                               // Se type row change expression ROW CHANGE TIMESTAMP 
	private boolean isRowChangeToken = false;                                   // Se type row change expression ROW CHANGE TOKEN
	private String rowChangeTableDesignator = "";                               // Se type row change expression ROW CHANGE ... FOR table-designator
	private boolean isSequenceReferenceNext = false;                            // Se type sequence reference NEXT VALUE FOR sequence-name
	private boolean isSequenceReferencePrevious = false;                        // Se type sequence reference PREVIOUS VALUE FOR sequence-name
	private String sequenceReferenceName = "";                                  // Se type sequence reference ...  VALUE FOR sequence-name
	private String XMLCastSpecification = "";                                   // Se type sequence XML Cast Specification CAST( specification )
	private String OLAPType = "";                                               // Se type OLAP Specification RANK|DENSE_RANK|ROW_NUMBER|FUNCTION
	private String OLAPAggregateFunctionName = "";                              // Se type OLAP Specification fnc-aggregate ( body )
	private String OLAPAggregateFunctionBody = "";                              // Se type OLAP Specification fnc-aggregate ( body )
	private String OLAPSpecification = "";                                      // Se type OLAP Specification OVER ( specification )
	private String valueNotAssigned = "";                                       // Se type NOT_ASSIGNED

	
	/**
	 * Costruttore vuoto
	 */
	public SqlExpressionElement() {
		super();
		typeElement = EnumSqlExpressionElementType.NOT_ASSIGNED;
		specialRegister = EnumPrecompilerReservedWords.NOT_ASSIGNED;
		al_functionParm = new ArrayList<String> ();
	}


	/**
	 * Restituisce il tipo di elemento<br>
	 * <p>
	 * @return the typeElement
	 */
	public EnumSqlExpressionElementType getTypeElement() {
		return typeElement;
	}


	/**
	 * Imposta il tipo di elemento<br>
	 * <p>
	 * @param typeElement the typeElement to set
	 */
	public void setTypeElement(EnumSqlExpressionElementType typeElement) {
		this.typeElement = typeElement;
	}


	/**
	 * Restituisce il tipo di labeled duration<br>
	 * <p>
	 * Si tratta di un elemento composto da:<br>
	 * <ul>
	 * 	<li>function-invocation</li>
	 * 	<li>(expression)</li>
	 * 	<li>constant</li>	 
	 * 	<li>column-name</li>	 
	 * 	<li>variable</li>	 
	 * </ul>
	 * Seguito da una indicazione di durata quale:
	 * <p>
	 * <ul>
	 * 	<li>YEAR</li>
	 * 	<li>YEARS 	</li>
	 * 	<li>MONTH</li>	 
	 * 	<li>MONTHS</li>	 
	 * 	<li>DAY</li>	 
	 * 	<li>DAYS</li>	 
	 * 	<li>HOUR</li>	 
	 * 	<li>HOURS</li>	 
	 * 	<li>MINUTE</li>	 
	 * 	<li>MINUTES</li>	 
	 * 	<li>SECOND</li>	 
	 * 	<li>SECONDS</li>	 
	 * 	<li>MICROSECOND</li>	 
	 * 	<li>MICROSECONDS</li>	 
	 * </ul>
	 * <p>
	 * Viene restituita l'enumerazionje che codifica la durata.<br>
	 * <p>
	 * @return the typeLabeledDuration
	 */
	public EnumPrecompilerReservedWords getTypeLabeledDuration() {
		return typeLabeledDuration;
	}


	/**
	 * Imposta il tipo di labeled duration<br>
	 * <p>
	 * Si tratta di un elemento composto da:<br>
	 * <ul>
	 * 	<li>function-invocation</li>
	 * 	<li>(expression)</li>
	 * 	<li>constant</li>	 
	 * 	<li>column-name</li>	 
	 * 	<li>variable</li>	 
	 * </ul>
	 * Seguito da una indicazione di durata quale:
	 * <p>
	 * <ul>
	 * 	<li>YEAR</li>
	 * 	<li>YEARS 	</li>
	 * 	<li>MONTH</li>	 
	 * 	<li>MONTHS</li>	 
	 * 	<li>DAY</li>	 
	 * 	<li>DAYS</li>	 
	 * 	<li>HOUR</li>	 
	 * 	<li>HOURS</li>	 
	 * 	<li>MINUTE</li>	 
	 * 	<li>MINUTES</li>	 
	 * 	<li>SECOND</li>	 
	 * 	<li>SECONDS</li>	 
	 * 	<li>MICROSECOND</li>	 
	 * 	<li>MICROSECONDS</li>	 
	 * </ul>
	 * <p>
	 * Viene impostata l'enumerazionje che codifica la durata.<br>
	 * <p>
	 * @param typeLabeledDuration the typeLabeledDuration to set
	 */
	public void setTypeLabeledDuration(EnumPrecompilerReservedWords typeLabeledDuration) {
		this.typeLabeledDuration = typeLabeledDuration;
	}


	/**
	 * Restituisce se l'elemento codifica una labeled duration<br>
	 * <p>
	 * Si tratta di labeled-duration se l'elemento è composto da:<br>
	 * <ul>
	 * 	<li>function-invocation</li>
	 * 	<li>(expression)</li>
	 * 	<li>constant</li>	 
	 * 	<li>column-name</li>	 
	 * 	<li>variable</li>	 
	 * </ul>
	 * Seguito da una indicazione di durata quale:
	 * <p>
	 * <ul>
	 * 	<li>YEAR</li>
	 * 	<li>YEARS 	</li>
	 * 	<li>MONTH</li>	 
	 * 	<li>MONTHS</li>	 
	 * 	<li>DAY</li>	 
	 * 	<li>DAYS</li>	 
	 * 	<li>HOUR</li>	 
	 * 	<li>HOURS</li>	 
	 * 	<li>MINUTE</li>	 
	 * 	<li>MINUTES</li>	 
	 * 	<li>SECOND</li>	 
	 * 	<li>SECONDS</li>	 
	 * 	<li>MICROSECOND</li>	 
	 * 	<li>MICROSECONDS</li>	 
	 * </ul>
	 * <p>
	 * @return the isLabeledDuration
	 */
	public boolean isLabeledDuration() {
		return isLabeledDuration;
	}


	/**
	 * Imposta se l'elemento codifica una labeled duration<br>
	 * <p>
	 * Si tratta di labeled-duration se l'elemento è composto da:<br>
	 * <ul>
	 * 	<li>function-invocation</li>
	 * 	<li>(expression)</li>
	 * 	<li>constant</li>	 
	 * 	<li>column-name</li>	 
	 * 	<li>variable</li>	 
	 * </ul>
	 * Seguito da una indicazione di durata quale:
	 * <p>
	 * <ul>
	 * 	<li>YEAR</li>
	 * 	<li>YEARS 	</li>
	 * 	<li>MONTH</li>	 
	 * 	<li>MONTHS</li>	 
	 * 	<li>DAY</li>	 
	 * 	<li>DAYS</li>	 
	 * 	<li>HOUR</li>	 
	 * 	<li>HOURS</li>	 
	 * 	<li>MINUTE</li>	 
	 * 	<li>MINUTES</li>	 
	 * 	<li>SECOND</li>	 
	 * 	<li>SECONDS</li>	 
	 * 	<li>MICROSECOND</li>	 
	 * 	<li>MICROSECONDS</li>	 
	 * </ul>
	 * <p>
	 * @param isLabeledDuration the isLabeledDuration to set
	 */
	public void setLabeledDuration(boolean isLabeledDuration) {
		this.isLabeledDuration = isLabeledDuration;
	}


	/**
	 * Restituisce l'espressione se il tipo elemento è <tt>EXPRESSION</tt><br>
	 * <p>
	 * @return the expression
	 */
	public SqlExpression getExpression() {
		return expression;
	}


	/**
	 * Imposta l'espressione se il tipo elemento è <tt>EXPRESSION</tt><br>
	 * <p>
	 * @param expression the expression to set
	 */
	public void setExpression(SqlExpression expression) {
		this.expression = expression;
	}


	/**
	 * Restituisce la full select se il tipo elemento è <tt>SCALAR_FULL_SELECT</tt><br>
	 * <p>
	 * @return the fullSelect
	 */
	public SqlFullSelect getFullSelect() {
		return scalarFullSelect;
	}


	/**
	 * Imposta la full select se il tipo elemento è <tt>SCALAR_FULL_SELECT</tt><br>
	 * <p>
	 * @param fullSelect the fullSelect to set
	 */
	public void setFullSelect(SqlFullSelect fullSelect) {
		this.scalarFullSelect = fullSelect;
	}


	/**
	 * Restituisce il nome della colonna se il tipo elemento è <tt>COLUMN_NAME</tt><br>
	 * <p>
	 * La colonna può essere qualificata o meno ma viene restituito solo il nome della colonna.<br>
	 * <p>
	 * Il qualificatore può essere il nome di una tabella, di una view, un alias, un sinonimo <br>
	 * o un correlation-name.<br>
	 * <p>
	 * <tt>table.column</tt><br>
	 * <tt>view.column</tt><br>
	 * <tt>alis.column</tt><br>
	 * <tt>synonym.column</tt><br>
	 * <tt>corr.column</tt><br>
	 * <p>
     * @return the columnName
	 */
	public String getColumnName() {
		int i = 0;
		i = columnName.indexOf(".");
		// Nome non qualificato
		if (i < 0) {
			return columnName;
		}
		return columnName.substring(i + 1);
	}


	/**
	 * Imposta il nome della colonna se il tipo elemento è <tt>COLUMN_NAME</tt><br>
	 * <p>
	 * La colonna può essere qualificata o meno.<br>
	 * <p>
	 * Il qualificatore può essere il nome di una tabella, di una view, un alias, un sinonimo o un correlation-name.<br>
	 * <p>
	 * <tt>table.column</tt><br>
	 * <tt>view.column</tt><br>
	 * <tt>alias.column</tt><br>
	 * <tt>synonym.column</tt><br>
	 * <tt>corr.column</tt><br>
	 * <p>
	 * Se la colonna è qualificata estrae e memorizza il qualificatore, che può<br>
	 * essere ottenuto con getColumnQualifier()<br>
	 * <p>
	 * @param columnNameQualified the columnNameQualified to set
	 */
	public void setColumnNameQualified(String columnNameQualified) {
		int i = 0;
		i = columnNameQualified.indexOf(".");
		
		// Nome non qualificato
		if (i < 0) {
			this.columnName = columnNameQualified;
			this.columnQualifier = "";
			return;
		}
        
		// Nome qualificato
		this.columnQualifier = columnNameQualified.substring(0, i);
		this.columnName = columnNameQualified.substring(i + 1);
	}


	/**
	 * Restituisce il qualificatore della della colonna se il tipo elemento è <tt>COLUMN_NAME</tt><br>
	 * <p>
	 * Il qualificatore può essere il nome di una tabella, di una view, un alias, un sinonimo <br>
	 * o un correlation-name.<br>
	 * <p>
	 * <tt>table.column</tt><br>
	 * <tt>view.column</tt><br>
	 * <tt>alis.column</tt><br>
	 * <tt>synonym.column</tt><br>
	 * <tt>corr.column</tt><br>
	 * <p>
	 * @return the columnQualifier
	 */
	public String getColumnQualifier() {
		return columnQualifier;
	}


	/**
	 * Imposta il qualificatore della della colonna se il tipo elemento è <tt>COLUMN_NAME</tt><br>
	 * <p>
	 * Il qualificatore può essere il nome di una tabella, di una view, un alias, un sinonimo <br>
	 * o un correlation-name.<br>
	 * <p>
	 * <tt>table.column</tt><br>
	 * <tt>view.column</tt><br>
	 * <tt>alis.column</tt><br>
	 * <tt>synonym.column</tt><br>
	 * <tt>corr.column</tt><br>
	 * <p>
	 * @param columnQualifier the columnQualifier to set
	 */
	public void setColumnQualifier(String columnQualifier) {
		this.columnQualifier = columnQualifier;
	}


	/**
	 * Restituisce una stringa con il valore della costante se il tipo elemento è <tt>CONSTANT</tt><br>
	 * <p>
	 * Il valore può essere una literal alfanumerica o una costante numerica.<br>
	 * <p>
	 * @return the constant
	 */
	public String getConstant() {
		return constant;
	}


	/**
	 * Imposta una stringa con il valore della costante se il tipo elemento è <tt>CONSTANT</tt><br>
	 * <p>
	 * Il valore può essere una literal alfanumerica o una costante numerica.<br>
	 * <p>
	 * @param constant the constant to set
	 */
	public void setConstant(String constant) {
		this.constant = constant;
	}


	/**
	 * Restituisce la variabile host, senza i due punti iniziali.<br>
	 * <p>
	 * @return the hostVar
	 */
	public String getHostVar() {
		return hostVar;
	}


	/**
	 * Imposta la variabile host, senza i due punti iniziali.<br>
	 * <p>
	 * @param hostVar the hostVar to set
	 */
	public void setHostVar(String hostVar) {
		this.hostVar = hostVar;
	}


	/**
	 * Restituisce il nome della variabile se il tipo elemento è <tt>VARIABILE</tt><br>
	 * <p>
	 * Le variabili sono definite nei <tt>TRIGGER</tt>.<br>
	 * <p>
	 * @return the variabileName
	 */
	public String getVariabileName() {
		return variabileName;
	}


	/**
	 * Imposta il nome della variabile se il tipo elemento è <tt>VARIABILE</tt><br>
	 * <p>
	 * Le variabili sono definite nei <tt>TRIGGER</tt>.<br>
	 * <p>
	 * @param variabileName the variabileName to set
	 */
	public void setVariabileName(String variabileName) {
		this.variabileName = variabileName;
	}


	/**
	 * Restituisce il nome della funzione se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * @return the functionName
	 */
	public String getFunctionName() {
		return functionName;
	}


	/**
	 * Imposta il nome della funzione se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * @param functionName the functionName to set
	 */
	public void setFunctionName(String functionName) {
		this.functionName = functionName;
	}


	/**
	 * Restituisce la funzione codificata se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * @return the functionNameCoded
	 */
	public EnumPrecompilerReservedWords getFunctionNameCoded() {
		return functionNameCoded;
	}


	/**
	 * Imposta la funzione codificata se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * @param functionNameCoded the functionNameCoded to set
	 */
	public void setFunctionNameCoded(EnumPrecompilerReservedWords functionNameCoded) {
		this.functionNameCoded = functionNameCoded;
	}


	/**
	 * Restituisce una striga con i parametri passati alla funzione se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * Viene restituita esattamente la striga dentro le parentesi senza nessuna elaborazione.<br>
	 * <p>
	 * @return the al_functionParm
	 */
	public List<String> getFunctionParms() {
		return al_functionParm;
	}


	/**
	 * Imposta una striga con i parametri passati alla funzione se il tipo elemento è <tt>FUNCTION_INVOCATION</tt><br>
	 * <p>
	 * Viene impostata esattamente la striga dentro le parentesi senza nessuna elaborazione.<br>
	 * <p>
	 * @param al_functionParm the al_functionParm to set
	 */
	public void setFunctionParms(List<String> al_functionParm) {
		this.al_functionParm = al_functionParm;
	}


	/**
	 * Restituisce il registro speciale se il tipo elemento è <tt>SPECIAL_REGISTER</tt><br>
	 * <p>
	 * Viene restituita una enumerazione con la parola riservata del registro.<br>
	 * <p>
	 * @return the specialRegister
	 */
	public EnumPrecompilerReservedWords getSpecialRegister() {
		return specialRegister;
	}


	/**
	 * Imposta il registro speciale se il tipo elemento è <tt>SPECIAL_REGISTER</tt><br>
	 * <p>
	 * Viene impostata una enumerazione con la parola riservata del registro.<br>
	 * <p>
	 * @param specialRegister the specialRegister to set
	 */
	public void setSpecialRegister(EnumPrecompilerReservedWords specialRegister) {
		this.specialRegister = specialRegister;
	}


	/**
	 * Restituisce la durata se il tipo elemento è <tt>LABELED_DURATION</tt><br>
	 * <p>
	 * La durata è espressa in termini di anni, mesi, giorni, ore, minuti, secondi e microsecondi<br>
	 * <p>
	 * L'elemento di durata è preceduto da un elemento del tipo:<br>
	 * <ul>
	 * <li> function-invocation</li>
	 * <li> (expression)</li>
	 * <li> constant</li>
	 * <li> column-name</li>
	 * <li> variabile</li>
	 * </ul>
	 * <p>
	 * @return the duration
	 */
	public EnumSqlDuration getDuration() {
		return duration;
	}


	/**
	 * Imposta la durata se il tipo elemento è <tt>LABELED_DURATION</tt><br>
	 * <p>
	 * La durata è espressa in termini di anni, mesi, giorni, ore, minuti, secondi e microsecondi<br>
	 * <p>
	 * L'elemento di durata è preceduto da un elemento del tipo:<br>
	 * <ul>
	 * <li> function-invocation</li>
	 * <li> (expression)</li>
	 * <li> constant</li>
	 * <li> column-name</li>
	 * <li> variabile</li>
	 * </ul>
	 * <p>
	 * @param duration the duration to set
	 */
	public void setDuration(EnumSqlDuration duration) {
		this.duration = duration;
	}


	/**
	 * Restituisce l'espressione di case se il tipo elemento è <tt>CASE_EXPRESSION</tt><br>
	 * <p>
	 * Viene restituita una stringa con il valore presente fra l'istruzione <tt>CASE</tt>
	 * e l'istruzione <tt>END</tt> escluse, senza nessuna elaborazione.
	 * <p>
	 * @return the caseExpression
	 */
	public String getCaseExpression() {
		return caseExpression;
	}


	/**
	 * Imposta l'espressione di case se il tipo elemento è <tt>CASE_EXPRESSION</tt><br>
	 * <p>
	 * Viene impostata una stringa con il valore presente fra l'istruzione <tt>CASE</tt>
	 * e l'istruzione <tt>END</tt> escluse, senza nessuna elaborazione.
	 * <p>
	 * @param caseExpression the caseExpression to set
	 */
	public void setCaseExpression(String caseExpression) {
		this.caseExpression = caseExpression;
	}


	/**
	 * Restituisce l'espressione di cast se il tipo elemento è <tt>CAST_SPECIFICATION</tt><br>
	 * <p>
	 * Viene restituita il valore fra parentesi dell'istruzione <tt>CAST( specification )</tt>
	 * senza nessuna elaborazione.
	 * <p>
	 * @return the castSpecification
	 */
	public String getCastSpecification() {
		return castSpecification;
	}


	/**
	 * Imposta l'espressione di cast se il tipo elemento è <tt>CAST_SPECIFICATION</tt><br>
	 * <p>
	 * Viene impostato il valore fra parentesi dell'istruzione <tt>CAST( specification )</tt>
	 * senza nessuna elaborazione.
	 * <p>
	 * @param castSpecification the castSpecification to set
	 */
	public void setCastSpecification(String castSpecification) {
		this.castSpecification = castSpecification;
	}


	/**
	 * Restituisce se indicato <tt>TIMESTAMP>, se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * @return the isRowChangeTimestamp
	 */
	public boolean isRowChangeTimestamp() {
		return isRowChangeTimestamp;
	}


	/**
	 * Imposta se indicato <tt>TIMESTAMP>, se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * @param isRowChangeTimestamp the isRowChangeTimestamp to set
	 */
	public void setRowChangeTimestamp(boolean isRowChangeTimestamp) {
		this.isRowChangeTimestamp = isRowChangeTimestamp;
	}


	/**
	 * Restituisce se indicato <tt>TOKEN>, se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * @return the isRowChangeToken
	 */
	public boolean isRowChangeToken() {
		return isRowChangeToken;
	}


	/**
	 * Imposta se indicato <tt>TIMESTAMP>, se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * @param isRowChangeToken the isRowChangeToken to set
	 */
	public void setRowChangeToken(boolean isRowChangeToken) {
		this.isRowChangeToken = isRowChangeToken;
	}


	/**
	 * Restituisce il table-designator  se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * Si tratta di <tt> ROW CHANGE TIMESTAMP|TOKEN FOR table-designator</tt><br>
	 * <p>
	 * @return the rowChangeTableDesignator
	 */
	public String getRowChangeTableDesignator() {
		return rowChangeTableDesignator;
	}


	/**
	 * Imposta il table-designator  se il tipo elemento è <tt>ROW_RANGE_EXPRESSION</tt><br>
	 * <p>
	 * Si tratta di <tt> ROW CHANGE TIMESTAMP|TOKEN FOR table-designator</tt><br>
	 * <p>
	 * @param rowChangeTableDesignator the rowChangeTableDesignator to set
	 */
	public void setRowChangeTableDesignator(String rowChangeTableDesignator) {
		this.rowChangeTableDesignator = rowChangeTableDesignator;
	}


	/**
	 * Restituisce se <tt>NEXT</tt>,  se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @return the isSequenceReferenceNext
	 */
	public boolean isSequenceReferenceNext() {
		return isSequenceReferenceNext;
	}


	/**
	 * Imposta se <tt>NEXT</tt>,  se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @param isSequenceReferenceNext the isSequenceReferenceNext to set
	 */
	public void setSequenceReferenceNext(boolean isSequenceReferenceNext) {
		this.isSequenceReferenceNext = isSequenceReferenceNext;
	}


	/**
	 * Restituisce se <tt>PREVIOUS</tt>,  se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @return the isSequenceReferencePrevious
	 */
	public boolean isSequenceReferencePrevious() {
		return isSequenceReferencePrevious;
	}


	/**
	 * Imposta se <tt>PREVIOUS</tt>,  se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @param isSequenceReferencePrevious the isSequenceReferencePrevious to set
	 */
	public void setSequenceReferencePrevious(boolean isSequenceReferencePrevious) {
		this.isSequenceReferencePrevious = isSequenceReferencePrevious;
	}


	/**
	 * Restituisce il sequence reference name, se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @return the sequenceReferenceName
	 */
	public String getSequenceReferenceName() {
		return sequenceReferenceName;
	}


	/**
	 * Imposta il sequence reference name, se il tipo elemento è <tt>SEQUENCE_REFERENCE</tt><br>
	 * <p>
	 * @param sequenceReferenceName the sequenceReferenceName to set
	 */
	public void setSequenceReferenceName(String sequenceReferenceName) {
		this.sequenceReferenceName = sequenceReferenceName;
	}


	/**
	 * Restituisce la specificazione dell'XML cast, se il tipo elemento è <tt>XML_CAST_SPECIFICATION</tt><br>
	 * Viene restituito il valore fra parentesi dell'istruzione <tt>XMLCAST( specification )</tt>
	 * senza nessuna elaborazione.
	 * <p>
	 * @return the xMLCastSpecification
	 */
	public String getXMLCastSpecification() {
		return XMLCastSpecification;
	}


	/**
	 * Imposta la specificazione dell'XML cast, se il tipo elemento è <tt>XML_CAST_SPECIFICATION</tt><br>
	 * Viene impostato il valore fra parentesi dell'istruzione <tt>XMLCAST( specification )</tt>
	 * senza nessuna elaborazione.
	 * <p>
	 * @param xMLCastSpecification the xMLCastSpecification to set
	 */
	public void setXMLCastSpecification(String xMLCastSpecification) {
		XMLCastSpecification = xMLCastSpecification;
	}


	/**
	 * Restituisce la specificazione OLAP, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * Viene impostata una stringa con la specificazione dentro il parametro OVER completa senza nessuna elaborazione.
	 * <p>
	 * @return the oLAPSpecification
	 */
	public String getOLAPSpecification() {
		return OLAPSpecification;
	}


	/**
	 * Imposta la specificazione OLAP, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * Viene restituita una stringa con la specificazione dentro il parametro OVER completa senza nessuna elaborazione.
	 * <p>
	 * @param oLAPSpecification the oLAPSpecification to set
	 */
	public void setOLAPSpecification(String oLAPSpecification) {
		OLAPSpecification = oLAPSpecification;
	}


	/**
	 * Restituisce il tipo di specificazione OLAP, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * Viene restituita una stringa con RANK|DENSE_RANK|ROW_NUMBER|function-name
	 * <p>
	 * @return the oLAPType
	 */
	public String getOLAPType() {
		return OLAPType;
	}


	/**
	 * Imposta il tipo di specificazione OLAP, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * Viene impostata una stringa con RANK|DENSE_RANK|ROW_NUMBER|function-name
	 * <p>
	 * @param oLAPType the oLAPType to set
	 */
	public void setOLAPType(String oLAPType) {
		OLAPType = oLAPType;
	}


	/**
	 * Restituisce il contenuto fra parentesi della funzione aggregata, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * Viene restituita una stringa la definizione della funzione aggregata codificata
	 * <p>
	 * @return the oLAPAggregateFunctionBody
	 */
	public String getOLAPAggregateFunctionBody() {
		return OLAPAggregateFunctionBody;
	}


	/**
	 * Imposta il contenuto fra parentesi della funzione aggregata, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * Viene impostata una stringa la definizione della funzione aggregata codificata
	 * <p>
	 * @param oLAPAggregateFunctionBody the oLAPAggregateFunctionBody to set
	 */
	public void setOLAPAggregateFunctionBody(String oLAPAggregateFunctionBody) {
		OLAPAggregateFunctionBody = oLAPAggregateFunctionBody;
	}


	/**
	 * Restituisce il nome della funzione aggregata, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * @return the oLAPAggregateFunctionName
	 */
	public String getOLAPAggregateFunctionName() {
		return OLAPAggregateFunctionName;
	}


	/**
	 * Imposta il nome della funzione aggregata, se il tipo elemento è <tt>OLAP_SPECIFICATION</tt><br>
	 * <p>
	 * @param oLAPAggregateFunctionName the oLAPAggregateFunctionName to set
	 */
	public void setOLAPAggregateFunctionName(String oLAPAggregateFunctionName) {
		OLAPAggregateFunctionName = oLAPAggregateFunctionName;
	}


	/**
	 * Restituisce il valore parsato nell'espressione sql non riconosciuto come elemento valido.<br>
	 * <p>
	 * @return the valueNotAssigned
	 */
	public String getValueNotAssigned() {
		return valueNotAssigned;
	}


	/**
	 * Imposta il valore parsato nell'espressione sql non riconosciuto come elemento valido.<br>
	 * <p>
	 * @param valueNotAssigned the valueNotAssigned to set
	 */
	public void setValueNotAssigned(String valueNotAssigned) {
		this.valueNotAssigned = valueNotAssigned;
	}



	
}
