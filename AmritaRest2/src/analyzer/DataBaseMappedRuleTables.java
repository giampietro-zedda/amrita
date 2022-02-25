package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import entities.EntityObject;

/**
 * copyright (c) 2010-2012 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMappedTableRule  Annotation
 * </h1>
 *  <p>
 *  
 * This annotation must be used with entity definition classes like, for example, {@link EntityObject}.<br>
 * It's used to map rule table, managed by means of forward generalyzed rule tables system,<br>
 * on fields declared by entity.<br>
 * Normally there is just only a field to map, to access to rule table data. This field can be an<br>
 * enumeration, translated in a numeric string or directly a string.<br>
 * There can be many rule table mapped to an entity, each with its own annotation.<br>
 * When rule table are mapped to an entity by this annotation, rule table data are automatically<br>
 * available to the forward logical data view declaring it.<br>
 * Rule table is defined in the forward table system manager, with own column names and type, just<br>
 * like a Sql table. Often there is only a column with a multilanguage description but the forward<br>
 * generalyzed table manager allows you to define as columns as necessary.<br>
 * In this annotation you must specify the id of the table, a string value with a numeric value that's the
 * table defined number or a non numeric string identifying the table.<br>
 * Then you must specify an array of strings.<br>
 * Each array elements represent a key to accesss to the table. This key is couple of names.<br>
 * The first name is the name of a field defined in the entity where the annotation is coded.<br>
 * The second name is the name of a key defined in the forward generalized tables management system<br>
 * to be a key or an element key.<br>
 * Normally, for rule tables, only a key field identifies a table element. However are automatically set<br>
 * and managed by forward all others key elements as table id and language.<br>
 * When only a key table element is required, apart table id and language, it's possible to omit the table key field
 * and to specify only the java
 * 
 * 
 * 
 * <p>
 * Here is an example to map table <code>T001</code> using <code>typeObject</code> java entity field :<pre>
   '@DataBaseMappedTableRule(tabId = "1"; java_key_tab = {"typeObject 1" }'
   </pre>
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 1/03/2012
 * @see DataBaseManager  
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedRuleTables {
	DataBaseMappedRuleTable[] tablesRule();
}
