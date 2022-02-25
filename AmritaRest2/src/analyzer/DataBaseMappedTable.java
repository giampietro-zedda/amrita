package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
* <h1>
* DataBaseMappedTable  Annotation
* </h1>
*  <p>
* E' l'annotation utilizzata per mappare le classi di gestione Entity con le tabelle di
* database. E' sufficiente dichiarare l'annotation con il nome della tabella fra parentesi.
* 
* 
* @author Giampietro Zedda
* @version 1.0.0
* @since 21/03/2010
* @see DataBaseManager  
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedTable {
	String value();
}
