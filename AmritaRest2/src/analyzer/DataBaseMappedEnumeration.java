package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
/**
 * Copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
* <h1>
* DataBaseMappedEnumeration  Annotation
* </h1>
*  <p>
* E' l'annotation marker utilizzata per marcare una enumeration utilizzata come campo,
* nel mapping con il database. 
* La colonna db viene trattata come intero.
* 
* 
* @author Giampietro Zedda
* @version 1.0.0
* @since 21/03/2010
* @see DataBaseManager 
* @see DataBaseMappedTable
* @see DataBaseMappedColumn
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedEnumeration {}
