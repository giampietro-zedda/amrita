package analyzer;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import entities.EntityObject;

/**
 * copyright (c) 2009-2010 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * DataBaseMappedColumns  Annotation
 * </h1>
 *  <p>
 * E' l'annotation utilizzata per mappare le classi di gestione Entity con le colonne della tabella
 * di database a cui si riferisce l'entity. 
 * Il mapping è realizzato dichiarando l'annotation e valorizzando l'array di stinghe java_db_col
 * con n righe quante sono le colonne. <br>
 * In ogni riga, separati da spaz, si indicano: il nome del campo java, il nome della colonna sql e
 * un'indicazione sulla natura della colonna. "PK" indica che la colonna fa parte della primary key
 * della tabella, "NK" indica che la colonna non è un campo di chiave primaria.<br>
 * Per un esempio di dichiarazione si veda {@link EntityObject}
 * 
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 21/03/2010
 * @see DataBaseManager  
*/
@Retention(RetentionPolicy.RUNTIME)
public @interface DataBaseMappedColumns {
	String[] java_db_col();
}
