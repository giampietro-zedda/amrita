package enums;


/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumPathCaching  
 * </h1>
 *
 * Tipologia metodi di caching dei path a fronte del processo di visita fra due nodi.
 * Sono gestite due cache separate per i path generati fra due nodi:
 * 1) Cache con i path contenenti nodi da espandere (richiami a sottografi interni)<br>
 * 2) Cache in cui tutti i path hanno effettuato il processo di espansione (ci sono solo nodi elementari)

 *
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 6/01/2010
 * @see GraphManager
 * 
 */
public enum EnumPathCaching {
	
	NOT_ASSIGNED,					// 00 
	
	CACHING_PATH_ONLY_TO_EXPAND, 	// 01 In cache i path senza espansione dei sottografi interni
	CACHING_PATH_ONLY_EXPANDED, 	// 02 In cache i path dopo l'espansione dei sottografi interni 
	CACHING_PATH_ALL, 			    // 03 In cache separati i path prima e dopo l'espansione
	CACHING_PATH_NONE; 			  	// 04 Nessun caching da effettuare
}
