package enums;



/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumPathDeep  
 * </h1>
 *
 * Profondità path generati nel processo di visita, ovvero con i i richiami a sottografi
 * interni espansi o meno.
 *
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 6/01/2010
 * @see GraphManager
 * 
 */
public enum EnumPathDeep {
	
	NOT_ASSIGNED,					// 00 
	
	OUTPUT_PATH_NOT_EXPANDED,   	// 01 Path generati con i richiami a sottografi interni NON espansi
	OUTPUT_PATH_EXPANDED;  	    	// 02 Path generati con i richiami a sottografi interni ESPANSI
}
