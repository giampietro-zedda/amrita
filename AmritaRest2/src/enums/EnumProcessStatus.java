package enums;

import analyzer.DataBaseMappedEnumeration;

/**
 * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * EnumProcessStatus  
 * </h1>
 *
 * Tipologie stati monitoraggio processi e funzioni, memorizzati in ProcessLog (PLOG)
 * 
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 19/03/2010
 * @see GraphManager
 * 
 */
@DataBaseMappedEnumeration
public enum EnumProcessStatus {
	
	NOT_ASSIGNED,				// 00  
	
	STARTED,					// 01 Processo/Funzione startato
	ENDED_WITH_NO_ERRORS,       // 02 Processo/Funzione terminato senza errori
	ENDED_WITH_ERRORS,          // 03 Processo/Funzione terminato con errori
	ENDED_WITH_EXCEPTION,       // 04 Processo/Funzione terminato con errori
	RUNNING,					// 05 Processo/Funzione in elaborazione
	STOPPED,                    // 06 Processo/Funzione stoppato dall'operatore
	RESTARTED;					// 07 Processo/Funzione riavviato a fronte di uno stop
}
