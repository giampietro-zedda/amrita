package analyzer;

import java.io.Serializable;

/**
 *   Classe contenitore di servizio con le informazioni
 *   di assegnazione del sottocampo nei programmi chiamanti.<br>
 *   <p>
 *   Ogni istanza identifica un programma chiamante, gli estremi
 *   dell'istruzione target di chiamata (call, link, ..), tutte
 *   le informazioni sulla nuova ricerca e il descrittore corrente
 *   {@link LogicDynamicFieldSub}
 */
public class LogicDynamicFieldSubSpreaded implements Serializable {
	
	private static final long serialVersionUID = 1L;

	// Programma chiamante a cui il sottocampo appartiene.
	ProgramCobol program = null;
	
	// Info su nuovo campo in pgm chiamante di cui trovare i valori
    LogicDynamicFieldSub subFieldCaller = null;
    
    // Numero istruzione chiamante.
    // Verranno cercati valori fra la prima istruzione del programma e questa
    int numInstrCaller = 0;
    
    
	/* Costruttore vuoto */
	public LogicDynamicFieldSubSpreaded() {
	}

}
