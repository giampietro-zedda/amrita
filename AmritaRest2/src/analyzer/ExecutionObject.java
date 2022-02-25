/**
 * 
 */
package analyzer;

import enums.EnumObject;

/**
 * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ExecutionObjectsToprocess
 * </h1>
 * <p>
 * 
 * Classe contenitore per un singolo oggetto da processare a fronte del file Pilot di esecuzione.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 03/01/2021
 * @see ExecutionDispatcherWeb
 * @see ExecutionShared
  *
 */
public class ExecutionObject {
    public String idObject = "";				// Nome oggetteo da processare
    public EnumObject typeObject = null;       // Tipologia oggetto   
    public ExecutionObject() {
	}

	
}
