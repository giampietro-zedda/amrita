package analyzer;

import enums.EnumDirectivesExecution;

/**
 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
 *
 * <h1>
 * FilterEntry  
 * </h1>
 *  <p>
 * Questa è classe modella un singolo filtro a fronte di direttive
 * di filtro di {@link EnumDirectivesSource}  o {@link EnumDirectivesProcess}.
 * Il parametro è proprio l'enumerazione in questione.
 * 
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 02/04/2010
 * @see Analyzer
 * @see EnumDirectivesSource
 * @see EnumDirectivesProcess 
 */

public class FilterEntry {
    EnumDirectivesExecution edf = null;    						// Direttiva di filtro 
 	
    // Prima occorrenza filtro e valore
	int posFilter = 1; 						// Posizione filtro in nome sorgente 1-based (primo valore)
    String valueFilterToMath = "";			// Valore filtro di match (primo valore)
    
	// Array di filtri da applicare in AND sullo stesso nome sorgente
	Integer arPosFilter[] = null;			// Array di posizioni (1-based)
    String arValueFilterToMath[] = null;	// Array di valori
    

    // Costruttore semplice con sola direttiva di filtro
	public FilterEntry(EnumDirectivesExecution edf) {
		this.edf = edf;
	}

    
    // Costruttore semplice con un solo filtro
	public FilterEntry(EnumDirectivesExecution edf, int posFilter, String valueFilterToMath) {
		this.posFilter = posFilter;
		this.valueFilterToMath = valueFilterToMath;
		this.edf = edf;
		this.arPosFilter = new Integer[1];
		this.arValueFilterToMath = new String[1];
		this.arPosFilter[0] = posFilter;
		this.arValueFilterToMath[0] = valueFilterToMath;		
	}

    // Costruttore con più filtri di pos/val dentro array di posizione e stringa separati
	public FilterEntry(EnumDirectivesExecution edf, Integer arPosFilter[], String arValueFilterToMath[]) {
		this.arPosFilter = arPosFilter;
		this.arValueFilterToMath = arValueFilterToMath;
		this.edf = edf;
	}

    // Costruttore con più filtri di pos/val dentro un array di stringhe
	FilterEntry(EnumDirectivesExecution edf, String arPosValueFilter[]) {
        int iOut = 0;
        this.edf = edf;
		this.arPosFilter = new Integer[arPosValueFilter.length/2];
		this.arValueFilterToMath = new String[arPosValueFilter.length/2];
        
		// Scan filtri
		for (int i = 0; i < arPosValueFilter.length - 1; i++) {
			arPosFilter[iOut] = new Integer(Integer.parseInt(arPosValueFilter[i]));
			i++;
			arValueFilterToMath[iOut] = arPosValueFilter[i];
			iOut++;
		}
	}

}
