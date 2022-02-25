package analyzer;

import java.io.Serializable;
import java.util.ArrayList;


/**
 *   Classe contenitore con la descrizione di una istruzione dinamica da risolvere o risolta.
 *   
 *   Tutte le informazioni sui sottocami, assegnazioni, etc sono memorizzati a partire da LogicDynamicField
 *   
 */
public class LogicDynamicInstruction implements Serializable {
	
	private static final long serialVersionUID = 1L;

	// Programma di appartenenza
	public String programName = "";
	
	// Istruzione dinamica (Call/Exec Cics Read/etc) 
	public Instruction dynamicInstr = null;					   
	
	// Operandi dinamici da risolvere/risolti
	public ArrayList<LogicDynamicField> al_dynamicField = null;
    
	
	/*
	 * Costruttore
	 */
	public LogicDynamicInstruction() {
		al_dynamicField = new ArrayList<LogicDynamicField> ();
	}


	/**
	 * @return the dynamicInstr
	 */
	public Instruction getDynamicInstr() {
		return dynamicInstr;
	}


	/**
	 * @param dynamicInstr the dynamicInstr to set
	 */
	public void setDynamicInstr(Instruction dynamicInstr) {
		this.dynamicInstr = dynamicInstr;
	}


	/**
	 * @return the al_dynamicField
	 */
	public ArrayList<LogicDynamicField> getDynamicFields() {
		return al_dynamicField;
	}


	/**
	 * @param al_dynamicField the al_dynamicField to set
	 */
	public void setDynamicFields(ArrayList<LogicDynamicField> al_dynamicField) {
		this.al_dynamicField = al_dynamicField;
	}
	
	
}

