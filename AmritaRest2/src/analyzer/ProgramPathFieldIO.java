package analyzer;

import java.util.ArrayList;
import java.util.List;

/*
 * ----------- 
 * PathFieldIO
 * ----------- 
 * 
 * Descrive un campo di Working/Linkage/Fd in Input/Output utilizzato in LogicTools
 * per ottenere i campi di I/O di uno specifico path di esecuzione
 * con xref alle istruzioni nel path che lo referenziano
 * 
 * 
 */
public class ProgramPathFieldIO {
	private String fieldName = "";        		//
    private int numInstrDef = 0;          		// Instruction Number in Working/Linkage/Fd
    private String section = "";          		// W/L/F
    private List<Integer> xrefInput  = null;    // References to input use in the same path
    private List<Integer> xrefOutput  = null;   // References to ouput use in the same path
    
    public ProgramPathFieldIO() { 	 
		this.xrefInput = new ArrayList<>();
		this.xrefOutput = new ArrayList<>();
	}

    public ProgramPathFieldIO(String fieldName, int numInstrDef, String section  ) {
		this.fieldName = fieldName;
		this.numInstrDef = numInstrDef;
		this.section = section;
		this.xrefInput = new ArrayList<>();
		this.xrefOutput = new ArrayList<>();
	}

	/**
	 * @return the fieldName
	 */
	public String getFieldName() {
		return fieldName;
	}

	/**
	 * @param fieldName the fieldName to set
	 */
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}


	/**
	 * @return the numInstrDef
	 */
	public int getNumInstrDef() {
		return numInstrDef;
	}

	/**
	 * @param numInstrDef the numInstrDef to set
	 */
	public void setNumInstrDef(int numInstrDef) {
		this.numInstrDef = numInstrDef;
	}

	/**
	 * @return the section
	 */
	public String getSection() {
		return section;
	}

	/**
	 * @param section the section to set
	 */
	public void setSection(String section) {
		this.section = section;
	}

	/**
	 * @return the xrefInput
	 */
	public List<Integer> getXrefInput() {
		return xrefInput;
	}

	/**
	 * @param xrefInput the xrefInput to set
	 */
	public void setXrefInput(List<Integer> xrefInput) {
		this.xrefInput = xrefInput;
	}

	/**
	 * @return the xrefOutput
	 */
	public List<Integer> getXrefOutput() {
		return xrefOutput;
	}

	/**
	 * @param xrefOutput the xrefOutput to set
	 */
	public void setXrefOutput(List<Integer> xrefOutput) {
		this.xrefOutput = xrefOutput;
	}

    
    

    
}
