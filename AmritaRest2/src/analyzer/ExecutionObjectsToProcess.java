/**
 * 
 */
package analyzer;

import java.io.File;
import java.util.ArrayList;
import analyzer.ExecutionShared.InnerDescriptorSource;
import enums.EnumDirectivesExecution;
import exception.ExceptionAmrita;

/**
 * Copyright (c) 2009-2021 e-Amrita - Ing. Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * ExecutionObjectsToprocess
 * </h1>
 * <p>
 * 
 * Classe dedicata al recupero degli oggetti da processare a fronte del file Pilot di esecuzione.
 * E' definito un solo metodo getObjectList() che restituisce l'elenco degli oggetti da eleborare.
 * Vengono richiamati i metodi pubblici di ExecutionDispatcherWeb e di ExecutionShared per ottenere
 * l'elenco degli oggetti con la stessa logica che si ha in esecuzione.
 * 
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 03/01/2021
 * @see ExecutionDispatcherWeb
 * @see ExecutionShared
  *
 */
public class ExecutionObjectsToProcess implements AmritaConstants{
	private UserConfiguration ucfg = null;				// User configuration attiva
    private SourceInput siPilotExecution = null;		// Container righe file pilot processi 
    
    public ExecutionObjectsToProcess(UserConfiguration ucfg) {
		 this.ucfg = ucfg;
	}
    /*
     * Restituisce l'elenco degli oggetti a fronte di un file pilot
     * 
     * @param pilotName 
     */
    public ArrayList<ExecutionObject> getObjectList(String pilotName) throws Exception {
     	ExecutionDirectives di = null;
		ExecutionDispatcherWeb exDspcWeb = null;
    	EnumDirectivesExecution en_processOrFunction = null;
    	ExecutionObject executionObject = null;
        ArrayList<ExecutionObject> al_object = new ArrayList<ExecutionObject>();
    	String[] arRowPilotExecution = null;         
    	
    	// Recupero Righe pilot
    	AmritaStartup.sm.setUcfg(ucfg);
		siPilotExecution = AmritaStartup.sm.getSource(ucfg.getPathUser() + File.separator + ucfg.getDirPilot() + File.separator +  pilotName, false, true);
	   	try {
			exDspcWeb = new ExecutionDispatcherWeb(ucfg, siPilotExecution);
		   	di = new ExecutionDirectives(ucfg);
		   	exDspcWeb.setDi(di);
			 
		} catch (ExceptionAmrita e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		arRowPilotExecution = siPilotExecution.getArrayRowSource();   // Recupero righe direttive pilota di esecuzione		 

		
		// Controllo, intabellamento e classificazione informazioni direttive di processo e funzione  
		for (int i = 0; i < arRowPilotExecution.length; i++) {

			// Commento o riga vuota: Skip
			if (arRowPilotExecution[i].startsWith(TAG_COMMENT )   
			||  arRowPilotExecution[i].trim().equals("")) {	
				continue;
			}

			en_processOrFunction = exDspcWeb.detectProcessOrFunctionType(arRowPilotExecution[i]);

			// Non è una direttiva valida: skip
			if (en_processOrFunction == EnumDirectivesExecution.NOT_ASSIGNED) {
				continue;
			}

			// Inserimento informazioni di filtro, selezione  oggetti, thread, modalità di esecuzione ..
			exDspcWeb.extractInfoDirective(en_processOrFunction, arRowPilotExecution[i]);  // -> al_directivesInfoToExec

		} // End-for
		
		// Ogni elemento è un ArrayList di direttive di una funzione/processo
		ArrayList<ArrayList<ExecutionDirectives>> al_splittedExecInfo = null;

		// Recupero descrittore funzione da eseguire
		al_splittedExecInfo = exDspcWeb.splitInfoExecutionUntit();
		
		ArrayList<ExecutionDirectives> al_di = al_splittedExecInfo.get(0);
		ProcessAnalyzeSource pas = new ProcessAnalyzeSource(ucfg, al_di);
		ArrayList<InnerDescriptorSource> al_srcDescriptor = pas.getObjectsToAnalyze();
		
		// Caricamento elenco di oggetti in output
		for (InnerDescriptorSource innerDescriptorSource : al_srcDescriptor) {
			executionObject = new ExecutionObject();
			executionObject.idObject = innerDescriptorSource.idObject;
			executionObject.typeObject = innerDescriptorSource.objectType;
			al_object.add(executionObject);
		}
		
		return al_object;
	}
	 
}
