package analyzer;

import java.io.Serializable;
import java.util.ArrayList;

import entities.EntityDynamicField;
import entities.EntityDynamicFieldSub;
import entities.EntityDynamicFieldSubSetting;
import entities.EntityDynamicFieldSubValue;
import enums.EnumDataItemType;
import enums.EnumObject;



/**
 * Copyright (c) 2009-2021 e-Amrita - Giampietro Zedda   Turin (ITALY)
 * 
 * <h1>
 * LogicInfoDynamic (NUOVA VERSIONE)
 * </h1>
 * <p>
 * Vengono modellate tutte le informazioni relative alle istruzioni dinamiche di un programma. 
 * Questo insieme di informazioni sono associate al programma nel suo complesso, organizzate per descrivere
 * ogni istruzione dinamica, da risolvere o risolta<br>
 * <p>
 * Viene gestita una Map<Integer, LogicDynamicInstruction> che mappa l'struzione dinamica, che a sua volta
 * mappa i campi dinamici dell'istruzione che a loro volta mappano sottocampi valori etc.<br>
 * E' quindi sufficiente individuare l'istruzione dinamica da risolvere/risolta.<br>
 * Questa classe si comporta solo da contenitore e non altera le informazioni sulle istruzioni dinamiche che
 * si diramano a partire dall'istruzione dinamica in LogicDynamicInstruction.
 * <br>
 * le classi che intervengono sono:
 *   LogicDynamicInstruction
 *   LogicDynamicField
 *   LogicDynamicFieldSub
 *   LogicDynamicFieldSubSetting
 * <p>
 * Per ogni istruzione dinamica sono memorizzati tutti gli operandi dinamici, gli eventuali sottocampi di cui sono composti, 
 * i valori che  questi assumono e tutte le operazioni di trasformazione, anche a cavallo fra programmi diversi.<br>
 * <p>
 * Viene mantenuta anche una ArrayList<EntityDynamicFieldSubWaitExt> con i sottocampi in attesa di dati esterni.
 * <p>
 * Sono disponibili i metodi per ottenere informazioni dalle classi di modellazione delle logiche.<br>
 * Il metodo putDatabaseInfo iinserisce tutte le informazioni dinamiche presenti nell'istanza di AnalizerDbInfo
 * per l'aggiornamento successivo su database.
 * <p>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 04/06/2010
 * @see LogicTools
 * @see LogicSamePgm
*/

public class LogicInfoDynamic implements Serializable, AmritaConstants {

	private static final long serialVersionUID = 1L;
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Variabili di istanza generali                                  //                                                        //
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// Reference a descrittore programma origine
	private ProgramCobol program = null;
	
	// Istruzioni dinamiche codificate in programma origine
	private ArrayList<LogicDynamicInstruction> al_DynamycInstr = null;
	
	
	/**
	 * Costruttore  
	 */
	public LogicInfoDynamic() {
	    this.al_DynamycInstr = new ArrayList<LogicDynamicInstruction>();	    
	} 
	
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////
	////////////////// Metodi pubblici                                       /////////////////// 
    ////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * 
     * Inserisce il descrittorre di una istruzione dinamica
     * 
     * @param Instruction dynamicInstr    
     */
	public void addDynamicInstr(Instruction dynamicInstr, LogicDynamicInstruction ldi) {
			this.al_DynamycInstr.add(ldi);
		return;
	}

	   /**
     * 
     * Restituisce il descrittore di una istruzione dinamica del programma origine
     * 
     * 
     * @param int numInstr     
     */
	public LogicDynamicInstruction getDynamicInstr(int numInstr) {
		String programName = "";
		programName = this.program.getProgramName();
		return getDynamicInstr(programName, numInstr);
	}

	/**
     * 
     * Restituisce il descrittore di una istruzione dinamica del programma fornito.
     * Si può trattare del programma origine o di un programma chiamante/chiamato.
     * 
     * 
     * 
     * @param int numInstr     
     */
	public LogicDynamicInstruction getDynamicInstr(String programName, int numInstr) {
			
		// Scan istruzioni dinamiche
		for (LogicDynamicInstruction logicDynamicInstruction : al_DynamycInstr) {
			
			if (!logicDynamicInstruction.programName.equals(programName)) {continue;}
			
			if (logicDynamicInstruction.getDynamicInstr().getNumInstr() == numInstr) {
				return logicDynamicInstruction;
			}
		}
		return null;
	}

	/**
     * 
     * Restituisce tutte le istruzioni dinamiche del programma
     */
	public ArrayList<LogicDynamicInstruction> getAllDynamicInstr() {
		return al_DynamycInstr;
	}

    /**
     * 
     * Richiamata a fronte di analisi programma e LogicSamePgm
     * 
     * Inserisce tutte le informazioni dinamiche per gli aggiornamenti finali.
     * Viene inserito il campo, i sottocampi, le trasformazioni e i valori.
     * 
     * In fase di analisi preliminare del programma LogicSamePgm gestisce tutte le
     * istruzioni dinamiche del programma origine e tutte le informazioni, apartire dall'istruzione
     * dinamica codificata in LogicDynamicInstruction (field, subfield, trasformazioni etc),
     * vengono memorizzate su db.
     * 
     * Se l'istruzione non è stata risolta allora viene trattata da LogicSpreadedPgm richiamando
     * all fine questo metodo per gli aggiornamenti.
     * In questo caso NON devono più essere inserite tutte le informazioni su campi e sottocampi
     * ma SOLO le trasformazioni dei vari sottocampi effettuate nei programmi chiamanti.
     * 
     * Vengono aggiornati in DynamicField i flag di stato campo dinamico:
     * 
     *  light
     *  solved
     *  solvedFull
     *  waitingForData
     *  spreaded
     *  
     * @param logicDynamicInstr 
     * @param AnalyzerDbinfo the database object container for finale db updates 
     */
	public void putDatabaseInfoAll(AnalyzerDbInfo adbi, LogicDynamicInstruction logicDynamicInstr, String fieldName) {
		EntityDynamicField entityDynamicField = null;
		EntityDynamicFieldSub entityDynamicFieldSub = null;
		EntityDynamicFieldSub entityDynamicFieldSubFirst = null;
		int iDbdynamicField = -1;                                     // DynamicField è già stato inserito in struttura db se NON spreaded pgm
		int progrValue = 0;
		int progrSet = 0;		
	    boolean isLight = false;
	    boolean isSolved = false;
	    boolean isSolvedFull = false;
	    boolean isWaitingForData = false;
	    boolean isSpreaded = false;
				
		// Scan fields (X execCics Send|Receive map possono essere 2)
		for (LogicDynamicField dynamicField : logicDynamicInstr.al_dynamicField) {
			
			// Clear flags
		    isLight = true;
		    isSolved = false;                             // true se almeno un valore è stato trovato
		    isSolvedFull = true;                          // false se almeno un sottocampo è senza valori o in attesa di dati o da risolvere in chiamanti
		    isWaitingForData = false;                     // true se almeno un sottocampo è ancora in attesa di valori
		    isSpreaded = false;
		    
			// true se field non movimentato con value
			if (!isDynamicLight(dynamicField)) {
				isLight = false;
			}
						
			// Su db solo la prima volta
			if (!dynamicField.isDbUpdated) {				
				iDbdynamicField = adbi.addObjEntityDynamicField(dynamicField.entityDynamicField);
				dynamicField.isDbUpdated = true;				
			} // end-if			
			
			// Se il campo è di gruppo NON era stato inserito un sottocampo di servizio a space 
			// Inserisco in struttura db un sottocampo a space
			if (dynamicField.dataItemCobolIdentifier.getDataItem().isGroupField()) {
				entityDynamicFieldSubFirst = dynamicField.al_FieldSub.get(0).entityDynamicFieldSub;
				entityDynamicFieldSub = new EntityDynamicFieldSub();
				entityDynamicFieldSub.setSystem(entityDynamicFieldSubFirst.getSystem());
				entityDynamicFieldSub.setSubSystem(entityDynamicFieldSubFirst.getSubSystem());
				entityDynamicFieldSub.setIdObject(entityDynamicFieldSubFirst.getIdObject());
				entityDynamicFieldSub.setTypeObject(entityDynamicFieldSubFirst.getTypeObject());
				entityDynamicFieldSub.setNumInstr(entityDynamicFieldSubFirst.getNumInstr());
				entityDynamicFieldSub.setIdField(entityDynamicFieldSubFirst.getIdField());
				entityDynamicFieldSub.setIdSubField("");
				entityDynamicFieldSub.setNumField(dynamicField.dataItemCobolIdentifier.getDataItem().getNumInstr());
				entityDynamicFieldSub.setNumSubField(dynamicField.dataItemCobolIdentifier.getDataItem().getNumInstr());
				entityDynamicFieldSub.setPosSubField(1);
				entityDynamicFieldSub.setSizeSubField(dynamicField.dataItemCobolIdentifier.getDataItem().getSizeBytes());
				entityDynamicFieldSub.setTypeSubField(EnumDataItemType.COBOL_GROUP);
				adbi.addObjEntityDynamicFieldSub(entityDynamicFieldSub);
			}
			
			// Scan subfield 
			for (LogicDynamicFieldSub dynamicFieldSub : dynamicField.al_FieldSub) {

				// Su db solo la prima volta 
				if (!dynamicFieldSub.isDbUpdated) {
					adbi.addObjEntityDynamicFieldSub(dynamicFieldSub.entityDynamicFieldSub);
					dynamicFieldSub.isDbUpdated = true;
				}
			
				// Scan valori sottocampi o assegnazioni implicite parziali o relative al campo complessivo X inserimento db
				progrValue = 0;
				for (LogicDynamicValue dynamicFieldSubValue : dynamicFieldSub.al_value) {
					
					// Su db solo la prima volta
					if (!dynamicFieldSubValue.isDbUpdated) {
						dynamicFieldSubValue.entityDynamicValue.setProgr(progrValue);
						adbi.addObjEntityDynamicFieldSubValue(dynamicFieldSubValue.entityDynamicValue);
						dynamicFieldSubValue.isDbUpdated = true;
					}
					progrValue++;
					
					// Sottocampo ha almeno un valore e si considera risolto
					dynamicFieldSub.entityDynamicFieldSub.setSolved(true);
					
					// Se campo elementare aggiorno flag valori  trovati (es default)
					if (dynamicFieldSubValue.entityDynamicValue.getIdSubField().isBlank()) {
						isSolved = true;
					}
				}
				
				// Nessun valore per il sottocampo: istruzione sicuramente non risolta
				if (dynamicFieldSub.al_value.size() == 0) {
					dynamicFieldSub.entityDynamicFieldSub.setSolved(false);
					dynamicFieldSub.isSolved = false;
					isSolvedFull = false;
				}
				
				// Scan catene trasformazione sottocampo
				for (ArrayList<LogicDynamicFieldSubSetting> al_chainSetSubField : dynamicFieldSub.al_al_chainSetSubField) {
				    
					progrSet = 0;
										
					// Scan assegnazioni catene trasformazione sottocampo
				    for (LogicDynamicFieldSubSetting dynamicFieldSubSetting : al_chainSetSubField) {
						
						// Sottocampo da risolvere nei programmi chiamanti
						if (LogicTools.isLastSetSpreaded(dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode())) {						
							isSpreaded = true;
							isSolvedFull = false;
							dynamicFieldSub.entityDynamicFieldSub.setSolved(false);
							dynamicFieldSub.entityDynamicFieldSub.setSpreaded(isSpreaded);
							dynamicFieldSub.isSpreaded = isSpreaded;
							
						// Sottocampo da risolvere con dati esterni ancora da rendere disponibili
						} else if (LogicTools.isLastSetFromExternalData(dynamicFieldSubSetting.entityDynamicFieldSetting.getSetMode()) 
							   &&  dynamicFieldSubSetting.isWaitingForExternalData) {
							
							// Su db solo la prima volta
							if (!dynamicFieldSubSetting.isDbUpdated) {
								adbi.addObjEntityDynamicFieldSubWaitExt(dynamicFieldSubSetting.entityDynamicFieldSubWaitExt);
							}
							
							isWaitingForData = dynamicFieldSub.isWaitingForExternalData;
							if (isWaitingForData) {isSolvedFull = false;}							
							dynamicFieldSub.entityDynamicFieldSub.setSolved(isSolved);
							dynamicFieldSub.entityDynamicFieldSub.setWaitingForData(isWaitingForData);;
							dynamicFieldSub.isSolved = isSolved;
							dynamicFieldSub.isWaitingForExternalData = isWaitingForData;
						}						
							
						// Su db solo la prima volta
						if (!dynamicFieldSubSetting.isDbUpdated) {
							dynamicFieldSubSetting.entityDynamicFieldSetting.setProgr(progrSet);
							adbi.addObjEntityDynamicFieldSubSetting(dynamicFieldSubSetting.entityDynamicFieldSetting);
							dynamicFieldSubSetting.isDbUpdated = true;
							updateLastSetTotalProgr(dynamicFieldSub, dynamicFieldSubSetting.entityDynamicFieldSetting);
						}
						
						progrSet++;
					}				
				}
				
			} // end-for subfield

			// Update flag a livello di campo, utilizzati per aggiornare anche lo stato dell'istruzione
			// Se attivazione a fronte di LogicSamePgm è presente un oggetto DynamicField da inserire
			// Se attivazione a fronte di LogicSpreaded si inserisce lo statement di update flags X fine elaborazione
			if (iDbdynamicField >= 0) {
				entityDynamicField = adbi.al_DbDynamicField.get(iDbdynamicField);
				entityDynamicField.setLight(isLight);
				entityDynamicField.setSolved(isSolved);
				entityDynamicField.setSolvedFull(isSolvedFull);
				entityDynamicField.setSpreaded(isSpreaded);
				entityDynamicField.setWaitingForData(isWaitingForData);
			} else {
				insertUpdateForFieldFlags(adbi, logicDynamicInstr.dynamicInstr.getNumInstr() , fieldName, isLight, isSolved, isSolvedFull, isSpreaded, isWaitingForData);
			}
	
			// Gestione inserimento valori a livello di campo
			
			// Campo elementare: valori già agganciati al sottocampo di servizio a spaces.
			// I valori finali combinati a fronte di assegnazioni parziali sono già stati inseriti 
			if (dynamicField.al_FieldSub.size() <= 1) {
				continue;
			}
			
			/*
			// Scan valori campo X inserimento db con sottocampo di servizio valorizzato a spaces
			entityDynamicFieldSubFirst = dynamicField.al_FieldSub.get(0).entityDynamicFieldSub;
			progrValue = 0;
			for (String valueField : dynamicField.al_valuesField) {
				isSolved = true;
				
				entityDynamicValue = new EntityDynamicFieldSubValue();
				
				entityDynamicValue.setSystem(entityDynamicFieldSubFirst.getSystem());
				entityDynamicValue.setSubSystem(entityDynamicFieldSubFirst.getSubSystem());
				entityDynamicValue.setIdObject(entityDynamicFieldSubFirst.getIdObject());
				entityDynamicValue.setTypeObject(entityDynamicFieldSubFirst.getTypeObject());
				entityDynamicValue.setNumInstr(entityDynamicFieldSubFirst.getNumInstr());
				entityDynamicValue.setIdField(entityDynamicFieldSubFirst.getIdField());
				entityDynamicValue.setIdSubField("");               // Di servizio X rappresentare tutto il campo
				entityDynamicValue.setProgr(progrValue++);	
				
				entityDynamicValue.setIdPgmFrom("");
				entityDynamicValue.setTypeObjectFrom(EnumObject.NOT_ASSIGNED);
				entityDynamicValue.setPosInSubField(0);
				entityDynamicValue.setLngInSubField(0);
				entityDynamicValue.setValue(valueField);
				
				adbi.addObjEntityDynamicFieldSubValue(entityDynamicValue);
				
			} // end-for values	
			*/
			
		} // end-for field
		
		// Aggiornamento flag in istruzione istruzione che verrà serializzata in ProgramCobol
		// Nel caso di attivazione logiche stesso programma i flag del campo e dell'istruzione sono allineati
		logicDynamicInstr.getDynamicInstr().setDynamicLight(isLight);
		logicDynamicInstr.getDynamicInstr().setDynamicSolved(isSolved);
		logicDynamicInstr.getDynamicInstr().setDynamicSolvedFull(isSolvedFull);
		logicDynamicInstr.getDynamicInstr().setDynamicSpreaded(isSpreaded);
		logicDynamicInstr.getDynamicInstr().setDynamicWaitingForData(isWaitingForData);
		
		return;
	}

	/*
	 * Update progressivo ultima assegnazione in struttura cimulativa lastSetTotal a livello di sottocampo
	 * con le ultime assegnazioni di tutte le catene di trasformazione
	 */
	private void updateLastSetTotalProgr(LogicDynamicFieldSub dynamicFieldSub, EntityDynamicFieldSubSetting lastSetInChain) {
		EntityDynamicFieldSubSetting lastSetTotal = null;
		
		// Scan lastSetTotal
		for (LogicDynamicFieldSubSetting logicLastSetTotal : dynamicFieldSub.al_lastSetTotal) {
			lastSetTotal = logicLastSetTotal.entityDynamicFieldSetting;
			if (lastSetTotal.getIdObject().equals(lastSetInChain.getIdObject())
			||  lastSetTotal.getIdField().equals(lastSetInChain.getIdField())
			||  lastSetTotal.getIdSubField().equals(lastSetInChain.getIdSubField())
			||  lastSetTotal.getIdPgmSet().equals(lastSetInChain.getIdPgmSet())
			||  lastSetTotal.getNumInstr() == lastSetInChain.getNumInstr()
			||  lastSetTotal.getNumInstrSet() == lastSetInChain.getNumInstrSet()
			||  lastSetTotal.getNumChain() == lastSetInChain.getNumChain()) {
				lastSetTotal.setProgr(lastSetInChain.getProgr());
			}
		}
		
	}




	/* ----------------------------------------------
	 * Inserimento Sql Update per fine elaborazione
	 * ----------------------------------------------
	 */
    private void insertUpdateForFieldFlags(AnalyzerDbInfo adbi, int numInstr, String fieldName, boolean isLight, boolean isSolved, boolean isSolvedFull, boolean isSpreaded, boolean isWaitingForData) {
    	String strSql = "";
    	    	
		strSql = "UPDATE DynamicField" +
				 "    SET light = "        + isLight + "," +
				 "        solved = "       + isSolved + "," +
				 "        solvedFull = "   + isSolvedFull + "," +
				 "        spreaded = "     + isSpreaded + "," +
				 "        waitingForData = "   + isWaitingForData +
				 "  WHERE sys = '"   + program.sysOwner + "'" +
		         "   AND subSys = '" + program.subSysOwner + "'" +
		         "   AND idObject = '" + program.programName + "'" +
		         "   AND typeObject = " + EnumObject.OBJECT_PGM_COBOL.ordinal() +
		         "   AND numInstr = " + numInstr  +  
		         "   AND idField = '" + fieldName  + "'" +
		         "    ";

		adbi.addSqUpdateStatement(strSql);
	}




	/*
     * true Se l'operando è un campo non movimentato con value il campo e poi l'istruzione,
     * viene classificata come dynamicLight
     */
	private boolean isDynamicLight(LogicDynamicField dynamicField) {
		
		// Se operando dinamico non movimentato con value
		// - viene generato un solo valore
		// - Il campo è un campo elementare
		// - Non ci sono catene di trasformazione
		if (dynamicField.getValuesField().size() == 1 
		&& dynamicField.al_FieldSub.size() == 1
		&& dynamicField.al_FieldSub.get(0).al_al_chainSetSubField.size() == 0
		&& this.program.xrefToDataItemInProcedure(dynamicField.dataItemCobolIdentifier.getNumInstr(), INSTR_USE_DATA_ITEM_OUTPUT) == null) {
			return true;
		}
		
		return false;
	}




	/**
     * Richiamata a fronte di processo spreaded da LogicSpreadedPgm
     * 
     * Inserisce le sole informazioni dinamiche relative a:
     * - Assegnazioni specifiche del programma chiamante
     * - Eventuali richieste di dati esterni
     * - Valori generati nel programma chiamante
     *  
     * 
     * @param AnalyzerDbinfo the database object container for finale db updates 
     * @param logicDynamicInstr 
     * @param fieldName
   */
	public void putDatabaseInfoValuesOnly(AnalyzerDbInfo adbi, LogicDynamicInstruction logicDynamicInstr, String fieldName) {
		EntityDynamicFieldSubValue entityDynamicValue = null;
		EntityDynamicFieldSub entityDynamicFieldSubFirst = null;		
		int progrValue = 0;
				
		// Scan fields (X execCics Send|Receive map possono essere 2)
		for (LogicDynamicField dynamicField : logicDynamicInstr.al_dynamicField) {
			
			// Non è il campo richiesto (es in Cancel o Cics Send)
			if (!dynamicField.dataItemCobolIdentifier.getDataItem().getName().equals(fieldName)) {
				continue;
			}
			
			// Scan subfield
			for (LogicDynamicFieldSub dynamicFieldSub : dynamicField.al_FieldSub) {

				// Scan valori sottocampi  X inserimento db
				progrValue = 0;
				for (LogicDynamicValue dynamicFieldSubValue : dynamicFieldSub.al_value) {
					dynamicFieldSubValue.entityDynamicValue.setProgr(progrValue++);
					adbi.addObjEntityDynamicFieldSubValue(dynamicFieldSubValue.entityDynamicValue);
				}
				// Campo elementare: valori già agganciati al sottocampo a spaces
				if (dynamicField.al_FieldSub.size() == 1) {
					continue;
				}
				
				// Scan valori campo X inserimento db con sottocampo di servizio valorizzato a spaces
				entityDynamicFieldSubFirst = dynamicField.al_FieldSub.get(0).entityDynamicFieldSub;
				progrValue = 0;
				for (String valueField : dynamicField.al_valuesField) {
					entityDynamicValue = new EntityDynamicFieldSubValue();
					
					entityDynamicValue.setSystem(entityDynamicFieldSubFirst.getSystem());
					entityDynamicValue.setSubSystem(entityDynamicFieldSubFirst.getSubSystem());
					entityDynamicValue.setIdObject(entityDynamicFieldSubFirst.getIdObject());
					entityDynamicValue.setTypeObject(entityDynamicFieldSubFirst.getTypeObject());
					entityDynamicValue.setNumInstr(entityDynamicFieldSubFirst.getNumInstr());
					entityDynamicValue.setIdField(entityDynamicFieldSubFirst.getIdField());
					entityDynamicValue.setIdSubField("");
					entityDynamicValue.setProgr(progrValue++);
					
					entityDynamicValue.setIdPgmFrom("");
					entityDynamicValue.setTypeObjectFrom(EnumObject.NOT_ASSIGNED);
					entityDynamicValue.setPosInSubField(0);
					entityDynamicValue.setLngInSubField(0);
					entityDynamicValue.setValue(valueField);	
					
					adbi.addObjEntityDynamicFieldSubValue(entityDynamicValue);
				}				
			}		
		}
		
		return;
	}	


	/**
     * 
     * Restituisce uno specifico campo dinamico dell'istruzione da risolvere.<br>
     * <p>
     * Viene restituito un oggetto della classe LogicDynamicField.
     * Se il nome del campo è errato viene restituito null.
     * <p>
     * @param numInstr     
     * @param  dynamicFieldName con nome del campo di gruppo 
     * @return LogicDynamicField
     */
	public LogicDynamicField getDynamicField(int numInstr
									       , String dynamicFieldName
									        ) {
		
		LogicDynamicInstruction idi = null;
		
		idi = getDynamicInstr(numInstr);
		
		// Descrittore istruzione dinamica
		idi = getDynamicInstr(numInstr);
		
		// Scan campi dinamici istruzione già inseriti
		for (LogicDynamicField dynamicField : idi.al_dynamicField) {
			if (dynamicField.entityDynamicField.getIdField().equals(dynamicFieldName)) {
				return dynamicField;
			}
		}
		
		// Campo dinamico non definito
		
		return null;
	}

	/**
     * 
     * Verifica se il campo dinamico in input è stato trasformato in programmi chiamanti <br>
     * nel processo spreaded <br>
     * <p>
     * Si verifica se esistono catene con assegnazioni nel programma corrente diverso da quello origine
     * <p>
     * @param int numInstr
     * @param  String dynamicFieldName
     * @param  String pgmCur
     * @return isFieldSpreaded
     */
	public boolean isDynamicFieldSpreaded(int numInstr, String dynamicFieldName) {
		
		LogicDynamicInstruction idi = null;
		boolean isFieldSpreaded = false;
		
		// Descrittore istruzione dinamica
		idi = getDynamicInstr(numInstr);
		
		// Scan campi dinamici istruzione già inseriti
		for (LogicDynamicField dynamicField : idi.al_dynamicField) {
			if (!dynamicField.entityDynamicField.getIdField().equals(dynamicFieldName)) {continue;};		
			// Verifica se esistono catene con assegnazioni nel programma corrente diverso da quello origine
			// verifica se esistono ultime assegnazioni che hanno generato valori in base a flag in input
			// TODO
		}
					
		return isFieldSpreaded;
	}

	/**
     * 
     * Restituisce i valori assegnati a un campo nel processo di logiche spreaded nel pgm chiamanti <br>
     * <p>
     * @param int numInstr
     * @param  String dynamicFieldName
     * @param  String pgmCur
     * @return isFieldSpreaded
     */
	public ArrayList<String> getDynamicFieldSpreadedValues(int numInstr, String dynamicFieldName) {
		
		LogicDynamicInstruction idi = null;
		ArrayList<String> al_value = null;
		
		al_value = new ArrayList<String>();
		
		// Descrittore istruzione dinamica
		idi = getDynamicInstr(numInstr);
		
		// Scan campi dinamici istruzione già inseriti
		for (LogicDynamicField dynamicField : idi.al_dynamicField) {
			if (!dynamicField.entityDynamicField.getIdField().equals(dynamicFieldName)) {continue;};
			// TODO			
		}
					
		return al_value;
	}


	
	/**
     * 
     * Restituisce i sottocampi elementari del campo dinamico da risolvere.
     * Viene restituito un ArrayList della classe interna LogicDynamicSubField.
     * Nel caso l'istruzione non sia catalogata come dinamica, restituisce
     * un ArrayList vuota. Questo può avvenire in caso di analisi di programma
     * con l'opzione di soluzione codice dinamico disabilitata.
     * 
     * @param int numInstr     
     * @param  String dynamicFieldName con nome del campo di gruppo 
     * @return ArrayList<LogicDynamicSubField>
     */
	public ArrayList<LogicDynamicFieldSub> getDynamicFieldsSub(int numInstr
														     , String dynamicFieldName
														     ) {

		LogicDynamicInstruction idi = null;
		LogicDynamicField idf = null;
		boolean fieldFound = false;
		
		// Descrittore istruzione dinamica
		idi = getDynamicInstr(numInstr);
		
		// Istruzione non catalogata come dinamica
		if (idi == null) {
			return new ArrayList<LogicDynamicFieldSub> ();
		}
		
		// Scan campi dinamici istruzione già inseriti
		for (LogicDynamicField dynamicField : idi.al_dynamicField) {
			if (dynamicField.entityDynamicField.getIdField().equals(dynamicFieldName)) {
				fieldFound = true;
				idf = dynamicField;
				break;
			}
		}
		
		// Campo dinamico non inserito
		if (!fieldFound) {
			return null;
		}
				
		return idf.al_FieldSub;
	}

	/**
     * 
     * Restituisce uno specifico sottocampo elementari del campo dinamico da risolvere.
     * Viene restituito un oggetto della classe interna LogicDynamicSubField.
     * Se il nome del campo è errato viene restituito null.
     * Se il nome del sottocampo è errato viene restituito null.
     * 
     * @param int numInstr     
     * @param  String dynamicFieldName con nome del campo di gruppo 
     * @param  String dynamicSubFieldName con nome del sottocampo
     * @return ArrayList<LogicDynamicSubField>
     */
	public LogicDynamicFieldSub getDynamicFieldSub(int numInstr
											     , String dynamicFieldName
											     , String dynamicSubFieldName
											     ) {
		
		ArrayList<LogicDynamicFieldSub> al_LogicDynamicFieldSub = null;

		// Estrazione di tutti i sottocampi definiti
		al_LogicDynamicFieldSub = getDynamicFieldsSub(numInstr, dynamicFieldName);
		
		// Campo dinamico non presente nell'istruzione
		if (al_LogicDynamicFieldSub == null) {
			return null;
		}
		
		// Scan sottocampi
		for (LogicDynamicFieldSub dynamiFieldcSub : al_LogicDynamicFieldSub) {
			if (dynamiFieldcSub.dataItemIdentifierSubField.getNameIdentifier().equals(dynamicSubFieldName)) {
				return dynamiFieldcSub;
			}
		}
		
		return null;
	}

	
	/**
     * 
     * Restituisce l'ultima impostazione di un sottocampo in una catena di trasformazioni specifica.<br>
     * <p>
     * Se il numero di chain fornita è -1 restituisce l'ultima. <br>
     * In caso di nome campo non inserito restituisce null. <br>
     * In caso di nome sotto campo non inserito restituisce null. <br>
     * In caso di nessuna impostazione presente restituisce null. <br>
     * 
     * @param int numInstr     
     * @param  String dynamicFieldName
     * @param  String dynamicSubFieldName
     * @param  EntityDynamicSubFieldSetting entityDynamicFieldSetting
     * @param  int numChain
     * @return LogicDynamicFieldSubSetting o null se campo/sottocampo/numero chain errati
     */
	public LogicDynamicFieldSubSetting getLastFieldSubSetting(int numInstr
													        , String dynamicFieldName
													        , String dynamicSubFieldName
													        , int numChain
										 					) {

		LogicDynamicInstruction idi = null;
		LogicDynamicField idf = null;
		LogicDynamicFieldSub idsf = null;
		ArrayList<LogicDynamicFieldSubSetting> al_LogicDynamicSubFieldSetting = null;
		
		boolean fieldFound = false;
		boolean subFieldFound = false;
		
		
		// Descrittore istruzione dinamica
		idi = getDynamicInstr(numInstr);
		
		// Istruzione non definita
		if (idi == null) {
			return null;
		}
		
		// Scan campi dinamici istruzione già inseriti
		for (LogicDynamicField dynamicField : idi.al_dynamicField) {
			if (dynamicField.entityDynamicField.getIdField().equals(dynamicFieldName)) {
				fieldFound = true;
				idf = dynamicField;
				break;
			}
		}
		
		// Campo dinamico non inserito
		if (!fieldFound) {
			return null;
		}
		
		// Scan sotto campi già inseriti per il campo
		for (LogicDynamicFieldSub dynamicSubField : idf.al_FieldSub) {
			if (dynamicSubField.dataItemIdentifierSubField.getNameIdentifier().equals(dynamicSubFieldName)) {
				subFieldFound = true;
				idsf = dynamicSubField;
				break;
			}
		}

		// Sotto Campo non inserito
		if (!subFieldFound) {
			return null;
		}
		
		// Nessuna impostazione presente
		if (idsf.al_al_chainSetSubField.size() == 0) {
			return null;
		}
		
		// Numero catena di trasformazione di cui estrarre l'ultima assegnazione
		if (numChain > idsf.al_al_chainSetSubField.size() - 1) {
			return null;
		}
		
		// Restituisco ultima trasformazione per il numero di catena fornito
		al_LogicDynamicSubFieldSetting = idsf.al_al_chainSetSubField.get(numChain);
		return al_LogicDynamicSubFieldSetting.get(al_LogicDynamicSubFieldSetting.size() - 1);
	}
			
	
	/**
	 * 
	 * Restituisce true se l'istruzione dinamica il cui numero è fornito in input,
	 * è completamente risolta in ogni suo sottocampo di ogni operando, in tutti i
	 * path analizzati.
	 * 
	 * 
	 * @return boolean the dynamicSolvedFull
	 */
	public boolean isDynamicSolvedFull(int numInstr) {
		LogicDynamicInstruction idi = null;		
	    idi = getDynamicInstr(numInstr);
		return idi.getDynamicInstr().isDynamicSolvedFull();
	}

	/**
	 * 
	 * Imposta l'istruzione dinamica il cui numero è fornito in input a full solved
	 * come completamente risolta in ogni suo sottocampo di ogni operando, in tutti i
	 * path analizzati.
	 * 
	 * 
	 * @return boolean the dynamicSolvedFull
	 */
	public void setDynamicSolvedFull(int numInstr) {
		LogicDynamicInstruction idi = null;		
	    idi = getDynamicInstr(numInstr);
		idi.getDynamicInstr().setDynamicSolvedFull(true);
		return;
	}

	
	/**
	 * Restituisce true se l'istruzione è già presente 
	 * 
	 * @return boolean the dynamicSolvedFull
	 */
	public boolean isDynamicInstr(int numInstr) {

		if (numInstr < this.al_DynamycInstr.size()) {
			return true;
		}
		
		return false;
	}

	


	/**
	 * Get programma serializzato
	 * 
	 * @return the program
	 */
	public ProgramCobol getProgram() {
		return program;
	}

	/**
	 * Set programma serializzato
	 * 
	 * @param program the program to set
	 */
	public void setProgram(ProgramCobol program) {
		this.program = program;
	}




	/**
	 *   Classe contenitore di servizio con le informazioni
	 *   di assegnazione del sottocampo nei programmi chiamanti.<br>
	 *   <p>
	 *   Ogni istanza identifica un programma chiamante, gli estremi
	 *   dell'istruzione target di chiamata (call, link, ..), tutte
	 *   le informazioni sulla nuova ricerca e il descrittore corrente
	 *   {@link LogicDynamicFieldSub}
	 */
	public class LogicDynamicSubFieldSpreaded implements Serializable {
		
		private static final long serialVersionUID = 1L;

		// Programma chiamante a cui il sottocampo appartiene.
		ProgramCobol program = null;
		
		// Info su nuovo campo in pgm chiamante di cui trovare i valori
        LogicDynamicFieldSub subFieldCaller = null;
        
        // Numero istruzione chiamante.
        // Verranno cercati valori fra la prima istruzione del programma e questa
        int numInstrCaller = 0;
        
        
		/* Costruttore vuoto */
		public LogicDynamicSubFieldSpreaded() {
		}

	}


}
