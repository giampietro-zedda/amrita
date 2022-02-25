package entities;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import analyzer.DataBaseMappedColumns;
import analyzer.DataBaseMappedTable;
import enums.EnumMetricsScope;
import enums.EnumMetricsSqualeRating;
import enums.EnumObject;

/**
	 * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
	 *
	 * <h1>
	 * EntityMetricValue (MetricValue, METR) 
	 * </h1>
	 *  <p>
	 * Questa classe descrive l'entity EntityMetric, ovvero la tabella oggetti METR che contiene le metriche a livello di
	 * programma e di sistema/sottosistema.
	 * 
	 * 
	 * @author Giampietro Zedda
	 * @version 1.0.0	
	 * @since 12/03/2010
	 * @see Analyzer
	 * @see EntityObject 
	 * @see EntityObjectOption
	 * @see EntityIndexItem
	 * @see EntityRelation
	 * @see EntityRelationOrigin
	 * @see EntityCopyEntityDefinition
	 * @see EntityWhereUsedItem
	 * @see EntityMapDescriptor
	 * @see EntityMapItem
	 * @see EntityTagValue
	 * @see EntityDynamicValueExt
	 * @see EntityScopeHeader
	 * @see EntityScopeSection
	 * @see EntityScopeItem
	 * @see EntityScopeChild
	 * @see EntityScopeProgram
	 * @see EntityScopeObject
	 * @see EntityScopeRelation
	 * @see EntityProcessLog
	 * @see EntityMetric
	 * @see EntityTableHeader    
	 * @see EntityTableStructure   
	 * @see EntityTableData 
	 * @see EntityDynamicField
	 * @see EntityDynamicFieldSub
	 * @see EntityDynamicFieldSubValue
	 * @see EntityDynamicFieldSubSetting
*/

@DataBaseMappedTable("MetricValue")
@DataBaseMappedColumns(java_db_col = {  
		                        // Colonne primary key
						         "system 			  		 sys                          PK"
                                ,"subSystem 			  	 subSys                       PK"    
                                ,"scope 			  	     scope                        PK"    
							    ,"idObject 			  		 idObject                     PK"    
							    ,"typeObject 		  		 typeObject                   PK"  
							    ,"section 		  		     section                      PK"  
							    // Colonne senza vincoli di chiave (No key)
                                ,"sectionThru                sectionThru                  NK" 
							    // Misure di conteggio sorgenti
							    ,"cntPgmAnalyzed 	  		 cntPgmAnalyzed               NK" 
 								,"cntPgmExecuted          	 cntPgmExecuted               NK"
								,"cntSubPgmAnalyzed   		 cntSubPgmAnalyzed            NK"
								,"cntSubPgmExecuted  		 cntSubPgmExecuted            NK"
								,"cntCopyDefined          	 cntCopyDefined               NK"
								,"cntJclJob  				 cntJclJob                    NK"
								,"cntJclInclude 			 cntJclInclude                NK"
								,"cntJclProc  				 cntJclProc                   NK"
								// Misure dimensionali sorgenti
								,"sizeLinesCodeLogical  	 sizeLinesCodeLogical         NK"
								,"sizeLinesCodePhisical 	 sizeLinesCodePhisical        NK"
								,"sizeLinesBlank          	 sizeLinesBlank               NK"
								,"sizeLinesBlankProc      	 sizeLinesBlankProc           NK"
								,"sizeLinesBlankData      	 sizeLinesBlankData           NK"
								,"sizeLinesComment 			 sizeLinesComment             NK"
								,"sizeLinesCommentProc  	 sizeLinesCommentProc         NK"
								,"sizeLinesCommentData 		 sizeLinesCommentData         NK"
								,"sizeInstr  				 sizeInstr                    NK"
								// Misure stimate con sizeLinesCodeLogical
								,"backFiredFunctionPoint     backFiredFunctionPoint       NK"
								,"timeDevelopment     		 timeDevelopment              NK"
								// Misure definizione dati
								,"defFields 				 defFields                    NK"
								,"defFieldsInCopy  			 defFieldsInCopy              NK"
								,"defLiterals  				 defLiterals                  NK"
								// Misure di documentazione
								,"percComByLogical 			 percComByLogical             NK"
								,"percComByPhisical 	     percComByPhisical            NK"
								,"percComByInstruction 		 percComByInstruction         NK"
								,"percBlankByPhisical 		 percBlankByPhisical          NK"
								,"percBlankByInstruction 	 percBlankByInstruction       NK"
								// Misure di codice dinamico
								,"dynamicPgm                 dynamicPgm                   NK"
								,"dynamicInstr               dynamicInstr                 NK"
								,"dynamicInstrLight          dynamicInstrLight            NK"
								,"percDynamicInstr           percDynamicInstr             NK"
								,"percDynamicInstrLight      percDynamicInstrLight        NK"
								// Misure violazioni
								,"violations                  violations                  NK"
								,"percViolationsByLogical     percViolationsByLogical     NK"
								,"percViolationsByPhisical    percViolationsByPhisical    NK"
								,"percViolationsByInstruction percViolationsByInstruction NK"
								// Misure di codice morto
								,"deadFields 				 deadFields                   NK"
								,"deadSubGraph 				 deadSubGraph                 NK"
								,"deadInstr 				 deadInstr                    NK"
								,"deadLabels 				 deadLabels                   NK"
								,"deadCopyData  			 deadCopyData                 NK"
								,"deadCopyProc 				 deadCopyProc                 NK"
								// Misure di jcl
								,"jclDD 					 jclDD                        NK"
								,"jclStepDefined          	 jclStepDefined               NK"
								,"jclStepUpdate 			 jclStepUpdate                NK"
								,"jclDsname  				 jclDsname                    NK"
								,"jclDsnameReferenced 		 jclDsnameReferenced          NK"
								,"jclDsnameUnReferenced  	 jclDsnameUnReferenced        NK"
								,"jclIncludeCalled  		 jclIncludeCalled             NK"
								,"jclProcCalled  			 jclProcCalled                NK"
								// Misure di complessità strutturale
								,"structFanIn  				 structFanIn                  NK"
								,"structFanOut  			 structFanOut                 NK"
								,"structSections  			 structSections               NK"
								,"structParagraphs  	     structParagraphs             NK"
								// Misure di complessità funzionale generiche
								,"funcObjects 			     funcObjects                  NK"
								,"funcRelations 			 funcRelations                NK"
								,"funcTranInternal  		 funcTranInternal             NK"
								,"funcTranExternal  		 funcTranExternal             NK"
								,"funcMap  					 funcMap                      NK"
								,"funcCallInternal  		 funcCallInternal             NK"
								,"funcCallExternal  		 funcCallExternal             NK"
								,"funcAccEntityInternal  	 funcAccEntityInternal        NK"
								,"funcAccEntityExternal  	 funcAccEntityExternal        NK"
								,"funcAccMediaInternal  	 funcAccMediaInternal         NK"
								,"funcAccMediaExternal  	 funcAccMediaExternal         NK"
								// Misure di complessità funzionale Function Point
								,"fpExternalOutputEO  	     fpExternalOutputEO           NK"
								,"fpExternalInputEI  	     fpExternalInputEI            NK"
								,"fpExternalInquiryEQ  	     fpExternalInquiryEQ          NK"
								,"fpInternalLogicalFileILF   fpInternalLogicalFileILF     NK"
								,"fpExternalInterfaceFileEIF fpExternalInterfaceFileEIF   NK"
								// Misure di complessità funzionale/tecnica per rehosting 
								,"rhRateObjectRelation       rhRateObjectRelation         NK"
								,"rhObjectsInternal          rhObjectsInternal            NK"
								,"rhObjectsExternal          rhObjectsExternal            NK"
								,"rhObjectsUnportable        rhObjectsUnportable          NK"
								,"rhFilesBynary              rhFilesBynary                NK"
								// Misure di complessità ciclomatica
								,"mcCabeArcs  				 mcCabeArcs                   NK"
								,"mcCabeNodes 				 mcCabeNodes                  NK"
								,"mcCabeGraphConn 		     mcCabeGraphConn              NK"
								,"mcCabeOperatorsOrAnd 		 mcCabeOperatorsOrAnd         NK"
							    // Misure di complessità di Halstead (o Software Science)
								,"halsteadOperators 		 halsteadOperators            NK"
								,"halsteadOperands  		 halsteadOperands             NK"
								,"halsteadOperatorsOcc  	 halsteadOperatorsOcc         NK"
								,"halsteadOperandsOcc  	     halsteadOperandsOcc          NK"
								,"halsteadLengthPgm  		 halsteadLengthPgm            NK"
								,"halsteadVocabularyPgm  	 halsteadVocabularyPgm        NK "
								,"halsteadVolumePgm  		 halsteadVolumePgm            NK"
								,"halsteadDifficultPgm  	 halsteadDifficultPgm         NK"
								,"halsteadEffortPgm 		 halsteadEffortPgm            NK"
								,"halsteadTimeWriting 		 halsteadTimeWriting          NK"
								// Indici di complessita/manutenibilità medi 
								,"idxMIAvg 					 idxMIAvg                     NK"
								,"idxFPAvg 					 idxFPAvg                     NK"
								,"idxMcCabeAvg 				 idxMcCabeAvg                 NK"
								,"idxReHostingAvg 			 idxReHostingAvg              NK"
								// Indici di complessita/manutenibilità massimi 
								,"idxMIHigh					 idxMIHigh                    NK"
								,"idxFPHigh					 idxFPHigh                    NK"
								,"idxMcCabeHigh				 idxMcCabeHigh                NK"
								,"idxReHostingHigh			 idxReHostingHigh             NK"
								// Indici di complessita/manutenibilità minimi
								,"idxMILow					 idxMILow                     NK"
								,"idxFPLow					 idxFPLow                     NK"
								,"idxMcCabeLow				 idxMcCabeLow                 NK"
								,"idxReHostingLow			 idxReHostingLow              NK"
								// Indici di complessita/manutenibilità totali 
								,"idxMITot					 idxMITot                     NK"
								,"idxFPTot					 idxFPTot                     NK"
								,"idxMcCabeTot				 idxMcCabeTot                 NK"
								,"idxReHostingTot			 idxReHostingTot              NK"
								// Sistema di qualità SQUALE, numero violazioni per categoria gravità
								,"squaleViolationsBlocker    squaleViolationsBlocker      NK"
								,"squaleViolationsCritical   squaleViolationsCritical     NK"
								,"squaleViolationsMajor      squaleViolationsMajor        NK"
								,"squaleViolationsMinor      squaleViolationsMinor        NK"
								,"squaleViolationsInfo       squaleViolationsInfo         NK"
								// Sistema di qualità SQUALE, valori generali
								,"squaleSSRL                 squaleSSRL                   NK"
								,"squaleSSRI                 squaleSSRI                   NK"
								,"squaleSSCI                 squaleSSCI                   NK"
								,"squaleSSQI                 squaleSSQI                   NK"
								// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SQI, SQxI
								,"squaleSQTI                 squaleSQTI                   NK"
								,"squaleSQRI                 squaleSQRI                   NK"
								,"squaleSQCI                 squaleSQCI                   NK"
								,"squaleSQEI                 squaleSQEI                   NK"
								,"squaleSQSI                 squaleSQSI                   NK"
								,"squaleSQMI                 squaleSQMI                   NK"
								,"squaleSQPI                 squaleSQPI                   NK"
								// Sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
								,"squaleSCTI                 squaleSCTI                   NK"
								,"squaleSCRI                 squaleSCRI                   NK"
								,"squaleSCCI                 squaleSCCI                   NK"
								,"squaleSCEI                 squaleSCEI                   NK"
								,"squaleSCSI                 squaleSCSI                   NK"
								,"squaleSCMI                 squaleSCMI                   NK"
								,"squaleSCPI                 squaleSCPI                   NK"
								// Sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI
								,"squaleSDTI                 squaleSDTI                   NK"
								,"squaleSDRI                 squaleSDRI                   NK"
								,"squaleSDCI                 squaleSDCI                   NK"
								,"squaleSDEI                 squaleSDEI                   NK"
								,"squaleSDSI                 squaleSDSI                   NK"
								,"squaleSDMI                 squaleSDMI                   NK"
								,"squaleSDPI                 squaleSDPI                   NK"
								// Sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
								,"squaleSRTI                 squaleSRTI                   NK"
								,"squaleSRRI                 squaleSRRI                   NK"
								,"squaleSRCI                 squaleSRCI                   NK"
								,"squaleSREI                 squaleSREI                   NK"
								,"squaleSRSI                 squaleSRSI                   NK"
								,"squaleSRMI                 squaleSRMI                   NK"
								,"squaleSRPI                 squaleSRPI                   NK"
								// Sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 
								,"squaleSRTL                 squaleSRTL                   NK"
								,"squaleSRRL                 squaleSRRL                   NK"
								,"squaleSRCL                 squaleSRCL                   NK"
								,"squaleSREL                 squaleSREL                   NK"
								,"squaleSRSL                 squaleSRSL                   NK"
								,"squaleSRML                 squaleSRML                   NK"
								,"squaleSRPL                 squaleSRPL                   NK"
								}
         )

		 /*
@DataBaseMappedForEachs(forEach = {@DataBaseMappedForEach(entity = "EntityMetricViolation",
									colsBound = {"system     sys"
										  	   , "subSystem  subSys"    
											   , "idObject   idObject"    
											   , "typeObject typeObject"  
											   , "section    section"  
											  }
										)
									} 
						)

@DataBaseMappedRuleTables(tablesRule = { @DataBaseMappedRuleTable(tabRef = "A", tabId = "1", tabSystem = true, tabNumCol = "numTableT001", tabKeyColsBound={"typeObject"})
									   , @DataBaseMappedRuleTable(tabRef = "B", tabId = "41",tabSystem = true, tabNumCol = "numTableT041", tabKeyColsBound={"squaleSSRL"})                                       
									   , @DataBaseMappedRuleTable(tabRef = "C", tabId = "45",tabSystem = true, tabNumCol = "numTableT045", tabKeyColsBound={"scope"})                                       
                                       }
                         ) 
*/                          
@Entity(name="MetricValue")
public class EntityMetricValue {

	///////////////////////////////////////////////////////////////////////
    // Data Items Metric                                                                                                     
    ///////////////////////////////////////////////////////////////////////
	
	// Primary key
	@Id
    @Column(name="sys")
	private String system = "";            			// METRSYST(PK) Sistema applicativo
	@Id
    @Column(name="subSys")
	private String subSystem = "";         			// METRSUBS(PK) Sotto sistema applicativo
	@Id
    @Column(name="scope")
	private EnumMetricsScope scope = null;			// METRSCOP(PK) Livello di aggregazione codificato  (T045)
	@Id
    @Column(name="idObject")
	private String idObject = "";          			// METRIDOB(PK) Nome oggetto (es. programma, Copy, Jcl, ..)
	@Id
    @Column(name="typeObject")
	private EnumObject typeObject = null;       	// METRTYPO(PK) Tipologia oggetto (T0001)
	@Id
    @Column(name="section")
	private String section = "";                	// METRSECT(PK) Program section di aggregazione se oggetto programma 
                                                    //              * per intero programma 
	
	// Data
    @Column(name="sectionThru")
	private String sectionThru = "";                // METRSTHR     Section Thru se oggetto programma
	
	// Misure di conteggio sorgenti
    @Column(name="cntPgmAnalyzed")
	private long  cntPgmAnalyzed = 0;           	// METRCPGA     Numero programmi analizzati
    @Column(name="cntPgmExecuted")
	private long  cntPgmExecuted = 0;           	// METRCPGE     Numero programmi eseguiti da Exec batch o da Cics Link/Xctl
    @Column(name="cntSubPgmAnalyzed")
	private long  cntSubPgmAnalyzed = 0;        	// METRCSPA     Numero sottoprogrammi analizzati
    @Column(name="cntSubPgmExecuted")
	private long  cntSubPgmExecuted = 0;        	// METRCSPE     Numero sottoprogrammi richiamati con Call
    @Column(name="cntCopyDefined")
	private long  cntCopyDefined = 0;           	// METRCCPD     Numero copy definiti
    @Column(name="cntJclJob")
	private long  cntJclJob = 0;                	// METRCJCJ     Numero sources contenenti jcl job
    @Column(name="cntJclInclude")
	private long  cntJclInclude = 0;            	// METRCJCI     Numero sources contenenti jcl include
    @Column(name="cntJclProc")
	private long  cntJclProc = 0;               	// METRCJCP     Numero sources contenenti jcl proc
	
	// Misure dimensionali sorgenti
    @Column(name="sizeLinesCodeLogical")
	private long  sizeLinesCodeLogical = 0;     	// METRSLCL     Numero linee di codice logiche, includenti istruzioni, senza commenti e righe a blank
    @Column(name="sizeLinesCodePhisical")
	private long  sizeLinesCodePhisical = 0;    	// METRSLCP     Numero linee di codice fisiche, includenti istruzioni, commenti e righe a blank
    @Column(name="sizeLinesBlank")
	private long  sizeLinesBlank = 0;           	// METRSLCB     Numero linee a blank
    @Column(name="sizeLinesBlankProc")
	private long  sizeLinesBlankProc = 0;       	// METRSLPB     Numero linee a blank in procedure division
    @Column(name="sizeLinesBlankData")
	private long  sizeLinesBlankData = 0;       	// METRSLDB     Numero linee a blank in data division
    @Column(name="sizeLinesComment")
	private long  sizeLinesComment = 0;         	// METRSLCC     Numero linee di commento
    @Column(name="sizeLinesCommentProc")
	private long  sizeLinesCommentProc = 0;     	// METRSLCI     Numero linee di commento in procedure division
    @Column(name="sizeLinesCommentData")
	private long  sizeLinesCommentData = 0;     	// METRSLCD     Numero linee di commento in data division
    @Column(name="sizeInstr")
	private long  sizeInstr = 0;                	// METRSINS     Numero istruzioni in procedure division

	// Misure stimate tempi di sviluppo con sizeLinesCodeLogical
    @Column(name="backFiredFunctionPoint")
	private double backFiredFunctionPoint = 0d;  	// METRBFFP     Function point stimati in base al numero logico di loc (sizeLinesCodeLogical)
    @Column(name="timeDevelopment")
	private double timeDevelopment = 0d;         	// METRTMDV     Tempo di sviluppo in giorni stimato in base alla produttività media giornaliera

	// Misure definizione dati
    @Column(name="defFields")
	private long  defFields = 0;                	// METRDFLD     Numero campi definiti
    @Column(name="defFieldsInCopy")
	private long  defFieldsInCopy = 0;          	// METRDFCP     Numero campi definiti dentro moduli copy
    @Column(name="defLiterals")
	private long  defLiterals = 0;              	// METRDLIT     Numero literal definite

	// Misure di documentazione
    @Column(name="percComByLogical")
	private double percComByLogical = 0d;         	// METRPCML    % commenti proc rispetto alle righe sorgente logiche, con istruzioni
    @Column(name="percComByPhisical")
	private double percComByPhisical = 0d;        	// METRPCMP    % commenti proc rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
    @Column(name="percComByInstruction")
	private double percComByInstruction = 0d;    	// METRPCMI    % commenti proc per istruzione
    @Column(name="percBlankByPhisical")
	private double percBlankByPhisical = 0d;      	// METRPBKP    % righe a blank rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
    @Column(name="percBlankByInstruction")
	private double percBlankByInstruction = 0d;   	// METRPBKI    % righe a blank per istruzione

	// Misure di codice dinamico
    @Column(name="dynamicPgm")
	private long dynamicPgm = 0;                 	// METRDPGM    Numero di programmi con codice dinamico
    @Column(name="dynamicInstr")
	private long dynamicInstr = 0;               	// METRDINT    Numero istruzioni dinamiche totali
    @Column(name="dynamicInstrLight")
	private long dynamicInstrLight = 0;          	// METRDINL    Numero istruzioni dinamiche light, con soli campi con value
    @Column(name="percDynamicInstr")
	private double percDynamicInstr = 0d;         	// METRPDIT    % istruzioni dinamiche su istruzioni totali
    @Column(name="percDynamicInstrLight")
	private double percDynamicInstrLight = 0d;    	// METRPDLT    % istruzioni dinamiche light su istruzioni totali
	
	// Misure violazioni generali (dettagliate solo per Squale)
    @Column(name="violations")
	private long violations = 0;              	 	// METRVCNT    Numero di violazioni rilevate
    @Column(name="percViolationsByLogical")
	private double percViolationsByLogical = 0d;  	// METRVPBL    % Violazioni rispetto alle righe sorgente logiche, con istruzioni
    @Column(name="percViolationsByPhisical")
	private double percViolationsByPhisical = 0d;  	// METRVPBP    % Violazioni rispetto alle righe sorgente fisiche, con istruzioni, commenti e righe blank
    @Column(name="percViolationsByInstruction")
	private double percViolationsByInstruction = 0d;// METRVPBI    % Violazioni per istruzione

	// Misure di codice morto
    @Column(name="deadFields")
	private long  deadFields = 0;               	// METRXFLD     Numero campi definiti non utilizzati
    @Column(name="deadSubGraph")
	private long  deadSubGraph = 0;             	// METRXSBG     Numero sottografi sconnessi non richiamati, in cobol si tratta di section non referenziate
    @Column(name="deadInstr")
	private long  deadInstr = 0;                	// METRXINS     Numero istruzioni definite e non referenziate (includono eventuali label)
    @Column(name="deadLabels")
	private long  deadLabels = 0;               	// METRXLAB     Numero label definite e non referenziate
    @Column(name="deadCopyData")
	private long  deadCopyData = 0;             	// METRXCPD     Numero copy definiti in data division e non utilizzati
    @Column(name="deadCopyProc")
	private long  deadCopyProc = 0;             	// METRXCPP     Numero copy definiti in proc division e non utilizzati

	// Misure di jcl
    @Column(name="jclDD")
	private long  jclDD = 0;                    	// METRJDDN     Numero di DD definite nel jcl
    @Column(name="jclStepDefined")
	private long  jclStepDefined = 0;           	// METRJSTD     Numero di step definiti nel jcl
    @Column(name="jclStepUpdate")
	private long  jclStepUpdate = 0;            	// METRJSTU     Numero step in aggiornamento
    @Column(name="jclDsname")
	private long  jclDsname = 0;                	// METRJDSN     Numero dsname dichiarati
    @Column(name="jclDsnameReferenced")
	private long  jclDsnameReferenced = 0;      	// METRJDSR     Numero dsname dichiarati e referenziati dai programmi
    @Column(name="jclDsnameUnReferenced")
	private long  jclDsnameUnReferenced = 0;    	// METRJDSU     Numero dsname dichiarati e NON referenziati dai programmi
    @Column(name="jclIncludeCalled")
	private long  jclIncludeCalled = 0;         	// METRJINC     Numero include richiamate
    @Column(name="jclProcCalled")
	private long  jclProcCalled = 0;            	// METRJPRC     Numero proc richiamate
	

	// Misure di complessità strutturale e tecnica
    @Column(name="structFanIn")
	private long  structFanIn = 0;              	// METRSFAI     Numero programmi chiamanti con Call, Cics Link, Cics Xctl
    @Column(name="structFanOut")
	private long  structFanOut = 0;             	// METRSFAO     Numero programmi chiamati con Call, Cics Link, Cics Xctl
    @Column(name="structSections")
	private long  structSections = 0;           	// METRSSEC     Numero section nel programma
    @Column(name="structParagraphs")
	private long  structParagraphs = 0;         	// METRSPAR     Numero paragrafi nel programma
	
	// Misure di complessità funzionale generiche
    @Column(name="funcObjects")
	private long  funcObjects = 0;              	// METRFOBJ     Numero oggetti
    @Column(name="funcRelations")
	private long  funcRelations = 0;            	// METRFREL     Numero relazioni fra oggetti
    @Column(name="funcTranInternal")
	private long  funcTranInternal = 0;         	// METRFTRI     Numero transazioni interne richiamate con Exec Cics Start o Exec Cics Return Transid
    @Column(name="funcTranExternal")
	private long  funcTranExternal = 0;         	// METRFTRE     Numero transazioni esterne richiamate con Exec Cics Start o Exec Cics Return Transid
    @Column(name="funcMap")
	private long  funcMap = 0;                  	// METRFMAP     Numero mappe video utilizzate
    @Column(name="funcCallInternal")
	private long  funcCallInternal = 0;         	// METRFCAI     Numero call a moduli interni
    @Column(name="funcCallExternal")
	private long  funcCallExternal = 0;         	// METRFCAE     Numero call a moduli esterni
    @Column(name="funcAccEntityInternal")
	private long  funcAccEntityInternal = 0;    	// METRFAEI     Numero accessi a entity (tabelle db) interni
    @Column(name="funcAccEntityExternal")
	private long  funcAccEntityExternal = 0;    	// METRFAEE     Numero accessi a entity (tabelle db) esterni
    @Column(name="funcAccMediaInternal")
	private long  funcAccMediaInternal = 0;     	// METRFAMI     Numero accessi a files sequenziali/Vsam/code ts/.. interni
    @Column(name="funcAccMediaExternal")
	private long  funcAccMediaExternal = 0;     	// METRFAME     Numero accessi a files sequenziali/Vsam/code ts/.. esterni

	// Misure di complessità funzionale Function Point
    @Column(name="fpExternalOutputEO")
	private long  fpExternalOutputEO = 0;        	// METRPXEO     Funzionalità utente (transazione o job) con output generati da un ILF o EIF
    @Column(name="fpExternalInputEI")
	private long  fpExternalInputEI = 0;         	// METRPXIO  	Funzionalità utente (transazione o job) con add, change,delete di un ILF
    @Column(name="fpExternalInquiryEQ")
	private long  fpExternalInquiryEQ = 0;       	// METRPXEQ   	Funzionalità utente (transazione o job) di sola read da ILF o EIF
    @Column(name="fpInternalLogicalFileILF")
	private long  fpInternalLogicalFileILF = 0;  	// METRPILF   	Tabelle/files definite e gestite dentro il sistema/sottosistema
    @Column(name="fpExternalInterfaceFileEIF")
	private long  fpExternalInterfaceFileEIF = 0;	// METRPEIF  	Tabelle/files definite fuori dal sistema/sottosistema acceduti in read/update

	// Misure di complessità funzionale/tecnica per rehosting 
    @Column(name="rhRateObjectRelation")
	private double rhRateObjectRelation = 0d;     	// METRHROR    Rapporto tra il numero di oggetti e numero di relazioni 
    @Column(name="rhObjectsInternal")
	private long  rhObjectsInternal = 0;         	// METRHINT    Numero di oggetti interni al perimetro 
    @Column(name="rhObjectsExternal")
	private long  rhObjectsExternal  = 0;        	// METRHEXT    Numero di oggetti esterni al perimetro 
    @Column(name="rhObjectsUnportable")
	private long  rhObjectsUnportable = 0;       	// METRHUNP    Numero di oggetti non portabili ((Assembler, PL/I, Load Module, ecc...) 
    @Column(name="rhFilesBynary")
	private long  rhFilesBynary = 0;             	// METRHBYN    Numero di files/tabelle contenenti campi binari 
	
	// Misure di complessità ciclomatica
    @Column(name="mcCabeArcs")
	private long  mcCabeArcs = 0;                	// METRMARC    Numero archi di programma
    @Column(name="mcCabeNodes")
	private long  mcCabeNodes = 0;               	// METRMNOD    Numero nodi di programma
    @Column(name="mcCabeGraphConn")
	private long  mcCabeGraphConn = 0;         	 	// METRMGCN    Numero di sottografi connessi, in cobol sono section/paragrafi richiamati
    @Column(name="mcCabeOperatorsOrAnd")
	private long  mcCabeOperatorsOrAnd = 0;      	// METRMOPC    Numero operatori condizionali OR AND per calcolo esteso

	// Misure di complessità di Halstead (o Software Science)
    @Column(name="halsteadOperators")
	private long  halsteadOperators = 0;         	// METRHOPT    Numero operatori distinti in un programma (n1)
    @Column(name="halsteadOperands")
	private long  halsteadOperands = 0;          	// METRHOPN    Numero operandi distinti in un programma  (n2)
    @Column(name="halsteadOperatorsOcc")
	private long  halsteadOperatorsOcc = 0;      	// METRHPTO    Numero occorrenze di operatori (N1)
    @Column(name="halsteadOperandsOcc")
	private long  halsteadOperandsOcc = 0;       	// METRHPNO    Numero occorrenze di operandi (N2)
    @Column(name="halsteadLengthPgm")
	private long  halsteadLengthPgm = 0;         	// METRHLNP    Lunghezza programma
    @Column(name="halsteadVocabularyPgm")
	private long  halsteadVocabularyPgm = 0;     	// METRHVBP    Vocabolario programma
    @Column(name="halsteadVolumePgm")
	private double halsteadVolumePgm = 0d;        	// METRHVLP    Volume programma
    @Column(name="halsteadDifficultPgm")
	private double halsteadDifficultPgm = 0d;     	// METRHDFP    Difficoltà programma
    @Column(name="halsteadEffortPgm")
	private double halsteadEffortPgm = 0d;        	// METRHEFP    Sforzo programma
    @Column(name="halsteadTimeWriting")
	private long halsteadTimeWriting = 0;        	// METRHTMW    Tempo stimato di scrittura programma in secondi

	// Indici di complessita/manutenibilità/Testabilità totali (somma di tutte le procedure interne)
    @Column(name="idxMITot")
	private double idxMITot = 0d;                 	// METRIMIT    Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
    @Column(name="idxFPTot")
	private double idxFPTot = 0d;                 	// METRIFPT    Indice di punti funzione
    @Column(name="idxMcCabeTot")
	private double idxMcCabeTot = 0d;             	// METRIMCT    Indice di complessità ciclomatica di McCabe con il seguente significato
    @Column(name="idxReHostingTot")
	private double idxReHostingTot = 0d;          	// METRIRET    Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità/Testabilità massimi
    @Column(name="idxMIHigh")
	private double idxMIHigh = 0d;                	// METRIMIH    Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
    @Column(name="idxFPHigh")
	private double idxFPHigh = 0d;                	// METRIFPH    Indice di punti funzione
    @Column(name="idxMcCabeHigh")
	private double idxMcCabeHigh = 0d;            	// METRIMCH    Indice di complessità ciclomatica di McCabe con il seguente significato
    @Column(name="idxReHostingHigh")
	private double idxReHostingHigh = 0d;         	// METRIREH    Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità/Testabilità minimi
    @Column(name="idxMILow")
	private double idxMILow = 0d;                 	// METRIMIL    Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
    @Column(name="idxFPLow")
	private double idxFPLow = 0d;                 	// METRIFPL    Indice di punti funzione
    @Column(name="idxMcCabeLow")
	private double idxMcCabeLow = 0d;             	// METRIMCL    Indice di complessità ciclomatica di McCabe con il seguente significato
    @Column(name="idxReHostingLow")
	private double idxReHostingLow = 0d;          	// METRIREL    Indice della complessità e sforzo di rehosting di una applicazione.
	
	// Indici di complessita/manutenibilità/Testabilità medio
    @Column(name="idxMIAvg")
	private double idxMIAvg = 0d;                 	// METRIMIA    Indice di manutenibilità introdotta nel 1991 da Oman e Hagemeister
    @Column(name="idxFPAvg")
	private double idxFPAvg = 0d;                 	// METRIFPA    Indice di punti funzione
    @Column(name="idxMcCabeAvg")
	private double idxMcCabeAvg = 0d;             	// METRIMCA    Indice di complessità ciclomatica di McCabe con il seguente significato
    @Column(name="idxReHostingAvg")
	private double idxReHostingAvg = 0d;          	// METRIREA    Indice della complessità e sforzo di rehosting di una applicazione.

	// Sistema di qualità SQUALE, numero violazioni per categoria gravità
    @Column(name="squaleViolationsBlocker")
	private long squaleViolationsBlocker = 0;     	// METRSVBK    Squale numero violazioni bloccanti
    @Column(name="squaleViolationsCritical")
	private long squaleViolationsCritical = 0;    	// METRSVCR    Squale numero violazioni critiche
    @Column(name="squaleViolationsMajor")
	private long squaleViolationsMajor = 0;    		// METRSVMJ    Squale numero violazioni maggiori
    @Column(name="squaleViolationsMinor")
	private long squaleViolationsMinor = 0;    		// METRSVMN    Squale numero violazioni minori
    @Column(name="squaleViolationsInfo")
	private long squaleViolationsInfo = 0;    		// METRSVIN    Squale numero violazioni informative

	// Sistema di qualità SQUALE, valori generali
    @Column(name="squaleSSRL")
	private EnumMetricsSqualeRating squaleSSRL = null;// METRSSRL  Squale rating  Livello A-E valore di rating (T0041)
    @Column(name="squaleSSRI")
	private double squaleSSRI = 0d;        	 	    // METRSSRI    Squale rating  value (SQI / tempo stimato di sviluppo)
    @Column(name="squaleSSCI")
	private double squaleSSCI = 0d;        			// METRSSCI    Squale rule compliance (100 indica perfetta aderenza)
    @Column(name="squaleSSQI")
	private long squaleSSQI = 0;					// METRSSQI    Squale absolute remediation cost. SQTI+SQRI+...+SQPI

	// Sistema di qualità SQUALE, valori di dettaglio indice qualità assoluto SSQI, SQxI
    @Column(name="squaleSQTI")
	private long squaleSQTI = 0;					// METRSQTI    Testability 		index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQRI")
	private long squaleSQRI = 0;					// METRSQRI    Reliability 		index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQCI")
	private long squaleSQCI = 0;					// METRSQCI    Changeability	index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQEI")
	private long squaleSQEI = 0;					// METRSQEI    Efficiency 		index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQSI")
	private long squaleSQSI = 0;					// METRSQSI    Security 		index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQMI")
	private long squaleSQMI = 0;					// METRSQMI    Maintenability 	index (somma costi remediation per questa caratteristica)
    @Column(name="squaleSQPI")
	private long squaleSQPI = 0;					// METRSQPI    Portability 		index (somma costi remediation per questa caratteristica)

	// Sistema di qualità SQUALE, valori di dettaglio indici consolidati SCTx
    @Column(name="squaleSCTI")
	private long squaleSCTI = 0;					// METRSCTI    Testability 		index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCRI")
	private long squaleSCRI = 0;					// METRSCRI    Reliability 		index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCCI")
	private long squaleSCCI = 0;					// METRSCCI    Changeability	index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCEI")
	private long squaleSCEI = 0;					// METRSCEI    Efficiency 		index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCSI")
	private long squaleSCSI = 0;					// METRSCSI    Security 		index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCMI")
	private long squaleSCMI = 0;					// METRSCMI    Maintenability 	index consolidato (somma costi remediation caratteristiche precedenti)
    @Column(name="squaleSCPI")
	private long squaleSCPI = 0;					// METRSCPI    Portability 		index consolidato (somma costi remediation caratteristiche precedenti)

	// Sistema di qualità SQUALE, valori di dettaglio indici di densita SDxI
    @Column(name="squaleSDTI")
	private double squaleSDTI = 0d;					// METRSDTI    Testability 		index consolidato (SQTI / dimensioni)
    @Column(name="squaleSDRI")
	private double squaleSDRI = 0d;					// METRSDRI    Reliability 		index consolidato (SQRI / dimensioni)
    @Column(name="squaleSDCI")
	private double squaleSDCI = 0d;					// METRSDCI    Changeability	index consolidato (SQCI / dimensioni)
    @Column(name="squaleSDEI")
	private double squaleSDEI = 0d;					// METRSDEI    Efficiency 		index consolidato (SQEI / dimensioni)
    @Column(name="squaleSDSI")
	private double squaleSDSI = 0d;					// METRSDSI    Security 		index consolidato (SQSI / dimensioni)
    @Column(name="squaleSDMI")
	private double squaleSDMI = 0d;					// METRSDMI    Maintenability 	index consolidato (SQMI / dimensioni)
    @Column(name="squaleSDPI")
	private double squaleSDPI = 0d;					// METRSDPI    Portability 		index consolidato (SQPI / dimensioni)

	// Sistema di qualità SQUALE, valori di dettaglio indici squale rating SRxI 
    @Column(name="squaleSRTI")
	private double squaleSRTI = 0d;					// METRSRTI    Testability 		Squale rating (SQTI / tempo stimato sviluppo)
    @Column(name="squaleSRRI")
	private double squaleSRRI = 0d;					// METRSRRI    Reliability 		Squale rating (SQRI / tempo stimato sviluppo)
    @Column(name="squaleSRCI")
	private double squaleSRCI = 0d;					// METRSRCI    Changeability	Squale rating (SQCI / tempo stimato sviluppo)
    @Column(name="squaleSREI")
	private double squaleSREI = 0d;					// METRSREI    Efficiency 		Squale rating (SQEI / tempo stimato sviluppo)
    @Column(name="squaleSRSI")
	private double squaleSRSI = 0d;					// METRSRSI    Security 		Squale rating (SQSI / tempo stimato sviluppo)
    @Column(name="squaleSRMI")
	private double squaleSRMI = 0d;					// METRSRNI    Maintenability 	Squale rating (SQMI / tempo stimato sviluppo)
    @Column(name="squaleSRPI")
	private double squaleSRPI = 0d;					// METRSRPI    Portability 		Squale rating (SQPI / tempo stimato sviluppo)

	// Sistema di qualità SQUALE, valori di dettaglio livelli squale rating SRxL 
    @Column(name="squaleSRTL")
	private EnumMetricsSqualeRating squaleSRTL = null;// METRSRTL    Testability 		Livello A-E valore di rating (T0041)
    @Column(name="squaleSRRL")
	private EnumMetricsSqualeRating squaleSRRL = null;// METRSRRL    Reliability 		Livello A-E valore di rating (T0041)
    @Column(name="squaleSRCL")
	private EnumMetricsSqualeRating squaleSRCL = null;// METRSRCL    Changeability	    Livello A-E valore di rating (T0041)
    @Column(name="squaleSREL")
	private EnumMetricsSqualeRating squaleSREL = null;// METRSREL    Efficiency 		Livello A-E valore di rating (T0041)
    @Column(name="squaleSRSL")
	private EnumMetricsSqualeRating squaleSRSL = null;// METRSRSL    Security 		    Livello A-E valore di rating (T0041)
    @Column(name="squaleSRML")
	private EnumMetricsSqualeRating squaleSRML = null;// METRSRNL    Maintenability 	Livello A-E valore di rating (T0041)
    @Column(name="squaleSRPL")
	private EnumMetricsSqualeRating squaleSRPL = null;// METRSRPL    Portability 		Livello A-E valore di rating (T0041)
	
		
	/*
	 * 
	 * Costruttore 
	 *
	 */
	public EntityMetricValue() {
		super();
		typeObject = EnumObject.NOT_ASSIGNED;
		scope = EnumMetricsScope.NOT_ASSIGNED;
		squaleSSRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRTL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRRL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRCL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSREL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRSL = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRML = EnumMetricsSqualeRating.NOT_ASSIGNED;
		squaleSRPL = EnumMetricsSqualeRating.NOT_ASSIGNED;

	}


	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}


	/**
	 * @param system the system to set
	 */
	public void setSystem(String system) {
		this.system = system;
	}


	/**
	 * @return the subSystem
	 */
	public String getSubSystem() {
		return subSystem;
	}


	/**
	 * @return the scope
	 */
	public EnumMetricsScope getScope() {
		return scope;
	}


	/**
	 * @param scope the scope to set
	 */
	public void setScope(EnumMetricsScope scope) {
		this.scope = scope;
	}


	/**
	 * @param subSystem the subSystem to set
	 */
	public void setSubSystem(String subSystem) {
		this.subSystem = subSystem;
	}


	/**
	 * @return the idObject
	 */
	public String getIdObject() {
		return idObject;
	}


	/**
	 * @param idObject the idObject to set
	 */
	public void setIdObject(String idObject) {
		this.idObject = idObject;
	}


	/**
	 * @return the typeObject
	 */
	public EnumObject getTypeObject() {
		return typeObject;
	}


	/**
	 * @param typeObject the typeObject to set
	 */
	public void setTypeObject(EnumObject typeObject) {
		this.typeObject = typeObject;
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
	 * @return the sectionThru
	 */
	public String getSectionThru() {
		return sectionThru;
	}


	/**
	 * @param sectionThru the sectionThru to set
	 */
	public void setSectionThru(String sectionThru) {
		this.sectionThru = sectionThru;
	}


	/**
	 * @return the cntPgmAnalyzed
	 */
	public long getCntPgmAnalyzed() {
		return cntPgmAnalyzed;
	}


	/**
	 * @param cntPgmAnalyzed the cntPgmAnalyzed to set
	 */
	public void setCntPgmAnalyzed(long cntPgmAnalyzed) {
		this.cntPgmAnalyzed = cntPgmAnalyzed;
	}


	/**
	 * @return the cntPgmExecuted
	 */
	public long getCntPgmExecuted() {
		return cntPgmExecuted;
	}


	/**
	 * @param cntPgmExecuted the cntPgmExecuted to set
	 */
	public void setCntPgmExecuted(long cntPgmExecuted) {
		this.cntPgmExecuted = cntPgmExecuted;
	}


	/**
	 * @return the cntSubPgmAnalyzed
	 */
	public long getCntSubPgmAnalyzed() {
		return cntSubPgmAnalyzed;
	}


	/**
	 * @param cntSubPgmAnalyzed the cntSubPgmAnalyzed to set
	 */
	public void setCntSubPgmAnalyzed(long cntSubPgmAnalyzed) {
		this.cntSubPgmAnalyzed = cntSubPgmAnalyzed;
	}


	/**
	 * @return the cntSubPgmExecuted
	 */
	public long getCntSubPgmExecuted() {
		return cntSubPgmExecuted;
	}


	/**
	 * @param cntSubPgmExecuted the cntSubPgmExecuted to set
	 */
	public void setCntSubPgmExecuted(long cntSubPgmExecuted) {
		this.cntSubPgmExecuted = cntSubPgmExecuted;
	}


	/**
	 * @return the cntCopyDefined
	 */
	public long getCntCopyDefined() {
		return cntCopyDefined;
	}


	/**
	 * @param cntCopyDefined the cntCopyDefined to set
	 */
	public void setCntCopyDefined(long cntCopyDefined) {
		this.cntCopyDefined = cntCopyDefined;
	}


	/**
	 * @return the cntJclJob
	 */
	public long getCntJclJob() {
		return cntJclJob;
	}


	/**
	 * @param cntJclJob the cntJclJob to set
	 */
	public void setCntJclJob(long cntJclJob) {
		this.cntJclJob = cntJclJob;
	}


	/**
	 * @return the cntJclInclude
	 */
	public long getCntJclInclude() {
		return cntJclInclude;
	}


	/**
	 * @param cntJclInclude the cntJclInclude to set
	 */
	public void setCntJclInclude(long cntJclInclude) {
		this.cntJclInclude = cntJclInclude;
	}


	/**
	 * @return the cntJclProc
	 */
	public long getCntJclProc() {
		return cntJclProc;
	}


	/**
	 * @param cntJclProc the cntJclProc to set
	 */
	public void setCntJclProc(long cntJclProc) {
		this.cntJclProc = cntJclProc;
	}


	/**
	 * @return the sizeLinesCodeLogical
	 */
	public long getSizeLinesCodeLogical() {
		return sizeLinesCodeLogical;
	}


	/**
	 * @param sizeLinesCodeLogical the sizeLinesCodeLogical to set
	 */
	public void setSizeLinesCodeLogical(long sizeLinesCodeLogical) {
		this.sizeLinesCodeLogical = sizeLinesCodeLogical;
	}


	/**
	 * @return the sizeLinesCodePhisical
	 */
	public long getSizeLinesCodePhisical() {
		return sizeLinesCodePhisical;
	}


	/**
	 * @param sizeLinesCodePhisical the sizeLinesCodePhisical to set
	 */
	public void setSizeLinesCodePhisical(long sizeLinesCodePhisical) {
		this.sizeLinesCodePhisical = sizeLinesCodePhisical;
	}


	/**
	 * @return the sizeLinesBlank
	 */
	public long getSizeLinesBlank() {
		return sizeLinesBlank;
	}


	/**
	 * @param sizeLinesBlank the sizeLinesBlank to set
	 */
	public void setSizeLinesBlank(long sizeLinesBlank) {
		this.sizeLinesBlank = sizeLinesBlank;
	}


	/**
	 * @return the sizeLinesBlankProc
	 */
	public long getSizeLinesBlankProc() {
		return sizeLinesBlankProc;
	}


	/**
	 * @param sizeLinesBlankProc the sizeLinesBlankProc to set
	 */
	public void setSizeLinesBlankProc(long sizeLinesBlankProc) {
		this.sizeLinesBlankProc = sizeLinesBlankProc;
	}


	/**
	 * @return the sizeLinesBlankData
	 */
	public long getSizeLinesBlankData() {
		return sizeLinesBlankData;
	}


	/**
	 * @param sizeLinesBlankData the sizeLinesBlankData to set
	 */
	public void setSizeLinesBlankData(long sizeLinesBlankData) {
		this.sizeLinesBlankData = sizeLinesBlankData;
	}


	/**
	 * @return the sizeLinesComment
	 */
	public long getSizeLinesComment() {
		return sizeLinesComment;
	}


	/**
	 * @param sizeLinesComment the sizeLinesComment to set
	 */
	public void setSizeLinesComment(long sizeLinesComment) {
		this.sizeLinesComment = sizeLinesComment;
	}


	/**
	 * @return the sizeLinesCommentProc
	 */
	public long getSizeLinesCommentProc() {
		return sizeLinesCommentProc;
	}


	/**
	 * @param sizeLinesCommentProc the sizeLinesCommentProc to set
	 */
	public void setSizeLinesCommentProc(long sizeLinesCommentProc) {
		this.sizeLinesCommentProc = sizeLinesCommentProc;
	}


	/**
	 * @return the sizeLinesCommentData
	 */
	public long getSizeLinesCommentData() {
		return sizeLinesCommentData;
	}


	/**
	 * @param sizeLinesCommentData the sizeLinesCommentData to set
	 */
	public void setSizeLinesCommentData(long sizeLinesCommentData) {
		this.sizeLinesCommentData = sizeLinesCommentData;
	}


	/**
	 * @return the sizeInstr
	 */
	public long getSizeInstr() {
		return sizeInstr;
	}


	/**
	 * @param sizeInstr the sizeInstr to set
	 */
	public void setSizeInstr(long sizeInstr) {
		this.sizeInstr = sizeInstr;
	}

	

	/**
	 * @return the backFiredFunctionPoint
	 */
	public double getBackFiredFunctionPoint() {
		return backFiredFunctionPoint;
	}


	/**
	 * @param backFiredFunctionPoint the backFiredFunctionPoint to set
	 */
	public void setBackFiredFunctionPoint(double backFiredFunctionPoint) {
		this.backFiredFunctionPoint = backFiredFunctionPoint;
	}


	/**
	 * @return the timeDevelopment
	 */
	public double getTimeDevelopment() {
		return timeDevelopment;
	}


	/**
	 * @param timeDevelopment the timeDevelopment to set
	 */
	public void setTimeDevelopment(double timeDevelopment) {
		this.timeDevelopment = timeDevelopment;
	}


	/**
	 * @return the defFields
	 */
	public long getDefFields() {
		return defFields;
	}


	/**
	 * @param defFields the defFields to set
	 */
	public void setDefFields(long defFields) {
		this.defFields = defFields;
	}


	/**
	 * @return the defFieldsInCopy
	 */
	public long getDefFieldsInCopy() {
		return defFieldsInCopy;
	}


	/**
	 * @param defFieldsInCopy the defFieldsInCopy to set
	 */
	public void setDefFieldsInCopy(long defFieldsInCopy) {
		this.defFieldsInCopy = defFieldsInCopy;
	}


	/**
	 * @return the defLiterals
	 */
	public long getDefLiterals() {
		return defLiterals;
	}


	/**
	 * @param defLiterals the defLiterals to set
	 */
	public void setDefLiterals(long defLiterals) {
		this.defLiterals = defLiterals;
	}


	/**
	 * @return the deadFields
	 */
	public long getDeadFields() {
		return deadFields;
	}


	/**
	 * @param deadFields the deadField to set
	 */
	public void setDeadFields(long deadFields) {
		this.deadFields = deadFields;
	}


	/**
	 * @return the deadSubGraph
	 */
	public long getDeadSubGraph() {
		return deadSubGraph;
	}


	/**
	 * @param deadSubGraph the deadSubGraph to set
	 */
	public void setDeadSubGraph(long deadSubGraph) {
		this.deadSubGraph = deadSubGraph;
	}


	/**
	 * @return the deadLabels
	 */
	public long getDeadLabels() {
		return deadLabels;
	}


	/**
	 * @param deadLabel the deadLabel to set
	 */
	public void setDeadLabels(long deadLabels) {
		this.deadLabels = deadLabels;
	}


	/**
	 * @return the deadInstr
	 */
	public long getDeadInstr() {
		return deadInstr;
	}


	/**
	 * @param deadInstr the deadInstr to set
	 */
	public void setDeadInstr(long deadInstr) {
		this.deadInstr = deadInstr;
	}


	/**
	 * @return the deadCopy
	 */
	public long getDeadCopyData() {
		return deadCopyData;
	}


	/**
	 * @param deadCopyDaty the deadCopy to set
	 */
	public void setDeadCopyData(long deadCopyData) {
		this.deadCopyData = deadCopyData;
	}


	/**
	 * @return the deadCopyProc
	 */
	public long getDeadCopyProc() {
		return deadCopyProc;
	}


	/**
	 * @param deadCopyDaty the deadCopyProc to set
	 */
	public void setDeadCopyProc(long deadCopyProc) {
		this.deadCopyProc = deadCopyProc;
	}


	/**
	 * @return the jclDD
	 */
	public long getJclDD() {
		return jclDD;
	}


	/**
	 * @param jclDD the jclDD to set
	 */
	public void setJclDD(long jclDD) {
		this.jclDD = jclDD;
	}


	/**
	 * @return the jclStepDefined
	 */
	public long getJclStepDefined() {
		return jclStepDefined;
	}


	/**
	 * @param jclStepDefined the jclStepDefined to set
	 */
	public void setJclStepDefined(long jclStepDefined) {
		this.jclStepDefined = jclStepDefined;
	}


	/**
	 * @return the jclStepUpdate
	 */
	public long getJclStepUpdate() {
		return jclStepUpdate;
	}


	/**
	 * @param jclStepUpdate the jclStepUpdate to set
	 */
	public void setJclStepUpdate(long jclStepUpdate) {
		this.jclStepUpdate = jclStepUpdate;
	}


	/**
	 * @return the jclDsname
	 */
	public long getJclDsname() {
		return jclDsname;
	}


	/**
	 * @param jclDsname the jclDsname to set
	 */
	public void setJclDsname(long jclDsname) {
		this.jclDsname = jclDsname;
	}


	/**
	 * @return the jclDsnameReferenced
	 */
	public long getJclDsnameReferenced() {
		return jclDsnameReferenced;
	}


	/**
	 * @param jclDsnameReferenced the jclDsnameReferenced to set
	 */
	public void setJclDsnameReferenced(long jclDsnameReferenced) {
		this.jclDsnameReferenced = jclDsnameReferenced;
	}


	/**
	 * @return the jclDsnameUnReferenced
	 */
	public long getJclDsnameUnReferenced() {
		return jclDsnameUnReferenced;
	}


	/**
	 * @param jclDsnameUnReferenced the jclDsnameUnReferenced to set
	 */
	public void setJclDsnameUnReferenced(long jclDsnameUnReferenced) {
		this.jclDsnameUnReferenced = jclDsnameUnReferenced;
	}


	/**
	 * @return the jclIncludeCalled
	 */
	public long getJclIncludeCalled() {
		return jclIncludeCalled;
	}


	/**
	 * @param jclIncludeCalled the jclIncludeCalled to set
	 */
	public void setJclIncludeCalled(long jclIncludeCalled) {
		this.jclIncludeCalled = jclIncludeCalled;
	}


	/**
	 * @return the jclProcCalled
	 */
	public long getJclProcCalled() {
		return jclProcCalled;
	}


	/**
	 * @param jclProcCalled the jclProcCalled to set
	 */
	public void setJclProcCalled(long jclProcCalled) {
		this.jclProcCalled = jclProcCalled;
	}


	/**
	 * @return the structFanIn
	 */
	public long getStructFanIn() {
		return structFanIn;
	}


	/**
	 * @param structFanIn the structFanIn to set
	 */
	public void setStructFanIn(long structFanIn) {
		this.structFanIn = structFanIn;
	}


	/**
	 * @return the structFanOut
	 */
	public long getStructFanOut() {
		return structFanOut;
	}


	/**
	 * @param structFanOut the structFanOut to set
	 */
	public void setStructFanOut(long structFanOut) {
		this.structFanOut = structFanOut;
	}


	/**
	 * @return the structSections
	 */
	public long getStructSections() {
		return structSections;
	}


	/**
	 * @param structSections the structSections to set
	 */
	public void setStructSections(long structSections) {
		this.structSections = structSections;
	}


	/**
	 * @return the structParagraphs
	 */
	public long getStructParagraphs() {
		return structParagraphs;
	}


	/**
	 * @param structParagraphs the structParagraphs to set
	 */
	public void setStructParagraphs(long structParagraphs) {
		this.structParagraphs = structParagraphs;
	}


	/**
	 * @return the funcObjects
	 */
	public long getFuncObjects() {
		return funcObjects;
	}


	/**
	 * @param funcObjects the funcObjects to set
	 */
	public void setFuncObjects(long funcObjects) {
		this.funcObjects = funcObjects;
	}


	/**
	 * @return the funcRelations
	 */
	public long getFuncRelations() {
		return funcRelations;
	}


	/**
	 * @param funcRelations the funcRelations to set
	 */
	public void setFuncRelations(long funcRelations) {
		this.funcRelations = funcRelations;
	}


	/**
	 * @return the funcTranInternal
	 */
	public long getFuncTranInternal() {
		return funcTranInternal;
	}


	/**
	 * @param funcTranInternal the funcTranInternal to set
	 */
	public void setFuncTranInternal(long funcTranInternal) {
		this.funcTranInternal = funcTranInternal;
	}


	/**
	 * @return the funcTranExternal
	 */
	public long getFuncTranExternal() {
		return funcTranExternal;
	}


	/**
	 * @param funcTranExternal the funcTranExternal to set
	 */
	public void setFuncTranExternal(long funcTranExternal) {
		this.funcTranExternal = funcTranExternal;
	}


	/**
	 * @return the funcMap
	 */
	public long getFuncMap() {
		return funcMap;
	}


	/**
	 * @param funcMap the funcMap to set
	 */
	public void setFuncMap(long funcMap) {
		this.funcMap = funcMap;
	}


	/**
	 * @return the funcCallInternal
	 */
	public long getFuncCallInternal() {
		return funcCallInternal;
	}


	/**
	 * @param funcCallInternal the funcCallInternal to set
	 */
	public void setFuncCallInternal(long funcCallInternal) {
		this.funcCallInternal = funcCallInternal;
	}


	/**
	 * @return the funcCallExternal
	 */
	public long getFuncCallExternal() {
		return funcCallExternal;
	}


	/**
	 * @param funcCallExternal the funcCallExternal to set
	 */
	public void setFuncCallExternal(long funcCallExternal) {
		this.funcCallExternal = funcCallExternal;
	}


	/**
	 * @return the funcAccEntityInternal
	 */
	public long getFuncAccEntityInternal() {
		return funcAccEntityInternal;
	}


	/**
	 * @param funcAccEntityInternal the funcAccEntityInternal to set
	 */
	public void setFuncAccEntityInternal(long funcAccEntityInternal) {
		this.funcAccEntityInternal = funcAccEntityInternal;
	}


	/**
	 * @return the funcAccEntityExternal
	 */
	public long getFuncAccEntityExternal() {
		return funcAccEntityExternal;
	}


	/**
	 * @param funcAccEntityExternal the funcAccEntityExternal to set
	 */
	public void setFuncAccEntityExternal(long funcAccEntityExternal) {
		this.funcAccEntityExternal = funcAccEntityExternal;
	}


	/**
	 * @return the funcAccMediaInternal
	 */
	public long getFuncAccMediaInternal() {
		return funcAccMediaInternal;
	}


	/**
	 * @param funcAccMediaInternal the funcAccMediaInternal to set
	 */
	public void setFuncAccMediaInternal(long funcAccMediaInternal) {
		this.funcAccMediaInternal = funcAccMediaInternal;
	}


	/**
	 * @return the funcAccMediaExternal
	 */
	public long getFuncAccMediaExternal() {
		return funcAccMediaExternal;
	}


	/**
	 * @param funcAccMediaExternal the funcAccMediaExternal to set
	 */
	public void setFuncAccMediaExternal(long funcAccMediaExternal) {
		this.funcAccMediaExternal = funcAccMediaExternal;
	}


	/**
	 * @return the mcCabeArcs
	 */
	public long getMcCabeArcs() {
		return mcCabeArcs;
	}


	/**
	 * @param mcCabeArcs the mcCabeArcs to set
	 */
	public void setMcCabeArcs(long mcCabeArcs) {
		this.mcCabeArcs = mcCabeArcs;
	}


	/**
	 * @return the mcCabeNodes
	 */
	public long getMcCabeNodes() {
		return mcCabeNodes;
	}


	/**
	 * @param mcCabeNodes the mcCabeNodes to set
	 */
	public void setMcCabeNodes(long mcCabeNodes) {
		this.mcCabeNodes = mcCabeNodes;
	}


	/**
	 * @return the mcCabeGraphConn
	 */
	public long getMcCabeGraphConn() {
		return mcCabeGraphConn;
	}


	/**
	 * @param mcCabeGraphConn the mcCabeGraphConn to set
	 */
	public void setMcCabeGraphConn(long mcCabeGraphConn) {
		this.mcCabeGraphConn = mcCabeGraphConn;
	}

	

	/**
	 * @return the mcCabeOperatorsOrAnd
	 */
	public long getMcCabeOperatorsOrAnd() {
		return mcCabeOperatorsOrAnd;
	}


	/**
	 * @param mcCabeOperatorsOrAnd the mcCabeOperatorsOrAnd to set
	 */
	public void setMcCabeOperatorsOrAnd(long mcCabeOperatorsOrAnd) {
		this.mcCabeOperatorsOrAnd = mcCabeOperatorsOrAnd;
	}


	/**
	 * @return the halsteadOperators
	 */
	public long getHalsteadOperators() {
		return halsteadOperators;
	}


	/**
	 * @param halsteadOperators the halsteadOperators to set
	 */
	public void setHalsteadOperators(long halsteadOperators) {
		this.halsteadOperators = halsteadOperators;
	}


	/**
	 * @return the halsteadOperands
	 */
	public long getHalsteadOperands() {
		return halsteadOperands;
	}


	/**
	 * @param halsteadOperands the halsteadOperands to set
	 */
	public void setHalsteadOperands(long halsteadOperands) {
		this.halsteadOperands = halsteadOperands;
	}


	/**
	 * @return the halsteadOperatorsOcc
	 */
	public long getHalsteadOperatorsOcc() {
		return halsteadOperatorsOcc;
	}


	/**
	 * @param halsteadOperatorsOcc the halsteadOperatorsOcc to set
	 */
	public void setHalsteadOperatorsOcc(long halsteadOperatorsOcc) {
		this.halsteadOperatorsOcc = halsteadOperatorsOcc;
	}


	/**
	 * @return the halsteadOperandsOcc
	 */
	public long getHalsteadOperandsOcc() {
		return halsteadOperandsOcc;
	}


	/**
	 * @param halsteadOperandsOcc the halsteadOperandsOcc to set
	 */
	public void setHalsteadOperandsOcc(long halsteadOperandsOcc) {
		this.halsteadOperandsOcc = halsteadOperandsOcc;
	}


	/**
	 * @return the halsteadLengthPgm
	 */
	public long getHalsteadLengthPgm() {
		return halsteadLengthPgm;
	}


	/**
	 * @param halsteadLengthPgm the halsteadLengthPgm to set
	 */
	public void setHalsteadLengthPgm(long halsteadLengthPgm) {
		this.halsteadLengthPgm = halsteadLengthPgm;
	}


	/**
	 * @return the halsteadVocabularyPgm
	 */
	public long getHalsteadVocabularyPgm() {
		return halsteadVocabularyPgm;
	}


	/**
	 * @param halsteadVocabularyPgm the halsteadVocabularyPgm to set
	 */
	public void setHalsteadVocabularyPgm(long halsteadVocabularyPgm) {
		this.halsteadVocabularyPgm = halsteadVocabularyPgm;
	}


	/**
	 * @return the halsteadVolumePgm
	 */
	public double getHalsteadVolumePgm() {
		return halsteadVolumePgm;
	}


	/**
	 * @param halsteadVolumePgm the halsteadVolumePgm to set
	 */
	public void setHalsteadVolumePgm(double halsteadVolumePgm) {
		this.halsteadVolumePgm = halsteadVolumePgm;
	}


	/**
	 * @return the halsteadDifficultPgm
	 */
	public double getHalsteadDifficultPgm() {
		return halsteadDifficultPgm;
	}


	/**
	 * @param halsteadDifficultPgm the halsteadDifficultPgm to set
	 */
	public void setHalsteadDifficultPgm(double halsteadDifficultPgm) {
		this.halsteadDifficultPgm = halsteadDifficultPgm;
	}


	/**
	 * @return the halsteadEffortPgm
	 */
	public double getHalsteadEffortPgm() {
		return halsteadEffortPgm;
	}


	/**
	 * @param halsteadEffortPgm the halsteadEffortPgm to set
	 */
	public void setHalsteadEffortPgm(double halsteadEffortPgm) {
		this.halsteadEffortPgm = halsteadEffortPgm;
	}

	
	

	/**
	 * @return the halsteadTimeWriting
	 */
	public long getHalsteadTimeWriting() {
		return halsteadTimeWriting;
	}


	/**
	 * @param halsteadTimeWriting the halsteadTimeWriting to set
	 */
	public void setHalsteadTimeWriting(long halsteadTimeWriting) {
		this.halsteadTimeWriting = halsteadTimeWriting;
	}


	/**
	 * @return the dynamicPgm
	 */
	public long getDynamicPgm() {
		return dynamicPgm;
	}


	/**
	 * @param dynamicPgm the dynamicPgm to set
	 */
	public void setDynamicPgm(long dynamicPgm) {
		this.dynamicPgm = dynamicPgm;
	}


	/**
	 * @return the dynamicInstr 
	 */
	public long getDynamicInstr() {
		return dynamicInstr;
	}


	/**
	 * @param dynamicInstr the dynamicInstr to set
	 */
	public void setDynamicInstr(long dynamicInstr) {
		this.dynamicInstr = dynamicInstr;
	}


	/**
	 * @return the dynamicInstrLight
	 */
	public long getDynamicInstrLight() {
		return dynamicInstrLight;
	}


	/**
	 * @param dynamicInstrLight the dynamicInstrLight to set
	 */
	public void setDynamicInstrLight(long dynamicInstrLight) {
		this.dynamicInstrLight = dynamicInstrLight;
	}


	/**
	 * @return the percDynamicInstr
	 */
	public double getPercDynamicInstr() {
		return percDynamicInstr;
	}


	/**
	 * @param percDynamicInstr the percDynamicInstr to set
	 */
	public void setPercDynamicInstr(double percDynamicInstr) {
		this.percDynamicInstr = percDynamicInstr;
	}


	/**
	 * @return the percDynamicInstrLight
	 */
	public double getPercDynamicInstrLight() {
		return percDynamicInstrLight;
	}


	/**
	 * @param percDynamicInstrLight the percDynamicInstrLight to set
	 */
	public void setPercDynamicInstrLight(double percDynamicInstrLight) {
		this.percDynamicInstrLight = percDynamicInstrLight;
	}


	/**
	 * @return the violations
	 */
	public long getViolations() {
		return violations;
	}


	/**
	 * @param violations the violations to set
	 */
	public void setViolations(long violations) {
		this.violations = violations;
	}


	/**
	 * @return the percViolationsByLogical
	 */
	public double getPercViolationsByLogical() {
		return percViolationsByLogical;
	}


	/**
	 * @param percViolationsByLogical the percViolationsByLogical to set
	 */
	public void setPercViolationsByLogical(double percViolationsByLogical) {
		this.percViolationsByLogical = percViolationsByLogical;
	}


	/**
	 * @return the percViolationsByPhisical
	 */
	public double getPercViolationsByPhisical() {
		return percViolationsByPhisical;
	}


	/**
	 * @param percViolationsByPhisical the percViolationsByPhisical to set
	 */
	public void setPercViolationsByPhisical(double percViolationsByPhisical) {
		this.percViolationsByPhisical = percViolationsByPhisical;
	}


	/**
	 * @return the percViolationsByInstruction
	 */
	public double getPercViolationsByInstruction() {
		return percViolationsByInstruction;
	}


	/**
	 * @param percViolationsByInstruction the percViolationsByInstruction to set
	 */
	public void setPercViolationsByInstruction(double percViolationsByInstruction) {
		this.percViolationsByInstruction = percViolationsByInstruction;
	}


	/**
	 * @return the idxMIAvg
	 */
	public double getIdxMIAvg() {
		return idxMIAvg;
	}


	/**
	 * @param idxMIAvg the idxMIAvg to set
	 */
	public void setIdxMIAvg(double idxMIAvg) {
		this.idxMIAvg = idxMIAvg;
	}


	/**
	 * @return the idxFPAvg
	 */
	public double getIdxFPAvg() {
		return idxFPAvg;
	}


	/**
	 * @param idxFPAvg the idxFPAvg to set
	 */
	public void setIdxFPAvg(double idxFPAvg) {
		this.idxFPAvg = idxFPAvg;
	}


	/**
	 * @return the idxMcCabeAvg
	 */
	public double getIdxMcCabeAvg() {
		return idxMcCabeAvg;
	}


	/**
	 * @param idxMcCabeAvg the idxMcCabeAvg to set
	 */
	public void setIdxMcCabeAvg(double idxMcCabeAvg) {
		this.idxMcCabeAvg = idxMcCabeAvg;
	}


	/**
	 * @return the idxReHostingAvg
	 */
	public double getIdxReHostingAvg() {
		return idxReHostingAvg;
	}


	/**
	 * @param idxReHostingAvg the idxReHostingAvg to set
	 */
	public void setIdxReHostingAvg(double idxReHostingAvg) {
		this.idxReHostingAvg = idxReHostingAvg;
	}

	/**
	 * @return the idxMIHigh
	 */
	public double getIdxMIHigh() {
		return idxMIHigh;
	}


	/**
	 * @param idxMIHigh the idxMIHigh to set
	 */
	public void setIdxMIHigh(double idxMIHigh) {
		this.idxMIHigh = idxMIHigh;
	}


	/**
	 * @return the idxFPHigh
	 */
	public double getIdxFPHigh() {
		return idxFPHigh;
	}


	/**
	 * @param idxFPHigh the idxFPHigh to set
	 */
	public void setIdxFPHigh(double idxFPHigh) {
		this.idxFPHigh = idxFPHigh;
	}


	/**
	 * @return the idxMcCabeHigh
	 */
	public double getIdxMcCabeHigh() {
		return idxMcCabeHigh;
	}


	/**
	 * @param idxMcCabeHigh the idxMcCabeHigh to set
	 */
	public void setIdxMcCabeHigh(double idxMcCabeHigh) {
		this.idxMcCabeHigh = idxMcCabeHigh;
	}


	/**
	 * @return the idxReHostingHigh
	 */
	public double getIdxReHostingHigh() {
		return idxReHostingHigh;
	}


	/**
	 * @param idxReHostingHigh the idxReHostingHigh to set
	 */
	public void setIdxReHostingHigh(double idxReHostingHigh) {
		this.idxReHostingHigh = idxReHostingHigh;
	}



	
	   /////////


	/**
	 * @return the idxMILow
	 */
	public double getIdxMILow() {
		return idxMILow;
	}


	/**
	 * @param idxMILow the idxMILow to set
	 */
	public void setIdxMILow(double idxMILow) {
		this.idxMILow = idxMILow;
	}


	/**
	 * @return the idxFPLow
	 */
	public double getIdxFPLow() {
		return idxFPLow;
	}


	/**
	 * @param idxFPLow the idxFPLow to set
	 */
	public void setIdxFPLow(double idxFPLow) {
		this.idxFPLow = idxFPLow;
	}


	/**
	 * @return the idxMcCabeLow
	 */
	public double getIdxMcCabeLow() {
		return idxMcCabeLow;
	}


	/**
	 * @param idxMcCabeLow the idxMcCabeLow to set
	 */
	public void setIdxMcCabeLow(double idxMcCabeLow) {
		this.idxMcCabeLow = idxMcCabeLow;
	}


	/**
	 * @return the idxReHostingLow
	 */
	public double getIdxReHostingLow() {
		return idxReHostingLow;
	}


	/**
	 * @param idxReHostingLow the idxReHostingLow to set
	 */
	public void setIdxReHostingLow(double idxReHostingLow) {
		this.idxReHostingLow = idxReHostingLow;
	}

	

	/**
	 * @return the idxMITot
	 */
	public double getIdxMITot() {
		return idxMITot;
	}


	/**
	 * @param idxMITot the idxMITot to set
	 */
	public void setIdxMITot(double idxMITot) {
		this.idxMITot = idxMITot;
	}


	/**
	 * @return the idxFPTot
	 */
	public double getIdxFPTot() {
		return idxFPTot;
	}


	/**
	 * @param idxFPTot the idxFPTot to set
	 */
	public void setIdxFPTot(double idxFPTot) {
		this.idxFPTot = idxFPTot;
	}


	/**
	 * @return the idxMcCabeTot
	 */
	public double getIdxMcCabeTot() {
		return idxMcCabeTot;
	}


	/**
	 * @param idxMcCabeTot the idxMcCabeTot to set
	 */
	public void setIdxMcCabeTot(double idxMcCabeTot) {
		this.idxMcCabeTot = idxMcCabeTot;
	}


	/**
	 * @return the idxReHostingTot
	 */
	public double getIdxReHostingTot() {
		return idxReHostingTot;
	}


	/**
	 * @param idxReHostingTot the idxReHostingTot to set
	 */
	public void setIdxReHostingTot(double idxReHostingTot) {
		this.idxReHostingTot = idxReHostingTot;
	}


	/**
	 * @return the percComByLogical
	 */
	public double getPercComByLogical() {
		return percComByLogical;
	}


	/**
	 * @param percComByLogical the percComByLogical to set
	 */
	public void setPercComByLogical(double percComByLogical) {
		this.percComByLogical = percComByLogical;
	}


	/**
	 * @return the percComByPhisical
	 */
	public double getPercComByPhisical() {
		return percComByPhisical;
	}


	/**
	 * @param percComByPhisical the percComByPhisical to set
	 */
	public void setPercComByPhisical(double percComByPhisical) {
		this.percComByPhisical = percComByPhisical;
	}


	/**
	 * @return the percComByInstruction
	 */
	public double getPercComByInstruction() {
		return percComByInstruction;
	}


	/**
	 * @param percComByInstruction the percComByInstruction to set
	 */
	public void setPercComByInstruction(double percComByInstruction) {
		this.percComByInstruction = percComByInstruction;
	}


	/**
	 * @return the rateBlankByPhisical
	 */
	public double getPercBlankByPhisical() {
		return percBlankByPhisical;
	}


	/**
	 * @param rateBlankByPhisical the rateBlankByPhisical to set
	 */
	public void setPercBlankByPhisical(double rateBlankByPhisical) {
		this.percBlankByPhisical = rateBlankByPhisical;
	}


	/**
	 * @return the rateBlankByInstruction
	 */
	public double getPercBlankByInstruction() {
		return percBlankByInstruction;
	}


	/**
	 * @param rateBlankByInstruction the rateBlankByInstruction to set
	 */
	public void setPercBlankByInstruction(double rateBlankByInstruction) {
		this.percBlankByInstruction = rateBlankByInstruction;
	}



	/**
	 * @return the fpExternalOutputEO
	 */
	public long getFpExternalOutputEO() {
		return fpExternalOutputEO;
	}


	/**
	 * @param fpExternalOutputEO the fpExternalOutputEO to set
	 */
	public void setFpExternalOutputEO(long fpExternalOutputEO) {
		this.fpExternalOutputEO = fpExternalOutputEO;
	}


	/**
	 * @return the fpExternalInputEI
	 */
	public long getFpExternalInputEI() {
		return fpExternalInputEI;
	}


	/**
	 * @param fpExternalInputEI the fpExternalInputEI to set
	 */
	public void setFpExternalInputEI(long fpExternalInputEI) {
		this.fpExternalInputEI = fpExternalInputEI;
	}


	/**
	 * @return the fpExternalInquiryEQ
	 */
	public long getFpExternalInquiryEQ() {
		return fpExternalInquiryEQ;
	}


	/**
	 * @param fpExternalInquiryEQ the fpExternalInquiryEQ to set
	 */
	public void setFpExternalInquiryEQ(long fpExternalInquiryEQ) {
		this.fpExternalInquiryEQ = fpExternalInquiryEQ;
	}


	/**
	 * @return the fpInternalLogicalFileILF
	 */
	public long getFpInternalLogicalFileILF() {
		return fpInternalLogicalFileILF;
	}


	/**
	 * @param fpInternalLogicalFileILF the fpInternalLogicalFileILF to set
	 */
	public void setFpInternalLogicalFileILF(long fpInternalLogicalFileILF) {
		this.fpInternalLogicalFileILF = fpInternalLogicalFileILF;
	}


	/**
	 * @return the fpExternalInterfaceFileEIF
	 */
	public long getFpExternalInterfaceFileEIF() {
		return fpExternalInterfaceFileEIF;
	}


	/**
	 * @param fpExternalInterfaceFileEIF the fpExternalInterfaceFileEIF to set
	 */
	public void setFpExternalInterfaceFileEIF(long fpExternalInterfaceFileEIF) {
		this.fpExternalInterfaceFileEIF = fpExternalInterfaceFileEIF;
	}


	/**
	 * @return the rhRateObjectRelation
	 */
	public double getRhRateObjectRelation() {
		return rhRateObjectRelation;
	}


	/**
	 * @param rhRateObjectRelation the rhRateObjectRelation to set
	 */
	public void setRhRateObjectRelation(double rhRateObjectRelation) {
		this.rhRateObjectRelation = rhRateObjectRelation;
	}


	/**
	 * @return the rhObjectsInternal
	 */
	public long getRhObjectsInternal() {
		return rhObjectsInternal;
	}


	/**
	 * @param rhObjectsInternal the rhObjectsInternal to set
	 */
	public void setRhObjectsInternal(long rhObjectsInternal) {
		this.rhObjectsInternal = rhObjectsInternal;
	}


	/**
	 * @return the rhObjectsExternal
	 */
	public long getRhObjectsExternal() {
		return rhObjectsExternal;
	}


	/**
	 * @param rhObjectsExternal the rhObjectsExternal to set
	 */
	public void setRhObjectsExternal(long rhObjectsExternal) {
		this.rhObjectsExternal = rhObjectsExternal;
	}


	/**
	 * @return the rhObjectsUnportable
	 */
	public long getRhObjectsUnportable() {
		return rhObjectsUnportable;
	}


	/**
	 * @param rhObjectsUnportable the rhObjectsUnportable to set
	 */
	public void setRhObjectsUnportable(long rhObjectsUnportable) {
		this.rhObjectsUnportable = rhObjectsUnportable;
	}


	/**
	 * @return the rhFilesBynary
	 */
	public long getRhFilesBynary() {
		return rhFilesBynary;
	}


	/**
	 * @param rhFilesBynary the rhFilesBynary to set
	 */
	public void setRhFilesBynary(long rhFilesBynary) {
		this.rhFilesBynary = rhFilesBynary;
	}

	
	
	/**
	 * @return the squaleViolationsBlocker
	 */
	public long getSqualeViolationsBlocker() {
		return squaleViolationsBlocker;
	}


	/**
	 * @param squaleViolationsBlocker the squaleViolationsBlocker to set
	 */
	public void setSqualeViolationsBlocker(long squaleViolationsBlocker) {
		this.squaleViolationsBlocker = squaleViolationsBlocker;
	}


	/**
	 * @return the squaleViolationsCritical
	 */
	public long getSqualeViolationsCritical() {
		return squaleViolationsCritical;
	}


	/**
	 * @param squaleViolationsCritical the squaleViolationsCritical to set
	 */
	public void setSqualeViolationsCritical(long squaleViolationsCritical) {
		this.squaleViolationsCritical = squaleViolationsCritical;
	}


	/**
	 * @return the squaleViolationsMajor
	 */
	public long getSqualeViolationsMajor() {
		return squaleViolationsMajor;
	}


	/**
	 * @param squaleViolationsMajor the squaleViolationsMajor to set
	 */
	public void setSqualeViolationsMajor(long squaleViolationsMajor) {
		this.squaleViolationsMajor = squaleViolationsMajor;
	}


	/**
	 * @return the squaleViolationsMinor
	 */
	public long getSqualeViolationsMinor() {
		return squaleViolationsMinor;
	}


	/**
	 * @param squaleViolationsMinor the squaleViolationsMinor to set
	 */
	public void setSqualeViolationsMinor(long squaleViolationsMinor) {
		this.squaleViolationsMinor = squaleViolationsMinor;
	}


	/**
	 * @return the squaleViolationsInfo
	 */
	public long getSqualeViolationsInfo() {
		return squaleViolationsInfo;
	}


	/**
	 * @param squaleViolationsInfo the squaleViolationsInfo to set
	 */
	public void setSqualeViolationsInfo(long squaleViolationsInfo) {
		this.squaleViolationsInfo = squaleViolationsInfo;
	}


	/**
	 * @return the squaleSSRL
	 */
	public EnumMetricsSqualeRating getSqualeSSRL() {
		return squaleSSRL;
	}


	/**
	 * @param squaleSSRL the squaleSSRL to set
	 */
	public void setSqualeSSRL(EnumMetricsSqualeRating squaleSSRL) {
		this.squaleSSRL = squaleSSRL;
	}


	/**
	 * @return the squaleSSRI
	 */
	public double getSqualeSSRI() {
		return squaleSSRI;
	}


	/**
	 * @param squaleSSRI the squaleSSRI to set
	 */
	public void setSqualeSSRI(double squaleSSRI) {
		this.squaleSSRI = squaleSSRI;
	}


	/**
	 * @return the squaleSSCI
	 */
	public double getSqualeSSCI() {
		return squaleSSCI;
	}


	/**
	 * @param squaleSCI the squaleSCI to set
	 */
	public void setSqualeSSCI(double squaleSSCI) {
		this.squaleSSCI = squaleSSCI;
	}


	/**
	 * @return the squaleSSQI
	 */
	public long getSqualeSSQI() {
		return squaleSSQI;
	}


	/**
	 * @param squaleSSQI the squaleSSQI to set
	 */
	public void setSqualeSSQI(long squaleSSQI) {
		this.squaleSSQI = squaleSSQI;
	}


	/**
	 * @return the squaleSQTI
	 */
	public long getSqualeSQTI() {
		return squaleSQTI;
	}


	/**
	 * @param squaleSQTI the squaleSQTI to set
	 */
	public void setSqualeSQTI(long squaleSQTI) {
		this.squaleSQTI = squaleSQTI;
	}


	/**
	 * @return the squaleSQRI
	 */
	public long getSqualeSQRI() {
		return squaleSQRI;
	}


	/**
	 * @param squaleSQRI the squaleSQRI to set
	 */
	public void setSqualeSQRI(long squaleSQRI) {
		this.squaleSQRI = squaleSQRI;
	}


	/**
	 * @return the squaleSQCI
	 */
	public long getSqualeSQCI() {
		return squaleSQCI;
	}


	/**
	 * @param squaleSQCI the squaleSQCI to set
	 */
	public void setSqualeSQCI(long squaleSQCI) {
		this.squaleSQCI = squaleSQCI;
	}


	/**
	 * @return the squaleSQEI
	 */
	public long getSqualeSQEI() {
		return squaleSQEI;
	}


	/**
	 * @param squaleSQEI the squaleSQEI to set
	 */
	public void setSqualeSQEI(long squaleSQEI) {
		this.squaleSQEI = squaleSQEI;
	}


	/**
	 * @return the squaleSQSI
	 */
	public long getSqualeSQSI() {
		return squaleSQSI;
	}


	/**
	 * @param squaleSQSI the squaleSQSI to set
	 */
	public void setSqualeSQSI(long squaleSQSI) {
		this.squaleSQSI = squaleSQSI;
	}


	/**
	 * @return the squaleSQMI
	 */
	public long getSqualeSQMI() {
		return squaleSQMI;
	}


	/**
	 * @param squaleSQMI the squaleSQMI to set
	 */
	public void setSqualeSQMI(long squaleSQMI) {
		this.squaleSQMI = squaleSQMI;
	}


	/**
	 * @return the squaleSQPI
	 */
	public long getSqualeSQPI() {
		return squaleSQPI;
	}


	/**
	 * @param squaleSQPI the squaleSQPI to set
	 */
	public void setSqualeSQPI(long squaleSQPI) {
		this.squaleSQPI = squaleSQPI;
	}


	/**
	 * @return the squaleSCTI
	 */
	public long getSqualeSCTI() {
		return squaleSCTI;
	}


	/**
	 * @param squaleSCTI the squaleSCTI to set
	 */
	public void setSqualeSCTI(long squaleSCTI) {
		this.squaleSCTI = squaleSCTI;
	}


	/**
	 * @return the squaleSCRI
	 */
	public long getSqualeSCRI() {
		return squaleSCRI;
	}


	/**
	 * @param squaleSCRI the squaleSCRI to set
	 */
	public void setSqualeSCRI(long squaleSCRI) {
		this.squaleSCRI = squaleSCRI;
	}


	/**
	 * @return the squaleSCCI
	 */
	public long getSqualeSCCI() {
		return squaleSCCI;
	}


	/**
	 * @param squaleSCCI the squaleSCCI to set
	 */
	public void setSqualeSCCI(long squaleSCCI) {
		this.squaleSCCI = squaleSCCI;
	}


	/**
	 * @return the squaleSCEI
	 */
	public long getSqualeSCEI() {
		return squaleSCEI;
	}


	/**
	 * @param squaleSCEI the squaleSCEI to set
	 */
	public void setSqualeSCEI(long squaleSCEI) {
		this.squaleSCEI = squaleSCEI;
	}


	/**
	 * @return the squaleSCSI
	 */
	public long getSqualeSCSI() {
		return squaleSCSI;
	}


	/**
	 * @param squaleSCSI the squaleSCSI to set
	 */
	public void setSqualeSCSI(long squaleSCSI) {
		this.squaleSCSI = squaleSCSI;
	}


	/**
	 * @return the squaleSCMI
	 */
	public long getSqualeSCMI() {
		return squaleSCMI;
	}


	/**
	 * @param squaleSCMI the squaleSCMI to set
	 */
	public void setSqualeSCMI(long squaleSCMI) {
		this.squaleSCMI = squaleSCMI;
	}


	/**
	 * @return the squaleSCPI
	 */
	public long getSqualeSCPI() {
		return squaleSCPI;
	}


	/**
	 * @param squaleSCPI the squaleSCPI to set
	 */
	public void setSqualeSCPI(long squaleSCPI) {
		this.squaleSCPI = squaleSCPI;
	}


	/**
	 * @return the squaleSDTI
	 */
	public double getSqualeSDTI() {
		return squaleSDTI;
	}


	/**
	 * @param squaleSDTI the squaleSDTI to set
	 */
	public void setSqualeSDTI(double squaleSDTI) {
		this.squaleSDTI = squaleSDTI;
	}


	/**
	 * @return the squaleSDRI
	 */
	public double getSqualeSDRI() {
		return squaleSDRI;
	}


	/**
	 * @param squaleSDRI the squaleSDRI to set
	 */
	public void setSqualeSDRI(double squaleSDRI) {
		this.squaleSDRI = squaleSDRI;
	}


	/**
	 * @return the squaleSDCI
	 */
	public double getSqualeSDCI() {
		return squaleSDCI;
	}


	/**
	 * @param squaleSDCI the squaleSDCI to set
	 */
	public void setSqualeSDCI(double squaleSDCI) {
		this.squaleSDCI = squaleSDCI;
	}


	/**
	 * @return the squaleSDEI
	 */
	public double getSqualeSDEI() {
		return squaleSDEI;
	}


	/**
	 * @param squaleSDEI the squaleSDEI to set
	 */
	public void setSqualeSDEI(double squaleSDEI) {
		this.squaleSDEI = squaleSDEI;
	}


	/**
	 * @return the squaleSDSI
	 */
	public double getSqualeSDSI() {
		return squaleSDSI;
	}


	/**
	 * @param squaleSDSI the squaleSDSI to set
	 */
	public void setSqualeSDSI(double squaleSDSI) {
		this.squaleSDSI = squaleSDSI;
	}


	/**
	 * @return the squaleSDMI
	 */
	public double getSqualeSDMI() {
		return squaleSDMI;
	}


	/**
	 * @param squaleSDMI the squaleSDMI to set
	 */
	public void setSqualeSDMI(double squaleSDMI) {
		this.squaleSDMI = squaleSDMI;
	}


	/**
	 * @return the squaleSDPI
	 */
	public double getSqualeSDPI() {
		return squaleSDPI;
	}


	/**
	 * @param squaleSDPI the squaleSDPI to set
	 */
	public void setSqualeSDPI(double squaleSDPI) {
		this.squaleSDPI = squaleSDPI;
	}


	/**
	 * @return the squaleSRTI
	 */
	public double getSqualeSRTI() {
		return squaleSRTI;
	}


	/**
	 * @param squaleSRTI the squaleSRTI to set
	 */
	public void setSqualeSRTI(double squaleSRTI) {
		this.squaleSRTI = squaleSRTI;
	}


	/**
	 * @return the squaleSRRI
	 */
	public double getSqualeSRRI() {
		return squaleSRRI;
	}


	/**
	 * @param squaleSRRI the squaleSRRI to set
	 */
	public void setSqualeSRRI(double squaleSRRI) {
		this.squaleSRRI = squaleSRRI;
	}


	/**
	 * @return the squaleSRCI
	 */
	public double getSqualeSRCI() {
		return squaleSRCI;
	}


	/**
	 * @param squaleSRCI the squaleSRCI to set
	 */
	public void setSqualeSRCI(double squaleSRCI) {
		this.squaleSRCI = squaleSRCI;
	}


	/**
	 * @return the squaleSREI
	 */
	public double getSqualeSREI() {
		return squaleSREI;
	}


	/**
	 * @param squaleSREI the squaleSREI to set
	 */
	public void setSqualeSREI(double squaleSREI) {
		this.squaleSREI = squaleSREI;
	}


	/**
	 * @return the squaleSRSI
	 */
	public double getSqualeSRSI() {
		return squaleSRSI;
	}


	/**
	 * @param squaleSRSI the squaleSRSI to set
	 */
	public void setSqualeSRSI(double squaleSRSI) {
		this.squaleSRSI = squaleSRSI;
	}


	/**
	 * @return the squaleSRMI
	 */
	public double getSqualeSRMI() {
		return squaleSRMI;
	}


	/**
	 * @param squaleSRMI the squaleSRMI to set
	 */
	public void setSqualeSRMI(double squaleSRMI) {
		this.squaleSRMI = squaleSRMI;
	}


	/**
	 * @return the squaleSRPI
	 */
	public double getSqualeSRPI() {
		return squaleSRPI;
	}


	/**
	 * @param squaleSRPI the squaleSRPI to set
	 */
	public void setSqualeSRPI(double squaleSRPI) {
		this.squaleSRPI = squaleSRPI;
	}


	/**
	 * @return the squaleSRTL
	 */
	public EnumMetricsSqualeRating getSqualeSRTL() {
		return squaleSRTL;
	}


	/**
	 * @param squaleSRTL the squaleSRTL to set
	 */
	public void setSqualeSRTL(EnumMetricsSqualeRating squaleSRTL) {
		this.squaleSRTL = squaleSRTL;
	}


	/**
	 * @return the squaleSRRL
	 */
	public EnumMetricsSqualeRating getSqualeSRRL() {
		return squaleSRRL;
	}


	/**
	 * @param squaleSRRL the squaleSRRL to set
	 */
	public void setSqualeSRRL(EnumMetricsSqualeRating squaleSRRL) {
		this.squaleSRRL = squaleSRRL;
	}


	/**
	 * @return the squaleSRCL
	 */
	public EnumMetricsSqualeRating getSqualeSRCL() {
		return squaleSRCL;
	}


	/**
	 * @param squaleSRCL the squaleSRCL to set
	 */
	public void setSqualeSRCL(EnumMetricsSqualeRating squaleSRCL) {
		this.squaleSRCL = squaleSRCL;
	}


	/**
	 * @return the squaleSREL
	 */
	public EnumMetricsSqualeRating getSqualeSREL() {
		return squaleSREL;
	}


	/**
	 * @param squaleSREL the squaleSREL to set
	 */
	public void setSqualeSREL(EnumMetricsSqualeRating squaleSREL) {
		this.squaleSREL = squaleSREL;
	}


	/**
	 * @return the squaleSRSL
	 */
	public EnumMetricsSqualeRating getSqualeSRSL() {
		return squaleSRSL;
	}


	/**
	 * @param squaleSRSL the squaleSRSL to set
	 */
	public void setSqualeSRSL(EnumMetricsSqualeRating squaleSRSL) {
		this.squaleSRSL = squaleSRSL;
	}


	/**
	 * @return the squaleSRML
	 */
	public EnumMetricsSqualeRating getSqualeSRML() {
		return squaleSRML;
	}


	/**
	 * @param squaleSRML the squaleSRML to set
	 */
	public void setSqualeSRML(EnumMetricsSqualeRating squaleSRML) {
		this.squaleSRML = squaleSRML;
	}


	/**
	 * @return the squaleSRPL
	 */
	public EnumMetricsSqualeRating getSqualeSRPL() {
		return squaleSRPL;
	}


	/**
	 * @param squaleSRPL the squaleSRPL to set
	 */
	public void setSqualeSRPL(EnumMetricsSqualeRating squaleSRPL) {
		this.squaleSRPL = squaleSRPL;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return  typeObject.toString() + ":" + idObject + " Sys:" + this.system + " SubSys:" + this.subSystem;
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		EntityMetricValue objEntityMetric = null;
		
		objEntityMetric = (EntityMetricValue) obj;
		 
		return this.system.equals(objEntityMetric.system)
		 &&    this.subSystem.equals(objEntityMetric.subSystem)
		 &&    this.idObject.equals(objEntityMetric.idObject)
		 &&    this.typeObject == objEntityMetric.typeObject
		 &&    this.section.equals(objEntityMetric.section);
		 
	}


}
