package utilities;

import enums.EnumCobolReservedWords;
import enums.EnumCobolUsage;
import enums.EnumDataItemGeneric;
import enums.EnumDataItemOption;
import enums.EnumDataItemSystemEnvironment;
import enums.EnumDataItemType;
import enums.EnumDateFormat;
import enums.EnumDirectivesExecution;
import enums.EnumExpressionItem;
import enums.EnumIndexOrder;
import enums.EnumInstrDataCategory;
import enums.EnumInstrStatus;
import enums.EnumLanguage;
import enums.EnumLanguageItemInstr;
import enums.EnumLogicSetMode;
import enums.EnumLogicSetPointerArea;
import enums.EnumMapTypeField;
import enums.EnumMessageType;
import enums.EnumMetricsQualityCharacteristics;
import enums.EnumMetricsQualitySections;
import enums.EnumMetricsScope;
import enums.EnumMetricsSqualeRating;
import enums.EnumMetricsViolation;
import enums.EnumMetricsViolationFixUnit;
import enums.EnumMetricsViolationSeverity;
import enums.EnumObject;
import enums.EnumObjectOption;
import enums.EnumObjectStatus;
import enums.EnumPrecompilerOptionsInstruction;
import enums.EnumPrecompilerReservedWords;
import enums.EnumProcessStatus;
import enums.EnumRelation;
import enums.EnumRelationOriginInstr;
import enums.EnumRelationSourceProcess;
import enums.EnumRelationType;
import enums.EnumScopeSection;
import enums.EnumSourceType;
import enums.EnumSymbolType;
import enums.EnumSymbolUsedAs;
import enums.EnumSymbolUsedBy;
import enums.EnumTable;
import enums.EnumTypeProcessAnalysis;
import enums.EnumWhereUsedType;
import enums.EnumWhereUsedTypeAlias;;

public class CreateInsertEnums<E> {

	public static void main(String[] args) {
// 		genIns(EnumTable.values(),                          0, "EnumTable", "Master index - Elenco generale tabelle definite nel sistema");
// 		genIns(EnumObject.values(),                         1, "EnumObject", "Oggetti");
//		genIns(EnumSourceType.values(),                     2, "EnumSourceType", "Tipo sorgente");
//		genIns(EnumObjectStatus.values(),                   3, "EnumObjectStatus", "Stato oggetto");	
// 		genIns(EnumObjectOption.values(),                   4, "EnumObjectOption", "Opzioni oggetto");
//  	genIns(EnumRelationType.values(),                   5, "EnumRelationType", "Tipologia Relazioni");
//  	genIns(EnumRelationSourceProcess.values(),          6, "EnumRelationSourceProcess", "Processo sorgente relazione");
//  	genIns(EnumLanguageItemInstr.values(),              7, "EnumLanguageItemInstr", "Linguaggio item/istruzione (Cobol, Pl1, Sql, ..)	");
//		genIns(EnumDataItemType.values(),                   8, "EnumDataItemType", "Tipo data item	");
//		genIns(EnumPrecompilerReservedWords.values(),       9, "EnumPrecompilerReservedWords", "Tipo istruzione per tutti i linguaggi/operandi/opzioni	"); 		
//		genIns(EnumWhereUsedType.values(),                 10, "EnumWhereUsedType", "Tipo where used (input/output)");
//		genIns(EnumWhereUsedTypeAlias.values(),            11, "EnumWhereUsedTypeAlias", "Tipo alias where used (FULL_MATCH, LOWER_MATCH, etc.)");
//		genIns(EnumIndexOrder.values(),                    12, "EnumIndexOrder","Ordinamento indice");
//		genIns(EnumProcessStatus.values(),                 13, "EnumProcessStatus","Stato processi e funzioni in esecuzione");
//		genIns(EnumDataItemGeneric.values(),               14, "EnumDataItemGeneric","Generico data item senza implementazione specifica (numerico, text, condizione)");  	
//		genIns(EnumPrecompilerOptionsInstruction.values(), 15, "EnumPrecompilerOptionsInstruction","Opzioni istruzioni precompiler, come cics ????? inserire su db");
// 		genIns(EnumCobolUsage.values(),                    16, "EnumCobolUsage","Usage cobol ");
// 		genIns(EnumExpressionItem.values(),                17, "EnumExpressionItem"," Cobol/Sqloperandi e operatori di una espressione/Condizione");  
  	genIns(EnumInstrDataCategory.values(),             18, "EnumInstrDataCategory","Categoria istruzione (procedure/data/jcl)");
//  	genIns(EnumDataItemOption.values(),                19, "EnumDataItemOption","Opzioni di un data item");
//      genIns(EnumSymbolType.values(),                    20, "EnumSymbolType","Categoria simboli di programma");
//  	genIns(EnumDataItemSystemEnvironment.values(),     21, "EnumDataItemSystemEnvironment","Data item dipendenti dall'ambiente operativo (Cics\", 0), ..)");
//  	genIns(EnumDateFormat.values(),                    22, "EnumDateFormat","Formati data"); 		  		
//		genIns(EnumInstrStatus.values(),                   23, "EnumInstrStatus","EnumInstrStatus");
//		genIns(EnumMessageType.values(),                   24, "EnumMessageType","Tipo messaggio (Info, Error, ...)");
//		genIns(EnumTypeProcessAnalysis.values(),           25, "EnumTypeProcessAnalysis","Tipo processo di analisi attivo");
//No    genIns(EnumLogicSetType.values(),                  26, "EnumLogicSetType","Tipologia ultima impostazione campo elementare");
//		genIns(EnumLogicSetMode.values(),                  27, "EnumLogicSetMode","Tipologia valorizzazioni in trasformazioni campi elementari");
//   	genIns(EnumLogicSetPointerArea.values(),           28, "EnumLogicSetPointerArea","Tipologie di aree dove è situato il pointer dell'area di Linkage");
//		genIns(EnumCobolReservedWords.values(),            29, "EnumCobolReservedWords","Tipologie istruzioni in programmi Cobol");
//		genIns(EnumDirectivesExecution.values(),           33, "EnumDirectivesExecution","Direttive di esecuzione processi e funzioni");
//   	genIns(EnumRelation.values(),                      34, "EnumRelation", "Relazioni fra oggetti");	  		
//		genIns(EnumRelationOriginInstr.values(),           35, "EnumRelationOriginInstr","Istruzione origine della relazione");
//		genIns(EnumSymbolType.values(),                    37, "EnumSymbolType","Tipologia simbolo di programma");
//		genIns(EnumSymbolUsedAs.values(),                  38, "EnumSymbolUsedAs","Tipologia utilizzo simbolo di programma");
//		genIns(EnumSymbolUsedBy.values(),                  39, "EnumSymbolUsedBy","Tipologia utilizzatore simbolo di programma	");
//		genIns(EnumLanguage.values(),                      40, "EnumLanguage","Linguaggio stessa codifica locale (it minuscolo)");
//		genIns(EnumMetricsSqualeRating.values(),           41, "EnumMetricsSqualeRating","Squale livelli di rating A-E");
//		genIns(EnumMetricsQualitySections.values(),        42, "EnumMetricsQualitySections","Sezioni qualità da tabella MetricValue");
//		genIns(EnumMapTypeField.values(),                  43, "EnumMapTypeField","Tipologia campo mappa bms");
//		genIns(EnumScopeSection.values(),                  44, "EnumScopeSection","Tipologia scope (campo di validita) ");
//		genIns(EnumMetricsScope.values(),                  45, "EnumMetricsScope","Tipologia scope per metriche");
//		genIns(EnumMetricsViolation.values(),              46, "EnumMetricsViolation","Violazioni metriche");
//		genIns(EnumMetricsViolationSeverity.values(),      47, "EnumMetricsViolationSeverity","Severità violazioni metriche");
//		genIns(EnumMetricsViolationFixUnit.values(),       48, "EnumMetricsViolationFixUnit","Costo remediation violazioni");
//		genIns(EnumMetricsQualityCharacteristics.values(), 49, "EnumMetricsQualityCharacteristics","Caratteristiche qualità metriche");
  		/*
  		*/
	}


	private static void genIns(Enum[] areo, int numTable, String enumName, String descTable) {
		
	   int i=0;
	   String seq = "";
	   
	   System.out.println("delete from tabledata where numTable=" + numTable + ";");
	   System.out.println("delete from TableHeader where numTable=" + numTable + ";");
	   System.out.println("insert into TableHeader (sys,subSys,numTable,descTb,tableSystem,tableStructured,enumJava,dtCreation,dtUpdate,tmCreation,tmUpdate)" 
       +" VALUES('*','*'," + numTable + ",'" + descTable + "',true,false,'" + enumName + "','','','','');");

       for (Enum eo : areo) {
			seq = "000" + i;
			seq = seq.substring(seq.length() - 4);
			System.out.println("insert into tabledata  (sys,subSys,numTable,language,keySeq,keyVal,rowData) values('*','*'," + numTable + ", 1, '" + seq + "','" 
			    +eo.ordinal() + "'," + "'" + eo.toString() +"');");
			 i++;
		
	   }
       System.out.println("commit;");
	   System.out.println("");
	}
	
	
}

