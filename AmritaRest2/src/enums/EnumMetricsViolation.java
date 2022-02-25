package enums;

import analyzer.DataBaseMappedEnumeration;
/**
  * Copyright (c) 2009-2011 e-Amrita - Ing. Giampietro Zedda    Turin (ITALY)
  *+
  * <h1>
  * EnumMetricsViolation	
  * </h1>
  *  <p>
  * Questa enum elenca le possibili categorie di violazioni qualitative.<br>
  * <p>
   * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 28/08/2011
  * @see Metrics
  *  
*/
@DataBaseMappedEnumeration
public enum EnumMetricsViolation {
	
	 NOT_ASSIGNED(), 																	 

     R0001_AVOID_GOTO_OUTSIDE_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 240, EnumMetricsViolationFixUnit.MINUTE),     				
     R0002_AVOID_GOTO_BACK_SAME_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_RELIABILITY,EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),    				
     R0003_AVOID_GOTO_OUTSIDE_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 240, EnumMetricsViolationFixUnit.MINUTE),    				
     R0004_AVOID_GOTO_BACK_SAME_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),    				
     R0005_AVOID_GOTO_FWD_TO_END(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
     R0006_AVOID_GOTO_FWD_NOT_TO_END(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0007_AVOID_RECURSIVE_PERFORM(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),				   
     R0008_AVOID_EMPTY_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0009_AVOID_UNDOCUMENTED_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),      				
     R0010_AVOID_LOW_DOCUMENTED_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.COB_COMM_PARAGRAPH, 30, EnumMetricsViolationFixUnit.MINUTE),         	
     R0011_AVOID_LOW_PARAGRAPH_PERC_COMM_BY_INSTR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PARAGRAPH_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE),
     R0012_AVOID_LOW_PARAGRAPH_PERC_COMM_BY_ROW(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PARAGRAPH_BY_ROW, 30, EnumMetricsViolationFixUnit.MINUTE),	
     R0013_AVOID_HIGH_PARAGRAPH_INSTR_SIZE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_PARAGRAPH_INSTR, 120, EnumMetricsViolationFixUnit.MINUTE), 	
     R0014_AVOID_HIGH_MCBE_PARAGRAPH_COMPLEXITY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_MCBE_SECTION_PARAGRAPH, 480, EnumMetricsViolationFixUnit.MINUTE),  
     R0015_AVOID_SECTION_UNREFERENCED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0016_AVOID_EMPTY_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),				 
     R0017_AVOID_SECTION_WITH_NO_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),					 
     R0018_AVOID_UNDOCUMENTED_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),        			 
     R0019_AVOID_LOW_DOCUMENTED_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_COMM_SECTION, 30, EnumMetricsViolationFixUnit.MINUTE),         		 
     R0020_AVOID_LOW_SECTION_PERC_COMM_BY_INSTR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_SECTION_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE),	 
     R0021_AVOID_LOW_SECTION_PERC_COMM_BY_ROW(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_SECTION_BY_ROW, 30, EnumMetricsViolationFixUnit.MINUTE),	 
     R0022_AVOID_HIGH_SECTION_INSTR_SIZE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY,EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_SECTION_INSTR, 120, EnumMetricsViolationFixUnit.MINUTE),      
     R0023_AVOID_HIGH_MCBE_SECTION_COMPLEXITY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL,EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_MCBE_SECTION_PARAGRAPH, 480, EnumMetricsViolationFixUnit.MINUTE),  	
     R0024_AVOID_HIGH_PERFORM_INNER_INSTR_SIZE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY,EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_PERFORM_INNER_INSTR, 120, EnumMetricsViolationFixUnit.MINUTE),	
     R0025_AVOID_HIGH_LEVEL_NESTING_IF(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY,EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_NESTING_IF, 4, EnumMetricsViolationFixUnit.MINUTE),				 
     R0026_AVOID_LABEL_UNREFERENCED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0027_AVOID_LABEL_REFERENCED_DUPLICATED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0028_AVOID_LABEL_UNREFERENCED_DUPLICATED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY,EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE), 					 
     R0029_AVOID_COMMIT_INSIDE_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 240, EnumMetricsViolationFixUnit.MINUTE),					 
     R0030_AVOID_FILE_OPEN_CLOSE_INSIDE_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),					 
     R0031_AVOID_INDEXING_NO_USAGE_INDEX_INSIDE_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),					 
     R0032_AVOID_ARITHMETIC_OPERAND_ZONED_INSIDE_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR,EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE), 					 
     R0033_AVOID_INDEXING_NO_USAGE_INDEX_NO_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),		 
     R0034_AVOID_ARITHMETIC_OPERAND_ZONED_NO_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),			 
     R0035_AVOID_PERFORM_VARYING_K_ZONED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),		 
     R0036_AVOID_BLOCK_N_RECORDS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),			 
     R0037_AVOID_CALL_STATIC(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
/*W*/R0038_AVOID_CALL_DYNAMIC_LARGE_PGM_INSIDE_LOOP(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0039_AVOID_FILE_OPEN_SINGLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),				 
     R0040_AVOID_INSTR_FORBIDDEN_FOR_CICS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.INSTRUCTION, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 480, EnumMetricsViolationFixUnit.MINUTE),		// In programma Cobol/Cics ACCEPT/ENTRY/DISPLAY/FD/SD/SELECT/OPEN/CLOSEREAD/WRITER/REWRITE/DELETE/MERGE/SORT/START/STOP
     R0041_AVOID_INSTR_SELECT_FOR_CICS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.INSTRUCTION, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 960, EnumMetricsViolationFixUnit.MINUTE),		    	 
     R0042_AVOID_INSTR_FD_SD_FOR_CICS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.INSTRUCTION, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),				 
     R0043_AVOID_BYNARY_FIELDS_NO_SYNC(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0044_AVOID_MERGE_STATEMENT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 240, EnumMetricsViolationFixUnit.MINUTE),   			 
     R0045_AVOID_ALTER_STATEMENT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),					  
     R0046_AVOID_GOTO_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.D_CRITICAL,EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),				   
     R0047_AVOID_GOTO_FROM_OUTSIDE_TO_LABEL_INSIDE_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),				   	
     R0048_AVOID_CICS_HANDLE_ABEND(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR,EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),                	 
     R0049_AVOID_CICS_HANDLE_CONDITION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),            		 
     R0050_AVOID_CICS_HANDLE_AID(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),            		 
     R0051_AVOID_CICS_IGNORE_CONDITION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),            		 
     R0052_AVOID_FILE_OPEN_NOT_CLOSED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),					 
     R0053_AVOID_FILE_DEFINED_NOT_USED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0054_AVOID_FILE_OPEN_NOT_USED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),				 
     R0055_AVOID_DISPLAY_UPON_CONSOLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.API_ABUSE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 0, EnumMetricsViolationFixUnit.MINUTE),				 
     R0056_Free(true, EnumMetricsRuleType.NOT_ASSIGNED, EnumMetricsQualityCharacteristics.NOT_ASSIGNED, EnumMetricsViolationSeverity.NOT_ASSIGNED, EnumThresholds.NOT_ASSIGNED, 0, EnumMetricsViolationFixUnit.NOT_ASSIGNED),				 
     R0057_AVOID_READ_WITHOUT_AT_END(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),				 
     R0058_AVOID_SELECT_WITHOUT_FILE_STATUS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),					 
     R0059_AVOID_EVALUATE_WITHOUT_WHEN_OTHER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),				 
     R0060_AVOID_SQLCA_NOT_INCLUDED_IN_SQL_PGM(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE), 				 
     R0061_AVOID_MOVE_REFERENCE_MODIFICATION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),         		 
     R0062_AVOID_CORRESPONDING_OPTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),         		 
     R0063_AVOID_MOVE_TRUNCATED(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),     			 
     R0064_AVOID_PERFORM_THRU(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),         		 
     R0065_AVOID_PERFORM_THRU_PARAGRAPH_NOT_SINGLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_RELIABILITY, EnumMetricsViolationSeverity.D_CRITICAL, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),				 
     R0066_AVOID_EVALUATE_CLOSED_BY_DOT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.C_MAJOR,EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),        			 
     R0067_AVOID_IF_CLOSED_BY_DOT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),     				 
     R0068_AVOID_HIGH_SECTIONS_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SECTIONS, 480, EnumMetricsViolationFixUnit.MINUTE),         			 
     R0069_AVOID_HIGH_PARAGRAPHS_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR,EnumThresholds.COB_PARAGRAPHS, 480, EnumMetricsViolationFixUnit.MINUTE),     			 
     R0070_AVOID_NEXT_SENTENCE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),         		 
     R0071_AVOID_HIGH_PGM_ROWS_SOURCE_SIZE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_SOURCE_ROWS, 480, EnumMetricsViolationFixUnit.MINUTE),			 
     R0072_AVOID_HIGH_PGM_INSTR_SIZE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_INSTR, 240, EnumMetricsViolationFixUnit.MINUTE),     			 
     R0073_AVOID_UNDOCUMENTED_PGM_TITLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),        			 
     R0074_AVOID_LOW_DOCUMENTED_PGM_TITLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_COMM_PGM, 30, EnumMetricsViolationFixUnit.MINUTE),         	     
     R0075_AVOID_LOW_PGM_PERC_COMM_BY_ROW(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PROC_BY_ROW, 30, EnumMetricsViolationFixUnit.MINUTE),   
     R0076_AVOID_LOW_PGM_PERC_COMM_BY_INSTR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PROC_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE), 	 
     R0077_AVOID_UNCHECKED_FILE_STATUS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PROC_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE),  
     R0078_AVOID_UNCHECKED_CICS_OPERATION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_PROC_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE), 	
     R0079_AVOID_UNUSED_PGM_FIELD(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0080_AVOID_DEAD_COPY_OR_SQL_INCLUDE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, 	EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0081_AVOID_DEAD_CODE_UNREACHABLE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, 	EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0082_AVOID_DEAD_SECTION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0083_AVOID_DEAD_PARAGRAPH(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 15, EnumMetricsViolationFixUnit.MINUTE),    				 
     R0084_AVOID_UNINITIALIZED_PGM_DATA_FIELD(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_RELIABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0085_AVOID_UNINITIALIZED_COPY_FIELD(true, EnumMetricsRuleType.FIXED, EnumMetricsQualityCharacteristics.DATA_RELIABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					 
     R0086_AVOID_LOW_PERC_FIELD_COMM_BY_ROW(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_DATA_BY_ROW, 30, EnumMetricsViolationFixUnit.MINUTE),     	 
     R0087_AVOID_LOW_PERC_FIELD_COMM_BY_INSTR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_PERC_COMM_DATA_BY_INSTR, 30, EnumMetricsViolationFixUnit.MINUTE),   	 
     R0088_AVOID_HIGH_MCBE_PGM_COMPLEXITY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_MCBE_TOT_PGM, 480, EnumMetricsViolationFixUnit.MINUTE),            	 
     R0089_AVOID_COBOL_DIVISION_STMT_INSIDE_COPY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),            	 
/*W*/R0090_AVOID_HIGH_PGM_FAN_IN(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.FAN_IN, 480, EnumMetricsViolationFixUnit.MINUTE),          			 
/*W*/R0091_AVOID_HIGH_PGM_FAN_OUT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.FAN_IN, 480, EnumMetricsViolationFixUnit.MINUTE),          			 
     R0092_AVOID_UNCHECKED_ALPHA_TO_NUM_MOVE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.INPUT_VALIDATION, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),		
     R0093_AVOID_CICS_COMMAREA_AND_LENGTH_MISSING(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),		 
     R0094_AVOID_GOBACK_NOT_IN_MAINLINE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),			
     R0095_AVOID_USE_OF_POINTERS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),			
     R0096_AVOID_PARAGRAPH_NOT_ENDED_BY_EXIT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),		    		 
     R0097_AVOID_SECTION_NOT_ENDED_BY_EXIT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),		    	 
/*W*/R0098_AVOID_NESTED_PGMS(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),      
     R0099_AVOID_NESTED_COPY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),    		 
     R0100_AVOID_COPY_ACROSS_DIVISION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY,EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),		 
     R0101_AVOID_ID_DIVISION_MISSING(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LANGUAGE, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),				 
     R0102_AVOID_ENV_DIVISION_MISSING(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LANGUAGE, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0103_AVOID_DATA_DIVISION_MISSING(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LANGUAGE, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0104_AVOID_SOURCE_ROW_TABSET(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.COMPILER, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),    		 		
     R0105_AVOID_UNCLOSED_FIELD_DEF_BY_DOT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LANGUAGE, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),	 
     R0106_AVOID_INSTR_START_BEFORE_AREA_A(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.COMPILER, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),	 
     R0107_AVOID_LABEL_AT_COL_GREATER_8(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.COMPILER, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0108_AVOID_NOT_STD_SIZE_LABEL(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_SIZE_LABEL, 15,EnumMetricsViolationFixUnit.MINUTE),		 
     R0109_AVOID_NOT_STD_SIZE_FIELD_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.COB_SIZE_FIELD_NAME, 15, EnumMetricsViolationFixUnit.MINUTE),  
     R0110_AVOID_NOT_STD_PGM_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),	 
     R0111_AVOID_NOT_STD_SECTION_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE),	 
     R0112_AVOID_NOT_STD_PARAGRAPH_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 30, EnumMetricsViolationFixUnit.MINUTE), 
     R0113_AVOID_NOT_STD_LABEL_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),	 
     R0114_AVOID_NOT_STD_FIELD_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),	
     R0115_AVOID_NOT_STD_PARAGRAPH_THRU_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		
     R0116_AVOID_NOT_STD_SECTION_THRU_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.A_INFO, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		
     R0117_AVOID_STOP_RUN_NOT_IN_MAINLINE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),			
     R0118_AVOID_PROGRAM_ID_UNEQUAL_FILE_NAME(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 60, EnumMetricsViolationFixUnit.MINUTE),		
     R0119_AVOID_HIGH_CONDITIONS_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_CONDITIONS, 5, EnumMetricsViolationFixUnit.MINUTE),					
     R0120_AVOID_COPY_INSIDE_PROCEDURE_DIVISION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.READABILTY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0121_AVOID_INITIALIZE_STATEMENT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0122_AVOID_REDEFINES_CLAUSE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
     R0123_AVOID_SECTION_IN_PROCEDURE(true, EnumMetricsRuleType.FIXED, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, 	EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0124_AVOID_PARAGRAPH_IN_PROCEDURE(true, EnumMetricsRuleType.FIXED, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0125_AVOID_VALUE_CLAUSE_IN_LINKAGE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.INSTRUCTION, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
     R0126_AVOID_LINKAGE_DATA_NOT_IN_COPYBOOK(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
/*W*/R0127_AVOID_BLOCK_CODE_DUPLICATED(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_BLOCK_DUPLICATED, 2, EnumMetricsViolationFixUnit.MINUTE),		
/*W*/R0128_AVOID_BLOCK_CODE_COMMENTED(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.COB_SIZE_BLOCK_COMMENTED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0129_AVOID_EVALUATE_WHEN_WITH_LOGIC_CONDITIONAL(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0130_AVOID_MAGIC_LITERAL_ALPHANUMERIC(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0131_AVOID_MAGIC_LITERAL_NUMERIC(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.LOGIC_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
     R0132_AVOID_UNINITIALIZED_CALL_PARAMETER_DEFINED_INSIDE_COPY(true, EnumMetricsRuleType.FIXED, EnumMetricsQualityCharacteristics.FAULT_TOLERANCE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0133_AVOID_UNINITIALIZED_CALL_PARAMETER_DEFINED_INSIDE_PGM(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.FAULT_TOLERANCE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),				
     R0134_AVOID_UNINITIALIZED_CALL_PARAMETER_AREA_COPY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.FAULT_TOLERANCE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
     R0135_AVOID_COMPUTE_FOR_SIMPLE_OPERATIONS(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0136_AVOID_IF_TRUE_EMPTY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),		
     R0137_AVOID_IF_ELSE_EMPTY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),		
     R0138_AVOID_SORT_STATEMENT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNIT_LEVEL, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),					
	 R0139_Free(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.NOT_ASSIGNED, EnumMetricsViolationSeverity.NOT_ASSIGNED, EnumThresholds.NOT_ASSIGNED, 0, EnumMetricsViolationFixUnit.NOT_ASSIGNED),		
     R0140_AVOID_SQL_SELECT_STAR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),
     R0141_AVOID_SQL_USE(true, EnumMetricsRuleType.FIXED, EnumMetricsQualityCharacteristics.ARCHITECTURE_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),			
     R0142_AVOID_SQL_OPEN_CURSOR_INSIDE_LOOP(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		
     R0143_AVOID_SQL_UNCLOSED_CURSOR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),				
     R0144_AVOID_SQL_DECLARED_AND_UNUSED_CURSOR(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),					
     R0145_AVOID_SQL_HIGH_WHERE_CONDITIONS_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.UNDERSTANDABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.SQL_WHERE_CONDITIONS, 5, EnumMetricsViolationFixUnit.MINUTE),			
     R0146_AVOID_SQL_HIGH_TABLES_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.SQL_TABLES_ACCESSED, 120, EnumMetricsViolationFixUnit.MINUTE),		    		
     R0147_AVOID_SQL_HIGH_SUBSELECT_NESTED_NUMBER(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.SQL_SELECT_NESTED, 120, EnumMetricsViolationFixUnit.MINUTE),		    	
     R0148_AVOID_SQL_HIGH_TABLES_JOINED_NUMBER(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.SQL_TABLES_JOINED, 120, EnumMetricsViolationFixUnit.MINUTE),		    		
     R0149_AVOID_SQL_DECLARE_CURSOR_WITH_NO_ORDER_BY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.COMPILER, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),					
     R0150_AVOID_SQL_SELECT_WITH_UNION(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),					
     R0151_AVOID_SQL_SELECT_WITH_DISTINCT(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0152_AVOID_SQL_SELECT_WITH_GROUP_BY(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.DATA_CHANGEABILITY, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),			
     R0153_AVOID_SQL_DYNAMIC(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 120, EnumMetricsViolationFixUnit.MINUTE),					
     R0154_AVOID_SQL_SELECT_WITH_NO_WHERE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0155_AVOID_SQL_DELETE_WITH_NO_WHERE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0156_AVOID_SQL_UPDATE_WITH_NO_WHERE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),		 
     R0157_AVOID_SQL_WHERE_WITH_LIKE_PREDICATE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.MEMORY_USE, EnumMetricsViolationSeverity.B_MINOR, 	EnumThresholds.NOT_ASSIGNED, 2, EnumMetricsViolationFixUnit.MINUTE),	
     R0158_AVOID_SQL_WITH_DEPRECATED_SYNTAX(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.COMPILER, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 5, EnumMetricsViolationFixUnit.MINUTE),	 
     R0159_AVOID_SQL_UNCHECKED_CODE(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.EXCEPTION_HANDLING, EnumMetricsViolationSeverity.B_MINOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),				 
/*W*/R0160_AVOID_SQL_ORDER_BY_WITH_NO_INDEX(true, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE),			 
/*W*/R0161_AVOID_SQL_TEMPORARY_TABLE(false, EnumMetricsRuleType.LINEAR, EnumMetricsQualityCharacteristics.PROCESSOR_USE, EnumMetricsViolationSeverity.C_MAJOR, EnumThresholds.NOT_ASSIGNED, 10, EnumMetricsViolationFixUnit.MINUTE);				 
	
	
	// Variabili di istanza
	EnumMetricsViolationSeverity metricViolationSeverity = EnumMetricsViolationSeverity.NOT_ASSIGNED;
    EnumThresholds threshold = EnumThresholds.NOT_ASSIGNED;														// Valore di soglia 
    EnumMetricsViolationFixUnit fixUnit = EnumMetricsViolationFixUnit.NOT_ASSIGNED;								// Unità remediation cost
    EnumMetricsQualityCharacteristics qualityCharacteristic = EnumMetricsQualityCharacteristics.NOT_ASSIGNED; 	// Caratteristica di qualità relativa alla violazione
    EnumMetricsRuleType ruleType = EnumMetricsRuleType.NOT_ASSIGNED;
    boolean ruleEnabled = true;	
    int remediationCost = 0;				// Costo di correzione in unita di remediation cost
	int totRemediationCost = 0;			    // Costo di correzione totale in giorni per tutte le violazioni individuate																								
    
	// Costruttore vuoto
	private EnumMetricsViolation() {
	}

	// Costruttore con soglia e remediation cost
	private EnumMetricsViolation(boolean ruleEnabled, EnumMetricsRuleType ruleType, EnumMetricsQualityCharacteristics qualityCharacteristic, EnumMetricsViolationSeverity metricViolationSeverity, EnumThresholds threshold, int remediationCost, EnumMetricsViolationFixUnit fixUnit) {
		this.ruleEnabled = ruleEnabled;
		this.ruleType = ruleType;
		this.qualityCharacteristic = qualityCharacteristic;
		this.metricViolationSeverity = metricViolationSeverity;
		this.threshold = threshold;
		this.remediationCost = remediationCost;
		this.fixUnit = fixUnit;

	}

	
	/**
	 * Restituisce se la regola è abilitata.<br>
	 * <p>
	 * 
	 *  @return the ruleEnabled
	 */
	public boolean isRuleEnabled() {
		return ruleEnabled;
	}

	/**
	 * Imposta se la regola è abilitata.<br>
	 * <p>
	 * @param ruleEnabled the ruleEnabled to set
	 */
	public void setRuleEnabled(boolean ruleEnabled) {
		this.ruleEnabled = ruleEnabled;
	}

	/**
	 * Restituisce il livello di severità della violazione.
	 * <br>
	 * @return the metricViolationSeverity
	 */
	public EnumMetricsViolationSeverity getMetricViolationSeverity() {
		return metricViolationSeverity;
	}

	/**
	 * Imposta il livello di severità della violazione.
	 * <br>
	 * @param metricViolationSeverity the metricViolationSeverity to set
	 */
	public void setMetricViolationSeverity(EnumMetricsViolationSeverity metricViolationSeverity) {
		this.metricViolationSeverity = metricViolationSeverity;
	}

	/**
	 * Restituisce l'enumerazione descrivente la soglia, minima o massima, associata alla violazione.<br>
	 * zero indica nessuna soglia.<br>
	 * <p>
	 * @return the threshold
	 */
	public EnumThresholds getThreshold() {
		return threshold;
	}

	/**
	 * Restituisce l'enumerazione descrivente la soglia, minima o massima, associata alla violazione.<br>
	 * zero indica nessuna soglia.<br>
	 * <p>
	 * @param threshold the threshold to set
	 */
	public void setThreshold(EnumThresholds threshold) {
		this.threshold = threshold;
	}

	/**
	 * Restituisce la tipologia di unità di tempo di remediation cost.<br>
	 * <p>
	 * @return the fixUnit
	 */
	public EnumMetricsViolationFixUnit getFixUnit() {
		return fixUnit;
	}

	/**
	 * Imposta la tipologia di unità di tempo di remediation cost.<br>
	 * <p>
	 * @param fixUnit the fixUnit to set
	 */
	public void setFixUnit(EnumMetricsViolationFixUnit fixUnit) {
		this.fixUnit = fixUnit;
	}

	/**
	 * Restituisce la caratteristica di qualità correlata alla violazione<br>
	 * <p>
	 * @return the qualityCharacteristic
	 */
	public EnumMetricsQualityCharacteristics getQualityCharacteristic() {
		return qualityCharacteristic;
	}

	/**
	 * Imposta la caratteristica di qualità correlata alla violazione<br>
	 * <p>
	 * @param qualityCharacteristic the qualityCharacteristic to set
	 */
	public void setQualityCharacteristic(EnumMetricsQualityCharacteristics qualityCharacteristic) {
		this.qualityCharacteristic = qualityCharacteristic;
	}

	/**
	 * Restituisce il costo di remediation della violazione in unità di remediation cost<br>
	 * <p>
	 * @return the remediationCost
	 */
	public int getRemediationCost() {
		return remediationCost;
	}

	/**
	 * Restituisce il costo di remediation della violazione in giorni<br>
	 * <p>
	 * @return the remediationCost
	 */
	public double getRemediationCostDays() {
		double remediationCostDays = 0;
		// TODO
		return remediationCostDays;
	}

	/**
	 * Restituisce il costo di remediation della violazione in minuti<br>
	 * <p>
	 * @return the remediationCost
	 */
	public Integer getRemediationCostMinutes() {
		if (this.fixUnit == EnumMetricsViolationFixUnit.MINUTE) {
			return this.remediationCost;
		}
		if (this.fixUnit == EnumMetricsViolationFixUnit.HOUR) {
			return this.remediationCost*60;
		}
		if (this.fixUnit == EnumMetricsViolationFixUnit.DAY) {
			return this.remediationCost*60*8;
		}
		if (this.fixUnit == EnumMetricsViolationFixUnit.MONTH) {
			return this.remediationCost*60*8*21;
		}
		return 0;
	}

	/**
	 * Imposta il costo di remediation della violazione in unità di remediation cost<br>
	 * <p>
	 * @param remediationCost the remediationCost to set
	 */
	public void setRemediationCost(int remediationCost) {
		this.remediationCost = remediationCost;
	}
	
	/**
	 * Restituisce il totale in giorni del remediation cost <br>
	 * della caratteristica di qualità.<br>
	 * <p>
	 * @return the totRemediationCost
	 */
	public int getTotRemediationCost() {
		return totRemediationCost;
	}

	/**
	 * Imposta il totale in giorni del remediation cost <br>
	 * della caratteristica di qualità.<br>
	 * <p>
	 * @param totRemediationCost the totRemediationCost to set
	 */
	public void setTotRemediationCost(int totRemediationCost) {
		this.totRemediationCost = totRemediationCost;
	}
	

}