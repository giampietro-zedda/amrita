package enums;

/**
  * copyright (c) 2009 e-Amrita - Giampietro Zedda    Turin (ITALY)
  *
  * <h1>
  * EnumDataItemOption	
  * </h1>
  *  <p>
  * Questa enum elenca le opzioni possibili per un data item.
  * 
  * 
  * @author Giampietro Zedda
  * @version 1.0.0
  * @since 14/03/2010
  * @see Analyzer
  * @see EnumObject
  * @see EnumInstrTypeSql
  * 
*/
public enum EnumDataItemOption {
	
	NOT_ASSIGNED,           			// 00 Di servizio 
	
	// Opzioni principali
	COBOL_OPT_DATA_NAME,           		// 01     
	COBOL_OPT_FILLER,           		// 02      
	COBOL_OPT_RENAMES,          		// 03    
	COBOL_OPT_RENAMES_THRU,         	// 04           
	COBOL_OPT_REDEFINES,           		// 05                			 
	COBOL_OPT_PICTURE,           		// 06                       
	COBOL_OPT_BLANK_WHEN_ZERO,      	// 07  
	COBOL_OPT_VALUE,           			// 08                     
	COBOL_OPT_LIKE,           			// 09   
	COBOL_OPT_LIKE_INTEGER,         	// 10             
	COBOL_OPT_CONDITION_NAME,       	// 11             
	COBOL_OPT_EXTERNAL,           		// 12                      
	COBOL_OPT_GLOBAL,           		// 13                     
	COBOL_OPT_SIGNED,           		// 14                     
	COBOL_OPT_UNDER_COPY,           	// 15                  

	// Sign
	COBOL_OPT_SIGN_LEADING,          	// 16                
	COBOL_OPT_SIGN_TRAILING,         	// 17               
	COBOL_OPT_SIGN_SEPARATE_CHARACTER, 	// 18    

	// Usage
	COBOL_OPT_USAGE_DISPLAY,           	// 19 Alfanumerico X
	COBOL_OPT_USAGE_DISPLAY_1,          // 20 Alfanumerico X
	COBOL_OPT_USAGE_INDEX,           	// 21 Numerico indice
	COBOL_OPT_USAGE_POINTER,           	// 22 Numerico Puntatore
	COBOL_OPT_USAGE_BINARY,           	// 23 Numerico binario
	COBOL_OPT_USAGE_PACKED_DECIMAL,     // 24 Numerico packed
	COBOL_OPT_USAGE_COMPUTATIONAL,      // 25 Numerico Zoned
	COBOL_OPT_USAGE_COMP,           	// 26 Numerico Zoned
	COBOL_OPT_USAGE_COMPUTATIONAL_1,    // 27 Numerico Binario
	COBOL_OPT_USAGE_COMP_1,           	// 28 Numerico binario
	COBOL_OPT_USAGE_COMPUTATIONAL_2,    // 29 Numerico Binario
	COBOL_OPT_USAGE_COMP_2,           	// 30 Numerico Binario
	COBOL_OPT_USAGE_COMPUTATIONAL_3,    // 31 Numerico Packed
	COBOL_OPT_USAGE_COMP_3,           	// 32 Numerico packed
	COBOL_OPT_USAGE_COMPUTATIONAL_4,    // 33 Numerico Floating
	COBOL_OPT_USAGE_COMP_4,           	// 34 Numerico Floating
    
	// Alignment and syncronization
	COBOL_OPT_SYNCRONIZED,           	// 35     
	COBOL_OPT_SYNCRONIZED_LEFT,         // 36              
	COBOL_OPT_SYNCRONIZED_RIGHT,        // 37            
	COBOL_OPT_JUSTIFIED,           		// 38   
	COBOL_OPT_JUSTIFIED_RIGHT,          // 39            
    
	// Occurs
	COBOL_OPT_OCCURS_FROM,           	// 40   
	COBOL_OPT_OCCURS_TO,           		// 41    
	COBOL_OPT_OCCURS_DEPENDING_ON,      // 42         
	COBOL_OPT_OCCURS_ASCENDING_KEY,     // 43        
	COBOL_OPT_OCCURS_DESCENDING_KEY,    // 44     
	COBOL_OPT_OCCURS_INDEXED_BY,        // 45            
    
	// Copy
	COBOL_OPT_COPY_REPLACING,           // 46             
	COBOL_OPT_COPY_SUPPRESS,           	// 47
	COBOL_OPT_COPY_WITH_POINT,          // 48
	COBOL_OPT_COPY_LVL01,           	// 49    
	COBOL_OPT_COPY_OF,           		// 50      
	COBOL_OPT_COPY_IN,           		// 51    
	COBOL_OPT_COPY_LITERAL,           	// 52
	COBOL_OPT_COPY_LIBRARY_NAME,       	// 53             
	COBOL_OPT_COPY_FILE_NAME;           // 54         
}
