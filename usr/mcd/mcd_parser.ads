with MCD_MNG;

package PARSER is

  function NEXT_ITEM return MCD_MNG.ITEM_REC; 

  PARSING_ERROR : exception;

  procedure PRINT_HELP;

  procedure DUMP_STACK;

end PARSER;

