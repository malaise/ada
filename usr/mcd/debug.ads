with MCD_MNG;
package DEBUG is

  type DEBUG_LEVEL_LIST is (PARSER, INPUT, CALL, STACK);
  DEBUG_LEVEL_ARRAY : array (DEBUG_LEVEL_LIST) of BOOLEAN;

  procedure INIT;

  procedure PUT (ITEM : in MCD_MNG.ITEM_REC);

end DEBUG;
