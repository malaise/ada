WITH TEXT_IO;
with DYNAMIC_LIST;
separate (MCD_MNG)

package body CALL_STACK is 

  type CALL_ENTRY_REC is RECORD
    STR : CHARS_TEXT;
    LEN : NATURAL;
  end record;
  CALL_ENTRY : CALL_ENTRY_REC;


  package CALL_STACK_LIST is new DYNAMIC_LIST (CALL_ENTRY_REC);
  LIST : CALL_STACK_LIST.LIST_TYPE;

  procedure PUSH (ITEM : in STRING) is
  begin
    CALL_ENTRY.LEN := ITEM'LENGTH;
    CALL_ENTRY.STR (1 .. CALL_ENTRY.LEN) := ITEM;
    CALL_STACK_LIST.INSERT(LIST, CALL_ENTRY);
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.CALL) then
      TEXT_IO.PUT_LINE ("Call_stack: Pushing >" & ITEM & "<"
        & "   Level is "
        & INTEGER'IMAGE(CALL_STACK_LIST.LIST_LENGTH(LIST)));
    end if;
  end PUSH;

  function  POP return STRING is
  begin
    CALL_STACK_LIST.GET(LIST, CALL_ENTRY, CALL_STACK_LIST.PREV);
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.CALL) then
      TEXT_IO.PUT_LINE ("Call_stack: Poping >"
        & CALL_ENTRY.STR(1 .. CALL_ENTRY.LEN) & "<"
        & "   Level is "
        & INTEGER'IMAGE(CALL_STACK_LIST.LIST_LENGTH(LIST)));
    end if;
    return CALL_ENTRY.STR(1 .. CALL_ENTRY.LEN);
  end POP;

  function LEVEL return NATURAL is
  begin
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.CALL) then
      TEXT_IO.PUT_LINE ("Call_stack: Level "
        & INTEGER'IMAGE(CALL_STACK_LIST.LIST_LENGTH(LIST)));
    end if;
    return CALL_STACK_LIST.LIST_LENGTH(LIST);
  end LEVEL;

end CALL_STACK;

