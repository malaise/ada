with TEXT_IO;
with DYNAMIC_LIST;
separate (MCD_MNG)

package body STACK is 

  package STACK_LIST is new DYNAMIC_LIST (ITEM_REC);
  LIST : STACK_LIST.LIST_TYPE;

  procedure PUSH (ITEM : in ITEM_REC) is
  begin
    if ITEM.KIND not in OPERAND_KIND_LIST then
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
        TEXT_IO.PUT ("Stack: ERROR Pushing ");
        DEBUG.PUT (ITEM);
        TEXT_IO.NEW_LINE;
      end if;
      raise INVALID_ARGUMENT;
    end if;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      TEXT_IO.PUT ("Stack: Pushing ");
      DEBUG.PUT (ITEM);
      TEXT_IO.NEW_LINE;
    end if;
    STACK_LIST.INSERT(LIST, ITEM);
  end PUSH;

  procedure POP (ITEM : out ITEM_REC) is
  begin
    STACK_LIST.GET(LIST, ITEM, STACK_LIST.PREV);
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      TEXT_IO.PUT ("Stack: Poping ");
      DEBUG.PUT (ITEM);
      TEXT_IO.NEW_LINE;
    end if;
  exception
    when STACK_LIST.EMPTY_LIST =>
      raise EMPTY_STACK;
  end POP;

  procedure READ (ITEM : out ITEM_REC) is
  begin
    STACK_LIST.READ(LIST, ITEM, STACK_LIST.CURRENT);
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      TEXT_IO.PUT ("Stack: Reading ");
      DEBUG.PUT (ITEM);
      TEXT_IO.NEW_LINE;
    end if;
  exception
    when STACK_LIST.EMPTY_LIST =>
      raise EMPTY_STACK;
  end READ;

  function STACK_SIZE return NATURAL is
  begin
    return STACK_LIST.LIST_LENGTH(LIST);
  end STACK_SIZE;

end STACK;

