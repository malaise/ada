with TEXT_IO;
with DYNAMIC_LIST;
separate (MCD_MNG)

package body STACK is 

  package STACK_LIST is new DYNAMIC_LIST (ITEM_REC);
  LIST, EXTRA_LIST : STACK_LIST.LIST_TYPE;

  procedure PUSH (ITEM : in ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE) is
  begin
    if ITEM.KIND not in OPERAND_KIND_LIST then
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
        if not DEFAULT_STACK then
          TEXT_IO.PUT ("Extra ");
        end if;
        TEXT_IO.PUT ("Stack: ERROR Pushing ");
        DEBUG.PUT (ITEM);
        TEXT_IO.NEW_LINE;
      end if;
      raise INVALID_ARGUMENT;
    end if;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
        if not DEFAULT_STACK then
          TEXT_IO.PUT ("Extra ");
        end if;
      TEXT_IO.PUT ("Stack: Pushing ");
      DEBUG.PUT (ITEM);
      TEXT_IO.NEW_LINE;
    end if;
    if DEFAULT_STACK then
      STACK_LIST.INSERT(LIST, ITEM);
    else
      STACK_LIST.INSERT(EXTRA_LIST, ITEM);
    end if;
  end PUSH;

  procedure POP (ITEM : out ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE) is
    LITEM : ITEM_REC;
  begin
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      if not DEFAULT_STACK then
        TEXT_IO.PUT ("Extra ");
      end if;
      TEXT_IO.PUT ("Stack: Poping ");
    end if;
    if DEFAULT_STACK then
      STACK_LIST.GET(LIST, LITEM, STACK_LIST.PREV);
    else
      STACK_LIST.GET(EXTRA_LIST, LITEM, STACK_LIST.PREV);
    end if;
    ITEM := LITEM;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      DEBUG.PUT (LITEM);
      TEXT_IO.NEW_LINE;
    end if;
  exception
    when STACK_LIST.EMPTY_LIST =>
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
        TEXT_IO.PUT_LINE("raises EMPTY_STACK");
      end if;
      raise EMPTY_STACK;
  end POP;

  procedure READ (ITEM : out ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE) is
    LITEM : ITEM_REC;
  begin
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      if not DEFAULT_STACK then
        TEXT_IO.PUT ("Extra ");
      end if;
      TEXT_IO.PUT ("Stack: Reading ");
    end if;
    if DEFAULT_STACK then
      STACK_LIST.READ(LIST, LITEM, STACK_LIST.CURRENT);
    else
      STACK_LIST.READ(EXTRA_LIST, LITEM, STACK_LIST.CURRENT);
    end if;
    ITEM := LITEM;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      DEBUG.PUT (LITEM);
      TEXT_IO.NEW_LINE;
    end if;
  exception
    when STACK_LIST.EMPTY_LIST =>
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
        TEXT_IO.PUT_LINE("raises EMPTY_STACK");
      end if;
      raise EMPTY_STACK;
  end READ;

  function STACK_SIZE (DEFAULT_STACK : BOOLEAN := TRUE) return NATURAL is
    SIZE : NATURAL;
  begin
    if DEFAULT_STACK then
      SIZE := STACK_LIST.LIST_LENGTH(LIST);
    else
      SIZE := STACK_LIST.LIST_LENGTH(EXTRA_LIST);
    end if;
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.STACK) then
      if not DEFAULT_STACK then
        TEXT_IO.PUT ("Extra ");
      end if;
      TEXT_IO.PUT_LINE ("Stack: size " & NATURAL'IMAGE(SIZE));
    end if;
    return SIZE;
  end STACK_SIZE;

end STACK;

