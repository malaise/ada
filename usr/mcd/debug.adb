with TEXT_IO;
with SYS_CALLS; use SYS_CALLS;
with BOOL_IO, INTE_IO, REAL_IO;
package body DEBUG is
  VAL : STRING (1 .. 1);
  SET, TRUNC : BOOLEAN;
  LEN : NATURAL;

  procedure INIT is
  begin
    DEBUG_LEVEL_ARRAY := (others => FALSE);

    GETENV ("MCD_DEBUG_PARSER", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(PARSER) := SET;

    GETENV ("MCD_DEBUG_INPUT", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(INPUT) := SET;
    
    GETENV ("MCD_DEBUG_CALL", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(CALL) := SET;

    GETENV ("MCD_DEBUG_STACK", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(STACK) := SET;

    GETENV ("MCD_DEBUG_REGISTER", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(REGISTER) := SET;

    GETENV ("MCD_DEBUG_OPER", SET, TRUNC, VAL, LEN);
    DEBUG_LEVEL_ARRAY(OPER) := SET;

    GETENV ("MCD_DEBUG_HISTORY", SET, TRUNC, VAL, LEN);
    if SET then
      DEBUG_LEVEL_ARRAY(HISTORY) := SET;
    end if;

    GETENV ("MCD_DEBUG_ALL", SET, TRUNC, VAL, LEN);
    if SET then
      DEBUG_LEVEL_ARRAY := (others => TRUE);
    end if;
  end INIT;

  procedure PUT (ITEM : in MCD_MNG.ITEM_REC) is
    use MCD_MNG;
  begin
    case ITEM.KIND is
      when INTE =>
        INTE_IO.PUT(ITEM.VAL_INTE);
      when REAL =>
        REAL_IO.PUT(ITEM.VAL_REAL);
      when BOOL =>
        BOOL_IO.PUT(ITEM.VAL_BOOL);
      when CHRS =>
        TEXT_IO.PUT (">" & ITEM.VAL_TEXT(1 .. ITEM.VAL_LEN) & "<");
      when REGI =>
        TEXT_IO.PUT (ITEM.VAL_REGI);
      when OPER =>
        TEXT_IO.PUT (OPERATOR_LIST'IMAGE(ITEM.VAL_OPER));
    end case;
  end PUT;

end DEBUG;
