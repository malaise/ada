separate (MCD_MNG)

package body REGISTERS is

  NB_OF_REGISTERS : constant := 2 * 26;
  subtype REGISTER_RANGE is POSITIVE range 1 .. NB_OF_REGISTERS;

  REGISTERS_ARRAY : array (REGISTER_RANGE) of ITEM_REC :=
    (others => (KIND => OPER, VAL_OPER => OPERATOR_LIST'FIRST));

  function REG2IND (REG : in ITEM_REC) return REGISTER_RANGE is
  begin
    if REG.KIND /= REGI then
      raise INVALID_REGISTER;
    end if;
    if REG.VAL_REGI in 'a' .. 'z' then
      return CHARACTER'POS(REG.VAL_REGI) -  CHARACTER'POS('a') + 1;
    elsif REG.VAL_REGI in 'A' .. 'Z' then
      return CHARACTER'POS(REG.VAL_REGI) -  CHARACTER'POS('A')
          +  CHARACTER'POS('z') -  CHARACTER'POS('a') + 1;
    else
       raise INVALID_REGISTER;
    end if;
  end REG2IND;

  procedure STORE (VAL : in ITEM_REC; TO_REG : in ITEM_REC) is
  begin
    if VAL.KIND not in REGISTER_CONTENT_LIST then
      raise INVALID_ARGUMENT;
    end if;
    REGISTERS_ARRAY(REG2IND(TO_REG)) := VAL;
  end STORE;
    
  function  RETRIEVE (FROM_REG : in ITEM_REC) return ITEM_REC is
    VAL : ITEM_REC;
  begin
    VAL := REGISTERS_ARRAY(REG2IND(FROM_REG));
    if VAL.KIND not in REGISTER_CONTENT_LIST then
      raise EMTPY_REGISTER;
    end if;
    return VAL;
  end RETRIEVE;

end REGISTERS;

