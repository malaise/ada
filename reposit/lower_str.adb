function LOWER_STR (STR : STRING) return STRING is
  OFFSET  : constant INTEGER   := CHARACTER'POS('A') - CHARACTER'POS('a');
  STR_LOC : STRING (STR'RANGE) := STR;
begin

  for I in STR_LOC'RANGE loop
    if STR_LOC(I) in 'A' .. 'Z' then
      STR_LOC(I) := CHARACTER'VAL( CHARACTER'POS(STR_LOC(I)) - OFFSET );
    end if;
  end loop;

  return STR_LOC;

end LOWER_STR;

