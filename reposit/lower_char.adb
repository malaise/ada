function LOWER_CHAR (CHAR : CHARACTER) return CHARACTER is
  OFFSET  : constant INTEGER   := CHARACTER'POS('A') - CHARACTER'POS('a');
begin

  if CHAR in 'A' .. 'Z' then
    return CHARACTER'VAL( CHARACTER'POS(CHAR) - OFFSET );
  else
    return CHAR;
  end if;

end LOWER_CHAR;
