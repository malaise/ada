function UPPER_CHAR (CHAR : CHARACTER) return CHARACTER is
  OFFSET  : constant INTEGER   := CHARACTER'POS('A') - CHARACTER'POS('a');
begin

  if CHAR in 'a' .. 'z' then
    return CHARACTER'VAL( CHARACTER'POS(CHAR) + OFFSET );
  else
    return CHAR;
  end if;
end UPPER_CHAR;
