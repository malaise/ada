-- Extract float from string (float or int format)
-- May raise text_io exceptions

with FLO_IO, INT_IO, IO_EXCEPTIONS;
function GET_FLOAT (STR : STRING) return FLOAT is
  F : FLOAT;
  L : POSITIVE;
  I : INTEGER;
  STR_LEN : NATURAL;
begin
  -- Locate last significant character of STR
  STR_LEN := 0;
  for J in reverse STR'RANGE loop
    if STR(J) /= ' ' then
      STR_LEN := J + 1 - STR'FIRST;
      exit;
    end if;
  end loop;
  if STR_LEN = 0 then
    raise CONSTRAINT_ERROR;
  end if; 

  TRY_TO_CONVERT:
  begin
    -- Float format
    FLO_IO.GET(STR, F, L);
  exception
    when IO_EXCEPTIONS.DATA_ERROR =>
       -- Int format
      INT_IO.GET(STR, I, L);
      F := FLOAT(I);
  end TRY_TO_CONVERT;


  if L /= STR'LAST then
    raise CONSTRAINT_ERROR;
  else
    return F;
  end if;

exception
  when others =>
    raise CONSTRAINT_ERROR;
end GET_FLOAT;

