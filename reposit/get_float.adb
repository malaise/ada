-- Extract float from string (float or int format)
-- May raise text_io exceptions

with FLO_IO, INT_IO, IO_EXCEPTIONS;
function GET_FLOAT (STR : STRING) return FLOAT is
  I : INTEGER;
  F : FLOAT;
  L : NATURAL;
begin
  begin
    -- Try to read float
    FLO_IO.GET (STR, F, L);
    return F;
  exception
    when IO_EXCEPTIONS.DATA_ERROR =>
      -- Try to read int
      INT_IO.GET (STR, I, L);
      return FLOAT (I);
  end;
end GET_FLOAT;
