with TEXT_IO;
with ARGUMENT;
with COMMON;
procedure C_MAGIC is
  DIM : COMMON.DIM_RANGE;
begin

  -- Get dimension
  declare
    SYNTAX_ERROR : exception;
  begin
    if ARGUMENT.GET_NBRE_ARG /= 1 then
      raise SYNTAX_ERROR;
    end if;
    DIM := COMMON.DIM_RANGE'VALUE(ARGUMENT.GET_PARAMETER);
  exception
    when others =>
      TEXT_IO.PUT_LINE ("SYNTAX ERROR. Usage : C_MAGIC <dim>");
      raise;
  end;

  -- Search all magic magic squares with this dimension
  COMMON.SEARCH (DIM);

end C_MAGIC;
