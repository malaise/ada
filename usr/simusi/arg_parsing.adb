with ARGUMENT;
package body ARG_PARSING is

  VERBOSE_FLAG : BOOLEAN;
  PM, PD : POSITIVE;

  function MANUFA_FILE_NAME return STRING is
  begin
    return ARGUMENT.GET_PARAMETER (PM);
  end MANUFA_FILE_NAME;
  
  function DESIGN_FILE_NAME return STRING is
  begin
    return ARGUMENT.GET_PARAMETER (PD);
  end DESIGN_FILE_NAME;

  function VERBOSE return BOOLEAN is
  begin
    return VERBOSE_FLAG;
  end VERBOSE;

  procedure CHECK is
  begin
    VERBOSE_FLAG := FALSE;
    PM := 1;
    PD := 2;
    if ARGUMENT.GET_NBRE_ARG = 3 then
      if ARGUMENT.GET_PARAMETER (1, "v") = ""
      and then ARGUMENT.GET_POSITION (1, "v") = 1 then
        VERBOSE_FLAG := TRUE;
        PM := 2;
        PD := 3;
      else
        raise ARG_ERROR;
      end if;
    elsif ARGUMENT.GET_NBRE_ARG /= 2 then
      raise ARG_ERROR;
    end if;
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      -- Default: no flag
      null;
  end;
  
end ARG_PARSING;

