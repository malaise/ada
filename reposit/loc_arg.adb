with ADA.COMMAND_LINE;
package body LOC_ARG is

  -- returns 1 if the current prog has 1 arg (0 if no arg)
  function COUNT return NATURAL is
  begin
    return ADA.COMMAND_LINE.ARGUMENT_COUNT;
  end COUNT;

  -- returns n th parameter of current prog (prog name if n=0)
  function DATA (POS : NATURAL) return STRING is
  begin
    if POS = 0 then
      return ADA.COMMAND_LINE.Command_Name;
    else
      return ADA.COMMAND_LINE.Argument(POS);
    end if;  
  end DATA;

end LOC_ARG;

