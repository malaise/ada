package LOC_ARG is

  -- returns 1 if the current prog has 1 arg (0 if no arg)
  function COUNT return NATURAL;

  -- returns n th parameter of current prog (prog name if n=0)
  function DATA (POS : NATURAL) return STRING;

end LOC_ARG;
