package ARG_PARSING is

  function MANUFA_FILE_NAME return STRING;
  function DESIGN_FILE_NAME return STRING;
  function VERBOSE return BOOLEAN;

  procedure CHECK;
  ARG_ERROR : exception;

end ARG_PARSING;

