package COMMAND is

  -- Print command line syntax
  procedure PRINT_USAGE;

  -- Parse the command line. Must be called once and only once,
  --  before other calls (or ALREADY_PARSED is raised);
  -- NO_COMMAND may be raised here
  -- Picks the first -adcf option, then builds the command_line argument
  procedure PARSE (NO_ACTION,      NO_NAME_OF_DIR,
                   NOT_IN_CURRENT, FIRST_LEVEL_ONLY, LEAVES_ONLY,
                   NO_STOP_ON_ERROR : out BOOLEAN);

  -- These 2 must be called after PARSE (or NOT_PARSED is raised).
  -- They give info about the command_line (NTH_COMMAND may return "").
  function NBRE_COMMANDS return NATURAL;

  function NTH_COMMAND (N : POSITIVE) return STRING;

  ALREADY_PARSED, NOT_PARSED : exception;
  NO_COMMAND : exception;

end COMMAND;
