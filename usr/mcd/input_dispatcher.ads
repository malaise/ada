package INPUT_DISPATCHER is

  -- Max line length on stdin
  -- Max string length
  MAX_STRING_LG : constant := 1024;

  -- Set input flow to a new string
  --  or stdin if STR is empty
  procedure SET_INPUT (STR : in STRING);

  -- Get the ungot words of current string
  -- PROGRAM_ERROR is current input is stdin
  --  or if no word already got from current string
  function GET_REMAINING return STRING;

  -- Get next word from current input
  -- Empty if end of input flow
  function NEXT_WORD return STRING;

  -- Error if end of string litteral not found
  STRING_ERROR : exception;

end INPUT_DISPATCHER;

