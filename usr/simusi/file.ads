with COMMON; use COMMON;
package FILE is

  -- MUST HAVE pragma ELABORATE!

  -- Does everything at elaboration:
  --  Errors are traced on stderr

  -- Parses both arguments
  -- Error can be argument error

  -- Load both sets of cotes from two files
  -- And checks
  -- Errors can be
  -- - File access, file format, file length too long
  -- - Invalid line number, start=stop
  -- - Duplicate (start, stop) in file
  -- - Line with no cote
  -- - Not same amount of cotes
  LOAD_ERROR : exception;

  function GET_NB_COTE return COTE_RANGE;

  function GET_MANUFA return MANUFA_ARRAY;
  function GET_DESIGN return DESIGN_ARRAY;

end FILE;
