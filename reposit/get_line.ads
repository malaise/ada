-- Opens a file and reads lines
-- Parses words separated by space or tab
with TEXT_IO;
with TEXT_HANDLER;

generic

  MAX_WORD_LEN : in POSITIVE;
  MAX_WORD_NB  : in POSITIVE;
  MAX_LINE_LEN : in POSITIVE;

package GET_LINE is

  subtype WORD_TXT is TEXT_HANDLER.TEXT(MAX_LEN => MAX_WORD_LEN);
  subtype WORD_RANGE is POSITIVE range 1 .. MAX_WORD_NB;
  subtype WORD_COUNT is NATURAL  range 0 .. MAX_WORD_NB;
  type LINE_ARRAY is array (WORD_RANGE) of WORD_TXT;
  subtype LINE_TXT is TEXT_HANDLER.TEXT(MAX_LEN => MAX_LINE_LEN);

  -- Opens the file. Exceptions are the one of TEXT_IO.OPEN (IN_FILE)
  -- Loads the first line
  procedure OPEN (FILE_NAME : in STRING);

  -- Closes the file
  procedure CLOSE;

  -- Loads next line of file
  procedure READ_NEXT_LINE;
  NO_MORE_LINE   : exception;
  TOO_MANY_WORDS : exception;
  LINE_TOO_LONG  : exception;
  WORD_TOO_LONG  : exception;

  -- Current line number
  function GET_LINE_NO return TEXT_IO.POSITIVE_COUNT;

  -- Number of words in currently loaded line
  function GET_WORD_NUMBER return WORD_COUNT;

  -- Words of the currently loaded line
  procedure GET_WORDS (LINE : in out LINE_ARRAY);

  -- Get the whole line (not parsed)
  procedure GET_WHOLE_LINE (LINE : in out LINE_TXT);

end GET_LINE;

