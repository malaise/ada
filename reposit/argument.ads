with TEXT_HANDLER;

package ARGUMENT is

  -- Extracts arguments from command line:

  -- Each argument of the command line can be preceeded by a key sequence:
  --  a separator ('-') and a key string.

  -- For example, if the key for file_name is "R" then the command line
  --  should be "program -Rfile_name" (note that there is no space
  --  between the key sequence and the argument) and "file_name" is returned..

  -- This makes it possible to specify arguments in any order in the
  --  command line and to pick them in the program.

  -- NOT_KEY indicates to pick any argument not preceded by '-'
  --  (all the argument is returned)
  -- ANY_ARG indicates to pick any argument
  --  (all the argument is returned)
  -- ANY_KEY indicates to pick any argument preceeded by a '-',
  --  (the string following th '-' is returned)
  -- any other string indicates to pick any argument beginning
  --  with '-' then the key string (the remaining characters are returned)
  --  an empty STRING is similar to ANY_KEY.

  -- The parameter 'occurence' may specify to pick the n th occurence
  --  of a parameter comforming the criteria, the returned value is the
  --  position of the parameter in the command line.
  -- If the given value is 0 then program name is returned if param key
  --  is NOT_KEY or ANY_ARG (else argument will not be found).

  -- If the proper separator is not provided in the command line, then the
  --  argument will not be found with ANY_KEY nor with a key string.
  -- A command line like "program -A" will make an empty string to
  --  be returned if A is the key.

  -- Given the command line: "prog -Ka1 -Na2 a3 -Na4 -p a5"
  --   KEY    OCCURENCE   returns     PARAM   POS
  --   "K"        1                    "a1"    1
  --   "K"        2                raises ARGUMENT_NOT_FOUND
  --   "N"        2                    "a4"    4
  --   "Na"       2                    "4"     4
  --   "p"        1                    ""      5
  --   ""         3                    "Na4"   4
  --  ANY_KEY     3                    "Na4"   4
  --  ANY_ARG     0                    "prog"  0
  --  ANY_ARG     4                    "-Na4"  4
  --  NOT_KEY     1                    "a3"    3
  --  NOT_KEY     3                raises ARGUMENT_NOT_FOUND

  -- The sequence "-p a5" can be picked by calling:
  --  KEY="p", OCCURENCE=1. Then checking that empty string is returned
  --  and storing position (pos). Then calling:
  --  KEY=ANY_ARG, OCCURENCE=pos+1. CHecking that the returned string
  --  is not empty.


  -- the maximum length supported for one argument
  MAX_LEN_ARG : constant := 1024;


  -- Key to get any key argument (-<anything>)
  function ANY_KEY return STRING;
  -- Key to get any not key argument (with no '-' at the beginning)
  function NOT_KEY return STRING;
  -- Key to get any argument
  function ANY_ARG return STRING;


  -- Most simple way to use
  function GET_PARAMETER (
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) return STRING;

  -- same but with string and length
  procedure GET_PARAMETER (
   PARAMETER : out STRING;
   PARAM_LENGTH : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG);

  -- same with text
  procedure GET_PARAMETER (
   PARAMETER : in out TEXT_HANDLER.TEXT;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG);


  -- result is string and position
  procedure GET_PARAM_AND_POS (
   PARAMETER : out STRING;
   PARAM_LENGTH : out NATURAL;
   POSITION : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG);

  -- result is text and position
  procedure GET_PARAM_AND_POS (
   PARAMETER : in out TEXT_HANDLER.TEXT;
   POSITION : out NATURAL;
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG);


  -- position only
  function GET_POSITION (
   OCCURENCE : in NATURAL := 1;
   PARAM_KEY : in STRING := ANY_ARG) return NATURAL;

  -- number of arguments (0 if no argument)
  function GET_NBRE_ARG return NATURAL;


  -- analyse of ARGUMENT(0)

  -- path of program  from ARGUMENT(0) (with last /)
  function GET_PROGRAM_PATH return STRING;
  procedure GET_PROGRAM_PATH (PATH : out STRING;
                              PATH_LENGTH : out NATURAL);
  procedure GET_PROGRAM_PATH (PATH : in out TEXT_HANDLER.TEXT);

  -- name of program from ARGUMENT(0)
  function GET_PROGRAM_NAME return STRING;
  procedure GET_PROGRAM_NAME (NAME : out STRING;
                              NAME_LENGTH : out NATURAL);
  procedure GET_PROGRAM_NAME (NAME : in out TEXT_HANDLER.TEXT);

  ARGUMENT_NOT_FOUND, ARGUMENT_TOO_LONG : exception;

end ARGUMENT;


