with Text_Handler;

package Argument is

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
  Max_Len_Arg : constant := 1024;


  -- Key to get any key argument (-<anything>)
  function Any_Key return String;
  -- Key to get any not key argument (with no '-' at the beginning)
  function Not_Key return String;
  -- Key to get any argument
  function Any_Arg return String;


  -- Most simple way to use
  function Get_Parameter (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return String;

  -- same but with string and length
  procedure Get_Parameter (
   Parameter : out String;
   Param_Length : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);

  -- same with text
  procedure Get_Parameter (
   Parameter : in out Text_Handler.Text;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);


  -- result is string and position
  procedure Get_Param_And_Pos (
   Parameter : out String;
   Param_Length : out Natural;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);

  -- result is text and position
  procedure Get_Param_And_Pos (
   Parameter : in out Text_Handler.Text;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);


  -- position only
  function Get_Position (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Natural;

  -- number of arguments (0 if no argument)
  function Get_Nbre_Arg return Natural;


  -- analyse of ARGUMENT(0)

  -- path of program  from ARGUMENT(0) (with last /)
  function Get_Program_Path return String;
  procedure Get_Program_Path (Path : out String;
                              Path_Length : out Natural);
  procedure Get_Program_Path (Path : in out Text_Handler.Text);

  -- name of program from ARGUMENT(0)
  function Get_Program_Name return String;
  procedure Get_Program_Name (Name : out String;
                              Name_Length : out Natural);
  procedure Get_Program_Name (Name : in out Text_Handler.Text);

  Argument_Not_Found, Argument_Too_Long : exception;

end Argument;


