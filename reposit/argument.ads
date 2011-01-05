with As.U;
package Argument is

  -- Extracts arguments from command line:

  -- Each argument of the command line can be preceeded by a key sequence:
  --  a separator ('-') and a key string.

  -- For example, if the key for file_name is "R" then the command line
  --  should be "program -Rfile_name" (note that there is no space
  --  between the key sequence and the argument) and "file_name" is returned..

  -- This makes it possible to specify arguments in any order in the
  --  command line and to pick them in the program.

  -- Not_Key indicates to pick any argument not preceded by '-'
  --  (all the argument is returned)
  -- Any_Arg indicates to pick any argument
  --  (all the argument is returned)
  -- Any_Key indicates to pick any argument preceeded by a '-',
  --  (the string following th '-' is returned)
  -- any other string indicates to pick any argument beginning
  --  with '-' then the key string (the remaining characters are returned)
  --  an empty String is similar to Any_Key.

  -- The parameter 'occurence' may specify to pick the n th occurence
  --  of a parameter comforming the criteria, the returned value is the
  --  position of the parameter in the command line.
  -- If the given value is 0 then program name is returned if param key
  --  is Not_Key or Any_Arg (else argument will not be found).

  -- If the proper separator is not provided in the command line, then the
  --  argument will not be found with Any_Key nor with a key string.
  -- A command line like "program -A" will make an empty string to
  --  be returned if A is the key.

  -- Given the command line: "prog -Ka1 -Na2 a3 -Na4 -p a5"
  --   Key    Occurence   returns     Param   Pos
  --   "K"        1                    "a1"    1
  --   "K"        2                raises Argument_Not_Found
  --   "N"        2                    "a4"    4
  --   "Na"       2                    "4"     4
  --   "p"        1                    ""      5
  --   ""         3                    "Na4"   4
  --  Any_Key     3                    "Na4"   4
  --  Any_Arg     0                    "prog"  0
  --  Any_Arg     4                    "-Na4"  4
  --  Not_Key     1                    "a3"    3
  --  Not_Key     3                raises Argument_Not_Found

  -- The sequence "-p a5" can be picked by calling:
  --  Key="p", Occurence=1. Then checking that empty string is returned
  --  and storing position (pos). Then calling:
  --  Key=Any_Arg, Occurence=pos+1. Checking that the returned string
  --  is not empty.


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

  -- Same but with string and length
  procedure Get_Parameter (
   Parameter : out String;
   Param_Length : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);

  -- Same but with unbounded string
  procedure Get_Parameter (
   Parameter : out As.U.Asu_Us;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);

  -- Result is string and position
  procedure Get_Param_And_Pos (
   Parameter : out String;
   Param_Length : out Natural;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);

  -- Result is unbounded string and position
  procedure Get_Param_And_Pos (
   Parameter : out As.U.Asu_Us;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg);


  -- Position only
  function Get_Position (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Natural;

  -- Is (a key) set
  function Is_Set (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Boolean;

  -- Number of arguments (0 if no argument)
  function Get_Nbre_Arg return Natural;


  -- Analyse of Argument(0)

  -- Path of program  from Argument(0) (with last /)
  function Get_Program_Path return String;
  procedure Get_Program_Path (Path : out String;
                              Path_Length : out Natural);

  -- Name of program from Argument(0)
  function Get_Program_Name return String;
  procedure Get_Program_Name (Name : out String;
                              Name_Length : out Natural);

  Argument_Not_Found, Argument_Too_Long : exception;

end Argument;

