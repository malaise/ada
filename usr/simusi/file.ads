with Common; use Common;
package File is

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
  Load_Error : exception;

  function Get_Nb_Cote return Cote_Range;

  function Get_Manufa return Manufa_Array;
  function Get_Design return Design_Array;

end File;

