package Arg_Parsing is

  function Manufa_File_Name return String;
  function Design_File_Name return String;
  function Verbose return Boolean;

  procedure Check;
  Arg_Error : exception;

end Arg_Parsing;

