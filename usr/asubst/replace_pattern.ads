package Replace_Pattern is

  -- Parses the replace patern
  -- Detects \t, \n and \&
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String);
  Parse_Error : exception;

  -- Returns if pattern is empty
  function Is_Empty return Boolean;

  -- Return replace pattern
  function Get return String;

  -- Returns the replacing string
  function Replace return String;
  -- If result string is too long?
  Replace_Error : exception;
  -- If open/read of external file fails
  File_Error : exception;
  -- If external shell command exit /= 0 or spawn error
  Command_Error : exception;
  -- If Ctr C while waiting for external shell result
  Terminate_Request : exception;
end Replace_Pattern;

