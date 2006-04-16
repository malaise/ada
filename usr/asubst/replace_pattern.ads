package Replace_Pattern is

  -- Parses the replace patern
  -- Detects \t, \n and \&
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String);
  Parse_Error : exception;

  -- Returns the replacing string
  function Replace return String;
  -- If result string is too long?
  Replace_Error : exception;

end Replace_Pattern;
 
