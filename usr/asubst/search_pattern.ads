with Regular_Expressions;
package Search_Pattern is

  -- Parses and compiles the search patern
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String; Extended, Case_Sensitive : Boolean);
  Parse_Error : exception;

  -- Returns the number of regex that are implied by the
  -- search pattern (one per regex and one per New_Line).
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive;

  -- Tells if the search pattern can be applied several times
  -- on one line of input (i.e. does not contain '\n', '^' or '$')
  -- Raises No_Regex if the pattern was not parsed OK
  function Multiple return Boolean;
  
  -- Checks if the input string matches one regex
  -- Returns a Match_Cell (set to (0, 0) if no match)
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Parse)
  function Check (Str : String;
                  Regex_Index : Positive)
           return Regular_Expressions.Match_Cell;
  No_Regex : exception;

end Search_Pattern;
 
