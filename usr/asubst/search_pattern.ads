with Regular_Expressions;
package Search_Pattern is

  -- Parses the search patern and returns the number of lines
  --  that it covers (number of regex).
  -- Reports errors on stderr and raises Parse_Error.
  function Parse (Pattern : String) return Positive;

  Parse_Error : exception;

  
  -- Checks if the input string matches one regex
  -- Returns a Match_Cell (set to (0, 0) if no match)
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Parse)
  function Check (Str : String;
                  Regex_Index : Positive)
           return Regular_Expressions.Match_Cell;
  No_Regex : Exception;

end Search_Pattern;
 
