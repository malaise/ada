with Regular_Expressions;
package Search_Pattern is

  -- Parses and compiles the search patern
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String; Extended, Case_Sensitive : Boolean);
  Parse_Error : exception;

  -- Returns the number of regex that are implied by the
  --  search pattern (one per regex and one per New_Line).
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive;

  -- Tells if the search pattern can be applied several times
  --  on one line of input (i.e. does not contain '\n', '^' or '$')
  -- Raises No_Regex if the pattern was not parsed OK
  function Multiple return Boolean;
  
  -- Checks if the input string matches one regex
  -- Returns a Match_Cell (set to (0, 0) if no match)
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  function Check (Str : String;
                  Regex_Index : Positive)
           return Regular_Expressions.Match_Cell;
  No_Regex : exception;

  -- Returns the number of substrings of one regex
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  subtype Nb_Sub_String_Range is Natural range 0 .. 16;
  subtype Sub_String_Range is Nb_Sub_String_Range range 1 .. 16;
  function Nb_Substrings (Regex_Index : Positive) return Nb_Sub_String_Range;

  -- Returns the Match_Cell of the Nth sub-matching string
  --  of one regex
  -- Returns the Match_Cell of the matching string of one regex
  --  if Sub_String_Index is 0
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  --  or if the Sub_String_Index is higher than the number
  --  of substring of this regex (returned by Nb_Substrings)
  --  or if last Checks did not succeed
  function Substr_Indexes (Regex_Index : Positive;
                           Sub_String_Index : Nb_Sub_String_Range)
           return Regular_Expressions.Match_Cell;

  -- Returns the Match_Cell of the complete matching string
  -- i.e. (Substr_Indexes(1, 0).Start_Offset,
  --       Substr_Indexes(Number, 0).End_Offset)
  -- Raises No_Regex if last Checks did not succeed
  function Str_Indexes return Regular_Expressions.Match_Cell;

end Search_Pattern;
 
