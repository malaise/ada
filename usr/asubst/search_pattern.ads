with Regular_Expressions;
package Search_Pattern is

  -- Parses and compiles the search patern
  -- Parses and compiles the exclude patern (if any)
  -- Parses and checks the delimiter (if any)
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Search  : in String;
                   Exclude : in String;
                   Delimiter : in String;
                   Case_Sensitive, Is_Regex, Dot_All : in Boolean);
  Parse_Error : exception;

  -- Returns the number of patterns that are implied by the
  --  search pattern (one per pattern and one per delimiters).
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive;

  -- Is search pattern a Regex
  -- Raises No_Regex if the pattern was not parsed OK
  function Search_Regex return Boolean;

  -- Return the Nts pattern or delimiter
  -- Raises No_Regex if the pattern was not parsed OK
  -- Raises Contraint_Error if N > Number;
  function Get_Pattern (Regex_Index : Positive) return String;

  -- Tells if the search pattern can be applied several times
  --  on one line of input (i.e. does not contain '\n', '^' or '$')
  -- Raises No_Regex if the pattern was not parsed OK
  function Iterative return Boolean;

  -- Get the delimiter
  function Get_Delimiter return String;

  -- Is an exclude pattern set
  function Has_Exclude return Boolean;

  -- Checks if the input string from Start to its end
  --  matches the regex no Regex_Index
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  --  or if successive calls do not provide crescent indexes
  function Check (Str : String; Start : Positive;
                  Search      : Boolean;
                  Regex_Index : Positive) return Boolean;
  No_Regex : exception;

  -- Returns the number of substrings of one regex
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  subtype Nb_Sub_String_Range is Natural range 0 .. 16;
  subtype Sub_String_Range is Nb_Sub_String_Range range 1 .. 16;
  function Nb_Substrings (Regex_Index : Positive) return Nb_Sub_String_Range;

  -- Returns the Nth sub-matching string of one regex
  -- Returns the complete matching string of one regex if Sub_String_Index is 0
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (returned by Number)
  --  or if the Sub_String_Index is higher than the number
  --  of substring of this regex (returned by Nb_Substrings)
  --  or if last Checks did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  (sub) string length
  Substr_Len_Error : exception;
  function Substring (Regex_Index : Positive;
                      Sub_String_Index : Nb_Sub_String_Range)
           return String;

  -- Returns the complete matching string of all regexes
  -- Raises No_Regex if last Checks did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  string length
  function Allstring return String;

  -- Returns the Match_Cell of the complete matching string
  -- i.e. (Substr_Indexes(1, 0).Start_Offset,
  --       Substr_Indexes(Number, 0).End_Offset)
  -- Raises No_Regex if last Checks did not succeed
  -- May raise Substr_Len_Error if Utf8 sequence leads to exceed
  --  string length
  function Str_Indexes return Regular_Expressions.Match_Cell;

end Search_Pattern;

