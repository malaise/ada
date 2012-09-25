-- More powerfull search and substitution in strings,
--  based on regular expressions (extended, case sensitive and matching
--  newline).
with As.U.Utils;
with Regular_Expressions;
package Str_Util.Regex is

  -- Result of search, from Regular_Expressions
  -- subtype Offset_Range is Integer;
  -- type Match_Cell is record
  --   First_Offset :  Offset_Range;
  --   Last_Offset_Start :  Offset_Range;
  --   Last_Offset_Stop  :  Offset_Range;
  -- end record;
  type Search_Result is new Regular_Expressions.Match_Cell;

  -- Result of search when no match found
  No_Match : constant Search_Result
           := Search_Result(Regular_Expressions.No_Match);

  -- Raised by Locate or Replace if Within is not Empty and an Index is not
  -- in Within'Range
  Invalid_Index : exception;
  -- Raised by Locate or Replace or Split if Criteria is not a valid
  -- extented regular expression
  Invalid_Regular_Expression : exception;

  -- Options for the Regexp matching
  type Options_Rec is record
    Case_Sensitive : Boolean := True;
    Multi_Line : Boolean := False;
    Dot_All : Boolean := False;
  end record;
  Tmp_Options : Options_Rec;
  Default_Options : constant Options_Rec := Tmp_Options;


  -- Locate a fragment of Within string matching the regexp Criteria.
  -- Criteria is an extented regular expression.
  -- Search is performed between the given From_Index (first if 0) and
  --  the To_Index (last if 0) indexes of Within.
  -- Search can be performed either forward or backward, and returns the
  --  Nth matching Occurence.
  -- Returns Start and Stop indexes in Within, of chars matching the criteria.
  -- Returns No_Match (Start=1 and Stop=0) if no corresponding match is found.
  function Locate (Within     : String;
                   Criteria   : String;
                   From_Index : Natural := 0;
                   To_Index   : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1;
                   Options    : Options_Rec := Default_Options)
           return Search_Result;


  -- Replace in Within string all occurences of Criteria by By.
  -- Criteria is an extented regular expression and By may:
  --   reference partial matching substrings (\0, \1, \2 up to \9)
  --   contain hexadecimal code (\xIJ)
  --   convert to UPPER, lower, Mixed case, stop converting (\u, \l, \m and \c).
  -- Search and replace is performed forward between the given From_Index
  --  (first if 0) and the given To_Index (last if 0) indexes of Within.
  -- Once a substitution has occured, the search continues from the
  --  first character after replacement (thus avoiding loops), up to To_Index.
  -- This cycle ends when no substitution occurs or after a maximum Nb_Cycles,
  --  (if not 0). Beware that Nb_Cycles=0 may lead to infinite loop.
  function Substit (Within     : String;
                    Criteria   : String;
                    By         : String;
                    From_Index : Natural := 0;
                    To_Index   : Natural := 0;
                    Options    : Options_Rec := Default_Options;
                    Nb_Cycles  : Natural := 1)
           return String;

  -- Split Str into several substrings that match the substrings "(...)"
  --  of the criteria.
  -- Returns the array of slices (empty array if Str does not strictly match).
  -- Example: Split ("1o2o", "(.)o(.)o)) -> ("1", "2")
  function Split (Str        : String;
                  Criteria   : String;
                  Max_Slices : Positive;
                  Options    : Options_Rec := Default_Options)
           return As.U.Utils.Asu_Array;

  -- Split Str into several substrings separated by strings matching the
  --  separator.
  -- Returns the array of slices (Str at pos 1 if no match,
  --   empty slice if Str at all matches Separator).
  -- Heading and trailing separators lead to heading and trailing empty strings
  --  in the result.
  function Split_Sep (Str       : String;
                      Separator : String;
                      Options   : Options_Rec := Default_Options)
           return As.U.Utils.Asu_Array;
end Str_Util.Regex;

