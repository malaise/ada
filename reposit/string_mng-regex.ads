-- More powerfull search and substitution in strings,
--  based on regular expressions (extended, case sensitive and matching
--  newline).
with As.U; use As.U;
with Regular_Expressions;
package String_Mng.Regex is

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
  -- Raised by Locate or Replace  or Split if Criteria is not a valid
  -- extented regularexpression
  Invalid_Regular_Expression : exception;

  -- Locate a fragment of Within string matching the regexp Criteria.
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
                   Occurence  : Positive := 1)
           return Search_Result;


  -- Replace in Within string all occurences of Criteria by By.
  -- Search and replace is performed between the given From_Index (first if 0)
  --  and the given To_Index (last if 0) indexes of Within.
  -- Criteria can be a regular expression and By may:
  --   reference partial matching substrings (\0, \1, \2 up to \9)
  --   contain hexadecimal code (\xIJ)
  --   convert in UPPER, lower, Mixed case, stop converting (\u, \l, \m and \c).
  -- Once a substitution has occured, the search continues from the
  --  first character after replacement (thus avoiding loops), up to To_Index.
  -- This cycle ends when no substitution occurs or after a maximum Nb_Cycles,
  --  (if not 0). Beware that Nb_Cycles=0 may lead to infinite.
  function Replace (Within     : String;
                    Criteria   : String;
                    By         : String;
                    From_Index : Natural := 0;
                    To_Index   : Natural := 0;
                    Nb_Cycles  : Natural := 1)
           return String;

  -- Split Str into several substrings that match the substrings "(...)"
  --  of the criteria.
  -- Returns the array of slices (empty array if Str does not strictly match).
  type String_Slice is array (Positive range <>) of Asu_Us;
  function Split (Str : String;
                  Criteria : String;
                  Max_Slices : Positive) return String_Slice;

  -- Split Str into several substrings separated by strings matching the
  --  separator.
  -- Returns the array of slices (Str at pos 1 if no match, empty slices if
  --  Str at all matches Separator).
  -- Raises Not_Enough_Slices if Max_Slices is not big enough
  Not_Enough_Slices : exception;
  subtype Slice_Sep_Range is Positive range 2 .. Positive'Last;
  function Split_Sep (Str : String;
                      Separator : String;
                      Max_Slices : Slice_Sep_Range) return String_Slice;

end String_Mng.Regex;

