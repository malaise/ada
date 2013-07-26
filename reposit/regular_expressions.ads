-- Binding to PCRE POSIX regular expression
with System, Ada.Finalization;
with Language;
package Regular_Expressions is

  -- Get current PCRE version
  function Get_Pcre_Version return String;

  -- Check PCRE version, raises anonymous exception Invalid_Pcre_Version
  procedure Check_Pcre_Version;

  -- Result of regex compilation
  type Compiled_Pattern is limited private;

  -- Matching information:
  -- Filled with indexes in string To_Check,
  --  defining matching substrings
  -- When a character is encoded on several bytes,
  --  Language is used to detect the end of this sequence and
  --  set Last_Offset_Stop to the last byte of the sequence.
  -- 'abcab' matches '(ab)c\1*' at pos [1-5/5] [1-2/2]
  -- 'to/to' matches 'to($)' 'at pos [4-5/5] [6-5/5]
  subtype Offset_Range is Integer;
  type Match_Cell is record
    First_Offset      :  Offset_Range;
    Last_Offset_Start :  Offset_Range;
    Last_Offset_Stop  :  Offset_Range;
  end record;
  -- First cell (0) is the indexes of the string matching the regex
  --  other cells (1 .. n) are the indexes os substrings
  type Match_Array is array (Natural range <>) of Match_Cell;
  -- Specific values
  No_Match_Array : Match_Array (1 .. 0);
  subtype One_Match_Array is Match_Array (1 .. 1);
  No_Match : constant Match_Cell := (0, 0, 0);
  Any_Match : constant Match_Cell := (1, 0, 0);

  -- Compile a regex
  -- By default
  --  Case is sensitive,
  --  '^' and '$' do not match a newline in the middle of Str
  --  '.' does not match all characters (e.g. new line, carriage return...)
  procedure Compile (Result : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Str : in String;
                     Case_Sensitive : in Boolean := True;
                     Multi_Line : in Boolean := False;
                     Dot_All : in Boolean := False);

  -- Check syntax of a regex, return True if OK
  function Check (Criteria : String) return Boolean;

  -- Execute a regex
  -- If Mach_Info is empty, N_Matched is set to 1 (match) or 0 (not match)
  -- Otherwise N_Matched is set to 0 (not match) or to the last non empty slot
  --  of Match_Info
  -- Beware that some slots from 1 to N_Matched might have strange values:
  --  - No_Match e.g. when an optional substring does not match
  --  - empty string (First_Offset > Last_Offset) e.g. when Criteria is like
  --    'toto($)', including (1, 0) when Criteria is like '(^)toto'.
  -- Also beware that an empty To_Check can match a Criteria that is
  --  only made of optional patterns (e.g. '' matches 't*'). In this
  --  case Match_Info is one cell of Any_Match.
  -- Use Valid_Match to check if you can use a Match_Cell to extract the
  --  corresponding substring.
  -- Begin_Line_Match and End_Line_Match allow '^' and '$' to match beginning
  --  and end of line respectively (if regexp was compiled with Multi_Line).
  No_Criteria : exception;
  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  N_Matched : out Natural;
                  Match_Info : out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True);

  -- Compare string Str to Criteria (Compile and Exec with default values)
  -- Return a Match_Array of size between 0 (no match) and the requested
  --  Max_Match depending on how many substrigs have matched
  -- May raise No_Criteria if Criteria does not compile
  function Match (Criteria, Str : String; Max_Match : Positive := 10)
                  return Match_Array;

  -- Compare string Str to Criteria (Compile and Exec with default values)
  -- Returns No_Match or a Match_Cell (possibly Any_Match) corresponding
  --  to Match_Info(1)
  -- May raise No_Criteria if Criteria does not compile
  function Match (Criteria, Str : String) return Match_Cell;

  -- Check that a Match_Cell (returned by Exec or Match) is valid
  --  i.e. it can be used to extract a matching (sub) string
  -- No_Match and Any_Match are not valid
  function Valid_Match (Cell : Match_Cell) return Boolean;

  -- Check that a Match_Cell or Match_Array (returned by Exec or Match)
  --  matches strictly the string Str
  -- Strict means that the complete Str matches the criteria, i.e.
  --  Cells(1).First_Offset = Str'First and
  --  Cells(1).Last_Offset_Stop = Str'Last
  -- Beware that a strict match is not necessarily valid (e.g. Any_Match
  --  strictly matches "" but is not valid)
  function Strict_Match (Str : String; Cell  : Match_Cell)  return Boolean;
  function Strict_Match (Str : String; Cells : Match_Array) return Boolean;

  -- Compare string Str to Criteria (Compile and Exec with default values)
  -- If Strict is set, then True is returned if and only if the
  --  complete Str matches the criteria (i.e. First_Offset = Str'First
  --  and Last_Offset_Stop = Str'Last)
  -- Note that setting Strict to True does not prevent lazyness of evaluation,
  --  so if there are different ways for Str to match Criteria then the Match
  --  may select the shortest alternative and the strictness check may fail;
  --  this is not the case if the Criteria starts by '^' and ends by '$'
  -- Example: "ti" matches "to?|ti"  but not strictly, while it stricly matches
  --  "^(to?|ti)$" and "ti|to?"
  -- May raise No_Criteria if Criteria does not compile
  function Match (Criteria, Str : String;
                  Strict : Boolean) return Boolean;

  -- Get Compilation error
  function Error (Criteria : Compiled_Pattern) return String;

  -- Free compiled (or error) pattern
  procedure Free (Criteria : in out Compiled_Pattern);

private

  type Compiled_Pattern is limited
                new Ada.Finalization.Limited_Controlled with record
    Lang : Language.Language_List := Language.Get_Env;
    Comp_Addr : System.Address := System.Null_Address;
    Error : Integer := 0;
  end record;

  overriding procedure Finalize  (Criteria : in out Compiled_Pattern);

end Regular_Expressions;

