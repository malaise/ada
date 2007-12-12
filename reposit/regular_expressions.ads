-- Binding to Posix regular expression
with System, Ada.Finalization;
with Language;
package Regular_Expressions is

  -- Result of regex compilation
  type Compiled_Pattern is limited private;

  -- Matching information:
  -- Filled with indexes in string To_Check, up to (1, 0)
  --  defining matching substrings
  -- When a character is encoded on several bytes,
  --  Language is used to detect the end of this sequence and
  --  set Last_Offset_Stop to the last byte of the sequence.
  -- abcab matches (ab)c\1* at pos [1-5] [1-2]
  subtype Offset_Range is Integer;
  type Match_Cell is record
    First_Offset      :  Offset_Range;
    Last_Offset_Start :  Offset_Range;
    Last_Offset_Stop  :  Offset_Range;
  end record;
  type Match_Array is array (Natural range <>) of Match_Cell;
  No_Match_Array : Match_Array (1 .. 0);
  subtype One_Match_Array is Match_Array (1 .. 1);
  No_Match : constant Match_Cell := (0, 0, 0);
  Any_Match : constant Match_Cell := (1, 0, 0);

  -- Compile a regex, using Language
  procedure Compile (Result : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Extended : in Boolean := True;
                     Case_Sensitive : in Boolean := True;
                     Match_Newline : in Boolean := True);

  -- Execute a regex
  -- If Mach_Info is empty, N_Matched is set to 1 (match) or 0 (not match)
  --  otherwise N_Matched is set to 0 or to the last non empty slot of
  --  Match_Info (but some slots from 1 to N_Matched might be empty)
  -- Also beware that an empty To_Check can match a Criteria that is
  --  only made of optional patterns (e.g. "" matches "t*"). In this
  --  case Match_Info is one cell of Any_Match.
  No_Criteria : exception;
  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  N_Matched : out Natural;
                  Match_Info : in out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True);

 -- Compare string Str to Criteria
  -- Returns No_Match or a Match_Cell (possibly Any_Match)
  -- May raise No_Criteria is Criteria does not compile
  function Match (Criteria, Str : String) return Match_Cell;

  -- Compare string Str to Criteria
  -- Returns True or False
  -- If Strict is set, then True is returned if and only if the
  --  complete Str matches the criteria (i.e. First_Offset = Str'First
  --  and Last_Offset_Stop = Str'Last)
  -- True is also returned if Any_Match
  -- May raise No_Criteria is Criteria does not compile
  function Match (Criteria, Str : String;
                  Strict : in Boolean) return Boolean;

  -- Get Compilation error
  function Error (Criteria : in Compiled_Pattern) return String;

  -- Free compiled (or error) pattern
  procedure Free (Criteria : in out Compiled_Pattern);

private

  type Compiled_Pattern is limited
                new Ada.Finalization.Limited_Controlled with record
    Lang : Language.Language_List := Language.Get_Env;
    Comp_Addr : System.Address := System.Null_Address;
    Error : Integer := 0;
  end record;

  procedure Finalize  (Criteria : in out Compiled_Pattern);

end Regular_Expressions;

