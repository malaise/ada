-- Binding to Posix regular expression
with System, Ada.Finalization;
package Regular_Expressions is

  -- Result of regex compilation
  type Compiled_Pattern is limited private;

  -- Matching information
  -- Filled with indexes in string To_Check, up to (1, 0)
  --  defining matching substrings
  -- abcab matches (ab)c\1* at pos [1-5] [1-2]
  subtype Offset_Range is Integer;
  type Match_Cell is record
    Start_Offset :  Offset_Range;
    End_Offset   :  Offset_Range;
  end record;
  type Match_Array is array (Natural range <>) of Match_Cell;
  No_Match_Array : Match_Array (1 .. 0);
  subtype One_Match_Array is Match_Array (1 .. 1);

  -- Compile a regex
  procedure Compile (Result : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Extended : in Boolean := True;
                     Case_Sensitive : in Boolean := True;
                     Match_Newline : in Boolean := True);

  -- Execute a regex
  No_Criteria : exception;
  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  Match : out Boolean;
                  Mach_Info : in out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True);

  -- Compare string to criteria
  -- May raise No_Criteria is Criteria does not compile.
  function Match (Criteria, Str : String) return Boolean;

  -- Get Compilation error
  function Error (Criteria : in Compiled_Pattern) return String;

  -- Free compiled (or error) pattern
  procedure Free (Criteria : in out Compiled_Pattern);

private

  type Compiled_Pattern is new Ada.Finalization.Controlled with record
    Comp_Addr : System.Address := System.Null_Address;
    Error : Integer := 0;
  end record;

  procedure Finalize  (Criteria : in out Compiled_Pattern);

end Regular_Expressions;

