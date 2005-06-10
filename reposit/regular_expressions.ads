-- Posix regular expression
with System, Ada.Finalization;
package Regular_Expressions is

  type Compiled_Pattern is limited private;

  subtype Offset_Range is Integer;
  type Match_Cell is record
    Start_Offset :  Offset_range;
    End_Offset   :  Offset_range;
  end record;
  type Match_Array is array (Natural range <>) of Match_Cell;

  procedure Compile (Result : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Extended : in Boolean := True;
                     Case_Sensitive : in Boolean := True;
                     Match_Newline : in Boolean := True);

  No_Criteria : exception;
  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  Match : out Boolean;
                  Mach_Info : in out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True);

  function Error (Criteria : in Compiled_Pattern) return String;

  procedure Free (Criteria : in out Compiled_Pattern);
                   
private

  type Compiled_Pattern is new Ada.Finalization.Controlled with record
    Comp_Addr : System.Address := System.Null_Address;
    Error : Integer := 0;
  end record;

  procedure Finalize  (Criteria : in out Compiled_Pattern);

end Regular_Expressions;

