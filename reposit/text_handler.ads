-- P. MALAISE
with System;
package Text_Handler is

  subtype Max_Len_Range is Integer range 0 .. 32*1024;

  type Text (Max_Len : Max_Len_Range) is limited private;

  Empty_Text : constant Text;

  function Length (T : Text) return Max_Len_Range;
  function Value  (T : Text) return String;
  function Empty  (T : Text) return Boolean;

  procedure Empty (T : in out Text);

  function To_Text (S : String;    Max_Len : Max_Len_Range) return Text;
  function To_Text (C : Character; Max_Len : Max_Len_Range) return Text;
  function To_Text (S : String)    return Text;
  function To_Text (C : Character) return Text;

  function "&" (Left : Text;      Right : Text)      return Text;
  function "&" (Left : Text;      Right : String)    return Text;
  function "&" (Left : String;    Right : Text)      return Text;
  function "&" (Left : Text;      Right : Character) return Text;
  function "&" (Left : Character; Right : Text)      return Text;

  function "="  (Left : Text; Right : Text) return Boolean;
  function "<"  (Left : Text; Right : Text) return Boolean;
  function "<=" (Left : Text; Right : Text) return Boolean;
  function ">"  (Left : Text; Right : Text) return Boolean;
  function ">=" (Left : Text; Right : Text) return Boolean;

  procedure Set (To : in out Text; Value : in Text);
  procedure Set (To : in out Text; Value : in String);
  procedure Set (To : in out Text; Value : in Character);

  procedure Append (To : in out Text; Tail : in Text);
  procedure Append (To : in out Text; Tail : in String);
  procedure Append (To : in out Text; Tail : in Character);

  procedure Amend (To : in out Text; By : in Text;
                   Position : in Max_Len_Range);
  procedure Amend (To : in out Text; By : in String;
                   Position : in Max_Len_Range);
  procedure Amend (To : in out Text; By : in Character;
                   Position : in Max_Len_Range);

  -- Return 0 if not found or if Within or Fragment is empty
  function Locate (Within : Text; Fragment : Text; Occurence : Positive := 1)
                   return Max_Len_Range;
  function Locate (Within : Text; Fragment : String; Occurence : Positive := 1)
                  return Max_Len_Range;
  function Locate (Within : Text; Fragment : Character; Occurence : Positive := 1)
                  return Max_Len_Range;

  -- Appends Ascii.Null to From.Val (From.Max_Len must be long enough).
  -- String_Address is From.Val'Address and can then be passed to a C function.
  procedure String_For_C (From : in out Text; String_Address : out System.Address);

private

  type Text (Max_Len : Max_Len_Range) is record
    Len : Max_Len_Range := 0;
    Val : String (1..Max_Len);
  end record;

  Empty_Text : constant Text := (Max_Len => 0, Len => 0, Val => "");

end Text_Handler;

