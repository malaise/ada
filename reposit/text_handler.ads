-- P. MALAISE
with System;
package Text_Handler is

  Maximum : constant := 32*1024;
  subtype Index is Integer range 0 .. Maximum;

  type Text (Maximum_Length : Index) is limited private;

  Empty_Text : constant Text;

  function Length (T : Text) return Index;
  function Value  (T : Text) return String;
  function Empty  (T : Text) return Boolean;

  procedure Empty (T : in out Text);

  function To_Text (S : String;    Max : Index) return Text;
  function To_Text (C : Character; Max : Index) return Text;
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

  procedure Amend (To : in out Text; By : in Text; Position : in Index);
  procedure Amend (To : in out Text; By : in String; Position : in Index);
  procedure Amend (To : in out Text; By : in Character; Position : in Index);

  -- Return 0 if not found or if Within or Fragment is empty
  function Locate (Within : Text; Fragment : Text;
                   Occurence : Positive := 1) return Index;
  function Locate (Within : Text; Fragment : String;
                   Occurence : Positive := 1) return Index;
  function Locate (Within : Text; Fragment : Character;
                   Occurence : Positive := 1) return Index;

  -- Appends Ascii Null to From.Val (From.Max_Len must be long enough).
  -- String_Address is From.Val'Address and can then be passed to a C function.
  procedure String_For_C (From : in out Text;
                          String_Address : out System.Address);

private

  type Text (Maximum_Length : Index) is record
    Len : Index := 0;
    Val : String (1 .. Maximum_Length);
  end record;

  Empty_Text : constant Text := (Maximum_Length => 0, Len => 0, Val => "");

end Text_Handler;

