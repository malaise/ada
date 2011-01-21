with Ada.Finalization;
package As.U is

  -- Unbounded strings
  type Asu_Us is tagged private;

  Asu_Null : constant Asu_Us;
  procedure Set_Null (Target : in out Asu_Us);
  function Is_Null (Source : Asu_Us) return Boolean;
  function Length (Source : Asu_Us) return Natural;

  function Tus (Str : String) return Asu_Us;
  function Tus (Char : Character) return Asu_Us;
  function Image (Str : Asu_Us) return String;
  procedure Set (Target : out Asu_Us; Val : in Asu_Us);
  procedure Set (Target : out Asu_Us; Val : in String);
  procedure Set (Target : out Asu_Us; Val : in Character);

  function Uslice (Source : Asu_Us;
                   Low : Positive; High : Natural) return Asu_Us;
  procedure Uslice (Source : in Asu_Us; Target : out Asu_Us;
                    Low : in Positive; High : in Natural);
  function Slice (Source : Asu_Us;
                  Low : Positive; High : Natural) return String;

  procedure Prepend (Source : in out Asu_Us; New_Item : in Asu_Us);
  procedure Prepend (Source : in out Asu_Us; New_Item : in String);
  procedure Prepend (Source : in out Asu_Us; New_Item : in Character);
  procedure Append (Source : in out Asu_Us; New_Item : in Asu_Us);
  procedure Append (Source : in out Asu_Us; New_Item : in String);
  procedure Append (Source : in out Asu_Us; New_Item : in Character);
  function "&" (Left, Right : Asu_Us) return Asu_Us;
  function "&" (Left : Asu_Us; Right : String) return Asu_Us;
  function "&" (Left : String; Right : Asu_Us) return Asu_Us;
  function "&" (Left : Asu_Us; Right : Character) return Asu_Us;
  function "&" (Left : Character; Right : Asu_Us) return Asu_Us;

  function Element (Source : Asu_Us; Index : Positive) return Character;
  procedure Replace_Element (Source : in out Asu_Us;
                             Index  : in Positive;
                             By     : in Character);

  function "="  (Left, Right : Asu_Us) return Boolean;
  function "="  (Left : Asu_Us; Right : String) return Boolean;
  function "="  (Left : String; Right : Asu_Us) return Boolean;
  function "<"  (Left, Right : Asu_Us) return Boolean;
  function "<"  (Left : Asu_Us; Right : String) return Boolean;
  function "<"  (Left : String; Right : Asu_Us) return Boolean;
  function "<=" (Left, Right : Asu_Us) return Boolean;
  function "<=" (Left : Asu_Us; Right : String) return Boolean;
  function "<=" (Left : String; Right : Asu_Us) return Boolean;
  function ">"  (Left, Right : Asu_Us) return Boolean;
  function ">"  (Left : Asu_Us; Right : String) return Boolean;
  function ">"  (Left : String; Right : Asu_Us) return Boolean;
  function ">=" (Left, Right : Asu_Us) return Boolean;
  function ">=" (Left : Asu_Us; Right : String) return Boolean;
  function ">=" (Left : String; Right : Asu_Us) return Boolean;

  function Locate (Within     : Asu_Us;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural;
  function Count (Source   : Asu_Us;
                  Pattern : String) return Natural;

  procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asu_Us);
  procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in String);

  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in Asu_Us);
  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in String);

  procedure Delete (Source  : in out Asu_Us;
                    From    : in Positive;
                    Through : in Natural);

  function Head (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us;
  function Tail (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us;

  function "*" (Left  : Natural; Right : Character) return Asu_Us;
  function "*" (Left  : Natural; Right : String) return Asu_Us;
  function "*" (Left  : Natural; Right : Asu_Us) return Asu_Us;

private
  type String_Access is access all String;
  Empty_String : String(1 .. 0);

  Null_String : aliased String := Empty_String;

  type Asu_Us is new Ada.Finalization.Controlled with record
    Ref : String_Access := Null_String'Access;
    Last : Natural := 0;
  end record;

  overriding procedure Initialize (Object : in out Asu_Us);
  overriding procedure Adjust (Object : in out Asu_Us);
  overriding procedure Finalize (Object : in out Asu_Us);

  Asu_Null : constant Asu_Us :=
     (Ada.Finalization.Controlled with Ref  => Null_String'Access,
                                       Last => 0);
end As.U;

