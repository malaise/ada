with Ada.Finalization;
package As.U is

  -- Unbounded strings
  type Asu_Us is tagged private;

  -- Access and array types
  type Asu_Us_Access is access all Asu_Us;
  type Asu_Array is array (Positive range <>) of Asu_Us;

  -- Null unbounded string and unbounded string length
  Asu_Null : constant Asu_Us;
  procedure Set_Null (Target : in out Asu_Us);
  function Is_Null (Source : Asu_Us) return Boolean;
  function Length (Source : Asu_Us) return Natural;

  -- Conversion from and to string
  function Tus (Str : String) return Asu_Us;
  function Tus (Char : Character) return Asu_Us;
  function Image (Str : Asu_Us) return String;
  procedure Set (Target : out Asu_Us; Val : in Asu_Us);
  procedure Set (Target : out Asu_Us; Val : in String);
  procedure Set (Target : out Asu_Us; Val : in Character);

  -- Slices
  -- May raise Index_Error if Low > Source.Length+1 or High > Source.Length
  function Uslice (Source : Asu_Us;
                   Low : Positive; High : Natural) return Asu_Us;
  procedure Uslice (Source : in Asu_Us; Target : out Asu_Us;
                    Low : in Positive; High : in Natural);
  function Slice (Source : Asu_Us;
                  Low : Positive; High : Natural) return String;

  -- Concatenations
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

  -- Element
  -- May raise Index_Error if Index > Source.Length
  function Element (Source : Asu_Us; Index : Positive) return Character;
  procedure Replace_Element (Source : in out Asu_Us;
                             Index  : in Positive;
                             By     : in Character);

  -- Comparisons
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

  -- Locate a fragment
  function Locate (Within     : Asu_Us;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural;

  -- Count occurences of a fragment
  function Count (Source  : Asu_Us;
                  Pattern : String) return Natural;

  -- Overwrite a part of a string by a new one
  -- Do nothing if New_Item is Asu_Null
  -- Append New_Item if Position = Source.Length + 1
  -- Extend Source if Position + New_Item.Length - 1 > Source.Length
  -- May raise Index_Error if Position > Source.Length + 1
  procedure Overwrite (Source   : in out Asu_Us;
                       Position : in Positive;
                       New_Item : in Asu_Us);
  procedure Overwrite (Source   : in out Asu_Us;
                       Position : in Positive;
                       New_Item : in String);

  -- Replace a slice by a new string
  -- Delete chars if By is Asu_Null (except if High < Low)
  -- Insert By before Low if High < Low
  -- Append By if Low = Source.Length + 1 (and High < Low)
  -- May raise Index_Error if Low > Source.Length + 1 or High > Source.Length
  procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asu_Us);
  procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in String);

  -- Insert a string before a given position
  -- Append if Before = Source.Length + 1
  -- May raise Index_Error if Before > Source.Length + 1
  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in Asu_Us);
  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in String);

  -- Delete characters from From to Through included
  -- Do nothing if Through < From
  -- May raise Index_Error if Through >= From
  --  and From > Source.Length or Through > Source.Length
  procedure Delete (Source  : in out Asu_Us;
                    From    : in Positive;
                    Through : in Natural);

  -- Delete Number characters from From included
  --  or as many characters as possible
  -- May raise Index_Error if From > Source.Length
  procedure Delete_Nb (Source : in out Asu_Us;
                       From   : in Positive;
                       Number : in Natural);

  -- Delete trailing characters
  -- Delete characters from Source.Length - Number + 1 to Source.Length
  -- Source becomes Asu_Null if Number >= Source.Length
  procedure Trail (Source : in out Asu_Us;
                   Number : in Natural);

  -- Extract Count characters from head or tail of Source
  -- Pad with Pad if Count > Source.Length
  function Head (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us;
  function Tail (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us;

  -- Initialize from a repeated pattern
  function "*" (Left : Natural; Right : Character) return Asu_Us;
  function "*" (Left : Natural; Right : String) return Asu_Us;
  function "*" (Left : Natural; Right : Asu_Us) return Asu_Us;

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

