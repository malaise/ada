package As.B is

  -- Bounded strings
  type Asb_Bs (Max : Natural) is tagged private;

  Length_Error : exception;

  procedure Set_Null (Target : in out Asb_Bs);
  function Is_Null (Source : Asb_Bs) return Boolean;
  function Length (Source : Asb_Bs) return Natural;

  function Tbs (Str : String) return Asb_Bs;
  function Tbs (Char : Character) return Asb_Bs;
  function Image (Str : Asb_Bs) return String;
  procedure Set (Target : out Asb_Bs; Val : in Asb_Bs);
  procedure Set (Target : out Asb_Bs; Val : in String);
  procedure Set (Target : out Asb_Bs; Val : in Character);

  function Bslice (Source : Asb_Bs;
                   Low : Positive; High : Natural) return Asb_Bs;
  procedure Bslice (Source : in Asb_Bs; Target : out Asb_Bs;
                    Low : in Positive; High : in Natural);
  function Slice (Source : Asb_Bs;
                  Low : Positive; High : Natural) return String;

  procedure Prepend (Source : in out Asb_Bs; New_Item : in Asb_Bs);
  procedure Prepend (Source : in out Asb_Bs; New_Item : in String);
  procedure Prepend (Source : in out Asb_Bs; New_Item : in Character);
  procedure Append (Source : in out Asb_Bs; New_Item : in Asb_Bs);
  procedure Append (Source : in out Asb_Bs; New_Item : in String);
  procedure Append (Source : in out Asb_Bs; New_Item : in Character);
  function "&" (Left, Right : Asb_Bs) return Asb_Bs;
  function "&" (Left : Asb_Bs; Right : String) return Asb_Bs;
  function "&" (Left : String; Right : Asb_Bs) return Asb_Bs;
  function "&" (Left : Asb_Bs; Right : Character) return Asb_Bs;
  function "&" (Left : Character; Right : Asb_Bs) return Asb_Bs;

  function Element (Source : Asb_Bs; Index : Positive) return Character;
  procedure Replace_Element (Source : in out Asb_Bs;
                             Index  : in Positive;
                             By     : in Character);

  function "="  (Left, Right : Asb_Bs) return Boolean;
  function "="  (Left : Asb_Bs; Right : String) return Boolean;
  function "="  (Left : String; Right : Asb_Bs) return Boolean;
  function "<"  (Left, Right : Asb_Bs) return Boolean;
  function "<"  (Left : Asb_Bs; Right : String) return Boolean;
  function "<"  (Left : String; Right : Asb_Bs) return Boolean;
  function "<=" (Left, Right : Asb_Bs) return Boolean;
  function "<=" (Left : Asb_Bs; Right : String) return Boolean;
  function "<=" (Left : String; Right : Asb_Bs) return Boolean;
  function ">"  (Left, Right : Asb_Bs) return Boolean;
  function ">"  (Left : Asb_Bs; Right : String) return Boolean;
  function ">"  (Left : String; Right : Asb_Bs) return Boolean;
  function ">=" (Left, Right : Asb_Bs) return Boolean;
  function ">=" (Left : Asb_Bs; Right : String) return Boolean;
  function ">=" (Left : String; Right : Asb_Bs) return Boolean;

  function Locate (Within     : Asb_Bs;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural;
  function Count (Source   : Asb_Bs;
                  Pattern : String) return Natural;

  procedure Replace (Source   : in out Asb_Bs;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asb_Bs);
  procedure Replace (Source   : in out Asb_Bs;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in String);

  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in Asb_Bs);
  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in String);

  procedure Delete (Source  : in out Asb_Bs;
                    From    : in Positive;
                    Through : in Natural);

  function Head (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs;
  function Tail (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs;

  function "*" (Left  : Natural; Right : Character) return Asb_Bs;
  function "*" (Left  : Natural; Right : String) return Asb_Bs;
  function "*" (Left  : Natural; Right : Asb_Bs) return Asb_Bs;

private

  type Asb_Bs (Max : Natural) is tagged record
    Ref : String (1 .. Max) := (others => ' ');
    Last : Natural := 0;
  end record;

end As.B;

