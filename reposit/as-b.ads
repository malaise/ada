package As.B is

  -- Bounded strings
  type Asb_Bs (Max : Natural) is tagged private;

  -- Raised when the result exceeds the Max
  Length_Error : exception;

  -- Null bounded string and bounded string length
  function Asb_Null (Max : in Natural := 0) return Asb_Bs;
  procedure Set_Null (Target : in out Asb_Bs);
  function Is_Null (Source : Asb_Bs) return Boolean;
  function Length (Source : Asb_Bs) return Natural;

  -- Conversion from and to string
  function Tbs (Str : String) return Asb_Bs;
  function Tbs (Char : Character) return Asb_Bs;
  function Image (Str : Asb_Bs) return String;
  procedure Set (Target : out Asb_Bs; Val : in Asb_Bs);
  procedure Set (Target : out Asb_Bs; Val : in String);
  procedure Set (Target : out Asb_Bs; Val : in Character);

  -- Slices
  -- May raise Index_Error if Low > Source.Length+1 or High > Source.Length
  function Bslice (Source : Asb_Bs;
                   Low : Positive; High : Natural) return Asb_Bs;
  procedure Bslice (Source : in Asb_Bs; Target : out Asb_Bs;
                    Low : in Positive; High : in Natural);
  function Slice (Source : Asb_Bs;
                  Low : Positive; High : Natural) return String;

  -- Concatenations
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

  -- Element
  -- May raise Index_Error if Index > Source.Length
  function Element (Source : Asb_Bs; Index : Positive) return Character;
  procedure Replace_Element (Source : in out Asb_Bs;
                             Index  : in Positive;
                             By     : in Character);

  -- Comparisons
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

  -- Locate a fragment
  function Locate (Within     : Asb_Bs;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural;

  -- Count occurences of a fragment
  function Count (Source   : Asb_Bs;
                  Pattern : String) return Natural;

  -- Overwrite a part of a string by a new one
  -- Do nothing if New_Item is Asb_Null
  -- Append New_Item if Position = Source.Length + 1
  -- Extend Source if Position + New_Item.Length - 1 > Source.Length
  -- May raise Index_Error if Position > Source.Length + 1
  procedure Overwrite (Source   : in out Asb_Bs;
                       Position   : in Positive;
                       New_Item : in Asb_Bs);
  procedure Overwrite (Source   : in out Asb_Bs;
                       Position   : in Positive;
                       New_Item : in String);

  -- Replace a slice by a new string
  -- Delete chars if By is Asu_Null (except if High < Low)
  -- Insert By before Low if High < Low
  -- Append By if Low = Source.Length + 1 (and High < Low)
  -- May raise Index_Error if Low > Source.Length + 1 or High > Source.Length
  procedure Replace (Source   : in out Asb_Bs;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asb_Bs);
  procedure Replace (Source   : in out Asb_Bs;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in String);

  -- Insert a string before a given position
  -- Append if Before = Source.Length + 1
  -- May raise Index_Error if Before > Source.Length + 1
  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in Asb_Bs);
  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in String);

  -- Delete some characters
  -- Do nothing if Through < From
  -- May raise Index_Error if Through >= From
  --  and From > Source.Length or Through > Source.Length
  procedure Delete (Source  : in out Asb_Bs;
                    From    : in Positive;
                    Through : in Natural);

  -- Extract Count characters from head or tail of Source
  -- Pad with Pad if Count > Source.Length
  function Head (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs;
  function Tail (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs;

  -- Initialize from a repeated pattern
  function "*" (Left  : Natural; Right : Character) return Asb_Bs;
  function "*" (Left  : Natural; Right : String) return Asb_Bs;
  function "*" (Left  : Natural; Right : Asb_Bs) return Asb_Bs;

private

  type Asb_Bs (Max : Natural) is tagged record
    Ref : String (1 .. Max) := (others => ' ');
    Last : Natural := 0;
  end record;

end As.B;

