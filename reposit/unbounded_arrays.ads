-- Unbounded array of elements
with Ada.Finalization;
generic
  type Element_Type is private;
  type Element_Array is array (Positive range <>) of Element_Type;

  -- Each time a re-allocation is needed, increment Length by
  --  Nb_To_Add + Growth_Offset + Curr_Length / Growth_Factor
  --  so that some further growths will not lead to too many re-allocations
  --  or too large allocations
  -- Always allocate a little bit more than requested
  Growth_Offset : Natural := 32;
  -- Allocate even a little bit more if the string becomes large
  Growth_Factor : Natural := 64;

package Unbounded_Arrays is

  -- Access to array
  type Array_Access is access all Element_Array;

  -- The unbounded array of elements
  type Unbounded_Array is tagged private;
  subtype Unb_Array is Unbounded_Array;

  Index_Error : exception;

  -- Empty unbounded array
  Null_Unbounded_Array : constant Unbounded_Array;
  Null_Unb_Array : constant Unbounded_Array;

  -- Null unbounded array and unbounded array length
  procedure Set_Null (Target : in out Unbounded_Array);
  function Is_Null (Source : Unbounded_Array) return Boolean;
  function Length (Source : Unbounded_Array) return Natural;

  -- Conversions from and to array
  function To_Unbounded_Array (Source : Element_Type) return Unbounded_Array;
  function To_Unb_Array (Source : Element_Type)  return Unbounded_Array
                                                 renames To_Unbounded_Array;
  function To_Unbounded_Array (Source : Element_Array) return Unbounded_Array;
  function To_Unb_Array (Source : Element_Array) return Unbounded_Array
                                                 renames To_Unbounded_Array;
  function To_Array (Source : Unbounded_Array) return Element_Array;
  procedure Set (Target : out Unbounded_Array; Val : in Unbounded_Array);
  procedure Set (Target : out Unbounded_Array; Val : in Element_Array);
  procedure Set (Target : out Unbounded_Array; Val : in Element_Type);

  -- Slices
  -- May raise Index_Error if Low > Source.Length+1 or High > Source.Length
  function Slice (Source : Unbounded_Array;
                  Low    : Positive;
                  High   : Natural) return Element_Array;
  function Unbounded_Slice (Source : Unbounded_Array;
                            Low    : Positive;
                            High   : Natural) return Unbounded_Array;
  function Unb_Slice (Source : Unbounded_Array;
                      Low    : Positive;
                      High   : Natural) return Unbounded_Array
                                        renames Unbounded_Slice;

  -- Concatenations
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Unbounded_Array);
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Array);
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Type);

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Unbounded_Array);
  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Array);
  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Type);
  function "&" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Unbounded_Array;
  function "&" (Left  : Unbounded_Array;
                Right : Element_Array) return Unbounded_Array;
  function "&" (Left  : Element_Array;
                Right : Unbounded_Array) return Unbounded_Array;
  function "&" (Left  : Unbounded_Array;
                Right : Element_Type) return Unbounded_Array;
  function "&" (Left  : Element_Type;
                Right : Unbounded_Array) return Unbounded_Array;

  -- Element
  -- May raise Index_Error if Index > Source.Length
  function Element (Source : Unbounded_Array;
                    Index  : Positive) return Element_Type;
  procedure Replace_Element (Source : in out Unbounded_Array;
                             Index  : in Positive;
                             By     : in Element_Type);

  -- Comparisons
  function "=" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Boolean;
  function "=" (Left  : Unbounded_Array;
                Right : Element_Array) return Boolean;
  function "=" (Left  : Element_Array;
                Right : Unbounded_Array) return Boolean;

  -- Locate a fragment
  function Locate (Within     : Unbounded_Array;
                   Fragment   : Element_Array;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural;

  -- Count occurences of a fragment
  function Count (Source  : Unbounded_Array;
                  Pattern : Element_Array) return Natural;


  -- Overwrite a part of a string by a new one
  -- Do nothing if New_Item is Asu_Null
  -- Append New_Item if Position = Source.Length + 1
  -- Extend Source if Position + New_Item.Length - 1 > Source.Length
  -- May raise Index_Error if Position > Source.Length + 1
  procedure Overwrite (Source   : in out Unbounded_Array;
                       Position : in Positive;
                       New_Item : in Unbounded_Array);
  procedure Overwrite (Source   : in out Unbounded_Array;
                       Position : in Positive;
                       New_Item : in Element_Array);


  -- Replace a slice by a new string
  -- Delete chars if By is Asu_Null (except if High < Low)
  -- Insert By before Low if High < Low
  -- Append By if Low = Source.Length + 1 (and High < Low)
  -- May raise Index_Error if Low > Source.Length + 1 or High > Source.Length
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Unbounded_Array);
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Element_Array);

  -- Insert a string before a given position
  -- Append if Before = Source.Length + 1
  -- May raise Index_Error if Before > Source.Length + 1
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Unbounded_Array);
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Element_Array);

  -- Delete characters from From to Through included
  -- Do nothing if Through < From
  -- May raise Index_Error if Through >= From
  --  and From > Source.Length or Through > Source.Length
  procedure Delete (Source  : in out Unbounded_Array;
                    From    : in Positive;
                    Through : in Natural);

  -- Delete Number elements from From included
  --  or as many elements as possible
  -- May raise Index_Error if From > Source.Length
  procedure Delete_Nb (Source : in out Unbounded_Array;
                       From   : in Positive;
                       Number : in Natural);

  -- Delete trailing elements
  -- Delete elements from Source.Length - Number + 1 to Source.Length
  -- Source becomes Asu_Null if Number >= Source.Length
  procedure Trail (Source : in out Unbounded_Array;
                   Number : in Natural);

  -- Extract Count elements from head or tail of Source
  -- Pad with Pad if Count > Source.Length
  function Head (Source : Unbounded_Array;
                 Count  : Natural;
                 Pad    : Element_Type) return Unbounded_Array;
  function Tail (Source : Unbounded_Array;
                 Count  : Natural;
                 Pad    : Element_Type) return Unbounded_Array;

  -- Initialize from a repeated pattern
  function "*" (Left : Natural; Right : Element_Type)    return Unbounded_Array;
  function "*" (Left : Natural; Right : Element_Array)   return Unbounded_Array;
  function "*" (Left : Natural; Right : Unbounded_Array) return Unbounded_Array;

private

  -- Default value is an ampty array
  Empty_Array : Element_Array (1 .. 0);
  Null_Array : aliased Element_Array := Empty_Array;


  type Unbounded_Array is new Ada.Finalization.Controlled with record
    Ref : Array_Access := Null_Array'Access;
    Last : Natural := 0;
  end record;

  overriding procedure Initialize (Object : in out Unbounded_Array);
  overriding procedure Adjust (Object : in out Unbounded_Array);
  overriding procedure Finalize (Object : in out Unbounded_Array);

  Null_Unbounded_Array : constant Unbounded_Array :=
     (Ada.Finalization.Controlled with Ref => Null_Array'Access,
                                       Last      => 0);
  Null_Unb_Array : constant Unbounded_Array := Null_Unbounded_Array;

end Unbounded_Arrays;

