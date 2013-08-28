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

  -- the unbounded array of elements
  type Unbounded_Array is tagged private;
  subtype Unb_Array is Unbounded_Array;

  -- Empty unbounded array
  procedure Set_Null (Target : in out Unbounded_Array);
  Null_Unbounded_Array : constant Unbounded_Array;
  Null_Unb_Array : constant Unbounded_Array;

  Index_Error : exception;

  -- Length and element
  function Is_Null (Source : Unbounded_Array) return Boolean;
  function Length (Source : Unbounded_Array) return Natural;
  function Element (Source : Unbounded_Array;
                    Index  : Positive) return Element_Type;
  procedure Replace_Element (Source : in out Unbounded_Array;
                             Index  : in Positive;
                             By     : in Element_Type);

  -- Conversions
  function To_Unbounded_Array (Source : Element_Type)  return Unbounded_Array;
  function To_Unb_Array (Source : Element_Type)  return Unbounded_Array
                                                 renames To_Unbounded_Array;
  function To_Unbounded_Array (Source : Element_Array) return Unbounded_Array;
  function To_Unb_Array (Source : Element_Array) return Unbounded_Array
                                                 renames To_Unbounded_Array;
  function To_Array (Source : Unbounded_Array) return Element_Array;

  -- Prepend, Append, Concatenate
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

  -- Replace / Insert / Delete
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Unbounded_Array);
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Element_Array);

  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Unbounded_Array);
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Element_Array);

  procedure Delete (Source  : in out Unbounded_Array;
                    From    : in Positive;
                    Through : in Natural);

  -- Extract Slice
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

  -- Comparisons
  function "=" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Boolean;
  function "=" (Left  : Unbounded_Array;
                Right : Element_Array) return Boolean;
  function "=" (Left  : Element_Array;
                Right : Unbounded_Array) return Boolean;

private

  -- Default value is an ampty array
  Empty_Array : Element_Array (1 .. 0);
  Null_Array : aliased Element_Array := Empty_Array;


  type Unbounded_Array is new Ada.Finalization.Controlled with record
    Reference : Array_Access := Null_Array'Access;
    Last : Natural := 0;
  end record;

  overriding procedure Initialize (Object : in out Unbounded_Array);
  overriding procedure Adjust (Object : in out Unbounded_Array);
  overriding procedure Finalize (Object : in out Unbounded_Array);

  Null_Unbounded_Array : constant Unbounded_Array :=
     (Ada.Finalization.Controlled with Reference => Null_Array'Access,
                                       Last      => 0);
  Null_Unb_Array : constant Unbounded_Array := Null_Unbounded_Array;

end Unbounded_Arrays;

