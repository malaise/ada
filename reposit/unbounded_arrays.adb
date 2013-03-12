with Ada.Unchecked_Deallocation;
package body Unbounded_Arrays is

  procedure Free (X : in out Array_Access) is
    procedure Deallocate is
       new Ada.Unchecked_Deallocation (Element_Array, Array_Access);
  begin
    --  Do not try to free statically allocated null array
    if X /= Null_Unbounded_Array.Reference then
       Deallocate (X);
    end if;
  end Free;

  procedure Check_Indexes (Low  : in Positive;
                           High : in Natural;
                           Last : in Natural;
                           Check_High : in Boolean) is
  begin
    if Low > Last + 1 or else (Check_High and then High > Last) then
      raise Index_Error;
    end if;
  end Check_Indexes;

  procedure Check_Index (Index        : in Positive;
                         Last         : in Natural;
                         Allow_Append : in Boolean) is
  begin
    if Index <= Last then
      -- Index within range
      return;
    end if;
    if Allow_Append and then Index = Last + 1 then
      -- Append = insert before last + 1
      return;
    end if;
    raise Index_Error;
  end Check_Index;

  -- Size computed to Add to Length
  function New_Size (Length : Natural; Add : Positive) return Positive is
  begin
    return Length + Add + Growth_Offset + (Length / Growth_Factor);
  end New_Size;

  -- Store Item in Unbounded array, re-alloc if necessary
  procedure Store (Within : in out Unbounded_Array;
                   Item  : in Element_Array) is
    Incr : Integer;
    Acc : Array_Access;
  begin
    Incr := Item'Length - Within.Reference'Length;
    if Incr <= 0 then
      if Item'Length = 0 then
        -- Item to store is empty
        Free (Within.Reference);
        Within.Reference := Null_Array'Access;
      else
        -- Item to store fits in Reference, no need to re-alloc
        Within.Reference.all(1 .. Item'Length) := Item;
      end if;
    else
      -- Re-alloc reference to new size
      Acc := new Element_Array (1 .. New_Size (Within.Reference'Length, Incr));
      Acc(1 .. Item'Length) := Item;
      Free (Within.Reference);
      Within.Reference := Acc;
    end if;
    Within.Last := Item'Length;
  end Store;

  -----------------------
  -- PUBLIC operations --
  -----------------------

  procedure Set_Null (Target : in out Unbounded_Array) is
  begin
   -- Optim: avoid copying Null_Unb_Array
    Free (Target.Reference);
    Target.Reference := Null_Array'Access;
    Target.Last := 0;
  end Set_Null;

  -- Length and element
  function Is_Null (Source : Unbounded_Array) return Boolean is
  begin
    return Source = Null_Unbounded_Array;
  end Is_Null;

  function Length (Source : Unbounded_Array) return Natural is
  begin
    return Source.Last;
  end Length;

  function Element (Source : Unbounded_Array;
                    Index  : Positive) return Element_Type is
  begin
    Check_Index (Index, Source.Last, False);
    return Source.Reference.all(Index);
  end Element;

  procedure Replace_Element (Source : in out Unbounded_Array;
                             Index  : in Positive;
                             By     : in Element_Type) is
  begin
    Check_Index (Index, Source.Last, False);
    Source.Reference.all(Index) := By;
  end Replace_Element;

  -- Conversions
  function To_Unbounded_Array (Source : Element_Type) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    Res.Reference := new Element_Array (1 .. 1);
    Res.Reference(1) := Source;
    Res.Last := 1;
    return Res;
  end To_Unbounded_Array;

  function To_Unbounded_Array (Source : Element_Array) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    if Source'Length = 0 then
      Res := Null_Unbounded_Array;
    else
      Res.Reference := new Element_Array (1 .. Source'Length);
      Res.Reference.all := Source;
      Res.Last := Source'Length;
    end if;
    return Res;
  end To_Unbounded_Array;

  function To_Array (Source : Unbounded_Array) return Element_Array is
  begin
    return Source.Reference(1 .. Source.Last);
  end To_Array;

  -- Prepend
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Unbounded_Array) is

  begin
    Store (Source, New_Item.Reference.all & Source.Reference(1 .. Source.Last));
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Array) is
  begin
    Store (Source, New_Item & Source.Reference(1 .. Source.Last));
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Type) is
  begin
    Store (Source, New_Item & Source.Reference(1 .. Source.Last));
  end Prepend;

  -- Append
  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Unbounded_Array) is

  begin
    if New_Item.Last <= Source.Reference'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Reference(Source.Last + 1 .. Source.Last +  New_Item.Last) :=
          New_Item.Reference(1 .. New_Item.Last);
      Source.Last := Source.Last + New_Item.Last;
    else
      Store (Source, Source.Reference(1 .. Source.Last)
                   & New_Item.Reference(1 .. New_Item.Last));
    end if;
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Array) is
  begin
    if New_Item'Length <= Source.Reference'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Reference(Source.Last + 1 .. Source.Last +  New_Item'Length) :=
          New_Item;
      Source.Last := Source.Last + New_Item'Length;
    else
      Store (Source, Source.Reference(1 .. Source.Last) & New_Item);
    end if;
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Type) is
  begin
    if 1 <= Source.Reference'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Reference(Source.Last + 1) := New_Item;
      Source.Last := Source.Last + 1;
    else
      Store (Source, Source.Reference.all & New_Item);
    end if;
  end Append;

  function "&" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Unbounded_Array;
                Right : Element_Array) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Element_Array;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array := To_Unbounded_Array (Left);
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Unbounded_Array;
                Right : Element_Type) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Element_Type;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array := To_Unbounded_Array (Left);
  begin
    Append (Res, Right);
    return Res;
  end "&";

  -- Replace
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Unbounded_Array) is
    Start_Tail : Positive;
  begin
    Check_Indexes (Low, High, Source.Last, False);
    Start_Tail := (if Low <= High then High + 1 -- Replace
                   else Low);                   -- Insert
    Store (Source, Source.Reference(1 .. Low - 1)
                 & By.Reference(1 .. By.Last)
                 & Source.Reference(Start_Tail .. Source.Last));
  end Replace;

  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Element_Array) is
    Start_Tail : Positive;
  begin
    Check_Indexes (Low, High, Source.Last, False);
    Start_Tail := (if Low <= High then High + 1 -- Replace
                   else Low);                   -- Insert
    Store (Source, Source.Reference(1 .. Low - 1)
                 & By
                 & Source.Reference(Start_Tail .. Source.Last));
  end Replace;

  -- Insert
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Unbounded_Array) is
  begin
    Check_Index (Before, Source.Last, True);
    Store (Source, Source.Reference(1 .. Before - 1)
                 & New_Item.Reference(1 .. New_Item.Last)
                 & Source.Reference(Before .. Source.Last));
  end Insert;

  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Element_Array) is
  begin
    Check_Index (Before, Source.Last, True);
    Store (Source, Source.Reference(1 .. Before - 1)
                 & New_Item
                 & Source.Reference(Before .. Source.Last));
  end Insert;

  -- Delete
  procedure Delete (Source  : in out Unbounded_Array;
                    From    : in Positive;
                    Through : in Natural) is
    New_Len : Natural;
  begin
    if Through < From then
      return;
    end if;
    if Through > Source.Last then
      raise Index_Error;
    end if;
    New_Len := Source.Last - (Through - From + 1);
    if New_Len = 0 then
      Free(Source.Reference);
      Source := Null_Unbounded_Array;
    else
      Source.Reference(1 .. New_Len) :=
             Source.Reference(1 .. From - 1)
           & Source.Reference(Through + 1 .. Source.Last);
      Source.Last := New_Len;
    end if;
  end Delete;

  -- Slice
  function Slice (Source : Unbounded_Array;
                  Low    : Positive;
                  High   : Natural) return Element_Array is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Source.Reference.all(Low .. High);
  end Slice;

  function Unbounded_Slice (Source : Unbounded_Array;
                            Low    : Positive;
                            High   : Natural) return Unbounded_Array is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return To_Unbounded_Array (Source.Reference.all(Low .. High));
  end Unbounded_Slice;

  -- Comparisons
  function "=" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left.Reference.all(1 .. Left.Last) =
           Right.Reference.all(1 .. Right.Last);
  end "=";
  function "=" (Left  : Unbounded_Array;
                Right : Element_Array) return Boolean is
  begin
    return Left.Reference.all(1 .. Left.Last) = Right;
  end "=";
  function "=" (Left  : Element_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left = Right.Reference.all(1 .. Right.Last);
  end "=";

  -- Life cycle
  overriding procedure Initialize (Object : in out Unbounded_Array) is
  begin
    Object.Reference := Null_Array'Access;
    Object.Last := 0;
  end Initialize;
  overriding procedure Adjust (Object : in out Unbounded_Array) is
  begin
    if Object.Reference /= Null_Array'Access then
      -- Real copy
      Object.Reference := new Element_Array'(
                        Object.Reference(1 .. Object.Last));
    end if;
  end Adjust;
  overriding procedure Finalize (Object : in out Unbounded_Array) is
  begin
    Free (Object.Reference);
    Object.Reference := Null_Array'Access;
    Object.Last := 0;
  end Finalize;

end Unbounded_Arrays;

