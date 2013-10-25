with Ada.Unchecked_Deallocation;
package body Unbounded_Arrays is

  procedure Free (X : in out Array_Access) is
    procedure Deallocate is
       new Ada.Unchecked_Deallocation (Element_Array, Array_Access);
  begin
    --  Do not try to free statically allocated null array
    if X /= Null_Unbounded_Array.Ref then
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
    Incr := Item'Length - Within.Ref'Length;
    if Incr <= 0 then
      if Item'Length = 0 then
        -- Item to store is empty
        Free (Within.Ref);
        Within.Ref := Null_Array'Access;
      else
        -- Item to store fits in Ref, no need to re-alloc
        Within.Ref.all(1 .. Item'Length) := Item;
      end if;
    else
      -- Re-alloc Ref to new size
      Acc := new Element_Array (1 .. New_Size (Within.Ref'Length, Incr));
      Acc(1 .. Item'Length) := Item;
      Free (Within.Ref);
      Within.Ref := Acc;
    end if;
    Within.Last := Item'Length;
  end Store;

  procedure Init (Target : in out Unbounded_Array; Length : Natural) is
  begin
    Free (Target.Ref);
    if Length = 0 then
      Target := Null_Unbounded_Array;
    else
      Target.Ref := new Element_Array(1 .. Length);
      Target.Last := Length;
    end if;
  end Init;

  -----------------------
  -- PUBLIC operations --
  -----------------------

  procedure Set_Null (Target : in out Unbounded_Array) is
  begin
   -- Optim: avoid copying Null_Unb_Array
    Free (Target.Ref);
    Target.Ref := Null_Array'Access;
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

  -- Conversions
  function To_Unbounded_Array (Source : Element_Type) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    Res.Ref := new Element_Array(1 .. 1);
    Res.Ref(1) := Source;
    Res.Last := 1;
    return Res;
  end To_Unbounded_Array;

  function To_Unbounded_Array (Source : Element_Array) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    if Source'Length = 0 then
      Res := Null_Unbounded_Array;
    else
      Res.Ref := new Element_Array(1 .. Source'Length);
      Res.Ref.all := Source;
      Res.Last := Source'Length;
    end if;
    return Res;
  end To_Unbounded_Array;

  function To_Array (Source : Unbounded_Array) return Element_Array is
  begin
    return Source.Ref(1 .. Source.Last);
  end To_Array;

  procedure Set (Target : out Unbounded_Array; Val : in Unbounded_Array) is
  begin
    if Val.Last = 0 then
      Target := Null_Unbounded_Array;
    else
      Target.Ref := new Element_Array(1 .. Val.Last);
      Target.Ref.all := Val.Ref(1 .. Val.Last);
      Target.Last := Val.Last;
    end if;
  end Set;
  procedure Set (Target : out Unbounded_Array; Val : in Element_Array) is
  begin
    if Val'Length = 0 then
      Target := Null_Unbounded_Array;
    else
      Target.Ref := new Element_Array(1 .. Val'Length);
      Target.Ref.all := Val;
      Target.Last := Val'Length;
    end if;
  end Set;
  procedure Set (Target : out Unbounded_Array; Val : in Element_Type) is
  begin
    Target.Ref := new Element_Array(1 .. 1);
    Target.Ref(1) := Val;
    Target.Last := 1;
  end Set;

  -- Slice
  function Slice (Source : Unbounded_Array;
                  Low    : Positive;
                  High   : Natural) return Element_Array is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Source.Ref.all(Low .. High);
  end Slice;

  function Unbounded_Slice (Source : Unbounded_Array;
                            Low    : Positive;
                            High   : Natural) return Unbounded_Array is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return To_Unbounded_Array (Source.Ref.all(Low .. High));
  end Unbounded_Slice;

  -- Prepend
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Unbounded_Array) is

  begin
    Store (Source, New_Item.Ref.all & Source.Ref(1 .. Source.Last));
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Array) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Type) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;

  -- Append
  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Unbounded_Array) is

  begin
    if New_Item.Last <= Source.Ref'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Ref(Source.Last + 1 .. Source.Last +  New_Item.Last) :=
          New_Item.Ref(1 .. New_Item.Last);
      Source.Last := Source.Last + New_Item.Last;
    else
      Store (Source, Source.Ref(1 .. Source.Last)
                   & New_Item.Ref(1 .. New_Item.Last));
    end if;
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Array) is
  begin
    if New_Item'Length <= Source.Ref'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Ref(Source.Last + 1 .. Source.Last +  New_Item'Length) :=
          New_Item;
      Source.Last := Source.Last + New_Item'Length;
    else
      Store (Source, Source.Ref(1 .. Source.Last) & New_Item);
    end if;
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Type) is
  begin
    if 1 <= Source.Ref'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Ref(Source.Last + 1) := New_Item;
      Source.Last := Source.Last + 1;
    else
      Store (Source, Source.Ref.all & New_Item);
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

  -- Element
  function Element (Source : Unbounded_Array;
                    Index  : Positive) return Element_Type is
  begin
    Check_Index (Index, Source.Last, False);
    return Source.Ref.all(Index);
  end Element;

  procedure Replace_Element (Source : in out Unbounded_Array;
                             Index  : in Positive;
                             By     : in Element_Type) is
  begin
    Check_Index (Index, Source.Last, False);
    Source.Ref.all(Index) := By;
  end Replace_Element;

  -- Comparisons
  function "=" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) =
           Right.Ref.all(1 .. Right.Last);
  end "=";
  function "=" (Left  : Unbounded_Array;
                Right : Element_Array) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) = Right;
  end "=";
  function "=" (Left  : Element_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left = Right.Ref.all(1 .. Right.Last);
  end "=";

  --Locate
  function Locate (Within     : Unbounded_Array;
                   Fragment   : Element_Array;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural is
    Index : Natural;
    Found_Occurence : Natural := 0;
  begin
    -- Fix Index
    Index := (if From_Index = 0 then
                (if Forward then 1 else Within.Last)
              else From_Index);

    -- Handle limit or incorrect values
    if Within.Last = 0
    or else Fragment'Length = 0
    or else Index > Within.Last then
      return 0;
    end if;
    if Forward then
      for I in Index .. Within.Last - Fragment'Length + 1 loop
        if Within.Ref(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    else
      for I in reverse 1 .. Index - Fragment'Length + 1 loop
        if Within.Ref(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    end if;
    return 0;
  exception
    when Constraint_Error =>
      return 0;
  end Locate;

  function Count (Source  : Unbounded_Array;
                  Pattern : Element_Array) return Natural is
    Result : Natural := 0;
  begin
    for I in 1 .. Source.Last - Pattern'Length + 1 loop
      if Source.Ref(I .. I + Pattern'Length - 1) = Pattern then
        Result := Result + 1;
      end if;
    end loop;
    return Result;
  end Count;

  -- Overwrite
  procedure Overwrite (Source   : in out Unbounded_Array;
                       Position : in Positive;
                       New_Item : in Unbounded_Array) is
    -- Index in New_Item of last overwritting char (others are appended)
    Lo : Natural;
  begin
    Check_Index (Position, Source.Last, True);
    Lo := (if Position + New_Item.Last - 1 > Source.Last then
             Source.Last - Position + 1
           else New_Item.Last);
    -- Overwrite by Lo chars from Position
    Source.Ref(Position .. Position + Lo - 1) := New_Item.Ref(1 ..  Lo);
    -- Append others
    Append (Source, New_Item.Ref(Lo + 1 .. New_Item.Last));
  end Overwrite;

  procedure Overwrite (Source   : in out Unbounded_Array;
                       Position : in Positive;
                       New_Item : in Element_Array) is
    -- Index in New_Item of last overwritting char (others are appended)
    Lo : Natural;
  begin
    Check_Index (Position, Source.Last, True);
    Lo := (if Position + New_Item'Length - 1 > Source.Last then
             New_Item'First + Source.Last - Position
           else New_Item'Last);
    -- Overwrite by Lo chars from Position
    Source.Ref(Position .. Position + Lo - New_Item'First) :=
        New_Item(New_Item'First ..  Lo);
    -- Append others
    Append (Source, New_Item(Lo + 1 .. New_Item'Last));
  end Overwrite;

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
    Store (Source, Source.Ref(1 .. Low - 1)
                 & By.Ref(1 .. By.Last)
                 & Source.Ref(Start_Tail .. Source.Last));
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
    Store (Source, Source.Ref(1 .. Low - 1)
                 & By
                 & Source.Ref(Start_Tail .. Source.Last));
  end Replace;

  -- Insert
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Unbounded_Array) is
  begin
    Check_Index (Before, Source.Last, True);
    Store (Source, Source.Ref(1 .. Before - 1)
                 & New_Item.Ref(1 .. New_Item.Last)
                 & Source.Ref(Before .. Source.Last));
  end Insert;

  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Element_Array) is
  begin
    Check_Index (Before, Source.Last, True);
    Store (Source, Source.Ref(1 .. Before - 1)
                 & New_Item
                 & Source.Ref(Before .. Source.Last));
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
      Free(Source.Ref);
      Source := Null_Unbounded_Array;
    else
      Source.Ref(1 .. New_Len) :=
             Source.Ref(1 .. From - 1)
           & Source.Ref(Through + 1 .. Source.Last);
      Source.Last := New_Len;
    end if;
  end Delete;

  -- Delete Number elements from From included
  --  or as many elements as possible
  -- May raise Index_Error if From > Source.Length
  procedure Delete_Nb (Source : in out Unbounded_Array;
                       From   : in Positive;
                       Number : in Natural) is
  begin
    if From + Number - 1 <= Source.Last then
      -- We can delete Number characters
      Delete (Source, From, From + Number - 1);
    else
      -- We delete from From to Last
      Delete (Source, From, Source.Last);
    end if;
  end Delete_Nb;

  -- Delete trailing elements
  -- Delete elements from Source.Length - Number + 1 to Source.Length
  -- Source becomes Asu_Null if Number >= Source.Length
  procedure Trail (Source : in out Unbounded_Array;
                   Number : in Natural) is
  begin
    if Number >= Source.Last then
      Set_Null (Source);
    else
      Source.Last := Source.Last - Number;
    end if;
  end Trail;

  -- Extract Count elements from head or tail of Source
  -- Pad with Pad if Count > Source.Length
  function Head (Source : Unbounded_Array;
                 Count  : Natural;
                 Pad    : Element_Type) return Unbounded_Array is
    Result : Unbounded_Array;
  begin
    Init (Result, Count);
    if Count <= Source.Last then
      Result.Ref.all := Source.Ref(1 .. Count);
    else
      Result.Ref(1 .. Source.Last) := Source.Ref(1 .. Source.Last);
      for I in Source.Last + 1 .. Count loop
        Result.Ref(I) := Pad;
      end loop;
    end if;
    return Result;
  end Head;

  function Tail (Source : Unbounded_Array;
                 Count  : Natural;
                 Pad    : Element_Type) return Unbounded_Array is
    Result : Unbounded_Array;
  begin
    Init (Result, Count);
    if Count <= Source.Last then
      Result.Ref.all := Source.Ref(Source.Last - Count + 1 .. Source.Last);
    else
      for I in 1 .. Count - Source.Last loop
        Result.Ref(I) := Pad;
      end loop;
      Result.Ref(Count - Source.Last + 1 .. Count) := Source.Ref.all;
    end if;
    return Result;
  end Tail;

  -- Initialize from a repeated pattern
  function "*" (Left : Natural; Right : Element_Type) return Unbounded_Array is
    Result : Unbounded_Array;
  begin
    Init (Result, Left);
    for I in 1 .. Left loop
      Result.Ref(I) := Right;
    end loop;
    return Result;
  end "*";

  function "*" (Left : Natural; Right : Element_Array) return Unbounded_Array is
    Result : Unbounded_Array;
    Ptr    : Integer := 1;
  begin
     Init (Result, Left * Right'Length);
     for I in 1 .. Left loop
       Result.Ref(Ptr .. Ptr + Right'Length - 1) := Right;
       Ptr := Ptr + Right'Length;
     end loop;
     return Result;
  end "*";

  function "*" (Left : Natural; Right : Unbounded_Array)
               return Unbounded_Array is
    Result : Unbounded_Array;
    Ptr    : Integer := 1;
  begin
     Init (Result, Left * Right.Last);
     for I in 1 .. Left loop
       Result.Ref(Ptr .. Ptr + Right.Last - 1) := Right.Ref(1 .. Right.Last);
       Ptr := Ptr + Right.Last;
     end loop;
     return Result;
  end "*";

  -- Life cycle
  overriding procedure Initialize (Object : in out Unbounded_Array) is
  begin
    Object.Ref := Null_Array'Access;
    Object.Last := 0;
  end Initialize;
  overriding procedure Adjust (Object : in out Unbounded_Array) is
  begin
    if Object.Ref /= Null_Array'Access then
      -- Real copy
      Object.Ref := new Element_Array'(
                        Object.Ref(1 .. Object.Last));
    end if;
  end Adjust;
  overriding procedure Finalize (Object : in out Unbounded_Array) is
  begin
    Free (Object.Ref);
    Object.Ref := Null_Array'Access;
    Object.Last := 0;
  end Finalize;

end Unbounded_Arrays;

