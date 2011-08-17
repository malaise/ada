with Ada.Unchecked_Deallocation;
package body As.U is

  -- Each time a re-allocation is needed, increment Length by
  --   Nb_To_Add + Growth_Offset + Curr_Length / Growth_Factor
  -- so that some further growths will not lead to re-alloc
  Growth_Factor : constant Natural := 64;
  Growth_Offset : constant Natural := 32;

  procedure Free (X : in out String_Access) is
    procedure Deallocate is
       new Ada.Unchecked_Deallocation (String, String_Access);
  begin
    --  Do not try to free statically allocated null array
    if X /= Asu_Null.Ref then
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
  procedure Store (Within : in out Asu_Us;
                   Item  : in String) is
    Incr : Integer;
    Acc : String_Access;
  begin
    Incr := Item'Length - Within.Ref'Length;
    if Incr <= 0 then
      if Item'Length = 0 then
        -- Item to store is empty
        Free (Within.Ref);
        Within.Ref := Null_String'Access;
      else
        -- Item to store fits in Reference, no need to re-alloc
        Within.Ref.all(1 .. Item'Length) := Item;
      end if;
    else
      -- Re-alloc reference to new size
      Acc := new String(1 .. New_Size (Within.Ref'Length, Incr));
      Acc(1 .. Item'Length) := Item;
      Free (Within.Ref);
      Within.Ref := Acc;
    end if;
    Within.Last := Item'Length;
  end Store;

  procedure Init (Target : in out Asu_Us; Length : Natural) is
  begin
    Free (Target.Ref);
    if Length = 0 then
      Target := Asu_Null;
    else
      Target.Ref := new String(1 .. Length);
      Target.Ref.all := (others => ' ');
      Target.Last := Length;
    end if;
  end Init;

  -- Move the slice Last .. Source.Last at Before
  procedure Move (Source : in out Asu_Us; Before, Last : in Positive) is
    Tmp : constant String := Source.Ref (Last .. Source.Last);
  begin
    -- Shift head from Before to Last - 1 at the tail
    Source.Ref(Source.Last - Last + Before + 1 .. Source.Last) :=
        Source.Ref(Before .. Last - 1);
    -- Copy Tmp at Before
    Source.Ref(Before .. Before + Tmp'Length - 1) := Tmp;
  end Move;

  -----------------------
  -- PUBLIC operations --
  -----------------------
  procedure Set_Null (Target : in out Asu_Us) is
  begin
    -- Optim: avoid copying Asu_Null
    Free (Target.Ref);
    Target.Ref := Null_String'Access;
    Target.Last := 0;
  end Set_Null;

  function Is_Null (Source : Asu_Us) return Boolean is
  begin
    return Source = Asu_Null;
  end Is_Null;

  function Length (Source : Asu_Us) return Natural is
  begin
    return Source.Last;
  end Length;

  function Tus (Str : String) return Asu_Us is
    Res : Asu_Us;
  begin
    if Str'Length = 0 then
      Res := Asu_Null;
    else
      Res.Ref := new String(1 .. Str'Length);
      Res.Ref.all := Str;
      Res.Last := Str'Length;
    end if;
    return Res;
  end Tus;
  function Tus (Char : Character) return Asu_Us is
    Res : Asu_Us;
  begin
    Res.Ref := new String(1 .. 1);
    Res.Ref(1) := Char;
    Res.Last := 1;
    return Res;
  end Tus;

  function Image (Str : Asu_Us) return String is
  begin
    return Str.Ref (1 .. Str.Last);
  end Image;

  procedure Set (Target : out Asu_Us; Val : in Asu_Us) is
  begin
    if Val.Last = 0 then
      Target := Asu_Null;
    else
      Target.Ref := new String(1 .. Val.Last);
      Target.Ref.all := Val.Ref(1 .. Val.Last);
      Target.Last := Val.Last;
    end if;
  end Set;
  procedure Set (Target : out Asu_Us; Val : in String) is
  begin
    if Val'Length = 0 then
      Target := Asu_Null;
    else
      Target.Ref := new String(1 .. Val'Length);
      Target.Ref.all := Val;
      Target.Last := Val'Length;
    end if;
  end Set;
  procedure Set (Target : out Asu_Us; Val : in Character) is
  begin
    Target.Ref := new String(1 .. 1);
    Target.Ref(1) := Val;
    Target.Last := 1;
  end Set;

  function Uslice (Source : Asu_Us;
                   Low : Positive; High : Natural) return Asu_Us is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Tus (Source.Ref.all(Low .. High));
  end Uslice;
  procedure Uslice (Source : in Asu_Us; Target : out Asu_Us;
                    Low : in Positive; High : in Natural) is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    Set (Target, Source.Ref.all(Low .. High));
  end Uslice;

  function Slice (Source : Asu_Us;
                  Low : Positive; High : Natural) return String is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Source.Ref.all(Low .. High);
  end Slice;

  procedure Prepend (Source : in out Asu_Us; New_Item : in Asu_Us) is
  begin
    Store (Source, New_Item.Ref.all & Source.Ref(1 .. Source.Last));
  end Prepend;
  procedure Prepend (Source : in out Asu_Us; New_Item : in String) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;
  procedure Prepend (Source : in out Asu_Us; New_Item : in Character) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;

  procedure Append (Source : in out Asu_Us; New_Item : in Asu_Us) is

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
  procedure Append (Source : in out Asu_Us; New_Item : in String) is
  begin
    if New_Item'Length <= Source.Ref'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Ref(Source.Last + 1 .. Source.Last +  New_Item'Length) := New_Item;
      Source.Last := Source.Last + New_Item'Length;
    else
      Store (Source, Source.Ref(1 .. Source.Last) & New_Item);
    end if;
  end Append;
  procedure Append (Source : in out Asu_Us; New_Item : in Character) is
  begin
    if 1 <= Source.Ref'Length - Source.Last then
      -- Optim: no copy if New_Item fits
      Source.Ref(Source.Last + 1) := New_Item;
      Source.Last := Source.Last + 1;
    else
      Store (Source, Source.Ref.all & New_Item);
    end if;
  end Append;

  function "&" (Left, Right : Asu_Us) return Asu_Us is
    Res : Asu_Us := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : Asu_Us; Right : String) return Asu_Us is
    Res : Asu_Us := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : String; Right : Asu_Us) return Asu_Us is
    Res : Asu_Us := Tus (Left);
  begin
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : Asu_Us; Right : Character) return Asu_Us is
    Res : Asu_Us := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : Character; Right : Asu_Us) return Asu_Us is
    Res : Asu_Us := Tus (Left);
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function Element (Source : Asu_Us; Index : Positive) return Character is
  begin
    Check_Index (Index, Source.Last, False);
    return Source.Ref.all(Index);
  end Element;

  procedure Replace_Element (Source : in out Asu_Us;
                             Index  : in Positive;
                             By     : in Character) is
  begin
    Check_Index (Index, Source.Last, False);
    Source.Ref.all(Index) := By;
  end Replace_Element;

  function "="  (Left, Right : Asu_Us) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) = Right.Ref.all(1 .. Right.Last);
  end "=";
  function "="  (Left : Asu_Us; Right : String) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) = Right;
  end "=";
  function "="  (Left : String; Right : Asu_Us) return Boolean is
  begin
    return Left = Right.Ref.all(1 .. Right.Last);
  end "=";

  function "<"  (Left, Right : Asu_Us) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) < Right.Ref.all(1 .. Right.Last);
  end "<";
  function "<"  (Left : Asu_Us; Right : String) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) < Right;
  end "<";
  function "<"  (Left : String; Right : Asu_Us) return Boolean is
  begin
    return Left < Right.Ref.all(1 .. Right.Last);
  end "<";

  function "<=" (Left, Right : Asu_Us) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) <= Right.Ref.all(1 .. Right.Last);
  end "<=";
  function "<=" (Left : Asu_Us; Right : String) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) <= Right;
  end "<=";
  function "<=" (Left : String; Right : Asu_Us) return Boolean is
  begin
    return Left <= Right.Ref.all(1 .. Right.Last);
  end "<=";

  function ">"  (Left, Right : Asu_Us) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) > Right.Ref.all(1 .. Right.Last);
  end ">";
  function ">"  (Left : Asu_Us; Right : String) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) > Right;
  end ">";
  function ">"  (Left : String; Right : Asu_Us) return Boolean is
  begin
    return Left > Right.Ref.all(1 .. Right.Last);
  end ">";

  function ">=" (Left, Right : Asu_Us) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) >= Right.Ref.all(1 .. Right.Last);
  end ">=";
  function ">=" (Left : Asu_Us; Right : String) return Boolean is
  begin
    return Left.Ref.all(1 .. Left.Last) >= Right;
  end ">=";
  function ">=" (Left : String; Right : Asu_Us) return Boolean is
  begin
    return Left >= Right.Ref.all(1 .. Right.Last);
  end ">=";

  function Locate (Within     : Asu_Us;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1) return Natural is
    Index : Natural;
    Found_Occurence : Natural := 0;
  begin
    -- Fix Index
    if From_Index = 0 then
      if Forward then
        Index := 1;
      else
        Index := Within.Last;
      end if;
    else
      Index := From_Index;
    end if;

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

  function Count (Source   : Asu_Us;
                  Pattern : String) return Natural is
    Result : Natural := 0;
  begin
    for I in 1 .. Source.Last - Pattern'Length + 1 loop
      if Source.Ref(I .. I + Pattern'Length - 1) = Pattern then
        Result := Result + 1;
      end if;
    end loop;
    return Result;
  end Count;

procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asu_Us) is
   Start_Tail : Positive;
  begin
    Check_Indexes (Low, High, Source.Last, False);
    if Low <= High then
      -- Replace
      Start_Tail := High + 1;
    else
      -- Insert
      Start_Tail := Low;
    end if;
    Store (Source, Source.Ref(1 .. Low - 1)
                 & By.Ref(1 .. By.Last)
                 & Source.Ref(Start_Tail .. Source.Last));
  end Replace;
  procedure Replace (Source   : in out Asu_Us;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in String) is
   Start_Tail : Positive;
  begin
    Check_Indexes (Low, High, Source.Last, False);
    if Low <= High then
      -- Replace
      Start_Tail := High + 1;
    else
      -- Insert
      Start_Tail := Low;
    end if;
    Store (Source, Source.Ref(1 .. Low - 1)
                 & By
                 & Source.Ref(Start_Tail .. Source.Last));
  end Replace;

  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in Asu_Us) is
    Last : constant Natural := Source.Last;
  begin
    if Before > Source.Last then
      raise Index_Error;
    end if;
    Append (Source, New_Item);
    Move (Source, Before, Last + 1);
  end Insert;

  procedure Insert (Source   : in out Asu_Us;
                    Before   : in Positive;
                    New_Item : in String) is
    Last : constant Natural := Source.Last;
  begin
    if Before > Source.Last then
      raise Index_Error;
    end if;
    Append (Source, New_Item);
    Move (Source, Before, Last + 1);
  end Insert;

  procedure Overwrite (Source   : in out Asu_Us;
                       Position   : in Positive;
                       New_Item : in Asu_Us) is
  begin
    if Position + New_Item.Last > Source.Last then
      raise Index_Error;
    end if;
    Source.Ref(Position .. Position + New_Item.Last - 1) :=
       New_Item.Ref(1 ..  New_Item.Last);
  end Overwrite;

  procedure Overwrite (Source   : in out Asu_Us;
                       Position   : in Positive;
                       New_Item : in String) is
  begin
    if Position + New_Item'Length > Source.Last then
      raise Index_Error;
    end if;
    Source.Ref(Position .. Position + New_Item'Length - 1) := New_Item;
  end Overwrite;

  procedure Delete (Source  : in out Asu_Us;
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
      Source := Asu_Null;
    else
      Source.Ref(1 .. New_Len) :=
             Source.Ref(1 .. From - 1)
           & Source.Ref(Through + 1 .. Source.Last);
      Source.Last := New_Len;
    end if;
  end Delete;

  function Head (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us is
    Result : Asu_Us;
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

  function Tail (Source : Asu_Us; Count : Natural; Pad : Character := Space)
          return Asu_Us is
    Result : Asu_Us;
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

  function "*" (Left  : Natural; Right : Character) return Asu_Us is
    Result : Asu_Us;
  begin
    Init (Result, Left);
    for I in 1 .. Left loop
      Result.Ref(I) := Right;
    end loop;
    return Result;
  end "*";

  function "*" (Left  : Natural; Right : String) return Asu_Us is
    Result : Asu_Us;
    Ptr    : Integer := 1;
  begin
     Init (Result, Left * Right'Length);
     for I in 1 .. Left loop
       Result.Ref(Ptr .. Ptr + Right'Length - 1) := Right;
       Ptr := Ptr + Right'Length;
     end loop;
     return Result;
  end "*";

  function "*" (Left  : Natural; Right : Asu_Us) return Asu_Us is
    Result : Asu_Us;
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
  overriding procedure Initialize (Object : in out Asu_Us) is
  begin
    Object.Ref := Null_String'Access;
    Object.Last := 0;
  end Initialize;
  overriding procedure Adjust (Object : in out Asu_Us) is
  begin
    if Object.Ref /= Null_String'Access then
      -- Real copy
      Object.Ref := new String'(Object.Ref(1 .. Object.Last));
    end if;
  end Adjust;
  overriding procedure Finalize (Object : in out Asu_Us) is
  begin
    Free (Object.Ref);
    Object.Ref := Null_String'Access;
    Object.Last := 0;
  end Finalize;

end As.U;

