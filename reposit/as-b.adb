package body As.B is

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

  -- Store Item in Bounded array, raise Length_Error if Item too long
  procedure Store (Within : in out Asb_Bs;
                   Item  : in String) is
  begin
    if Item'Length > Within.Max then
      raise Length_Error;
    end if;
    Within.Ref(1 .. Item'Length) := Item;
    Within.Last := Item'Length;
  end Store;

  -- Move the slice Last .. Source.Last at Before
  procedure Move (Source : in out Asb_Bs; Before, Last : in Positive) is
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

  function Asb_Null (Max : in Natural := 0) return Asb_Bs is
    Res : Asb_Bs(Max);
  begin
    return Res;
  end Asb_Null;

  procedure Set_Null (Target : in out Asb_Bs) is
  begin
    Target.Last := 0;
  end Set_Null;

  function Is_Null (Source : Asb_Bs) return Boolean is
  begin
    return Source.Last = 0;
  end Is_Null;

  function Length (Source : Asb_Bs) return Natural is
  begin
    return Source.Last;
  end Length;

  function Tbs (Str : String) return Asb_Bs is
    Res : Asb_Bs(Str'Length);
  begin
    Res.Ref := Str;
    Res.Last := Str'Length;
    return Res;
  end Tbs;
  function Tbs (Char : Character) return Asb_Bs is
    Res : Asb_Bs(1);
  begin
    Res.Ref(1) := Char;
    Res.Last := 1;
    return Res;
  end Tbs;

  function Image (Str : Asb_Bs) return String is
  begin
    return Str.Ref (1 .. Str.Last);
  end Image;

  procedure Set (Target : out Asb_Bs; Val : in Asb_Bs) is
  begin
    if Val.Last > Target.Max then
      raise Length_Error;
    else
      Target.Ref(1 .. Val.Last) := Val.Ref(1 .. Val.Last);
      Target.Last := Val.Last;
    end if;
  end Set;
  procedure Set (Target : out Asb_Bs; Val : in String) is
  begin
    if Val'Length > Target.Max then
      raise Length_Error;
    else
      Target.Ref(1 .. Val'Length) := Val;
      Target.Last := Val'Length;
    end if;
  end Set;
  procedure Set (Target : out Asb_Bs; Val : in Character) is
  begin
    if 1 > Target.Max then
      raise Length_Error;
    else
      Target.Ref(1) := Val;
      Target.Last := 1;
    end if;
  end Set;

  function Bslice (Source : Asb_Bs;
                   Low : Positive; High : Natural) return Asb_Bs is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Tbs (Source.Ref(Low .. High));
  end Bslice;
  procedure Bslice (Source : in Asb_Bs; Target : out Asb_Bs;
                    Low : in Positive; High : in Natural) is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    Set (Target, Source.Ref(Low .. High));
  end Bslice;

  function Slice (Source : Asb_Bs;
                  Low : Positive; High : Natural) return String is
  begin
    Check_Indexes (Low, High, Source.Last, True);
    return Source.Ref(Low .. High);
  end Slice;

  procedure Prepend (Source : in out Asb_Bs; New_Item : in Asb_Bs) is
  begin
    Store (Source,
           New_Item.Ref(1 .. New_Item.Last) & Source.Ref(1 .. Source.Last));
  end Prepend;
  procedure Prepend (Source : in out Asb_Bs; New_Item : in String) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;
  procedure Prepend (Source : in out Asb_Bs; New_Item : in Character) is
  begin
    Store (Source, New_Item & Source.Ref(1 .. Source.Last));
  end Prepend;

  procedure Append (Source : in out Asb_Bs; New_Item : in Asb_Bs) is
  begin
    if New_Item.Last > Source.Max - Source.Last then
      raise Length_Error;
    end if;
    Source.Ref(Source.Last + 1 .. Source.Last +  New_Item.Last) :=
        New_Item.Ref(1 .. New_Item.Last);
    Source.Last := Source.Last + New_Item.Last;
  end Append;
  procedure Append (Source : in out Asb_Bs; New_Item : in String) is
  begin
    if New_Item'Length > Source.Max - Source.Last then
      raise Length_Error;
    end if;
    Source.Ref(Source.Last + 1 .. Source.Last +  New_Item'Length) := New_Item;
    Source.Last := Source.Last + New_Item'Length;
  end Append;
  procedure Append (Source : in out Asb_Bs; New_Item : in Character) is
  begin
    if 1 > Source.Max - Source.Last then
      raise Length_Error;
    end if;
    Source.Ref(Source.Last + 1) := New_Item;
    Source.Last := Source.Last + 1;
  end Append;

  function "&" (Left, Right : Asb_Bs) return Asb_Bs is
    Res : Asb_Bs(Left.Last + Right.Last);
  begin
    Set (Res, Left);
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : Asb_Bs; Right : String) return Asb_Bs is
    Res : Asb_Bs(Left.Last + Right'Length);
  begin
    Set (Res, Left);
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : String; Right : Asb_Bs) return Asb_Bs is
    Res : Asb_Bs(Left'Length + Right.Last);
  begin
    Set (Res, Right);
    Prepend (Res, Left);
    return Res;
  end "&";
  function "&" (Left : Asb_Bs; Right : Character) return Asb_Bs is
    Res : Asb_Bs(Left.Last + 1);
  begin
    Set (Res, Left);
    Append (Res, Right);
    return Res;
  end "&";
  function "&" (Left : Character; Right : Asb_Bs) return Asb_Bs is
    Res : Asb_Bs(1 + Right.Last);
  begin
    Set (Res, Right);
    Prepend (Res, Left);
    return Res;
  end "&";

  function Element (Source : Asb_Bs; Index : Positive) return Character is
  begin
    Check_Index (Index, Source.Last, False);
    return Source.Ref(Index);
  end Element;

  procedure Replace_Element (Source : in out Asb_Bs;
                             Index  : in Positive;
                             By     : in Character) is
  begin
    Check_Index (Index, Source.Last, False);
    Source.Ref(Index) := By;
  end Replace_Element;

  function "="  (Left, Right : Asb_Bs) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) = Right.Ref(1 .. Right.Last);
  end "=";
  function "="  (Left : Asb_Bs; Right : String) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) = Right;
  end "=";
  function "="  (Left : String; Right : Asb_Bs) return Boolean is
  begin
    return Left = Right.Ref(1 .. Right.Last);
  end "=";

  function "<"  (Left, Right : Asb_Bs) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) < Right.Ref(1 .. Right.Last);
  end "<";
  function "<"  (Left : Asb_Bs; Right : String) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) < Right;
  end "<";
  function "<"  (Left : String; Right : Asb_Bs) return Boolean is
  begin
    return Left < Right.Ref(1 .. Right.Last);
  end "<";

  function "<=" (Left, Right : Asb_Bs) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) <= Right.Ref(1 .. Right.Last);
  end "<=";
  function "<=" (Left : Asb_Bs; Right : String) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) <= Right;
  end "<=";
  function "<=" (Left : String; Right : Asb_Bs) return Boolean is
  begin
    return Left <= Right.Ref(1 .. Right.Last);
  end "<=";

  function ">"  (Left, Right : Asb_Bs) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) > Right.Ref(1 .. Right.Last);
  end ">";
  function ">"  (Left : Asb_Bs; Right : String) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) > Right;
  end ">";
  function ">"  (Left : String; Right : Asb_Bs) return Boolean is
  begin
    return Left > Right.Ref(1 .. Right.Last);
  end ">";

  function ">=" (Left, Right : Asb_Bs) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) >= Right.Ref(1 .. Right.Last);
  end ">=";
  function ">=" (Left : Asb_Bs; Right : String) return Boolean is
  begin
    return Left.Ref(1 .. Left.Last) >= Right;
  end ">=";
  function ">=" (Left : String; Right : Asb_Bs) return Boolean is
  begin
    return Left >= Right.Ref(1 .. Right.Last);
  end ">=";

  function Locate (Within     : Asb_Bs;
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

  function Count (Source   : Asb_Bs;
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

  procedure Overwrite (Source   : in out Asb_Bs;
                       Position   : in Positive;
                       New_Item : in Asb_Bs) is
   -- Index in New_Item of last overwritting char (others are appended)
   Lo : Natural;
  begin
    Check_Index (Position, Source.Last, True);
    if Position + New_Item.Last - 1 > Source.Last then
      Lo := Source.Last - Position + 1;
    else
      Lo := New_Item.Last;
    end if;
    -- Overwrite by Lo chars from Position
    Source.Ref(Position .. Position + Lo - 1) := New_Item.Ref(1 ..  Lo);
    -- Append others
    Append (Source, New_Item.Ref(Lo + 1 .. New_Item.Last));
  end Overwrite;

  procedure Overwrite (Source   : in out Asb_Bs;
                       Position   : in Positive;
                       New_Item : in String) is
    -- Index in New_Item of last overwritting char (others are appended)
    Lo : Natural;
  begin
    Check_Index (Position, Source.Last, True);
    if Position + New_Item'Length - 1 > Source.Last then
      Lo := New_Item'First + Source.Last - Position;
    else
      Lo := New_Item'Last;
    end if;
    -- Overwrite by Lo chars from Position
    Source.Ref(Position .. Position + Lo - 1) :=
        New_Item(New_Item'First ..  Lo);
    -- Append others
    Append (Source, New_Item(Lo + 1 .. New_Item'Last));
  end Overwrite;

  procedure Replace (Source   : in out Asb_Bs;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Asb_Bs) is
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

  procedure Replace (Source   : in out Asb_Bs;
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

  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in Asb_Bs) is
    Last : constant Natural := Source.Last;
  begin
    if Before > Source.Last + 1 then
      raise Index_Error;
    end if;
    Append (Source, New_Item);
    Move (Source, Before, Last + 1);
  end Insert;

  procedure Insert (Source   : in out Asb_Bs;
                    Before   : in Positive;
                    New_Item : in String) is
    Last : constant Natural := Source.Last;
  begin
    if Before > Source.Last + 1 then
      raise Index_Error;
    end if;
    Append (Source, New_Item);
    Move (Source, Before, Last + 1);
  end Insert;

  procedure Delete (Source  : in out Asb_Bs;
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
      Source.Last := 0;
    else
      Source.Ref(1 .. New_Len) :=
             Source.Ref(1 .. From - 1)
           & Source.Ref(Through + 1 .. Source.Last);
      Source.Last := New_Len;
    end if;
  end Delete;

  procedure Trail (Source : in out Asb_Bs;
                   Number : in Positive) is
  begin
    if Number >= Source.Last then
      Set_Null (Source);
    else
      Source.Last := Source.Last - Number;
    end if;
  end Trail;

  function Head (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs is
    Result : Asb_Bs(Count);
  begin
    if Count <= Source.Last then
      Result.Ref := Source.Ref(1 .. Count);
    else
      Result.Ref(1 .. Source.Last) := Source.Ref(1 .. Source.Last);
      for I in Source.Last + 1 .. Count loop
        Result.Ref(I) := Pad;
      end loop;
    end if;
    return Result;
  end Head;

  function Tail (Source : Asb_Bs; Count : Natural; Pad : Character := Space)
          return Asb_Bs is
    Result : Asb_Bs(Count);
  begin
    if Count <= Source.Last then
      Result.Ref := Source.Ref(Source.Last - Count - 1 .. Source.Last);
    else
      for I in 1 .. Count - Source.Last loop
        Result.Ref(I) := Pad;
      end loop;
      Result.Ref(Count - Source.Last + 1 .. Count) := Source.Ref;
    end if;
    return Result;
  end Tail;

  function "*" (Left  : Natural; Right : Character) return Asb_Bs is
    Result : Asb_Bs(Left);
  begin
    for I in 1 .. Left loop
      Result.Ref(I) := Right;
    end loop;
    return Result;
  end "*";

  function "*" (Left  : Natural; Right : String) return Asb_Bs is
    Result : Asb_Bs(Left * Right'Length);
    Ptr    : Integer := 1;
  begin
     for I in 1 .. Left loop
       Result.Ref(Ptr .. Ptr + Right'Length - 1) := Right;
       Ptr := Ptr + Right'Length;
     end loop;
     return Result;
  end "*";

  function "*" (Left  : Natural; Right : Asb_Bs) return Asb_Bs is
    Result : Asb_Bs(Right.Last);
    Ptr    : Integer := 1;
  begin
     for I in 1 .. Left loop
       Result.Ref(Ptr .. Ptr + Right.Last - 1) := Right.Ref(1 .. Right.Last);
       Ptr := Ptr + Right.Last;
     end loop;
     return Result;
  end "*";

end As.B;

