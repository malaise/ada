with Dynamic_List, Basic_Proc, Queues, Images;
separate (Mcd_Mng)

package body Stack is

  package Stack_Dyn_List is new Dynamic_List (Item_Rec);
  package Stack_List renames Stack_Dyn_List.Dyn_List;
  List, Extra_List : Stack_List.List_Type;

  -- Hitory of Last 3 popped items
  package History_Mng is new Queues.Circ (3, Item_Rec);
  History : History_Mng.Circ_Type;

  procedure Push (Item : in Item_Rec; Default_Stack : in Boolean := True) is
  begin
    if Item.Kind not in Operand_Kind_List then
      Debug.Log (Debug.Stack, Item,
            (if not Default_Stack then "Extra " else "") & "ERROR Pushing");
      raise Invalid_Argument;
    end if;
    Debug.Log (Debug.Stack, Item,
            (if not Default_Stack then "Extra " else "") & "Pushing");
    if Default_Stack then
      List.Insert(Item);
    else
      Extra_List.Insert(Item);
    end if;
  end Push;

  procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    Debug.Log (Debug.Stack,
            (if not Default_Stack then "Extra " else "") & "Poping");
    if Default_Stack then
      List.Get(Litem, Stack_List.Prev);
      History.Push (Litem);
    else
      Extra_List.Get(Litem, Stack_List.Prev);
    end if;
    Item := Litem;
    Debug.Log (Debug.Stack, Item);
  exception
    when Stack_List.Empty_List =>
      Debug.Log (Debug.Stack, "Raises EMPTY_STACK");
      raise Empty_Stack;
  end Pop;

  procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    Debug.Log (Debug.Stack,
            (if not Default_Stack then "Extra " else "") & "Reading");
    if Default_Stack then
      List.Read(Litem, Stack_List.Current);
      History.Push (Litem);
    else
      Extra_List.Read(Litem, Stack_List.Current);
    end if;
    Item := Litem;
    Debug.Log (Debug.Stack, Item);
  exception
    when Stack_List.Empty_List =>
      Debug.Log (Debug.Stack, "Raises EMPTY_STACK");
      raise Empty_Stack;
  end Read;


  procedure Readn (Item : out Item_Rec; N : in Positive) is
    Litem : Item_Rec;
  begin
    Debug.Log (Debug.Stack, "Reading " & Images.Integer_Image (N) & "th ");
    List.Move_At (N, Stack_List.Prev);
    List.Read(Litem, Stack_List.Current);
    History.Push (Litem);
    Item := Litem;
    List.Move_At (1, Stack_List.Prev);
    Debug.Log (Debug.Stack, Item);
  exception
    when Stack_List.Not_In_List =>
      Debug.Log (Debug.Stack, "Raises EMPTY_STACK");
      raise Empty_Stack;
  end Readn;

  procedure Getn (Item : out Item_Rec; N : in Positive) is
    Litem : Item_Rec;
    Moved : Boolean;
  begin
    Debug.Log (Debug.Stack, "Getting " & Images.Integer_Image (N) & "th ");
    List.Move_At (N, Stack_List.Prev);
    List.Get(Litem, Stack_List.Next, Moved);
    History.Push (Litem);
    Item := Litem;
    List.Move_At (1, Stack_List.Prev);
    Debug.Log (Debug.Stack, Item);
  exception
    when Stack_List.Not_In_List =>
      Debug.Log (Debug.Stack, "Raises EMPTY_STACK");
      raise Empty_Stack;
  end Getn;


  function Stack_Size (Default_Stack : Boolean := True) return Natural is
    Size : Natural;
  begin
    if Default_Stack then
      Size := List.List_Length;
    else
      Size := Extra_List.List_Length;
    end if;
    Debug.Log (Debug.Stack,
          (if not Default_Stack then "Extra " else "")
        & "Size " & Natural'Image(Size));
    return Size;
  end Stack_Size;

  procedure Popfe (Item : out Item_Rec) is
    Litem : Item_Rec;
  begin
    Debug.Log (Debug.Stack, "Extra Poping first");
    -- Get first pushed item
    Extra_List.Rewind;
    Extra_List.Get(Litem, Stack_List.Next);
    Item := Litem;
    Debug.Log (Debug.Stack, Item);
    -- Move back to last pushed item
    Extra_List.Rewind (False, Stack_List.Prev);
    Debug.Log (Debug.Stack, Item);
  exception
    when Stack_List.Empty_List =>
      Debug.Log (Debug.Stack, "Raises EMPTY_STACK");
      raise Empty_Stack;
  end Popfe;

  procedure Pushfe (Item : in Item_Rec) is
  begin
     Debug.Log (Debug.Stack, Item, "Extra pushing at first");
    if Extra_List.Is_Empty then
      Extra_List.Insert(Item);
      return;
    end if;

    -- Insert before first
    Extra_List.Rewind;
    Extra_List.Insert (Item, Stack_List.Prev);

    -- Move back to last pushed item
    Extra_List.Rewind (True, Stack_List.Prev);
  end Pushfe;

  -- Dump last items popped or read
  procedure Dump_History is
    Litem, Sitem : Item_Rec;
    Len : constant Natural := History.Length;
    Text : As.U.Asu_Us;
  begin
    if not Debug.Loggers(Debug.History).Debug_On or else Len = 0 then
      return;
    end if;
    Text.Set ("Last Stack (bottom to top): ");
    for I in 1 .. Len loop
      History.Look_Last (Litem);
      History.Discard_Last;
      Sitem := Ios.Strof (Litem);
      if Litem.Kind = Chrs then
        Text.Append ('"' & Sitem.Val_Text.Image &'"');
      elsif Litem.Kind = Prog then
        Text.Append ("[ " & Sitem.Val_Text.Image & " ]");
      else
        Text.Append (Sitem.Val_Text.Image);
      end if;
      if I /= Len then
        Text.Append (" ");
      end if;
    end loop;
    Debug.Log (Debug.History, Text.Image);
  end Dump_History;

  procedure Clear_History is
  begin
    History.Clear;
  end Clear_History;

end Stack;

