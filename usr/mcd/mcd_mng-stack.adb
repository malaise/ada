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
      if Debug.Debug_Level_Array(Debug.Stack) then
        if not Default_Stack then
          Async_Stdin.Put_Err ("Extra ");
        end if;
        Async_Stdin.Put_Err ("Stack: ERROR Pushing ");
        Debug.Put (Item);
        Async_Stdin.New_Line_Err;
      end if;
      raise Invalid_Argument;
    end if;
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Async_Stdin.Put_Err ("Extra ");
      end if;
      Async_Stdin.Put_Err ("Stack: Pushing ");
      Debug.Put (Item);
      Async_Stdin.New_Line_Err;
    end if;
    if Default_Stack then
      List.Insert(Item);
    else
      Extra_List.Insert(Item);
    end if;
  end Push;

  procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Async_Stdin.Put_Err ("Extra ");
      end if;
      Async_Stdin.Put_Err ("Stack: Poping ");
    end if;
    if Default_Stack then
      List.Get(Litem, Stack_List.Prev);
      History.Push (Litem);
    else
      Extra_List.Get(Litem, Stack_List.Prev);
    end if;
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Async_Stdin.New_Line_Err;
    end if;
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Line_Err("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Pop;

  procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Async_Stdin.Put_Err ("Extra ");
      end if;
      Async_Stdin.Put_Err ("Stack: Reading ");
    end if;
    if Default_Stack then
      List.Read(Litem, Stack_List.Current);
      History.Push (Litem);
    else
      Extra_List.Read(Litem, Stack_List.Current);
    end if;
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Async_Stdin.New_Line_Err;
    end if;
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Line_Err("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Read;


  procedure Readn (Item : out Item_Rec; N : in Positive) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      Async_Stdin.Put_Err ("Stack: Reading "
                         & Images.Integer_Image (N) & "th ");
    end if;
    List.Move_At (N, Stack_List.Prev);
    List.Read(Litem, Stack_List.Current);
    History.Push (Litem);
    Item := Litem;
    List.Move_At (1, Stack_List.Prev);
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Async_Stdin.New_Line_Err;
    end if;
  exception
    when Stack_List.Not_In_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Line_Err("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Readn;

  procedure Getn (Item : out Item_Rec; N : in Positive) is
    Litem : Item_Rec;
    Moved : Boolean;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      Async_Stdin.Put_Err ("Stack: Getting "
                         & Images.Integer_Image (N) & "th ");
    end if;
    List.Move_At (N, Stack_List.Prev);
    List.Get(Litem, Stack_List.Next, Moved);
    History.Push (Litem);
    Item := Litem;
    List.Move_At (1, Stack_List.Prev);
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Async_Stdin.New_Line_Err;
    end if;
  exception
    when Stack_List.Not_In_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Line_Err("raises EMPTY_STACK");
      end if;
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
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Async_Stdin.Put_Err ("Extra ");
      end if;
      Async_Stdin.Put_Line_Err ("Stack: size " & Natural'Image(Size));
    end if;
    return Size;
  end Stack_Size;

  procedure Popfe (Item : out Item_Rec) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Err ("Extra ");
      Async_Stdin.Put_Err ("Stack: Poping first ");
    end if;
    -- Get first pushed item
    Extra_List.Rewind;
    Extra_List.Get(Litem, Stack_List.Next);
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Async_Stdin.New_Line_Err;
    end if;
    -- Move back to last pushed item
    Extra_List.Rewind(False, Stack_List.Prev);
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Line_Err("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Popfe;

  procedure Pushfe (Item : in Item_Rec) is
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
        Async_Stdin.Put_Err ("Extra ");
      Async_Stdin.Put_Err ("Stack: pushing at first ");
    end if;
    if Extra_List.Is_Empty then
      Extra_List.Insert(Item);
      return;
    end if;

    -- Insert before first
    Extra_List.Rewind;
    Extra_List.Insert(Item, Stack_List.Prev);

    -- Move back to last pushed item
    Extra_List.Rewind(True, Stack_List.Prev);
  end Pushfe;

  -- Dump last items popped or read
  procedure Dump_History is
    Litem, Sitem : Item_Rec;
    Len : constant Natural := History.Length;
  begin
    if not Debug.Debug_Level_Array(Debug.History)
    or else Len = 0 then
      return;
    end if;
    Basic_Proc.Put_Line_Error ("Last Stack (bottom to top):");
    for I in 1 .. Len loop
      History.Look_Last (Litem);
      History.Discard_Last;
      Sitem := Ios.Strof (Litem);
      if Litem.Kind = Chrs then
        Basic_Proc.Put_Error ('"' & Sitem.Val_Text.Image &'"');
      elsif Litem.Kind = Prog then
        Basic_Proc.Put_Error ("[ " & Sitem.Val_Text.Image & " ]");
      else
        Basic_Proc.Put_Error (Sitem.Val_Text.Image);
      end if;
      if I /= Len then
        Basic_Proc.Put_Error (" ");
      else
        Basic_Proc.New_Line_Error;
      end if;
    end loop;
  end Dump_History;

  procedure Clear_History is
  begin
    History.Clear;
  end Clear_History;

end Stack;

