with Text_Io;
with Dynamic_List;
separate (Mcd_Mng)

package body Stack is 

  package Stack_List is new Dynamic_List (Item_Rec);
  List, Extra_List : Stack_List.List_Type;

  procedure Push (Item : in Item_Rec; Default_Stack : in Boolean := True) is
  begin
    if Item.Kind not in Operand_Kind_List then
      if Debug.Debug_Level_Array(Debug.Stack) then
        if not Default_Stack then
          Text_Io.Put ("Extra ");
        end if;
        Text_Io.Put ("Stack: ERROR Pushing ");
        Debug.Put (Item);
        Text_Io.New_Line;
      end if;
      raise Invalid_Argument;
    end if;
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Text_Io.Put ("Extra ");
      end if;
      Text_Io.Put ("Stack: Pushing ");
      Debug.Put (Item);
      Text_Io.New_Line;
    end if;
    if Default_Stack then
      Stack_List.Insert(List, Item);
    else
      Stack_List.Insert(Extra_List, Item);
    end if;
  end Push;

  procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Text_Io.Put ("Extra ");
      end if;
      Text_Io.Put ("Stack: Poping ");
    end if;
    if Default_Stack then
      Stack_List.Get(List, Litem, Stack_List.Prev);
    else
      Stack_List.Get(Extra_List, Litem, Stack_List.Prev);
    end if;
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Text_Io.New_Line;
    end if;
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Text_Io.Put_Line("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Pop;

  procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Text_Io.Put ("Extra ");
      end if;
      Text_Io.Put ("Stack: Reading ");
    end if;
    if Default_Stack then
      Stack_List.Read(List, Litem, Stack_List.Current);
    else
      Stack_List.Read(Extra_List, Litem, Stack_List.Current);
    end if;
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Text_Io.New_Line;
    end if;
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Text_Io.Put_Line("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Read;

  function Stack_Size (Default_Stack : Boolean := True) return Natural is
    Size : Natural;
  begin
    if Default_Stack then
      Size := Stack_List.List_Length(List);
    else
      Size := Stack_List.List_Length(Extra_List);
    end if;
    if Debug.Debug_Level_Array(Debug.Stack) then
      if not Default_Stack then
        Text_Io.Put ("Extra ");
      end if;
      Text_Io.Put_Line ("Stack: size " & Natural'Image(Size));
    end if;
    return Size;
  end Stack_Size;

  procedure Popf (Item : out Item_Rec) is
    Litem : Item_Rec;
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
        Text_Io.Put ("Extra ");
      Text_Io.Put ("Stack: Poping first ");
    end if;
    -- Get first pushed item
    Stack_List.Move_To(Extra_List, Stack_List.Next, 0, False);
    Stack_List.Get(Extra_List, Litem, Stack_List.Next);
    Item := Litem;
    if Debug.Debug_Level_Array(Debug.Stack) then
      Debug.Put (Litem);
      Text_Io.New_Line;
    end if;
    -- Move back to last pushed item
    if not Stack_List.Is_Empty(Extra_List) then
      Stack_List.Move_To(Extra_List, Stack_List.Prev, 0, False);
    end if;
  exception
    when Stack_List.Empty_List =>
      if Debug.Debug_Level_Array(Debug.Stack) then
        Text_Io.Put_Line("raises EMPTY_STACK");
      end if;
      raise Empty_Stack;
  end Popf;

  procedure Pushf (Item : in Item_Rec) is
  begin
    if Debug.Debug_Level_Array(Debug.Stack) then
        Text_Io.Put ("Extra ");
      Text_Io.Put ("Stack: pushing at first ");
    end if;
    if Stack_List.Is_Empty(Extra_List) then
      Stack_List.Insert(Extra_List, Item);
      return;
    end if;

    -- Insert before first
    Stack_List.Move_To(Extra_List, Stack_List.Next, 0, False);
    Stack_List.Insert(Extra_List, Item, Stack_List.Prev);

    -- Move back to last pushed item
    Stack_List.Move_To(Extra_List, Stack_List.Prev, 0, False);
  end Pushf;

end Stack;

