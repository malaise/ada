with Dynamic_List;
separate (Mcd_Mng)

package body Call_Stack is

  type Call_Entry_Rec is record
    Str : Unb.Unbounded_String;
  end record;
  Call_Entry : Call_Entry_Rec;


  package Call_Stack_Dyn_List is new Dynamic_List (Call_Entry_Rec);
  package Call_Stack_List renames Call_Stack_Dyn_List.Dyn_List;
  List : Call_Stack_List.List_Type;

  procedure Push (Item : in Unb.Unbounded_String) is
  begin
    Call_Entry.Str := Item;
    Call_Stack_List.Insert(List, Call_Entry);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Pushing >" & Unb.To_String (Item) & "<"
        & "   Level is "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
  end Push;

  function  Pop return Unb.Unbounded_String is
  begin
    Call_Stack_List.Get(List, Call_Entry, Call_Stack_List.Prev);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Poping >"
        & Unb.To_String (Call_Entry.Str) & "<"
        & "   Level is "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
    return Call_Entry.Str;
  end Pop;

  function Level return Natural is
  begin
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Level "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
    return Call_Stack_List.List_Length(List);
  end Level;

end Call_Stack;

