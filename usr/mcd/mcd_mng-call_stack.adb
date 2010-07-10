separate (Mcd_Mng)

package body Call_Stack is

  List : Asu_Dyn_List_Mng.List_Type;

  procedure Push (Item : in Asu_Us) is
  begin
    List.Insert (Item);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Pushing >" & Asu_Ts (Item) & "<"
        & "   Level is " & Integer'Image(List.List_Length));
    end if;
  end Push;

  function  Pop return Asu_Us is
    Item : Asu_Us;
  begin
    List.Get(Item, Asu_Dyn_List_Mng.Prev);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Poping >" & Asu_Ts (Item) & "<"
        & "   Level is " & Integer'Image(List.List_Length));
    end if;
    return Item;
  end Pop;

  function Level return Natural is
  begin
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Level "
        & Integer'Image(List.List_Length));
    end if;
    return List.List_Length;
  end Level;

end Call_Stack;

