with As.U.Utils;
separate (Mcd_Mng)

package body Call_Stack is

  List : As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  procedure Push (Item : in As.U.Asu_Us) is
  begin
    List.Insert (Item);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Pushing >" & Item.Image & "<"
        & "   Level is " & Integer'Image(List.List_Length));
    end if;
  end Push;

  function  Pop return As.U.Asu_Us is
    Item : As.U.Asu_Us;
  begin
    List.Get(Item, As.U.Utils.Asu_Dyn_List_Mng.Prev);
    if Debug.Debug_Level_Array(Debug.Call) then
      Async_Stdin.Put_Line_Err ("Call_stack: Poping >" & Item.Image & "<"
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

