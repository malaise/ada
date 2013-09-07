separate (Mcd_Mng)

package body Call_Stack is

  List : As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  procedure Push (Item : in As.U.Asu_Us) is
  begin
    List.Insert (Item);
    Debug.Log (Debug.Call, "Pushing >" & Item.Image & "<"
                         & "   Level is " & Integer'Image(List.List_Length));
  end Push;

  function  Pop return As.U.Asu_Us is
    Item : As.U.Asu_Us;
  begin
    List.Get(Item, As.U.Utils.Asu_Dyn_List_Mng.Prev);
    Debug.Log (Debug.Call, "Poping >" & Item.Image & "<"
                         & "   Level is " & Integer'Image(List.List_Length));
    return Item;
  end Pop;

  function Level return Natural is
  begin
    Debug.Log (Debug.Call, "Level " & Integer'Image(List.List_Length));
    return List.List_Length;
  end Level;

end Call_Stack;

