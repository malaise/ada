with Ada.Text_Io;
with Dynamic_List;
separate (Mcd_Mng)

package body Call_Stack is 

  type Call_Entry_Rec is Record
    Str : Chars_Text;
    Len : Natural;
  end record;
  Call_Entry : Call_Entry_Rec;


  package Call_Stack_List is new Dynamic_List (Call_Entry_Rec);
  List : Call_Stack_List.List_Type;

  procedure Push (Item : in String) is
  begin
    Call_Entry.Len := Item'Length;
    Call_Entry.Str (1 .. Call_Entry.Len) := Item;
    Call_Stack_List.Insert(List, Call_Entry);
    if Debug.Debug_Level_Array(Debug.Call) then
      Ada.Text_Io.Put_Line ("Call_stack: Pushing >" & Item & "<"
        & "   Level is "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
  end Push;

  function  Pop return String is
  begin
    Call_Stack_List.Get(List, Call_Entry, Call_Stack_List.Prev);
    if Debug.Debug_Level_Array(Debug.Call) then
      Ada.Text_Io.Put_Line ("Call_stack: Poping >"
        & Call_Entry.Str(1 .. Call_Entry.Len) & "<"
        & "   Level is "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
    return Call_Entry.Str(1 .. Call_Entry.Len);
  end Pop;

  function Level return Natural is
  begin
    if Debug.Debug_Level_Array(Debug.Call) then
      Ada.Text_Io.Put_Line ("Call_stack: Level "
        & Integer'Image(Call_Stack_List.List_Length(List)));
    end if;
    return Call_Stack_List.List_Length(List);
  end Level;

end Call_Stack;

