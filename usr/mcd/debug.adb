with Text_Io;
with Sys_Calls; use Sys_Calls;
with Bool_Io, Inte_Io, Real_Io;
package body Debug is
  Val : String (1 .. 1);
  Set, Trunc : Boolean;
  Len : Natural;

  procedure Init is
  begin
    Debug_Level_Array := (others => False);

    Getenv ("MCD_DEBUG_PARSER", Set, Trunc, Val, Len);
    Debug_Level_Array(Parser) := Set;

    Getenv ("MCD_DEBUG_INPUT", Set, Trunc, Val, Len);
    Debug_Level_Array(Input) := Set;
    
    Getenv ("MCD_DEBUG_CALL", Set, Trunc, Val, Len);
    Debug_Level_Array(Call) := Set;

    Getenv ("MCD_DEBUG_STACK", Set, Trunc, Val, Len);
    Debug_Level_Array(Stack) := Set;

    Getenv ("MCD_DEBUG_REGISTER", Set, Trunc, Val, Len);
    Debug_Level_Array(Register) := Set;

    Getenv ("MCD_DEBUG_OPER", Set, Trunc, Val, Len);
    Debug_Level_Array(Oper) := Set;

    Getenv ("MCD_DEBUG_HISTORY", Set, Trunc, Val, Len);
    if Set then
      Debug_Level_Array(History) := Set;
    end if;

    Getenv ("MCD_DEBUG_ALL", Set, Trunc, Val, Len);
    if Set then
      Debug_Level_Array := (others => True);
    end if;
  end Init;

  procedure Put (Item : in Mcd_Mng.Item_Rec) is
    use Mcd_Mng;
  begin
    case Item.Kind is
      when Inte =>
        Inte_Io.Put(Item.Val_Inte);
      when Real =>
        Real_Io.Put(Item.Val_Real);
      when Bool =>
        Bool_Io.Put(Item.Val_Bool);
      when Chrs =>
        Text_Io.Put ("""" & Item.Val_Text(1 .. Item.Val_Len) & """");
      when Prog =>
        Text_Io.Put ("[ " & Item.Val_Text(1 .. Item.Val_Len) & " ]");
      when Regi =>
        Text_Io.Put (Item.Val_Regi);
      when Oper =>
        Text_Io.Put (Operator_List'Image(Item.Val_Oper));
    end case;
  end Put;

end Debug;
