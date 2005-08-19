with Ada.Text_Io, Ada.Strings.Unbounded;
with Environ, Bool_Io, Arbitrary;
with Inte_Io, Real_Io;
package body Debug is

  procedure Init is
  begin
    Debug_Level_Array := (others => False);

    Debug_Level_Array(Parser) := Environ.Is_Yes ("MCD_DEBUG_PARSER");
    Debug_Level_Array(Input) := Environ.Is_Yes ("MCD_DEBUG_INPUT");
    Debug_Level_Array(Call) := Environ.Is_Yes ("MCD_DEBUG_CALL");
    Debug_Level_Array(Stack) := Environ.Is_Yes ("MCD_DEBUG_STACK");
    Debug_Level_Array(Register) := Environ.Is_Yes ("MCD_DEBUG_REGISTER");
    Debug_Level_Array(Oper) := Environ.Is_Yes ("MCD_DEBUG_OPER");
    Debug_Level_Array(Flow) := Environ.Is_Yes ("MCD_DEBUG_FLOW");
    Debug_Level_Array(History) := Environ.Is_Yes ("MCD_DEBUG_HISTORY");

    if Environ.Is_Yes ("MCD_DEBUG_ALL") then
      Debug_Level_Array := (others => True);
    end if;
  end Init;

  procedure Put (Item : in Mcd_Mng.Item_Rec) is
    use Mcd_Mng;
  begin
    case Item.Kind is
      when Arbi =>
        Ada.Text_Io.Put ("@" & Arbitrary.Image(Item.Val_Arbi));
      when Inte =>
        Inte_Io.Put(Item.Val_Inte);
      when Real =>
        Real_Io.Put(Item.Val_Real);
      when Bool =>
        Bool_Io.Put(Item.Val_Bool);
      when Chrs =>
        Ada.Text_Io.Put ("""" & Ada.Strings.Unbounded.To_String (Item.Val_Text) & """");
      when Prog =>
        Ada.Text_Io.Put ("[ " & Ada.Strings.Unbounded.To_String (Item.Val_Text) & " ]");
      when Regi =>
        Ada.Text_Io.Put (Item.Val_Regi);
      when Oper =>
        Ada.Text_Io.Put (Operator_List'Image(Item.Val_Oper));
    end case;
  end Put;

end Debug;

