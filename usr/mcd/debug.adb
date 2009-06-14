with Ada.Strings.Unbounded;
with Environ, Bool_Io, Arbitrary.Fractions, Async_Stdin;
package body Debug is

  package Unb renames Ada.Strings.Unbounded;

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
        Async_Stdin.Put_Err ("@" & Arbitrary.Image(Item.Val_Arbi));
      when Frac =>
        Async_Stdin.Put_Err ("@" & Arbitrary.Fractions.Image(Item.Val_Frac));
      when Inte =>
        Async_Stdin.Put_Err (Item.Val_Inte'Img);
      when Real =>
        Async_Stdin.Put_Err (Item.Val_Real'Img);
      when Bool =>
        Bool_Io.Put(Item.Val_Bool);
      when Chrs =>
        Async_Stdin.Put_Err ("""" & Unb.To_String (Item.Val_Text) & """");
      when Prog =>
        Async_Stdin.Put_Err ("[ " & Unb.To_String (Item.Val_Text) & " ]");
      when Regi =>
        Async_Stdin.Put_Err (Item.Val_Regi & "");
      when Oper =>
        Async_Stdin.Put_Err (Operator_List'Image(Item.Val_Oper));
    end case;
  end Put;

end Debug;

