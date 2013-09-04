with Trace, Arbitrary.Fractions, Async_Stdin, Mixed_Str;
package body Debug is

  procedure Init is
    -- Loggers, used only to detect if DEBUG is set
    Loggers : array (Debug_Level_List) of Trace.Logger;
  begin
    for Level in Debug_Level_List loop
      Loggers(Level).Init (Mixed_Str (Level'Img));
      Debug_Level_Array(Level) := Loggers(Level).Debug_On;
    end loop;
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
        Async_Stdin.Put_Err (Item.Val_Bool'Img);
      when Chrs =>
        Async_Stdin.Put_Err ("""" & Item.Val_Text.Image & """");
      when Prog =>
        Async_Stdin.Put_Err ("[ " & Item.Val_Text.Image & " ]");
      when Regi =>
        Async_Stdin.Put_Err (Item.Val_Regi & "");
      when Oper =>
        Async_Stdin.Put_Err (Operator_List'Image(Item.Val_Oper));
    end case;
  end Put;

end Debug;

