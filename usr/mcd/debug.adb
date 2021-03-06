with As.U, Arbitrary.Fractions, Mixed_Str, Argument, Environ;
package body Debug is

  procedure Init is
  begin
    for Level in Level_List loop
      Loggers(Level).Init (Mixed_Str (Level'Img));
    end loop;
    if Environ.Is_Yes (Argument.Get_Program_Name & "_DEBUG") then
      for Level in Level_List loop
        Loggers(Level).Add_Mask (Trace.Debug);
      end loop;
    end if;
  end Init;

  procedure Log (Level : in Level_List; Message : in String) is
  begin
    Loggers(Level).Log_Debug (Message);
  end Log;

  procedure Log (Level   : in Level_List;
                 Item    : in Mcd_Mng.Item_Rec;
                 Message : in String := "") is
    Text : As.U.Asu_Us;
    use Mcd_Mng;
  begin
    if Message /= "" then
      Text.Set (Message & " ");
    end if;
    case Item.Kind is
      when Arbi =>
        Text.Append ("@" & Item.Val_Arbi.Image);
      when Frac =>
        Text.Append ("@" & Item.Val_Frac.Image);
      when Inte =>
        Text.Append (Item.Val_Inte'Img);
      when Real =>
        Text.Append (Item.Val_Real'Img);
      when Bool =>
        Text.Append (Item.Val_Bool'Img);
      when Chrs =>
        Text.Append ("""" & Item.Val_Text.Image & """");
      when Prog =>
        Text.Append ("[ " & Item.Val_Text.Image & " ]");
      when Regi =>
        Text.Append (Item.Val_Regi & "");
      when Oper =>
        Text.Append (Operator_List'Image(Item.Val_Oper));
    end case;
    Loggers(Level).Log_Debug (Text.Image);
  end Log;

end Debug;

