with Text_Line;
with Sok_Types;
package body Dump_Sav is
  File : Text_Line.File_Type;

  procedure Dump (Mvt : in Sok_Movement.Saved_Data_Rec; Pushed : in Boolean) is
  begin
    if not File.Is_Open then
      File.Create_All ("dump");
    end if;

    if Pushed then
      File.Put_Line ("PUSHED");
    else
      File.Put_Line ("POPED");
    end if;

    File.Put_Line ("POS_ORIG " &
     Sok_Types.Row_Range'Image(Mvt.Pos_Orig.Row) & " " &
     Sok_Types.Col_Range'Image(Mvt.Pos_Orig.Col) );

    File.Put_Line ("MOVEMENT "
                 & Sok_Movement.Movement_List'Image (Mvt.Movement) );

    File.Put_Line ("RESULT "
                 & Sok_Movement.Saved_Result_List'Image (Mvt.Result) );

    File.Put_Line ("---------------------");
  end Dump;

end Dump_Sav;

