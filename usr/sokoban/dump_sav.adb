with Sok_Movement;

package Dump_Save is
  procedure Dump (Mvt : in Sok_Movement.Saved_Data_Rec; Pushed : in Boolean);
end Dump_Save;

with Sok_Types;
with Text_Io; use Text_Io;
package body Dump_Save is
  File : File_Type;
  Open : Boolean := False;

  procedure Dump (Mvt : in Sok_Movement.Saved_Data_Rec; Pushed : in Boolean) is
  begin
    if not Open then
      Create (File, Out_File, "dump");
      Open := True;
    end if;

    if Pushed then
      Put_Line ("PUSHED");
    else
      Put_Line ("POPED");
    end if;

    Put_Line ("POS_ORIG " &
     Sok_Types.Row_Range'Image(Mvt.Pos_Orig.Row) & " " &
     Sok_Types.Col_Range'Image(Mvt.Pos_Orig.Col) );

    Put_Line ("MOVEMENT " & Sok_Movement.Movement_List'Image (Mvt.Movement) );

    Put_Line ("RESULT " & Sok_Movement.Saved_Result_List'Image (Mvt.Result) );

    Put_Line ("---------------------");
  end Dump;

end Dump_Save;

