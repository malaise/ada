with Con_Io;
with Nav_Data, Nav_Screen;
use  Nav_Data, Nav_Screen;
procedure T_Scr is
  Act : Action;
  B : Boolean;
begin
  Reset;
  Title;
  Put_Mask;
  Put (Wind_S, "  29.5");
  Put (Traj_A, "361.40", True);
  Put (Drift, "-178.00");
  Put_Result (Traj_A, "  1.40");
  Arrow (Traj_A);
  Put_Result (Plan_S, " 120.0");
  Dot (Plan_S);
  Act := Get_Action;
  Err_Format;
  Clear_Line (Traj_A);
  Clear_Line (Plan_S);
  delay 2.0;
  Clear_Err;
  Put_Help;
  B := Confirm_Quit;
end T_Scr;
