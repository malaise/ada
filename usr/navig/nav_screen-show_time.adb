-- time displaying
with Ada.Calendar;
with Date_Text;

separate (Nav_Screen)
procedure Show_Time is
  Date : Date_Text.Date_Rec;
begin
  -- Get date and ours, minutes and seconds
  Date := Date_Text.Split (Ada.Calendar.Clock);
  -- put
  W_Time.Move ( (0, 0));
  W_Time.Put (
   S => Date_Text.Put (Date, "%d/%m/%Y %H:%M:%S"));
end Show_Time;

