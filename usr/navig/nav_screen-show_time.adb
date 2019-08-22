-- time displaying
with Ada.Calendar;
with Date_Text;

separate (Nav_Screen)
procedure Show_Time is
  -- Get date and hours, minutes and seconds
  Date : constant Date_Text.Date_Rec := Date_Text.Split (Ada.Calendar.Clock);
begin
  -- Put
  W_Time.Move ( (0, 0));
  W_Time.Put (
   S => Date_Text.Put (Date, "%d/%m/%Y %H:%M:%S"));
end Show_Time;

