-- time displaying
with Calendar;
with Normal, Day_Mng;

separate (Nav_Screen)
procedure Show_Time is
  Year  : Calendar.Year_Number;
  Month : Calendar.Month_Number;
  Day   : Calendar.Day_Number;
  Dur   : Calendar.Day_Duration;
  Hor : Day_Mng.T_Hours;
  Min : Day_Mng.T_Minutes;
  Sec : Day_Mng.T_Seconds;
  Mil : Day_Mng.T_Millisec;
begin
  -- get date and time
  Calendar.Split (Calendar.Clock, Year, Month, Day, Dur);
  -- compute time hours, minutes and seconds
  Day_Mng.Split (Dur, Hor, Min, Sec, Mil);
  -- put
  Con_Io.Move ( (0, 0), W_Time);
  Con_Io.Put (
   S =>
    Normal(Day, 2, True, '0') & "/" & Normal(Month, 2, True, '0') &
    "/" & Normal(Year, 4, True, '0') & " " &
    Normal(Hor, 2, True, '0') & ":" & Normal(Min, 2, True, '0') & ":" &
    Normal(Sec, 2, True, '0'),
   Name => W_Time);
end Show_Time;
