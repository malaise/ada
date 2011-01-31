-- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
-- 23 characters
with Normal, Day_Mng;
function Date_Image (Date : Ada.Calendar.Time;
                     Iso  : Boolean := False) return String is
  Year   : Ada.Calendar.Year_Number;
  Month  : Ada.Calendar.Month_Number;
  Day    : Ada.Calendar.Day_Number;
  Dur    : Ada.Calendar.Day_Duration;
  Hours  : Day_Mng.T_Hours;
  Mins   : Day_Mng.T_Minutes;
  Secs   : Day_Mng.T_Seconds;
  Millis : Day_Mng.T_Millisec;

  Sepd : Character := '/';
  Sepdt : Character := ' ';

begin
  Ada.Calendar.Split (Date, Year, Month, Day, Dur);
  Day_Mng.Split (Dur, Hours, Mins, Secs, Millis);

  if Iso then
    Sepd := '-';
    Sepdt := 'T';
  end if;

  return Normal (Year,   4, Gap => '0') & Sepd
       & Normal (Month,  2, Gap => '0') & Sepd
       & Normal (Day,    2, Gap => '0') & Sepdt
       & Normal (Hours,  2, Gap => '0') & ':'
       & Normal (Mins,   2, Gap => '0') & ':'
       & Normal (Secs,   2, Gap => '0') & '.'
       & Normal (Millis, 3, Gap => '0');

end Date_Image;

