with Day_Mng, Perpet;
separate (Als)

function Parse_Date (Str : String) return Entities.Date_Spec_Rec is
  -- Local copy of input, so it starts at 1
  Lstr : constant String (1 .. Str'Length) := Str;
  -- Date and time fields
  Year : Ada.Calendar.Year_Number;
  Month : Ada.Calendar.Month_Number;
  Day : Ada.Calendar.Day_Number;
  Dur : Ada.Calendar.Day_Duration;
  Hour : Day_Mng.T_Hours;
  Minute : Day_Mng.T_Minutes;
  Second : Day_Mng.T_Seconds;
  Milli : Day_Mng.T_Millisec;

  -- Result
  Crit : Entities.Date_Spec_Rec;
  use type Entities.Date_Oper_List;
begin
  -- At least 4 characters xxNy
  if Lstr'Length < 4 then
    raise Error_Exception;
  end if;
  -- Parse operation
  if Lstr(1 .. 2) = "eq" then
    Crit.Oper := Entities.Equal;
  elsif Lstr(1 .. 2) = "lt" then
    Crit.Oper := Entities.Less_Than;
  elsif Lstr(1 .. 2) = "le" then
    Crit.Oper := Entities.Less_Or_Equal;
  elsif Lstr(1 .. 2) = "gt" then
    Crit.Oper := Entities.Greater_Than;
  elsif Lstr(1 .. 2) = "ge" then
    Crit.Oper := Entities.Greater_Or_Equal;
  else
    raise Error_Exception;
  end if;
  if Lstr'Length = (18) and then Lstr(7) = '/' and then Lstr(10) = '/'
  and then Lstr(13) = '-' and then Lstr(16) = ':' then
    -- yyyy/mm/dd-hh:mm
    Year := Ada.Calendar.Year_Number'Value (Lstr(3 .. 6));
    Month := Ada.Calendar.Month_Number'Value (Lstr(8 .. 9));
    Day := Ada.Calendar.Day_Number'Value (Lstr(11 .. 12));
    Hour := Day_Mng.T_Hours'Value (Lstr(14 .. 15));
    Minute := Day_Mng.T_Minutes'Value (Lstr(17 .. 18));
    Second := 0;
    Milli := 0;
    Crit.Date := Ada.Calendar.Time_Of (Year, Month, Day,
                   Day_Mng.Pack (Hour, Minute, Second, Milli));
  elsif Lstr'Length = (7) and then Lstr(5) = ':' then
    -- hh:mm
    Hour := Day_Mng.T_Hours'Value (Lstr(3 .. 4));
    Minute := Day_Mng.T_Minutes'Value (Lstr(6 .. 7));
    Second := 0;
    Milli := 0;
    -- Apply to current day
    Ada.Calendar.Split (Ada.Calendar.Clock,
                        Year, Month, Day, Dur);
    Crit.Date := Ada.Calendar.Time_Of (Year, Month, Day,
                   Day_Mng.Pack (Hour, Minute, Second, Milli));
  else
    -- <positive><letter>, where <letter> ::= Y|M|D|hm
    declare
      N : constant Positive := Positive'Value (Lstr(3 .. Lstr'Last-1));
      C : constant Character := Lstr(Lstr'Last);
      use Perpet, Ada.Calendar;
    begin
      case C is
        when 'Y' =>
          Crit.Date := Ada.Calendar.Clock - (Years => N, Months => 0);
        when 'M' =>
          Crit.Date := Ada.Calendar.Clock - (Years => 0, Months => N);
        when 'D' =>
          Crit.Date := Ada.Calendar.Clock - N;
        when 'h' =>
          Crit.Date := Ada.Calendar.Clock - Duration(N) * 3600;
        when 'm' =>
          Crit.Date := Ada.Calendar.Clock - Duration(N) * 60;
        when others =>
          raise Error_Exception;
      end case;
    end;
  end if;
  return Crit;
exception
  when others =>
    Error ("Invalid date: " & Str);
    -- Error already raises it... but we get a warning here
    raise Error_Exception;
end Parse_Date;

