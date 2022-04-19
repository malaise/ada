with Ada.Calendar;
with Perpet, Sys_Calls, Date_Text;
separate (Als)

function Parse_Date (Str : String) return Entities.Date_Spec_Rec is
  -- Local copy of input, so it starts at 1
  Lstr : constant String (1 .. Str'Length) := Str;
  -- Date and time fields
  Date, Time : Date_Text.Date_Rec;

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
  if Lstr'Length = 21 and then Lstr(7) = '-' and then Lstr(10) = '-'
  and then Lstr(13) = 'T' and then Lstr(16) = ':' and then Lstr(19) = ':' then
    -- yyyy-mm-ddThh:mm:ss
    Date := Date_Text.Scan (Lstr(3 .. 21), "%Y-%m-%dT%H:%M:%S");
    if Crit.Oper = Entities.Less_Or_Equal
    or else Crit.Oper = Entities.Greater_Than then
      Date.Millisecs := 999;
      Date.Microsecs := 999;
    else
      Date.Millisecs := 0;
      Date.Microsecs := 0;
    end if;
    Crit.Date := Date_Text.Pack (Date);
  elsif Lstr'Length = 12 and then Lstr(7) = '-' and then Lstr(10) = '-' then
    -- yyyy-mm-dd
    Date := Date_Text.Scan (Lstr(3 .. 12), "%Y-%m-%d");
    if Crit.Oper = Entities.Less_Or_Equal
    or else Crit.Oper = Entities.Greater_Than then
      Date.Hours := 23;
      Date.Minutes := 59;
      Date.Seconds := 59;
      Date.Millisecs := 999;
      Date.Microsecs := 999;
    else
      Date.Hours := 0;
      Date.Minutes := 0;
      Date.Seconds := 0;
      Date.Millisecs := 0;
      Date.Microsecs := 0;
    end if;
    Crit.Date := Date_Text.Pack (Date);
  elsif Lstr'Length = 10
  and then Lstr(5) = ':' and then Lstr(8) = ':' then
    -- Get current day
    Date := Date_Text.Split (Ada.Calendar.Clock);
    -- hh:mm:ss
    Time := Date_Text.Scan (Lstr(3 .. 10), "%H:%M:%S");
    if Crit.Oper = Entities.Less_Or_Equal
    or else Crit.Oper = Entities.Greater_Than then
      Time.Millisecs := 999;
      Date.Microsecs := 999;
    else
      Time.Millisecs := 0;
      Date.Microsecs := 0;
    end if;
    -- Apply to current day
    Time.Years  := Date.Years;
    Time.Months := Date.Months;
    Time.Days   := Date.Days;
    Crit.Date := Date_Text.Pack (Time);
  else
    -- <positive><letter>, where <letter> ::= Y|M|D|h|m|s
    declare
      N : constant Positive := Positive'Value (Lstr(3 .. Lstr'Last-1));
      C : constant Character := Lstr(Lstr'Last);
      use type Ada.Calendar.Time, Perpet.Duration_Rec;
    begin
      case C is
        when 'Y' =>
          Crit.Date := Ada.Calendar.Clock - (Years => N, Months => 0);
        when 'M' =>
          Crit.Date := Ada.Calendar.Clock - (Years => 0, Months => N);
        when 'D' =>
          Crit.Date := Perpet."-" (Ada.Calendar.Clock, N);
        when 'h' =>
          Crit.Date := Ada.Calendar.Clock - Duration(N) * 3600;
        when 'm' =>
          Crit.Date := Ada.Calendar.Clock - Duration(N) * 60;
        when 's' =>
          Crit.Date := Ada.Calendar.Clock - Duration(N);
        when others =>
          raise Error_Exception;
      end case;
    end;
  end if;
  -- Clock is in Local time, convert into UTC if requested
  if Utc then
    declare
      use type Ada.Calendar.Time;
    begin
      Crit.Date := Crit.Date - Sys_Calls.Gmt_Offset;
    end;
  end if;
  return Crit;
exception
  when others =>
    Error ("Invalid date: " & Str);
    -- Error already raises it... but we get a warning here
    raise Error_Exception;
end Parse_Date;

