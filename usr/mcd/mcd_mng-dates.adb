with Ada.Calendar;
with Perpet, Day_Mng, Normal;
separate (Mcd_Mng)

package body Dates is


  Seconds_Per_Day : constant := Integer(Ada.Calendar.Day_Duration'Last);
  Millisecs_Per_Day : constant My_Math.Inte := 1000 * Seconds_Per_Day;

  -- YYyy/mm/dd-hh:mm:ss.mmm
  Date_Length : constant := 23;


  -- Reference time
  Ref_Cal_Time : constant Ada.Calendar.Time
               := Ada.Calendar.Time_Of (1970, 01, 01, 0.0);


  function Clock return Item_Rec is
    Cal_Time : constant Ada.Calendar.Time :=  Ada.Calendar.Clock;
    Delta_D : Perpet.Delta_Rec;
    Res_Time : Item_Rec (Inte);
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisec;
  begin
    -- Get days and dur sinc ref time
    Delta_D := Perpet."-" (Cal_Time, Ref_Cal_Time);
    Res_Time.Val_Inte := My_Math.Inte(Delta_D.Days) * Seconds_Per_Day;

    -- Round millisecs
    begin
      Day_Mng.Split (Delta_D.Secs, Hours, Mins, Secs, Millis);
      Delta_D.Secs := Day_Mng.Pack (Hours, Mins, Secs, Millis);
      Res_Time.Val_Inte := Res_Time.Val_Inte + 60 * 60 * My_Math.Inte(Hours);
      Res_Time.Val_Inte := Res_Time.Val_Inte + 60 * My_Math.Inte(Mins);
      Res_Time.Val_Inte := Res_Time.Val_Inte + My_Math.Inte(Secs);
      Res_Time.Val_Inte := 1000 * Res_Time.Val_Inte + My_Math.Inte(Millis);
    exception
      when Constraint_Error =>
        -- Round leads to 24 hours
        Res_Time.Val_Inte := Res_Time.Val_Inte + Seconds_Per_Day;
        Res_Time.Val_Inte := 1000 * Res_Time.Val_Inte;
    end;
    return Res_Time;
  exception
    when Constraint_Error =>
      raise Compute_Error;
  end Clock;

  function Time_To_Date (Time : Item_Rec) return Item_Rec is
    Neg : Boolean;
    Days : Perpet.Day_Range;
    Tmp_Inte : My_Math.Inte;
    Seconds : Ada.Calendar.Day_Duration;
    Cal_Time : Ada.Calendar.Time;
    Date  : Item_Rec (Chrs);
    Year  : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day   : Ada.Calendar.Day_Number;
    Dur   : Ada.Calendar.Day_Duration;
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisec;
  begin
    if Time.Kind /= Inte then
      raise Invalid_Argument;
    end if;

    -- Split Time in days and seconds
    Neg := Time.Val_Inte < 0;
    Days := Perpet.Day_Range (abs (Time.Val_Inte) / Millisecs_Per_Day);
    Tmp_Inte := abs (Time.Val_Inte) rem Millisecs_Per_Day;
    Seconds := Ada.Calendar.Day_Duration(Tmp_Inte / 1000)
             + Ada.Calendar.Day_Duration(Tmp_Inte rem 1000) / 1000.0;


    -- Add time to ref_time
    if not Neg then
      Cal_Time := Perpet."+" (Ref_Cal_Time, Days);
    elsif Seconds = 0.0 then
      Cal_Time := Perpet."-" (Ref_Cal_Time, Days);
    else
      Cal_Time := Perpet."-" (Ref_Cal_Time, Days + 1);
      Seconds := 24.0 * 3600.0 - Seconds;
    end if;

    -- Split date and seconds
    Ada.Calendar.Split (Cal_Time, Year, Month, Day, Dur);
    if Dur /= 0.0 then
      raise Compute_Error;
    end if;
    Day_Mng.Split (Seconds, Hours, Mins, Secs, Millis);

    -- Format result in string
    Date.Val_Text := As.U.Tus (
            Normal (Year,  4, Gap => '0')
      & "/" & Normal (Month, 2, Gap => '0')
      & "/" & Normal (Day,   2, Gap => '0')
      & "-" & Normal (Hours, 2, Gap => '0')
      & ":" & Normal (Mins, 2,  Gap => '0')
      & ":" & Normal (Secs, 2,  Gap => '0')
      & "." & Normal (Millis, 3, Gap => '0') );
    return Date;
  exception
    when Constraint_Error =>
      raise Compute_Error;
  end Time_To_Date;

  function Time_To_Days (Time : Item_Rec) return Item_Rec is
    Days : Perpet.Day_Range;
    Tmp_Inte : My_Math.Inte;
    Seconds : Ada.Calendar.Day_Duration;
    Days_Image : Item_Rec;
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisec;
    Date  : Item_Rec (Chrs);
    use type As.U.Asu_Us;
  begin
    if Time.Kind /= Inte or else Time.Val_Inte < 0 then
      raise Invalid_Argument;
    end if;

    -- Split Time in days and seconds
    Days := Perpet.Day_Range (Time.Val_Inte / Millisecs_Per_Day);
    Tmp_Inte := Time.Val_Inte rem Millisecs_Per_Day;
    Seconds := Ada.Calendar.Day_Duration(Tmp_Inte / 1000)
             + Ada.Calendar.Day_Duration(Tmp_Inte rem 1000) / 1000.0;

    -- Get image (using format) of days
    Days_Image := Ios.Strof ((Kind => Inte,
                              Val_Inte => My_Math.Inte(Days)));
    Day_Mng.Split (Seconds, Hours, Mins, Secs, Millis);

    -- Format result in string
    Date.Val_Text := Days_Image.Val_Text
      & "-" & Normal (Hours, 2, Gap => '0')
      & ":" & Normal (Mins, 2,  Gap => '0')
      & ":" & Normal (Secs, 2,  Gap => '0')
      & "." & Normal (Millis, 3, Gap => '0');
    return Date;
  exception
    when Constraint_Error =>
      raise Compute_Error;
  end Time_To_Days;

  function Date_To_Time (Date : Item_Rec) return Item_Rec is
    Neg : Boolean;
    Year  : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day   : Ada.Calendar.Day_Number;
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisec;
    Cal_Time : Ada.Calendar.Time;
    Delta_D : Perpet.Delta_Rec;
    Res_Time : Item_Rec(Inte);
  begin
    -- Check kind and length
    if Date.Kind /= Chrs or else Date.Val_Text.Length /= Date_Length then
      raise Invalid_Argument;
    end if;

    -- Check format
    if      Date.Val_Text.Element (05) /= '/'
    or else Date.Val_Text.Element (08) /= '/'
    or else Date.Val_Text.Element (11) /= '-'
    or else Date.Val_Text.Element (14) /= ':'
    or else Date.Val_Text.Element (17) /= ':'
    or else Date.Val_Text.Element (20) /= '.' then
      raise Invalid_Argument;
    end if;

    -- Extract
    Year  := Ada.Calendar.Year_Number'Value  (Date.Val_Text.Slice (01, 04));
    Month := Ada.Calendar.Month_Number'Value (Date.Val_Text.Slice (06, 07));
    Day   := Ada.Calendar.Day_Number'Value   (Date.Val_Text.Slice (09, 10));
    Hours := Day_Mng.T_Hours'Value           (Date.Val_Text.Slice (12, 13));
    Mins  := Day_Mng.T_Minutes'Value         (Date.Val_Text.Slice (15, 16));
    Secs  := Day_Mng.T_Seconds'Value         (Date.Val_Text.Slice (18, 19));
    Millis := Day_Mng.T_Millisec'Value       (Date.Val_Text.Slice (21, 23));

    -- Compute Time for the day
    Cal_Time := Ada.Calendar.Time_Of (Year, Month, Day);

    -- Compute delta days since reference
    Neg := Ada.Calendar."<" (Cal_Time, Ref_Cal_Time);
    if not Neg then
      Delta_D := Perpet."-" (Cal_Time, Ref_Cal_Time);
    else
      Delta_D := Perpet."-" (Ref_Cal_Time, Cal_Time);
    end if;

    -- Convert to milliseconds
    Res_Time.Val_Inte := My_Math.Inte (Delta_D.Days);
    Res_Time.Val_Inte :=   24 * Res_Time.Val_Inte + My_Math.Inte (Hours);
    Res_Time.Val_Inte :=   60 * Res_Time.Val_Inte + My_Math.Inte (Mins);
    Res_Time.Val_Inte :=   60 * Res_Time.Val_Inte + My_Math.Inte (Secs);
    Res_Time.Val_Inte := 1000 * Res_Time.Val_Inte + My_Math.Inte (Millis);

    if Neg then
    Res_Time.Val_Inte := - Res_Time.Val_Inte;
    end if;
    return Res_Time;
  exception
    when Constraint_Error | Ada.Calendar.Time_Error =>
      raise Compute_Error;
  end Date_To_Time;

end Dates;

