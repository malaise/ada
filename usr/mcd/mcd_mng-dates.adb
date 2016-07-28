with Ada.Calendar;
with Perpet, Day_Mng, Normal, Date_Text;
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
    Millis : Day_Mng.T_Millisecs;
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
    Result  : Item_Rec (Chrs);
    Date : Date_Text.Date_Rec;
    Dur   : Ada.Calendar.Day_Duration;
  begin
    if Time.Kind /= Inte then
      raise Invalid_Argument;
    end if;

    -- Split Time in days and seconds
    Neg := Time.Val_Inte < 0;
    Days := Perpet.Day_Range (abs Time.Val_Inte / Millisecs_Per_Day);
    Tmp_Inte := abs Time.Val_Inte rem Millisecs_Per_Day;
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
    Ada.Calendar.Split (Cal_Time, Date.Years, Date.Months, Date.Days, Dur);
    if Dur /= 0.0 then
      raise Compute_Error;
    end if;
    Day_Mng.Split (Dur, Date.Hours, Date.Minutes, Date.Seconds, Date.Millisecs);

    -- Format result in string
    Result.Val_Text := As.U.Tus (Date_Text.Put (Date, "%Y/%m/%d-%H:%M:%S.%s"));
    return Result;
  exception
    when Constraint_Error =>
      raise Compute_Error;
  end Time_To_Date;

  function Time_To_Days (Time : Item_Rec) return Item_Rec is
    Days : Perpet.Day_Range;
    Tmp_Inte : My_Math.Inte;
    Seconds : Ada.Calendar.Day_Duration;
    Days_Image : Item_Rec;
    Date : Date_Text.Date_Rec;
    Result  : Item_Rec (Chrs);
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
    Day_Mng.Split (Seconds, Date.Hours, Date.Minutes, Date.Seconds,
                   Date.Millisecs);

    -- Format result in string
    Result.Val_Text := Days_Image.Val_Text
                     & Date_Text.Put (Date, "-%H:%M:%S.%s");
    return Result;
  exception
    when Constraint_Error =>
      raise Compute_Error;
  end Time_To_Days;

  function Date_To_Time (Date : Item_Rec) return Item_Rec is
    Neg : Boolean;
    Date_Rec : Date_Text.Date_Rec;
    Cal_Time : Ada.Calendar.Time;
    Delta_D : Perpet.Delta_Rec;
    Res_Time : Item_Rec(Inte);
    Res_Error : Item_Rec(Bool);
  begin
    -- Check kind and length
    if Date.Kind /= Chrs or else Date.Val_Text.Length /= Date_Length then
      raise Invalid_Argument;
    end if;

    -- Scan
    begin
      Date_Rec := Date_Text.Scan (Date.Val_Text.Slice (01, 23),
                                 "%Y/%m/%d-%H:%M:%S.%s");
    exception
      when Date_Text.Invalid_String =>
        raise Invalid_Argument;
    end;

    -- Compute Time for the day (check validity)
    Cal_Time := Ada.Calendar.Time_Of (Date_Rec.Years, Date_Rec.Months,
                                      Date_Rec.Days);

    -- Compute delta days since reference
    Neg := Ada.Calendar."<" (Cal_Time, Ref_Cal_Time);
    if not Neg then
      Delta_D := Perpet."-" (Cal_Time, Ref_Cal_Time);
    else
      Delta_D := Perpet."-" (Ref_Cal_Time, Cal_Time);
    end if;

    -- Convert to milliseconds
    Res_Time.Val_Inte := My_Math.Inte (Delta_D.Days);
    Res_Time.Val_Inte :=   24 * Res_Time.Val_Inte
                       + My_Math.Inte (Date_Rec.Hours);
    Res_Time.Val_Inte :=   60 * Res_Time.Val_Inte
                       + My_Math.Inte (Date_Rec.Minutes);
    Res_Time.Val_Inte :=   60 * Res_Time.Val_Inte
                       + My_Math.Inte (Date_Rec.Seconds);
    Res_Time.Val_Inte := 1000 * Res_Time.Val_Inte
                       + My_Math.Inte (Date_Rec.Millisecs);

    if Neg then
      Res_Time.Val_Inte := - Res_Time.Val_Inte;
    end if;
    return Res_Time;
  exception
    when Constraint_Error | Ada.Calendar.Time_Error =>
      -- Format is correct (otherwise Invalid_Argument)
      -- But date values are not valid => return False
      Res_Error.Val_Bool := False;
      return Res_Error;
  end Date_To_Time;

end Dates;

