-- Tranformation from and to struct timeval
with Ada.Calendar;
with Images, Normal;
package body Timeval is

  function Sec_Image is new Images.Int_Image (C_Types.Time_T);

  Dur_Day : constant Perpet.Natural_Duration
          := Ada.Calendar.Day_Duration'Last;

  Million : constant := 1_000_000;
  Milliond : constant Duration := Duration (Million);

  function Delta2Timeout (Delta_Date : Perpet.Delta_Rec)
                         return C_Types.Timeval_T is
    Result : C_Types.Timeval_T;
  begin
    -- Split Delta.Secs into Result.Tv_Sec and Result.Tv_Usec
    Result.Tv_Sec := C_Types.Time_T (Delta_Date.Secs / Dur_Day);
    Result.Tv_Usec := C_Types.Suseconds_T (
      (Delta_Date.Secs - Dur_Day * Duration(Result.Tv_Sec)) * Milliond);
    Result.Tv_Sec := Result.Tv_Sec
                   + C_Types.Time_T (Delta_Date.Days * Integer (Dur_Day));
    return Result;
  exception
    when Constraint_Error =>
      raise Timeval_Error;
  end Delta2Timeout;

  function Timeout2Delta (Timeout : C_Types.Timeval_T) return Perpet.Delta_Rec is
    Result : Perpet.Delta_Rec;
  begin
    -- Split Timeout.Tv_Sec into Result.Days and Result.Secs
    Result.Days := Perpet.Day_Range (Timeout.Tv_Sec / C_Types.Time_T (Dur_Day));
    Result.Secs :=  Ada.Calendar.Day_Duration (
      Timeout.Tv_Sec rem C_Types.Time_T(Dur_Day));
    -- Add Timeout.Tv_Usec to Result.Secs
    -- we assume that Duration'Last > 1_000_000.0
    Result.Secs := Result.Secs + Duration (Timeout.Tv_Usec) / Milliond;
    return Result;
  exception
    when Constraint_Error =>
      raise Timeval_Error;
  end Timeout2Delta;

  function Image (Timeout : C_Types.Timeval_T) return String is
    (if Timeout.Tv_Sec < 0 or else Timeout.Tv_Usec < 0 then "Infinite"
     else Sec_Image (Timeout.Tv_Sec) & "."
        & Normal (Integer (Timeout.Tv_Usec), 6, Gap => '0'));

  -- Ensure that abs(Tv_Usec) < 1_000_000 and that Tv_Sec and Tv_Usec have the
  --  same sign
  procedure Normalize (Timeout : in out C_Types.Timeval_T) is
  begin
    -- Ensure that abs(Tv_Usec) < Million. Adjust Tv_Sec
    while Timeout.Tv_Usec <= -Million loop
      Timeout.Tv_Sec  := Timeout.Tv_Sec  - 1;
      Timeout.Tv_Usec := Timeout.Tv_Usec + Million;
    end loop;
    while Timeout.Tv_Usec >= Million loop
      Timeout.Tv_Sec  := Timeout.Tv_Sec  + 1;
      Timeout.Tv_Usec := Timeout.Tv_Usec - Million;
    end loop;

    -- Ensure that Tv_Sec and Tv_Usec have the same sign
    if Timeout.Tv_Sec > 0 and then Timeout.Tv_Usec < 0 then
      Timeout.Tv_Sec  := Timeout.Tv_Sec  - 1;
      Timeout.Tv_Usec := Timeout.Tv_Usec + Million;
    elsif Timeout.Tv_Sec < 0 and then Timeout.Tv_Usec > 0 then
      Timeout.Tv_Sec  := Timeout.Tv_Sec  + 1;
      Timeout.Tv_Usec := Timeout.Tv_Usec - Million;
    end if;
  end Normalize;

end Timeval;

