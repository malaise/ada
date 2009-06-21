-- Tranformation from and to struct timeval
with Ada.Calendar;
with Int_Image, Normal;
package body Timeval is

  function Sec_Image is new Int_Image (Integer);

  Dur_Day : constant Perpet.Natural_Duration
          := Ada.Calendar.Day_Duration'Last;

  function To_C_Timeout (Delta_Date : Perpet.Delta_Rec) return C_Timeout_T is
    Result : C_Timeout_T;
  begin
    -- Split Delta.Secs into Result.Tv_Sec and Result.Tv_Usec
    Result.Tv_Sec := Integer (Delta_Date.Secs / Dur_Day);
    Result.Tv_Usec := Integer (
      (Delta_Date.Secs - Dur_Day * Duration(Result.Tv_Sec)) * 1_000_000.0);
    Result.Tv_Sec := Result.Tv_Sec + Delta_Date.Days * Integer (Dur_Day);
    return Result;
  exception
    when Constraint_Error =>
      raise Timeval_Error;
  end To_C_Timeout;

  function To_Delta (Timeout : C_Timeout_T) return Perpet.Delta_Rec is
    Result : Perpet.Delta_Rec;
  begin
    -- Split Timeout.Tv_Sec into Result.Days and Result.Secs
    Result.Days := Timeout.Tv_Sec / Integer(Dur_Day);
    Result.Secs :=  Ada.Calendar.Day_Duration (
      Timeout.Tv_Sec rem Integer(Dur_Day));
    -- Add Timeout.Tv_Usec to Result.Secs
    -- we assume that Duration'Last > 1_000_000.0
    Result.Secs := Result.Secs + Duration (Timeout.Tv_Usec) / 1_000_000.0;
    return Result;
  exception
    when Constraint_Error =>
      raise Timeval_Error;
  end To_Delta;

  function Image (Timeout : C_Timeout_T) return String is
  begin
    if Timeout.Tv_Sec < 0 or else Timeout.Tv_Usec < 0 then
      return "Infinite";
    else
      return Sec_Image (Timeout.Tv_Sec) & "."
           & Normal (Timeout.Tv_Usec, 6, Gap => '0');
    end if;
  end Image;
end Timeval;

