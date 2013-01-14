-- Tranformation from and to struct timeval
with Ada.Calendar;
with Images, Normal;
package body Timeval is

  function Sec_Image is new Images.Int_Image (C_Types.Time_T);

  Dur_Day : constant Perpet.Natural_Duration
          := Ada.Calendar.Day_Duration'Last;

  function Delta2Timeout (Delta_Date : Perpet.Delta_Rec)
                         return C_Types.Timeval_T is
    Result : C_Types.Timeval_T;
  begin
    -- Split Delta.Secs into Result.Tv_Sec and Result.Tv_Usec
    Result.Tv_Sec := C_Types.Time_T (Delta_Date.Secs / Dur_Day);
    Result.Tv_Usec := C_Types.Suseconds_T (
      (Delta_Date.Secs - Dur_Day * Duration(Result.Tv_Sec)) * 1_000_000.0);
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
    Result.Secs := Result.Secs + Duration (Timeout.Tv_Usec) / 1_000_000.0;
    return Result;
  exception
    when Constraint_Error =>
      raise Timeval_Error;
  end Timeout2Delta;

  function Image (Timeout : C_Types.Timeval_T) return String is
  begin
    if Timeout.Tv_Sec < 0 or else Timeout.Tv_Usec < 0 then
      return "Infinite";
    else
      return Sec_Image (Timeout.Tv_Sec) & "."
           & Normal (Integer (Timeout.Tv_Usec), 6, Gap => '0');
    end if;
  end Image;

end Timeval;

