-- Tranformation from and to struct timeval
with Perpet;
package Timeval is

  type C_Timeout_T is record
    Tv_Sec : Integer;
    Tv_Usec : Integer;
  end record;

  Infinite_C_Timeout : constant C_Timeout_T := (-1, -1);

  function To_C_Timeout (Delta_Date : Perpet.Delta_Rec) return C_Timeout_T;

  function To_Delta (Timeout : C_Timeout_T) return Perpet.Delta_Rec;

  function Image (Timeout : C_Timeout_T) return String;

  -- If overflow on Tv_Sec in To_C_Timeout
  -- If Timeout of To_Delta is < 0
  Timeval_Error : exception;
end Timeval;

