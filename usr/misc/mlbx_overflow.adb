-- Overflow management in MLBX
-- First delay (No=1) is 10 us
-- Next is Prev + 10 * 5 ** (No-1)
with My_Io, Normal;
procedure Mlbx_Overflow is

  subtype Retry_Range is Natural range 1 .. 11;
  Current_Delay, Previous_Delay : Natural;
  Sum_Delay : Natural;

  function Next_Delay (No_Try : Natural;
                       Prev_Delay : Natural) return Natural is
  begin
    return Prev_Delay + 10 * (5 ** No_Try);
  end Next_Delay;

begin
  Previous_Delay := 0;
  Sum_Delay := 0;

  for I in Retry_Range loop
    Current_Delay := Next_Delay (I - 1, Previous_Delay);
    Sum_Delay := Sum_Delay + Current_Delay;
    My_Io.Put_Line ("Try no: " & Normal (I, 2)
                  & ", this try delay: " & Normal (Current_Delay, 9) & " us"
                  & "; Total delay: " & Normal (Sum_Delay, 9) & " us");
    Previous_Delay := Current_Delay;
  end loop;
end Mlbx_Overflow;

