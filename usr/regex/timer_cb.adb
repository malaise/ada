with Timers;
with Screen;
function Timer_Cb (Unused_Id : in Timers.Timer_Id;
                   Unused_Data : in Timers.Timer_Data) return Boolean is
begin
  -- Generate a Afpx Timer even if input has changed
  return Screen.Input_Changed;
end Timer_Cb;

