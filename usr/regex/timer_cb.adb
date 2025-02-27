with Timers;
with Screen;
function Timer_Cb (Unused_Id : in Timers.Timer_Id;
                   Unused_Data : in Timers.Timer_Data) return Boolean is
begin
  -- Generate a Afpx Timer event if input has changed
  return Screen.Has_Cursor_Changed
      or Screen.Has_Input_Changed; --## rule line off Andor_Boolean

end Timer_Cb;

