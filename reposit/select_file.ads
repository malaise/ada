with Afpx, Null_Procedure;
generic
  -- Any initialisation to do after descriptor activation
  with procedure Init_Procedure;
  -- Any action to do when a fd_event has been received (see Afpx Fd_Event)
  with procedure Fd_Callback is Null_Procedure;
  -- Any action to do when a timer_event has occured (see Afpx Timer_Event)
  with procedure Timer_Callback is Null_Procedure;
  -- Any action to do when a signal_event has occured (see Afpx Signal_Event)
  with procedure Signal_Callback is Null_Procedure;

function Select_File (Descriptor : Afpx.Descriptor_Range;
                      Current_File : String;
                      For_Read : Boolean) return String;


