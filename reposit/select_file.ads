with Afpx, Null_Procedure;
generic
  -- Any initialisation to do after descriptor activation
  with procedure Init_Procedure;
  -- Any action to do when a fd_event has been received (see Afpx Fd_event)
  with procedure Fd_Callback is Null_Procedure;
  -- Any action to do when a Timer_event has occured (see Afpx Fd_event)
  with procedure Timer_Callback is Null_Procedure;

function Select_File (Descriptor : Afpx.Descriptor_Range;
                      Current_File : String;
                      For_Read : Boolean) return String;


