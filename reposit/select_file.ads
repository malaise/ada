with Afpx;
generic
  -- Any initialisation to do after descriptor activation
  with procedure Init_Procedure is null;
  -- Any action to do when a fd_event has been received (see Afpx Fd_Event)
  with procedure Fd_Callback is null;
  -- Any action to do when a timer_event has occured (see Afpx Timer_Event)
  with procedure Timer_Callback is null;
  -- Any action to do when a signal_event has occured (see Afpx Signal_Event)
  with procedure Signal_Callback is null;
  -- Title when reading or writting
  Read_Title  : in String := "";
  Write_Title : in String := "";

-- Handle the file selection screen
-- Using the provided Afpx descriptor
-- Fills text input field with the provided Current_File (if not too long)
-- Display title according to For_Read and the provided titles
--  If titles are empty, display default title
function Select_File (Descriptor   : Afpx.Descriptor_Range;
                      Current_File : String;
                      For_Read     : Boolean) return String;

