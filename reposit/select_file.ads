-- Handle the file selection screen
-- Using the provided Afpx descriptor
-- Get_File:
--   Fill text input field with the provided Current_File
--   Display title according to For_Read and the provided titles
--    If titles are empty, display default title
--   Allow selection (in Get field) of a non existing file if not For_Read
--   Change directory transparently
--   Try or not to select Current_File if it is in list
--   Return "" if CANCEL
-- Report_Error:
--  Show message in message field until OK

-- Note that the descriptor can be freely specified but that the field numbers
--  in this descriptor are fixed (at least from 1 to 16), and that there are
--  some hidden constraints on their geometry
with Afpx;
generic
  -- The descriptor to use
  Descriptor : Afpx.Descriptor_Range;
  -- Any initialisation to do after descriptor activation
  with procedure Init_Procedure is null;
  -- Any action to do when a fd_event has been received (see Afpx Fd_Event)
  with procedure Fd_Callback is null;
  -- Any action to do when a timer_event has occured (see Afpx Timer_Event)
  with procedure Timer_Callback is null;
  -- Any action to do when a signal_event has occured (see Afpx Signal_Event)
  with procedure Signal_Callback is null;
  -- Title when reading or writting
  Read_Title  : in String := ""; -- Default: "Load a file"
  Write_Title : in String := ""; -- Default: "Save in a file"

package Select_File is
  -- On Ctrl C, or close window
  Exit_Requested : exception;

  -- Get file name
  -- Get field (text) is initialised with Current_File
  -- Title field is set according to For_Read (and Read_Title and Write_Title)
  -- If Select_Current, then try to set selected entry in list to Current_File
  --  and center it
  function Get_File (Current_File   : String;
                     For_Read       : Boolean;
                     Select_Current : Boolean) return String;

  -- Report an error
  procedure Report_Error (Message : in String);
end Select_File;

