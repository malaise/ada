with Con_Io;
with Nav_Data;
-- All the primitives to access the screen
package Nav_Screen is

  -- Where to go after a field is got
  subtype Movement is Con_Io.Curs_Mvt;

  -- Result of get of action to do
  type Action is (Compute, Quit, Help, Clear, Prev, Next, Refresh, Break);

  -- Clear all the screen
  procedure Reset;
  -- To write the title
  procedure Title;

  -- To put the mask (all fixed text around) for the get the problem and
  --  for the put of the result
  procedure Put_Mask;


  -- Get a problem data field
  procedure Get (Field : in Nav_Data.T_List_Data; Blink : in Boolean := False;
   Str : in out String; Pos : in out Positive; Insert : in out Boolean;
   Next : out Movement);
  -- Put the formated field when successfully got
  procedure Put (Field : in Nav_Data.T_List_Data; Str : in String;
   Blink : in Boolean := False);

  -- Put a field of the result
  procedure Put_Result (Field : in Nav_Data.T_List_Data; Str : in String);

  -- Draw a line of dots between field in got area and it in result area
  procedure Dot (Field : in Nav_Data.T_List_Data);
  -- Draw an arrow between a clear field in got area and the result
  procedure Arrow (Field : in Nav_Data.T_List_Data);
  -- Clears a line of dots or an arrow
  procedure Clear_Line (Field : in Nav_Data.T_List_Data);


  -- Get an action
  function Get_Action (Initial : Action) return Action;

  -- Displays the "wrong format" error message
  procedure Err_Format;
  -- Display an error adapted to the detected inconsistency of data
  --  (result of check)
  procedure Err_Check (Error : in Nav_Data.T_Consistency);
  -- Clears the error message
  procedure Clear_Err;

  -- Ask the operator wether he realy wants to quit
  function Confirm_Quit return Boolean;
  -- Displays the help screen, return false if Break
  function Put_Help return Boolean;

end Nav_Screen;
