with Players, Game, Image;

package File is

  Max_File_Name_Len : constant := 512;

  -- File manipulation error
  File_Error : exception;

  -- File interpretation error
  Format_Error : exception renames Image.Value_Error;

  -- Open / Create a file for reading then saving actions
  -- File_Error if file exists and cannot be open
  -- or file cannot be created
  -- Format_Error if first significant line is too long
  procedure Open (File_Name : in String);

  -- Delete a file if it exists
  procedure Delete (File_Name : in String);

  -- Read next move (white then black then white...)
  -- Returns a not valid action at end of file
  -- File_error on IO error;
  -- Format_Error if invalid format
  function Read return Players.Action_Rec;

  -- After all movements have been read (otherwise File_Error)
  -- Append new movement (or "- " if not valid)
  procedure Write (Action : in Players.Action_Rec;
                   Result : in Game.Move_Status_List);

  -- At the end
  procedure Close;

end File;
 
