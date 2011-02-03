-- Executes the procedure Do_In_Dir in the current directory and
--  in each directory of the sub-tree

-- Display "==> complete_path <==" when entering each dir, or not
-- Do action in current directory also, or not
-- Stop after one level of subdirectories or not
-- Do action only in leaves (directory with no subdirectory) or not
-- Stop on error (Do_In_Dir returning False), or not
-- Follow symbolic links (to directories) or not
generic
  with function Do_In_Dir return Boolean;
procedure Recurs (
 Name_Of_Dir : in Boolean := True;
 In_Current : in Boolean := True;
 First_Level_Only : in Boolean := False;
 Leaves_Only : in Boolean := False;
 Stop_On_Error : in Boolean := True;
 Follow_Links : in Boolean := False);

