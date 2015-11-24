-- Executes the procedure Do_In_Dir (Current_Path) in the current directory
--  and in each of its sub-directories

-- Do_In_Dir shall set
--  Result:
--    True: OK => Go on
--    False: Error => Stop (if Stop_On_Error, otherwise continue)
--  Go_On:
--    True: Go on recusive
--    False: Skip sub-directories of current


-- Display "==> complete_path <==" when entering each dir, or not
-- Do action in current directory also, or not
-- Stop after one level of subdirectories or not
-- Do action only in leaves (directory with no subdirectory) or not
-- Stop on error (Do_In_Dir returning False), or not
-- Follow symbolic links (to directories) or not
procedure Recurs (Do_In_Dir : access procedure (Path   : in String;
                                                Result : out Boolean;
                                                Go_On  : out Boolean);
                  Name_Of_Dir : in Boolean := True;
                  In_Current : in Boolean := True;
                  First_Level_Only : in Boolean := False;
                  Leaves_Only : in Boolean := False;
                  Stop_On_Error : in Boolean := True;
                  Follow_Links : in Boolean := False);

