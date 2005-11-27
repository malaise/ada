with Normal, Sys_Calls;
package body Temp_File  is



  -- From man 2 open (O_EXCL):
  -- The solution for performing atomic file locking using a lockfile is to
  -- create a unique file on the same file system (e.g., incorporating
  -- hostname and pid), use link(2) to make a link to the lockfile.
  -- If link() returns 0, the lock is successful. Otherwise, use stat(2)
  -- on the unique file to check if its link count has increased to 2,
  -- in which case the lock is also successful.

  -- We check if Temp_File.000 exists, if yes we skip to next (Temp_File.001).
  -- If not, we create Temp_File_Tmp.000 and a hard link to Temp_File.000
  -- If the link succeeds, or else if if the number of links of
  -- Temp_File_Tmp.000 is 2, then it is OK, we can remove Temp_File_Tmp.000
  -- and close Temp_File.000

  --
  subtype Suffix_Range is Natural range 000 .. 999;

  -- Try with this suffix. Return the successful file name or ""
  function Try (Path : String; Suffix : Suffix_Range) return String is
    Tfn : constant String := Path & '/' & "Temp_File_Tmp."
                                        & Normal (Suffix, 3, Gap => '0');
    Fn : constant String := Path & '/' & "Temp_File."
                                 & Normal (Suffix, 3, Gap => '0');
    Fstat : Sys_Calls.File_Stat_Rec;
    Fd : Sys_Calls.File_Desc;
  begin
    -- Check if that file exists
    begin
      Fstat := Sys_Calls.File_Stat (Fn);
      -- This file already exists, try next
      return "";
    exception
      when Sys_Calls.Name_Error =>
        -- The file does not exist, good
        null;
      when Sys_Calls.Access_Error =>
        -- Other problem accessing the fail, bad
        raise Invalid_Dir;
    end ;
    -- Try to create a temporary temporary file
    begin
      Fd := Sys_Calls.Create (Tfn);
      -- File created. Close it.
      Sys_Calls.Close (Fd);
    exception
      when others =>
        -- Creation failed, bad
        raise Invalid_Dir;
    end;
    -- Try to make a hard link to it
    begin
      Sys_Calls.Link (Tfn, Fn, Hard => True);
      -- Link succeeded, Fn is unique
    exception
      when Sys_Calls.Name_Error =>
        -- Fn already exists now, try next
        return "";
      when Sys_Calls.Access_Error =>
        -- Link failed, but maybe the link could be done? Check
        if Sys_Calls.File_Stat (Fn).Nb_Links /= 2 then
          -- Link really failed
          return "";
        end if;
    end;
    -- It worked. Remove temp file
    if not Sys_Calls.Unlink (Tfn) then
      -- This should work
      raise Invalid_Dir;
    end if;
    return Fn;
  exception
    when Invalid_Dir =>
      raise;
    when others =>
      return "";
  end Try;

  function Create (Temp_Dir : String) return String is
    Fstat : Sys_Calls.File_Stat_Rec;
  begin
    -- Check that Temp_Dir exists, and is user RWX (rights >= 7)
    declare
      use type Sys_Calls.File_Kind_List;
    begin
      Fstat := Sys_Calls.File_Stat (Temp_Dir);
      if Fstat.Kind /= Sys_Calls.Dir or else Fstat.Rights < 7 then
        raise Invalid_Dir;
      end if;
    exception
      when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
        raise Invalid_Dir;
    end ;

    -- Try each of the 1000 possible files
    for I in Suffix_Range loop
      declare
        Str : constant String := Try (Temp_Dir, I);
      begin
        if Str /= "" then
          -- It worked
          return Str;
        end if;
      end;
    end loop;
    -- No success
    raise No_More_Temp;
  end Create;
end Temp_File;

