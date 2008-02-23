with Normal, Sys_Calls;
package body Temp_File  is

  -- From man 2 open (O_EXCL):
  -- The solution for performing atomic file locking using a lockfile is to
  -- create a unique file on the same file system (e.g., incorporating
  -- hostname and pid), use link(2) to make a link to the lockfile.
  -- If link() returns 0, the lock is successful. Otherwise, use stat(2)
  -- on the unique file to check if its link count has increased to 2,
  -- in which case the lock is also successful.

  -- From man 2 link:
  -- BUGS: On NFS file systems, the return code may be wrong in case the NFS
  -- server performs the link creation and dies before it can say so. Use
  -- stat(2) to find out if the link got created.


  -- We check if Temp_File.000 exists, if yes we skip to next (Temp_File.001)
  -- If not, we create Temp_File_Tmp.000 and a hard link to Temp_File.000
  -- If the link succeeds, then it is OK, we can remove Temp_File_Tmp.000
  -- If the link fails with Name_Error, we skip to next file
  -- If the link fails another way, it's a problem of file system, directory...

  -- Remove a file is possible, no error
  procedure Remove (File_Name : in String) is
    Dummy : Boolean;
  begin
    Dummy := Sys_Calls.Unlink (File_Name);
  end Remove;


  -- Possible suffixes, tried one after another
  subtype Suffix_Range is Natural range 000 .. 999;

  -- Try with this suffix. Return the successful file name or ""
  function Try (Path : String; Suffix : Suffix_Range) return String is
    Tfn : constant String := Path & '/' & "Temp_File_Tmp."
                                        & Normal (Suffix, 3, Gap => '0');
    Fn : constant String := Path & '/' & "Temp_File."
                                 & Normal (Suffix, 3, Gap => '0');
    Fd : Sys_Calls.File_Desc;
  begin
    -- Check if that Tmp file exists
    case Sys_Calls.File_Status (Fn) is
      when Sys_Calls.Found =>
        -- File already exists, try next
        return "";
      when Sys_Calls.Not_Found =>
        -- File does not exist, good
        null;
      when Sys_Calls.Error =>
        -- Other problem accessing the file, bad
        raise Invalid_Dir;
    end case;

    -- Try to create a Tmp_tmp file
    begin
      Fd := Sys_Calls.Create (Tfn);
      -- File created, good
      Sys_Calls.Close (Fd);
    exception
      when others =>
        -- Creation failed, bad
        raise Invalid_Dir;
    end;
    -- From now we will need to remove Tfn in any case
    -- Maybe several competitors will try to remove the same Tmp_tmp
    --  but what is important is that at least one (the first) succeeds

    -- Try to make a hard link from Tmp_tmp to Tmp
    -- This is atomic
    begin
      Sys_Calls.Link (Tfn, Fn, Hard => True);
      -- Link succeeded, Fn is unique
      Remove (Tfn);
      return Fn;
    exception
      when Sys_Calls.Name_Error =>
        -- Fn now already exists, try next
        Remove (Tfn);
        return "";
      when Sys_Calls.Access_Error =>
        -- Link raised Access_Error, but maybe succeeded
        --  (possible if nfs server crashes, see man link)
        Remove (Tfn);
        if Sys_Calls.File_Found (Fn) then
          -- Link probably worked but exclusive acces is not guaranteed
          -- Take the risk of a dandling orphan Tmp_tmp file.
          return "";
        else
          raise Invalid_Dir;
        end if;
    end;
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

