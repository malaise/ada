with As.U, Sys_Calls, Str_Util, Environ;
package body Files is

  -- File names
  Spec_Suffix : constant String := ".ads";
  Body_Suffix :  constant String := ".adb";
  Body_File_Name : As.U.Asu_Us;

  -- Environ STUB_KEEP_ON_ERROR
  Keep_Name : constant String := "ASTUB_KEEP_ON_ERROR";

  -- Open --
  procedure Open (Spec_File_Name : in String;
                  Delete_Body : in Boolean) is
  begin
    -- Check that spec file ends with spec suffix
    if Spec_File_Name'Length <= Spec_Suffix'Length
    or else Str_Util.Extract (
       Spec_File_Name, Spec_Suffix'Length, False) /= Spec_Suffix then
      raise In_Error;
    end if;

    -- Open In file for Text_Char
    begin
      In_File.Open_All (Spec_File_Name);
    exception
      when Text_Char.Name_Error =>
        raise In_Error;
    end;

    -- Create Out file for Text_line
    Body_File_Name := As.U.Tus (
          Str_Util.Cut (Spec_File_Name, Spec_Suffix'Length, False)
        & Body_Suffix);

    -- Check if body file exists and delete it if requested
    begin
      if Sys_Calls.File_Check (Body_File_Name.Image)
      and then Delete_Body then
        if not Sys_Calls.Unlink (Body_File_Name.Image) then
          raise Sys_Calls.Access_Error;
        end if;
      end if;
    exception
      when Sys_Calls.Access_Error =>
        -- Raised by File_Check or failure of Unlink
        Body_File_Name.Set_Null;
        Close (Remove);
        raise Out_Error;
    end;

    -- Check that Out file does not exist
    begin
      Out_File.Open_All (Text_Line.In_File, Body_File_Name.Image);
      -- File already exists
      Out_File.Close_All;
      Body_File_Name.Set_Null;
      Close (Remove);
      raise Out_Error;
    exception
      when Text_Line.Name_Error =>
         -- Ok, Out file does not exist
         null;
    end;

    -- Create Out file
    begin
      Out_File.Create_All (Body_File_Name.Image);
    exception
      when Text_Line.Name_Error =>
        Body_File_Name.Set_Null;
        Close (Remove);
        raise Out_Error;
    end;

  end Open;

  -- Close --
  -- Keep it (success)
  -- Remove_If_Not_Keep (error)
  -- Remove (success empty)
  -- type Result_Action_List is (Keep, Remove_If_Not_Keep, Remove);
  -- Close files
  procedure Close (Action : in Result_Action_List) is
  begin
    -- Close Out file if it is open
    if Out_File.Is_Open then
      Out_File.Close_All;
    end if;
    -- Close In file if it is open
    if In_File.Is_Open then
      In_File.Close_All;
    end if;

    -- Delete result if it was created
    -- and if needed
    if Body_File_Name.Length /= 0
    and then (Action = Remove
        or else (Action = Remove_If_Not_Keep
            and then not Environ.Is_Yes (Keep_Name)) ) then
      Sys_Calls.Unlink (Body_File_Name.Image);
    end if;

  end Close;

end Files;

