with As.U; use As.U;
with Sys_Calls, String_Mng, Environ;
package body Files is

  -- File names
  Spec_Suffix : constant String := ".ads";
  Body_Suffix :  constant String := ".adb";
  Body_File_Name : Asu_Us;

  -- Environ STUB_KEEP_ON_ERROR
  Keep_Name : constant String := "ASTUB_KEEP_ON_ERROR";

  -- Open --
  procedure Open (Spec_File_Name : in String;
                  Delete_Body : in Boolean) is
    Fd : Sys_Calls.File_Desc;
  begin
    -- Check that spec file ends with spec suffix
    if Spec_File_Name'Length <= Spec_Suffix'Length
    or else String_Mng.Extract (
       Spec_File_Name, Spec_Suffix'Length, False) /= Spec_Suffix then
      raise In_Error;
    end if;

    -- Open In file for Text_Char
    begin
      Fd := Sys_Calls.Open (Spec_File_Name, Sys_Calls.In_File);
    exception
      when Sys_Calls.Name_Error =>
        raise In_Error;
    end;
    -- This should work ok
    Text_Char.Open (In_File, Fd);

    -- Create Out file for Text_line
    Body_File_Name := Asu_Tus (
          String_Mng.Cut (Spec_File_Name, Spec_Suffix'Length, False)
        & Body_Suffix);

    -- Check if body file exists and delete it if requested
    begin
      if Sys_Calls.File_Check (Asu.To_String (Body_File_Name))
      and then Delete_Body then
        if not Sys_Calls.Unlink (Asu.To_String (Body_File_Name)) then
          raise Sys_Calls.Access_Error;
        end if;
      end if;
    exception
      when Sys_Calls.Access_Error =>
        -- Raised by File_Check or failure of Unlink
        Body_File_Name := Asu_Null;
        Close (Remove);
        raise Out_Error;
    end;

    -- Check that Out file does not exist
    begin
      Fd := Sys_Calls.Open (Asu.To_String (Body_File_Name), Sys_Calls.In_File);
      Sys_Calls.Close (Fd);
      Body_File_Name := Asu_Null;
      Close (Remove);
      raise Out_Error;
    exception
      when Sys_Calls.Name_Error =>
         -- Ok, Out file does not exist
         null;
    end;

    -- Create Out file
    begin
      Fd := Sys_Calls.Create (Asu.To_String (Body_File_Name));
    exception
      when Sys_Calls.Name_Error =>
        Body_File_Name := Asu_Null;
        Close (Remove);
        raise Out_Error;
    end;
    -- This should work ok
    Text_Line.Open (Out_File, Text_Line.Out_File, Fd);

  end Open;

  -- Close --
  -- Keep it (success)
  -- Remove_If_Not_Keep (error)
  -- Remove (success empty)
  -- type Result_Action_List is (Keep, Remove_If_Not_Keep, Remove);
  -- Close files
  procedure Close (Action : in Result_Action_List) is
    Fd : Sys_Calls.File_Desc;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    -- Close Out file if it is open
    if Text_Line.Is_Open (Out_File) then
      Fd := Text_Line.Get_Fd (Out_File);
      Text_Line.Close (Out_File);
      Sys_Calls.Close (Fd);
    end if;
    -- Close In file if it is open
    if Text_Char.Is_Open (In_File) then
      Fd := Text_Char.Get_Fd (In_File);
      Text_Char.Close (In_File);
      Sys_Calls.Close (Fd);
    end if;

    -- Delete result if it was created
    -- and if needed
    if Asu.Length (Body_File_Name) /= 0
    and then (Action = Remove
        or else (Action = Remove_If_Not_Keep
            and then not Environ.Is_Yes (Keep_Name)) ) then
      Dummy := Sys_Calls.Unlink (Asu.To_String (Body_File_Name));
    end if;

  end Close;

end Files;

