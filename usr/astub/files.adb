with Ada.Strings.Unbounded;
with Sys_Calls, String_Mng;
package body Files is

  package Asu renames Ada.Strings.Unbounded;

  Spec_Suffix : constant String := ".ads";
  Body_Suffix :  constant String := ".adb";
  Body_File_Name : Asu.Unbounded_String;

  In_Desc : Text_Char.File_Type;
  Out_Desc : Text_Line.File_Type;

  -- Open --
  procedure Open (Spec_File_Name : in String) is
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
    Text_Char.Open (In_Desc, Fd);

    -- Create Out file for Text_line
    Body_File_Name := Asu.To_Unbounded_String (
          String_Mng.Cut (Spec_File_Name, Spec_Suffix'Length, False)
        & Body_Suffix);
    -- Check that Out file does not exist
    begin
      Fd := Sys_Calls.Open (Asu.To_String (Body_File_Name), Sys_Calls.In_File);
      Sys_Calls.Close (Fd);
      Body_File_Name := Asu.Null_Unbounded_String;
      Close (False);
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
        Body_File_Name := Asu.Null_Unbounded_String;
        Close (False);
        raise Out_Error;
    end;
    -- This should work ok
    Text_Line.Open (Out_Desc, Text_Line.Out_File, Fd);

  end Open;

  -- In_File --
  function In_File return Text_Char.File_Type is
  begin
    return In_Desc;
  end In_File;

  -- Out_File --
  function Out_File return Text_Line.File_Type is
  begin
    return Out_Desc;
  end Out_File;

  -- Close --
  procedure Close (Success : in Boolean) is
    Fd : Sys_Calls.File_Desc;
    Dummy : Boolean;
  begin
    -- Close Out file if it is open
    if Text_Line.Is_Open (Out_Desc) then
      Fd := Text_Line.Get_Fd (Out_Desc);
      Text_Line.Close (Out_Desc);
      Sys_Calls.Close (Fd);
    end if;
    -- Close In file if it is open
    if Text_Char.Is_Open (In_Desc) then
      Fd := Text_Char.Get_Fd (In_Desc);
      Text_Char.Close (In_Desc);
      Sys_Calls.Close (Fd);
    end if;

    -- Delete result if failure
    if not Success
    and then Asu.Length (Body_File_Name) /= 0 then
--      Dummy := Sys_Calls.Unlink (Asu.To_String (Body_File_Name));
null;
    end if;

  end Close;

end Files;

