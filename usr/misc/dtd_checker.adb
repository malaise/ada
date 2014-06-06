-- Check a dtd file
with Ada.Exceptions;
with As.U, Argument, Basic_Proc, Xml_Parser;
procedure Dtd_Checker is
  -- Current version
  Version : constant String := "V4.0";

  -- Xml Parser dtd
  File : As.U.Asu_Us;
  Dtd : Xml_Parser.Dtd_Type;
  Error_Msg : As.U.Asu_Us;

  -- Program help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                             & " [ -w | --warnings ] [ <dtd_file> ]");
    Basic_Proc.Put_Line_Error ("  Default: only checks for errors on stdin.");
    Basic_Proc.Put_Line_Error ("or " & Argument.Get_Program_Name
                             & "  -h | --help | -v | --version");
  end Usage;

  Abort_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Abort_Error;
  end Error;

  procedure Warning (Ctx : in  Xml_Parser.Ctx_Type; Msg : in String) is
  pragma Unreferenced (Ctx);
  begin
    Basic_Proc.Put_Line_Error (Msg);
  end Warning;
  Warnings : Xml_Parser.Warning_Callback_Access;


  File_Pos : Natural := 1;

begin
  -- Parse arguments
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter (1) = "-h"
    or else Argument.Get_Parameter (1) = "--help") then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter (1) = "-v"
    or else Argument.Get_Parameter (1) = "--version") then
    Basic_Proc.Put_Line_Output ("Parser version:      " & Xml_Parser.Version);
    Basic_Proc.Put_Line_Output ("Dtd_Checker version: " & Version);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter (1) = "-w"
    or else Argument.Get_Parameter (1) = "--warnings") then
    Warnings := Warning'Unrestricted_Access;
    File_Pos := 2;
  end if;

  if Argument.Get_Nbre_Arg = File_Pos then
    Argument.Get_Parameter (File, File_Pos);
  elsif Argument.Get_Nbre_Arg = File_Pos - 1 then
    File.Set_Null;
  else
    Error ("Invalid arguments");
  end if;

  -- Parse and check Dtd
  begin
    Xml_Parser.Parse_Dtd_File (File.Image,
                               Warnings, Dtd, Error_Msg);
  exception
    when Xml_Parser.File_Error =>
      Error ("File " & String'(Argument.Get_Parameter (File_Pos))
                     & " not found.");
  end;

  if not Error_Msg.Is_Null then
    Basic_Proc.Put_Line_Error (Error_Msg.Image);
    Basic_Proc.Set_Error_Exit_Code;
  end if;
exception
  when Abort_Error =>
    null;
  when Error:others =>
    -- Unexpected or internal error
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end Dtd_Checker;

