with Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Basic_Proc, Xml_Parser;
procedure Dtd_Checker is

  -- Ada.Strings.Unbounded and Ada.Exceptions re-definitions
  package Asu renames Ada.Strings.Unbounded;

  -- Xml Parser dtd
  Dtd : Xml_Parser.Dtd_Type;
  Error_Msg : Asu.Unbounded_String;

  -- Program help
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
 "Usage: " & Argument.Get_Program_Name & " [ -w | --warnings ] <dtd_file>");
    Basic_Proc.Put_Line_Error ("  Default: only checks for errors on stdin.");
  end Usage;

  Abort_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Abort_Error;
  end Error;

  Warnings : Boolean := False;
  File_Pos : Natural := 1;

  use type Asu.Unbounded_String;
begin
  -- Parse arguments
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Nbre_Arg > 2 then
    Error ("Invalid arguments");
  end if;

  if Argument.Get_Parameter (1) = "-h"
  or else Argument.Get_Parameter (1) = "--help" then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  if Argument.Get_Parameter (1) = "-w"
  or else Argument.Get_Parameter (1) = "--warnings" then
    Warnings := True;
    if Argument.Get_Nbre_Arg > 1 then
      File_Pos := 2;
    end if;
  end if;

  if Argument.Get_Nbre_Arg /= File_Pos then
    Error ("Invalid arguments");
  end if;

  -- Parse and check Dtd
  begin
    Xml_Parser.Parse_Dtd_File (Argument.Get_Parameter (File_Pos),
                                Warnings, Dtd, Error_Msg);
  exception
    when Xml_Parser.File_Error =>
      Error ("File " & String'(Argument.Get_Parameter (File_Pos))
                     & " not found.");
  end;

  if Error_Msg /= Asu.Null_Unbounded_String then
    Basic_Proc.Put_Line_Error (Asu.To_String (Error_Msg));
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
