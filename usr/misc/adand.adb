-- Source file indenter. See procedure Usage.
with Ada.Exceptions;
with As.U, Basic_Proc, Sys_Calls, Argument, Text_Line;

procedure Adand is
  Line_Deb, Line_Fin : Positive;
  L : Natural;

  subtype Indent_Range is Integer range -12 .. +12;
  Ind : Indent_Range;

  File_Name : As.U.Asu_Us;
  Sav_Suf : constant String := ".bak";
  File_Suf : As.U.Asu_Us;

  Str, Pad : As.U.Asu_Us;

  Tld, Tlf, Ti : As.U.Asu_Us;

  F, Fb : Text_Line.File_Type;

  System_Call_Error : exception;
  use type As.U.Asu_Us;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: indent "
     & "[-F]file_name [-ffirst_line] [-llast_line] [-i[+|-]col]");
  end Usage;


begin

  -- parse arguments (file_name, lines, indentation)
  begin
    Argument.Get_Parameter (File_Name, 1, Argument.Not_Key);
  exception
    when Argument.Argument_Not_Found =>

      begin
        Argument.Get_Parameter (File_Name, 1, "F");
      exception
        when Argument.Argument_Not_Found =>
          Basic_Proc.Put_Line_Error ("ERROR: Missing file name");
          Usage;
          raise;
        when Constraint_Error =>
          Basic_Proc.Put_Line_Error ("ERROR: File name too long to store.");
          raise;
      end;

    when Constraint_Error =>
      Basic_Proc.Put_Line_Error ("ERROR: File name too long to store.");
      raise;
  end;

  begin
    Argument.Get_Parameter (Tld, 1, "f");
    Line_Deb := Positive'Value (Tld.Image);
  exception
    when Argument.Argument_Not_Found =>
      Line_Deb := 1;
    when Constraint_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter (Tlf, 1, "l");
    Line_Fin := Positive'Value (Tlf.Image);
  exception
    when Argument.Argument_Not_Found =>
      Line_Fin := Positive'Last;
    when Constraint_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter (Ti, 1, "i");
    Ind := Indent_Range'Value (Ti.Image);
  exception
    when Argument.Argument_Not_Found =>
      Ind := 2;
    when Constraint_Error =>
      Usage;
      raise;
  end;

  -- mv file to file.bak
  declare
    No_Err : Boolean;
  begin
    -- build .bak file name
    File_Suf := File_Name;
    File_Suf.Append (Sav_Suf);

    -- eventualy remove .bak file
    No_Err := Sys_Calls.Unlink (File_Suf.Image);
    -- rename file to file.bak
    No_Err := Sys_Calls.Rename (File_Name.Image, File_Suf.Image);
    if not No_Err then
      raise System_Call_Error;
    end if;
  exception
    when System_Call_Error =>
      Basic_Proc.Put_Line_Error ("ERROR: "
       & Sys_Calls.Str_Error (Sys_Calls.Errno)
       & " renaming file " & File_Name.Image & " to " & File_Suf.Image);
      raise;
    when Constraint_Error =>
      Basic_Proc.Put_Line_Error ("ERROR: File name too long to build commands.");
      raise;
  end;

  -- open file.bak file and create file
  begin
    Fb.Open_All (Text_Line.In_File, File_Suf.Image);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Error opening file " & File_Suf.Image);
      raise;
  end;
  begin
    F.Create_All (File_Name.Image);
  exception
    when others =>
    Basic_Proc.Put_Line_Error ("Error creating file " & File_Name.Image);
  end;

  Pad := abs(Ind) * ' ';
  L := 0;
  loop
    -- read file.bak line
    Str := Fb.Get;
    exit when Str.Is_Null;
    L := L + 1;
    if L >= Line_Deb and then L <= Line_Fin then
      -- if ld<=line<=lf and non empty then indent
      if Ind > 0 then
        Str := Pad & Str;
      elsif Ind < 0 then
        if Str.Length >= -Ind and then Str.Uslice (1, -Ind) = Pad then
          Str.Delete (1, -Ind);
        end if;
      end if;
    end if;

    -- write line in file
    F.Put (Str.Image);

  end loop;
  F.New_Line;

  -- close files
  Fb.Close_All;
  F.Close_All;

  Basic_Proc.Put_Line_Output ("Done.");

exception
  when Argument.Argument_Not_Found =>
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    Basic_Proc.Put_Line_Error ("Exception "
     & Ada.Exceptions.Exception_Name (Error)
     & " raised when processing file " & File_Name.Image);
    Basic_Proc.Set_Error_Exit_Code;
end Adand;

