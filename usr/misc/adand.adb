-- Source file indenter. See procedure Usage.
with Ada.Text_Io, Ada.Exceptions;
with As.U; use As.U;
with Sys_Calls, My_Io, Argument;

procedure Adand is
  Line_Deb, Line_Fin : Positive;
  L : Natural;

  subtype Indent_Range is Integer range -12 .. +12;
  Ind : Indent_Range;

  File_Name : Asu_Us;
  Sav_Suf : constant String := ".bak";
  File_Suf : Asu_Us;

  Str_Max : constant := 500;
  Str : String(1..Str_Max+1);
  Lst : Natural;

  Tld, Tlf, Ti : Asu_Us;

  F, Fb : Ada.Text_Io.File_Type;

  System_Call_Error : exception;

  procedure Usage is
  begin
    My_Io.Put_Line ("Usage: indent "
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
          Usage;
          raise;
        when Constraint_Error =>
          My_Io.Put_Line ("File name too long to store.");
          raise;
      end;

    when Constraint_Error =>
      My_Io.Put_Line ("File name too long to store.");
      raise;
  end;

  begin
    Argument.Get_Parameter (Tld, 1, "f");
    Line_Deb := Positive'Value (Asu_Ts (Tld));
  exception
    when Argument.Argument_Not_Found =>
      Line_Deb := 1;
    when Constraint_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter (Tlf, 1, "l");
    Line_Fin := Positive'Value (Asu_Ts (Tlf));
  exception
    when Argument.Argument_Not_Found =>
      Line_Fin := Positive'Last;
    when Constraint_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter (Ti, 1, "i");
    Ind := Indent_Range'Value (Asu_Ts (Ti));
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
    Asu.Append (File_Suf, Sav_Suf);

    -- eventualy remove .bak file
    No_Err := Sys_Calls.Unlink (Asu_Ts (File_Suf));
    -- rename file to file.bak
    No_Err := Sys_Calls.Rename (Asu_Ts (File_Name), Asu_Ts (File_Suf));
    if not No_Err then
      raise System_Call_Error;
    end if;
  exception
    when System_Call_Error =>
      My_Io.Put_Line ("ERROR : " & Sys_Calls.Str_Error (Sys_Calls.Errno)
       & " renaming file " & Asu_Ts (File_Name) & " to " & Asu_Ts (File_Suf));
      raise;
    when Constraint_Error =>
      My_Io.Put_Line ("File name too long to build commands.");
      raise;
  end;

  -- open file.bak file and create file
  begin
    Ada.Text_Io.Open (Fb, Ada.Text_Io.In_File, Asu_Ts (File_Suf));
  exception
    when others =>
      My_Io.Put_Line ("Error opening file " & Asu_Ts (File_Suf));
      raise;
  end;
  begin
    Ada.Text_Io.Create (F, Ada.Text_Io.Out_File, Asu_Ts (File_Name));
  exception
    when others =>
      My_Io.Put_Line ("Error creating file " & Asu_Ts (File_Name));
  end;

  L := 0;
  loop
    -- read file.bak line
    Ada.Text_Io.Get_Line (Fb, Str, Lst);
    L := L + 1;
    if Lst /= 0 and then L >= Line_Deb and then L<=Line_Fin then
      -- if ld<=line<=lf and non empty then indent
      if Ind > 0 then
        if Lst + Ind <= Str_Max then
          Str (Ind + 1 .. Ind + Lst) := Str (1 .. Lst);
          Str (1 .. Ind) := (others => ' ');
          Lst := Lst + Ind;
        end if;
      elsif Ind < 0 then
        declare
          Mind : constant Positive := - Ind;
          Spaces : constant String (1 .. Mind) := (others => ' ');
        begin
          if Lst >= Mind and then Str (1 .. Mind) = Spaces then
            Str (1 .. Lst - Mind) := Str (Mind + 1 .. Lst);
            Lst := Lst - Mind;
          end if;
        end;
      end if;
    end if;

    -- write line in file
    Ada.Text_Io.Put_Line (F, Str(1 .. Lst));

    exit when Ada.Text_Io.End_Of_File (Fb);

  end loop;
  Ada.Text_Io.New_Line (F);

  -- close files
  Ada.Text_Io.Close (Fb);
  Ada.Text_Io.Close (F);

  My_Io.Put_Line ("Done.");

exception
  when Error:others =>
    My_Io.Put_Line ("Exception " & Ada.Exceptions.Exception_Name (Error)
     & " raised when processing file " & Asu_Ts (File_Name));
    raise;
end Adand;

