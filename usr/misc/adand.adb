-- Source file indenter. See procedure USAGE.
with Text_Io;

with Sys_Calls;

with My_Io, Text_Handler, Argument;

procedure Adand is
  Line_Deb, Line_Fin : Positive;
  L : Natural;

  subtype Indent_Range is Integer range -12 .. +12;
  Ind : Indent_Range;

  File_Name : Text_Handler.Text(1024);
  Sav_Suf : constant String := ".bak";
  File_Suf : Text_Handler.Text(1024);

  Str_Max : constant := 500;
  Str : String(1..Str_Max+1);
  Lst : Natural;

  Tld, Tlf, Ti : Text_Handler.Text(10);

  F, Fb : Text_Io.File_Type;

  System_Call_Error : exception;

  procedure Usage is
  begin
    My_Io.Put_Line ("Usage: indent "
     & "[-F]file_name [-ffirst_line] [-llast_line] [-i[+|-]col]");
  end Usage;


begin

  -- parse arguments (file_name, lines, indentation)
  begin
    Argument.Get_Parameter(File_Name, 1, Argument.Not_Key);
  exception
    when Argument.Argument_Not_Found =>

      begin
        Argument.Get_Parameter(File_Name, 1, "F");
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
    Argument.Get_Parameter(Tld, 1, "f");
    Line_Deb := Positive'Value(Text_Handler.Value(Tld));
  exception
    when Argument.Argument_Not_Found =>
      Line_Deb := 1;
    when Constraint_Error | Numeric_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter(Tlf, 1, "l");
    Line_Fin := Positive'Value(Text_Handler.Value(Tlf));
  exception
    when Argument.Argument_Not_Found =>
      Line_Fin := Positive'Last;
    when Constraint_Error | Numeric_Error =>
      Usage;
      raise;
  end;

  begin
    Argument.Get_Parameter(Ti, 1, "i");
    Ind := Indent_Range'Value(Text_Handler.Value(Ti));
  exception
    when Argument.Argument_Not_Found =>
      Ind := 2;
    when Constraint_Error | Numeric_Error =>
      Usage;
      raise;
  end;

  -- mv file to file.bak
  declare
    No_Err : Boolean;
  begin
    -- build .BAK file name
    Text_Handler.Set (File_Suf, File_Name);
    Text_Handler.Append (File_Suf, Sav_Suf);

    -- eventualy remove .bak file
    No_Err := Sys_Calls.Unlink (Text_Handler.Value(File_Suf));
    -- rename file to file.bak
    No_Err := Sys_Calls.Rename (Text_Handler.Value(File_Name),
            Text_Handler.Value(File_Suf));
    if not No_Err then
      raise System_Call_Error;
    end if;
  exception
    when System_Call_Error =>
      My_Io.Put_Line ("ERROR : " & Sys_Calls.Str_Error(Sys_Calls.Errno)
       & " renaming file "
       & Text_Handler.Value(File_Name) & " to "
       & Text_Handler.Value(File_Suf));
      raise;
    when Constraint_Error =>
      My_Io.Put_Line ("File name too long to build commands.");
      raise;
  end;

  -- open file.bak file and create file
  begin
    Text_Io.Open (Fb, Text_Io.In_File,
     Text_Handler.Value(File_Suf));
  exception
    when others =>
      My_Io.Put_Line ("Error opening file " &
       Text_Handler.Value(File_Suf));
      raise;
  end;
  begin
    Text_Io.Create (F, Text_Io.Out_File,
     Text_Handler.Value(File_Name));
  exception
    when others =>
      My_Io.Put_Line ("Error creating file " &
       Text_Handler.Value(File_Name));
  end;

  L := 0;
  loop
    -- read file.bak line
    Text_Io.Get_Line (Fb, Str, Lst);
    L := L + 1;
    if Lst /= 0 and then L>= Line_Deb and then L<=Line_Fin then
      -- if ld<=line<=lf and non empty then indent
      if Ind > 0 then
        if Lst + Ind <= Str_Max then
          Str (Ind+1 .. Ind+Lst) := Str (1 .. Lst);
          Str (1 .. Ind) := (others => ' ');
          Lst := Lst + Ind;
        end if;
      elsif Ind < 0 then
        declare
          Mind : constant Positive := - Ind;
          Spaces : constant String (1..Mind) := (others => ' ');
        begin
          if Lst >= Mind and then Str (1 .. Mind) = Spaces then
            Str (1 .. Lst - Mind) := Str (Mind+1 .. Lst);
            Lst := Lst - Mind;
          end if;
        end;
      end if;
    end if;

    -- write line in file
    Text_Io.Put_Line (F, Str(1..Lst));

    exit when Text_Io.End_Of_File(Fb);

  end loop;
  Text_Io.New_Line (F);

  -- close files
  Text_Io.Close (Fb);
  Text_Io.Close (F);

  My_Io.Put_Line ("Done.");

exception
  when others =>
    My_Io.Put_Line ("Exception "
     & "raised when processing file "
     & Text_Handler.Value (File_Name));
    raise;
end Adand;

