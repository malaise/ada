with Ada.Text_Io, Ada.Characters.Latin_1;
with Argument, Text_Char, Text_Line, Sys_Calls, Ada_Parser, Mixed_Str;
procedure T_Ada_Parser is

  Fd : Sys_Calls.File_Desc;
  Ifile : Text_Char.File_Type;
  Ofile : Text_Line.File_Type;

  Line_Feed : constant String := Ada.Characters.Latin_1.Lf & "";

  procedure Callback (Text : in String;
                      Lexic : in Ada_Parser.Lexical_Kind_List) is
    Lex_Str : String (1 .. 17) := (others => ' ');
  begin
    declare
      Str : constant String := Mixed_Str (Lexic'Img);
    begin
      Lex_Str (1 .. Str'Length) := Str;
    end;
    if Text /= Line_Feed then
      Sys_Calls.Put_Line_Error (Lex_Str & ">" & Text & "<");
    else
      Sys_Calls.Put_Line_Error (Lex_Str & "><Line_Feed><");
    end if;
    Text_Line.Put (Ofile, Text);
  end Callback;

begin
  -- One arg
  if Argument.Get_Nbre_Arg /= 1 then
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <file_name>");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;

  -- Open file for Text_Line
  begin
    Fd := Sys_Calls.Open (Argument.Get_Parameter, Sys_Calls.In_File);
  exception
    when Sys_Calls.Name_Error =>
      Sys_Calls.Put_Line_Error ("Error: Cannot open file " &
          Argument.Get_Parameter);
      Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <file_name>");
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  -- This should work ok
  Text_Char.Open (Ifile, Fd);
  Text_Line.Open (Ofile, Text_Line.Out_File, Sys_Calls.Stdout);

  -- Parse
  Ada_Parser.Parse (Ifile, Callback'Unrestricted_Access);

  -- Done: close
  Text_Line.Close (Ofile);
  Text_Char.Close (Ifile);
  Sys_Calls.Close (Fd);

end T_Ada_Parser;

