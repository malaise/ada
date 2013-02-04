with Ada.Characters.Latin_1;
with Argument, Text_Char, Text_Line, Sys_Calls, Ada_Parser, Mixed_Str;
procedure T_Ada_Parser is

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
    Sys_Calls.Put_Line_Error ("Dumps parsed words on stdout"
                            & " and lexical elements on stderr.");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;

  -- Open file for Text_Line
  begin
    Ifile.Open_All (Argument.Get_Parameter);
  exception
    when Text_Char.Name_Error =>
      Sys_Calls.Put_Line_Error ("Error: Cannot open file " &
          Argument.Get_Parameter);
      Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " <file_name>");
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  -- Stdout
  Ofile.Open_All (Text_Line.Out_File, "");

  -- Parse
  Ada_Parser.Parse (Ifile, Callback'Access);

  -- Done: close
  Ofile.Close_All;
  Ifile.Close_All;

end T_Ada_Parser;

