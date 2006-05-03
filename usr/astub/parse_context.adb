with Ada.Strings.Unbounded;
with Sys_calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Package;

procedure Parse_Context is
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  Level : Natural := 0;
  use type Ada_Parser.Lexical_Kind_List;
begin
  -- Loop until expected word
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      if Lexic = Ada_Parser.Comment then
        Output.Put_Line (Str, Level, False);
      elsif Str = "" then
        -- End of file
        exit;
      elsif Lexic = Ada_Parser.Separator then
        -- Skip separators
        null;
      elsif Str = "package" then
        Level := 0;
        Parse_Package (0);
      elsif Str = "procedure" then
        Level := 0;
        Parse_Procedure (0);
      elsif Str = "function" then
        Level := 0;
        Parse_Function (0);
      elsif Str = "private" then
        -- Skip private prefix of package/procedure/function
        null;
      elsif Str = "generic" then
        -- Put generic as a comment
        Output.Put_Line (Str, 0, True);
        -- Generic parameters will be put as unexpected and level 1
        -- Same for comments before unit
        Level := 1;
      else
        -- Unexpected, word. Parse to end as comment
        Words.Add (Text);
        Parse_To_End (Level, ";", True, True);
      end if;
    end;
  end loop;
exception
  when Ada_Parser.End_Error =>
    Sys_Calls.Put_Line_Error ("-->EOF<");
    raise Common.Syntax_Error;
end Parse_Context;

