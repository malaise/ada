with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Package;

procedure Parse_Context (Generated : out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  Level : Natural := 0;
  use type Ada_Parser.Lexical_Kind_List;
begin
  -- By default, nothing is generated
  Generated := False;
  -- Loop until expected word
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      if Str = "" then
        -- End of file
        exit;
      elsif Str = "package" then
        Level := 0;
        Parse_Package (0, Generated);
      elsif Str = "procedure" then
        Level := 0;
        Parse_Procedure (0, Generated);
      elsif Str = "function" then
        Level := 0;
        Parse_Function (0, Generated);
      elsif Str = "private" then
        -- Skip private prefix of package/procedure/function
        null;
      elsif Str = "generic" then
        -- Not terminated by ";"
        Output.Put (Str, True, 0);
      elsif Lexic = Ada_Parser.Separator
      or else Lexic = Ada_Parser.Comment then
        -- Put separators and comments unchanged
        Output.Put (Str, False, 0);
      else
        -- Unexpected, word (with, use, generic arguments...)
        -- Parse up to end of statement
        Words.Add (Lexic, Text);
        Parse_To_End (Ada_Parser.Delimiter, ";", 0);
        -- Put this statement as a comment
        Output.Put (Words.Concat, True, 0);
        Words.Reset;
      end if;
    end;
  end loop;

exception
  when Ada_Parser.End_Error =>
    Common.Error ("EOF");
end Parse_Context;

