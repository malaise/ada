with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada,
     Parse_To_End, Parse_Procedure, Parse_Function, Parse_Package;

procedure Parse_Context (Generated : out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  Word : Words.Word_Rec;
  Level : Natural := 0;
  use type Parser_Ada.Lexical_Kind_List;
begin
  -- By default, nothing is generated
  Generated := False;
  -- Loop until expected word
  loop
    Word := Parser_Ada.MultiParse.Get (False);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      if Str = "" then
        -- End of file
        exit;
      elsif Str = "package" then
        Level := 0;
        Output.New_Line;
        Parse_Package (0, Generated);
      elsif Str = "procedure" then
        Level := 0;
        Output.New_Line;
        Parse_Procedure (0, Generated);
      elsif Str = "function" then
        Level := 0;
        Output.New_Line;
        Parse_Function (0, Generated);
      elsif Str = "private" then
        -- Skip private prefix of package/procedure/function
        null;
      elsif Str = "generic" then
        -- Not terminated by ";"
        Output.New_Line;
        Output.Put (Str, True, 0);
      elsif Word.Lexic = Parser_Ada.Separator
      or else Word.Lexic = Parser_Ada.Comment then
        -- Put separators and comments unchanged
        Output.Put (Str, False, 0);
      else
        -- Unexpected, word (with, use, generic arguments...)
        -- Parse up to end of statement
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", 0);
        -- Put this statement as a comment
        Output.Put (Words.Concat, True, 0);
        Words.Reset;
      end if;
    end;
  end loop;

exception
  when Parser_Ada.End_Error =>
    Common.Error ("EOF");
end Parse_Context;

