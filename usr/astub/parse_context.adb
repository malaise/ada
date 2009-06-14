with Ada.Strings.Unbounded;
with Common, Output, Words, Parser_Ada,
     Parse_To_End, Parse_Procedure, Parse_Function, Parse_Package, Fix_Comment;

procedure Parse_Context (Generated : out Boolean) is
  Word, Trash_Word : Words.Word_Rec;
  pragma Unreferenced (Trash_Word);
  Prev_Private : Boolean;
  use type Parser_Ada.Lexical_Kind_List;
begin
  -- By default, nothing is generated
  Generated := False;
  Prev_Private := False;
  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparse.Get (False);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Word.Text);
    begin
      if Str = "" then
        -- End of file
        exit;
      elsif Str = "package" then
        Output.New_Line;
        Parse_Package (0, Generated);
      elsif Str = "procedure" then
        Output.New_Line;
        Parse_Procedure (0, Generated);
      elsif Str = "function" then
        Output.New_Line;
        Parse_Function (0, Generated);
      elsif Str = "not" then
        -- Propagate [ not ] overriding
        Words.Add (Word);
      elsif Str = "overriding" then
        Words.Add (Word);
      elsif Str = "private" then
        -- Skip private prefix of package/procedure/function
        --   keep trace for "private with"
        Trash_Word := Parser_Ada.Multiparse.Get (False);
      elsif Str = "generic" then
        -- Not terminated by ";"
        Output.New_Line;
        Output.Put (Str, True, 0);
      elsif Word.Lexic = Parser_Ada.Separator
      or else Word.Lexic = Parser_Ada.Comment then
        -- Put separators and comments unchanged
        Output.Put (Str, False, 0);
      else
        -- Unexpected word ([ limited ] [ private ] with, use,
        --  generic arguments...)
        -- Parse up to end of statement
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", 0);
        Fix_Comment (0);
        -- Restore private if "private with"
        -- And put this statement as a comment
        if Str = "with" and then Prev_Private then
          Output.Put_Line ("private " & Words.Concat, True, 0);
        else
          Output.Put_Line (Words.Concat, True, 0);
        end if;
        Words.Reset;
      end if;
      -- Store if prev significant word was "private"
      if Word.Lexic /= Parser_Ada.Separator
      and then Word.Lexic /= Parser_Ada.Comment then
        Prev_Private := Str = "private";
      end if;
    end;
  end loop;

  -- Done
  Output.Flush;
exception
  when Parser_Ada.End_Error =>
    Common.Error ("EOF");
end Parse_Context;

