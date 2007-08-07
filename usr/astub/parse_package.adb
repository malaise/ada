with Ada.Strings.Unbounded;
with Text_Char;
with Common, Files, Output, Words, Parser_Ada, Parse_To_End, Parse_Type,
     Parse_Procedure, Parse_Function, Parse_Task, Parse_Protected,
     Put_Comments, Get_Separators, Fix_Comment;

procedure Parse_Package (Level : in Natural;
                         Generated : in out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name : Asu.Unbounded_String;
  Word : Parser_Ada.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List, Asu.Unbounded_String;

  -- Put current "package body <name> is" and comments read ahead
  -- Because called due to a keyword (procedure/function...)
  Body_Put : Boolean := False;
  procedure Put_Body is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Asu.To_String (Name) & " is",
                        False, Level, True);
      Output.New_Line;
      -- Put comments and line_feeds saved so far, keep separators in words
      Put_Comments;
      -- Remove any leading line_feed, just keep indent
      while String'(Words.Read (1)) = String'(Common.Line_Feed) loop
         Words.Del (1);
      end loop;
      Body_Put := True;
    end if;
  end Put_Body;

begin

  -- Get package name
  Parse_Name (File, Level, Name);
  Words.Reset;

  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Asu.To_String (Word.Text);
    begin
      if Word.Lexic = Parser_Ada.Comment then
        if Body_Put then
          -- Within the package, Output comments
          Output.Put (Words.Concat & Asu.To_String (Word.Text), False);
          Words.Reset;
        else
          -- Not knowing yet if this is a real package
          -- Save comments
          Words.Add (Parser_Ada.Comment, Word.Text);
        end if;
      elsif Word.Lexic = Parser_Ada.Separator then
        if Body_Put then
          -- Within the package, Output Line_Feed, save other separators
          if Word.Text = String'(Common.Line_Feed) then
            Output.New_Line;
            Words.Reset;
          else
            Words.Add (Word);
          end if;
        else
          -- Not knowing yet if this is a real package
          -- Save separators
          Words.Add (Word);
        end if;
      elsif Str = "is" then
        -- Skip "is"
        null;
      elsif Str = "end" then
        -- End of this package
        -- Put "package body <Name> is" if needed
        Put_Body;
        exit;
      elsif Str = "package" then
        Put_Body;
        Parse_Package (Level + 1, Generated);
      elsif Str = "procedure" then
        -- Put name is this is this is the first statement
        --   after "package <name> is"
        Put_Body;
        Parse_Procedure (Level + 1, Generated);
      elsif Str = "function" then
        Put_Body;
        Parse_Function (Level + 1, Generated);
      elsif Str = "task" then
        Put_Body;
        Parse_Task (Level + 1);
        Generated := True;
      elsif Str = "protected" then
        Put_Body;
        Parse_Protected (Level + 1);
        Generated := True;
      elsif Str = "private" then
        Put_Body;
        -- Put "private" as a comment
        Output.Put_Line (Str, True, Level);
      elsif Str = "renames" then
        -- This package is in fact a renaming declaration
        -- Reset to "package <name> renames" and put as comment
        Parse_To_End (Parser_Ada.Delimiter, ";", Level);
        Fix_Comment (Level);
        Output.Put_Line ("package " & Asu.To_String (Name)
                  & " renames" & Words.Concat, True, Level, True);
        Words.Reset;
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Parse_To_End (Parser_Ada.Delimiter, ";", Level);
        Fix_Comment (Level);
        Output.Put_Line ("package " & Asu.To_String (Name)
                  & " is new" & Words.Concat, True, Level, True);
        Words.Reset;
        return;
      elsif Str = "type"
      or else Str = "for" then
        -- Type or representation clause
        Put_Body;
        -- Reset indent
        Words.Add (Word);
        Parse_Type (Level + 1);
      elsif Str = "not" then
        -- Skip "not overriding" of function/procedure
        Put_Body;
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Reserved_Word, "overriding", Level + 1,
                      Up_To_Next_Significant => False);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level + 1, True);
        Words.Reset;
        Output.Put ("", False, Level + 1, True);
        Word := Parser_Ada.Multiparse.Get (True);
      elsif Str = "overriding" then
        -- Skip "overriding" of function/procedure
        Put_Body;
        Words.Add (Word);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level + 1, True);
        Words.Reset;
        Output.Put ("", False, Level + 1, True);
        Word := Parser_Ada.Multiparse.Get (True);
      else
        -- Unexpected word. Parse to end as comment
        Put_Body;
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
        Fix_Comment (Level + 1);
        Output.Put (Words.Concat, True, Level + 1, True);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  Put_Comments;
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level, True);
end Parse_Package;

