with As.U;
with Common, Output, Words, Parser_Ada, Parse_To_End, Parse_Type,
     Parse_Procedure, Parse_Function, Parse_Task, Parse_Protected,
     Parse_Name, Put_Comments, Fix_Comment;

procedure Parse_Package (Level : in Natural;
                         Generated : in out Boolean) is
  Name : As.U.Asu_Us;
  Word : Parser_Ada.Word_Rec;
  use type As.U.Asu_Us, Parser_Ada.Lexical_Kind_List;

  -- Put current "package body <name> is" and comments read ahead
  -- Because called due to a keyword (procedure/function...)
  Body_Put : Boolean := False;
  procedure Put_Body is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Name.Image & " is",
                        False, Level, True);
      Output.New_Line;
      -- Put comments and line_feeds saved so far, keep separators in words
      Put_Comments;
      -- Remove any leading line_feed, just keep indent
      while As.U.Asu_Us'(Words.Read (1)) = As.U.Asu_Us'(Common.Line_Feed) loop
         Words.Del (1);
      end loop;
      Body_Put := True;
    end if;
  end Put_Body;

begin

  -- Get package name
  Parse_Name (Level, Name);
  Words.Reset;

  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparser.Get (True);
    declare
      Str : constant String := Word.Text.Image;
    begin
      if Word.Lexic = Parser_Ada.Comment then
        if Body_Put then
          -- Within the package, Output comments
          Output.Put (Words.Concat & Word.Text.Image, False);
          Words.Reset;
        else
          -- Not knowing yet if this is a real package
          -- Save comments
          Words.Add (Parser_Ada.Comment, Word.Text);
        end if;
      elsif Word.Lexic = Parser_Ada.Separator then
        if Body_Put then
          -- Within the package, Output Line_Feed, save other separators
          if Word.Text = As.U.Asu_Us'(Common.Line_Feed) then
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
      elsif Str = "with" then
        -- Skip aspect, until "is"
        loop
          Word := Parser_Ada.Multiparser.Get (True);
          exit when Word.Lexic = Parser_Ada.Reserved_Word
          and then Word.Text.Image = "is";
        end loop;
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
      elsif Str = "not" then
        Words.Add (Word);
      elsif Str = "overriding" then
        Words.Add (Word);
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
        Output.Put_Line ("package " & Name.Image
                  & " renames" & Words.Concat, True, Level, True);
        Words.Reset;
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Parse_To_End (Parser_Ada.Delimiter, ";", Level);
        Fix_Comment (Level);
        Output.Put_Line ("package " & Name.Image
                  & " is new " & Words.Concat, True, Level, True);
        Words.Reset;
        return;
      elsif Str = "type"
      or else Str = "for" then
        -- Type or representation clause
        Put_Body;
        -- Reset indent
        Words.Add (Word);
        Parse_Type (Level + 1);
      else
        -- Unexpected word. Parse to end as comment
        Put_Body;
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level + 1, True);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  Put_Comments;
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Name.Image & ";", False, Level, True);
end Parse_Package;

