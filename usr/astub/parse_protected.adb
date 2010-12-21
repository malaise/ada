with As.U; use As.U;
with Common, Output, Words, Parser_Ada, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Entry, Fix_Comment, Put_Comments;

procedure Parse_Protected (Level : in Natural) is
  Name : Asu_Us;
  Word : Parser_Ada.Word_Rec;
  Dummy : Boolean := True;
  use type Parser_Ada.Lexical_Kind_List;
begin

  -- Read until protected name, skip "type"
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Word.Text.Image;
    begin
      if Word.Lexic = Parser_Ada.Comment then
        -- Put comment
        Output.Put_Line (Str, False);
      elsif Str = "type" then
        -- Skip type
        null;
      elsif Word.Lexic = Parser_Ada.Identifier then
        -- Got protected name
        Name := Word.Text;
        exit;
      elsif Word.Lexic = Parser_Ada.Separator then
        -- Skip separators
        null;
      else
        -- Unexpected word
        Common.Error (Name.Image);
      end if;
    end;
  end loop;

  -- Output protected body <name> is
  Output.Put_Line (Words.Concat & "protected body " & Name.Image
                 & " is", False, Level);

  -- Skip until "is", put comments
  Parse_To_End (Parser_Ada.Reserved_Word, "is", Level);
  Words.Reset;

  -- Loop until expected word
  loop
    Word := Parser_Ada.Multiparse.Get (True);
    declare
      Str : constant String := Word.Text.Image;
    begin
      if Word.Lexic = Parser_Ada.Comment then
        Output.Put (Words.Concat & Word.Text.Image, False);
        Words.Reset;
      elsif Word.Lexic = Parser_Ada.Separator then
        -- Within the protected, Output Line_Feed, save other separators
        if Word.Text = Asu_Us'(Common.Line_Feed) then
          Output.Put_Line (Words.Concat, False);
          Words.Reset;
        else
          Words.Add (Word);
        end if;
      elsif Str = "end" then
        -- End of this protected
        -- Normally, Words contains only separators
        Words.Reset;
        exit;
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1, Dummy);
      elsif Str = "function" then
        Parse_Function (Level + 1, Dummy);
      elsif Str = "not" then
        Words.Add (Word);
      elsif Str = "overriding" then
        Words.Add (Word);
      elsif Str = "entry" then
        Parse_Entry (Level + 1);
      elsif Str = "private" then
        -- Put "private" as a comment
        -- Normally, Words contains only separators
        Words.Reset;
        Output.Put_Line (Str, True, Level, True);
      elsif Str = "new" then
        -- Parse until "with" as comment
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Reserved_Word, "with", Level + 1);
        Output.Put (Words.Concat, True, Level, True);
        Words.Reset;
      else
        -- Unexpected, word. Parse to end as comment
        Words.Add (Word);
        Parse_To_End (Parser_Ada.Delimiter, ";", Level + 1);
        Fix_Comment (Level + 1);
        Output.Put_Line (Words.Concat, True, Level, True);
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
end Parse_Protected;

