with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Type,
     Parse_Procedure, Parse_Function, Parse_Task, Parse_Protected,
     Put_Comments, Get_Separators;

procedure Parse_Package (Level : in Natural;
                         Generated : in out Boolean) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;

  -- Put current "package body <name> is" and comments read ahead, once
  -- Because called due to a keyword (procedure/function...)
  Body_Put : Boolean := False;
  procedure Put_Body is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Asu.To_String (Name) & " is",
                        False, Level);
      Output.New_Line;
      Put_Comments (Level + 1);
      -- Remove any leading line_feed, just keep indent
      while String'(Words.Read (1)) = String'(Common.Line_Feed) loop
         Words.Del (1);
      end loop;
      Body_Put := True;
    end if;
  end Put_Body;

  procedure Clear_Indent is
    Dummy : constant String := Get_Separators;
  begin
    null;
  end Clear_Indent;

begin

  -- Get package name
  Parse_Name (File, Level, Name);
  Words.Reset;

  -- Loop until expected word
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Asu.To_String (Text);
    begin
      if Lexic = Ada_Parser.Comment then
        if Body_Put then
          -- Within the package, Output comment
          Output.Put (Words.Concat & Str, False);
          Words.Reset;
        else
          -- Not knowing yet if this is a real package
          -- Save comments only
          Words.Add (Ada_Parser.Comment, Text);
        end if;
      elsif Lexic = Ada_Parser.Separator then
        if Str = Common.Line_Feed and then Body_Put then
          -- Put New line now if in package
          Output.New_Line;
          Words.Reset;
        else
          -- Save separator for later Output
          Words.Add (Ada_Parser.Separator, Text);
        end if;
      elsif Str = "is" then
        -- Skip "is"
        Words.Reset;
      elsif Str = "end" then
        -- End of this package
        -- Put "package body <Name> is" if needed
        Put_Body;
        Words.Reset;
        exit;
      elsif Str = "package" then
        Put_Body;
        Clear_Indent;
        Parse_Package (Level + 1, Generated);
      elsif Str = "procedure" then
        -- Put name is this is this is the first statement
        --   after "package <name> is"
        Put_Body;
        Clear_Indent;
        Parse_Procedure (Level + 1, Generated);
      elsif Str = "function" then
        Put_Body;
        Clear_Indent;
        Parse_Function (Level + 1, Generated);
      elsif Str = "task" then
        Put_Body;
        Clear_Indent;
        Parse_Task (Level + 1);
        Generated := True;
      elsif Str = "protected" then
        Put_Body;
        Clear_Indent;
        Parse_Protected (Level + 1);
        Generated := True;
      elsif Str = "private" then
        Put_Body;
        -- Put "private" as a comment
        Clear_Indent;
        Output.Put_Line (Str, True, Level);
      elsif Str = "renames" then
        -- This package is in fact a renaming declaration
        -- Reset to "package <name> renames" and put as comment
        Parse_To_End (Ada_Parser.Delimiter, ";", Level);
        Output.Put ("package " & Asu.To_String (Name)
                  & " renames" & Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Parse_To_End (Ada_Parser.Delimiter, ";", Level);
        Output.Put ("package " & Asu.To_String (Name)
                  & " is new" & Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "type"
      or else Str = "for" then
        -- Type or representation clause
        Put_Body;
        Words.Add (Lexic, Text);
        Parse_Type (Level + 1);
      else
        -- Unexpected word. Parse to end as comment
        Put_Body;
        Clear_Indent;
        Words.Add (Lexic, Text);
        Parse_To_End (Ada_Parser.Delimiter, ";", Level + 1);
        Output.Put (Words.Concat, True, Level + 1);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Ada_Parser.Delimiter, ";", Level);
  Put_Comments (Level);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Package;

