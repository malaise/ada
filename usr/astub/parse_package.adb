with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End, Parse_Type,
     Parse_Procedure, Parse_Function, Parse_Task, Parse_Protected,
     Put_Comments;

procedure Parse_Package (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;

  -- Put current "package body <name> is" and comments read ahead, once
  -- Because called due to a keyword (procedure/function...)
  -- Re-indent for next word
  Body_Put : Boolean := False;
  procedure Put_Body (Sub_Level : in Boolean := True) is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Asu.To_String (Name) & " is", False);
      Put_Comments (Level + 1);
      -- Re-indent for next word
      if Sub_Level then
        Output.Put ("", False, Level + 1);
      else
        Output.Put ("", False, Level);
      end if;
      Body_Put := True;
    end if;
  end Put_Body;

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
      if Lexic = Ada_Parser.Comment
      or else Lexic = Ada_Parser.Separator then
        if Body_Put then
          -- Within the package, Output comment or separator
          Output.Put (Str, False);
        elsif Lexic = Ada_Parser.Comment then
          -- Not knowing yet if this is a real package
          -- Save comments only
          Words.Add (Ada_Parser.Comment, Text);
        end if;
      elsif Str = "is" then
        -- Skip "is"
        null;
      elsif Str = "end" then
        -- End of this package
        -- Put "package body <Name> is" if needed
        Put_Body (False);
        exit;
      elsif Str = "package" then
        Put_Body (True);
        Parse_Package (Level + 1);
      elsif Str = "procedure" then
        -- Put name is this is this is the first statement
        --   after "package <name> is"
        Put_Body (True);
        Parse_Procedure (Level + 1);
      elsif Str = "function" then
        Put_Body (True);
        Parse_Function (Level + 1);
      elsif Str = "task" then
        Put_Body (True);
        Parse_Task (Level + 1);
      elsif Str = "protected" then
        Put_Body (True);
        Parse_Protected (Level + 1);
      elsif Str = "private" then
        Put_Body (False);
        -- Put "private" as a comment
        Output.Put_Line (Str, True);
      elsif Str = "renames" then
        -- This package is in fact a renaming declaration
        -- Reset to "package <name> renames" and put as comment
        Parse_To_End (Ada_Parser.Delimiter, ";");
        Output.Put ("package " & Asu.To_String (Name)
                  & " renames" & Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Parse_To_End (Ada_Parser.Delimiter, ";");
        Output.Put ("package " & Asu.To_String (Name)
                  & " is new" & Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "type"
      or else Str = "for" then
        -- Type or representation clause
        Words.Add (Lexic, Text);
        Put_Body (True);
        Parse_Type (Level + 1);
      else
        Put_Body (True);
        -- Unexpected, word. Parse to end as comment
        Words.Add (Lexic, Text);
        Parse_To_End (Ada_Parser.Delimiter, ";");
        Output.Put (Words.Concat, True, Level);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (Ada_Parser.Delimiter, ";");
  Put_Comments (Level);
  Words.Reset;

  -- end <name>;
  Output.Put ("end " & Asu.To_String (Name) & ";", False);
end Parse_Package;

