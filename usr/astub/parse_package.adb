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
  Body_Put : Boolean := False;
  procedure Put_Body is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Asu.To_String (Name) & " is",
                       False, Level);
      Put_Comments (Level + 1);
    end if;
    Body_Put := True;
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
        Put_Body;
        exit;
      elsif Str = "package" then
        Put_Body;
        Parse_Package (Level + 1);
      elsif Str = "procedure" then
        -- Put name is this is this is the first statement
        --   after "package <name> is"
        Put_Body;
        Parse_Procedure (Level + 1);
      elsif Str = "function" then
        Put_Body;
        Parse_Function (Level + 1);
      elsif Str = "task" then
        Put_Body;
        Parse_Task (Level + 1);
      elsif Str = "protected" then
        Put_Body;
        Parse_Protected (Level + 1);
      elsif Str = "private" then
        Put_Body;
        -- Put "private" as a comment
        Output.Put_Line (Str, True, Level);
      elsif Str = "renames" then
        -- This package is in fact a renaming declaration
        -- Reset to "package <name> renames" and put as comment
        Put_Comments (Level);
        Words.Add (Ada_Parser.Reserved_Word, "package");
        Words.Add (Ada_Parser.Separator, " ");
        Words.Add (Ada_Parser.Identifier, Name);
        Words.Add (Ada_Parser.Separator, " ");
        Words.Add (Ada_Parser.Reserved_Word, "renames");
        Parse_To_End (";");
        Output.Put (Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Put_Comments (Level);
        Words.Add (Ada_Parser.Reserved_Word, "package");
        Words.Add (Ada_Parser.Separator, " ");
        Words.Add (Ada_Parser.Identifier, Name);
        Words.Add (Ada_Parser.Separator, " ");
        Words.Add (Ada_Parser.Reserved_Word, "is");
        Words.Add (Ada_Parser.Separator, " ");
        Words.Add (Ada_Parser.Reserved_Word, "new");
        Parse_To_End (";");
        Output.Put (Words.Concat, True, Level);
        Words.Reset;
        return;
      elsif Str = "type" then
        Put_Body;
        Parse_Type (Level + 1);
      else
        Put_Body;
        -- Unexpected, word. Parse to end as comment
        Words.Add (Lexic, Text);
        Parse_To_End (";");
        Output.Put (Words.Concat, True, Level);
        Words.Reset;
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (";");
  Put_Comments (Level);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Package;

