with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser, Dynamic_List;
with Common, Files, Output, Words, Parse_To_End, Parse_Type,
     Parse_Procedure, Parse_Function, Parse_Task, Parse_Protected;

procedure Parse_Package (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;

  -- This stores the comments that are read after "package"
  --  but before knowing if this is a real package or a renames/instantiation
  package Comments_Mng is new Dynamic_List (Asu.Unbounded_String);
  Comments_Ahead : Comments_Mng.Dyn_List.List_Type;

  -- Display comments that where read ahead
  procedure Flush_Comments (Level : in Natural) is
    Comment :  Asu.Unbounded_String;
    Done : Boolean;
  begin
    if Comments_Mng.Dyn_List.Is_Empty (Comments_Ahead) then
      return;
    end if;
    Comments_Mng.Dyn_List.Rewind (Comments_Ahead);
    loop
      Comments_Mng.Dyn_List.Get (Comments_Ahead, Comment, Done => Done);
      Output.Put_Line (Asu.To_String (Comment), Level, False);
      exit when not Done;
    end loop;
  end Flush_Comments;

  -- Put current "package body <name> is" and comments read ahead, once
  Body_Put : Boolean := False;
  procedure Put_Body is
  begin
    if not Body_Put then
      Output.Put_Line ("package body " & Asu.To_String (Name) & " is",
                       Level, False);
      Output.Put_Line ("", 0, False);
      Flush_Comments (Level + 1);
    end if;
    Body_Put := True;
  end Put_Body;

begin

  -- Get package name
  Parse_Name (File, Name, Text);
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
          Output.Put_Line (Str, Level + 1, False);
        else
          -- Not knowing yet if this is a real package
          -- Save comment
          Comments_Mng.Dyn_List.Insert (Comments_Ahead, Text);
        end if;
      elsif Lexic = Ada_Parser.Separator then
        -- Skip separators
        null;
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
        Output.Put_Line ("", 0, False);
      elsif Str = "procedure" then
        -- Put name is this is this is the first statement
        --   after "package <name> is"
        Put_Body;
        Parse_Procedure (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "function" then
        Put_Body;
        Parse_Function (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "task" then
        Put_Body;
        Parse_Task (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "protected" then
        Put_Body;
        Parse_Protected (Level + 1);
        Output.Put_Line ("", 0, False);
      elsif Str = "private" then
        Put_Body;
        -- Put "private" as a comment
        Output.Put_Line (Str, Level, True);
        Output.Put_Line ("", 0, False);
      elsif Str = "renames" then
        -- This package is in fact a renaming declaration
        -- Reset to "package <name> renames" and put as comment
        Flush_Comments (Level);
        Words.Add ("package");
        Words.Add (" ");
        Words.Add (Name);
        Words.Add (" ");
        Words.Add ("renames");
        Parse_To_End (";", True, Level);
        return;
      elsif Str = "new" then
        -- This package is in fact a generic instanciation
        -- Reset to "package <name> is new" and put as comment
        Flush_Comments (Level);
        Words.Add ("package");
        Words.Add (" ");
        Words.Add (Name);
        Words.Add (" ");
        Words.Add ("is");
        Words.Add (" ");
        Words.Add ("new");
        Parse_To_End (";", True, Level);
        return;
      elsif Str = "type" then
        Put_Body;
        Parse_Type (Level + 1);
        Output.Put_Line ("", 0, False);
      else
        Put_Body;
        -- Unexpected, word. Parse to end as comment
        Words.Add (Text);
        Parse_To_End (";", True, Level + 1);
      end if;
    end;
  end loop;

  -- Skip up to last ";"
  Parse_To_End (";", False);
  Words.Reset;

  -- end <name>;
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Package;

