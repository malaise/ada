with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End,
     Parse_Procedure, Parse_Function, Parse_Entry;

procedure Parse_Protected (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until protected name, skip "type"
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Asu.To_String (Text);
    begin
      if Lexic = Ada_Parser.Comment
      or else Lexic = Ada_Parser.Separator then
        -- Put comment or separator
        Output.Put (Str, False);
      elsif Str = "type" then
        -- Skip type
        null;
      elsif Lexic = Ada_Parser.Identifier then
        -- Got protected name
        Name := Text;
        exit;
      else
        -- Unexpected word
        Common.Error (Asu.To_String (Name));
      end if;
    end;
  end loop;

  -- Skip until "is"
  Parse_To_End (Ada_Parser.Reserved_Word, "is");
  Words.Del;
  Put_Comments (Level);

  -- Output this and " is"
  Output.Put_Line ("protected body " & Asu.To_String (Name)
                 & " is", False, Level);

  -- Loop until expected word
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    declare
      Str : constant String := Ada.Strings.Unbounded.To_String (Text);
    begin
      if Lexic = Ada_Parser.Comment then
        Output.Put_Line (Str, False, Level + 1);
      elsif Lexic = Ada_Parser.Separator then
        -- Skip separators
        null;
      elsif Str = "end" then
        -- End of this protected
        exit;
      elsif Str = "procedure" then
        Parse_Procedure (Level + 1);
      elsif Str = "function" then
        Parse_Function (Level + 1);
      elsif Str = "entry" then
        Parse_Entry (Level + 1);
      elsif Str = "new" then
        -- This protected is in fact a generic instanciation
        Common.Error ("new");
      elsif Str = "private" then
        -- Put "private" as a comment
        Output.Put_Line (Str, True, Level);
      else
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
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Protected;

