with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Files, Output, Words, Parse_To_End;

procedure Parse_Entry (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Family, Last_Id, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Id, In_Parent : Boolean;
  use type Ada_Parser.Lexical_Kind_List;
begin

  Words.Add ("entry");
  -- Read until entry name
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    Words.Add (Name);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Asu.To_String (Name) & "<");
    raise Common.Syntax_Error;
  end if;

  -- Put "entry <name>"
  Output.Put (Words.Get, Level, False);
  Words.Reset;

  -- Skip family and arguments, store Family
  -- Store arguments lexical elements in words
  In_Parent := False;
  In_Id := False;
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    Words.Add (Text);
    if Asu.To_String (Text) = "(" then
      In_Parent := True;
      In_Id := True;
    elsif Asu.To_String (Text) = ")" then
      In_Parent := False;
      if In_Id then
        -- Identifier was not followed by ':', it was the family
        Family := Last_Id;
        Words.Reset;
      end if;
    end if;
    if In_Id and then Lexic = Ada_Parser.Identifier then
      -- Save this adentifier, it might be the entry family
      Last_Id := Text;
    elsif Asu.To_String (Text) = ":" then
      -- End of argument formal names (entering in | out | inout ...)
      In_Id := False;
    elsif Asu.To_String (Text) = ";" then
      if In_Parent then
        -- End of previous argument, expecting a new one
        In_Id := True;
      else
        -- Outside (), this ends the entry definition
        exit;
      end if;
    end if;
  end loop;

  -- Remove last ";"
  Words.Del;

  -- Put Family if set
  if Asu.Length (Family) /= 0 then
    Output.Put (" (for I in " & Asu.to_String (Family) & ")", 0, False);
  end if;
  -- Put Args if set
  if Words.Length /= 0 then
    -- Delete last saved ;
    Output.Put (" " & Words.Get, 0 , False);
  end if;
  Output.Put_Line (" when True is", 0, False);

  -- begin
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", Level, False);
  Output.Put_Line ("null;", Level + 1, False);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";",
                   Level, False);
end Parse_Entry;

