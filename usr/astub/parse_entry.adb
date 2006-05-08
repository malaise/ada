with Ada.Strings.Unbounded;
with Text_Char, Ada_Parser;
with Common, Files, Output, Words;

procedure Parse_Entry (Level : in Natural) is
  File : constant Text_Char.File_Type := Files.In_File;
  package Asu renames Ada.Strings.Unbounded;
  Name, Family, Last_Id, Text : Asu.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  In_Id, In_Parent : Boolean;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until entry name
  loop
    Ada_Parser.Parse_Next (File, Name, Lexic, True);
    Words.Add (Lexic, Name);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Common.Error (Asu.To_String (Name));
  end if;

  -- Put "entry <name>"
  Words.Add (Ada_Parser.Reserved_Word, "entry");
  Output.Put (Words.Get, False, Level);
  Words.Reset;

  -- Skip family and arguments, store Family
  -- Store arguments lexical elements in words
  In_Parent := False;
  In_Id := False;
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    Words.Add (Lexic, Text);
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
    Output.Put (" (for I in " & Asu.To_String (Family) & ")", False, 0);
  end if;
  -- Put Args if set
  if Words.Length /= 0 then
    -- Delete last saved ;
    Output.Put (" " & Words.Get, False, 0);
  end if;
  Output.Put_Line (" when True is", False, 0);

  -- begin
  --   return <name> (<args>);
  -- end <name>;
  Output.Put_Line ("begin", False, Level);
  Output.Put_Line ("null;", False, Level + 1);
  Output.Put_Line ("end " & Asu.To_String (Name) & ";", False, Level);
end Parse_Entry;

