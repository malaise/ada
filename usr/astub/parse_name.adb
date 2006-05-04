with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Words;

procedure Parse_Name (File : in Text_Char.File_Type;
                      Name : out Ada.Strings.Unbounded.Unbounded_String;
                      Ending : out Ada.Strings.Unbounded.Unbounded_String) is
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until identifier
  loop
    Ada_Parser.Parse_Next (File, Ending, Lexic, True);
    Words.Add (Ending);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Ada.Strings.Unbounded.To_String (Ending) & "<");
    raise Common.Syntax_Error;
  end if;

  -- Read until identifier or "." and concat
  Name := Ending;
  loop
    Ada_Parser.Parse_Next (File, Ending, Lexic, True);
    Words.Add (Ending);
    exit when Lexic /= Ada_Parser.Identifier
    and then Lexic /=  Ada_Parser.String_Literal
    and then Ada.Strings.Unbounded.To_String (Ending) /= ".";
      Ada.Strings.Unbounded.Append (Name, Ending);
  end loop;

end Parse_Name;

