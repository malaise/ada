with Ada.Strings.Unbounded;
with Sys_Calls, Text_Char, Ada_Parser;
with Common, Words;

function Parse_Name (File : in Text_Char.File_Type) return String is
  Name, Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin

  -- Read until identifier
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    Words.Add (Text);
    exit when Lexic /= Ada_Parser.Separator;
  end loop;
  if Lexic /= Ada_Parser.Identifier
  and then Lexic /= Ada_Parser.String_Literal then
    Sys_Calls.Put_Line_Error (" -->" 
         & Ada.Strings.Unbounded.To_String (Name) & "<");
    raise Common.Syntax_Error;
  end if;

  -- Read until identifier or "." and concat
  Name := Text;
  loop
    Ada_Parser.Parse_Next (File, Text, Lexic, True);
    Words.Add (Text);
    exit when Lexic /= Ada_Parser.Identifier
    and then  Lexic /=  Ada_Parser.String_Literal
    and then  Ada.Strings.Unbounded.To_String (Text) /= ".";
      Ada.Strings.Unbounded.Append (Name, Text);
  end loop;

  return Ada.Strings.Unbounded.To_String (Name);

end Parse_Name;

