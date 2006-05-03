-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
-- Put line if Line_Feed occurs
-- Put_End if last line and its End_String must be put (and words is cleared)
-- Indent is current indentation level
-- Comment is if result is a comment
with Ada.Strings.Unbounded;
with Text_Line, Text_Char, Ada_Parser;
with Common, Files, Output, Words;

procedure Parse_To_End (Indent : in Natural;
                        End_String : in String;
                        Comment : in Boolean;
                        Put_End : in Boolean) is
  Level : Natural := 0;
  File : constant Text_Char.File_Type := Files.In_File;
  Text : Ada.Strings.Unbounded.Unbounded_String;
  Lexic : Ada_Parser.Lexical_Kind_List;
  use type Ada_Parser.Lexical_Kind_List;
begin
  -- Loop until End_Char
  loop
    -- Read next lexical element
    Ada_Parser.Parse_Next (File, Text, Lexic, True);

    if Ada.Strings.Unbounded.To_String (Text) = Common.Line_Feed then
      -- End of line: put whole line
      Output.Put_Line (Words.Get, Indent, Comment);
      Words.Reset;
    elsif Ada.Strings.Unbounded.To_String (Text) = End_String 
    and then Level = 0 then
      -- Look for End_String at parentheses level 0
      -- If not Put_End, return without adding End_Char
      if Put_End then
        Words.Add (End_String);
        Output.Put_Line (Words.Get, Indent, Comment);
        Words.Reset;
      end if;
      return;
    elsif Ada.Strings.Unbounded.To_String (Text) = "(" then
      -- keep Level of parentheses
      Level := Level + 1;
      Words.Add (Text);
    elsif Ada.Strings.Unbounded.To_String (Text) = ")" then
      Level := Level - 1;
      Words.Add (Text);
    else
      -- In any case, save this word
      Words.Add (Text);
    end if;
  end loop;

end Parse_To_End;

