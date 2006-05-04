-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
-- if Display then Output Words as a comment, when end of line or End_String
--   and reset Words (Output is made with coorrect Indent).
-- else keep Words
with Ada.Strings.Unbounded;
with Text_Line, Text_Char, Ada_Parser;
with Common, Files, Output, Words;

procedure Parse_To_End (End_String : in String;
                        Display : in Boolean;
                        Indent : in Natural := 0) is
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
      -- End of line: put whole line or skip Line_Feed (it would break comment)
      if Display then
        Output.Put_Line (Words.Get, Indent, True);
        Words.Reset;
      end if;
    elsif Ada.Strings.Unbounded.To_String (Text) = End_String 
    and then Level = 0 then
      -- End_String at parentheses level 0
      Words.Add (End_String);
      if Display then
        Output.Put_Line (Words.Get, Indent, True);
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

