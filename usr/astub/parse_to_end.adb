-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
with Ada.Strings.Unbounded;
with Ada_Parser;
with Words, Parse_To_Ends;
procedure Parse_To_End (End_Lexic : in Ada_Parser.Lexical_Kind_List;
                        End_String : in String := "";
                        Already_In_Parent : Boolean := False) is
begin
  Parse_To_Ends (Words.Word_Array'(1 =>
    (End_Lexic,
     Ada.Strings.Unbounded.To_Unbounded_String (End_String))),
                 Already_In_Parent);

end Parse_To_End;

