-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
with As.U;
with Words, Parser_Ada, Parse_To_Ends;
procedure Parse_To_End (End_Lexic : in Parser_Ada.Lexical_Kind_List;
                        End_String : in String;
                        Level : in Natural;
                        Put_Comments : in Boolean := True;
                        Up_To_Next_Significant : in Boolean := True) is
begin
  Parse_To_Ends (
    End_Criteria => Words.Word_Array'(1 =>
          (End_Lexic, As.U.Tus (End_String))),
   Level => Level,
   Put_Comments => Put_Comments,
   Up_To_Next_Significant => Up_To_Next_Significant);
end Parse_To_End;

