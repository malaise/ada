-- Go on parsing current input file up to the next End_String
--  that is not within parentheses.
with Ada.Strings.Unbounded;
with Common, Words, Parser_Ada, Parse_To_Ends;
procedure Parse_To_End (End_Lexic : in Parser_Ada.Lexical_Kind_List;
                        End_String : in String;
                        Level : in Natural;
                        Put_Comments : in Boolean := True;
                        Then_Line_Feed : in Boolean := False;
                        Already_In_Parent : in Boolean := False) is
begin
  Parse_To_Ends (
    End_Criteria => Words.Word_Array'(1 =>
          (End_Lexic,
           Ada.Strings.Unbounded.To_Unbounded_String (End_String))),
   Level => Level,
   Put_Comments => Put_Comments,
   Already_In_Parent => Already_In_Parent);

  if Then_Line_Feed then
    Parse_To_Ends (
      End_Criteria => Words.Word_Array'(1 =>
            (Parser_Ada.Separator, Common.Line_Feed)),
     Level => Level,
     Put_Comments => Put_Comments,
     Already_In_Parent => Already_In_Parent);
  end if;
end Parse_To_End;

