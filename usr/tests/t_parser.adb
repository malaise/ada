-- Parse a string according to a separator, both provided as arguments
-- Dump words, indexes, and parse all in one
-- With arguments " " "check size", make infinite loop of préating a
--  temporary itérator
with Basic_Proc, Parser.All_In_One, Argument, Mixed_Str;
procedure T_Parser is

  Sep : Character := '.';

  function Is_Sep (C : Character) return Boolean is (C = Sep);

  function Str return String is
    (if Argument.Get_Nbre_Arg = 1 then ""
     else Argument.Get_Parameter(Occurence => 2));

  It : Parser.Iterator;

begin

  if (Argument.Get_Nbre_Arg = 1
      or else Argument.Get_Nbre_Arg = 2)
  and then Argument.Get_Parameter(Occurence => 1)'Length = 1 then
    Sep := Argument.Get_Parameter(Occurence => 1)(1);
  else
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " <separator_char> [ <string_to_parse> ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  Basic_Proc.Put_Line_Output ("Parsing >" & Str & "< with separator '" & Sep & "':");
  Parser.Set (It, Str, Is_Sep'Unrestricted_Access);

  loop
    Basic_Proc.Put_Output (">" & Parser.Next_Word (It) & "< ");
    exit when Parser.Current_Word (It) = "";
  end loop;
  Basic_Proc.New_Line_Output;
  Basic_Proc.New_Line_Output;

  Parser.Reset (It);

  Basic_Proc.Put_Line_Output ("Getting indexes:");
  loop
    Parser.Next_Word (It);
    Basic_Proc.Put_Line_Output ("Word is >" & Parser.Current_Word (It) & "<"
                        & " from" & Positive'Image(Parser.First_Index (It))
                        & " to" & Natural'Image(Parser.Last_Index (It))
                        & " separated by >"
                        & Parser.Prev_Separators (It) & "<");
    exit when Parser.Current_Word (It) = "";
  end loop;

  if Parser.First_Word (It) = "check"
  and then Parser.Next_Word (It) = "size"
  and then Parser.Next_Word (It) = "" then
    Basic_Proc.Put_Line_Output ("Checking size... Ctrl C to abort");
    -- Size of process shall not grow
    loop
      declare
        Itt : Parser.Iterator;
      begin
        Itt := It;
        Parser.Set (Itt, Str, Is_Sep'Unrestricted_Access);
      end;
    end loop;
  end if;
  Parser.Del (It);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Parsing all in one:");
  Parser.Set (It, Str, Is_Sep'Unrestricted_Access);
  declare
    Parsed : constant Parser.All_In_One.Parsed_Array
           := Parser.All_In_One.Parse_All (It);
  begin
    for P of Parsed loop
      Basic_Proc.Put_Line_Output ("Got >" & P.Str.Image
       & "< of kind " & Mixed_Str (P.Kind'Img) );
    end loop;
  end;
  Parser.Del (It);
  Basic_Proc.New_Line_Output;

  Basic_Proc.Put_Line_Output ("Done.");

end T_Parser;

