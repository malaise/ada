with Ada.Text_Io;
with Basic_Proc, Parser, Argument;
procedure T_Parser is

  Sep : Character := '.';

  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sep;
  end Is_Sep;
  Is_Sep_Acc : constant Parser.Separing_Function
             := Is_Sep'Unrestricted_Access;


  function Str return String is
  begin
    if Argument.Get_Nbre_Arg = 1 then
      return "";
    else
      return Argument.Get_Parameter(Occurence => 2);
    end if;
  end Str;

  It : Parser.Iterator;

begin

  if (Argument.Get_Nbre_Arg = 1
      or else Argument.Get_Nbre_Arg = 2)
  and then Argument.Get_Parameter(Occurence => 1)'Length = 1 then
    Sep := Argument.Get_Parameter(Occurence => 1)(1);
  else
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " <separator_char> [ <string_to_parse> ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  Ada.Text_Io.Put_Line ("Parsing >" & Str & "< with separator '" & Sep & "'.");
  Parser.Set (It, Str, Is_Sep_Acc);

  loop
    Ada.Text_Io.Put (">" & Parser.Next_Word (It) & "< ");
    exit when Parser.Current_Word (It) = "";
  end loop;
  Ada.Text_Io.New_Line (2);

  Parser.Reset (It);

  loop
    Parser.Next_Word (It);
    Ada.Text_Io.Put_Line ("Word is >" & Parser.Current_Word (It) & "<"
                        & " from" & Positive'Image(Parser.First_Index (It))
                        & " to" & Natural'Image(Parser.Last_Index (It))
                        & " separated by >"
                        & Parser.Prev_Separators (It) & "<");
    exit when Parser.Current_Word (It) = "";
  end loop;

  if Parser.First_Word (It) = "check"
  and then Parser.Next_Word (It) = "size"
  and then Parser.Next_Word (It) = "" then
    Ada.Text_Io.Put_Line ("Checking size... Ctrl C to abort");
    -- Size of process shall not grow
    loop
      declare
        Itt : Parser.Iterator;
      begin
        Parser.Copy (Itt, It);
        Parser.Set (Itt, Str, Is_Sep_Acc);
      end;
    end loop;
  end if;

  Parser.Del (It);
  Ada.Text_Io.Put_Line ("Done.");

end T_Parser;

