with Ada.Text_Io;
with Sys_Calls, Parser, Argument;
procedure T_Parser is

  Sep : Character := '.';

  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sep;
  end Is_Sep;
  Is_Sep_Acc : Parser.Separing_Function := Is_Sep'Unrestricted_Access;

  Occ : Natural;
  It : Parser.Iterator;

begin

  if Argument.Get_Nbre_Arg = 1 then
    Occ := 1;
  elsif Argument.Get_Nbre_Arg = 2
  and then Argument.Get_Parameter(Occurence => 1)'Length = 1 then
    Sep := Argument.Get_Parameter(Occurence => 1)(1);
    Occ := 2;
  else
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " [ <separator_char> ] <string_to_parse>");
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;
    
  Ada.Text_Io.Put_Line ("Parsing " & Argument.Get_Parameter(Occurence => Occ)
                      & " with separator '" & Sep & "'.");
  Parser.Create (Argument.Get_Parameter (Occurence => Occ), Is_Sep_Acc, It);


  loop
    declare
      Word : constant String := Parser.Next_Word (It);
    begin
      Ada.Text_Io.Put_Line ("Word is >" & Word & "<");
      Ada.Text_Io.Put_Line ("Separated by >"
                          & Parser.Prev_Separators (It) & "<");
      exit when Word = "";
    end;
  end loop;


  Parser.Delete (It);
  Ada.Text_Io.Put_Line ("Done.");

end T_Parser;

