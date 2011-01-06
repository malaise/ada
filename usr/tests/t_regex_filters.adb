with Ada.Text_Io;
with Argument, Regex_Filters, Parser, Mixed_Str;
procedure T_Regex_Filters is
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("Usage: " & Argument.Get_Program_Name
                                   & " [ { <criteria> } ]");
    Ada.Text_Io.Put_Line("<criteria > ::= <regex> <match>");
    Ada.Text_Io.Put_Line("<match>     ::= T  |  F");
  end Usage;

  Filter : Regex_Filters.Regex_Filter;
  Str : String(1 .. 32*1024);
  Len : Natural;
  Iter : Parser.Iterator;
  Res : Boolean;
begin

  -- Check argument are pairs
  if Argument.Get_Nbre_Arg rem 2 /= 0  then
    Usage;
  end if;

  -- Store argument pairs "criteria T" or "criteria F" in filter
  for I in 1 .. Argument.Get_Nbre_Arg loop
    -- Check match
    if I rem 2 = 0 then
      if Argument.Get_Parameter(Occurence => I) /= "T"
      and then Argument.Get_Parameter(Occurence => I) /= "F" then
        Usage;
        return;
      end if;
    end if;
    -- Add filter
    begin
      Regex_Filters.Add_Regex(Filter,
                              Argument.Get_Parameter(Occurence => I-1),
                              Argument.Get_Parameter(Occurence => I) = "T");
    exception
      when Regex_Filters.Invalid_Regex =>
        Ada.Text_Io.Put_Line("Invalid regex "
                           & Argument.Get_Parameter(Occurence => I-1));
        Usage;
        return;
    end;
  end loop;

  -- Parse stdin and submit strings
  -- Each line
  loop
    -- Get next line and initalise its parsing
    Ada.Text_Io.Get_Line (Str, Len);
    Parser.Set (Iter, Str(1 .. Len));
    while Parser.Next_Word(Iter) /= "" loop
      Res := Regex_Filters.Check(Parser.Current_Word(Iter), Filter);
      Ada.Text_Io.Put_Line ("Check " & Parser.Current_Word(Iter)
                          & " -> " & Mixed_Str(Res'Img));
    end loop;
  end loop;

exception
  when Ada.Text_Io.End_Error =>
    if Parser.Is_Set(Iter) then
      Parser.Del(Iter);
    end if;
end T_Regex_Filters;

