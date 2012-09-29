with Basic_Proc, Argument, Regex_Filters, Parser, Mixed_Str;
procedure T_Regex_Filters is
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output("Usage: " & Argument.Get_Program_Name
                                   & " [ { <criteria> } ]");
    Basic_Proc.Put_Line_Output("<criteria > ::= <regex> <match>");
    Basic_Proc.Put_Line_Output("<match>     ::= T  |  F");
  end Usage;

  Filter : Regex_Filters.Regex_Filter;
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
        Basic_Proc.Put_Line_Output("Invalid regex "
                           & Argument.Get_Parameter(Occurence => I-1));
        Usage;
        return;
    end;
  end loop;

  -- Parse stdin and submit strings
  -- Each line
  loop
    -- Get next line and initalise its parsing
    Parser.Set (Iter, Basic_Proc.Get_Line);
    while Parser.Next_Word(Iter) /= "" loop
      Res := Regex_Filters.Check(Parser.Current_Word(Iter), Filter);
      Basic_Proc.Put_Line_Output ("Check " & Parser.Current_Word(Iter)
                          & " -> " & Mixed_Str(Res'Img));
    end loop;
  end loop;

exception
  when Basic_Proc.End_Error =>
    if Parser.Is_Set(Iter) then
      Parser.Del(Iter);
    end if;
end T_Regex_Filters;

