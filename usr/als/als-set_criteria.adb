with Many_Strings, Regular_Expressions;
with Debug;
separate (Als)
procedure Set_Criteria (Name     : in String;
                        Criteria : in String;
                        Call     : in Call_Access) is
  First : constant Natural := Criteria'First;
  Last : constant Natural := Criteria'Last;
  Length : constant Natural := Criteria'Length;
  Regex_Char : constant Character := '@';

  -- Split a template by ',' and call on each slice
  procedure Split_Template (Crit : in String) is
    Mstr : constant Many_Strings.Many_String
         := Str_Util.Split (Crit, ',');
  begin
    for I in 1 .. Many_Strings.Nb (Mstr) loop
      Debug.Log ("  Adding " & Name & " template "
               & Many_Strings.Nth (Mstr, I));
      Call (Many_Strings.Nth (Mstr, I), False);
    end loop;
  end Split_Template;

begin
  if Length >= 1
  and then Criteria(First) = Regex_Char then
    -- @regex -> regex, no split
    Debug.Log ("  Adding " & Name & " regexp " & Criteria(First + 1 .. Last));
    Call (Criteria(First + 1 .. Last), True);
  elsif Regular_Expressions.Match ("\\+" & Regex_Char & ".*",
                                   Criteria(First .. Last),
                                   Strict => True) then
    -- \..\@template -> \..@template (skip first '\')
    Split_Template (Criteria(First + 1 .. Last));
  else
    -- template
    Split_Template (Criteria);
  end if;
end Set_Criteria;

