with String_Mng.Regex, String_Mng.Navigator;
separate (Search_Pattern)
-- Check that the string does not contain any significant ^ or $
--  except ^ in first and $ in last post
procedure Check_In (Str : in String; Extended : in Boolean) is
  -- The navigator
  Nav : String_Mng.Navigator.Navigator_Type;
  No_Char : constant Character := String_Mng.Navigator.Default_No_Char;
  -- Are we in brakets
  In_Brakets : Boolean := False;
  -- Current character
  Char, Exp_Char : Character;
begin
  -- Replace any "\x" but "\(" and "\)" by 'a'
  Nav.Set (String_Mng.Regex.Replace (Str, "\\[^()]", "a"));
  -- Scan the string
  loop
    Char := Nav.Lookup;
    if Char = '[' then
      if not In_Brakets then
        if Nav.Lookup(1, True) = '[' then
          -- [[: [[= or [[. skip up to the corresponding x]]
          Exp_Char := Nav.Lookup (2, True);
          Nav.Move (3, True);
          while Nav.Lookup (0, True) /= Exp_Char
          or else Nav.Lookup (1, True) /= ']'
          or else Nav.Lookup (2, True) /= ']' loop
            Nav.Move;
          end loop;
        else
          In_Brakets := True;
        end if;
      end if;
    elsif Char = ']' then
        -- The ']' of "[]" or "[^]" is not closing the brakets
      if In_Brakets
      and then Nav.Lookup (-1) /= '['
      and then (Nav.Lookup (-2) /= '[' or else Nav.Lookup (-1) /= '^') then
        In_Brakets := False;
      end if;
    elsif Char = '^' and then not In_Brakets then
      -- '^' is allowed at beginning and when not meaning "begin of line"
      -- '^' means begin of line except in brakets in Extented regex
      -- '^' means begin of line before \( in Basic regex
      if Nav.Position /= 1 then
        if Extended then
          Error ("Unexpected '^' within regex");
        elsif not Extended
        and then Nav.Lookup (1) = '\'
        and then Nav.Lookup (2) = '(' then
          Error ("Unexpected '^' within regex");
        end if;
      end if;
    elsif Char = '$'  and then not In_Brakets then
      -- '$' is allowed at end and when not meaning "end of line"
      -- '$' means end of line except in brakets in Extented regex
      -- '$' means end of line after \) in Basic regex
      if Nav.Position /= Nav.Length then
        if Extended then
          Error ("Unexpected '$' within regex");
        elsif not Extended
        and then Nav.Lookup (-2) = '\'
        and then Nav.Lookup (-1) = ')' then
          Error ("Unexpected '$' within regex");
        end if;
      end if;
    end if;
    Nav.Move;
    exit when not Nav.In_Bounds;
  end loop;
exception
  when String_Mng.Navigator.Out_Of_Bounds =>
    Error ("Unexpected end of regex """ & Str & """");
end Check_In;

