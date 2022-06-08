package body Language_Utils is

  -- Make Unicode_Sequence
  function To_Unicode (Str : String) return Unicode_Sequence is
    Res : Unicode_Sequence (1 .. Str'Length);
  begin
    for I in Str'Range loop
      Res(I - Str'First + 1) := Character'Pos (Str(I));
    end loop;
    return Res;
  end To_Unicode;

  -- Make character set
  function To_Set (Code : Char_Code) return Char_Set is
    Res : Char_Set;
  begin
    for I in Code'Range loop
      Res (I - Code'First + 1) := Wide_Character'Val (Code(I));
    end loop;
    return Res;
  end To_Set;

  -- Make descriptor
  function Build_Desc (Name : Unicode_Sequence;
                       Code : Char_Code) return Language_Desc is
    Res : Language_Desc (Name'Length);
  begin
    Res.Name.Txt := Name;
    Res.Set := To_Set (Code);
    -- Store first and last character of the set
    Res.First := Wide_Character'Last;
    Res.Last := Wide_Character'First;
    for C of Res.Set loop
      if C > '9' and then C /= 'I' and then C /= 'O' then
        -- Discard characterers that are shared
        if C < Res.First then
          Res.First := C;
        end if;
        if C > '9' and then C > Res.Last then
          Res.Last := C;
        end if;
      end if;
    end loop;
    return Res;
  end Build_Desc;

end Language_Utils;

