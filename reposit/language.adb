with Environ, Utf_8;
package body Language is

  -- When ENV, UTF_8 is set if a Getenv on "LANG" gives a value
  --  ending by ".UTF-8". This is the default behaviour.
  -- type Language_List is (Lang_C, Lang_Utf_8, Get_Env);

  -- Language management
  Lang : Language_List := Get_Env;
  procedure Set_Language (Language : in Language_List) is
  begin
    Lang := Language;
  end Set_Language;

  function Get_Language return Language_Set_List is
  begin
    if Lang = Get_Env then
      declare
        Lang_Str : constant String := Environ.Getenv ("LANG");
      begin
        if Lang_Str'Length > 6
        and then Lang_Str(Lang_Str'Last-5 .. Lang_Str'Last) = ".UTF-8" then
          Lang := Lang_Utf_8;
        else
          Lang := Lang_C;
        end if;
      end;
    end if;
    return Lang;
  exception
    when others =>
      Lang := Lang_C;
      return Lang;
  end Get_Language;

  -- When a character is encoded on several bytes,
  --  language is used to detect the end of this sequence,
  --  thus to compute the length of the printed string.
  function Length (Str : String) return Natural is
    Lang : constant Language_Set_List := Get_Language;
    Len : Natural;
    Index : Natural;
  begin
    if Lang = Lang_C then
      -- Easy, one char for one printed cell
      return Str'Length;
    elsif Lang = Lang_Utf_8 then
      if Str'Length = 0 then
        return 0;
      end if;
      -- Count effective printed len
      Index := Str'First;
      Len := 0;
      loop
        Len := Len + 1;
        -- Skip the Nb_Chars of this sequence
        Index := Index + Utf_8.Nb_Chars (Str(index));
        exit when Index > Str'last;
      end loop;
      return Len;
    else
      -- Default
      return Str'Length;
    end if;
  end Length;

end Language;

