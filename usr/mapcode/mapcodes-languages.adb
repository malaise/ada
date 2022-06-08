with Ada.Characters.Handling;
package body Mapcodes.Languages is
  use Mapcode_Utils;

  -- Get the Language from its name in its language
  -- Raises, if the output language is not known:
  -- Unknown_Language : exception;
  function Get_Language (Name : Unicode_Sequence) return Language_List is
    use type Language_Utils.Unicode_Sequence;
  begin
    for L in Language_Defs.Langs'Range loop
      if Language_Defs.Langs(L).Name.Txt = Name then
        -- Name is the name of language L
        return Language_List (L);
      end if;
    end loop;
    return Default_Language;
  end Get_Language;

  -- Get the language of a text (territory name or mapcode)
  -- All the characters must be of the same language, otherwise raises
  --   Invalid_Text : exception;
  function Get_Language (Input : Wide_String) return Language_List is
    First : Natural;
    Found : Boolean;
    Lang : Language_Defs.Language_List;
    function Is_Shared (C : Wide_Character) return Boolean is
      (C <= '9' or else C = 'I' or else C = 'O');
  begin
    -- Discard empty string
    if Input'Length = 0 then
      raise Invalid_Text;
    end if;
    -- Find first non shared character
    First := 0;
    for I in Input'Range loop
      if not Is_Shared (Input(I)) then
        First := I;
        exit;
      end if;
    end loop;
    if First = 0 then
      -- All characters are shared => Roman
      Lang := Language_Defs.Language_List (Default_Language);
    else
      -- Find language of first character
      Found := False;
      for L in Language_Defs.Langs'Range loop
        if not Is_Shared (Input(First))
        and then Input(First) >= Language_Defs.Langs(L).First
        and then Input(First) <= Language_Defs.Langs(L).Last then
          Found := True;
          Lang := L;
          exit;
        end if;
      end loop;
      if not Found then
        raise Invalid_Text;
      end if;
    end if;
    -- Check that all characters belong to this language
    for W of Input loop
      if      W /= '.'
      and then W /= '-'
      and then not Is_Shared (W)
      and then (W < Language_Defs.Langs(Lang).First
        or else W > Language_Defs.Langs(Lang).Last) then
        raise Invalid_Text;
      end if;
    end loop;
    return Language_List (Lang);
  end Get_Language;

  -- Does a language use Abjad conversion
  function Is_Abjad (Language : Language_List) return Boolean is
  begin
    return  Language = Greek
    or else Language = Hebrew
    or else Language = Arabic
    or else Language = Korean;
  end Is_Abjad;

  -- Convert From Abjad
  procedure Convert_From_Abjad (Str : in out As_U.Asu_Us) is
    Lstr, Tail : As_U.Asu_Us;
    Index, Form : Natural;
    Code : Integer;
    use type As_U.Asu_Us;

    -- Character code at a given index ( '4' -> 4)
    function Str_At (P : Positive) return Natural is
        (Character'Pos (Lstr.Element (P)) - Character'Pos ('0'));
  begin
    -- Save precision tail, including '-'
    Lstr := Str;
    Index := Lstr.Locate ("-");
    if Index /= 0 then
      Tail := Lstr.Uslice (Index, Lstr.Length);
      Lstr.Delete (Index, Lstr.Length);
    end if;

    Lstr := As_U.Tus (Aeu_Unpack (Lstr.Image));
    Index := Lstr.Locate (".");
    if Index < 3 or else Index > 6 then
      return;
    end if;

    Form := 10 * (Index - 1) + (Lstr.Length - Index);
    if Form = 23 then
      Code := Str_At (4) * 8 + Str_At (5) - 18;
      Lstr := Lstr.Uslice (1, 2)
             & '.' & Encode_A_Char (Code) & Lstr.Element (6);
    elsif Form = 24 then
      Code := Str_At (4) * 8 + Str_At (5) - 18;
      if Code >= 32 then
        Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code - 32)
               & '.' & Lstr.Element (6) & Lstr.Element (7);
      else
        Lstr := Lstr.Uslice (1, 2)
               & '.' & Encode_A_Char (Code) & Lstr.Element (6) & Lstr.Element (7);
      end if;
    elsif Form = 34 then
      Code := Str_At (3) * 10 + Str_At (6) - 7;
      if Code < 31 then
        Lstr := Lstr.Uslice (1, 2)
               & '.' & Encode_A_Char (Code) & Lstr.Element (5) & Lstr.Element (7)
               & Lstr.Element (8);
      elsif Code < 62 then
        Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code - 31)
               & '.' & Lstr.Element (5) & Lstr.Element (7) & Lstr.Element (8);

      else
        Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code - 62) & Lstr.Element (5)
               & '.' & Lstr.Element (7) & Lstr.Element (8);
      end if;
    elsif Form = 35 then
      Code := Str_At (3) * 8 + Str_At (7) - 18;
      if Code >= 32 then
        Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code - 32) & Lstr.Element (5)
               & '.' & Lstr.Element (6) & Lstr.Element (8)
               & Lstr.Element (9);
      else
        Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code)
               & '.' & Lstr.Element (5) & Lstr.Element (6) & Lstr.Element (8)
                & Lstr.Element (9);
      end if;
    elsif Form = 45 then
      Code := Str_At (3) * 100 + Str_At (6) * 10 +  Str_At (9) - 39;
      Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code / 31) & Lstr.Element (4)
             & '.' & Lstr.Element (7)  & Lstr.Element (8) & Lstr.Element (10)
             & Encode_A_Char (Code rem 31);
    elsif Form = 55 then
      Code := Str_At (3) * 100 + Str_At (7) * 10 +  Str_At (10) - 39;
      Lstr := Lstr.Uslice (1, 2) & Encode_A_Char (Code / 31) & Lstr.Element (4)
             & Lstr.Element (5)
             & '.' & Lstr.Element (8)  & Lstr.Element (9) & Lstr.Element (11)
             & Encode_A_Char (Code rem 31);
    else
      return;
    end if;

     Str := As_U.Tus (Aeu_Pack (Lstr, False)) & Tail;

  end Convert_From_Abjad;

  -- Convert into Abjad
  procedure Convert_Into_Abjad (Str : in out As_U.Asu_Us) is
    Lstr, Tail : As_U.Asu_Us;
    Index, Form : Natural;
    Code : Integer;
    C1, C2, C3 : Integer := 0;
    use type As_U.Asu_Us;

    -- Character pos at a given index
    function Str_At (P : Positive) return Natural is
        (Character'Pos (Lstr.Element (P)));
    function Char_Of (P : Natural) return Character is
        (Character'Val (P + Character'Pos('0')));
  begin
    -- Save precision tail, including '-'
    Lstr := Str;
    Index := Str.Locate ("-");
    if Index /= 0 then
      Tail := Lstr.Uslice (Index, Str.Length);
      Lstr.Delete (Index, Lstr.Length);
    end if;

    Lstr := As_U.Tus (Aeu_Unpack (Lstr.Image));
    Index := Lstr.Locate (".");
    if Index < 3 or else Index > 6 then
      return;
    end if;
    Form := 10 * (Index - 1) + (Lstr.Length - Index);
    -- See if more than 2 non-digits in a row
    Index := 0;
    for I in 1 .. Lstr.Length loop
      Code := Str_At (I);
      -- Skip the dot
      if Code /= 46 then
        Index := Index + 1;
        if Decode_A_Char (Code) <= 9 then
          -- A digit
          Index := 0;
        elsif Index > 2 then
          exit;
        end if;
      end if;
    end loop;

    -- No need to convert
    if Index < 3 and then
       (Form = 22 or else
        Form = 32 or else Form = 33 or else
        Form = 42 or else Form = 43 or else Form = 44 or else
        Form = 54) then
      return;
    end if;
    Code := Decode_A_Char (Str_At (3));
    if Code < 0 then
      Code := Decode_A_Char (Str_At (4));
      if Code < 0 then
         return;
      end if;
    end if;

    -- Convert
    if Form >= 44 then
      Code := Code * 31 + Decode_A_Char (Str_At (Lstr.Length)) + 39;
      if Code < 39 or else Code > 999 then
        return;
      end if;
      C1 := Code / 100;
      C2 := (Code rem 100) / 10;
      C3 := Code rem 10;
    elsif Lstr.Length = 7 then
      if Form = 24 then
        Code := Code + 7;
      elsif Form = 33 then
        Code := Code + 38;
      elsif Form = 42 then
        Code := Code + 69;
      end if;
      C1 := Code / 10;
      C2 := Code rem 10;
    else
      C1 := 2 + Code / 8;
      C2 := 2 + Code rem 8;
    end if;

    if Form = 22 then
      Lstr := Lstr.Uslice (1, 2)
             & '.' & Char_Of (C1) & Char_Of (C2) & Lstr.Element (5);
    elsif Form = 23 then
      Lstr := Lstr.Uslice (1, 2)
             & '.' & Char_Of (C1) & Char_Of (C2)
                   & Lstr.Element (5) & Lstr.Element (6);
    elsif Form = 32 then
      Lstr := Lstr.Uslice (1, 2)
             & '.' & Char_Of (C1 + 4) & Char_Of (C2)
                   & Lstr.Element (5) & Lstr.Element (6);
    elsif Form = 24 or else Form = 33 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1)
             & '.' & Lstr.Element (5) & Char_Of (C2)
                   & Lstr.Element (6) & Lstr.Element (7);
    elsif Form = 42 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1)
             & '.' & Lstr.Element (4) & Char_Of (C2)
                   & Lstr.Element (6) & Lstr.Element (7);
    elsif Form = 43 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1 + 4)
             & '.' & Lstr.Element (4) & Lstr.Element (6)
             & Char_Of (C2) & Lstr.Element (7) & Lstr.Element (8);
    elsif Form = 34 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1)
             & '.' & Lstr.Element (5) & Lstr.Element (6)
             & Char_Of (C2) & Lstr.Element (7) & Lstr.Element (8);
    elsif Form = 44 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1) & Lstr.Element (4)
             & '.' &  Char_Of (C2) & Lstr.Element (6) & Lstr.Element (7)
             & Char_Of (C3) & Lstr.Element (8);
    elsif Form = 54 then
      Lstr := Lstr.Uslice (1, 2) & Char_Of (C1) & Lstr.Element (4)
             & Lstr.Element (5)
             & '.' &  Char_Of (C2) & Lstr.Element (7) & Lstr.Element (8)
             & Char_Of (C3) & Lstr.Element (9);
    else
      return;
    end if;

    Str := As_U.Tus (Aeu_Pack (Lstr, False)) & Tail;
  end Convert_Into_Abjad;

  --Conversion of a text (territory name or mapcode) into a given language
  -- The language of the input is detected automatically
  -- Raises Invalid_Text if the input is not valid
  function Convert (Input : Wide_String;
                    Output_Language : Language_List := Default_Language)
           return Wide_String is
    Input_Language : constant Language_List := Get_Language (Input);
    Def_Input : constant Language_Defs.Language_List
              := Language_Defs.Language_List (Input_Language);
    Def_Output : constant Language_Defs.Language_List
              := Language_Defs.Language_List (Output_Language);
    Tmp_Wide : Wide_String (1 .. Input'Length);
    Found : Boolean;
    Tmp_Str, Tail : As_U.Asu_Us;
    Index : Natural;
    Code : Positive;
    use type As_U.Asu_Us;
  begin
    -- Optim: nothing to do
    if Output_Language = Input_Language then
      return Input;
    end if;

    -- Convert Input into Roman
    ---------------------------
    if Input_Language = Roman then
      Tmp_Str := As_U.Tus (Ada.Characters.Handling.To_String (Input));
    else
      Index := 0;
      -- Replace each wide char
      for W of Input loop
        Index := Index + 1;
        if W = '-' or else W = '.' then
          Tmp_Wide(Index) := W;
        else
          Found := False;
          for J in Language_Defs.Langs(Def_Input).Set'Range loop
            if W = Language_Defs.Langs(Def_Input).Set(J) then
              -- Found W in Set of Input_Language
              --  => Replace by corresponding Romain
              Found := True;
              Tmp_Wide(Index) :=
                  Language_Defs.Langs(Language_Defs.Roman).Set(J);
              exit;
            end if;
          end loop;
          if not Found then
            raise Invalid_Text;
          end if;
        end if;
      end loop;

      -- Roman string
      Tmp_Str := As_U.Tus (Ada.Characters.Handling.To_String (Tmp_Wide));

      -- Repack
      if Tmp_Str.Element (1) = 'A' then
        Index := Tmp_Str.Locate ("-");
        if Index /= 0 then
          -- Save precision tail, including '-'
          Tail := Tmp_Str.Uslice (Index, Tmp_Str.Length);
          Tmp_Str.Delete (Index, Tmp_Str.Length);
        end if;
        Tmp_Str := Mapcodes.Aeu_Pack (
                    As_U.Tus (Aeu_Unpack (Tmp_Str.Image)),
                    False)
                   & Tail;
      end if;

      -- Abjad conversion
      if Is_Abjad (Input_Language) then
        Convert_From_Abjad (Tmp_Str);
      end if;
    end if;

    if Output_Language = Roman then
      return Ada.Characters.Handling.To_Wide_String (Tmp_Str.Image);
    end if;

    -- Convert Roman into output language
    -------------------------------------
    if Is_Abjad (Output_Language) then
      -- Abjad conversion
      Convert_Into_Abjad (Tmp_Str);
    end if;

    -- Unpack for languages that do not support E and U
    if Output_Language = Greek then
      Index := Tmp_Str.Locate ("-");
      Tail.Set_Null;
      if Index /= 0 then
        -- Save precision tail, including '-'
        Tail := Tmp_Str.Uslice (Index, Tmp_Str.Length);
        Tmp_Str.Delete (Index, Tmp_Str.Length);
      end if;
      if Tmp_Str.Locate ("E") /= 0 or else Tmp_Str.Locate ("U") /= 0 then
        Tmp_Str := As_U.Tus
            (Aeu_Pack (As_U.Tus (Aeu_Unpack (Tmp_Str.Image)), True));
      end if;
      Tmp_Str := Tmp_Str & Tail;
    end if;

    -- Substitute
    declare
     Result : Wide_String (1 .. Tmp_Str.Length);
    begin
      for I in 1 .. Tmp_Str.Length loop
        Code := Character'Pos (Tmp_Str.Element (I));
        if Code >= 65 and then Code <= 90 then
          Result(I) := Language_Defs.Langs(Def_Output).Set(Code - 64);
        elsif Code >= 48 and then Code <= 57 then
          Result(I) := Language_Defs.Langs(Def_Output).Set(Code + 26 - 47);
        else
          Result(I) := Wide_Character'Val (Code);
        end if;
      end loop;
      return Result;
    end;
  end Convert;

end Mapcodes.Languages;

