with Text_Handler, Upper_Char;
package body Vigenere is

  -- Coding generates upper char
  subtype Car is Character range 'A' .. 'Z';
  subtype Car_Index is Positive range 1 .. 26;
  function Car2Index (C : Car) return Car_Index is
  begin
    return Character'Pos(C) - Character'Pos(Car'First) + 1;
  end Car2Index;
  function Index2Car (I : Car_Index) return Car is
  begin
    return Character'Val(Character'Pos(Car'First) + I - 1);
  end Index2Car;
  -- Allow lower or upper case -> return upper, else space
  function Char2Car (C : Character) return Character is
    R : Character;
  begin
    R := Upper_Char (C);
    if R not in Car then
      return ' ';
    end if;
    return R;
  end Char2Car;

  function Key2Car (Key : String) return String is
    Res : String := Key;
  begin
    if Key'Length = 0 then
      raise Empty_Key;
    end if;
    for I in Res'Range loop
      Res(I) := Char2Car (Res(I));
      -- No space in key, put A
      if Res(I) = ' ' then
        Res(I) := Car'First;
      end if;
    end loop;
    return Res;
  end Key2Car;

  procedure Encode (Key : in String; Str : in out Long_String) is
    Carkey : constant String := Key2Car (Key);
    Kindex : Positive;
    C : Character;
    Cindex, Offset : Car_Index;
    T : Positive;
  begin
    Kindex := 1;
    -- Encode each character one by one
    for I in Str'Range loop
      -- This character to encode
      C := Char2Car(Str(I));
      if C /= ' ' then
        -- Index of Car corresponding to this character
        Cindex := Car2Index(C);
        -- Offest: Index of Car in Key
        Offset := Car2Index(Carkey(Kindex));
        -- Add offest
        T := Cindex + Offset;
        if T > Car_Index'Last then
          T := T - Car_Index'Last;
        end if;
        -- Get corresponding Car
        C := Index2Car(T);
        -- Next Key for next char
        if Kindex /= Carkey'Last then
          Kindex := Kindex + 1;
        else
          Kindex := Carkey'First;
        end if;
      end if;
      -- Store
      Str(I) := C;
    end loop;
  end Encode;

  procedure Decode (Key : in String; Str : in out Long_String) is
    Carkey : constant String := Key2Car (Key);
    Kindex : Positive;
    C : Character;
    Cindex, Offset : Car_Index;
    T : Integer;
  begin
    Kindex := 1;
    -- Decode each character one by one
    for I in Str'Range loop
      -- This character to decode
      if Str(I) not in Car then
        raise Decode_Error;
      end if;
      C := Char2Car(Str(I));
      -- Index of Car corresponding to this character
      Cindex := Car2Index(C);
      -- Offest: Index of Car in Key
      Offset := Car2Index(Carkey(Kindex));
      -- Sub offest
      T := Cindex - Offset;
      if T < Car_Index'First then
        T := T + Car_Index'Last;
      end if;
      -- Get corresponding Car
      C := Index2Car(T);
      -- Next Key for next char
      if Kindex /= Carkey'Last then
        Kindex := Kindex + 1;
      else
        Kindex := Carkey'First;
      end if;
      -- Store
      Str(I) := C;
    end loop;
  end Decode;

end Vigenere;

