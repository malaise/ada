with As.U; use As.U;
package body Num_Letters is

  -- Conversion Number -> letters
  --  e.g. 9789  nine thousand seven hundred eighty nine

  -- type Number is Natural range 0 .. 99_999;
  type Name_Access is access String;
  type Name_Array is array (Natural range <>) of Name_Access;
  -- Stores "zero", "one" .... "twelve"
  Names : Name_Array (0 .. 12);
  -- Stores "thir" and "fif" (others are left null)
  Suffixes : Name_Array (0 .. 9);
  -- Stores "teen" "ty" " hundred" "thousand"
  Tenth : Name_Array (0 .. 3);

  -- Result
  -- Should store "seventy seven thousand seven hundred seventy seven"
  --  or same with eight
  Txt : Asu_Us;

  Initialized : Boolean := False;
  procedure Init is
  begin
    Txt := Asu_Null;
    if Initialized then
      return;
    end if;
    Initialized := True;
    Names := (new String'("zero"),
              new String'("one"),
              new String'("two"),
              new String'("three"),
              new String'("four"),
              new String'("five"),
              new String'("six"),
              new String'("seven"),
              new String'("eight"),
              new String'("nine"),
              new String'("ten"),
              new String'("eleven"),
              new String'("twelve") );
   Suffixes (2) := new String'("twen");
   Suffixes (3) := new String'("thir");
   Suffixes (5) := new String'("fif");
   Suffixes (8) := new String'("eigh");
   Tenth := (new String'("teen"),
             new String'("ty"),
             new String'("hundred"),
             new String'("thousand") );
  end Init;

  subtype Str1 is String (1 .. 1);
  subtype Str2 is String (1 .. 2);

  -- Generate 1 .. 9
  function Make1 (Str : Str1) return String is
  begin
    if Str = "0" then
      return "";
    end if;
    return Names(Character'Pos (Str(1)) - Character'Pos ('0')).all;
  end Make1;

  function Make2 (Str : Str2) return String is
    Num : Natural;
    T, U : Natural;
  begin
    Num := Natural'Value(Str);
    T := Num / 10;
    U := Num rem 10;
    if Num in Names'Range then
      -- 0 .. 12
      return Names(Num).all;
    elsif T = 1 then
      -- 13 .. 19
      if U in Suffixes'Range and then Suffixes(U) /= null then
        return Suffixes(U).all & Tenth(0).all;
      else
        return Names(U).all & Tenth(0).all;
      end if;
    else
      -- 20 .. 99
      if Str(2) = '0' then
        -- 20, 30 .. 90
        if T in Suffixes'Range and then Suffixes(T) /= null then
          return Suffixes(T).all & Tenth(1).all;
        else
          return Names(T).all & Tenth(1).all;
        end if;
      else
        -- Others
        if T in Suffixes'Range and then Suffixes(T) /= null then
          return Suffixes(T).all & Tenth(1).all & " " & Make1 (Str(2 .. 2));
        else
          return Names(T).all & Tenth(1).all & " " & Make1 (Str(2 .. 2));
        end if;
      end if;
    end if;
  end Make2;

  function Letters_Of (N : Number) return String is
    Img : constant String := N'Img;
    Rev : String (1 .. Img'Length);
    Last : Positive;
  begin
    Init;
    -- Handle specific case of 0
    if N = 0 then
      return Names(0).all;
    end if;

    -- Store N image in reverse order and skip leading space" 9876" -> "6789"
    Last := 1;
    for I in reverse Img'Range loop
      Rev(Last) := Img(I);
      exit when I = Img'First;
      exit when Img(I - 1) = ' ';
      Last := Last + 1;
    end loop;

    -- Generate thousands, leave hundreds
    if Last = 5 then
      Txt := Tus (Make2 (Rev(5) & Rev(4)) & " " & Tenth(3).all);
      Last := 3;
    elsif Last = 4 then
      Txt := Tus (Make1 (Rev(4 .. 4)) & " " & Tenth(3).all);
      Last := 3;
    end if;

    -- Skip hundreds if no hundreds
    if Last = 3 and then Rev(3) = '0' then
      Last := 2;
    end if;

    -- Generate hundreds, leave tenths
    if Last = 3 then
      -- Add separator from thousands
      if not Txt.Is_Null then
        Txt.Append (" ");
      end if;
      Txt.Append (Make1 (Rev(3 .. 3)) & " " & Tenth(2).all);
      Last := 2;
    end if;

    -- Skip tenths if no tenths
    if Last = 2 and then Rev(2) = '0' then
      Last := 1;
    end if;

    -- Generate tenths or unit
    if Last = 2 then
      -- Add separator from hundreds
      if not Txt.Is_Null then
        Txt.Append (" ");
      end if;
      Txt.Append (Make2 (Rev(2) & Rev(1)));
    elsif Rev(1) /= '0' then
      -- Add separator from hundreds
      if not Txt.Is_Null then
        Txt.Append (" ");
      end if;
      Txt.Append (Make1 (Rev(1 .. 1)));
    end if;

    return Txt.Image;
  end Letters_Of;

end Num_Letters;

