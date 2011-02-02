with As.U;
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

  Initialized : Boolean := False;
  procedure Init is
  begin
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
  subtype Str3 is String (1 .. 3);

  -- Name of 0 .. 9
  function Make1 (Str : Str1) return String is
  begin
    if Str = "0" then
      return "";
    end if;
    return Names(Character'Pos (Str(1)) - Character'Pos ('0')).all;
  end Make1;

  -- Name of 0 .. 99
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

  -- Name of 0 .. 999
  function Make3 (Str : Str3) return String is
    -- String (1 .. 3)
    Lstr : constant Str3 := Str;
  begin
    if Lstr(1) /= '0' then
      if Lstr (2 .. 3) /= "00" then
        -- xxx hundred yyy
        return Make1 (Str(1 .. 1)) & " " & Tenth(2).all
                    & " " & Make2 (Str(2 .. 3));
      else
        -- xxx hundred
        return Make1 (Str(1 .. 1)) & " " & Tenth(2).all;
      end if;
    else
      -- xxx hundred yyy
      return Make2 (Str(2 .. 3));
    end if;
  end Make3;

  function Letters_Of (N : Number) return String is
    Img : constant String := N'Img;
    Rev : String (1 .. Img'Length);
    Last : Positive;
    Txt : As.U.Asu_Us;
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
    if Last = 6 then
      Txt := As.U.Tus (Make3 (Rev(6) & Rev(5) & Rev(4)));
    elsif Last = 5 then
      Txt := As.U.Tus (Make2 (Rev(5) & Rev(4)));
      Last := 3;
    elsif Last = 4 then
      Txt := As.U.Tus (Make1 (Rev(4 .. 4)));
      Last := 3;
    end if;
    if not Txt.Is_Null then
      Txt.Append (" " & Tenth(3).all);
      if Rev(1 .. 3) = "000" then
        -- xxx000 -> thousands => Done
        return Txt.Image;
      end if;
      -- Prepare to process hundreds
      Txt.Append (" ");
      Last := 3;
    end if;

    -- Generate hundreds
    if Last = 3 then
      Txt.Append (Make3 (Rev(3) & Rev(2) & Rev(1)));
    elsif Last = 2 then
      Txt.Append (Make2 (Rev(2) & Rev(1)));
    elsif Last = 1 then
      Txt.Append (Make1 (Rev(1 .. 1)));
    else
      raise Program_Error;
    end if;

    return Txt.Image;
  end Letters_Of;

end Num_Letters;

