with As.U;
package body Num_Letters is

  -- Conversion Number -> letters
  --  e.g. 9789  nine thousand seven hundred eighty nine

  type Name_Access is access String;
  type Name_Array is array (Natural range <>) of Name_Access;
  -- Stores "zero", "one" .... "twelve"
  Names : Name_Array (0 .. 12);
  -- Stores "thir" and "fif" (others are left null)
  Suffixes : Name_Array (0 .. 9);
  -- Stores "teen" "ty" " hundred"
  Tenth : Name_Array (0 .. 2);
  -- Stores "", "thousand", "million", "milliard", billion", "billiard"
  --  "trillion", "trilliard"
  Thousand : Name_Array (0 .. 21);

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
             new String'("hundred") );
   Thousand := (new String'(""),
                new String'("thousand"),
                new String'("million"),
                new String'("milliard"),
                new String'("billion"),
                new String'("billiard"),
                new String'("trillion"),
                new String'("trilliard"),
                new String'("quadrillion"),
                new String'("quadrilliard"),
                new String'("quintillion"),
                new String'("quintilliard"),
                new String'("sextillion"),
                new String'("sextilliard"),
                new String'("septillion"),
                new String'("septilliard"),
                new String'("octillion"),
                new String'("octilliard"),
                new String'("nonillion"),
                new String'("nonilliard"),
                new String'("decillion"),
                new String'("decilliard") );
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

  -- Name of 00 .. 99
  function Make2 (Str : Str2) return String is
    Num : Natural;
    T, U : Natural;
  begin
    if Str = "00" then
      return "";
    end if;

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
        -- x hundred yy
        return Make1 (Str(1 .. 1)) & " " & Tenth(2).all
                    & " " & Make2 (Str(2 .. 3));
      else
        -- x hundred
        return Make1 (Str(1 .. 1)) & " " & Tenth(2).all;
      end if;
    else
      -- yy
      return Make2 (Str(2 .. 3));
    end if;
  end Make3;

  -- Make name of 1 to 3 digits
  function Make100 (Str : in String) return String is
    Lstr : Str3 := (others => '0');
  begin
    if Str'Length > 3 then
      raise Constraint_Error;
    end if;
    -- Build string 00x or 0xx or xxx
    Lstr (Lstr'First - Str'Length + Lstr'Length .. Lstr'Last) := Str;
    return Make3 (Lstr);
  end Make100;

  function Letters_Of (N : Number) return String is
    -- Image: +xxxx
    Tmp : constant String := Arbitrary.Image (N);
    -- Digits, skip '+'
    Img : constant String (1 .. Tmp'Length - 1)
        := Tmp (Integer'Succ(Tmp'First) .. Tmp'Last);
    -- Start and stop of slice of 3 chars max:= Arbitrary.Image (N);
    -- Start and stop of slice of 3 chars max
    Start, Stop : Positive;
    -- Do we need to prepend a separator between previous output and current
    Sep : Boolean;
    -- Result
    Txt : As.U.Asu_Us;
    use type Number;
  begin
    Init;
    if N > Max_Number then
      raise Constraint_Error;
    end if;
    -- Handle specific case of 0 => "zero"
    if N = Arbitrary.Zero then
      return Names(0).all;
    end if;

    -- 5 Slices of 3 digits, 1..3, 4..6, ...
    Start := 1;
    -- Depending on len, Stop is: for 1->1, 2->2, 3->3, 4->1, 5->2...
    Stop := (Img'Length - 1) rem 3 + 1;
    -- Initially no separator
    Sep := False;
    -- I:= 7, 6, ... 0
    for I in reverse Thousand'Range loop
      if Img'Length >= I * 3 + 1 then
        -- This slice is covered
        declare
          Str : constant String := Make100 (Img(Start .. Stop));
        begin
          if Str /= "" then
            -- Append separator if needed
            -- Then the image of slice (from "one" to "nine hundred...")
            -- Then the multiplicator ("trillion", million"....)
            Txt.Append ( (if Sep then " " else "" )
                       & Str
                       & (if I /= 0 then " " & Thousand(I).all else "") );
            -- Next output will need a separation
            Sep := True;
          end if;
        end;
        -- Next slice
        Start := Stop + 1;
        Stop := Stop + 3;
      end if;
    end loop;

    return Txt.Image;
  end Letters_Of;

end Num_Letters;

