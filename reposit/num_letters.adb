with As.U;
package body Num_Letters is

  -- Conversion Number -> letters
  --  e.g. 9789  nine thousand seven hundred eighty nine

  type Name_Access is access String;
  type Name_Array is array (Natural range <>) of Name_Access;
  type Name_Arrays_Access is access all Name_Array;

  -- Will access long or short scale
  Thousand : Name_Arrays_Access;

  -- Stores "zero", "one" .... "twelve"
  Names : constant Name_Array := (
                new String'("zero"),
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
  -- Stores "thir" and "fif" (others are left null)
  Suffixes : constant Name_Array := (
           2 => new String'("twen"),
           3 => new String'("thir"),
           5 => new String'("fif"),
           8 => new String'("eigh"),
           1|4|6|7|9 => null );
  -- Stores "teen" "ty" " hundred"
  Tenth : constant Name_Array := (
                new String'("teen"),
                new String'("ty"),
                new String'("hundred") );

  -- Multiplicators long scale
  Thoulong : aliased Name_Array := (
                new String'(""),
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
                new String'("decilliard"),
                new String'("undecillion"),
                new String'("undecilliard"),
                new String'("duodecillion"),
                new String'("duodecilliard"),
                new String'("tredecillion"),
                new String'("tredecilliard"),
                new String'("quattuordecillion"),
                new String'("quattuordecilliard"),
                new String'("quindecillion"),
                new String'("quindecilliard"),
                new String'("sexdecillion"),
                new String'("sexdecilliard"),
                new String'("septendecillion"),
                new String'("septendecilliard"),
                new String'("octodecillion"),
                new String'("octodecilliard"),
                new String'("nonidecillion"),
                new String'("nonidecilliard"),

                new String'("vigentillion"),
                new String'("vigentilliard"),
                new String'("unvigentillion"),
                new String'("unvigentilliard"),
                new String'("duovigentillion"),
                new String'("duovigentilliard"),
                new String'("trevigentillion"),
                new String'("trevigentilliard"),
                new String'("quattuorvigentillion"),
                new String'("quattuorvigentilliard"),
                new String'("quinvigentillion"),
                new String'("quinvigentilliard"),
                new String'("sexvigentillion"),
                new String'("sexvigentilliard"),
                new String'("septenvigentillion"),
                new String'("septenvigentilliard"),
                new String'("octovigentillion"),
                new String'("octovigentilliard"),
                new String'("novemvigentillion"),
                new String'("novemvigentilliard"),

                new String'("trigintillion"),
                new String'("trigintillard"),
                new String'("untrigintillion"),
                new String'("untrigintillard"),
                new String'("duotrigintillion"),
                new String'("duotrigintillard"),
                new String'("tretrigintillion"),
                new String'("tretrigintillard"),
                new String'("quattuortrigintillion"),
                new String'("quattuortrigintillard"),
                new String'("quintrigintillion"),
                new String'("quintrigintillard"),
                new String'("sextrigintillion"),
                new String'("sextrigintillard"),
                new String'("septentrigintillion"),
                new String'("septentrigintillard"),
                new String'("octotrigintillion"),
                new String'("octotrigintillard"),
                new String'("novemtrigintillion"),
                new String'("novemtrigintillard"),

                new String'("quatrigintillion"),
                new String'("quatrigintilliart"),
                new String'("unquatrigintillion"),
                new String'("unquatrigintillard"),
                new String'("duoquatrigintillion"),
                new String'("duoquatrigintillard"),
                new String'("trequatrigintillion"),
                new String'("trequatrigintillard"),
                new String'("quattuorquatrigintillion"),
                new String'("quattuorquatrigintillard"),
                new String'("quinquatrigintillion"),
                new String'("quinquatrigintillard"),
                new String'("sexquatrigintillion"),
                new String'("sexquatrigintillard"),
                new String'("septenquatrigintillion"),
                new String'("septenquatrigintillard"),
                new String'("octoquatrigintillion"),
                new String'("octoquatrigintillard"),
                new String'("novemquatrigintillion"),
                new String'("novemquatrigintillard"),

                new String'("quinquagintillion"),
                new String'("quinquagintilliart"),
                new String'("unquinquagintillion"),
                new String'("unquinquagintillard"),
                new String'("duoquinquagintillion"),
                new String'("duoquinquagintillard"),
                new String'("trequinquagintillion"),
                new String'("trequinquagintillard"),
                new String'("quattuorquinquagintillion"),
                new String'("quattuorquinquagintillard"),
                new String'("quinquinquagintillion"),
                new String'("quinquinquagintillard"),
                new String'("sexquinquagintillion"),
                new String'("sexquinquagintillard"),
                new String'("septenquinquagintillion"),
                new String'("septenquinquagintillard"),
                new String'("octoquinquagintillion"),
                new String'("octoquinquagintillard"),
                new String'("novemquinquagintillion"),
                new String'("novemquinquagintillard"),

                new String'("sexagintillion"),
                new String'("sexagintilliart"),
                new String'("unsexagintillion"),
                new String'("unsexagintillard"),
                new String'("duosexagintillion"),
                new String'("duosexagintillard"),
                new String'("tresexagintillion"),
                new String'("tresexagintillard"),
                new String'("quattuorsexagintillion"),
                new String'("quattuorsexagintillard"),
                new String'("quinsexagintillion"),
                new String'("quinsexagintillard"),
                new String'("sexsexagintillion"),
                new String'("sexsexagintillard"),
                new String'("septensexagintillion"),
                new String'("septensexagintillard"),
                new String'("octosexagintillion"),
                new String'("octosexagintillard"),
                new String'("novemsexagintillion"),
                new String'("novemsexagintillard"),

                new String'("septuagintillion"),
                new String'("septuagintilliart"),
                new String'("unseptuagintillion"),
                new String'("unseptuagintillard"),
                new String'("duoseptuagintillion"),
                new String'("duoseptuagintillard"),
                new String'("treseptuagintillion"),
                new String'("treseptuagintillard"),
                new String'("quattuorseptuagintillion"),
                new String'("quattuorseptuagintillard"),
                new String'("quinseptuagintillion"),
                new String'("quinseptuagintillard"),
                new String'("sexseptuagintillion"),
                new String'("sexseptuagintillard"),
                new String'("septenseptuagintillion"),
                new String'("septenseptuagintillard"),
                new String'("octoseptuagintillion"),
                new String'("octoseptuagintillard"),
                new String'("novemseptuagintillion"),
                new String'("novemseptuagintillard"),

                new String'("octogintillion"),
                new String'("octogintilliart"),
                new String'("unoctogintillion"),
                new String'("unoctogintillard"),
                new String'("duooctogintillion"),
                new String'("duooctogintillard"),
                new String'("treoctogintillion"),
                new String'("treoctogintillard"),
                new String'("quattuoroctogintillion"),
                new String'("quattuoroctogintillard"),
                new String'("quinoctogintillion"),
                new String'("quinoctogintillard"),
                new String'("sexoctogintillion"),
                new String'("sexoctogintillard"),
                new String'("septenoctogintillion"),
                new String'("septenoctogintillard"),
                new String'("octooctogintillion"),
                new String'("octooctogintillard"),
                new String'("novemoctogintillion"),
                new String'("novemoctogintillard"),

                new String'("nonagintillion"),
                new String'("nonagintilliart"),
                new String'("unnonagintillion"),
                new String'("unnonagintillard"),
                new String'("duononagintillion"),
                new String'("duononagintillard"),
                new String'("trenonagintillion"),
                new String'("trenonagintillard"),
                new String'("quattuornonagintillion"),
                new String'("quattuornonagintillard"),
                new String'("quinnonagintillion"),
                new String'("quinnonagintillard"),
                new String'("sexnonagintillion"),
                new String'("sexnonagintillard"),
                new String'("septennonagintillion"),
                new String'("septennonagintillard"),
                new String'("octononagintillion"),
                new String'("octononagintillard"),
                new String'("novemnonagintillion"),
                new String'("novemnonagintillard"),

                new String'("centillion"),
                new String'("centilliard") );

  -- Multiplicators short scale
  Thoushort : aliased Name_Array := (
                new String'(""),
                new String'("thousand"),
                new String'("million"),
                new String'("billion"),
                new String'("trillion"),
                new String'("quadrillion"),
                new String'("quintillion"),
                new String'("sextillion"),
                new String'("septillion"),
                new String'("octillion"),
                new String'("nonillion"),

                new String'("decillion"),
                new String'("undecillion"),
                new String'("duodecillion"),
                new String'("tredecillion"),
                new String'("quattuordecillion"),
                new String'("quindecillion"),
                new String'("sexdecillion"),
                new String'("septendecillion"),
                new String'("octodecillion"),
                new String'("novemdecillion"),

                new String'("vigintillion"),
                new String'("unvigintillion"),
                new String'("duovigintillion"),
                new String'("trevigintillion"),
                new String'("quattuorvigintillion"),
                new String'("quinvigintillion"),
                new String'("sexvigintillion"),
                new String'("septenvigintillion"),
                new String'("octovigintillion"),
                new String'("novemvigintillion"),

                new String'("trigintillion"),
                new String'("untrigintillion"),
                new String'("duotrigintillion"),
                new String'("tretrigintillion"),
                new String'("quattuortrigintillion"),
                new String'("quintrigintillion"),
                new String'("sextrigintillion"),
                new String'("septentrigintillion"),
                new String'("octotrigintillion"),
                new String'("novemtrigintillion"),

                new String'("quatrigintillion"),
                new String'("unquatrigintillion"),
                new String'("duoquatrigintillion"),
                new String'("trequatrigintillion"),
                new String'("quattuorquatrigintillion"),
                new String'("quinquatrigintillion"),
                new String'("sexquatrigintillion"),
                new String'("septenquatrigintillion"),
                new String'("octoquatrigintillion"),
                new String'("novemquatrigintillion"),

                new String'("quinquagintillion"),
                new String'("unquinquagintillion"),
                new String'("duoquinquagintillion"),
                new String'("trequinquagintillion"),
                new String'("quattuorquinquagintillion"),
                new String'("quinquinquagintillion"),
                new String'("sexquinquagintillion"),
                new String'("septenquinquagintillion"),
                new String'("octoquinquagintillion"),
                new String'("novemquinquagintillion"),

                new String'("sexagintillion"),
                new String'("unsexagintillion"),
                new String'("duosexagintillion"),
                new String'("tresexagintillion"),
                new String'("quattuorsexagintillion"),
                new String'("quinsexagintillion"),
                new String'("sexsexagintillion"),
                new String'("septensexagintillion"),
                new String'("octosexagintillion"),
                new String'("novemsexagintillion"),

                new String'("septuagintillion"),
                new String'("unseptuagintillion"),
                new String'("duoseptuagintillion"),
                new String'("treseptuagintillion"),
                new String'("quattuorseptuagintillion"),
                new String'("quinseptuagintillion"),
                new String'("sexseptuagintillion"),
                new String'("septenseptuagintillion"),
                new String'("octoseptuagintillion"),
                new String'("novemseptuagintillion"),

                new String'("octogintillion"),
                new String'("unoctogintillion"),
                new String'("duooctogintillion"),
                new String'("treoctogintillion"),
                new String'("quattuoroctogintillion"),
                new String'("quinoctogintillion"),
                new String'("sexoctogintillion"),
                new String'("septenoctogintillion"),
                new String'("octooctogintillion"),
                new String'("novemoctogintillion"),

                new String'("nonagintillion"),
                new String'("unnonagintillion"),
                new String'("duononagintillion"),
                new String'("trenonagintillion"),
                new String'("quattuornonagintillion"),
                new String'("quinnonagintillion"),
                new String'("sexnonagintillion"),
                new String'("septennonagintillion"),
                new String'("octononagintillion"),
                new String'("novemnonagintillion"),

                new String'("centillion") );

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

  function Letters_Of (N : Number; Scale : Scale_List := Long) return String is
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
    -- Handle specific case of 0 => "zero"
    if N = Arbitrary.Zero then
      return Names(0).all;
    end if;

    -- Access to the proper scale
    if Scale = Long then
      Thousand := Thoulong'Access;
    else
      Thousand := Thoushort'Access;
    end if;

    -- Check that N is within range
    -- Thousand'Length = 1 -> Max is 10^4, 2 -> 10^7
    if Img'Length > Thousand'Length * 3 then
      raise Constraint_Error;
    end if;

    -- Slices of 3 digits, 1..3, 4..6, ...
    Start := 1;
    -- Depending on len, Stop is: for 1->1, 2->2, 3->3, 4->1, 5->2...
    Stop := (Img'Length - 1) rem 3 + 1;
    -- Initially no separator
    Sep := False;
    -- I:= 7, 6, ... 0
    for I in reverse Thousand.all'Range loop
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

  -- Raises Constraint_Error is Words is an invalid number
  function Num_Of (Words : String; Scale : Scale_List := Long) return Number is
  begin
    return Arbitrary.Zero;
  end Num_Of;
end Num_Letters;

