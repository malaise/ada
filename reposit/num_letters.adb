with As.U, Parser.Keywords, Trace.Loggers, Str_Util;
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
                new String'("trigintilliard"),
                new String'("untrigintillion"),
                new String'("untrigintilliard"),
                new String'("duotrigintillion"),
                new String'("duotrigintilliard"),
                new String'("tretrigintillion"),
                new String'("tretrigintilliard"),
                new String'("quattuortrigintillion"),
                new String'("quattuortrigintilliard"),
                new String'("quintrigintillion"),
                new String'("quintrigintilliard"),
                new String'("sextrigintillion"),
                new String'("sextrigintilliard"),
                new String'("septentrigintillion"),
                new String'("septentrigintilliard"),
                new String'("octotrigintillion"),
                new String'("octotrigintilliard"),
                new String'("novemtrigintillion"),
                new String'("novemtrigintilliard"),

                new String'("quatrigintillion"),
                new String'("quatrigintilliart"),
                new String'("unquatrigintillion"),
                new String'("unquatrigintilliard"),
                new String'("duoquatrigintillion"),
                new String'("duoquatrigintilliard"),
                new String'("trequatrigintillion"),
                new String'("trequatrigintilliard"),
                new String'("quattuorquatrigintillion"),
                new String'("quattuorquatrigintilliard"),
                new String'("quinquatrigintillion"),
                new String'("quinquatrigintilliard"),
                new String'("sexquatrigintillion"),
                new String'("sexquatrigintilliard"),
                new String'("septenquatrigintillion"),
                new String'("septenquatrigintilliard"),
                new String'("octoquatrigintillion"),
                new String'("octoquatrigintilliard"),
                new String'("novemquatrigintillion"),
                new String'("novemquatrigintilliard"),

                new String'("quinquagintillion"),
                new String'("quinquagintilliart"),
                new String'("unquinquagintillion"),
                new String'("unquinquagintilliard"),
                new String'("duoquinquagintillion"),
                new String'("duoquinquagintilliard"),
                new String'("trequinquagintillion"),
                new String'("trequinquagintilliard"),
                new String'("quattuorquinquagintillion"),
                new String'("quattuorquinquagintilliard"),
                new String'("quinquinquagintillion"),
                new String'("quinquinquagintilliard"),
                new String'("sexquinquagintillion"),
                new String'("sexquinquagintilliard"),
                new String'("septenquinquagintillion"),
                new String'("septenquinquagintilliard"),
                new String'("octoquinquagintillion"),
                new String'("octoquinquagintilliard"),
                new String'("novemquinquagintillion"),
                new String'("novemquinquagintilliard"),

                new String'("sexagintillion"),
                new String'("sexagintilliart"),
                new String'("unsexagintillion"),
                new String'("unsexagintilliard"),
                new String'("duosexagintillion"),
                new String'("duosexagintilliard"),
                new String'("tresexagintillion"),
                new String'("tresexagintilliard"),
                new String'("quattuorsexagintillion"),
                new String'("quattuorsexagintilliard"),
                new String'("quinsexagintillion"),
                new String'("quinsexagintilliard"),
                new String'("sexsexagintillion"),
                new String'("sexsexagintilliard"),
                new String'("septensexagintillion"),
                new String'("septensexagintilliard"),
                new String'("octosexagintillion"),
                new String'("octosexagintilliard"),
                new String'("novemsexagintillion"),
                new String'("novemsexagintilliard"),

                new String'("septuagintillion"),
                new String'("septuagintilliart"),
                new String'("unseptuagintillion"),
                new String'("unseptuagintilliard"),
                new String'("duoseptuagintillion"),
                new String'("duoseptuagintilliard"),
                new String'("treseptuagintillion"),
                new String'("treseptuagintilliard"),
                new String'("quattuorseptuagintillion"),
                new String'("quattuorseptuagintilliard"),
                new String'("quinseptuagintillion"),
                new String'("quinseptuagintilliard"),
                new String'("sexseptuagintillion"),
                new String'("sexseptuagintilliard"),
                new String'("septenseptuagintillion"),
                new String'("septenseptuagintilliard"),
                new String'("octoseptuagintillion"),
                new String'("octoseptuagintilliard"),
                new String'("novemseptuagintillion"),
                new String'("novemseptuagintilliard"),

                new String'("octogintillion"),
                new String'("octogintilliart"),
                new String'("unoctogintillion"),
                new String'("unoctogintilliard"),
                new String'("duooctogintillion"),
                new String'("duooctogintilliard"),
                new String'("treoctogintillion"),
                new String'("treoctogintilliard"),
                new String'("quattuoroctogintillion"),
                new String'("quattuoroctogintilliard"),
                new String'("quinoctogintillion"),
                new String'("quinoctogintilliard"),
                new String'("sexoctogintillion"),
                new String'("sexoctogintilliard"),
                new String'("septenoctogintillion"),
                new String'("septenoctogintilliard"),
                new String'("octooctogintillion"),
                new String'("octooctogintilliard"),
                new String'("novemoctogintillion"),
                new String'("novemoctogintilliard"),

                new String'("nonagintillion"),
                new String'("nonagintilliart"),
                new String'("unnonagintillion"),
                new String'("unnonagintilliard"),
                new String'("duononagintillion"),
                new String'("duononagintilliard"),
                new String'("trenonagintillion"),
                new String'("trenonagintilliard"),
                new String'("quattuornonagintillion"),
                new String'("quattuornonagintilliard"),
                new String'("quinnonagintillion"),
                new String'("quinnonagintilliard"),
                new String'("sexnonagintillion"),
                new String'("sexnonagintilliard"),
                new String'("septennonagintillion"),
                new String'("septennonagintilliard"),
                new String'("octononagintillion"),
                new String'("octononagintilliard"),
                new String'("novemnonagintillion"),
                new String'("novemnonagintilliard"),

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

  -- Logger (debug and errors)
  Logger : Trace.Loggers.Logger;
  -- Data stored for a word
  type Word_Kind_List is (Multiplicators,     -- "centillion" .. "thousand"
                          Hundred,            -- "hundred"
                          Ties,               -- "ninety" .. "twenty"
                          Teens,              -- "nineteen" .. "ten"
                          Units,              -- "nine" .. "one"
                          Not_Found,          -- keyword not found
                          End_Reached);       -- end of Words reached
  subtype Stored_Word_Kind_List is Word_Kind_List
                                   range Multiplicators .. Units;
  type Data_Rec is record
    Kind : Stored_Word_Kind_List;
    -- For a multiplicator, the power of ten, else 100, 90, 80 .. 20 .. 1
    Val  : Natural;
  end record;

  -- Hashed lists of words
  package Keys_Mng is new Parser.Keywords (Data_Rec);
  H_Long, H_Short : aliased Keys_Mng.Iterator;

  -- Parsing result handled internally
  type Word_Rec is record
    Kind : Word_Kind_List;
    Word : As.U.Asu_Us;
    Val : Natural;
  end record;

  function Next_Word (Iter : in out Keys_Mng.Iterator) return Word_Rec is
    Res : Keys_Mng.Result_Rec;
  begin
    Res := Iter.Next_Word;
    case Res.Kind is
      when Keys_Mng.Found =>
        Logger.Log_Debug ("Got keyword " & Res.Keyword.Image &  " "
                        & Res.Data.Kind'Img);
        return (Res.Data.Kind, Res.Keyword, Res.Data.Val);
      when Keys_Mng.Not_Found =>
        Logger.Log_Debug ("Got unknown word " & Res.Word.Image);
        return (Not_Found, Res.Word, 0);
      when Keys_Mng.End_Reached =>
        Logger.Log_Debug ("Got end reached");
        return (End_Reached, As.U.Asu_Null, 0);
    end case;
  end Next_Word;

  -- Init at first call
  procedure Init is
    Name : As.U.Asu_Us;
  begin
    if Logger.Is_Init then
      return;
    end if;
    -- Init logger
    Logger.Init ("Num_Letters");
    Logger.Add_Mask (Trace.Error);
    -- Init arrays
    -- Multiplicators decilliard | decillion .. thousand
    for I in 1 .. Thoulong'Last loop
      H_Long.Add  (Thoulong(I).all, (Multiplicators, I * 3));
    end loop;
    for I in 1 .. Thoushort'Last loop
      H_Short.Add (Thoushort(I).all, (Multiplicators, I * 3));
    end loop;
    -- Hundred
    H_Long.Add  ("hundred", (Hundred, 100));
    H_Short.Add ("hundred", (Hundred, 100));
    -- Ties: ninety .. twenty
    for I in Suffixes'Range loop
      if Suffixes(I) = null then
        -- Use name in Names
        Name := As.U.Tus (Names(I).all);
      else
        Name := As.U.Tus (Suffixes(I).all);
      end if;
      H_Long.Add  (Name.Image & "ty", (Ties, I * 10) );
      H_Short.Add (Name.Image & "ty", (Ties, I * 10) );
    end loop;
    -- Teens: nineteen .. thirteen, twelve, eleven, ten
    for I in 10 .. 12 loop
      H_Long.Add  (Names(I).all, (Teens, I) );
      H_Short.Add (Names(I).all, (Teens, I) );
    end loop;
    for I in 3 .. Suffixes'Last loop
      if Suffixes(I) = null then
        -- Use name in Names
        Name := As.U.Tus (Names(I).all);
      else
        Name := As.U.Tus (Suffixes(I).all);
      end if;
      H_Long.Add  (Name.Image & "teen", (Teens, 10 + I) );
      H_Short.Add (Name.Image & "teen", (Teens, 10 + I) );
    end loop;
    -- Units, not "zero", one .. nine
    for I in 1 .. 9 loop
      H_Long.Add  (Names(I).all, (Units, I) );
      H_Short.Add (Names(I).all, (Units, I) );
    end loop;
    -- End init
  end Init;

  -- Raises Constraint_Error is Words is an invalid number
  function Num_Of (Words : String; Scale : Scale_List := Long) return Number is
    Iter : access Keys_Mng.Iterator;
    Piter : Parser.Iterator;
    Prev_Multiplicator : Natural;
    Word, Tmp_Word : Word_Rec;
    Result : Number;
    Factor : Number;
    Done : Boolean;
    use type Number;
  begin
    -- Init result
    Result := Arbitrary.Zero;

    -- Handle empty input and "zero"
    if Words = "" then
      raise Constraint_Error;
    elsif Str_Util.Strip (Words, Str_Util.Both) = Names(0).all then
      return Result;
    end if;

    -- Init tables
    Init;
    -- Set iterators
    if Scale = Long then
      Iter := H_Long'Access;
    else
      Iter := H_Short'Access;
    end if;
    Piter.Set (Words);
    Iter.Set (Piter);

    -- Parse words, series of: Factor Multiplicator then [ Factor ]
    -- Factor = [ Units Hundred ] [ Ties [ Units ] | Teen | Units ]
    Prev_Multiplicator := Natural'Last;
    Word := Next_Word (Iter.all);
    loop
      -- Init factor
      Factor := Arbitrary.Zero;
      Done := False;

      exit when Word.Kind = End_Reached;
      if Word.Kind = Units then
        Tmp_Word := Next_Word (Iter.all);
        if Tmp_Word.Kind = Hundred then
          -- Units hundred
          Factor := Arbitrary.Set (Word.Val * Tmp_Word.Val);
          Word := Next_Word (Iter.all);
        else
          -- Units
          Factor := Arbitrary.Set (Word.Val);
          Word := Tmp_Word;
          Done := True;
        end if;
      end if;

      -- Ties?
      if not Done then
        if Word.Kind = Ties then
          -- Ties [ Units ]
          Tmp_Word := Next_Word (Iter.all);
          Factor := Factor + Arbitrary.Set (Word.Val);
          if Tmp_Word.Kind = Units then
            Factor := Factor + Arbitrary.Set (Tmp_Word.Val);
            Word := Next_Word (Iter.all);
          else
            Word := Tmp_Word;
          end if;
        elsif Word.Kind = Teens then
          -- Teens
          Factor := Factor + Arbitrary.Set (Word.Val);
          Word := Next_Word (Iter.all);
        elsif Word.Kind = Units then
          -- Units
          Factor := Factor + Arbitrary.Set (Word.Val);
          Word := Next_Word (Iter.all);
        elsif Word.Kind /= Multiplicators
        and then Word.Kind /= End_Reached then
          Logger.Log_Error ("Unexpected word " & Word.Word.Image);
          raise Constraint_Error;
        end if;
      end if;

      -- Check that a factor is set
      if Factor = Arbitrary.Zero then
        Logger.Log_Error ("Missing factor at " & Word.Word.Image);
        raise Constraint_Error;
      end if;

      -- Multiplicator?
      if Word.Kind = Multiplicators then
        -- Check that multiplicators are decreasing
        if Word.Val >= Prev_Multiplicator then
          Logger.Log_Error ("Crescent multiplicator " & Word.Word.Image);
          raise Constraint_Error;
        end if;
        -- Muliplicator is a power of 10
        Factor := Factor
               * Arbitrary.Set (Integer'(10)) ** Arbitrary.Set (Word.Val);
        Word := Next_Word (Iter.all);
      elsif Word.Kind /= End_Reached then
        Logger.Log_Error ("Unexpected word " & Word.Word.Image);
          raise Constraint_Error;
      end if;

      -- Add
      Result := Result + Factor;

      -- Next word
      exit when Word.Kind = End_Reached;
    end loop;

    return Result;
  end Num_Of;
end Num_Letters;

