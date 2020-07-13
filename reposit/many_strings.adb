package body Many_Strings is

  -- Clear
  procedure Reset (Str : in out Many_String) is
  begin
    Str := Empty_String;
  end Reset;
  function Empty return Many_String is (Empty_String);

  -- True if Str is really empty (no separator nor string)
  function Is_Empty (Str : Many_String) return Boolean is (Str = Empty_String);

  subtype Unb_Array is Asu_Ua.Unb_Array;

  -- Normalize a Many_String
  -- Split any substring delimited by Separator
  procedure Normalize (M : in out Many_String) is
    Index : Natural;
    Len : Natural;
    Str : As.U.Asu_Us;
  begin
    -- Scan all Unb strings
    Index := 1;
    loop
      -- Look for separator
      Len := M.Element (Index).Length;
      A_String : for I in 1 .. Len loop
        if M.Element(Index).Element(I) = Separator then
          -- Insert tail after current Unb string
          Str := M.Element(Index);
          M.Insert (Index + 1, Str.Uslice (I + 1, Len));
          -- Delete separator and tail from current
          Str.Delete (I, Len);
          M.Replace_Element (Index, Str);
          -- Switch to next (new) Unb
          exit A_String;
        end if;
      end loop A_String;
      exit when Index = M.Length;
      Index := Index + 1;
    end loop;
  end Normalize;

  -- Init functions, rely on procedures
  function Set (From : String) return Many_String is
    Result : Many_String;
  begin
    Set (Result, From);
    return Result;
  end Set;
  function Set (From : As.U.Asu_Us) return Many_String is
    Result : Many_String;
  begin
    Set (Result, From);
    return Result;
  end Set;
  function Set (From : As.U.Asu_Array) return Many_String is
    Result : Many_String;
  begin
    Set (Result, From);
    return Result;
  end Set;

  -- Init procedures
  procedure Set (Str : out Many_String; From : in String) is
  begin
    Set (Str, As.U.Tus (From));
  end Set;
  -- The two Set operations that do something
  overriding procedure Set (Str : out Many_String;
                            From : in As.U.Asu_Us) is
  begin
    Unb_Array(Str).Set (From);
    Normalize (Str);
  end Set;
  overriding procedure Set (Str : out Many_String;
                            From : in As.U.Asu_Array) is
  begin
    Reset (Str);
    for C of From loop
      Str.Append (C);
    end loop;
    Normalize (Str);
  end Set;

  -- Concatenation functions, rely on procedures
  function Cat (Str : Many_String; What : String) return Many_String is
    Result : Many_String := Str;
  begin
    Cat (Result, What);
    return Result;
  end Cat;
  function Cat (Str : Many_String; What : As.U.Asu_Us) return Many_String is
    Result : Many_String := Str;
  begin
    Cat (Result, What);
    return Result;
  end Cat;
  function Cat (Str : Many_String; What : Many_String) return Many_String is
    Result : Many_String := Str;
  begin
    Cat (Result, What);
    return Result;
  end Cat;

  -- Concatenation procedures
  procedure Cat (Str : in out Many_String; What : in String) is
  begin
    Cat (Str, As.U.Tus (What));
  end Cat;
  procedure Cat (Str : in out Many_String; What : in As.U.Asu_Us) is
    Tmp : constant Many_String := Set (What);
  begin
    Cat (Str, Tmp);
  end Cat;
  procedure Cat (Str : in out Many_String; What : in Many_String) is
  begin
    Str.Append (What);
  end Cat;

  -- Same, but do nothing of What is empty
  -- Catif functions, rely on procedures
  function Catif (Str : Many_String; What : String) return Many_String is
    Result : Many_String := Str;
  begin
    Catif (Result, What);
    return Result;
  end Catif;
  function Catif (Str : Many_String; What : As.U.Asu_Us) return Many_String is
    Result : Many_String := Str;
  begin
    Catif (Result, What);
    return Result;
  end Catif;
  function Catif (Str : Many_String; What : Many_String) return Many_String is
    Result : Many_String := Str;
  begin
    Catif (Result, What);
    return Result;
  end Catif;

  -- Catif procedures
  procedure Catif (Str : in out Many_String; What : in String) is
  begin
    Str := Catif (Str, As.U.Tus (What));
  end Catif;
  procedure Catif (Str : in out Many_String; What : in As.U.Asu_Us) is
  begin
    if not What.Is_Null then
      Cat (Str, What);
    end if;
  end Catif;
  procedure Catif (Str : in out Many_String; What : in Many_String) is
  begin
    if not What.Is_Null then
      Cat (Str, What);
    end if;
  end Catif;

  -- String image
  function Image (Str : Many_String) return String is (Image (Str, Separator));
  function Image (Str : Many_String) return As.U.Asu_Us is
    (Image (Str, Separator));
  function Image (Str : Many_String; Separator : in Character) return String is
    (Image(Str, Separator).Image);

  function Image (Str : Many_String; Separator : in Character)
           return As.U.Asu_Us is
    Result : As.U.Asu_Us;
  begin
    for I in 1 .. Str.Length loop
      if I /= 1 then
        Result.Append (Separator);
      end if;
      Result.Append (Str.Element (I));
    end loop;
    return Result;
  end Image;

  -- Count number of substrings
  function Nb (Str : Many_String) return Positive is
    Len : constant Natural := Str.Length;
  begin
    return (if Len /= 0 then Len else 1);
  end Nb;

  function Nth (Str : Many_String; N : Positive) return String is
    (Nth (Str, N).Image);
  function Nth (Str : Many_String; N : Positive) return As.U.Asu_Us is
  begin
    if Str.Length /= 0 then
      return Str.Element (N);
    else
     return As.U.Asu_Null;
    end if;
  exception
    when Asu_Ua.Index_Error =>
      raise String_Error;
  end Nth;

  -- Split a Many_String into strings
  function Split (Str : Many_String) return As.U.Asu_Array is
  begin
    if Str.Length /= 0 then
      return Str.To_Array;
    else
      return Result : constant As.U.Asu_Array (1 .. 1)
                    := (others => As.U.Asu_Null) do
        null;
      end return;
    end if;
  end Split;

end Many_Strings;

