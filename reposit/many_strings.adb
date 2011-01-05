package body Many_Strings is

  Sep : constant Character := Separator;

  -- Clear
  procedure Reset (Str : in out Many_String) is
  begin
    Str.Ustr := As.U.Asu_Null;
  end Reset;
  function Empty return Many_String is
    Str : Many_String;
  begin
    return Str;
  end Empty;

  -- Init
  function Set (To : String) return Many_String is
  begin
    return (Ustr => As.U.Tus (To));
  end Set;
  function Set (To : As.U.Asu_Us) return Many_String is
  begin
    return (Ustr => To);
  end Set;
  procedure Set (Str : in out Many_String; To : in String) is
  begin
    Str.Ustr := As.U.Tus (To);
  end Set;
  procedure Set (Str : in out Many_String; To : in As.U.Asu_Us) is
  begin
    Str.Ustr := To;
  end Set;

  -- Concatenation
  function Cat (Str : Many_String; What : String) return Many_String is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      return (Ustr => As.U.Tus (What));
    else
      return (Ustr => Str.Ustr & As.U.Tus (Sep) & What);
    end if;
  end Cat;
  function Cat (Str : Many_String; What : As.U.Asu_Us) return Many_String is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      return (Ustr => What);
    else
      return (Ustr => Str.Ustr & As.U.Tus (Sep) & What);
    end if;
  end Cat;
  function Cat (Str : Many_String; What : Many_String) return Many_String is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      return What;
    else
      return (Ustr => Str.Ustr & As.U.Tus (Sep) & What.Ustr);
    end if;
  end Cat;

  procedure Cat (Str : in out Many_String; What : in String) is
  begin
    if Str.Ustr.Is_Null then
      Str.Ustr := As.U.Tus (What);
    else
      Str.Ustr.Append (Sep & What);
    end if;
  end Cat;
  procedure Cat (Str : in out Many_String; What : in As.U.Asu_Us) is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      Str.Ustr := What;
    else
      Str.Ustr.Append (Sep & What);
    end if;
  end Cat;
  procedure Cat (Str : in out Many_String; What : in Many_String) is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      Str := What;
    else
      Str.Ustr.Append (Sep & What.Ustr);
    end if;
  end Cat;

  -- String image
  function Image (Str : Many_String) return String is
  begin
    return Str.Ustr.Image;
  end Image;
  function Image (Str : Many_String) return As.U.Asu_Us is
  begin
    return Str.Ustr;
  end Image;

  -- Decode
  function Nb (Str : Many_String) return Positive is
    N : Natural;
  begin
    N := 1;
    for I in 1 .. Str.Ustr.Length loop
      if Str.Ustr.Element (I) = Sep then
        N := N + 1;
      end if;
    end loop;
    return N;
  end Nb;

  -- Internal: get indexes in Str of Nth substring
  procedure Indexes (Str : in String; N : in Positive;
                     Start : out Positive; Stop : out Natural) is
    Last : constant Natural := Str'Last;

    -- Returns 0 if not found
    function Next (I : Natural) return Natural is
    begin
      for J in I .. Last loop
        if Str(J) = Sep then
          return J;
        end if;
      end loop;
      return 0;
    end Next;

  begin
    -- Specific to empty string
    if Str = "" then
      if N = 1 then
        Start := 1;
        Stop := 0;
        return;
      else
        raise String_Error;
      end if;
    end if;

    -- Init search of string
    Start := 1;

    -- Next string
    for Num in 1 .. N loop
      Stop := Next (Start);
      exit when Num = N;
      -- Check for end of string
      if Stop = 0 then
        raise String_Error;
      end if;
      Start := Stop + 1;
    end loop;

    -- Got it. Stop means Last or a Sep
    if Stop = 0 then
      Stop := Last;
    else
      Stop := Stop - 1;
    end if;
  end Indexes;

  function Nth (Str : Many_String; N : Positive) return String is
    Start : Positive;
    Stop : Natural;
  begin
    Indexes (Str.Ustr.Image, N, Start, Stop);
    if Stop = 0 then
      return "";
    else
      return Str.Ustr.Slice (Start, Stop);
    end if;
  end Nth;
  function Nth (Str : Many_String; N : Positive) return As.U.Asu_Us is
    Start : Positive;
    Stop : Natural;
  begin
    Indexes (Str.Ustr.Image, N, Start, Stop);
    if Stop = 0 then
      return As.U.Asu_Null;
    else
      return Str.Ustr.Uslice (Start, Stop);
    end if;
  end Nth;

end Many_Strings;

