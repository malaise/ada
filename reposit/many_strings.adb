package body Many_Strings is

  Sep : constant Character := Separator;

  -- Clear
  procedure Reset (Str : in out Many_String) is
  begin
    Str.Ustr := Asu_Null;
  end Reset;
  function Empty return Many_String is
    Str : Many_String;
  begin
    return Str;
  end Empty;

  -- Init
  function Set (To : String) return Many_String is
  begin
    return (Ustr => Asu_Tus (To));
  end Set;
  function Set (To : Asu_Us) return Many_String is
  begin
    return (Ustr => To);
  end Set;
  procedure Set (Str : in out Many_String; To : in String) is
  begin
    Str.Ustr := Asu_Tus (To);
  end Set;
  procedure Set (Str : in out Many_String; To : in Asu_Us) is
  begin
    Str.Ustr := To;
  end Set;

  -- Concatenation
  function Cat (Str : Many_String; What : String) return Many_String is
    use type Asu_Us;
  begin
    if Asu_Is_Null (Str.Ustr) then
      return (Ustr => Asu_Tus (What));
    else
      return (Ustr => Str.Ustr & Asu_Tus (Sep) & What);
    end if;
  end Cat;
  function Cat (Str : Many_String; What : Asu_Us) return Many_String is
    use type Asu_Us;
  begin
    if Asu_Is_Null (Str.Ustr) then
      return (Ustr => What);
    else
      return (Ustr => Str.Ustr & Asu_Tus (Sep) & What);
    end if;
  end Cat;
  function Cat (Str : Many_String; What : Many_String) return Many_String is
    use type Asu_Us;
  begin
    if Asu_Is_Null (Str.Ustr) then
      return What;
    else
      return (Ustr => Str.Ustr & Asu_Tus (Sep) & What.Ustr);
    end if;
  end Cat;

  procedure Cat (Str : in out Many_String; What : in String) is
  begin
    if Asu_Is_Null (Str.Ustr) then
      Str.Ustr := Asu_Tus (What);
    else
      Asu.Append (Str.Ustr, Sep & What);
    end if;
  end Cat;
  procedure Cat (Str : in out Many_String; What : in Asu_Us) is
    use type Asu_Us;
  begin
    if Asu_Is_Null (Str.Ustr) then
      Str.Ustr := What;
    else
      Asu.Append (Str.Ustr, Sep & What);
    end if;
  end Cat;
  procedure Cat (Str : in out Many_String; What : in Many_String) is
    use type Asu_Us;
  begin
    if Asu_Is_Null (Str.Ustr) then
      Str := What;
    else
      Asu.Append (Str.Ustr, Sep & What.Ustr);
    end if;
  end Cat;

  -- String image
  function Image (Str : Many_String) return String is
  begin
    return Asu_Ts (Str.Ustr);
  end Image;
  function Image (Str : Many_String) return Asu_Us is
  begin
    return Str.Ustr;
  end Image;

  -- Decode
  function Nb (Str : Many_String) return Positive is
    N : Natural;
  begin
    N := 1;
    for I in 1 .. Asu.Length (Str.Ustr) loop
      if Asu.Element (Str.Ustr, I) = Sep then
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
    Indexes (Asu_Ts (Str.Ustr), N, Start, Stop);
    if Stop = 0 then
      return "";
    else
      return Asu.Slice (Str.Ustr, Start, Stop);
    end if;
  end Nth;
  function Nth (Str : Many_String; N : Positive) return Asu_Us is
    Start : Positive;
    Stop : Natural;
  begin
    Indexes (Asu_Ts (Str.Ustr), N, Start, Stop);
    if Stop = 0 then
      return Asu_Null;
    else
      return Asu.Unbounded_Slice (Str.Ustr, Start, Stop);
    end if;
  end Nth;

end Many_Strings;

