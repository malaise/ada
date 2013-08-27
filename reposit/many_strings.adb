package body Many_Strings is

  Sep : constant Character := Separator;

  -- Clear
  procedure Reset (Str : in out Many_String) is
  begin
    Str.Ustr.Set_Null;
  end Reset;
  function Empty return Many_String is
  begin
    return Empty_String;
  end Empty;

  -- True if Str is really empty (no separator nor string)
  function Is_Empty (Str : Many_String) return Boolean is
  begin
    return Str.Ustr.Is_Null;
  end Is_Empty;

  -- Init
  function Set (From : String) return Many_String is
  begin
    return (Ustr => As.U.Tus (From));
  end Set;
  function Set (From : As.U.Asu_Us) return Many_String is
  begin
    return (Ustr => From);
  end Set;
  function Set (From : As.U.Asu_Array) return Many_String is
    Result : Many_String := Empty_String;
  begin
    for I in From'Range loop
      Cat (Result, From(I));
    end loop;
    return Result;
  end Set;
  procedure Set (Str : in out Many_String; From : in String) is
  begin
    Str.Ustr := As.U.Tus (From);
  end Set;
  procedure Set (Str : in out Many_String; From : in As.U.Asu_Us) is
  begin
    Str.Ustr := From;
  end Set;
  procedure Set (Str : in out Many_String; From : in As.U.Asu_Array) is
  begin
    Reset (Str);
    for I in From'Range loop
      Cat (Str, From(I));
    end loop;
  end Set;

  -- Concatenation
  function Cat (Str : Many_String; What : String) return Many_String is
    use type As.U.Asu_Us;
  begin
    if Str.Ustr.Is_Null then
      return Set (What);
    elsif What = "" then
      return Str;
    else
      return (Ustr => Str.Ustr & Sep & What);
    end if;
  end Cat;
  function Cat (Str : Many_String; What : As.U.Asu_Us) return Many_String is
  begin
    return Cat (Str, What.Image);
  end Cat;
  function Cat (Str : Many_String; What : Many_String) return Many_String is
  begin
    return Cat (Str, What.Ustr.Image);
  end Cat;

  procedure Cat (Str : in out Many_String; What : in String) is
  begin
    Str := Cat (Str, What);
  end Cat;
  procedure Cat (Str : in out Many_String; What : in As.U.Asu_Us) is
  begin
    Str := Cat (Str, What.Image);
  end Cat;
  procedure Cat (Str : in out Many_String; What : in Many_String) is
  begin
    Str := Cat (Str, What.Ustr.Image);
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

  -- Split a Many_String into strings
  function Split (Str : Many_String) return As.U.Asu_Array is
    Empty : constant As.U.Asu_Array(1 .. 0) := (others => <>);
    Start : Natural;
    Nb : Positive;

  begin
    -- Specific to empty string
    if Str = Empty_String then
      return Empty;
    end if;

    -- Count Nb of separators
    Nb := 1;
    for I in 1 .. Str.Ustr.Length loop
      if Str.Ustr.Element (I) = Sep then
        Nb := Nb + 1;
      end if;
    end loop;

    declare
      Result : As.U.Asu_Array(1 .. Nb);
    begin
      -- Init index in result and start of next string
      Nb := 1;
      Start := 1;

      -- Next string
      for I in 1 .. Str.Ustr.Length loop
        if Str.Ustr.Element (I) = Sep then
          -- Store previous string
          Result(Nb) := Str.Ustr.Uslice (Start, I - 1);
          Nb := Nb + 1;
          Start := I + 1;
        end if;
        -- Handle end of string
        if I = Str.Ustr.Length then
          -- If Str ends by a Set then last element of Result is empty,
          --  which is already the case
          if Str.Ustr.Element (I) /= Sep then
            -- Str ends by a non Sep: append last string
            Result(Nb) := Str.Ustr.Uslice (Start, I);
          end if;
          exit;
        end if;
      end loop;
      return Result;
    end;
  end Split;

end Many_Strings;

