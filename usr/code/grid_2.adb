package body Grid_2 is
  use My_Math;

  subtype Long_Natural is My_Math.Inte range 0 .. My_Math.Inte'Last;

  Dimension : My_Math.Inte;
  First_Row : My_Math.Inte;
  First_Col : My_Math.Inte;
  Last_Row : My_Math.Inte;
  Last_Col : My_Math.Inte;

  procedure Initialize (Key_Length, Text_Length : in My_Math.Inte) is
    R : My_Math.Real;

  begin
    R := My_Math.Real(Key_Length) + My_Math.Real(Text_Length);
    Dimension := My_Math.Inte(Trunc(My_Math.Sqrt(R))) + 1;
    First_Row := Key_Length / Dimension + 1;
    First_Col := Key_Length mod Dimension + 1;
    Last_Row :=
         Key_Length / Dimension
       + Text_Length / Dimension
       + (Key_Length mod Dimension + Text_Length mod Dimension) / Dimension
       + 1;
    Last_Col :=
         (Key_Length mod Dimension + Text_Length mod Dimension) mod Dimension
       + 1;
    -- So far , last row, col is first empty slot
    if Last_Col /= 1 then
      Last_Col := Last_Col - 1;
    else
      Last_Row := Last_Row - 1;
      Last_Col := Dimension;
    end if;
  end Initialize;

  function Index (R, C : My_Math.Inte; Encode : Boolean)
                 return Long_Natural is
    N : Long_Natural;
  begin
    if R < First_Row then
      return 0;
    elsif R = First_Row and then C < First_Col then
      return 0;
    elsif R = Last_Row and then C > Last_Col then
      return 0;
    elsif R > Last_Row then
      return 0;
    else
      -- In effective table
      if Encode then
        -- Substract key offset
        return (R - First_Row) * Dimension + C - First_Col + 1;
      else
        N := 0;
        for I in 1 .. C - 1 loop
          -- General column amount
          N := N - (First_Row - 1) + Last_Row;
          if I < First_Col then
            -- Before first col, so -1
            N := N - 1;
          end if;
          if I > Last_Col then
            -- After last col, so -1
            N := N - 1;
          end if;
        end loop;
        -- Add key offset (raow part)
        N := N + R - First_Row;
        if C < First_Col then
          -- Current col is before first col, so -1
          N := N - 1;
        end if;
        -- Always add 1
        N := N + 1;
        return N;
      end if;
    end if;
  end Index;


  function Encode (Key : in String; Text : Long_String)
           return Long_String is
    Str : Long_String (1 .. Text'Length);
    I : Long_Positive;
    J : Long_Natural;
  begin
    Initialize (Key'Length, Text'Length);

    I := 1;
    for C in 1 .. Dimension loop
      for R in 1 .. Dimension loop
        J := Index (R, C, True);
        if J /= 0 then
          Str(I) := Text(J);
          I := I + 1;
        end if;
      end loop;
    end loop;
    return Str;
  end Encode;

  function Decode (Key : in String; Text : Long_String)
           return Long_String is
    Str : Long_String (1 .. Text'Length);
    I : Long_Positive;
    J : Long_Natural;
  begin
    Initialize (Key'Length, Text'Length);

    I := 1;
    for R in 1 .. Dimension loop
      for C in 1 .. Dimension loop
        J := Index (R, C, False);
        if J /= 0 then
          Str(I) := Text(J);
          I := I + 1;
        end if;
      end loop;
    end loop;
    return Str;
  end Decode;

end Grid_2;

