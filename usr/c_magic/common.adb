with Ada.Calendar;
with Basic_Proc, Normal, Sorts, My_Math;
package body Common is
  -- The dimension of square
  Dim : Dim_Range;
  Dm1 : Natural;

  -- Array of content of square
  Max_Len : constant := Max_Dim * Max_Dim;
  subtype Len_Range is Positive range 1 .. Max_Len;

  -- Curent length of array, current array
  Len : Len_Range;
  type Lis_Array is array (Len_Range range <>) of Len_Range;
  Lis : Lis_Array (Len_Range);

  -- Expected sum of each row, column and diag
  Sigma : Positive;
  -- Number of magic squares found
  Nb_Square : Natural;

  -- Real -> integer : round or trunc
  function Trunc (X : in Float) return Integer is
    (Integer(My_Math.Trunc (My_Math.Real(X))));

  function Round (X : in Float) return Integer is
    (Integer(My_Math.Round (My_Math.Real(X))));

  function Frac (X : in Float) return Float is
    (Float(My_Math.Frac (My_Math.Real(X))));

  -- Recursive procedure to try a a level
  procedure Try (Cur : Len_Range);

  -- Init of array and file and start first try
  procedure Search (Dim : in Dim_Range) is
    Start_Time : Ada.Calendar.Time;
    Search_Duration : Float;
    use type Ada.Calendar.Time;
  begin
    Common.Dim := Dim;
    Dm1 := Dim - 1;

    -- Compute len and sigma : sigma := (len * (len+1)) / 2 / dim
    Len := Dim * Dim;
    Sigma := (Dim * (Len + 1) ) / 2;

    -- Initialise array
    for I in 1 .. Len loop
      Lis(I) := I;
    end loop;
    Nb_Square := 0;

    -- Start searching
    Start_Time := Ada.Calendar.Clock;
    Try(1);
    Search_Duration := Float (Ada.Calendar.Clock - Start_Time);

    -- Done
    Basic_Proc.Put_Line_Output (Natural'Image(Nb_Square)
                   &  " squares of "
                   & Dim_Range'Image(Dim)
                   & " found in "
                   & Natural'Image(Trunc(Search_Duration))
                   & "."
                   & Normal (Round(Frac(Search_Duration) * 1000.0), 3, Gap => '0')
                   & " s." );

  end Search;

  -- Check if, up to N, the array content may be a magic square
  function Check (N : Len_Range) return Boolean is separate;

  -- Display and log array (square) content
  procedure Dump;

  -- To sort a part of the array
  package Sort is new Sorts (
   Typ_Object => Len_Range,
   Typ_Index  => Len_Range, --## rule line off Generic_Aliasing
   "<"        => "<",
   Typ_Array  => Lis_Array);

  -- Supposing that Lis is sorted from Cur to Len
  -- Tries all possibilities of numbers in Lis(Cur) .. Lis(Len)
  procedure Try (Cur : Len_Range) is
    Rest : Len_Range;
    Tmp : Len_Range;
  begin

    if Cur = Len then
      -- Square is complete
      if Check(Len) then
        -- Square is complete and magic
        Nb_Square := Nb_Square + 1;
        Dump;
      end if;
      -- No more possibility
      return;
    end if;


    -- We will put, at Cur position, one after one, each  number of
    -- Lis(Cur) .. Lis(Len)
    Rest := Cur;

    loop

      -- No change for try with Rest = Cur : Lis(Cur)
      if Rest /= Cur then
        -- Exchange Lis(Cur) <-> Lis(Rest)
        Tmp := Lis(Cur);
        Lis(Cur) := Lis(Rest);
        Lis(Rest) := Tmp;

        -- Sort Cur+1 .. Len for try at Cur+1
        Sort.Quick_Sort(Lis(Cur+1 .. Len));
      end if;

      if Check(Cur) then
        -- Optim : try next level if and only if current level is convenient
        Try(Cur+1);
        -- Done for Cur if Rest=Len
        exit when Rest = Len;
        -- Optim : sort only if try has been done
        -- Sort Cur+1 .. Len for next exchange
        Sort.Quick_Sort(Lis(Cur+1 .. Len));
      else
        -- Done for Cur if Rest=Len
        exit when Rest = Len;
      end if;

      -- Next Rest
      Rest := Rest + 1;

    end loop;

  end Try;

  procedure Dump is
    I : Len_Range;
    J : Natural;
  begin
    -- Dump lines
    for Lin in 1 .. Dim loop
      J := Lin * Dim - Dim;
      for Col in 1 .. Dim loop
        I := Lis(J + Col);
        Basic_Proc.Put_Output (Normal(I, 2) & " ");
      end loop;
      Basic_Proc.New_Line_Output;
    end loop;
    Basic_Proc.New_Line_Output;
  end Dump;

end Common;

