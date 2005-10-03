package Types is

  -- The letters known by the Coder
  subtype Letter is Character range 'A' .. 'Z';
  -- Letter index: A->0 .. Z->25
  type Lid is mod 26;

  -- Conversions
  function Id_Of (L : Letter) return Lid;
  function Letter_Of (I : Lid) return Letter;

  -- An association of letters or letter ids (E, D) means
  --  E is encoded to D and D decoded to E
  type Letter_Pair_T is record
    E : Types.Letter;
    D : Types.Letter;
  end record;
  type Lid_Pair_T is record
    E : Types.Lid;
    D : Types.Lid;
  end record;

end Types;

