
package Rnd is
  pragma Elaborate_Body(Rnd);

  -- Initialisation of sequence,
  --   on  Init if 0.0 <= Init < 1.0
  --   randomly otherwise
  procedure Randomize (Init : in Float := 1.0);

  generic
    type Num is (<>);
  -- Next element in sequence: Mini <= R <= Maxi
  function Discr_Random (Mini : Num := Num'First;
                         Maxi : Num := Num'Last) return Num;

  -- Next element in sequence: Mini <= R <= Maxi
  function Int_Random (Mini : Integer := 0;
                       Maxi : Integer := 1) return Integer;

  -- Next element in sequence: Mini <= R < Maxi
  function Float_Random (Mini : Float := 0.0;
                         Maxi : Float := 1.0) return Float;

  -- Next element in sequence: Mini <= R < Maxi
  function Dur_Random (Mini : Duration := 0.0;
                       Maxi : Duration := 1.0) return Duration;


end Rnd;

