-- Pseudo random number generator
with Ada.Finalization;
with Mutex_Manager, U_Rand;
package Rnd is

  -- A random generator
  type Generator is tagged limited private;

  -- A common global generator
  Gen : constant Generator;

  -- Initialisation of the sequence,
  --   on  Init if 0.0 <= Init < 1.0
  --   randomly otherwise
  procedure Randomize (Gen : in Generator; Init : in Float := 1.0);

  generic
    type Num is (<>);
  -- Next element in sequence: Mini <= R <= Maxi
  function Discr_Random (Gen : Generator;
                         Mini : Num := Num'First;
                         Maxi : Num := Num'Last) return Num;

  -- Next element in sequence: Mini <= R <= Maxi
  function Int_Random (Gen : Generator;
                       Mini : Integer := 0;
                       Maxi : Integer := 1) return Integer;

  -- Next element in sequence: Mini <= R < Maxi
  function Float_Random (Gen : Generator;
                         Mini : Float := 0.0;
                         Maxi : Float := 1.0) return Float;

  -- Next element in sequence: Mini <= R < Maxi
  function Dur_Random (Gen : Generator;
                       Mini : Duration := 0.0;
                       Maxi : Duration := 1.0) return Duration;
private
  type Gen_Rec is record
    Lock : Mutex_Manager.Simple_Mutex;
    Ugen : U_Rand.Generator;
  end record;
  type Gen_Access is access Gen_Rec;

  type Generator is limited new Ada.Finalization.Limited_Controlled
                                  with record
    Acc : Gen_Access := new Gen_Rec;
  end record;
  overriding procedure Finalize (Gen : in out Generator);

  Gen : constant Generator := (Ada.Finalization.Limited_Controlled
                               with others => <>);

end Rnd;

