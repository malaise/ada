-- Pseudo random number generator
with Mutex_Manager, U_Rand;
package Rnd is

  -- A random generator
  type Generator is tagged limited private;

  -- A common global generator
  Gen : constant access Generator;

  -- Initialisation of the sequence,
  --   on  Init if 0.0 <= Init < 1.0
  --   randomly otherwise
  procedure Randomize (Gen : in out Generator; Init : in Float := 1.0);

  -- A Generator is initially not radomized
  function Is_Randomized (Gen : in out Generator) return Boolean;

  -- Don't exceed Long_Long'First .. Long_Long'Last
  generic
    type Num is (<>);
  -- Next element in sequence: Mini <= R <= Maxi
  function Discr_Random (Gen : in out Generator;
                         Mini : in Num := Num'First;
                         Maxi : in Num := Num'Last) return Num;

  -- Next element in sequence: Mini <= R <= Maxi
  function Int_Random (Gen : in out Generator;
                       Mini : in Integer := 0;
                       Maxi : in Integer := 1) return Integer;
  generic
    type Modulus is mod <>;
  -- Next element in sequence: Mini <= R <= Maxi
  function Mod_Random (Gen : in out Generator;
                       Mini : in Modulus := Modulus'First;
                       Maxi : in Modulus := Modulus'Last) return Modulus;

  -- Next element in sequence: Mini <= R < Maxi
  function Float_Random (Gen : in out Generator;
                         Mini : in Float := 0.0;
                         Maxi : in Float := 1.0) return Float;

  -- Next element in sequence: Mini <= R < Maxi
  function Dur_Random (Gen : in out Generator;
                       Mini : in Duration := 0.0;
                       Maxi : in Duration := 1.0) return Duration;
private

  type Generator is tagged limited record
    Randomized : Boolean := False;
    Lock : Mutex_Manager.Simple_Mutex;
    Ugen : U_Rand.Generator;
  end record;

  -- A global generator
  Init : aliased Generator := (others => <>);
  Gen : constant access Generator := Init'Access;

end Rnd;

