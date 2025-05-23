-- Pseudo random number generator
private with Mutexes;
private with K_Rand, S_Rand, U_Rand;
package Rnd is

  -- Kind of generator
  type Kind_List is (Simple, Kiss, Universal);

  -- A random generator, protected by a mutex
  type Generator (Kind : Kind_List) is tagged limited private;

  -- Kinds of generators
  subtype Simple_Generator    is Generator (Simple);
  subtype Kiss_Generator      is Generator (Kiss);
  subtype Universal_Generator is Generator (Universal);

  -- A common global Simple generator
  Gen : constant not null access Simple_Generator;

  -- Initialisation of the sequence,
  --   on Init if 0.0 <= Init < 1.0 (always at each call)
  --   otherwise randomly (the default, only if not yet randomised by default)
  Randomly : constant Float := 1.0;
  procedure Randomize (Agen : in out Generator;
                       Init : in Float := Randomly;
                       Only_First_Time : in Boolean := True);

  -- A Generator is initially not randomized
  function Is_Randomized (Agen : in out Generator) return Boolean;

  -- Run the generator a random amount of times, from 0 to Max
  procedure Run (Agen : in out Generator; Max : Positive);

  -- Don't exceed Long_Long'First .. Long_Long'Last
  generic
    type Num is (<>);
  -- Next element in sequence: Mini <= R <= Maxi
  function Discr_Random (Agen : in out Generator;
                         Mini : in Num := Num'First;
                         Maxi : in Num := Num'Last) return Num;

  -- Next element in sequence: Mini <= R <= Maxi
  function Int_Random (Agen : in out Generator;
                       Mini : in Integer := 0;
                       Maxi : in Integer := 1) return Integer;
  generic
    type Modulus is mod <>;
  -- Next element in sequence: Mini <= R <= Maxi
  function Mod_Random (Agen : in out Generator;
                       Mini : in Modulus := Modulus'First;
                       Maxi : in Modulus := Modulus'Last) return Modulus;

  -- Next element in sequence: Mini <= R < Maxi
  function Float_Random (Agen : in out Generator;
                         Mini : in Float := 0.0;
                         Maxi : in Float := 1.0) return Float;

  -- Next element in sequence: Mini <= R < Maxi
  function Dur_Random (Agen : in out Generator;
                       Mini : in Duration := 0.0;
                       Maxi : in Duration := 1.0) return Duration;
private

  type Generator (Kind : Kind_List) is tagged limited record
    Randomized : Boolean := False;
    Lock : Mutexes.Simple_Mutex;
    case Kind is
       when Simple =>
         Sgen : S_Rand.Generator;
       when Universal =>
         Ugen : U_Rand.Generator;
       when Kiss =>
         Kgen : K_Rand.Generator;
       end case;
  end record;

  -- A global generator
  Init : aliased Simple_Generator := (Kind => Simple, others => <>);
  Gen : constant not null access Simple_Generator := Init'Access;

end Rnd;

