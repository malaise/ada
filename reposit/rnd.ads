
package RND is
  pragma ELABORATE_BODY(RND);

  -- Initialisation of sequence,
  --   on  INIT if 0.0 <= INIT < 1.0
  --   randomly otherwise
  procedure RANDOMIZE (INIT : in FLOAT := 1.0);

  generic
    type NUM is (<>);
  -- Next element in sequence: MINI <= R <= MAXI
  function DISCR_RANDOM (MINI : NUM := NUM'FIRST;
                         MAXI : NUM := NUM'LAST) return NUM;

  -- Next element in sequence: MINI <= R <= MAXI
  function INT_RANDOM (MINI : INTEGER := 0;
                       MAXI : INTEGER := 1) return INTEGER;

  -- Next element in sequence: MINI <= R < MAXI
  function FLOAT_RANDOM (MINI : FLOAT := 0.0;
                         MAXI : FLOAT := 1.0) return FLOAT;

  -- Next element in sequence: MINI <= R < MAXI
  function DUR_RANDOM (MINI : DURATION := 0.0;
                       MAXI : DURATION := 1.0) return DURATION;


end RND;

