-- General definition of the types for aeronautics navigation

package NAV_TYPES is

  -- Degrees and minutes for vectors and drift
  type T_COMMON_DEG is new INTEGER;
  type T_MINUTE is new NATURAL range 0 .. 59;

  -- Degrees for vectors : 1 circle
  subtype T_DEGREE is T_COMMON_DEG range 0 .. 359;

  -- Angle of a vector, speed of a vector and a vector
  type T_ANGLE is record
   DEGREES : T_DEGREE := 0;
   MINUTES : T_MINUTE := 0;
  end record;
  function REDUCTION (DEG : T_COMMON_DEG; MIN : T_MINUTE) return T_ANGLE;
  function "+" (A1, A2 : T_ANGLE) return T_ANGLE;
  function "-" (A1, A2 : T_ANGLE) return T_ANGLE;
  function ">" (A1, A2 : T_ANGLE) return BOOLEAN;
  function "<" (A1, A2 : T_ANGLE) return BOOLEAN;
  function "+" (A1 : T_ANGLE; A2 : T_DEGREE) return T_ANGLE;
  -- Speed of a vector
  subtype T_SPEED is FLOAT range 0.0 .. 999.9;
  -- Vector
  type T_VECTOR is record
   SPEED : T_SPEED := 0.0;
   ANGLE : T_ANGLE;
  end record;

  -- Special definition of the angle of drift
  subtype T_DEG_DRIFT is T_COMMON_DEG range 0 .. 179;
  type T_DRIFT is record
   POSITIV : BOOLEAN := TRUE;
   DEGREES : T_DEG_DRIFT := 0;
   MINUTES : T_MINUTE := 0;
  end record;
  function "+" (A : T_ANGLE; D : T_DRIFT) return T_ANGLE;
  function "-" (A : T_ANGLE; D : T_DRIFT) return T_ANGLE;
  function "-" (A1, A2 : T_ANGLE) return T_DRIFT;


end NAV_TYPES;

