-- General definition of the types for aeronautics navigation

package Nav_Types is

  -- Degrees and minutes for vectors and drift
  type T_Common_Deg is new Integer;
  type T_Minute is new Natural range 0 .. 59;

  -- Degrees for vectors : 1 circle
  subtype T_Degree is T_Common_Deg range 0 .. 359;

  -- Angle of a vector, speed of a vector and a vector
  type T_Angle is record
   Degrees : T_Degree := 0;
   Minutes : T_Minute := 0;
  end record;
  function Reduction (Deg : T_Common_Deg; Min : T_Minute) return T_Angle;
  function "+" (A1, A2 : T_Angle) return T_Angle;
  function "-" (A1, A2 : T_Angle) return T_Angle;
  function ">" (A1, A2 : T_Angle) return Boolean;
  function "<" (A1, A2 : T_Angle) return Boolean;
  function "+" (A1 : T_Angle; A2 : T_Degree) return T_Angle;
  -- Speed of a vector
  subtype T_Speed is Float range 0.0 .. 999.9;
  -- Vector
  type T_Vector is record
   Speed : T_Speed := 0.0;
   Angle : T_Angle;
  end record;

  -- Special definition of the angle of drift
  subtype T_Deg_Drift is T_Common_Deg range 0 .. 179;
  type T_Drift is record
   Positiv : Boolean := True;
   Degrees : T_Deg_Drift := 0;
   Minutes : T_Minute := 0;
  end record;
  function "+" (A : T_Angle; D : T_Drift) return T_Angle;
  function "-" (A : T_Angle; D : T_Drift) return T_Angle;
  function "-" (A1, A2 : T_Angle) return T_Drift;


end Nav_Types;

