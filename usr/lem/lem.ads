with My_Math;
package Lem is

  subtype Real is My_Math.Real;
  use type My_Math.Real;

  -- Power thrust in N (1N = 1kg.m/s2)
  type Thrust_Range is new Integer;

  -- X thrust (neg thrust moves Lem left)
  Max_X_Thrust : constant := 4;
  subtype X_Thrust_Range is Thrust_Range range -Max_X_Thrust .. +Max_X_Thrust;
  -- Y Thrust, down to top
  Max_Y_Thrust : constant := 4;
  subtype Y_Thrust_Range is Thrust_Range range 0 .. Max_Y_Thrust;

  -- Set X thrust for a X_thrust slice
  procedure Set_X_Thrust (X_Thrust : in X_Thrust_Range);

  -- Set Y thrust until next setting
  procedure Set_Y_Thrust (Y_Thrust : in Y_Thrust_Range);

  -- Get current Y thrust
  function Get_Y_Thrust return Y_Thrust_Range;
  

  -- Speed in m/s
  -- Checks max speed
  Max_Speed : constant := 10_000.0;
  type Speed_Range is new Real range -Max_Speed .. +Max_Speed;
  type Speed_Rec is record
    X_Speed : Speed_Range;
    Y_Speed : Speed_Range;
  end record;
  Speed_Exceeded : exception;
  function Get_Speed return Speed_Rec;


  -- Position in space
  -- Does not check position
  type Position_Range is new Real;
  type Position_Rec is record
    X_Pos : Position_Range;
    Y_Pos : Position_Range;
  end record;
  function Get_Position return Position_Rec;


  -- Init Lem position
  -- Thrust is set to compensate weight to 25 kN
  -- Init speed is set to X = 0 and -20 <= Y <= 0
  procedure Init (Position : in Position_Rec);

  -- Stop Lem life
  procedure Stop;

end Lem;

