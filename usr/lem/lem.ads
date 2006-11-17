with My_Math;
with Space;
package Lem is

  subtype Real is My_Math.Real;
  use type My_Math.Real;

  -- Width and Height of the LEM
  Width : constant Space.Position_Range := 10.0;
  Height : constant Space.Position_Range := 10.0;

  -- Mass of the LEM in kg
  type Mass_Range is new Real range 0.0 .. Real'Last;

  -- Fuel quantity in kg
  Max_Fuel : constant := 5_600.0;
  subtype Fuel_Range is Mass_Range range 0.0 .. Max_Fuel;

  -- Get current level (mass) of fuel
  function Get_Fuel return Fuel_Range;


  -- Power thrust in N (1N = 1kg.m/s2)
  type Thrust_Range is new Integer;

  -- X thrust (neg thrust moves Lem left)
  Max_X_Thrust : constant := 40_000;
  subtype X_Thrust_Range is Thrust_Range range -Max_X_Thrust .. +Max_X_Thrust;
  -- Y Thrust, down to top
  Max_Y_Thrust : constant := 40_000;
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
  subtype Position_Range is Space.Position_Range;
  subtype Position_Rec is Space.Position_Rec;
  -- Does not check position
  function Get_Position return Position_Rec;

  -- Set position when landed
  procedure Set_Landed_Position (Position : in Space.Position_Rec);

  -- Landed as soon as Set_Landed_Position called
  --  and as long as not taking off
  function Is_Landed return boolean;


  -- Init Lem position
  -- Thrust is set to compensate weight to 25 kN
  -- Init speed is set to X = 0 and -20 <= Y <= 0
  procedure Init (Position : in Position_Rec);

  -- Stop Lem life
  procedure Stop;

  -- Exception when setting thrust... while stopped, initialising or setting
  -- landing position while not stopped
  Invalid_Mode : exception;

end Lem;

