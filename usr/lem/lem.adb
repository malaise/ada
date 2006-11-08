with Timers, Rnd;
with Moon;
package body Lem is

  -- Mass of the LEM in kg
  type Mass_Range is new Real range 0.0 .. Real'Last;

  -- Empty mass of the LEM
  Empty_Mass : constant Mass_Range := 10_000.0;

  -- Fuel density in kg/m3
  Fuel_Density : constant := 700.0;

  -- Fuel quantity in m3
  Max_Fuel : constant := 8_000.0;
  type Fuel_Range is new Real range 0.0 .. Max_Fuel;
  Current_Fuel : Fuel_Range;

  -- Current mass: Empty + fuel mass
  function Current_Mass return Mass_Range is
  begin
    return Empty_Mass + Mass_Range(Current_Fuel / Fuel_Density);
  end Current_Mass;


  -- Power thrust in N (1N = 1kg.m/s2)
  -- Set X thrust for a period
  Current_X_Thrust : X_Thrust_Range;
  procedure Set_X_Thrust (X_Thrust : in X_Thrust_Range) is
  begin
    Current_X_Thrust := X_Thrust;
  end Set_X_Thrust;

  -- Set Y thrust until next setting
  Current_Y_Thrust : Y_Thrust_Range;
  procedure Set_Y_Thrust (Y_Thrust : in Y_Thrust_Range) is
  begin
    Current_Y_Thrust := Y_Thrust;
  end Set_Y_Thrust;

  -- Get current Y thrust
  function Get_Y_Thrust return Y_Thrust_Range is
  begin
    return Current_Y_Thrust;
  end Get_Y_Thrust;

  
  -- Acceleration in m/s2
  type Acceleration_Range is new Real;
  type Acceleration_Rec is record
    X_Acc : Acceleration_Range;
    Y_Acc : Acceleration_Range;
  end record;
  Current_Acceleration : Acceleration_Rec;

  -- Speed_Exceeded : exception;
  Current_Speed : Speed_Rec;
  function Get_Speed return Speed_Rec is
  begin
    return Current_Speed;
  end Get_Speed;

  -- Position in space
  Current_Position : Position_Rec;
  function Get_Position return Position_Rec is
  begin
    return Current_Position;
  end Get_Position;


  -- Operations
  -- Acceleration * Mass -> Thrust
  function "*" (Acceleration : in Acceleration_Range;
                Mass : in Mass_Range) return Thrust_Range is
  begin
    return Y_Thrust_Range (My_Math.Trunc(My_Math.Real(Acceleration)
                                       * My_Math.Real(Mass)));
  end "*";

  -- Thrust * Duration -> Fuel quantity
  subtype Pos_Thrust is Thrust_Range range 0 .. Thrust_Range'Last;
  subtype Pos_Duration is Duration range 0.0 .. Duration'Last;
  function Conso_Of (Thrust : in Pos_Thrust;
                     Dur : Pos_Duration) return Fuel_Range is
    -- Fuel consumption in m3/N/s
    Fuel_Consumption : constant := 2.61e-6;
  begin
    return Fuel_Range (My_Math.Real(Thrust) * Fuel_Consumption * My_Math.Real(Dur));
  end Conso_Of;

  -- Thrust / Mass -> Acceleration
  function "/" (Thrust : Thrust_Range; Mass : Mass_Range)
               return Acceleration_Range is
  begin
    return Acceleration_Range (My_Math.Real (Thrust) / My_Math.Real (Mass));
  end "/";

  -- Acceleration * Duration -> Speed
  function "*" (Acceleration : Acceleration_Range; Dur : in Duration)
               return Speed_Range is
  begin
   return Speed_Range (My_Math.Real(Acceleration) * My_Math.Real(Dur));
  end "*";

  -- Speedi * Duration -> Position
  function "*" (Speed : Speed_Range; Dur : in Duration)
               return Position_Range is
  begin
   return Position_Range (My_Math.Real(Speed) * My_Math.Real(Dur));
  end "*";


  -- Periodic computations
  -- Period of activation of the computation
  Period : constant Duration := 1.0;

  -- Timer callback computing new LEM characteristics
  function Timer_Cb (Id : Timers.Timer_Id; Data : in Timers.Timer_Data)
                    return Boolean is
    Fuel_Consumed : Fuel_Range;
    Mass : Mass_Range;
    Prev_Acceleration : Acceleration_Rec;
    Prev_Speed : Speed_Rec;
  begin
    -- Compute LEM characteristics
    -- New fuel
    Fuel_Consumed := Conso_Of (abs Current_X_Thrust + abs Current_Y_Thrust, Period);
    if Fuel_Consumed <= Current_Fuel then
      Current_Fuel := Current_Fuel - Fuel_Consumed;
    else
      Current_Fuel := 0.0;
    end if;
    -- New mass
    Mass := Current_Mass;
    -- New acceleration
    Prev_Acceleration := Current_Acceleration;
    Current_Acceleration := (X_Acc => Current_X_Thrust / Mass,
                             Y_Acc => Current_Y_Thrust / Mass + Moon.Acceleration);
    -- New speed
    Prev_Speed := Current_Speed;
    Current_Speed := (
      X_Speed => Current_Speed.X_Speed + Prev_Acceleration.X_Acc * Period,
      Y_Speed => Current_Speed.Y_Speed + Prev_Acceleration.Y_Acc * Period);
    -- New position
    Current_Position := (
      X_Pos => Current_Position.X_Pos
             + 0.5 * Prev_Acceleration.X_Acc * Period * Period
             + Prev_Speed.X_Speed * Period,
      Y_Pos => Current_Position.Y_Pos 
             + 0.5 * Prev_Acceleration.Y_Acc * Period * Period
             + Current_Speed.Y_Speed * Period);
    -- Reset X thrust
    Current_X_Thrust := 0;
    return True;
  end Timer_Cb;
  

  -- Periodical timer
  Tid : Timers.Timer_Id := Timers.No_Timer;

  -- Init Lem position
  -- Thrust is set to compensate weight to 25 kN
  -- Init speed is set to X = 0 and -20 <= Y <= 0
  procedure Init (Position : in Position_Rec) is
  begin
    Rnd.Randomize;
    -- Set initial data
    -- Full fuel
    Current_Fuel := Max_Fuel;
    -- Xthrust = 0. Ythrust compensates from (full) weight
    Current_X_Thrust := 0;
    Current_Y_Thrust := -Moon.Acceleration * Current_Mass;
    -- Acceleration, speed, position
    Current_Acceleration := (0.0, 0.0);
    Current_Speed := (0.0, - Speed_Range(Rnd.Float_Random(0.0, 10.0)));
    Current_Position := Position;
    -- Start periodical timer
    Tid := Timers.Create ( (Delay_Kind => Timers.Delay_Sec,
                           Delay_Seconds => Period,
                           Period => Period),
                          Timer_Cb'Access);
  end Init;

  -- Stop Lem life: stop timer
  procedure Stop is
    use type Timers.Timer_Id;
  begin
    if Tid /= Timers.No_Timer then
      Timers.Delete (Tid);
    end if;
  end Stop;

end Lem;


