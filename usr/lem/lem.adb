with Ada.Text_Io, Ada.Calendar;
with Timers, Rnd, Chronos;
with Moon, Debug;
package body Lem is

  Running : Boolean := False;
  Landed : Boolean := False;

  ----------
  -- MASS --
  ----------
  -- Empty mass of the LEM in kg
  Empty_Mass : constant Mass_Range := 10_000.0;

  -- Fuel quantity in kg
  Current_Fuel : Fuel_Range := 0.0;

  -- Get current level (mass) of fuel
  function Get_Fuel return Fuel_Range is
  begin
    return Current_Fuel;
  end Get_Fuel;


  ----------------
  -- OPERATIONS --
  ----------------
  type Acceleration_Range is new Real;
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
    -- Fuel consumption in kg/N/s
    Fuel_Consumption : constant := 1.83E-003;
  begin
    return Fuel_Range (My_Math.Real(Thrust) * Fuel_Consumption
                                            * My_Math.Real(Dur));
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

  -- Speed * Duration -> Position
  function "*" (Speed : Speed_Range; Dur : in Duration)
               return Position_Range is
  begin
   return Position_Range (My_Math.Real(Speed) * My_Math.Real(Dur));
  end "*";


  ------------
  -- THRUST --
  ------------
  -- Power thrust in N (1N = 1kg.m/s2)
  -- Set X thrust for a second
  -- Return true if too much X thrust while landed
  Current_X_Thrust : X_Thrust_Range := 0;
  Thrust_Tid : Timers.Timer_Id;
  function Timer_Thrust_Cb (Id : Timers.Timer_Id;
                            Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    -- Reset X thrust
    Current_X_Thrust := 0;
    return False;
  end Timer_Thrust_Cb;
  procedure Set_X_Thrust (X_Thrust : in X_Thrust_Range) is
  begin
    if not Running then
      raise Invalid_Mode;
    end if;
    -- Delete previous timer
    Thrust_Tid.Delete_If_Exists;
    -- No fuel => no thrust, if landed, no effect
    if Current_Fuel = 0.0 or else Landed then
      return;
    end if;
    -- Set new thrust and arm timer
    Current_X_Thrust := X_Thrust;
    Thrust_Tid := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
                                   Clock         => Space.Clock'Access,
                                   Period        => Timers.No_Period,
                                   Delay_Seconds => 1.0),
                                   Timer_Thrust_Cb'Access);
  end Set_X_Thrust;

  -- Set Y thrust until next setting
  Current_Y_Thrust : Y_Thrust_Range := 0;
  procedure Set_Y_Thrust (Y_Thrust : in Y_Thrust_Range) is
  begin
    if not Running then
      raise Invalid_Mode;
    end if;
    if Current_Fuel /= 0.0 then
      Current_Y_Thrust := Y_Thrust;
    else
      Current_Y_Thrust := 0;
    end if;
  end Set_Y_Thrust;

  -- Get current Y thrust
  function Get_Y_Thrust return Y_Thrust_Range is
  begin
    return Current_Y_Thrust;
  end Get_Y_Thrust;


  ---------------
  -- CINEMATIC --
  ---------------
  -- Time when Current_Acceleration, Current_Speed and Current_Position
  --  have been set (by init or periodic timer callback)
  Chrono : Chronos.Chrono_Type;

  -- Get duration since last computation
  function Read_Chrono return Duration is
    Time : constant Chronos.Time_Rec := Chrono.Read;
  begin
    -- Assumption that result is less that one day,
    --  or at least will not overflow duration
    return Duration(Time.Days) * Ada.Calendar.Day_Duration'Last
         + Time.Secs;
  end Read_Chrono;

  -- Acceleration in m/s2
  type Acceleration_Rec is record
    X_Acc : Acceleration_Range;
    Y_Acc : Acceleration_Range;
  end record;
  Current_Acceleration : Acceleration_Rec := (0.0, 0.0);

  -- Speed
  Current_Speed : Speed_Rec := (0.0, 0.0);

  -- Interpolate from Current_Speed for a given Duration
  function Speed_At (Delta_Time : Duration) return Speed_Rec is
  begin
    return (
      X_Speed => Current_Speed.X_Speed + Current_Acceleration.X_Acc * Delta_Time,
      Y_Speed => Current_Speed.Y_Speed + Current_Acceleration.Y_Acc * Delta_Time);
  exception
    when Constraint_Error =>
      raise Speed_Exceeded;
  end Speed_At;

  -- Current position: Speed_At (Now)
  function Get_Speed return Speed_Rec is
    Delta_Time : constant Duration := Read_Chrono;
  begin
    return Speed_At (Delta_Time);
  end Get_Speed;

  -- Get a valid (reasonnably negative) initial vertical speed
  -- At present, init speed is set to -1 <= X = 1
  --  and -10 <= Y <= 0
  function Get_Init_Speed return Speed_Rec is
  begin
    return (X_Speed => Speed_Range(Rnd.Float_Random(-1.0, 1.0)),
            Y_Speed => - Speed_Range(Rnd.Float_Random(0.0, 10.0)));
  end Get_Init_Speed;

  -- Position in space
  Current_Position : Position_Rec := (0.0, 0.0);

  -- Interpolate from Current_Position for a given Duration
  function Position_At (Delta_Time : Duration) return Position_Rec is
    use type Space.Position_Range;
  begin
    return (
      X_Pos => Current_Position.X_Pos
             + 0.5 * Current_Acceleration.X_Acc * Delta_Time * Delta_Time
             + Current_Speed.X_Speed * Delta_Time,
      Y_Pos => Current_Position.Y_Pos
             + 0.5 * Current_Acceleration.Y_Acc * Delta_Time * Delta_Time
             + Current_Speed.Y_Speed * Delta_Time);
  end Position_At;

  -- Current position: Position_At (Now)
  function Get_Position return Position_Rec is
    Delta_Time : constant Duration := Read_Chrono;
  begin
    if Debug.Set_Lem then
      Ada.Text_Io.Put_Line ("LEM Delta time is " & Delta_Time'Img);
    end if;
    return Position_At (Delta_Time);
  end Get_Position;


  --------------
  -- PERIODIC --
  --------------
  -- Periodic computation of mass and cinematic
  -- Period of activation of the computation
  Period : constant Duration := 1.0;

  -- Timer callback computing new LEM characteristics
  function Period_Timer_Cb (Id : Timers.Timer_Id;
                            Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
    Fuel_Consumed : Fuel_Range;
    Mass : Mass_Range;
    New_Position : Position_Rec;
    New_Speed : Speed_Rec;
  begin
    if Debug.Set_Lem then
      Ada.Text_Io.Put_Line ("LEM Periodic");
    end if;
    -- Compute LEM characteristics
    -- Time of computation for further linear interpolation
    Chrono.Reset;
    -- New position
    -- GNAT Bug? Current_Position is set to 0 before calling Position_At!
    New_Position := Position_At (Period);
    Current_Position := New_Position;
    -- New speed
    -- GNAT Bug? Current_Speed is set to 0 before calling Speed_At!
    New_Speed := Speed_At (Period);
    Current_Speed := New_Speed;
    if Debug.Set_Lem then
      Ada.Text_Io.Put_Line ("LEM Pos is " & Current_Position.X_Pos'Img
                                    & "/" & Current_Position.Y_Pos'Img);
      Ada.Text_Io.Put_Line ("LEM Speed is "
                        & Current_Speed.X_Speed'Img
                  & "/" & Current_Speed.Y_Speed'Img);
    end if;

    -- And for next time
    -- Fuel consumed during the Period
    Fuel_Consumed := Conso_Of (abs Current_X_Thrust + Current_Y_Thrust,
                               Period);
    -- New fuel remaining
    if Fuel_Consumed <= Current_Fuel then
      Current_Fuel := Current_Fuel - Fuel_Consumed;
    else
      Current_Fuel := 0.0;
      Current_Y_Thrust := 0;
    end if;
    -- New mass
    Mass := Empty_Mass + Current_Fuel;
    -- New acceleration
    Current_Acceleration := (X_Acc => Current_X_Thrust / Mass,
                             Y_Acc => Current_Y_Thrust / Mass + Moon.Acceleration);
    if Debug.Set_Lem then
      Ada.Text_Io.Put_Line ("LEM Acc is " & Current_Acceleration.X_Acc'Img
                                    & "/" & Current_Acceleration.Y_Acc'Img);
    end if;
    -- Check if lem is landed
    if Landed then
      Current_X_Thrust := 0;
      if Current_Acceleration.Y_Acc < 0.0 then
        -- Don't go down when landed
        Current_Acceleration.Y_Acc := 0.0;
        if Debug.Set_Lem then
          Ada.Text_Io.Put_Line ("LEM is landed. Accel 0.");
        end if;
      else
        -- Takin' off
        Landed := False;
        if Debug.Set_Lem then
          Ada.Text_Io.Put_Line ("LEM is taking off.");
        end if;
      end if;
    end if;
    return False;
  end Period_Timer_Cb;

  -- Periodical timer
  Period_Tid : Timers.Timer_Id;

  -- Init Lem position
  -- Thrust is set to compensate weight to 25 kN
  procedure Init (Position : in Position_Rec;
                  Speed    : in Speed_Rec) is
  begin
    if Running then
      raise Invalid_Mode;
    end if;
    Running := True;
    -- Set initial data
    Landed := False;
    -- Full fuel
    Current_Fuel := Max_Fuel;
    -- Xthrust = 0. Ythrust compensates from (full) weight, so initial
    --  (random) vertical speed remains constant
    Current_X_Thrust := 0;
    Current_Y_Thrust := (-Moon.Acceleration) * (Empty_Mass + Current_Fuel);
    -- Time: Re-init chrono
    Chrono.Stop;
    Chrono.Reset;
    Chrono.Attach (Space.Clock'Access);
    Chrono.Start;
    -- Acceleration, speed, position
    Current_Acceleration := (0.0, 0.0);
    Current_Speed := Speed;
    Current_Position := Position;
    -- Start periodical timer
    Period_Tid := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
                                   Clock         => Space.Clock'Access,
                                   Period        => Period,
                                   Delay_Seconds => Period),
                                  Period_Timer_Cb'Access);
  end Init;

  -- Stop Lem life: stop timer
  procedure Stop is
  begin
    -- Stop timers
    Period_Tid.Delete_If_Exists;
    Timers.Delete (Thrust_Tid);
    Thrust_Tid.Delete_If_Exists;
    -- Reset Trust, acceleration and speed
    Chrono.Stop;
    Current_X_Thrust := 0;
    Current_Y_Thrust := 0;
    Current_Acceleration := (0.0, 0.0);
    Current_Speed := (0.0, 0.0);
    Running := False;
  end Stop;

  -- Set position when landed
  procedure Set_Landed_Position (Position : in Space.Position_Rec) is
  begin
    if not Running then
      raise Invalid_Mode;
    end if;
    -- Landed at position
    if Debug.Set_Lem then
      Ada.Text_Io.Put_Line ("LEM has just landed.");
    end if;
    Landed := True;
    Current_Position := Position;
    Current_Speed := (0.0, 0.0);
    Current_Acceleration := (0.0, 0.0);
  end Set_Landed_Position;

  -- Landed as soon as Set_Landed_Position called
  --  and as long as not taking off
  function Is_Landed return Boolean is
  begin
    return Landed;
  end Is_Landed;

end Lem;

