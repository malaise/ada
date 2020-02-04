-- Given the QFU of the runway and the current heading, compute an interception
-- procedure, among:
-- If Heading is the QFU, then direct final, straight on.
-- Otherwise, from a fixed interception distance:
-- * If within +/- a fixed max interception angle (40Deg), then reach the final
--   at a fixed direct final distance (13Nm), so with an angle less that the
--   max,
-- * Otherwise reach the entrance of a joining leg of fixed length
--   (join distance, 5Nm) that reaches the final with the max interception
--   angle at a fixed distance, indirect final distance (14Nm),
-- + In both cases, also propose an alternative indirect interception, the same
--   as above but on the other side of the runway.

-- Direct interception:
--          \
--           \
--            \
--             \
--              -------->=========
-- Indirect interception:
--        |
--        |
--        +
--         \
--          \
--           ----------->=========

with Argument, Basic_Proc, My_Math, Images, Normalization, Trace.Loggers;
procedure Intercept is

  -- Trace logger
  Logger : Trace.Loggers.Logger;

  type Angle is mod 360;
  subtype Signed_Angle is Integer range -180 .. 180;
  subtype Distance is Natural;

  -- Constants
  -- Max direct interception angle
  Max_Intercep_Angle : constant Angle := 40;
  -- Distance for interception
  Intercep_Distance : constant Distance := 30;
  -- Final lengths
  Direct_Final_Distance : constant Distance := 13;
  Indirect_Final_Distance : constant Distance := 14;
  -- Joining leg length
  Join_Distance : constant Distance := 5;

  -- Inputs
  Short_Mode : Boolean;
  Next_Arg : Positive;
  Qfu, Init_Head : Angle;

  -- Approach angle
  App_Angle : Angle;
  App_Delta : Signed_Angle;
  -- Interception leg angle and length
  Leg_Angle, Alt_Leg_Angle : Angle;
  Leg_Distance : Distance;
  -- Turn right
  Turn_Right, Alt_Turn1_Right, Alt_Turn2_Right : Boolean;
  -- Headings
  Heading_1, Alt_Heading_1, Heading_2, Alt_Heading_2 : Angle;

  -- Indirect intermediate point Beta
  Beta_Distance, Beta_Angle, Alt_Beta_Qdr : My_Math.Real;
  Beta_Dme : Distance;
  Beta_Qdm, Alt_Beta_Qdm : Angle;

  -- Temporary intermediate variable
  Tmp : My_Math.Real;

  -- Output images
  function Angle_Image is new Images.Mod_Image (Modul => Angle);
  function Angle_Image (I : Integer) return String renames Images.Integer_Image;
  function Dist_Image (I : Integer) return String renames Images.Integer_Image;
  function Right_Left_Image (Right : in Boolean) return String is
    (if Right then "right" else "left");

  -- Angle and turn normalised images
  function Angle_Norm is new Normalization.Normal_Mod (Modulus => Angle);
  function Angle_Norm (A : Angle) return String is
    (Angle_Norm (A, 3, True, ' '));
  function Right_Left_Norm (Right : in Boolean) return String is
    (if Right then "right" else " left");

  -- Syntax error
  procedure Error is
  begin
    Basic_Proc.Put_Line_Error ("Syntax error. Usage:"
        & Argument.Get_Program_Name & " <QfU> <Heading>");
    Basic_Proc.Put_Line_Error ("Angles from 0 to 359");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  function To_Signed (A : Angle) return Signed_Angle is
  begin
    if A <= 180 then
      return Signed_Angle (A);
    else
      return Signed_Angle (-(360 - Integer (A)));
    end if;
  end To_Signed;

  use type My_Math.Real;
begin
  Logger.Init ("Intercept");

  -- Parse arguments
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-d"
            or else Argument.Get_Parameter = "--dme") then
    Basic_Proc.Put_Line_Output (Dist_Image (Intercep_Distance));
    return;
  end if;

  begin
    Next_Arg := 1;
    Short_Mode := False;
    if Argument.Get_Parameter = "-s"
    or else Argument.Get_Parameter = "--short" then
      Short_Mode := True;
      Next_Arg := Next_Arg + 1;
    end if;
    if Argument.Get_Nbre_Arg /= Next_Arg + 1 then
      raise Constraint_Error;
    end if;
    Qfu       := Angle'Value (Argument.Get_Parameter (Occurence => Next_Arg));
    Next_Arg := Next_Arg + 1;
    Init_Head := Angle'Value (Argument.Get_Parameter (Occurence => Next_Arg));
  exception
    when Constraint_Error =>
      Error;
      return;
  end;
  Logger.Log_Debug ("QFU: " & Angle_Image (Qfu)
                  & "  Init heading: " & Angle_Image (Init_Head));

  -- Inital approach angle
  App_Angle := Init_Head - Qfu;
  App_Delta := To_Signed (App_Angle);
  Logger.Log_Debug ("Approach angle: " & Angle_Image (App_Angle));
  Logger.Log_Debug ("Approach delta: " & Angle_Image (App_Delta));
  if Short_Mode then
    Basic_Proc.Put_Output (Angle_Norm (Qfu) & " "
        & Angle_Norm (Init_Head) & " ");
  end if;

  -- Interception angle
  Tmp :=  My_Math.Sqrt (
       My_Math.Real (Direct_Final_Distance) ** 2
     + My_Math.Real (Intercep_Distance) ** 2
     - 2.0 * My_Math.Real (Direct_Final_Distance)
           * My_Math.Real (Intercep_Distance)
           * My_Math.Cos (My_Math.Real (App_Angle), My_Math.Degree));
  Leg_Distance := Distance (My_Math.Round (Tmp));
  Logger.Log_Debug ("Direct leg length: " & Dist_Image (Leg_Distance));
  Tmp := My_Math.Sin (My_Math.Real (abs App_Delta), My_Math.Degree)
         / Tmp * My_Math.Real (Intercep_Distance);
  Tmp := My_Math.Arc_Sin (Tmp, My_Math.Degree);
  if abs App_Delta > 90 then
    Tmp := 180.0 - Tmp;
  end if;
  Leg_Angle := Angle (My_Math.Round (Tmp));
  Logger.Log_Debug ("Direct interception angle: " & Angle_Image (Leg_Angle));

  -- Select proper interception strategy
  if App_Angle = 0 then
    -- Already in final
    if Short_Mode then
      Basic_Proc.Put_Line_Output ("Final");
    else
      Basic_Proc.Put_Line_Output ("You are in final heading "
         & Angle_Image (Qfu) & ".");
    end if;
    return;
  end if;

  -- For indirect interceptions
  -- Angle and DME of Beta, the start of the last (direct) leg
  -- Independant from initial heading
  Beta_Distance := My_Math.Sqrt (
        My_Math.Real (Indirect_Final_Distance) ** 2
      + My_Math.Real (Join_Distance) ** 2
      - 2.0 * My_Math.Real (Indirect_Final_Distance)
            * My_Math.Real (Join_Distance)
            * My_Math.Cos (My_Math.Real (180 - Max_Intercep_Angle),
                           My_Math.Degree));
  Beta_Dme := Distance (My_Math.Round (Beta_Distance));
  Logger.Log_Debug ("Beta DME: " & Dist_Image (Beta_Dme));
  Beta_Angle := abs My_Math.Arc_Sin (
      My_Math.Sin (My_Math.Real (180 - Max_Intercep_Angle))
        * My_Math.Real (Join_Distance) / Beta_Distance,
      My_Math.Degree);
  Logger.Log_Debug ("Beta Angle: "
      & Angle_Image (Angle (My_Math.Round (Beta_Angle))));

  -- Direct or indirect primary interception
  if Leg_Angle <= Max_Intercep_Angle then
    -- Direct interception
    if Short_Mode then
      Basic_Proc.Put_Output ("Direct ");
    else
      Basic_Proc.Put_Line_Output ("Direct interception");
    end if;
    Turn_Right := App_Delta > 0;
    if Turn_Right then
      Heading_1 := Qfu + Leg_Angle;
    else
      Heading_1 := Qfu - Leg_Angle;
    end if;
    if Short_Mode then
      Basic_Proc.Put_Output (Right_Left_Norm (Turn_Right) & " "
          & Angle_Norm (Heading_1) & " ");
    else
      Basic_Proc.Put_Line_Output (
            "At DME " & Dist_Image (Intercep_Distance) & " turn "
          & Right_Left_Image (Turn_Right) & " heading "
          & Angle_Image (Heading_1) & " to intercept final heading "
          & Angle_Image (Qfu) & ".");
    end if;
  else
    if Short_Mode then
      Basic_Proc.Put_Output ("Indirect ");
    else
      Basic_Proc.Put_Line_Output ("Indirect interception");
    end if;
    -- Set Beta QDM v.s. QFU, way of first turn and heading
    Beta_Qdm := Angle (My_Math.Round (Beta_Angle));
    if App_Delta >= 0 then
      -- First turn is right and second is left
      Turn_Right := True;
      Beta_Qdm := Qfu + Beta_Qdm;
      Heading_2 := Qfu + Max_Intercep_Angle;
    else
      -- First turn is left and second is right
      Turn_Right := False;
      Beta_Qdm := Qfu - Beta_Qdm;
      Heading_2 := Qfu - Max_Intercep_Angle;
    end if;
    Logger.Log_Debug ("Beta QDM: " & Angle_Image (Beta_Qdm));

    -- First (indirect leg), distance and turn to Beta
    Tmp := My_Math.Sqrt (
        My_Math.Real (Intercep_Distance) ** 2
      + Beta_Distance ** 2
      - 2.0 * My_Math.Real (Intercep_Distance) * Beta_Distance
            * My_Math.Cos (My_Math.Real (abs App_Delta) - Beta_Angle,
                           My_Math.Degree));
    Logger.Log_Debug ("Indirect leg length: "
        & Dist_Image (Distance (My_Math.Round (Tmp))));
    Tmp := abs My_Math.Arc_Sin (
        My_Math.Sin (My_Math.Real (abs App_Delta) - Beta_Angle, My_Math.Degree)
      / Tmp * Beta_Distance,
      My_Math.Degree);
    Leg_Angle := Angle (My_Math.Round (Tmp));
    if Turn_Right then
      -- First turn is right
      Heading_1 := Init_Head + Leg_Angle;
    else
      -- First turn is left
      Heading_1 := Init_Head - Leg_Angle;
    end if;
    if Short_Mode then
      Basic_Proc.Put_Output (Right_Left_Norm (Turn_Right) & " "
          & Angle_Norm (Heading_1) & " "
          & Angle_Norm (Beta_Qdm) & " "
          & Right_Left_Norm (not Turn_Right) & " "
          & Angle_Norm (Heading_2) & " ");
    else
      Basic_Proc.Put_Line_Output (
            "At DME " & Dist_Image (Intercep_Distance) & " turn "
          & Right_Left_Image (Turn_Right) & " heading "
          & Angle_Image (Heading_1) & ",");
      Basic_Proc.Put_Line_Output (
            "at DME " & Dist_Image (Beta_Dme) & " QDM " & Angle_Image (Beta_Qdm)
          & " turn " & Right_Left_Image (not Turn_Right) & " heading "
          & Angle_Image (Heading_2) & " to intercept final heading "
          & Angle_Image (Qfu) & ".");
    end if;
  end if;

  -- Alternate interception
  if not Short_Mode then
    Basic_Proc.Put_Line_Output ("Alternate indirect interception");
  end if;
  -- Set Qdm of alternate Beta
  Alt_Beta_Qdm := Angle (My_Math.Round (Beta_Angle));
  if App_Delta >= 0 then
    Alt_Beta_Qdm := Qfu - Alt_Beta_Qdm;
    Alt_Beta_Qdr := My_Math.Real (Qfu + 180) - Beta_Angle;
    Alt_Heading_2 := Qfu - Max_Intercep_Angle;
  else
    Alt_Beta_Qdm := Qfu + Alt_Beta_Qdm;
    Alt_Beta_Qdr := My_Math.Real (Qfu + 180) + Beta_Angle;
    Alt_Heading_2 := Qfu + Max_Intercep_Angle;
  end if;
  if Alt_Beta_Qdr > 360.0 then
    Alt_Beta_Qdr := Alt_Beta_Qdr - 360.0;
  end if;
  if Alt_Beta_Qdr < 0.0 then
    Alt_Beta_Qdr := Alt_Beta_Qdr + 360.0;
  end if;
  Logger.Log_Debug ("Alternate Beta QDM: " & Angle_Image (Alt_Beta_Qdm));

  -- First (indirect leg), distance and turn to Beta
  Tmp := My_Math.Sqrt (
        My_Math.Real (Intercep_Distance) ** 2
      + Beta_Distance ** 2
      - 2.0 * My_Math.Real (Intercep_Distance) * Beta_Distance
            * My_Math.Cos (My_Math.Real (abs App_Delta) + Beta_Angle,
                           My_Math.Degree));
  Logger.Log_Debug ("Indirect alternate leg length: "
        & Dist_Image (Distance (My_Math.Round (Tmp))));
  Tmp := abs My_Math.Arc_Sin (
        My_Math.Sin (My_Math.Real (abs App_Delta) + Beta_Angle, My_Math.Degree)
      / Tmp * Beta_Distance,
      My_Math.Degree);
  Alt_Leg_Angle := Angle (My_Math.Round (Tmp));
  Logger.Log_Debug ("Indirect alternate leg angle: "
      & Angle_Image (Alt_Leg_Angle));
  -- Set way of first turn and heading: Init_Head v.s. Alt_Beta_Qdm
  Tmp :=  My_Math.Real (Init_Head) - Alt_Beta_Qdr - 180.0;
  while Tmp > 360.0 loop
    Tmp := Tmp - 360.0;
  end loop;
  while Tmp < -0.0 loop
    Tmp := Tmp + 360.0;
  end loop;
  if Tmp > 180.0 then
    Tmp := Tmp - 360.0;
  end if;
  Logger.Log_Debug ("Heading - Alt_Beta_Qdm: "
      & Angle_Image (Signed_Angle (My_Math.Round (Tmp))));

  -- First turn
  if Tmp >= 0.0 then
    Alt_Turn1_Right := True;
    Alt_Heading_1 := Init_Head + Alt_Leg_Angle;
  else
    Alt_Turn1_Right := False;
    Alt_Heading_1 := Init_Head - Alt_Leg_Angle;
  end if;
  -- Second turn
  Alt_Turn2_Right := Alt_Heading_1 - Alt_Heading_2 >= 180;
  if Short_Mode then
    Basic_Proc.Put_Line_Output (Right_Left_Norm (Alt_Turn1_Right) & " "
        & Angle_Norm (Alt_Heading_1) & " "
        & Angle_Norm (Alt_Beta_Qdm) & " "
        & Right_Left_Norm (Alt_Turn2_Right) & " "
        & Angle_Norm (Alt_Heading_2));
  else
    Basic_Proc.Put_Line_Output (
          "At DME " & Dist_Image (Intercep_Distance) & " turn "
        & Right_Left_Image (Alt_Turn1_Right) & " heading "
        & Angle_Image (Alt_Heading_1) & ",");
    Basic_Proc.Put_Line_Output (
          "at DME " & Dist_Image (Beta_Dme) & " QDM " & Angle_Image (Alt_Beta_Qdm)
        & " turn " & Right_Left_Image (Alt_Turn2_Right) & " heading "
        & Angle_Image (Alt_Heading_2) & " to intercept final heading "
        & Angle_Image (Qfu) & ".");
  end if;

end Intercept;

