with Basic_Proc, Images, Normalization;
with Nav_Types, Nav_Data;
procedure T_Nav is

  Data_In, Data_Out : Nav_Data.T_Data;
  subtype Ti is Positive range 1..7;
  I1, I2, I3 : Ti;
  Report : Nav_Data.T_Consistency;

  function Dot (S : String) return Natural is
  begin
    for I in S'Range loop
      if S(I)='.' then return I; end if;
    end loop;
    return 0;
  end Dot;

  function Get_Indice (Msg : String) return Ti is
    S : String (1..10);
    L : Natural;
  begin
    loop
      begin
        Basic_Proc.Put_Output (Msg & " unknown ? ");
        Basic_Proc.Get_Line (S, L);
        return Ti'Value (S(1..L));
      exception
        when others => Basic_Proc.Put_Line_Output ("ERREUR...");
      end;
    end loop;
  end Get_Indice;

  function Get_Angle (Msg : String) return Nav_Types.T_Angle is
    S : String (1..10);
    L : Natural;
    D : Natural;
    A : Nav_Types.T_Angle;
  begin
    loop
      begin
        Basic_Proc.Put_Output (Msg & " angle ? ");
        Basic_Proc.Get_Line (S, L);
        D := Dot (S(1..L));
        if D = 0 then
          A.Degrees := Nav_Types.T_Degree'Value (S(1..L));
          A.Minutes := 0;
        else
          A.Degrees := Nav_Types.T_Degree'Value (S(1..D-1));
          A.Minutes := Nav_Types.T_Minute'Value (S(D+1..L));
        end if;
        return A;
      exception
        when others => Basic_Proc.Put_Line_Output ("ERREUR...");
      end;
    end loop;
  end Get_Angle;

  function Get_Drift (Msg : String) return Nav_Types.T_Drift is
    S : String (1..10);
    L : Natural;
    D : Natural;
    A : Nav_Types.T_Drift;
  begin
    loop
      begin
        Basic_Proc.Put_Output (Msg & " angle ? ");
        Basic_Proc.Get_Line (S, L);
        if S(1)='-'  then
          S(1..L-1) := S(2..L);
          L := L - 1;
          A.Positiv := False;
        else
          A.Positiv := True;
        end if;
        D := Dot (S(1..L));
        if D = 0 then
          A.Degrees := Nav_Types.T_Deg_Drift'Value (S(1..L));
          A.Minutes := 0;
        else
          A.Degrees := Nav_Types.T_Deg_Drift'Value (S(1..D-1));
          A.Minutes := Nav_Types.T_Minute'Value (S(D+1..L));
        end if;
        return A;
      exception
        when others => Basic_Proc.Put_Line_Output ("ERREUR...");
      end;
    end loop;
  end Get_Drift;

  function Get_Speed (Msg : String) return Nav_Types.T_Speed is
    S : String (1..10);
    L : Natural;
  begin
    loop
      begin
        Basic_Proc.Put_Output (Msg & " speed ? ");
        Basic_Proc.Get_Line (S, L);
        return Nav_Types.T_Speed (Positive'Value (S(1..L)));
      exception
        when others => Basic_Proc.Put_Line_Output ("ERREUR...");
      end;
    end loop;
  end Get_Speed;

begin
  Basic_Proc.Put_Line_Output ("1 : WIND SPEED");
  Basic_Proc.Put_Line_Output ("2 : WIND ANGLE");
  Basic_Proc.Put_Line_Output ("3 : PLAN SPEED");
  Basic_Proc.Put_Line_Output ("4 : PLAN ANGLE");
  Basic_Proc.Put_Line_Output ("5 : TRAJ SPEED");
  Basic_Proc.Put_Line_Output ("6 : TRAJ ANGLE");
  Basic_Proc.Put_Line_Output ("7 : DRIFT");
  I1 := Get_Indice ("1st");
  loop
    I2 := Get_Indice ("2nd");
    exit when I2 /= I1;
  end loop;
  loop
    I3 := Get_Indice ("3rd");
    exit when I3 /= I1 and then I3 /= I2;
  end loop;
  Data_In.Set(Nav_Data.Wind_S) := I1 /= 1 and then I2 /= 1 and then I3 /= 1;
  Data_In.Set(Nav_Data.Wind_A) := I1 /= 2 and then I2 /= 2 and then I3 /= 2;
  Data_In.Set(Nav_Data.Plan_S) := I1 /= 3 and then I2 /= 3 and then I3 /= 3;
  Data_In.Set(Nav_Data.Plan_A) := I1 /= 4 and then I2 /= 4 and then I3 /= 4;
  Data_In.Set(Nav_Data.Traj_S) := I1 /= 5 and then I2 /= 5 and then I3 /= 5;
  Data_In.Set(Nav_Data.Traj_A) := I1 /= 6 and then I2 /= 6 and then I3 /= 6;
  Data_In.Set(Nav_Data.Drift)  := I1 /= 7 and then I2 /= 7 and then I3 /= 7;

  if Data_In.Set (Nav_Data.Wind_S) then
    Data_In.Wind.Speed := Get_Speed("wind");
  end if;
  if Data_In.Set (Nav_Data.Wind_A) then
    Data_In.Wind.Angle := Get_Angle ("wind");
  end if;

  if Data_In.Set (Nav_Data.Plan_S) then
    Data_In.Plan.Speed := Get_Speed("plan");
  end if;
  if Data_In.Set (Nav_Data.Plan_A) then
    Data_In.Plan.Angle := Get_Angle ("plan");
  end if;

  if Data_In.Set (Nav_Data.Traj_S) then
    Data_In.Traj.Speed := Get_Speed("traj");
  end if;
  if Data_In.Set (Nav_Data.Traj_A) then
    Data_In.Traj.Angle := Get_Angle ("traj");
  end if;


  if Data_In.Set (Nav_Data.Drift) then
    Data_In.Drift := Get_Drift ("drift");
  end if;

  Nav_Data.Resolution (Data_In, Report, Data_Out);
  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output (Report'Img);
  Basic_Proc.New_Line_Output;
  if Nav_Data."/=" (Report, Nav_Data.Ok) then
    return;
  end if;

  Basic_Proc.Put_Output ("WIND SPEED ");
  Basic_Proc.Put_Line_Output (
      Normalization.Normal_Fixed (Data_Out.Wind.Speed, 6, 3));
  Basic_Proc.Put_Output ("WIND ANGLE ");
  Basic_Proc.Put_Output (
      Images.Integer_Image (Integer(Data_Out.Wind.Angle.Degrees)));
  Basic_Proc.Put_Output (".");
  Basic_Proc.Put_Line_Output (
      Images.Integer_Image (Integer(Data_Out.Wind.Angle.Minutes)));

  Basic_Proc.Put_Output ("PLAN SPEED ");
  Basic_Proc.Put_Line_Output (
      Normalization.Normal_Fixed (Data_Out.Plan.Speed, 6, 3));
  Basic_Proc.Put_Output ("PLAN ANGLE ");
  Basic_Proc.Put_Output (
      Images.Integer_Image (Integer(Data_Out.Plan.Angle.Degrees)));
  Basic_Proc.Put_Output (".");
  Basic_Proc.Put_Line_Output (
      Images.Integer_Image (Integer(Data_Out.Plan.Angle.Minutes)));

  Basic_Proc.Put_Output ("TRAJ SPEED ");
  Basic_Proc.Put_Line_Output (
      Normalization.Normal_Fixed (Data_Out.Traj.Speed, 6, 3));
  Basic_Proc.Put_Output ("TRAJ ANGLE ");
  Basic_Proc.Put_Output (
      Images.Integer_Image (Integer(Data_Out.Traj.Angle.Degrees)));
  Basic_Proc.Put_Output (".");
  Basic_Proc.Put_Line_Output (
      Images.Integer_Image (Integer(Data_Out.Traj.Angle.Minutes)));

  Basic_Proc.Put_Output ("DRIFT ");
  if Data_Out.Drift.Positiv then
    Basic_Proc.Put_Output ("+");
  else
    Basic_Proc.Put_Output ("-");
  end if;
  Basic_Proc.Put_Output (
      Images.Integer_Image (Integer(Data_Out.Drift.Degrees)));
  Basic_Proc.Put_Output (".");
  Basic_Proc.Put_Line_Output (
      Images.Integer_Image (Integer(Data_Out.Drift.Minutes)));
end T_Nav;

