with Nav_Types, Nav_Data;
with My_Io, Text_Io;
procedure T_Nav is
str : string (1..20);
lst : natural;

  Data_In, Data_Out : Nav_Data.T_Data;
  subtype Ti is Positive range 1..7;
  I1, I2, I3 : Ti;
  Report : Nav_Data.T_Consistency;
  package Cons_Io is new Text_Io.Enumeration_Io (Nav_Data.T_Consistency);

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
        My_Io.Put (Msg & " unknown ? ");
        My_Io.Get_Line (S, L);
        return Ti'Value (S(1..L));
      exception
        when others => My_Io.Put_Line ("ERREUR...");
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
        My_Io.Put (Msg & " angle ? ");
        My_Io.Get_Line (S, L);
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
        when others => My_Io.Put_Line ("ERREUR...");
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
        My_Io.Put (Msg & " angle ? ");
        My_Io.Get_Line (S, L);
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
        when others => My_Io.Put_Line ("ERREUR...");
      end;
    end loop;
  end Get_Drift;

  function Get_Speed (Msg : String) return Nav_Types.T_Speed is
    S : String (1..10);
    L : Natural;
  begin
    loop
      begin
        My_Io.Put (Msg & " speed ? ");
        My_Io.Get_Line (S, L);
        return Nav_Types.T_Speed (Positive'Value (S(1..L)));
      exception
        when others => My_Io.Put_Line ("ERREUR...");
      end;
    end loop;
  end Get_Speed;

begin
  My_Io.Put_Line ("1 : WIND SPEED");
  My_Io.Put_Line ("2 : WIND ANGLE");
  My_Io.Put_Line ("3 : PLAN SPEED");
  My_Io.Put_Line ("4 : PLAN ANGLE");
  My_Io.Put_Line ("5 : TRAJ SPEED");
  My_Io.Put_Line ("6 : TRAJ ANGLE");
  My_Io.Put_Line ("7 : DRIFT");
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
  My_Io.New_Line;
  Cons_Io.Put (Report);
  My_Io.New_Line;
  if Nav_Data."/=" (Report, Nav_Data.Ok) then return; end if;

  My_Io.Put ("WIND SPEED "); My_Io.Put_Line (Data_Out.Wind.Speed);
  My_Io.Put ("WIND ANGLE "); My_Io.Put (Integer(Data_Out.Wind.Angle.Degrees));
  My_Io.Put ("."); My_Io.Put_Line (Integer(Data_Out.Wind.Angle.Minutes));

  My_Io.Put ("PLAN SPEED "); My_Io.Put_Line (Data_Out.Plan.Speed);
  My_Io.Put ("PLAN ANGLE "); My_Io.Put (Integer(Data_Out.Plan.Angle.Degrees));
  My_Io.Put ("."); My_Io.Put_Line (Integer(Data_Out.Plan.Angle.Minutes));

  My_Io.Put ("TRAJ SPEED "); My_Io.Put_Line (Data_Out.Traj.Speed);
  My_Io.Put ("TRAJ ANGLE "); My_Io.Put (Integer(Data_Out.Traj.Angle.Degrees));
  My_Io.Put ("."); My_Io.Put_Line (Integer(Data_Out.Traj.Angle.Minutes));

  My_Io.Put ("DRIFT ");
  if Data_Out.Drift.Positiv then
    My_Io.Put ("+");
  else
    My_Io.Put ("-");
  end if;
  My_Io.Put (Integer(Data_Out.Drift.Degrees)); My_Io.Put (".");
  My_Io.Put_Line (Integer(Data_Out.Drift.Minutes));
end T_Nav;

