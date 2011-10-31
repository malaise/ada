with Con_Io;
with Nav_Types;
with Nav_Format;
-- Management of the dialog with the operator
package body Nav_Dialog is

  -- remanent data across the get
  -- has the get (mask and data, and put) to be written at next get
  Refresh : Boolean;
  -- same for result
  Result_Put : Boolean;
  Result_Data : Nav_Data.T_Data;

  -- is there an error
  Chk_Error : Boolean;
  Fmt_Error : Boolean;

  -- lengths of strings
  Len_Speed : constant Positive :=
   Nav_Format.Imag(Result_Data.Wind.Speed, False)'Last;
  Len_Angle : constant Positive :=
   Nav_Format.Imag(Result_Data.Wind.Angle, False)'Last;
  Len_Drift : constant Positive :=
   Nav_Format.Imag(Result_Data.Drift, False)'Last;


  -- to Put all the data fields to get
  procedure Put_Data (Data : in Nav_Data.T_Data) is
  begin
    Nav_Screen.Put (
     Field => Nav_Data.Wind_S,
     Str => Nav_Format.Imag(Data.Wind.Speed, Data.Set(Nav_Data.Wind_S) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Wind_A,
     Str => Nav_Format.Imag(Data.Wind.Angle, Data.Set(Nav_Data.Wind_A) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Plan_S,
     Str => Nav_Format.Imag(Data.Plan.Speed, Data.Set(Nav_Data.Plan_S) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Plan_A,
     Str => Nav_Format.Imag(Data.Plan.Angle, Data.Set(Nav_Data.Plan_A) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Traj_S,
     Str => Nav_Format.Imag(Data.Traj.Speed, Data.Set(Nav_Data.Traj_S) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Traj_A,
     Str => Nav_Format.Imag(Data.Traj.Angle, Data.Set(Nav_Data.Traj_A) ),
     Blink => False );
    Nav_Screen.Put (
     Field => Nav_Data.Drift,
     Str => Nav_Format.Imag(Data.Drift, Data.Set(Nav_Data.Drift) ),
     Blink => False );
  end Put_Data;


  -- put the result
  procedure Put (Result : in Nav_Data.T_Data) is
  begin
    Result_Data := Result;
    Result_Put := True;
    for Field in Nav_Data.T_List_Data loop
      if Result.Set(Field) then
        Nav_Screen.Dot(Field);
      else
        Nav_Screen.Arrow(Field);
      end if;
    end loop;
    Nav_Screen.Put_Result (Field => Nav_Data.Wind_S,
     Str  => Nav_Format.Imag(Result.Wind.Speed, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Wind_A,
     Str  => Nav_Format.Imag(Result.Wind.Angle, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Plan_S,
     Str  => Nav_Format.Imag(Result.Plan.Speed, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Plan_A,
     Str  => Nav_Format.Imag(Result.Plan.Angle, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Traj_S,
     Str  => Nav_Format.Imag(Result.Traj.Speed, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Traj_A,
     Str  => Nav_Format.Imag(Result.Traj.Angle, True));
    Nav_Screen.Put_Result (Field => Nav_Data.Drift,
     Str  => Nav_Format.Imag(Result.Drift, True));
  end Put;


  -- clear the result
  procedure Clear_Result is
  begin
    Result_Put := False;
    for Field in Nav_Data.T_List_Data loop
      Nav_Screen.Clear_Line(Field);
    end loop;
    Nav_Screen.Put_Result (Field => Nav_Data.Wind_S,
     Str  => Nav_Format.Imag(Result_Data.Wind.Speed, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Wind_A,
     Str  => Nav_Format.Imag(Result_Data.Wind.Angle, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Plan_S,
     Str  => Nav_Format.Imag(Result_Data.Plan.Speed, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Plan_A,
     Str  => Nav_Format.Imag(Result_Data.Plan.Angle, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Traj_S,
     Str  => Nav_Format.Imag(Result_Data.Traj.Speed, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Traj_A,
     Str  => Nav_Format.Imag(Result_Data.Traj.Angle, False));
    Nav_Screen.Put_Result (Field => Nav_Data.Drift,
     Str  => Nav_Format.Imag(Result_Data.Drift, False));
  end Clear_Result;


  -- put consistency error message
  procedure Put (Error : in Nav_Data.T_Consistency) is
  begin
    Chk_Error := True;
    Fmt_Error := False;
    Nav_Screen.Err_Check(Error);
  end Put;


  -- Inits the string according to a field a field
  procedure To_Str (
   Curr_Field : in Nav_Data.T_List_Data;
   Data       : in Nav_Data.T_Data;
   Str : out String;
   Len : out Positive) is
  begin
    case Curr_Field is
      when Nav_Data.Wind_S =>
        Len := Len_Speed;
        Str (Str'First .. Len_Speed) :=
         Nav_Format.Imag(Data.Wind.Speed, Data.Set(Nav_Data.Wind_S));
      when Nav_Data.Wind_A =>
        Len := Len_Angle;
        Str (Str'First .. Len_Angle) :=
         Nav_Format.Imag(Data.Wind.Angle, Data.Set(Nav_Data.Wind_A));
      when Nav_Data.Plan_S =>
        Len := Len_Speed;
        Str (Str'First .. Len_Speed) :=
         Nav_Format.Imag(Data.Plan.Speed, Data.Set(Nav_Data.Plan_S));
      when Nav_Data.Plan_A =>
        Len := Len_Angle;
        Str (Str'First .. Len_Angle) :=
         Nav_Format.Imag(Data.Plan.Angle, Data.Set(Nav_Data.Plan_A));
      when Nav_Data.Traj_S =>
        Len := Len_Speed;
        Str (Str'First .. Len_Speed) :=
         Nav_Format.Imag(Data.Traj.Speed, Data.Set(Nav_Data.Traj_S));
      when Nav_Data.Traj_A =>
        Len := Len_Angle;
        Str (Str'First .. Len_Angle) :=
         Nav_Format.Imag(Data.Traj.Angle, Data.Set(Nav_Data.Traj_A));
      when Nav_Data.Drift =>
        Len := Len_Drift;
        Str (Str'First .. Len_Drift) :=
         Nav_Format.Imag(Data.Drift, Data.Set(Nav_Data.Drift));
    end case;
  end To_Str;


  -- Updates the data if string is correct
  procedure To_Value (
   Curr_Field : in Nav_Data.T_List_Data;
   Str : in String;
   Data : in out Nav_Data.T_Data;
   Ok  : out Boolean;
   Pos : out Positive) is
    Speed : Nav_Types.T_Speed;
    Angle : Nav_Types.T_Angle;
    Drift : Nav_Types.T_Drift;
    Res : Nav_Format.Format_Result;
    use Nav_Format; -- for = tests
  begin

    -- get value and update data value if Set
    case Curr_Field is
      when Nav_Data.Wind_S =>
        Nav_Format.Value (Str, Speed, Res, Pos);
        if Res = Nav_Format.Set then Data.Wind.Speed := Speed; end if;
      when Nav_Data.Wind_A =>
        Nav_Format.Value (Str, Angle, Res, Pos);
        if Res = Nav_Format.Set then Data.Wind.Angle := Angle; end if;
      when Nav_Data.Plan_S =>
        Nav_Format.Value (Str, Speed, Res, Pos);
        if Res = Nav_Format.Set then Data.Plan.Speed := Speed; end if;
      when Nav_Data.Plan_A =>
        Nav_Format.Value (Str, Angle, Res, Pos);
        if Res = Nav_Format.Set then Data.Plan.Angle := Angle; end if;
      when Nav_Data.Traj_S =>
        Nav_Format.Value (Str, Speed, Res, Pos);
        if Res = Nav_Format.Set then Data.Traj.Speed := Speed; end if;
      when Nav_Data.Traj_A =>
        Nav_Format.Value (Str, Angle, Res, Pos);
        if Res = Nav_Format.Set then Data.Traj.Angle := Angle; end if;
      when Nav_Data.Drift =>
        Nav_Format.Value (Str, Drift, Res, Pos);
        if Res = Nav_Format.Set then Data.Drift := Drift; end if;
    end case;

    -- update data.set and ok
    case Res is
      when Nav_Format.Set =>
        -- data already updated
        Data.Set(Curr_Field) := True;
        Ok := True;
      when Nav_Format.Unset =>
        Data.Set(Curr_Field) := False;
        Ok := True;
      when Nav_Format.Error =>
        -- pos already set. data unchanged
        Ok := False;
    end case;

  exception
    when others =>
      Pos := 1;
      Ok := False;
  end To_Value;


  -- get data and then the action (compute or quit)
  procedure Get (Data : in out Nav_Data.T_Data; To_Do : out Action) is
    In_Action : Boolean;
    -- data field if not in action
    Curr_Field : Nav_Data.T_List_Data;
    -- Action position if in action
    Curr_Action : Nav_Screen.Action;
    -- if error
    Blink : Boolean;
    -- got string
    Get_Str : String (1..10);
    Get_Len : Positive;
    -- cursor position and insert status
    Pos : Positive;
    Ins : Boolean;
    -- result of try to convert string to a value : ok if set or unset
    Ok : Boolean;
    -- movement
    Nxt : Nav_Screen.Movement;
    -- data at beginning of get
    Data_In : constant Nav_Data.T_Data := Data;

    use type Nav_Screen.Movement;

  begin
    Fmt_Error := False;
    Nxt := Con_Io.Down;
    -- go to first field. may be adapted in case of check error
    In_Action := False;
    Curr_Field := Nav_Data.T_List_Data'First;

    loop
      if Refresh then
        -- data part has to be put again
        Nav_Screen.Reset;
        Nav_Screen.Title;
        Nav_Screen.Put_Mask;
        Put_Data (Data);
        Refresh := False;
        if Result_Put then
          -- result has to be put again
          Put (Result_Data);
        else
          Clear_Result;
        end if;
      end if;

      -- show error : may be set also if check error
      Blink := Fmt_Error; -- or Chk_Error

      if not In_Action then

        if Nxt /= Con_Io.Timeout then
          -- new field or error
          Ins := False;
          -- build field from data if not format error (new field)
          if not Fmt_Error then
            To_Str(Curr_Field, Data, Get_Str, Get_Len);
            Pos := 1;
          end if;
        end if;

        -- get
        Nav_Screen.Get (Curr_Field, Blink, Get_Str(1..Get_Len), Pos, Ins, Nxt);

        -- after validated get or timeout, clear error
        if Chk_Error or else Fmt_Error then
          Nav_Screen.Clear_Err;
        end if;
        Chk_Error := False;
        Fmt_Error := False;

        -- if not timeout, try to have a value
        if Nxt /= Con_Io.Timeout then

          if Nxt = Con_Io.Esc then
            -- on escape : unknown
            Get_Str (1) := '?';
          end if;

          -- Value of got field.
          To_Value (Curr_Field, Get_Str(1..Get_Len), Data, Ok, Pos);

          if Result_Put and then Nav_Data."/=" (Data, Data_In) then
            -- result (if any) is not valid any more
            Clear_Result;
            Result_Put := False;
          end if;


          if Ok then
            -- put formated data in got field
            To_Str(Curr_Field, Data, Get_Str, Get_Len);
            Nav_Screen.Put (Curr_Field, Get_Str(1..Get_Len), False);
            -- Movement if ok
            case Nxt is
              when Con_Io.Up | Con_Io.Left | Con_Io.Stab =>
                if Nav_Data."/="
                 (Curr_Field, Nav_Data.T_List_Data'First) then
                  Curr_Field := Nav_Data.T_List_Data'Pred(Curr_Field);
                else
                  In_Action := True;
                end if;
              when Con_Io.Down | Con_Io.Right | Con_Io.Tab |
                   Con_Io.Ret | Con_Io.Full =>
                if Nav_Data."/="
                 (Curr_Field, Nav_Data.T_List_Data'Last) then
                  Curr_Field := Nav_Data.T_List_Data'Succ(Curr_Field);
                else
                  In_Action := True;
                end if;
              when Con_Io.Pgup =>
                Curr_Field := Nav_Data.T_List_Data'First;
              when Con_Io.Pgdown =>
                In_Action := True;
              when Con_Io.Ctrl_Pgup | Con_Io.Ctrl_Pgdown
                 | Con_Io.Ctrl_Up   | Con_Io.Ctrl_Down
                 | Con_Io.Ctrl_Left | Con_Io.Ctrl_Right =>
                null;
              when Con_Io.Refresh =>
                Refresh := True;
              when Con_Io.Fd_Event | Con_Io.Timer_Event | Con_Io.Signal_Event =>
                null;
              when Con_Io.Timeout | Con_Io.Esc |
                   Con_Io.Mouse_Button | Con_Io.Break | Con_Io.Selection =>
                -- impossible to be here
                null;
            end case;
            Pos := 1;
          else
            -- format error and loop
            Fmt_Error := True;
            Nav_Screen.Err_Format;
          end if;
        end if;
      else -- in action

        -- get action
        Curr_Action := Nav_Screen.Get_Action;

        -- do action or movement
        case Curr_Action is
          when Nav_Screen.Compute =>
            To_Do := Nav_Screen.Compute;
            return;
          when Nav_Screen.Quit =>
            if Nav_Screen.Confirm_Quit then
              To_Do := Nav_Screen.Quit;
              return;
            else
              Refresh := True;
            end if;
          when Nav_Screen.Help =>
            Nav_Screen.Put_Help;
            Refresh := True;
          when Nav_Screen.Clear =>
            -- clear the data
            for Field in Nav_Data.T_List_Data loop
              Data.Set (Field) := False;
            end loop;
            Refresh := True;
            -- clear the result
            Clear_Result;
          when Nav_Screen.Prev =>
            In_Action := False;
            Curr_Field := Nav_Data.T_List_Data'Last;
          when Nav_Screen.Next =>
            In_Action := False;
            Curr_Field := Nav_Data.T_List_Data'First;
          when Nav_Screen.Refresh =>
            Refresh := True;
        end case;

      end if;

    end loop;

  end Get;

  -- initialisation of screen and of remanent data
  procedure Init is
  begin
    -- init screen
    Nav_Screen.Reset;

    -- get mask and data fields have to be put at next get
    Refresh := True;
    Result_Put := False;
    Chk_Error := False;
  end Init;

end Nav_Dialog;


