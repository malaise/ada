-- Test timers
-- Periodical : 1 then each 3 secs
-- Single     : 1 then [1, 5] secs re-created in the callback
-- Funny      : 0 then in [2.1, 2.9[ with callback (10 times)
--                then once in 20 with no callback (then never)
-- Never      : never

with Ada.Text_Io;
with Afpx, Con_Io, Timers, Rnd, Event_Mng;
procedure T_Timers is

  Timers_Error : exception;

  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean;

  -- Row Management
  F : constant Afpx.Field_Range := 1;
  Cur_Row : Natural := 0;
  Erase : Boolean := False;
  procedure Put_Line (Str : in String) is
    S : String(1 .. Afpx.Get_Field_Width(F)) := (others => ' ');
    H : Afpx.Height_Range;
    W : Afpx.Width_Range;
  begin
    if Erase then
      Afpx.Clear_Field (F);
      Erase := False;
    end if;
    S (1 .. Str'Length) := Str;
    Afpx.Encode_Field (F, (Cur_Row, 0), S);
    Afpx.Get_Field_Size (F, H, W);
    if Cur_Row = H - 1 then
      Cur_Row := 0;
      Erase := True;
    else
      Cur_Row := Cur_Row + 1;
    end if;
  end Put_Line;


  -- The timers stuff
  type Timer_List is (Periodic, Single, Funny, Never);

  The_Timers : array (Timer_List) of Timers.Timer_Id;

  Use_Afpx : Boolean := False;

  -- Generic callback
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean;

  -- Start a timer and store its id
  procedure Start (T : Timer_List;
                   D : in Duration;
                   P : in Timers.Period_Range;
                   Cb : in Boolean) is
    A : Timers.Timer_Callback;
  begin
    if Cb
      then A := Callback'Unrestricted_Access;
    else
      A := null;
    end if;
    The_Timers(T) := Timers.Create (
        Delay_Spec => (Delay_Kind => Timers.Delay_Sec,
                       Delay_Seconds => D,
                       Period => P),
        Callback   => A);
    if Use_Afpx then
      Put_Line ("Created timer " & Timer_List'Image(T) & ": "
              & Timers.Image(The_Timers(T)));
    else
      Ada.Text_Io.Put_Line ("Created timer " & Timer_List'Image(T) & ": "
                          & Timers.Image(The_Timers(T)));
    end if;
  end Start;

  Nb_Funny : Natural := 0;
  Max_Funny : constant Natural := 10;

  -- Generic callback
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    use type Timers.Timer_Id;
    N : Positive;
  begin
    if not Use_Afpx then
      declare
        W : Afpx.Width_Range;
      begin
        W := Afpx.Get_Field_Width (F);
        Use_Afpx := True;
      exception
        when Afpx.No_Descriptor =>
          null;
      end;
    end if;
    for T in Timer_List loop
      if The_Timers(T) = Id then
        if Use_Afpx then
          Put_Line ("Expiration of " & Timer_List'Image(T));
        else
          Ada.Text_Io.Put_Line ("Expiration of " & Timer_List'Image(T));
        end if;
        if T = Single then
          N := Rnd.Int_Random (1, 5);
          Start (Single, Duration(N), Timers.No_Period, True);
        elsif T = Funny then
          Nb_Funny := Nb_Funny + 1;
          if Nb_Funny <= Max_Funny then
            Start (Funny, Rnd.Dur_Random (2.1, 2.9), Timers.No_Period, True);
          else
            Start (Funny, 20.0, Timers.No_Period, False);
          end if;
          return False;
        end if;
        return True;
      end if;
    end loop;
    Ada.Text_Io.Put_Line ("Expiration of unknown timer:" & Timers.Image(Id));
    return False;
  end Callback;

begin
  Rnd.Randomize;

  -- Start timers
  Start (Periodic, 1.0, 3.0, True);
  Start (Single, 1.0, Timers.No_Period, True);
  Start (Funny, 0.0, Timers.No_Period, True);

  -- Wait a bit
  loop
    exit when Event_Mng.Wait (1_000);
  end loop;

  -- Afpx init
  Afpx.Use_Descriptor (6);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Insert := False;
  Redisplay := False;


  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, Redisplay);
    case Ptg_Result.Event is
      when Afpx.Keyboard =>

        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            null;
          when Afpx.Escape_Key =>
            null;
          when Afpx.Break_Key =>
            exit;
        end case;
      when Afpx.Mouse_Button =>
        null;
      when Afpx.Fd_Event =>
        null;
      when Afpx.Wakeup_Event =>
        null;
      when Afpx.Signal_Event =>
        exit;
      when Afpx.Timer_Event =>
        Put_Line ("Timer Event");
      when Afpx.Refresh =>
        Redisplay := True;
    end case;
  end loop;


  for T in Timer_List loop
    begin
      Timers.Delete (The_Timers(T));
    exception
      when Timers.Invalid_Timer =>
        Ada.Text_Io.Put_Line ("Invalid timer when closing "
          & Timer_List'Image (T));
    end;
  end loop;

end T_Timers;

