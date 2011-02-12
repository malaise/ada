-- Test timers
-- Periodical : 1 then each 3 secs
-- Single     : 1 then [1, 5] secs re-created in the callback
-- Funny      : 0 then in [2.1, 2.9[ with callback (10 times)
--                then once in 20 with no callback, then never
-- Never      : never
-- Escape suspends the Periodic and the Single (if not expired)
-- Return resumes them

with Ada.Text_Io;
with Afpx, Con_Io, Timers, Rnd, Event_Mng;
procedure T_Timers is

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

  Use_Afpx : Boolean := False;

  procedure Display (Str : in String) is
  begin
    if Use_Afpx then
      Put_Line (Str);
    else
      Ada.Text_Io.Put_Line (Str);
    end if;
  end Display;

  -- The timers stuff
  type Timer_List is (Periodic, Single, Funny, Never);

  The_Timers : array (Timer_List) of Timers.Timer_Id;


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
    if Cb then
      A := Callback'Unrestricted_Access;
    else
      A := null;
    end if;
    The_Timers(T).Create (
        Delay_Spec => (Delay_Kind    => Timers.Delay_Sec,
                       Clock         => null,
                       Period        => P,
                       Delay_Seconds => D),
        Callback   => A);
    Display ("Created timer " & Timer_List'Image(T));
  end Start;

  Nb_Funny : Natural := 0;
  Max_Funny : constant Natural := 10;

  -- Generic callback
  function Callback (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Data);
    use type Timers.Timer_Id;
    N : Positive;
  begin
    if not Use_Afpx then
      if Afpx.Is_Descriptor_Set then
        Use_Afpx := True;
      end if;
    end if;
    for T in Timer_List loop
      if The_Timers(T) = Id then
        Display ("Expiration of " & Timer_List'Image(T));
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
    Display ("Expiration of unknown timer");
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
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>

        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Display ("Resuming Periodical and Single.");
            The_Timers(Periodic).Resume;
            begin
              The_Timers(Single).Resume;
            exception
              when Timers.Invalid_Timer =>
                null;
            end;
          when Afpx.Escape_Key =>
            Display ("Suspending Periodical and Single. Resume with Return.");
            The_Timers(Periodic).Suspend;
            begin
              The_Timers(Single).Suspend;
            exception
              when Timers.Invalid_Timer =>
                null;
            end;
          when Afpx.Break_Key =>
            exit;
        end case;
      when Afpx.Mouse_Button =>
        null;
      when Afpx.Fd_Event =>
        null;
      when Afpx.Signal_Event =>
        exit;
      when Afpx.Timer_Event =>
        Display ("Timer Event");
      when Afpx.Refresh =>
Ada.Text_Io.Put_Line ("Refresh");
        Redisplay := True;
    end case;
  end loop;


  for T in Timer_List loop
    begin
      The_Timers(T).Delete;
    exception
      when Timers.Invalid_Timer =>
        Display ("Invalid timer when closing " & Timer_List'Image (T));
    end;
  end loop;

end T_Timers;

