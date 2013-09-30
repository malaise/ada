-- Test timers
-- Periodical : 1 then each 3 secs
-- Single     : 1 then [1, 5] secs re-created in the callback
-- Funny      : 0 then in [2.1, 2.9[ with callback (10 times)
--                then once in 20 with no callback, then never
-- Never      : never
-- Escape suspends the Periodic and the Single (if not expired)
-- Return resumes them

with Basic_Proc, Afpx, Timers, Rnd, Event_Mng, Mixed_Str;
procedure T_Timers is

  -- Afpx stuff
  Get_Handle : Afpx.Get_Handle_Rec;
  Ptg_Result : Afpx.Result_Rec;

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
      Basic_Proc.Put_Line_Output (Str);
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
    Display ("Created timer " & Timer_List'Image(T)
           & " with Cb " & Mixed_Str (Cb'Img));
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
          N := Rnd.Gen.Int_Random (1, 5);
          Start (Single, Duration(N), Timers.No_Period, True);
        elsif T = Funny then
          Nb_Funny := Nb_Funny + 1;
          if Nb_Funny <= Max_Funny then
            Start (Funny, Rnd.Gen.Dur_Random (2.1, 2.9),
                   Timers.No_Period, True);
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
  Display ("Use Esc/Return to Suspend/Resume periodical and single timers");
  Rnd.Gen.Randomize;

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


  loop
    Afpx.Put_Then_Get (Get_Handle,Ptg_Result);
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
      when Afpx.Timer_Event =>
        Display ("Timer Event");
      when Afpx.Mouse_Button =>
        null;
      when Afpx.Fd_Event =>
        null;
      when Afpx.Signal_Event =>
        exit;
      when Afpx.Refresh =>
        null;
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

