-- Test timers
-- Periodical : 1 then each 3 secs
-- Single     : 1 then [1, 5] secs re-created in the callback
-- Funny      : 0 then once [10, 15[ in then callback then never
-- Never      : never

with Ada.Text_Io;
with Afpx, Con_Io, Timers, Rnd, X_Mng;
procedure T_Timers is

  Timers_Error : exception;

  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
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

  -- Generic callback
  procedure CallBack (Id : in Timers.Timer_Id);

  -- Start a timer and store its id
  procedure Start (T : Timer_List;
                   D : in Duration;
                   P : in Timers.Period_Range;
                   CB : in Boolean) is
    A : Timers.Timer_Callback;
  begin
   if CB
     then A := CallBack'Unrestricted_Access;
   else
     A := null;
   end if;
   The_Timers(T) := Timers.Create (
       Delay_Spec => (Delay_Kind => Timers.Delay_Sec,
                      Delay_Seconds => D,
                      Period => P),
       Callback   => A);
  end Start;

  -- Generic callback
  procedure CallBack (Id : in Timers.Timer_Id) is
    Use_Afpx : Boolean;
    use type Timers.Timer_Id;
    N : Positive;
  begin
    declare
      W : Afpx.Width_Range;
    begin
      W := AFpx.Get_FIeld_Width (F);
      Use_Afpx := True;
    exception
      when Afpx.No_Descriptor =>
        Use_Afpx := False;
    end;
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
          Start (Funny, Rnd.Dur_Random (10.0, 15.0), Timers.No_Period, False);
        end if;     
        return;
      end if;
    end loop;
    Ada.Text_Io.Put_Line ("Expiration of unknown timer");
  end CallBack;

begin
  Rnd.Randomize;

  -- Start timers
  Start (Periodic, 1.0, 3.0, True);
  Start (Single, 1.0, Timers.No_Period, True);
  Start (Funny, 0.0, Timers.No_Period, True);

  -- Wait a bit
  loop
    exit when X_Mng.Select_No_X (1_000);
  end loop;

  -- Afpx init
  Afpx.Use_Descriptor (6);
  Cursor_Field := 1;
  Cursor_Col := 0;
  Redisplay := False;


  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
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

