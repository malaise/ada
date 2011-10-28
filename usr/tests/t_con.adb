with Ada.Exceptions, Ada.Calendar;
with My_Io, Normal, Argument, Timers, Language;
with Con_Io;

procedure T_Con is


  task type Task_T is
    pragma Storage_Size (1024 * 1024);
    entry Start(I : in Positive);
  end Task_T;

  Nb_Tasks : constant := 2;
  T : array (1 .. Nb_Tasks) of Task_T;


  task body Task_T is

    Console : Con_Io.Console;

    Me : Positive;

    W1, W2, W3 : Con_Io.Window;
    Str : Language.Unicode_Sequence (1..25);
    Last : Natural;
    Col : Natural;
    Stat : Con_Io.Curs_Mvt;
    Str_Exit : constant Language.Unicode_Sequence
             := Language.Copy (String'("exit"));
    Width : Natural;
    Delt : constant Con_Io.Delay_Rec(Timers.Delay_Sec)
         := (Delay_Kind    => Timers.Delay_Sec,
             Clock         => null,
             Period        => Con_Io.No_Period,
             Delay_Seconds => 10.0);
    Pos : Positive;
    Ins : Boolean;
    T0  : constant Ada.Calendar.Day_Duration
        := Ada.Calendar.Seconds(Ada.Calendar.Clock);

    Mouse_Event : Con_Io.Mouse_Event_Rec;

    use type Con_Io.Curs_Mvt, Con_Io.Mouse_Button_Status_List,
             Con_Io.Mouse_Button_List;

    procedure Show_Clock is
      T : Natural;
    begin
      W3.Move (0, 0);
      T := Natural(Ada.Calendar.Seconds(Ada.Calendar.Clock) - T0);
      W3.Put (Natural'Image(T) & String'("   "));
    end Show_Clock;

    procedure Redraw is
    begin
      W1.Clear;
      W2.Clear;
      W3.Clear;
      Show_Clock;
    end Redraw;

    use type Con_Io.Unicode_Sequence;
  begin
    begin
      Col := 1;
      Width := 20;
      Col := Natural'Value (Argument.Get_Parameter (1));
      Width := Natural'Value (Argument.Get_Parameter (2));
    exception
      when Argument.Argument_Not_Found => null;
    end;
    accept Start(I : in Positive) do
      Me := I;
      Console := Con_Io.Create (1);
    end Start;

    Console.Reset_Term;
    Console.Enable_Motion_Events(True);
    -- fenetre de saisie, fenetre d'affichage
    W1 := Console.Open ( ( 5, 15), (10, 78)).all;
    W1.Set_Foreground (Con_Io.Color_Of ("Light_Blue"));
    W1.Set_Background (Con_Io.Color_Of ("Blue"));

    W2 := Console.Open ( (15,  1), (17, 78)).all;
    W2.Set_Foreground (Con_Io.Color_Of ("Cyan"));
    W2.Set_Background (Con_Io.Color_Of ("Red"));

    W3 := Console.Open ( (20,  0), (20, 9)).all;
    W3.Set_Foreground (Con_Io.Color_Of ("Lime_Green"));
    W3.Set_Background (Con_Io.Color_Of ("Dark_Green"));

    Redraw;

    W1.Move (1, Col);
    W1.Get (Str(1..Width), Last, Stat, Pos, Ins,
       Con_Io.Current, Con_Io.Color_Of ("Red"), Delt);
    loop
        W2.Clear;
        W2.Putu (
              Language.Char_To_Unicode ('>')
            & Str(1..Last)
            & Language.Char_To_Unicode ('<')
            & Language.String_To_Unicode (Con_Io.Curs_Mvt'Image(Stat)));
        My_Io.Put_Line (Positive'Image(Me)
                      & " >" & Language.Unicode_To_String (Str(1 .. Last))
                      & "<" & Con_Io.Curs_Mvt'Image(Stat));
        case Stat is
          when Con_Io.Esc =>
            Str (1 .. Width) := (others => Con_Io.Space);
            Pos := 1;
            Ins := False;
          when Con_Io.Refresh =>
            Redraw;
          when Con_Io.Break =>
            exit;
          when Con_Io.Ret =>
            if Str (1..Last) = Str_Exit then
              exit;
            end if;
          when Con_Io.Mouse_Button =>
            Console.Get_Mouse_Event (Mouse_Event);
            if Mouse_Event.Valid then
              W2.Put (" T");
            else
              W2.Put (" D");
            end if;
            if Mouse_Event.Status = Con_Io.Pressed then
              W2.Put (" P");
            elsif Mouse_Event.Status = Con_Io.Released then
              W2.Put (" R");
            elsif Mouse_Event.Status = Con_Io.Motion then
              W2.Put (" M");
            end if;
            if Mouse_Event.Button = Con_Io.Left then
              W2.Put (" L");
            elsif Mouse_Event.Button = Con_Io.Middle then
              W2.Put (" M");
            elsif Mouse_Event.Button = Con_Io.Right then
              W2.Put (" R");
            elsif Mouse_Event.Button = Con_Io.Motion then
              W2.Put (" x");
            elsif Mouse_Event.Button = Con_Io.Up then
              W2.Put (" U");
            elsif Mouse_Event.Button = Con_Io.Down then
              W2.Put (" D");
            end if;
            W2.Put (Normal(Mouse_Event.Row, 4)
                      & Normal(Mouse_Event.Col, 4));
            if Mouse_Event.Valid
            and then Mouse_Event.Status = Con_Io.Pressed
            and then Mouse_Event.Button = Con_Io.Left
            and then Mouse_Event.Row = Con_Io.Row_Range'First
            and then Mouse_Event.Col = Con_Io.Col_Range'First then
              exit;
            end if;
          when Con_Io.Up | Con_Io.Down | Con_Io.Ctrl_Up | Con_Io.Ctrl_Down |
               Con_Io.Pgup | Con_Io.Pgdown |
               Con_Io.Ctrl_Pgup | Con_Io.Ctrl_Pgdown |
               Con_Io.Left | Con_Io.Right |
               Con_Io.Ctrl_Left | Con_Io.Ctrl_Right |
               Con_Io.Full | Con_Io.Tab | Con_Io.Stab |
               Con_Io.Selection | Con_Io.Timeout | Con_Io.Fd_Event |
               Con_Io.Timer_Event | Con_Io.Signal_Event =>
            null;
        end case;
        Show_Clock;
        W1.Move (1, Col);
        W1.Put_Then_Get (Str(1..Width), Last, Stat, Pos, Ins,
          Con_Io.Current, Con_Io.Color_Of ("Red"), Delt);
    end loop;


    Console.Enable_Motion_Events (False);
    for I in 1 .. 3 loop
      W1.Clear;
      W1.Move (6 - I, 2);
      W1.Put ("Exiting", Con_Io.Color_Of ("Red"),
                  Con_Io.Color_Of ("Dark_Green"));
      W1.Get (Str(1..0), Last, Stat, Pos, Ins,
         Con_Io.Current, Con_Io.Color_Of ("Red"),
         (Delay_Kind    => Timers.Delay_Sec,
          Clock         => null,
          Period        => Con_Io.No_Period,
          Delay_Seconds => 3.0) );
    end loop;

    Console.Destroy;
    delay 3.0;

    Console := Con_Io.Create (1);
    delay 2.0;
    Console.Destroy;
    delay 1.0;

    My_Io.Put_Line (Me'Img & " Terminated");

  exception
    when Error : others =>
       My_Io.Put_Line ("Exception in " &  Me'Img
                     & " " & Ada.Exceptions.Exception_Name (Error));
       raise;
  end Task_T;


begin
  -- Init Con_Io with stack of main
  Con_Io.Initialise;

  for I in T'Range loop
    T(I).Start(I);
    delay 1.0;
  end loop;
  My_Io.Put_Line ("Main Terminated");
end T_Con;

