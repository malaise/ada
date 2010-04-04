with Ada.Exceptions, Ada.Calendar;
with My_Io, Normal, Argument, Timers, Language;
with Generic_Con_Io;

procedure T_Con is


  task type Task_T is
    pragma Storage_Size (1024 * 1024);
    entry Start(I : in Positive);
  end Task_T;

  Nb_Tasks : constant := 2;
  T : array (1 .. Nb_Tasks) of Task_T;

  task body Task_T is

    package Con_Io is new Generic_Con_Io.One_Con_Io(1);

    Me : Positive;

    W1, W2, W3 : Con_Io.Window;
    Str : Wide_String (1..25);
    Last : Natural;
    Col : Natural;
    Stat : Con_Io.Curs_Mvt;
    Str_Exit : constant Wide_String := "exit";
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
      Con_Io.Move (0, 0, W3);
      T := Natural(Ada.Calendar.Seconds(Ada.Calendar.Clock) - T0);
      Con_Io.Put (Natural'Image(T) & String'("   "), W3);
    end Show_Clock;

    procedure Redraw is
    begin
      Con_Io.Clear (W1);
      Con_Io.Clear (W2);
      Con_Io.Clear (W3);
      Show_Clock;
    end Redraw;

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
      Con_Io.Init;
    end Start;

    Con_Io.Reset_Term;
    Con_Io.Enable_Motion_Events(True);
    -- fenetre de saisie, fenetre d'affichage
    Con_Io.Open ( W1, ( 5, 15), (10, 78));
    Con_Io.Open ( W2, (15,  1), (17, 78));
    Con_Io.Open ( W3, (20,  0), (20, 9));

    Con_Io.Set_Foreground (Con_Io.Color_Of ("Light_Blue"), W1);
    Con_Io.Set_Foreground (Con_Io.Color_Of ("Cyan"), W2);
    Con_Io.Set_Foreground (Con_Io.Color_Of ("Lime_Green"), W3);

    Con_Io.Set_Background (Con_Io.Color_Of ("Blue"), W1);
    Con_Io.Set_Background (Con_Io.Color_Of ("Red"), W2);
    Con_Io.Set_Background (Con_Io.Color_Of ("Dark_Green"), W3);

    Redraw;

    Con_Io.Move (1, Col, W1);
    Con_Io.Get (Str(1..Width), Last, Stat, Pos, Ins,
       W1, Con_Io.Current, Con_Io.Color_Of ("Red"), Delt);
    loop
        Con_Io.Clear (W2);
        Con_Io.Putw (">" & Str(1..Last) & "<"
            & Language.String_To_Wide (Con_Io.Curs_Mvt'Image(Stat)), W2);
        My_Io.Put_Line (Positive'Image(Me)
                      & " >" & Con_Io.Wide_To_String (Str(1..Last))
                      & "<" & Con_Io.Curs_Mvt'Image(Stat));
        case Stat is
          when Con_Io.Esc =>
            Str (1 .. Width) := (others => ' ');
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
            Con_Io.Get_Mouse_Event (Mouse_Event);
            if Mouse_Event.Valid then
              Con_Io.Put (" T", W2);
            else
              Con_Io.Put (" D", W2);
            end if;
            if Mouse_Event.Status = Con_Io.Pressed then
              Con_Io.Put (" P", W2);
            elsif Mouse_Event.Status = Con_Io.Released then
              Con_Io.Put (" R", W2);
            elsif Mouse_Event.Status = Con_Io.Motion then
              Con_Io.Put (" M", W2);
            end if;
            if Mouse_Event.Button = Con_Io.Left then
              Con_Io.Put (" L", W2);
            elsif Mouse_Event.Button = Con_Io.Middle then
              Con_Io.Put (" M", W2);
            elsif Mouse_Event.Button = Con_Io.Right then
              Con_Io.Put (" R", W2);
            elsif Mouse_Event.Button = Con_Io.Motion then
              Con_Io.Put (" x", W2);
            elsif Mouse_Event.Button = Con_Io.Up then
              Con_Io.Put (" U", W2);
            elsif Mouse_Event.Button = Con_Io.Down then
              Con_Io.Put (" D", W2);
            end if;
            Con_Io.Put (Normal(Mouse_Event.Row, 4)
                      & Normal(Mouse_Event.Col, 4), W2);
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
        Con_Io.Move (1, Col, W1);
        Con_Io.Put_Then_Get (Str(1..Width), Last, Stat, Pos, Ins,
         W1, Con_Io.Current, Con_Io.Color_Of ("Red"), Delt);
    end loop;


    Con_Io.Enable_Motion_Events (False);
    for I in 1 .. 3 loop
      Con_Io.Clear (W1);
      Con_Io.Move (6 - I, 2, W1);
      Con_Io.Put ("Exiting", W1, Con_Io.Color_Of ("Red"),
                  Con_Io.Color_Of ("Dark_Green"));
      Con_Io.Get (Str(1..0), Last, Stat, Pos, Ins,
         W1, Con_Io.Current, Con_Io.Color_Of ("Red"),
         (Delay_Kind    => Timers.Delay_Sec,
          Clock         => null,
          Period        => Con_Io.No_Period,
          Delay_Seconds => 3.0) );
    end loop;

    Con_Io.Destroy;
    delay 3.0;

    Con_Io.Init;
    delay 2.0;
    Con_Io.Destroy;
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
  Generic_Con_Io.Initialise;

  for I in T'Range loop
    T(I).Start(I);
    delay 1.0;
  end loop;
  My_Io.Put_Line ("Main Terminated");
end T_Con;

