with Ada.Exceptions, Ada.Calendar;
with My_Io, Normal, Argument, Timers;
with Generic_Con_Io;

procedure T_Con is

  
  task type Task_T is
    entry Start(I : in Positive);
  end Task_T;
  for Task_T'Storage_Size use 64 * 1024;

  Nb_Tasks : constant := 2;
  T : array (1 .. Nb_Tasks) of Task_T;

  task body Task_T is

    package Con_Io is new Generic_Con_Io.One_Con_Io(1);
    use Con_Io;

    Me : Positive;

    W1, W2, W3 : Window;
    Str : String (1..25);
    Last : Natural;
    Col : Natural;
    Stat : Curs_Mvt;
    Str_Exit : constant String := "exit";
    Width : Natural;
    Delt : constant Con_Io.Delay_Rec(Timers.Delay_Sec)
         := (Delay_Kind => Timers.Delay_Sec,
             Period => Con_Io.No_Period,
             Delay_Seconds => 10.0);
    Pos : Positive;
    Ins : Boolean;
    T0  : constant Ada.Calendar.Day_Duration
        := Ada.Calendar.Seconds(Ada.Calendar.Clock);

    Mouse_Event : Con_Io.Mouse_Event_Rec;

    procedure Show_Clock is
      T : Natural;
    begin
      Move (0, 0, W3);
      T := Natural(Ada.Calendar.Seconds(Ada.Calendar.Clock) - T0);
      Put (Natural'Image(T) & "   ", W3);
    end Show_Clock;

    procedure Redraw is
    begin
      Clear (W1);
      Clear (W2);
      Clear (W3);
      Frame(Name => W2);
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
      Init;
    end Start;

    Reset_Term;
    Enable_Motion_Events(True);
    -- fenetre de saisie, fenetre d'affichage
    Open ( W1, ( 5, 15), (10, 78));
    Open ( W2, (15,  1), (17, 78));
    Open ( W3, (20,  0), (20, 9));

    Set_Foreground (Light_Blue, Not_Blink, W1);
    Set_Foreground (Cyan, Not_Blink, W2);
    Set_Foreground (Light_Green, Not_Blink, W3);

    Set_Background (Blue, W1);
    Set_Background (Red, W2);
    Set_Background (Green, W3);

    Redraw;

    Move (1, Col, W1);
    Get (Str(1..Width), Last, Stat, Pos, Ins,
       W1, Current, Current, Red, Delt);
    loop
        Clear (W2);
        Put (">" & Str(1..Last) & "<" & Curs_Mvt'Image(Stat), W2 );
        My_Io.Put_Line ( Positive'Image(Me) & " >" & Str(1..Last) & "<" & Curs_Mvt'Image(Stat));
        if Stat = Esc then
          Str (1 .. Width) := (others => ' ');
          Pos := 1;
          Ins := False;
        elsif Stat = Refresh then
          Redraw;
        elsif Stat = Break then
          exit;
        elsif Stat = Ret then
          if Str (1..Last) = Str_Exit then
            exit;
          end if;
        elsif Stat = Mouse_Button then
          Con_Io.Get_Mouse_Event (Mouse_Event);   
          if Mouse_Event.Valid then
            Put (" T", W2);
          else
            Put (" D", W2);
          end if;
          if Mouse_Event.Status = Pressed then
            Put (" P", W2);
          elsif Mouse_Event.Status = Released then
            Put (" R", W2);
          elsif Mouse_Event.Status = Motion then
            Put (" M", W2);
          end if;
          if Mouse_Event.Button = Left then
            Put (" L", W2);
          elsif Mouse_Event.Button = Middle then
            Put (" M", W2);
          elsif Mouse_Event.Button = Right then
            Put (" R", W2);
          elsif Mouse_Event.Button = Motion then
            Put (" x", W2);
          elsif Mouse_Event.Button = Up then
            Put (" U", W2);
          elsif Mouse_Event.Button = Down then
            Put (" D", W2);
          end if;
          Put (Normal(Mouse_Event.Row, 4) & Normal(Mouse_Event.Col, 4), W2);
          if Mouse_Event.Valid
          and then Mouse_Event.Status = Pressed
          and then Mouse_Event.Button = Left
          and then Mouse_Event.Row = Row_Range'First
          and then Mouse_Event.Col = Col_Range'First then
            exit;
          end if;
        end if;
        Show_Clock;
        Move (1, Col, W1);
        Put_Then_Get(Str(1..Width), Last, Stat, Pos, Ins,
         W1, Current, Current, Red, Delt);
    end loop;


    Enable_Motion_Events(False);
    for I in 1 .. 3 loop
      Clear (W1);
      Move (6 - I, 2, W1);
      Put ("Exiting", W1, Red, Blink, Green);
      Get (Str(1..0), Last, Stat, Pos, Ins,
         W1, Current, Current, Red,
         (Delay_Kind => Timers.Delay_Sec,
          Period => Con_Io.No_Period,
          Delay_Seconds => 3.0) );
    end loop;

    Destroy;
    delay 3.0;

    Init;
    delay 2.0;
    Destroy;
    delay 1.0;


  exception
    when Error : others =>
       My_Io.Put_Line ("Exception in " &  Me'Img
                     & " " & Ada.Exceptions.Exception_Name (Error));
       raise;
  end Task_T;


begin
  for I in T'Range loop
    T(I).Start(I);
  end loop;
end T_Con;

