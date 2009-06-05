with Ada.Text_Io;
with Date_Image;
with Virtual_Time, Chronos;
procedure T_Virtual is

  type Observer_Rec is new Virtual_Time.Observer with null record;

  procedure Notify (An_Observer : in out Observer_Rec;
                    Vtime : Virtual_Time.Time;
                    A_Clock : in Virtual_Time.Clock_Access) is
    Rt, Vt : Virtual_Time.Time;
  begin
    Ada.Text_Io.Put_Line ("Observer notified of change at "
                          & Date_Image (Vtime));
    A_Clock.Get_Synchro (Rt, Vt);
    Ada.Text_Io.Put_Line ("Synchro is " & Date_Image (Rt)
                        & " " & Date_Image (Vt));
    Ada.Text_Io.Put_Line ("Speed is " & A_Clock.Get_Speed'Img);
  end Notify;

  My_Clock : aliased Virtual_Time.Clock;
  My_Observer : aliased Observer_Rec;
  My_Chrono : Chronos.Chrono_Type;

begin
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Start chrono");
  My_Chrono.Start;
  Ada.Text_Io.Put_Line ("Registering observer");
  My_Clock.Add_Observer (My_Observer'Unrestricted_Access);
  Ada.Text_Io.Put_Line ("Stopping, attaching and starting chrono");
  My_Chrono.Stop;
  My_Chrono.Attach (My_Clock'Unrestricted_Access);
  My_Chrono.Start;
  Ada.Text_Io.Put_Line ("Waiting 1s");
  delay 1.0;
  Ada.Text_Io.Put_Line ("Setting speed to 0.1 and waiting 5s");
  My_Clock.Set_Speed (0.1);
  delay 5.0;
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
end T_Virtual;

