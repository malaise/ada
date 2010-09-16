with As.U; use As.U;
with Basic_Proc;
with Ios;
procedure Tios is
  Port : constant Positive := 50_000;
  Txt : Asu_Us;
  Evt : Ios.Event_Type;
  Disco : Boolean;
  use type Ios.Event_Kind_List;
begin
  Txt := Asu_Tus (Port'Img);
  Asu.Delete (Txt, 1, 1);
  Basic_Proc.Put_Line_Output ("Initializing on port " & Asu_Ts (Txt));
  Ios.Init (Txt);

  Basic_Proc.Put_Line_Output ("Arming timer for 1 min");
  Ios.Start_Global_Timer (60_000);

  Basic_Proc.Put_Line_Output ("Waiting 10s");
  Evt := Ios.Wait (10_000);
  Basic_Proc.Put_Line_Output ("Got " & Evt.Kind'Img);
  if Evt.Kind = Ios.Exit_Requested then
    Basic_Proc.Put_Line_Output ("Exiting");
    return;
  end if;

  loop
    Basic_Proc.Put_Line_Output ("Reading for 5s");
    Evt := Ios.Read (5_000);
    Basic_Proc.Put_Line_Output ("Got " & Evt.Kind'Img);
    case Evt.Kind is
      when Ios.Local_Timeout =>
        null;
      when Ios.Got_Sentence =>
        Basic_Proc.Put_Line_Output ("  >" & Asu_Ts (Evt.Sentence) & "<");
      when Ios.Global_Timeout =>
        exit;
      when Ios.Disconnection =>
        Basic_Proc.Put_Line_Output ("Reseting");
        Ios.Reset;
      when Ios.Exit_Requested =>
        Basic_Proc.Put_Line_Output ("Exiting");
        return;
    end case;
  end loop;

  Txt := Asu_Tus ("Toto");
  Basic_Proc.Put_Line_Output ("Sending """ & Asu_Ts (Txt) & """");
  Ios.Send (Txt, Disco);
  if Disco then
    Basic_Proc.Put_Line_Output ("Disconnected");
  end if;

  Basic_Proc.Put_Line_Output ("Closing");
  Ios.Close;

  Basic_Proc.Put_Line_Output ("Done.");
end Tios;

