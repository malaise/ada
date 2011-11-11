with Ada.Calendar, Ada.Text_Io;
with Basic_Proc, Day_Mng;

procedure T_Day_Mng is

  package Dur_Io is new Ada.Text_Io.Fixed_Io (Ada.Calendar.Day_Duration);

  Dur : Ada.Calendar.Day_Duration;
  Hours    : Day_Mng.T_Hours;
  Minutes  : Day_Mng.T_Minutes;
  Seconds  : Day_Mng.T_Seconds;
  Millisec : Day_Mng.T_Millisec;


begin

  loop

    loop
      begin
        Basic_Proc.Put_Output ("Enter a duration: ");
        Dur_Io.Get(Dur);
        exit;
      exception
        when others =>
          Basic_Proc.Put_Line_Output ("Error.");
      end;
    end loop;

    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);

    Basic_Proc.Put_Line_Output (Day_Mng.T_Hours'Image(Hours) & " h  "
                        & Day_Mng.T_Minutes'Image(Minutes) & " min  "
                        & Day_Mng.T_Seconds'Image(Seconds) & " sec  "
                        & Day_Mng.T_Millisec'Image(Millisec) & " msec");

    Dur := Day_Mng.Pack (Hours, Minutes, Seconds, Millisec);

    Dur_Io.Put(Dur);
    Basic_Proc.New_Line_Output;

  end loop;

end T_Day_Mng;

